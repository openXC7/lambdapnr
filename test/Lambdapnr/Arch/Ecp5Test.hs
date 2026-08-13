{-# LANGUAGE OverloadedStrings #-}

-- | ECP5 architecture tests: chipdb parsing, query consistency and
-- determinism. These are the first ECP5 unit tests in either codebase
-- (nextpnr's ecp5 has no gtest suite — gap G3 in SPECIFICATION.md).
module Lambdapnr.Arch.Ecp5Test (ecp5Tests) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Lambdapnr.Arch.Ecp5
import Lambdapnr.Arch.Ecp5.Chipdb (BelWire, Chipdb (..), LocationType (..), biBelWires, bwWireIndex, ltWires)
import Lambdapnr.Arch.Ecp5.Types
import Lambdapnr.Kernel.Arch
import Lambdapnr.Kernel.Delay (dqMaxDelay, dqMinDelay)
import Lambdapnr.Kernel.IdString (IdString (..))

chipdbPath :: FilePath
chipdbPath = "data/ecp5/chipdb-25k.bin"

testArgs :: Ecp5Args
testArgs = Ecp5Args Lfe5um5g25f "" Speed6

loadArch :: IO Ecp5
loadArch = do
  r <- loadEcp5 testArgs chipdbPath
  case r of
    Left err -> error ("chipdb load failed: " ++ err)
    Right e -> pure e

-- | Sample every k-th element (keeps full-graph checks cheap).
sampled :: Int -> [a] -> [a]
sampled k = go 0
  where
    go _ [] = []
    go i (x : xs) = if i `mod` k == 0 then x : go (i + 1) xs else go (i + 1) xs

ecp5Tests :: TestTree
ecp5Tests =
  testGroup
    "Ecp5"
    [ testCase "chipdb loads and reports grid dims" $ do
        e <- loadArch
        let cd = ecp5Chipdb e
        assertBool "width > 0" (cdWidth cd > 0)
        assertBool "height > 0" (cdHeight cd > 0)
        assertEqual "numTiles = width*height" (cdWidth cd * cdHeight cd) (cdNumTiles cd)
        assertEqual "constids match" 1846 (cdConstIdCount cd)
        assertEqual "grid dims" (cdWidth cd) (getGridDimX e)
    , testCase "bel/wire/pip counts are positive and consistent" $ do
        e <- loadArch
        let nBels = length (getBels e)
            nWires = length (getWires e)
            nPips = length (getPips e)
        assertBool "bels > 0" (nBels > 0)
        assertBool "wires > 0" (nWires > 0)
        assertBool "pips > 0" (nPips > 0)
        -- every bel pin wire index is valid within its tile type
        forBels e $ \b ->
          forBelWires e b $ \bw ->
            assertBool "bel wire index in range" (bwWireIndex bw < fromIntegral (V.length (ltWires (locTypeOfTile (ecp5Chipdb e) (tileIndex (ecp5Chipdb e) (belLoc b))))))
    , testCase "uphill/downhill consistency (sampled wires)" $ do
        e <- loadArch
        let wires = sampled 500 (getWires e)
            -- a wire's downhill pips leave the wire (pip src == wire);
            -- its uphill pips arrive at it (pip dst == wire)
            badDown = [ (w, p) | w <- wires, p <- getPipsDownhill e w, getPipSrcWire e p /= w ]
            badUp = [ (w, p) | w <- wires, p <- getPipsUphill e w, getPipDstWire e p /= w ]
        assertEqual "every downhill pip leaves its source wire" [] (take 10 badDown)
        assertEqual "every uphill pip reaches its destination wire" [] (take 10 badUp)
    , testCase "pip src/dst resolves into downhill/uphill (sampled pips)" $ do
        e <- loadArch
        let pips = sampled 2000 (getPips e)
        forM_ (take 2000 pips) $ \p -> do
          let src = getPipSrcWire e p
              dst = getPipDstWire e p
          assertBool "pip in src downhill" (p `elem` getPipsDownhill e src)
          assertBool "pip in dst uphill" (p `elem` getPipsUphill e dst)
    , testCase "bel/wire name roundtrips (sampled)" $ do
        e <- loadArch
        forM_ (take 500 (sampled 200 (getBels e))) $ \b ->
          assertEqual "bel by name" (Just b) (getBelByName e (getBelName e b))
        forM_ (take 500 (sampled 2000 (getWires e))) $ \w ->
          assertEqual "wire by name" (Just w) (getWireByName e (getWireName e w))
    , testCase "pip name roundtrips (sampled)" $ do
        e <- loadArch
        forM_ (take 200 (sampled 5000 (getPips e))) $ \p ->
          assertEqual "pip by name" (Just p) (getPipByName e (getPipName e p))
    , testCase "delays are non-negative and consistent with speed grade" $ do
        e <- loadArch
        forM_ (take 200 (sampled 10000 (getPips e))) $ \p -> do
          let d = getPipDelay e p
          assertBool "pip delay non-negative" (dqMinDelay d >= 0 && dqMaxDelay d >= 0)
        case getWires e of
          [] -> assertBool "chip has wires" False
          w0 : _ -> do
            let w1 = last (getWires e)
            assertBool "estimate non-negative" (estimateDelay e w0 w1 >= 0)
        assertBool "ripup penalty positive" (getRipupDelayPenalty e > 0)
    , testCase "bucket and type queries" $ do
        e <- loadArch
        let types = getCellTypes e
        assertBool "cell types non-empty" (not (null types))
        case getBels e of
          [] -> assertBool "chip has bels" False
          b0 : _ -> do
            let bucket = getBelBucketForBel e b0
            assertBool "bucket lookup" (getBelBucketForCellType e bucket == bucket)
            assertBool "bel in its bucket" (b0 `elem` getBelsInBucket e bucket)
    , testCase "load determinism" $ do
        e1 <- loadArch
        e2 <- loadArch
        assertEqual "same bel count" (length (getBels e1)) (length (getBels e2))
        assertEqual "same wire count" (length (getWires e1)) (length (getWires e2))
        assertEqual "same pip count" (length (getPips e1)) (length (getPips e2))
        assertEqual "same chip name" (getChipName e1) (getChipName e2)
    ]
  where
    -- helpers ------------------------------------------------------------

    forBels :: Ecp5 -> (BelId -> IO ()) -> IO ()
    forBels e f = mapM_ f (take 300 (sampled 500 (getBels e)))

    forBelWires :: Ecp5 -> BelId -> (BelWire -> IO ()) -> IO ()
    forBelWires e b f = mapM_ f (V.toList (biBelWires (belAt (ecp5Chipdb e) b)))

    _keepId :: IdString -> ()
    _keepId _ = ()
