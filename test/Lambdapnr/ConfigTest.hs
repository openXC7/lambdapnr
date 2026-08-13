{-# LANGUAGE OverloadedStrings #-}

-- | Text-config format tests: the writer golden, reader roundtrip, the
-- reference rioctrl config, the base-config entry counts vs the C++
-- source, and the bitgen pip arcs.
module Lambdapnr.ConfigTest (configTests) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Lambdapnr.Arch.Ecp5
import Lambdapnr.Arch.Ecp5.BaseConfigs (configEmpty)
import Lambdapnr.Arch.Ecp5.Binding (bindPip)
import Lambdapnr.Arch.Ecp5.Bitgen (buildConfig, getPipTileName, getTrellisWireName)
import Lambdapnr.Arch.Ecp5.Chipdb (pipAt)
import Lambdapnr.Arch.Ecp5.Config
import Lambdapnr.Arch.Ecp5.Types
import Lambdapnr.Kernel.Arch
import Lambdapnr.Kernel.IdString (emptyId, newIdTable)
import Lambdapnr.Kernel.Netlist

-- | Golden writer output for a small hand-built config.
writerGolden :: T.Text
writerGolden =
    T.unlines
        [ ".device LFE5U-12F"
        , ""
        , ".comment Part: LFE5U-12F-6CABGA256"
        , ""
        , ".tile CIB_R10C1:CIB_LR"
        , "arc: E1_H02E0201 S1_V02N0201"
        , "word: SLICEA.K0.INIT 0000000000000001"
        , "enum: CIB.JC1MUX 0"
        , "unknown: F2B0"
        , ""
        ]

sampleConfig :: ChipConfig
sampleConfig =
    emptyChipConfig
        { ccChipName = "LFE5U-12F"
        , ccMetadata = ["Part: LFE5U-12F-6CABGA256"]
        , ccTiles =
            M.fromList
                [ ( "CIB_R10C1:CIB_LR"
                  , emptyTileConfig
                        & addArc "E1_H02E0201" "S1_V02N0201"
                        & addEnum "CIB.JC1MUX" "0"
                        & addWord "SLICEA.K0.INIT" (True : replicate 15 False)
                        & addUnknown 2 0
                  )
                ]
        }

-- | Left-to-right application for the builder chain.
(&) :: a -> (a -> b) -> b
x & f = f x

configTests :: TestTree
configTests =
    testGroup
        "Config"
        [ testCase "writer golden" $ do
            assertEqual "exact text" writerGolden (renderChipConfig sampleConfig)
        , testCase "reader roundtrip" $ do
            let parsed = parseChipConfig (renderChipConfig sampleConfig)
            case parsed of
                Left err -> assertBool ("parse failed: " ++ err) False
                Right cc -> do
                    assertEqual "chip name" "LFE5U-12F" (ccChipName cc)
                    assertEqual "metadata" ["Part: LFE5U-12F-6CABGA256"] (ccMetadata cc)
                    assertEqual "tile config roundtrip" (M.lookup "CIB_R10C1:CIB_LR" (ccTiles sampleConfig)) (M.lookup "CIB_R10C1:CIB_LR" (ccTiles cc))
        , testCase "reference config parses" $ do
            src <- TIO.readFile "test/ecp5/rioctrl/reference/rioctrl_controller.textcfg"
            case parseChipConfig src of
                Left err -> assertBool ("reference parse failed: " ++ err) False
                Right cc -> do
                    assertEqual "device" "LFE5U-12F" (ccChipName cc)
                    let nTiles = M.size (ccTiles cc)
                        nArcs = sum (map (length . tcArcs) (M.elems (ccTiles cc)))
                        nEnums = sum (map (length . tcEnums) (M.elems (ccTiles cc)))
                        nWords = sum (map (length . tcWords) (M.elems (ccTiles cc)))
                    -- the reference has 90,845 .tile/arc:/enum: lines
                    assertBool "many tiles" (nTiles > 1000)
                    assertBool "arcs present" (nArcs > 10000)
                    assertBool "enums present" (nEnums > 10000)
                    assertBool "words present" (nWords > 4000)
        , testCase "base config entry counts match the C++ source" $ do
            -- golden counts from baseconfigs.cc (per device)
            let expect =
                    [ ("LFE5U-25F", 166, 4, 9)
                    , ("LFE5U-45F", 315, 4, 9)
                    , ("LFE5U-85F", 315, 4, 33)
                    , ("LFE5UM-25F", 21, 4, 154)
                    , ("LFE5UM-45F", 25, 4, 299)
                    , ("LFE5UM5G-25F", 21, 4, 154)
                    , ("LFE5UM5G-45F", 25, 4, 299)
                    , ("LFE5UM5G-85F", 25, 4, 323)
                    , ("LFE5UM-85F", 25, 4, 323)
                    ]
            forM_ expect $ \(dev, e, a, u) -> do
                let tiles = configEmpty dev
                    nE = sum (map (length . tcEnums) (M.elems tiles))
                    nA = sum (map (length . tcArcs) (M.elems tiles))
                    nU = sum (map (length . tcUnknowns) (M.elems tiles))
                assertEqual (T.unpack dev ++ " enums") e nE
                assertEqual (T.unpack dev ++ " arcs") a nA
                assertEqual (T.unpack dev ++ " unknowns") u nU
        , testCase "bound pips become routing arcs" $ do
            e <- loadEcp5 (Ecp5Args Lfe5um5g25f "" Speed6) "data/ecp5/chipdb-25k.bin" >>= either (error . show) pure
            let p = head (getPips e)
                src = getPipSrcWire e p
                dst = getPipDstWire e p
                (bs, d) = bindPip (ecp5Chipdb e) emptyId p StrengthWeak (ecp5Bind e) emptyDesign
                e' = setEcp5Bind bs e
                cc = buildConfig e' d
                tile = getPipTileName e' p
            -- the pip must land in its tile's arc list with trellis names
            case M.lookup tile (ccTiles cc) of
                Nothing -> assertBool ("pip tile missing: " ++ show tile) False
                Just tc -> do
                    let arc = ConfigArc (getTrellisWireName e' (pipLoc p) dst) (getTrellisWireName e' (pipLoc p) src)
                    assertBool "arc present" (arc `elem` tcArcs tc)
            -- the rest of the config is the base: no cells bound
            assertBool "config non-empty" (not (null (ccTiles cc)))
        , testCase "12k uses the 25k base config" $ do
            e <- loadEcp5 (Ecp5Args Lfe5u12f "" Speed6) "data/ecp5/chipdb-25k.bin" >>= either (error . show) pure
            let cc = buildConfig e emptyDesign
            assertEqual "chip name" "LFE5U-12F" (ccChipName cc)
            assertEqual "base = 25f base" (configEmpty "LFE5U-25F") (ccTiles cc)
        ]
