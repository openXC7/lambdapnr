{-# LANGUAGE OverloadedStrings #-}

-- | Checksum tests: determinism, order independence, sensitivity.
module Lambdapnr.Kernel.ChecksumTest (checksumTests) where

import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Data.Word (Word32)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Lambdapnr.Kernel.Checksum
import Lambdapnr.Kernel.IdString (IdString (..))
import Lambdapnr.Kernel.Netlist
import Lambdapnr.Kernel.Property (propFromInt)

type DummyBel = Int
type DummyWire = Int
type DummyPip = Int

-- Dummy arch checksums: identity on the index, like the C++ defaults
-- (bel.hash() etc).
belCk :: DummyBel -> Word32
belCk = fromIntegral

wireCk :: DummyWire -> Word32
wireCk = fromIntegral

pipCk :: DummyPip -> Word32
pipCk = fromIntegral

{- | A small design: one slice-like cell with two input ports and one
output, a driver cell, three nets.
id helpers: (cell 1 = slice, cell 2 = driver; ports: I0=3, I1=4, F=5,
O=6; nets: 30 = input net, 31 = output net; attr/param keys: 20/21)
-}
i :: Int -> IdString
i = IdString

slice :: CellInfo DummyBel DummyWire DummyPip
slice =
    CellInfo
        { cellName = i 1
        , cellType = i 10
        , cellHierpath = i 0
        , cellPorts =
            M.fromList
                [ (i 3, PortInfo (i 3) Nothing PortIn 0)
                , (i 4, PortInfo (i 4) Nothing PortIn 1)
                , (i 5, PortInfo (i 5) Nothing PortOut 0)
                ]
        , cellPortOrder = [i 3, i 4, i 5]
        , cellAttrs = M.singleton (i 20) (propFromInt 5 16)
        , cellParams = M.singleton (i 21) (propFromInt 1 32)
        , cellBel = Just 42
        , cellBelStrength = StrengthWeak
        , cellCluster = i 0
        , cellConstrX = 0
        , cellConstrY = 0
        , cellConstrZ = 0
        , cellConstrAbsZ = False
        , cellConstrChildren = []
        }

drv :: CellInfo DummyBel DummyWire DummyPip
drv =
    CellInfo
        { cellName = i 2
        , cellType = i 11
        , cellHierpath = i 0
        , cellPorts = M.fromList [(i 6, PortInfo (i 6) Nothing PortOut 0)]
        , cellPortOrder = [i 6]
        , cellAttrs = M.empty
        , cellParams = M.empty
        , cellBel = Nothing
        , cellBelStrength = StrengthNone
        , cellCluster = i 0
        , cellConstrX = 0
        , cellConstrY = 0
        , cellConstrZ = 0
        , cellConstrAbsZ = False
        , cellConstrChildren = []
        }

net0 :: NetInfo DummyBel DummyWire DummyPip
net0 = NetInfo (i 30) (i 0) (PortRef Nothing (i 6)) V.empty [] M.empty M.empty (i 0)

net1 :: NetInfo DummyBel DummyWire DummyPip
net1 = NetInfo (i 31) (i 0) (PortRef (Just (i 1)) (i 5)) V.empty [] M.empty M.empty (i 0)

mkDesign :: Design DummyBel DummyWire DummyPip
mkDesign =
    connectPort (i 2) (i 6) (i 30) $
        connectPort (i 1) (i 3) (i 31) d2
  where
    d0 = emptyDesign
    d1 = addCell (i 1) slice (addCell (i 2) drv d0)
    d2 = addNet (i 30) net0 (addNet (i 31) net1 d1)

checksumTests :: TestTree
checksumTests =
    testGroup
        "Checksum"
        [ testCase "stable for identical designs" $ do
            assertEqual "same design, same checksum" (checksum belCk wireCk pipCk mkDesign) (checksum belCk wireCk pipCk mkDesign)
        , testCase "order of insertion does not matter" $ do
            -- rebuild the same design inserting nets/cells in a different order
            let d0 = emptyDesign
                -- different order: nets first, then cells
                d1 = addNet (i 30) net0 (addNet (i 31) net1 d0)
                d2 = addCell (i 2) drv (addCell (i 1) slice d1)
                d3 = connectPort (i 1) (i 3) (i 31) d2
                d4 = connectPort (i 2) (i 6) (i 30) d3
            assertEqual
                "order-independent"
                (checksum belCk wireCk pipCk mkDesign)
                (checksum belCk wireCk pipCk d4)
        , testCase "sensitive to net content" $ do
            let d' = mkDesign{designNets = M.adjust (\ni -> ni{netAttrs = M.insert (IdString 99) (propFromInt 1 1) (netAttrs ni)}) (IdString 30) (designNets mkDesign)}
            assertBool "attr change alters checksum" (checksum belCk wireCk pipCk d' /= checksum belCk wireCk pipCk mkDesign)
        , testCase "sensitive to cell binding" $ do
            let d' = mkDesign{designCells = M.adjust (\ci -> ci{cellBel = Just 43}) (IdString 1) (designCells mkDesign)}
            assertBool "bel change alters checksum" (checksum belCk wireCk pipCk d' /= checksum belCk wireCk pipCk mkDesign)
        , testCase "sensitive to strength" $ do
            let d' = mkDesign{designCells = M.adjust (\ci -> ci{cellBelStrength = StrengthStrong}) (IdString 1) (designCells mkDesign)}
            assertBool "strength change alters checksum" (checksum belCk wireCk pipCk d' /= checksum belCk wireCk pipCk mkDesign)
        , testCase "empty design checksum is a fixed value" $ do
            assertEqual "empty" 0x4a82ab01 (checksum belCk wireCk pipCk emptyDesign)
        ]
