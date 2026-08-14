{-# LANGUAGE OverloadedStrings #-}

-- | Netlist model tests: connect/disconnect semantics.
module Lambdapnr.Kernel.NetlistTest (netlistTests) where

import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Lambdapnr.Kernel.IdString (IdString (..))
import Lambdapnr.Kernel.Netlist

-- | Convenience: id from a raw index (tests exercise structure, not names).
i :: Int -> IdString
i = IdString

-- Tests use a dummy id type (ids are opaque to the netlist).
type DummyBel = Int
type DummyWire = Int
type DummyPip = Int

mkCell :: IdString -> IdString -> CellInfo DummyBel DummyWire DummyPip
mkCell name typ =
    CellInfo
        { cellName = name
        , cellType = typ
        , cellHierpath = IdString 0
        , cellPorts = M.empty
        , cellAttrs = M.empty
        , cellParams = M.empty
        , cellBel = Nothing
        , cellBelStrength = StrengthNone
        , cellCluster = IdString 0
        , cellConstrX = 0
        , cellConstrY = 0
        , cellConstrZ = 0
        , cellConstrAbsZ = False
        , cellConstrChildren = []
        }

addPort :: IdString -> PortDir -> CellInfo DummyBel DummyWire DummyPip -> CellInfo DummyBel DummyWire DummyPip
addPort name dir ci =
    ci{cellPorts = M.insert name (PortInfo name Nothing dir (-1)) (cellPorts ci)}

-- | A one-cell one-net design with the cell port connected to the net.
setup :: IdString -> IdString -> PortDir -> Design DummyBel DummyWire DummyPip
setup cellId netId dir =
    connectPort cellId portName netId d2
  where
    portName = i 100
    ci = addPort portName dir (mkCell cellId (IdString 2))
    d0 = emptyDesign
    d1 = addCell cellId ci d0
    d2 =
        addNet
            netId
            (NetInfo netId (IdString 0) (PortRef Nothing portName) V.empty [] M.empty M.empty (IdString 0))
            d1

netlistTests :: TestTree
netlistTests =
    testGroup
        "Netlist"
        [ testCase "output port becomes the net driver" $ do
            let d = setup (IdString 1) (IdString 3) PortOut
            case lookupNet (IdString 3) d of
                Nothing -> assertBool "net exists" False
                Just ni -> do
                    assertEqual "driver cell" (Just (IdString 1)) (prCell (netDriver ni))
                    assertEqual "driver port" (i 100) (prPort (netDriver ni))
                    assertEqual
                        "port points at net"
                        (Just (IdString 3))
                        (portNet (cellPorts (mustCell d (IdString 1)) M.! i 100))
        , testCase "input port is appended to users" $ do
            let d = setup (IdString 1) (IdString 3) PortIn
            case lookupNet (IdString 3) d of
                Nothing -> assertBool "net exists" False
                Just ni -> do
                    assertEqual "one user" 1 (V.length (netUsers ni))
                    assertEqual "user cell" (Just (IdString 1)) (prCell =<< V.head (netUsers ni))
                    assertEqual "user port" (Just (i 100)) (prPort <$> V.head (netUsers ni))
                    assertEqual "driver stays dangling" Nothing (prCell (netDriver ni))
        , testCase "disconnect removes the user" $ do
            let d = setup (IdString 1) (IdString 3) PortIn
                d' = disconnectPort (IdString 1) (i 100) d
            case lookupNet (IdString 3) d' of
                Nothing -> assertBool "net exists" False
                Just ni -> do
                    assertEqual "user removed" 0 (V.length (V.filter (maybe False (const True)) (netUsers ni)))
                    assertEqual
                        "port cleared"
                        Nothing
                        (portNet (cellPorts (mustCell d' (IdString 1)) M.! i 100))
        , testCase "disconnect clears the driver" $ do
            let d = setup (IdString 1) (IdString 3) PortOut
                d' = disconnectPort (IdString 1) (i 100) d
            case lookupNet (IdString 3) d' of
                Nothing -> assertBool "net exists" False
                Just ni -> assertEqual "driver cleared" Nothing (prCell (netDriver ni))
        , testCase "unknown cell or port is a no-op" $ do
            let d0 = emptyDesign
                ci = addPort (i 100) PortIn (mkCell (IdString 1) (IdString 2))
                d1 = addCell (IdString 1) ci d0
            assertEqual "unknown port" d1 (connectPort (IdString 1) (i 101) (IdString 3) d1)
            assertEqual "unknown cell" d1 (connectPort (IdString 9) (i 100) (IdString 3) d1)
        , testCase "multiple users accumulate in order" $ do
            let d0 = emptyDesign
                c1 = addPort (i 100) PortIn (mkCell (IdString 1) (IdString 2))
                c2 = addPort (i 100) PortIn (mkCell (IdString 4) (IdString 2))
                d1 = addCell (IdString 1) c1 (addCell (IdString 4) c2 emptyDesign)
                d2 =
                    addNet
                        (IdString 3)
                        (NetInfo (IdString 3) (IdString 0) (PortRef Nothing (i 100)) V.empty [] M.empty M.empty (IdString 0))
                        d1
                d3 = connectPort (IdString 1) (i 100) (IdString 3) d2
                d4 = connectPort (IdString 4) (i 100) (IdString 3) d3
            case lookupNet (IdString 3) d4 of
                Nothing -> assertBool "net exists" False
                Just ni -> do
                    assertEqual "two users" 2 (V.length (netUsers ni))
                    assertEqual "order preserved" [IdString 1, IdString 4] (map (maybe (IdString 0) (maybe (IdString 0) id . prCell)) (V.toList (netUsers ni)))
        ]
  where
    mustCell :: Design DummyBel DummyWire DummyPip -> IdString -> CellInfo DummyBel DummyWire DummyPip
    mustCell d c = maybe (error "cell missing") id (lookupCell c d)
