{-# LANGUAGE OverloadedStrings #-}

-- | ECP5 binding-state and cell-timing tests.
--
-- The binding operations mirror @ecp5\/arch.h@ side effects exactly:
-- bindPip binds the dst wire + bumps the src fanout; unbindWire of a
-- pip-bound wire cleans up the pip; pip delays include the fanout
-- adder. The cell-timing tests verify port classification and the
-- timing-DB lookups against golden values read from the chipdb
-- (SPEED_6).
module Lambdapnr.Arch.Ecp5BindingTest (ecp5BindingTests) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Lambdapnr.Arch.Ecp5
import Lambdapnr.Arch.Ecp5.Binding
import Lambdapnr.Arch.Ecp5.CellTiming (TimingDb (..), getDelayFromTmgDb, getSetupholdFromTmgDb)
import Lambdapnr.Arch.Ecp5.Chipdb (Chipdb (..), PipDelay (..), PipInfo (..), SpeedGrade (..), pipAt)
import Lambdapnr.Arch.Ecp5.Types
import Lambdapnr.Kernel.Arch
import Lambdapnr.Kernel.Delay
import Lambdapnr.Kernel.IdString (IdString, emptyId)
import Lambdapnr.Kernel.Netlist
import Lambdapnr.Kernel.Timing

chipdbPath :: FilePath
chipdbPath = "data/ecp5/chipdb-25k.bin"

loadArch :: IO Ecp5
loadArch = do
  r <- loadEcp5 (Ecp5Args Lfe5um5g25f "" Speed6) chipdbPath
  case r of
    Left err -> error ("chipdb load failed: " ++ err)
    Right e -> pure e

-- | A minimal design with one empty net @n1@.
testDesign :: Design BelId WireId PipId
testDesign =
  addNet emptyId (NetInfo emptyId emptyId (PortRef Nothing emptyId) V.empty [] M.empty M.empty emptyId) emptyDesign

-- | A pip whose timing class has a nonzero fanout adder (deterministic
-- chipdb order).
pipWithFanoutAdder :: Ecp5 -> PipId
pipWithFanoutAdder e =
  case [ p
       | p <- getPips e
       , let cls = sgPipClasses (cdSpeedGrades (ecp5Chipdb e) V.! 0) V.! fromIntegral (piTimingClass (pipAt (ecp5Chipdb e) p))
       , pdMinFanout cls /= 0 || pdMaxFanout cls /= 0
       ] of
    p : _ -> p
    [] -> error "no pip with fanout adder in chipdb" 

ecp5BindingTests :: TestTree
ecp5BindingTests =
  testGroup
    "Ecp5 binding + timing"
    [ testCase "bindPip binds pip, dst wire and bumps src fanout" $ do
        e0 <- loadArch
        let p = pipWithFanoutAdder e0
            src = getPipSrcWire e0 p
            dst = getPipDstWire e0 p
            (e1, d1) = bindPip (ecp5Chipdb e0) emptyId p StrengthWeak (ecp5Bind e0) testDesign
        -- pip bound
        let e1a = setEcp5Bind e1 e0
        assertEqual "pip bound to net" (Just emptyId) (boundPipNet p (e1))
        assertEqual "arch query agrees" (Just emptyId) (getBoundPipNet e1a p)
        assertBool "pip unavailable while bound" (not (checkPipAvail e1a p))
        -- dst wire bound to the net
        assertEqual "dst wire bound" (Just emptyId) (boundWireNet dst (e1))
        assertEqual "dst wire unavailable" False (checkWireAvail e1a dst)
        -- src fanout incremented
        assertEqual "src fanout" 1 (wireFanoutOf src (e1))
        -- design bookkeeping: netWires[dst] = {Just pip, strength}
        let ni = M.findWithDefault (error "net n1 missing") emptyId (designNets d1)
        assertEqual "design netWires dst pip" (Just (PipMap (Just p) StrengthWeak)) (M.lookup dst (netWires ni))
        -- pip delay includes one fanout adder
        let cls = sgPipClasses (cdSpeedGrades (ecp5Chipdb e0) V.! 0) V.! fromIntegral (piTimingClass (pipAt (ecp5Chipdb e0) p))
            want0 = dqScalar (fromIntegral (pdMinBase cls)) (fromIntegral (pdMaxBase cls))
            want1 = dqScalar (fromIntegral (pdMinBase cls) + fromIntegral (pdMinFanout cls)) (fromIntegral (pdMaxBase cls) + fromIntegral (pdMaxFanout cls))
        assertEqual "delay before binding" want0 (getPipDelay e0 p)
        assertEqual "delay with fanout 1" want1 (getPipDelay e1a p)
    , testCase "unbindPip restores everything" $ do
        e0 <- loadArch
        let p = pipWithFanoutAdder e0
            src = getPipSrcWire e0 p
            dst = getPipDstWire e0 p
            (e1, d1) = bindPip (ecp5Chipdb e0) emptyId p StrengthWeak (ecp5Bind e0) testDesign
            (e2, d2) = unbindPip (ecp5Chipdb e0) p e1 d1
        assertEqual "pip freed" Nothing (boundPipNet p (e2))
        assertBool "pip available again" (checkPipAvail (setEcp5Bind e2 e0) p)
        assertEqual "dst wire freed" Nothing (boundWireNet dst (e2))
        assertEqual "fanout back to 0" 0 (wireFanoutOf src (e2))
        assertEqual "design wire removed" Nothing (M.lookup dst (netWires (M.findWithDefault (error "net missing") emptyId (designNets d2))))
        assertEqual "delay back to base" (getPipDelay e0 p) (getPipDelay (setEcp5Bind e2 e0) p)
    , testCase "fanout accumulates over multiple pips" $ do
        e0 <- loadArch
        -- two pips leaving the same src wire (take the first pip of a wire and a second)
        let p1 = pipWithFanoutAdder e0
            src = getPipSrcWire e0 p1
            p2 = case [q | q <- getPipsDownhill e0 src, q /= p1] of
              q : _ -> q
              [] -> error "no second downhill pip" 
            (e1, d1) = bindPip (ecp5Chipdb e0) emptyId p1 StrengthWeak (ecp5Bind e0) testDesign
            (e2, _d2) = bindPip (ecp5Chipdb e0) emptyId p2 StrengthWeak e1 d1
        assertEqual "fanout 2" 2 (wireFanoutOf src (e2))
        let e2a = setEcp5Bind e2 e0
            cls1 = sgPipClasses (cdSpeedGrades (ecp5Chipdb e2a) V.! 0) V.! fromIntegral (piTimingClass (pipAt (ecp5Chipdb e2a) p1))
        assertEqual
            "p1 delay with fanout 2"
            (dqScalar (fromIntegral (pdMinBase cls1) + 2 * fromIntegral (pdMinFanout cls1)) (fromIntegral (pdMaxBase cls1) + 2 * fromIntegral (pdMaxFanout cls1)))
            (getPipDelay e2a p1)
    , testCase "unbindWire of a pip-bound dst clears the pip" $ do
        e0 <- loadArch
        let p = pipWithFanoutAdder e0
            src = getPipSrcWire e0 p
            dst = getPipDstWire e0 p
            (e1, d1) = bindPip (ecp5Chipdb e0) emptyId p StrengthWeak (ecp5Bind e0) testDesign
            (e2, d2) = unbindWire (ecp5Chipdb e0) dst e1 d1
        assertEqual "dst freed" Nothing (boundWireNet dst (e2))
        assertEqual "pip cleaned up" Nothing (boundPipNet p (e2))
        assertEqual "fanout decremented" 0 (wireFanoutOf src (e2))
        assertBool "pip available" (checkPipAvail (setEcp5Bind e2 e0) p)
        assertBool "dst available" (checkWireAvail (setEcp5Bind e2 e0) dst)
        _ <- pure d2
        pure ()
    , testCase "bindWire/unbindWire roundtrip" $ do
        e0 <- loadArch
        let w = case getWires e0 of
              x : _ -> x
              [] -> error "chip has no wires"
            (e1, d1) = bindWire emptyId w StrengthFixed (ecp5Bind e0) testDesign
        assertEqual "wire bound" (Just emptyId) (boundWireNet w (e1))
        assertBool "wire unavailable" (not (checkWireAvail (setEcp5Bind e1 e0) w))
        let (e2, _d2) = unbindWire (ecp5Chipdb e0) w e1 d1
        assertEqual "wire freed" Nothing (boundWireNet w (e2))
        assertBool "wire available" (checkWireAvail (setEcp5Bind e2 e0) w)
    , testCase "bindBel/unbindBel roundtrip" $ do
        e0 <- loadArch
        let b = case getBels e0 of
              x : _ -> x
              [] -> error "chip has no bels"
            -- a design containing the cell to be bound
            cell =
                CellInfo
                    { cellName = emptyId
                    , cellType = emptyId
                    , cellHierpath = emptyId
                    , cellPorts = M.empty
                    , cellPortOrder = []
                    , cellAttrs = M.empty
                    , cellParams = M.empty
                    , cellBel = Nothing
                    , cellBelStrength = StrengthNone
                    , cellCluster = emptyId
                    , cellConstrX = 0
                    , cellConstrY = 0
                    , cellConstrZ = 0
                    , cellConstrAbsZ = False
                    , cellConstrChildren = []
                    }
            (e1, d1) = bindBel emptyId b StrengthLocked (ecp5Bind e0) (addCell emptyId cell testDesign)
        assertEqual "bel bound" (Just emptyId) (boundBelCell b (e1))
        assertEqual "arch query agrees" (Just emptyId) (getBoundBelCell (setEcp5Bind e1 e0) b)
        assertBool "bel unavailable" (not (checkBelAvail (setEcp5Bind e1 e0) b))
        let ci = M.findWithDefault (error "cell missing") emptyId (designCells d1)
        assertEqual "cell bel set" (Just b) (cellBel ci)
        assertEqual "cell strength" StrengthLocked (cellBelStrength ci)
        let (e2, _d2) = unbindBel emptyId b e1 d1
        assertEqual "bel freed" Nothing (boundBelCell b (e2))
        assertBool "bel available" (checkBelAvail (setEcp5Bind e2 e0) b)
    , testCase "timing DB: TRELLIS_COMB combinational arcs" $ do
        e <- loadArch
        let db = ecp5TimingDb e
            cid' name = M.findWithDefault emptyId name (tdConstIdByName db)
        -- golden values from the chipdb (SPEED_6)
        assertEqual "A->F" (Just (dqScalar 200 236)) (getDelayFromTmgDb db (cid' "TRELLIS_COMB") (cid' "A") (cid' "F"))
        assertEqual "A->OFX" (Just (dqScalar 268 401)) (getDelayFromTmgDb db (cid' "TRELLIS_COMB") (cid' "A") (cid' "OFX"))
        assertEqual "no such arc" Nothing (getDelayFromTmgDb db (cid' "TRELLIS_COMB") (cid' "F") (cid' "A"))
    , testCase "timing DB: SLOGICB clock-to-Q and setup/hold" $ do
        e <- loadArch
        let db = ecp5TimingDb e
            cid' name = M.findWithDefault emptyId name (tdConstIdByName db)
        assertEqual "CLK->Q0" (Just (dqScalar 430 525)) (getDelayFromTmgDb db (cid' "SLOGICB") (cid' "CLK") (cid' "Q0"))
        assertEqual
            "CLK->DI0 setup/hold"
            (Just (DelayPair 0 0, DelayPair 267 303))
            (getSetupholdFromTmgDb db (cid' "SLOGICB") (cid' "CLK") (cid' "DI0"))
        assertEqual
            "SDPRAME WCK->WD0 setup/hold"
            (Just (DelayPair 0 0, DelayPair 202 297))
            (getSetupholdFromTmgDb db (cid' "SDPRAME") (cid' "WCK") (cid' "WD0"))
    , testCase "port timing classes" $ do
        e <- loadArch
        let db = ecp5TimingDb e
            cid' name = M.findWithDefault emptyId name (tdConstIdByName db)
            mkPort p = PortInfo{portName = cid' p, portNet = Just emptyId, portType = PortIn, portUserIdx = 0}
            combWithInputs =
                CellInfo
                    { cellName = emptyId
                    , cellType = cid' "TRELLIS_COMB"
                    , cellHierpath = emptyId
                    , cellPorts = M.fromList [(cid' "A", mkPort "A"), (cid' "B", mkPort "B"), (cid' "C", mkPort "C"), (cid' "D", mkPort "D"), (cid' "FCI", mkPort "FCI"), (cid' "F", mkPort "F")]
                    , cellPortOrder = map cid' ["A", "B", "C", "D", "FCI", "F"]
                    , cellAttrs = M.empty
                    , cellParams = M.empty
                    , cellBel = Nothing
                    , cellBelStrength = StrengthNone
                    , cellCluster = emptyId
                    , cellConstrX = 0
                    , cellConstrY = 0
                    , cellConstrZ = 0
                    , cellConstrAbsZ = False
                    , cellConstrChildren = []
                    }
            combNoInputs = combWithInputs{cellPorts = M.fromList [(cid' "F", mkPort "F")], cellPortOrder = [cid' "F"]}
            ff =
                CellInfo
                    { cellName = emptyId
                    , cellType = cid' "TRELLIS_FF"
                    , cellHierpath = emptyId
                    , cellPorts = M.fromList [(cid' "CLK", mkPort "CLK"), (cid' "DI", mkPort "DI"), (cid' "Q", mkPort "Q")]
                    , cellPortOrder = map cid' ["CLK", "DI", "Q"]
                    , cellAttrs = M.empty
                    , cellParams = M.empty
                    , cellBel = Nothing
                    , cellBelStrength = StrengthNone
                    , cellCluster = emptyId
                    , cellConstrX = 0
                    , cellConstrY = 0
                    , cellConstrZ = 0
                    , cellConstrAbsZ = False
                    , cellConstrChildren = []
                    }
        assertEqual "comb A is comb input" (TmgCombInput, 0) (getPortTimingClass e combWithInputs (cid' "A"))
        assertEqual "comb F is comb output" (TmgCombOutput, 0) (getPortTimingClass e combWithInputs (cid' "F"))
        assertEqual "comb WCK is clock input" (TmgClockInput, 0) (getPortTimingClass e combWithInputs (cid' "WCK"))
        assertEqual "comb WD is register input" (TmgRegisterInput, 1) (getPortTimingClass e combWithInputs (cid' "WD"))
        assertEqual "comb F with no inputs is ignore" (TmgIgnore, 0) (getPortTimingClass e combNoInputs (cid' "F"))
        assertEqual "ff CLK is clock input" (TmgClockInput, 0) (getPortTimingClass e ff (cid' "CLK"))
        assertEqual "ff DI is register input" (TmgRegisterInput, 1) (getPortTimingClass e ff (cid' "DI"))
        assertEqual "ff Q is register output" (TmgRegisterOutput, 1) (getPortTimingClass e ff (cid' "Q"))
    , testCase "cell combinational delays" $ do
        e <- loadArch
        let db = ecp5TimingDb e
            cid' name = M.findWithDefault emptyId name (tdConstIdByName db)
            comb =
                CellInfo
                    { cellName = emptyId
                    , cellType = cid' "TRELLIS_COMB"
                    , cellHierpath = emptyId
                    , cellPorts = M.empty
                    , cellPortOrder = []
                    , cellAttrs = M.empty
                    , cellParams = M.empty
                    , cellBel = Nothing
                    , cellBelStrength = StrengthNone
                    , cellCluster = emptyId
                    , cellConstrX = 0
                    , cellConstrY = 0
                    , cellConstrZ = 0
                    , cellConstrAbsZ = False
                    , cellConstrChildren = []
                    }
            dcca =
                CellInfo
                    { cellName = emptyId
                    , cellType = cid' "DCCA"
                    , cellHierpath = emptyId
                    , cellPorts = M.empty
                    , cellPortOrder = []
                    , cellAttrs = M.empty
                    , cellParams = M.empty
                    , cellBel = Nothing
                    , cellBelStrength = StrengthNone
                    , cellCluster = emptyId
                    , cellConstrX = 0
                    , cellConstrY = 0
                    , cellConstrZ = 0
                    , cellConstrAbsZ = False
                    , cellConstrChildren = []
                    }
        assertEqual "comb A->F" (Just (dqScalar 200 236)) (getCellDelay e comb (cid' "A") (cid' "F"))
        assertEqual "comb F->A has no arc" Nothing (getCellDelay e comb (cid' "F") (cid' "A"))
        assertEqual "DCCA CLKI->CLKO" (Just (dqFromDelay 0)) (getCellDelay e dcca (cid' "CLKI") (cid' "CLKO"))
    , testCase "clocking info: FF Q clock-to-Q from SLOGICB" $ do
        e <- loadArch
        let db = ecp5TimingDb e
            cid' name = M.findWithDefault emptyId name (tdConstIdByName db)
            ff =
                CellInfo
                    { cellName = emptyId
                    , cellType = cid' "TRELLIS_FF"
                    , cellHierpath = emptyId
                    , cellPorts = M.empty
                    , cellPortOrder = []
                    , cellAttrs = M.empty
                    , cellParams = M.empty
                    , cellBel = Nothing
                    , cellBelStrength = StrengthNone
                    , cellCluster = emptyId
                    , cellConstrX = 0
                    , cellConstrY = 0
                    , cellConstrZ = 0
                    , cellConstrAbsZ = False
                    , cellConstrChildren = []
                    }
            info = getPortClockingInfo e ff (cid' "Q") 0
        assertEqual "clock port" (cid' "CLK") (tciClockPort info)
        assertEqual "edge" RisingEdge (tciEdge info)
        assertEqual "clock-to-Q" (dqScalar 430 525) (tciClockToQ info)
    ]
