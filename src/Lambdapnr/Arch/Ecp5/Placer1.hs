{-# LANGUAGE OverloadedStrings #-}

-- | @placer1_refine@ (@common\/place\/placer1.cc@, the @SAPlacer@ with
-- @place(true)@): a simulated-annealing refinement at a low fixed
-- temperature, run by the ECP5 @place()@ immediately after
-- @placer_heap@. It re-places non-clustered cells and swaps carry/ram
-- chains to shave wirelength/timing, using the same incremental
-- net-bounding-box cost machinery as the standalone @placer1@ SA.
--
-- This module ports only the /refine/ path (the heap handoff calls
-- @placer1_refine@ with @netShareWeight@ taken from @placerHeap@ — 0 by
-- default — so the net-share bonus code is skipped). The constraint
-- distance term is provably zero for refine moves (both the moved cell
-- and any displaced cell are non-clustered), so it is omitted too.
module Lambdapnr.Arch.Ecp5.Placer1
  ( place1Refine
  ) where

import Data.Int (Int64)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Word (Word32)
import Data.Maybe (fromMaybe)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

import Lambdapnr.Arch.Ecp5
import Lambdapnr.Arch.Ecp5.Types (BelId, PipId, WireId)
import Lambdapnr.Kernel.DeterministicRng (Rng, rng30, rngBounded, rngState)
import GHC.Float (castFloatToWord32)
import Lambdapnr.Kernel.Arch (Loc (..), getBelByLocation, getBelGlobalBuf, getBelLocation, getBelType, getBels, getGridDimX, getGridDimY, isValidBelForCellType, predictDelay)
import Lambdapnr.Kernel.Delay (DelayPair (..), DelayQuad (..))
import Lambdapnr.Kernel.IdString (IdString (..), emptyId)
import Lambdapnr.Kernel.Netlist
import Lambdapnr.Kernel.Timing (TimingPortClass (..))
import Lambdapnr.Kernel.TimingAnalyser (ArrivReqTime (..), CellPortKey (..), PerPort (..), PortDomainPairData (..), TimingAnalyser (..), buildTimingAnalyser, criticalityOf, runTimingAnalyser)
import Lambdapnr.Arch.Ecp5.ArchCellInfo (ArchInfo, assignArchInfo)
import Lambdapnr.Arch.Ecp5.CellTiming (TimingDb, getCellDelayAi, getPortClockingInfoAi, getPortTimingClassAi)
import Lambdapnr.Arch.Ecp5.Binding (BindState, bindBel, boundBelCell, unbindBel)
import Lambdapnr.Arch.Ecp5.PlacerHeap (isBelLocValidE)

-- ---------------------------------------------------------------------------
-- Bounding box (placer1.cc: BoundingBox)
-- ---------------------------------------------------------------------------

data SaBB = SaBB
    { bbX0 :: !Int
    , bbX1 :: !Int
    , bbY0 :: !Int
    , bbY1 :: !Int
    , bbNx0 :: !Int
    , bbNx1 :: !Int
    , bbNy0 :: !Int
    , bbNy1 :: !Int
    }
    deriving (Eq, Show)

defaultBB :: SaBB
defaultBB = SaBB 0 0 0 0 0 0 0 0

hpwlBB :: SaBB -> Int64
hpwlBB bb = fromIntegral ((bbX1 bb - bbX0 bb) + (bbY1 bb - bbY0 bb))

-- ---------------------------------------------------------------------------
-- FastBels (placer1.cc: FastBels with check_bel_available=false)
-- ---------------------------------------------------------------------------

-- | @FastBelsData@ shape: @(xSize, rows)@ where @rows@ maps x to
-- @(ySize, Map y [BelId])@. Bels appear in @getBels@ order.
type FastBels1T = (Int, M.Map Int (Int, M.Map Int [BelId]))

minBelsForGridPick :: Int
minBelsForGridPick = 64

-- | Build the fast-bel table for one cell type (no availability filter,
-- collapsing to (0,0) when the type has fewer than @minBelsForGridPick@
-- possible bels). Returns @(type_cnt, bel_data)@.
buildFastBels1 :: Ecp5 -> IdString -> (Int, FastBels1T)
buildFastBels1 e t =
    let bels = [b | b <- getBels e, isValidBelForCellType e t b]
        typeCnt = length bels
        collapse = typeCnt < minBelsForGridPick
        xy b = let Loc x y _ = getBelLocation e b in if collapse then (0, 0) else (x, y)
        rows =
            foldl'
                ( \m b ->
                    let (x, y) = xy b
                     in M.insertWith (\_nr o -> M.insertWith (flip (++)) y [b] o) x (M.singleton y [b]) m
                )
                M.empty
                bels
        xSize = maybe 0 ((+ 1) . fst) (M.lookupMax rows)
        rowsSized = M.map (\m -> (maybe 0 ((+ 1) . fst) (M.lookupMax m), m)) rows
     in (typeCnt, (xSize, rowsSized))

fb1XSize :: FastBels1T -> Int
fb1XSize = fst

fb1RowSize :: FastBels1T -> Int -> Int
fb1RowSize (_, rows) x = maybe 0 fst (M.lookup x rows)

fb1At :: FastBels1T -> Int -> Int -> [BelId]
fb1At (_, rows) x y = maybe [] (M.findWithDefault [] y . snd) (M.lookup x rows)

-- ---------------------------------------------------------------------------
-- Constant context + threaded state
-- ---------------------------------------------------------------------------

-- | Everything that is fixed for the whole refine run.
data SaConst = SaConst
    { scCidOf :: T.Text -> Maybe IdString
    , scAi :: !ArchInfo
    , scDb :: !TimingDb
    , scAuto :: ![IdString]
    -- ^ non-clustered, non-locked cells (refine @autoplaced@)
    , scChain :: ![IdString]
    -- ^ cluster roots (refine @chain_basis@)
    , scFbMap :: !(M.Map IdString (Int, FastBels1T))
    , scNetIdx :: !(M.Map IdString Int)
    -- ^ net name -> udata
    , scNetByIdx :: !(V.Vector IdString)
    -- ^ udata -> net name
    , scClusterCells :: !(M.Map IdString [IdString])
    -- ^ cluster id -> cells (root + children) in cellsIter order
    , scMaxX :: !Int
    , scMaxY :: !Int
    , scIsGlobalNet :: NetInfo BelId WireId PipId -> Bool
    }

data BoundChangeType
    = NoChange
    | CellMovedInwards
    | CellMovedOutwards
    | FullRecompute
    deriving (Eq, Show)

-- | The move scratch (placer1.cc: MoveChangeData).
data MoveChange = MoveChange
    { mcNewBounds :: !(IM.IntMap SaBB)
    -- ^ working bounds overlay (missing = committed value)
    , mcChangedX :: !(IM.IntMap BoundChangeType)
    , mcChangedY :: !(IM.IntMap BoundChangeType)
    , mcNetsX :: ![Int]
    -- ^ bounds_changed_nets_x, push order
    , mcNetsY :: ![Int]
    , mcChangedArcsSet :: !(S.Set (Int, Int))
    -- ^ already_changed_arcs (net, user slot)
    , mcArcs :: ![(Int, Int)]
    -- ^ changed_arcs: (net udata, user slot) in push order
    }

data SaState = SaState
    { ssE :: !Ecp5
    , ssD :: !(Design BelId WireId PipId)
    , ssRng :: !Rng
    , ssNetBounds :: !(IM.IntMap SaBB)
    -- ^ committed bounds (missing = defaultBB)
    , ssNetArc :: !(IM.IntMap (V.Vector Double))
    -- ^ committed arc costs (missing = all zeros)
    , ssCurrWl :: !Int64
    , ssCurrTim :: !Double
    , ssLastWl :: !Int64
    , ssLastTim :: !Double
    , ssDiameter :: !Int
    , ssTemp :: !Float
    , ssAvgWl :: !Int64
    , ssMinWl :: !Int64
    , ssNoProgress :: !Int
    , ssTmg :: !TimingAnalyser
    }

-- | Refine config constants (placer1.cc class members + Placer1Cfg).
lambdaF :: Float
lambdaF = 0.5

critExpF :: Float
critExpF = 8

epsilon :: Double
epsilon = 1.0e-20

refineTemp :: Float
refineTemp = 1.0e-7

{-# NOINLINE p1MoveCntRef #-}
p1MoveCntRef :: IORef Int
p1MoveCntRef = unsafePerformIO (newIORef 0)

{-# NOINLINE p1DumpIterRef #-}
p1DumpIterRef :: IORef Int
p1DumpIterRef = unsafePerformIO (newIORef (-1))

{-# NOINLINE p1RbfcRef #-}
p1RbfcRef :: IORef Int
p1RbfcRef = unsafePerformIO (newIORef 0)
-- ---------------------------------------------------------------------------
-- Net predicates and costs
-- ---------------------------------------------------------------------------

-- | @ignore_net@ (placer1.cc:784): unplaced driver or a global-buffer
-- driver bel.
ignoreNet :: Ecp5 -> Design BelId WireId PipId -> NetInfo BelId WireId PipId -> Bool
ignoreNet e d net =
    case prCell (netDriver net) >>= \dc -> lookupCell dc d of
        Nothing -> True
        Just drvCi -> case cellBel drvCi of
            Nothing -> True
            Just bel -> getBelGlobalBuf e bel

-- | @predictArcDelay@ (context.cc:98): first-bel-pin identity mapping
-- (ECP5 uses the BaseArch default @getBelPinsForCellPin@ == identity).
predictArcDelay1 :: Ecp5 -> Design BelId WireId PipId -> NetInfo BelId WireId PipId -> PortRef -> Int64
predictArcDelay1 e d net user =
    case prCell (netDriver net) >>= \dc -> lookupCell dc d of
        Nothing -> 0
        Just drvCi -> case cellBel drvCi of
            Nothing -> 0
            Just drvBel -> case prCell user >>= \uc -> lookupCell uc d of
                Nothing -> 0
                Just usrCi -> case cellBel usrCi of
                    Nothing -> 0
                    Just sinkBel ->
                        let drvPin = prPort (netDriver net)
                            sinkPin = prPort user
                         in if drvPin == emptyId || sinkPin == emptyId
                                then 0
                                else predictDelay e drvBel drvPin sinkBel sinkPin

-- | @get_timing_cost@ (placer1.cc:838).
getTimingCost :: SaConst -> Ecp5 -> Design BelId WireId PipId -> TimingAnalyser -> NetInfo BelId WireId PipId -> PortRef -> Double
getTimingCost sc e d tmg net user =
    case prCell (netDriver net) >>= \dc -> lookupCell dc d of
        Nothing -> 0.0
        Just drvCi ->
            let (cls, _) = getPortTimingClassAi (scDb sc) (scAi sc) drvCi (prPort (netDriver net))
             in if cls == TmgIgnore
                    then 0.0
                    else
                        let crit = criticalityOf tmg (CellPortKey (fromMaybe emptyId (prCell user)) (prPort user))
                            delay = delayNS (predictArcDelay1 e d net user)
                         in delay * realToFrac (crit ** critExpF)

-- | @getDelayNS@ (ecp5 arch.h:975): @(double)((float)((double)v * 0.001))@ —
-- the C++ returns a @float@, so the double multiply is rounded to float.
delayNS :: Int64 -> Double
delayNS v = realToFrac (realToFrac (fromIntegral v * (0.001 :: Double)) :: Float)

-- | @ctx->rng() \/ float(0x3fffffff)@: the 30-bit draw divided by the
-- float-rounded @0x3fffffff@, promoted back to double for the comparison.
rngAcceptFrac :: Word32 -> Double
rngAcceptFrac w = realToFrac (fromIntegral w / (1073741823.0 :: Float) :: Float)

-- | @get_net_bounds@ (placer1.cc:791).
getNetBounds :: Ecp5 -> Design BelId WireId PipId -> NetInfo BelId WireId PipId -> SaBB
getNetBounds e d net =
    let (dloc, _) =
            case prCell (netDriver net) >>= \dc -> lookupCell dc d of
                Just drvCi -> case cellBel drvCi of
                    Just bel -> let Loc x y _ = getBelLocation e bel in (Loc x y 0, drvCi)
                    Nothing -> error "getNetBounds: unplaced driver"
                Nothing -> error "getNetBounds: null driver"
        step bb u =
            case prCell u >>= \uc -> lookupCell uc d of
                Nothing -> bb
                Just ucCi -> case cellBel ucCi of
                    Nothing -> bb -- !isPseudo && no bel -> skip (no pseudo cells)
                    Just ub ->
                        let Loc ux uy _ = getBelLocation e ub
                            updX bb' =
                                if bbX0 bb' == ux
                                    then bb'{bbNx0 = bbNx0 bb' + 1}
                                    else
                                        if ux < bbX0 bb'
                                            then bb'{bbX0 = ux, bbNx0 = 1}
                                            else bb'
                            updX1 bb' =
                                if bbX1 bb' == ux
                                    then bb'{bbNx1 = bbNx1 bb' + 1}
                                    else
                                        if ux > bbX1 bb'
                                            then bb'{bbX1 = ux, bbNx1 = 1}
                                            else bb'
                            updY bb' =
                                if bbY0 bb' == uy
                                    then bb'{bbNy0 = bbNy0 bb' + 1}
                                    else
                                        if uy < bbY0 bb'
                                            then bb'{bbY0 = uy, bbNy0 = 1}
                                            else bb'
                            updY1 bb' =
                                if bbY1 bb' == uy
                                    then bb'{bbNy1 = bbNy1 bb' + 1}
                                    else
                                        if uy > bbY1 bb'
                                            then bb'{bbY1 = uy, bbNy1 = 1}
                                            else bb'
                         in updY1 (updY (updX1 (updX bb)))
     in foldl' step (SaBB (locX dloc) (locX dloc) (locY dloc) (locY dloc) 1 1 1 1) [u | Just u <- V.toList (netUsers net)]

-- | @setup_costs@ (placer1.cc:852): rebuild committed bounds + arc costs.
setupCosts :: SaConst -> Ecp5 -> Design BelId WireId PipId -> TimingAnalyser -> (IM.IntMap SaBB, IM.IntMap (V.Vector Double))
setupCosts sc e d tmg =
    foldl' step (IM.empty, IM.empty) (netsIter d)
  where
    step (bbMap, arcMap) ni
        | ignoreNet e d ni = (bbMap, arcMap)
        | otherwise =
            let nIdx = scNetIdx sc M.! netName ni
                bb = getNetBounds e d ni
                cap = V.length (netUsers ni)
                arcVec =
                    V.replicate cap 0.0
                        V.// [ (slot, getTimingCost sc e d tmg ni u)
                             | (slot, Just u) <- zip [0 ..] (V.toList (netUsers ni))
                             ]
             in (IM.insert nIdx bb bbMap, IM.insert nIdx arcVec arcMap)

-- | @total_wirelen_cost@ (placer1.cc:866). Integer sum — order-independent.
totalWirelenCost :: IM.IntMap SaBB -> Int64
totalWirelenCost = IM.foldl' (\acc bb -> acc + hpwlBB bb) 0

-- | @total_timing_cost@ (placer1.cc:875): running double accumulation in
-- udata order, each net's arcs added in ascending slot order (the C++
-- @cost += arc_cost@ — NOT per-net sub-sums, which would change rounding).
totalTimingCost :: IM.IntMap (V.Vector Double) -> Double
totalTimingCost arcMap = IM.foldl' (\acc vec -> V.foldl' (+) acc vec) 0.0 arcMap

-- ---------------------------------------------------------------------------
-- Move change machinery
-- ---------------------------------------------------------------------------

-- | @moveChange.reset(this)@ (placer1.cc:921): clear the change lists and
-- flags. The working-bounds overlay is dropped (its fallback is the
-- committed bounds), which is exactly the C++ "restore @new_net_bounds@
-- to @net_bounds@ for previously-changed nets".
emptyMoveChange :: MoveChange
emptyMoveChange = MoveChange IM.empty IM.empty IM.empty [] [] S.empty []

-- | @add_move_cell@ (placer1.cc:943): incremental bounds + arc updates for
-- one cell moving from @oldBel@ to its current bel.
addMoveCell :: SaConst -> Ecp5 -> Design BelId WireId PipId -> IM.IntMap SaBB -> MoveChange -> IdString -> BelId -> MoveChange
addMoveCell sc e d committed mc cell oldBel =
    let ci = M.findWithDefault (error "addMoveCell: missing cell") cell (designCells d)
        (cx, cy) = case cellBel ci of
            Just bel -> let Loc x y _ = getBelLocation e bel in (x, y)
            Nothing -> error "addMoveCell: unplaced cell"
        (ox, oy) = let Loc x y _ = getBelLocation e oldBel in (x, y)
        ports = reverse (cellPortOrder ci)
        _dbgPorts = unsafePerformIO $ do
            want <- lookupEnv "LP_P1_DUMP"
            case want of
                Just _ | unIdString cell == 31233 -> do
                    let ln p = case M.lookup p (cellPorts ci) of
                            Nothing -> show (unIdString p) ++ " net=-1 dir=-"
                            Just pi -> show (unIdString p) ++ " net=" ++ (case portNet pi of Just n -> show (unIdString n); Nothing -> "-1") ++ " dir=" ++ show (portType pi)
                    appendFile "/tmp/hs_p1_ports.txt" (unlines (map ln ports))
                _ -> pure ()
        pushArc nIdx mc' slot =
            if S.member (nIdx, slot) (mcChangedArcsSet mc')
                then mc'
                else mc'{mcChangedArcsSet = S.insert (nIdx, slot) (mcChangedArcsSet mc'), mcArcs = mcArcs mc' ++ [(nIdx, slot)]}
        -- x-axis: x0 then x1 (placer1.cc:959-1018).
        axisX bb flg =
            let (bb1, f1, p1) =
                    if cx < bbX0 bb
                        then (bb{bbX0 = cx, bbNx0 = 1}, fOut flg, flg == NoChange)
                        else
                            if cx == bbX0 bb && ox > bbX0 bb
                                then (bb{bbNx0 = bbNx0 bb + 1}, fOut flg, flg == NoChange)
                                else
                                    if ox == bbX0 bb && cx > bbX0 bb
                                        then
                                            if bbNx0 bb == 1
                                                then (bb, FullRecompute, flg == NoChange)
                                                else (bb{bbNx0 = bbNx0 bb - 1}, fIn flg, flg == NoChange)
                                        else (bb, flg, False)
                (bb2, f2, p2) =
                    if cx > bbX1 bb1
                        then (bb1{bbX1 = cx, bbNx1 = 1}, fOut f1, f1 == NoChange)
                        else
                            if cx == bbX1 bb1 && ox < bbX1 bb1
                                then (bb1{bbNx1 = bbNx1 bb1 + 1}, fOut f1, f1 == NoChange)
                                else
                                    if ox == bbX1 bb1 && cx < bbX1 bb1
                                        then
                                            if bbNx1 bb1 == 1
                                                then (bb1, FullRecompute, f1 == NoChange)
                                                else (bb1{bbNx1 = bbNx1 bb1 - 1}, fIn f1, f1 == NoChange)
                                        else (bb1, f1, False)
             in (bb2, f2, p1 || p2)
        -- y-axis: y0 then y1 (placer1.cc:1020-1073).
        axisY bb flg =
            let (bb1, f1, p1) =
                    if cy < bbY0 bb
                        then (bb{bbY0 = cy, bbNy0 = 1}, fOut flg, flg == NoChange)
                        else
                            if cy == bbY0 bb && oy > bbY0 bb
                                then (bb{bbNy0 = bbNy0 bb + 1}, fOut flg, flg == NoChange)
                                else
                                    if oy == bbY0 bb && cy > bbY0 bb
                                        then
                                            if bbNy0 bb == 1
                                                then (bb, FullRecompute, flg == NoChange)
                                                else (bb{bbNy0 = bbNy0 bb - 1}, fIn flg, flg == NoChange)
                                        else (bb, flg, False)
                (bb2, f2, p2) =
                    if cy > bbY1 bb1
                        then (bb1{bbY1 = cy, bbNy1 = 1}, fOut f1, f1 == NoChange)
                        else
                            if cy == bbY1 bb1 && oy < bbY1 bb1
                                then (bb1{bbNy1 = bbNy1 bb1 + 1}, fOut f1, f1 == NoChange)
                                else
                                    if oy == bbY1 bb1 && cy < bbY1 bb1
                                        then
                                            if bbNy1 bb1 == 1
                                                then (bb1, FullRecompute, f1 == NoChange)
                                                else (bb1{bbNy1 = bbNy1 bb1 - 1}, fIn f1, f1 == NoChange)
                                        else (bb1, f1, False)
             in (bb2, f2, p1 || p2)
        fOut flg = if flg == NoChange then CellMovedOutwards else flg
        fIn flg = if flg == NoChange then CellMovedInwards else flg
        stepPort mc' portName =
            case M.lookup portName (cellPorts ci) of
                Nothing -> mc'
                Just pi -> case portNet pi of
                    Nothing -> mc'
                    Just netId ->
                        if ignoreNet e d (M.findWithDefault (error "addMoveCell: missing net") netId (designNets d))
                            then mc'
                            else
                                let nIdx = scNetIdx sc M.! netId
                                    committedBB = IM.findWithDefault defaultBB nIdx committed
                                    workingBB = IM.findWithDefault committedBB nIdx (mcNewBounds mc')
                                    xFlg = IM.findWithDefault NoChange nIdx (mcChangedX mc')
                                    yFlg = IM.findWithDefault NoChange nIdx (mcChangedY mc')
                                    (wb1, cx1, pushedX) = if xFlg == FullRecompute then (workingBB, xFlg, False) else axisX workingBB xFlg
                                    (wb2, cy1, pushedY) = if yFlg == FullRecompute then (wb1, yFlg, False) else axisY wb1 yFlg
                                    mcX = if pushedX then mc'{mcNetsX = mcNetsX mc' ++ [nIdx]} else mc'
                                    mcY = if pushedY then mcX{mcNetsY = mcNetsY mcX ++ [nIdx]} else mcX
                                    mcB =
                                        mcY
                                            { mcNewBounds = IM.insert nIdx wb2 (mcNewBounds mcY)
                                            , mcChangedX = IM.insert nIdx cx1 (mcChangedX mcY)
                                            , mcChangedY = IM.insert nIdx cy1 (mcChangedY mcY)
                                            }
                                    ni = M.findWithDefault (error "addMoveCell: net") netId (designNets d)
                                    mcA =
                                        if portType pi == PortOut
                                            then
                                                let (cls, _) = getPortTimingClassAi (scDb sc) (scAi sc) ci portName
                                                 in if cls == TmgIgnore
                                                        then mcB
                                                        else foldl' (pushArc nIdx) mcB [slot | (slot, Just _) <- zip [0 ..] (V.toList (netUsers ni))]
                                            else
                                                if portType pi == PortIn
                                                    then pushArc nIdx mcB (portUserIdx pi)
                                                    else mcB
                                 in mcA
     in _dbgPorts `seq` foldl' stepPort mc ports

-- | @compute_cost_changes@ (placer1.cc:1098). Returns the final working
-- bounds overlay, the new arc costs, and the wirelen/timing deltas.
computeCostChanges ::
    SaConst ->
    Ecp5 ->
    Design BelId WireId PipId ->
    TimingAnalyser ->
    IM.IntMap SaBB ->
    IM.IntMap (V.Vector Double) ->
    MoveChange ->
    (IM.IntMap SaBB, [((Int, Int), Double)], Int64, Double)
computeCostChanges sc e d tmg committed committedArcs mc =
    let -- full recompute for x-side FULL_RECOMPUTE nets
        mc1 =
            foldl'
                ( \m nIdx ->
                    if IM.findWithDefault NoChange nIdx (mcChangedX m) == FullRecompute
                        then m{mcNewBounds = IM.insert nIdx (getNetBounds e d (netByUdx sc d nIdx)) (mcNewBounds m)}
                        else m
                )
                mc
                (mcNetsX mc)
        -- full recompute for y-only FULL_RECOMPUTE nets
        mc2 =
            foldl'
                ( \m nIdx ->
                    if IM.findWithDefault NoChange nIdx (mcChangedX m) /= FullRecompute
                        && IM.findWithDefault NoChange nIdx (mcChangedY m) == FullRecompute
                        then m{mcNewBounds = IM.insert nIdx (getNetBounds e d (netByUdx sc d nIdx)) (mcNewBounds m)}
                        else m
                )
                mc1
                (mcNetsY mc1)
        newBMap = mcNewBounds mc2
        wlDeltaX =
            foldl'
                ( \acc nIdx ->
                    let oldB = IM.findWithDefault defaultBB nIdx committed
                        newB = IM.findWithDefault defaultBB nIdx newBMap
                     in acc + (hpwlBB newB - hpwlBB oldB)
                )
                0
                (mcNetsX mc2)
        wlDelta =
            foldl'
                ( \acc nIdx ->
                    if IM.findWithDefault NoChange nIdx (mcChangedX mc2) == NoChange
                        then
                            let oldB = IM.findWithDefault defaultBB nIdx committed
                                newB = IM.findWithDefault defaultBB nIdx newBMap
                             in acc + (hpwlBB newB - hpwlBB oldB)
                        else acc
                )
                wlDeltaX
                (mcNetsY mc2)
        (arcs', tDelta) =
            foldl'
                ( \(arcsAcc, tAcc) (nIdx, slot) ->
                    let net = netByUdx sc d nIdx
                        vec = IM.findWithDefault V.empty nIdx committedArcs
                        oldCost = if slot < V.length vec then vec V.! slot else 0.0
                        newCost =
                            case netUsers net V.!? slot of
                                Just (Just u) -> getTimingCost sc e d tmg net u
                                _ -> 0.0
                     in (((nIdx, slot), newCost) : arcsAcc, tAcc + (newCost - oldCost))
                )
                ([], 0.0)
                (mcArcs mc2)
     in (newBMap, reverse arcs', wlDelta, tDelta)

-- | @commit_cost_changes@ (placer1.cc:1128): fold the working bounds and
-- new arc costs into the committed state and bump the running costs.
commitCostChanges :: SaState -> IM.IntMap SaBB -> [((Int, Int), Double)] -> Int64 -> Double -> SaState
commitCostChanges st overlay newArcCosts wlDelta tDelta =
    let newBounds = IM.union overlay (ssNetBounds st)
        newArc = foldl' (\m ((nIdx, slot), cost) -> IM.adjust (\v -> v V.// [(slot, cost)]) nIdx m) (ssNetArc st) newArcCosts
     in st
            { ssNetBounds = newBounds
            , ssNetArc = newArc
            , ssCurrWl = ssCurrWl st + wlDelta
            , ssCurrTim = ssCurrTim st + tDelta
            }

-- | Resolve a udata index back to its NetInfo.
netByUdx :: SaConst -> Design BelId WireId PipId -> Int -> NetInfo BelId WireId PipId
netByUdx sc d nIdx = M.findWithDefault (error "netByUdx") (scNetByIdx sc V.! nIdx) (designNets d)

-- ---------------------------------------------------------------------------
-- Move generators
-- ---------------------------------------------------------------------------

-- | @random_bel_for_cell@ (placer1.cc:736). Returns a candidate bel and the
-- advanced RNG. @forceZ@ is @Nothing@ (=-1) for autoplaced cells and
-- @Just z@ for chain roots.
randomBelForCell :: SaConst -> Ecp5 -> Design BelId WireId PipId -> Int -> Maybe Int -> IdString -> Rng -> (BelId, Rng)
randomBelForCell sc e d diameter forceZ cell rng0 =
    let ci = M.findWithDefault (error "randomBelForCell: cell") cell (designCells d)
        t = cellType ci
        (typeCnt, fb) = M.findWithDefault (buildFastBels1 e t) t (scFbMap sc)
        Just curBel = cellBel ci
        Loc cx cy _ = getBelLocation e curBel
        dx = diameter
        dy = diameter
        go rng =
            let (nx0, rng1) = rngBounded (2 * dx + 1) rng
                (ny0, rng2) = rngBounded (2 * dy + 1) rng1
                nx = nx0 + max (cx - dx) 0
                ny = ny0 + max (cy - dy) 0
                (nx', ny') = if typeCnt < minBelsForGridPick then (0, 0) else (nx, ny)
             in if nx' >= fb1XSize fb || ny' >= fb1RowSize fb nx'
                    then go rng2
                    else
                        let fbl = fb1At fb nx' ny'
                         in if null fbl
                                then go rng2
                                else
                                    let (bi, rng3) = rngBounded (length fbl) rng2
                                        bel = fbl !! bi
                                        Loc _ _ z = getBelLocation e bel
                                     in case forceZ of
                                            Just fz | z /= fz -> go rng3
                                            _ -> (bel, rng3)
        (result, rngA) = go rng0
     in dbgRbfc sc e cell (cx, cy) typeCnt result rng0 rngA (result, rngA)

-- | Debug: dump the first ~10 @random_bel_for_cell@ calls.
dbgRbfc :: SaConst -> Ecp5 -> IdString -> (Int, Int) -> Int -> BelId -> Rng -> Rng -> a -> a
dbgRbfc _sc e cell (cx, cy) typeCnt tryBel rngB rngA x =
    unsafePerformIO $ do
        want <- lookupEnv "LP_P1_DUMP"
        case want of
            Just _ -> do
                c <- atomicModifyIORef' p1RbfcRef (\n -> (n + 1, n))
                if c < 10
                    then do
                        let Loc tx ty tz = getBelLocation e tryBel
                        appendFile "/tmp/hs_p1_rbfc.txt" (show c ++ " cell " ++ show (unIdString cell) ++ " old " ++ show cx ++ " " ++ show cy ++ " try " ++ show tx ++ " " ++ show ty ++ " " ++ show tz ++ " tcnt " ++ show typeCnt ++ " rngb " ++ printf "%016x" (rngState rngB) ++ " rnga " ++ printf "%016x" (rngState rngA) ++ "\n")
                    else pure ()
            Nothing -> pure ()
        pure x

{-# NOINLINE p1GateRef #-}
p1GateRef :: IORef Int
p1GateRef = unsafePerformIO (newIORef 0)

dbgGate :: IdString -> BelId -> Maybe BelId -> Maybe IdString -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> a -> a
dbgGate cell newBel oldB otherName cellClustered rejectOther isValidCell isValidOther ocClustered ocStrong x =
    unsafePerformIO $ do
        want <- lookupEnv "LP_P1_DUMP"
        case want of
            Just _ -> do
                c <- atomicModifyIORef' p1GateRef (\n -> (n + 1, n))
                if c < 12
                    then appendFile "/tmp/hs_p1_gate.txt" (show c ++ " cell " ++ show (unIdString cell) ++ " other " ++ show otherName ++ " clustered " ++ show cellClustered ++ " rejectOther " ++ show rejectOther ++ " vcell " ++ show isValidCell ++ " vother " ++ show isValidOther ++ " ocClustered " ++ show ocClustered ++ " ocStrong " ++ show ocStrong ++ "\n")
                    else pure ()
            Nothing -> pure ()
        pure x

{-# NOINLINE p1ChainRef #-}
p1ChainRef :: IORef Int
p1ChainRef = unsafePerformIO (newIORef 0)

dbgChain :: Ecp5 -> IdString -> BelId -> a -> a
dbgChain e cell newBase x =
    unsafePerformIO $ do
        want <- lookupEnv "LP_P1_DUMP"
        case want of
            Just _ -> do
                c <- atomicModifyIORef' p1ChainRef (\n -> (n + 1, n))
                if c < 15
                    then do
                        let Loc nx ny nz = getBelLocation e newBase
                        appendFile "/tmp/hs_p1_chain_swap.txt" (show c ++ " cell " ++ show (unIdString cell) ++ " base " ++ show nx ++ " " ++ show ny ++ " " ++ show nz ++ "\n")
                    else pure ()
            Nothing -> pure ()
        pure x

dbgChainMove :: a -> a
{-# NOINLINE dbgChainMove #-}
dbgChainMove x =
    unsafePerformIO $ do
        want <- lookupEnv "LP_P1_DUMP"
        case want of
            Just _ -> appendFile "/tmp/hs_p1_chain_move.txt" "MOVE\n"
            Nothing -> pure ()
        pure x
-- absolute-z coercion) + constr children at their offsets.
clusterPlacement1 :: Ecp5 -> Design BelId WireId PipId -> IdString -> BelId -> Maybe [(IdString, BelId)]
clusterPlacement1 e d cl rootBel =
    case M.lookup cl (designCells d) of
        Nothing -> Nothing
        Just rootCell ->
            let Loc rx ry _ = getBelLocation e rootBel
             in case if cellConstrAbsZ rootCell
                        then case getBelByLocation e (Loc rx ry (cellConstrZ rootCell)) of
                            Just b | isValidBelForCellType e (cellType rootCell) b -> Just (b, getBelLocation e b)
                            _ -> Nothing
                        else Just (rootBel, getBelLocation e rootBel) of
                    Nothing -> Nothing
                    Just (rootBel', Loc rbx rby rbz) ->
                        let children =
                                [ (childName, bel)
                                | childName <- cellConstrChildren rootCell
                                , Just child <- [M.lookup childName (designCells d)]
                                , let lx = rbx + cellConstrX child
                                      ly = rby + cellConstrY child
                                      lz = if cellConstrAbsZ child then cellConstrZ child else rbz + cellConstrZ child
                                , Just bel <- [getBelByLocation e (Loc lx ly lz)]
                                , isValidBelForCellType e (cellType child) bel
                                ]
                         in if length children == length (cellConstrChildren rootCell)
                                then Just ((cellName rootCell, rootBel') : children)
                                else Nothing

-- | @try_swap_position@ (placer1.cc:471). Returns (state, nMoveInc, nAcceptInc).
trySwapPosition :: SaConst -> SaState -> IdString -> BelId -> (SaState, Int, Int)
trySwapPosition sc st cell newBel =
    let e = ssE st
        d = ssD st
        Just ci = lookupCell cell d
        otherName = boundBelCell newBel (ecp5Bind e)
        otherCi = otherName >>= \oc -> lookupCell oc d
        rejectOther = case otherCi of
            Nothing -> False
            Just oc -> cellCluster oc /= emptyId || cellBelStrength oc > StrengthWeak
        cellClustered = cellCluster ci /= emptyId
        oldBelMaybe = cellBel ci
        isValidCell = isValidBelForCellType e (cellType ci) newBel
        isValidOther = maybe True (\ob -> maybe True (\oc -> isValidBelForCellType e (cellType oc) ob) otherCi) oldBelMaybe
        (ocClustered, ocStrong) = case otherCi of
            Just oc -> (cellCluster oc /= emptyId, cellBelStrength oc > StrengthWeak)
            Nothing -> (False, False)
        _dbg = dbgGate cell newBel oldBelMaybe otherName cellClustered rejectOther isValidCell isValidOther ocClustered ocStrong ()
     in _dbg `seq` if cellClustered || rejectOther
            then (st, 0, 0)
            else case cellBel ci of
                Nothing -> (st, 0, 0)
                Just oldB ->
                    if not (isValidBelForCellType e (cellType ci) newBel)
                        then (st, 0, 0)
                        else
                            if maybe False (\oc -> not (isValidBelForCellType e (cellType oc) oldB)) otherCi
                                then (st, 0, 0)
                                else
                                    let (bs1, d1) = unbindBel cell oldB (ecp5Bind e) d
                                        (bs2, d2) = case otherName of
                                            Nothing -> (bs1, d1)
                                            Just oc -> unbindBel oc newBel bs1 d1
                                        (bs3, d3) = bindBel cell newBel StrengthWeak bs2 d2
                                        (bs4, d4) = case otherName of
                                            Nothing -> (bs3, d3)
                                            Just oc -> bindBel oc oldB StrengthWeak bs3 d3
                                        e4 = setEcp5Bind bs4 e
                                        committed = ssNetBounds st
                                        mc1 = addMoveCell sc e4 d4 committed emptyMoveChange cell oldB
                                        mc2 = case otherName of
                                            Nothing -> mc1
                                            Just oc -> addMoveCell sc e4 d4 committed mc1 oc newBel
                                        valid = isBelLocValidE (scAi sc) e4 (scCidOf sc) d4 newBel && isBelLocValidE (scAi sc) e4 (scCidOf sc) d4 oldB
                                     in if not valid
                                            then
                                                -- swap_fail (no n_move, no rng): unbind newBel(cell),
                                                -- unbind oldBel(other), bind oldBel->cell, newBel->other
                                                let (bs5, d5) = unbindBel cell newBel bs4 d4
                                                    (bs6, d6) = case otherName of
                                                        Nothing -> (bs5, d5)
                                                        Just oc -> unbindBel oc oldB bs5 d5
                                                    (bs7, d7) = bindBel cell oldB StrengthWeak bs6 d6
                                                    (bs8, d8) = case otherName of
                                                        Nothing -> (bs7, d7)
                                                        Just oc -> bindBel oc newBel StrengthWeak bs7 d7
                                                 in (st{ssE = setEcp5Bind bs8 e, ssD = d8}, 0, 0)
                                            else
                                                let (overlay, arcCosts, wlDelta, tDelta) = computeCostChanges sc e4 d4 (ssTmg st) committed (ssNetArc st) mc2
                                                    delta =
                                                        realToFrac lambdaF * (tDelta / max (ssLastTim st) epsilon)
                                                            + realToFrac (1.0 - lambdaF) * (fromIntegral wlDelta / max (fromIntegral (ssLastWl st)) epsilon)
                                                    tempOK = realToFrac (ssTemp st) > 1.0e-8
                                                    needRng = delta >= 0 && tempOK
                                                    (rngVal, rng1) = if needRng then rng30 (ssRng st) else (0, ssRng st)
                                                    accept = delta < 0 || (tempOK && rngAcceptFrac rngVal <= exp (-delta / realToFrac (ssTemp st)))
                                                    _dbgMove =
                                                        unsafePerformIO $ do
                                                            want <- lookupEnv "LP_P1_DUMP"
                                                            case want of
                                                                Just _ -> do
                                                                    c <- atomicModifyIORef' p1MoveCntRef (\x -> (x + 1, x))
                                                                    dit <- readIORef p1DumpIterRef
                                                                    if dit == 17 && c < 200
                                                                        then do
                                                                            let Loc ox oy oz = getBelLocation e4 oldB
                                                                                Loc nx ny nz = getBelLocation e4 newBel
                                                                            appendFile "/tmp/hs_p1_move_dump.txt" (show c ++ " cell " ++ show (unIdString cell) ++ " old " ++ show ox ++ " " ++ show oy ++ " " ++ show oz ++ " new " ++ show nx ++ " " ++ show ny ++ " " ++ show nz ++ " wld " ++ show wlDelta ++ " tmd " ++ printf "%.17g" tDelta ++ " d " ++ printf "%.17g" delta ++ "\n")
                                                                            if c == 16
                                                                                then do
                                                                                    let am = ssNetArc st
                                                                                        mkLine (nIdx, slot) =
                                                                                            let vec = IM.findWithDefault V.empty nIdx am
                                                                                                oc = if slot < V.length vec then vec V.! slot else 0.0
                                                                                             in show nIdx ++ " " ++ show slot ++ " old " ++ printf "%.17g" oc
                                                                                        arcLines = unlines (map mkLine (mcArcs mc2))
                                                                                        newLines = unlines ["NEW " ++ show n ++ " " ++ show s ++ " " ++ printf "%.17g" cst | ((n, s), cst) <- arcCosts]
                                                                                    appendFile "/tmp/hs_p1_arcs.txt" ("== move " ++ show c ++ " cell " ++ show (unIdString cell) ++ " ==\n" ++ arcLines ++ newLines)
                                                                                else pure ()
                                                                        else pure ()
                                                                Nothing -> pure ()
                                                 in _dbgMove `seq` if accept
                                                        then (commitCostChanges st{ssE = e4, ssD = d4, ssRng = rng1} overlay arcCosts wlDelta tDelta, 1, 1)
                                                        else
                                                            -- swap_fail: unbind oldBel(other), unbind newBel(cell),
                                                            -- bind oldBel->cell, newBel->other
                                                            let (bs5, d5) = case otherName of
                                                                    Nothing -> (bs4, d4)
                                                                    Just oc -> unbindBel oc oldB bs4 d4
                                                                (bs6, d6) = unbindBel cell newBel bs5 d5
                                                                (bs7, d7) = bindBel cell oldB StrengthWeak bs6 d6
                                                                (bs8, d8) = case otherName of
                                                                    Nothing -> (bs7, d7)
                                                                    Just oc -> bindBel oc newBel StrengthWeak bs7 d7
                                                             in (st{ssE = setEcp5Bind bs8 e, ssD = d8, ssRng = rng1}, 1, 0)

-- | Restore @moved_cells@ to their saved bels (the @swap_fail@ path of
-- @try_swap_chain@, placer1.cc:711-731): unbind everything currently bound,
-- then rebind each saved bel with @STRENGTH_WEAK@ (reverse insertion order).
restoreChain :: Ecp5 -> M.Map IdString BelId -> [IdString] -> (BindState, Design BelId WireId PipId) -> (BindState, Design BelId WireId PipId)
restoreChain e mmap morder (bs, d) =
    let revOrder = reverse morder
        stepUnbind (b', d') name =
            case M.lookup name (designCells d') >>= cellBel of
                Just bel -> unbindBel name bel b' d'
                Nothing -> (b', d')
        stepBind (b', d') name =
            let oldBel = M.findWithDefault (error "restoreChain") name mmap
             in bindBel name oldBel StrengthWeak b' d'
        (bs1, d1) = foldl' stepUnbind (bs, d) revOrder
     in foldl' stepBind (bs1, d1) revOrder

-- | @try_swap_chain@ (placer1.cc:592).
trySwapChain :: SaConst -> SaState -> IdString -> BelId -> (SaState, Int, Int)
trySwapChain sc st0 cell newBase =
    dbgChain e cell newBase () `seq`
    case processQueue (ecp5Bind e) d M.empty [] [(cl, newBase)] of
            Left (bsF, dF, mmapF, morderF) ->
                let (bsR, dR) = restoreChain e mmapF morderF (bsF, dF)
                 in (st0{ssE = setEcp5Bind bsR e, ssD = dR}, 0, 0)
            Right (bsF, dF, mmapF, morderF) ->
                let eF = setEcp5Bind bsF e
                 in case addMoves eF dF committed mmapF emptyMoveChange (reverse morderF) of
                        Nothing ->
                            let (bsR, dR) = restoreChain e mmapF morderF (bsF, dF)
                             in (st0{ssE = setEcp5Bind bsR e, ssD = dR}, 0, 0)
                        Just mcFinal ->
                            let (overlay, arcCosts, wlDelta, tDelta) = computeCostChanges sc eF dF (ssTmg st0) committed (ssNetArc st0) mcFinal
                                delta =
                                    realToFrac lambdaF * (tDelta / ssLastTim st0)
                                        + realToFrac (1.0 - lambdaF) * (fromIntegral wlDelta / fromIntegral (ssLastWl st0))
                                tempOK = realToFrac (ssTemp st0) > 1.0e-8
                                needRng = delta >= 0 && tempOK
                                (rngVal, rng1) = if needRng then rng30 (ssRng st0) else (0, ssRng st0)
                                accept = delta < 0 || (tempOK && rngAcceptFrac rngVal <= exp (-delta / realToFrac (ssTemp st0)))
                             in dbgChainMove () `seq` if accept
                                    then (commitCostChanges st0{ssE = eF, ssD = dF, ssRng = rng1} overlay arcCosts wlDelta tDelta, 1, 1)
                                    else
                                        let (bsR, dR) = restoreChain e mmapF morderF (bsF, dF)
                                         in (st0{ssE = setEcp5Bind bsR e, ssD = dR, ssRng = rng1}, 1, 0)
  where
    e = ssE st0
    d = ssD st0
    committed = ssNetBounds st0
    ci = M.findWithDefault (error "trySwapChain: cell") cell (designCells d)
    cl = cellCluster ci
    belAvail bs b = boundBelCell b bs == Nothing

    processQueue bs d mmap morder [] = Right (bs, d, mmap, morder)
    processQueue bs d mmap morder ((ccl, newRoot) : rest) =
        case clusterPlacement1 e d ccl newRoot of
            Nothing -> Left (bs, d, mmap, morder)
            Just destBels ->
                let -- first loop: unbind dest cells, record old bels
                    (bs1, d1, mmap1, morder1) = foldl' unbindDest (bs, d, mmap, morder) destBels
                 in case handleDests (bs1, d1, mmap1, morder1, rest) destBels of
                        Left (bs2, d2, mmap2, morder2) -> Left (bs2, d2, mmap2, morder2)
                        Right (bs2, d2, mmap2, morder2, queue') -> processQueue bs2 d2 mmap2 morder2 queue'

    unbindDest (b', d', m', o') (cname, _) =
        case M.lookup cname (designCells d') >>= cellBel of
            Just oldB ->
                let (b'', d'') = unbindBel cname oldB b' d'
                 in (b'', d'', M.insert cname oldB m', o' ++ [cname])
            Nothing -> (b', d', m', o')

    handleDests acc [] = Right acc
    handleDests (b', d', m', o', q') ((dbName, dbBel) : rest) =
        let boundName = boundBelCell dbBel b'
            oldBel = M.findWithDefault (error "handleDests: db") dbName m'
         in if (not (belAvail b' oldBel)) && boundName /= Nothing
                then Left (b', d', m', o')
                else case boundName of
                    Nothing ->
                        if not (belAvail b' dbBel)
                            then Left (b', d', m', o')
                            else
                                let (b'', d'') = bindBel dbName dbBel StrengthWeak b' d'
                                 in handleDests (b'', d'', m', o', q') rest
                    Just bn ->
                        if M.member bn m'
                            then Left (b', d', m', o')
                            else
                                let bCi = M.findWithDefault (error "handleDests: bcell") bn (designCells d')
                                 in if cellBelStrength bCi > StrengthStrong
                                        then Left (b', d', m', o')
                                        else
                                            if cellCluster bCi /= emptyId
                                                then
                                                    let Just bBel = cellBel bCi
                                                        oldLoc = getBelLocation e oldBel
                                                        boundLoc = getBelLocation e bBel
                                                        rootName = cellCluster bCi
                                                        rootBel = cellBel (M.findWithDefault (error "handleDests: root") rootName (designCells d'))
                                                        rootLoc = case rootBel of
                                                            Just rb -> getBelLocation e rb
                                                            Nothing -> oldLoc
                                                        newLoc =
                                                            Loc
                                                                (locX oldLoc + (locX rootLoc - locX boundLoc))
                                                                (locY oldLoc + (locY rootLoc - locY boundLoc))
                                                                (locZ oldLoc + (locZ rootLoc - locZ boundLoc))
                                                     in if locX newLoc < 0 || locX newLoc >= getGridDimX e || locY newLoc < 0 || locY newLoc >= getGridDimY e
                                                            then Left (b', d', m', o')
                                                            else case getBelByLocation e newLoc of
                                                                Nothing -> Left (b', d', m', o')
                                                                Just newRootBel ->
                                                                    let clCells = M.findWithDefault [] rootName (scClusterCells sc)
                                                                        (b2, d2, m2, o2) = foldl' unbindDest (b', d', m', o') [(cn, M.findWithDefault (error "cl") cn m') | cn <- clCells]
                                                                        (b3, d3) = bindBel dbName dbBel StrengthWeak b2 d2
                                                                     in handleDests (b3, d3, m2, o2, q' ++ [(rootName, newRootBel)]) rest
                                                else
                                                    let Just bBel = cellBel bCi
                                                        (b2, d2) = unbindBel bn bBel b' d'
                                                        (b3, d3) = bindBel bn oldBel StrengthWeak b2 d2
                                                        m2 = M.insert bn bBel m'
                                                        o2 = o' ++ [bn]
                                                        (b4, d4) = bindBel dbName dbBel StrengthWeak b3 d3
                                                     in handleDests (b4, d4, m2, o2, q') rest

    addMoves _eF _dF _committed _mmapF mc [] = Just mc
    addMoves eF dF committed mmapF mc (name : rest) =
        let oldB = M.findWithDefault (error "addMoves: oldbel") name mmapF
            mc' = addMoveCell sc eF dF committed mc name oldB
            ci = M.findWithDefault (error "addMoves: cell") name (designCells dF)
         in case cellBel ci of
                Just bel ->
                    if isBelLocValidE (scAi sc) eF (scCidOf sc) dF bel
                        then addMoves eF dF committed mmapF mc' rest
                        else Nothing
                Nothing -> Nothing

-- ---------------------------------------------------------------------------
-- Main refine loop
-- ---------------------------------------------------------------------------

-- | @place(true)@ (placer1.cc:137 with refine=true): the SA loop at fixed
-- low temperature.
place1Refine :: Ecp5 -> (T.Text -> Maybe IdString) -> Design BelId WireId PipId -> Rng -> (Ecp5, Design BelId WireId PipId, Rng)
place1Refine e cidOf d0 rng0 =
    let db = ecp5TimingDb e
        ai = assignArchInfo cidOf d0
        isGlobNet ni = maybe False (const True) (M.lookup (fromMaybe emptyId (cidOf "ECP5_IS_GLOBAL")) (netAttrs ni))
        (maxX, maxY) = foldl' (\(mx, my) b -> let Loc x y _ = getBelLocation e b in (max mx x, max my y)) (0, 0) (getBels e)
        cells = cellsIter d0
        classify (aRev, cRev) ci
            | cellBelStrength ci > StrengthStrong = (aRev, cRev)
            | cellCluster ci /= emptyId =
                if cellCluster ci == cellName ci then (aRev, cellName ci : cRev) else (aRev, cRev)
            | otherwise = (cellName ci : aRev, cRev)
        (aRev, cRev) = foldl' classify ([], []) cells
        autoplaced = reverse aRev
        chainBasis = reverse cRev
        _dbgAuto =
            unsafePerformIO $ do
                want <- lookupEnv "LP_P1_DUMP"
                case want of
                    Just _ -> do
                        writeFile "/tmp/hs_p1_auto.txt" (unlines (map (show . unIdString) autoplaced))
                        writeFile "/tmp/hs_p1_chain.txt" (unlines (map (show . unIdString) chainBasis))
                    Nothing -> pure ()
        netNames = map netName (netsIter d0)
        netIdx = M.fromList (zip netNames [0 ..])
        netByIdx = V.fromList netNames
        fbTypes = foldl' (\s ci -> S.insert (cellType ci) s) S.empty cells
        fbMap = M.fromList [(t, buildFastBels1 e t) | t <- S.toList fbTypes]
        clusterCells =
            foldl'
                (\m ci -> if cellCluster ci /= emptyId then M.insertWith (flip (++)) (cellCluster ci) [cellName ci] m else m)
                M.empty
                cells
        sc =
            SaConst
                { scCidOf = cidOf
                , scAi = ai
                , scDb = db
                , scAuto = autoplaced
                , scChain = chainBasis
                , scFbMap = fbMap
                , scNetIdx = netIdx
                , scNetByIdx = netByIdx
                , scClusterCells = clusterCells
                , scMaxX = maxX
                , scMaxY = maxY
                , scIsGlobalNet = isGlobNet
                }
        tmg0 = buildTimingAnalyser e (getPortTimingClassAi db ai) (getPortClockingInfoAi db ai) (getCellDelayAi db ai) isGlobNet True d0
        (bbMap0, arcMap0) = setupCosts sc e d0 tmg0
        currWl0 = totalWirelenCost bbMap0
        currTim0 = totalTimingCost arcMap0
        st0 =
            SaState
                { ssE = e
                , ssD = d0
                , ssRng = rng0
                , ssNetBounds = bbMap0
                , ssNetArc = arcMap0
                , ssCurrWl = currWl0
                , ssCurrTim = currTim0
                , ssLastWl = currWl0
                , ssLastTim = currTim0
                , ssDiameter = 3
                , ssTemp = refineTemp
                , ssAvgWl = currWl0
                , ssMinWl = currWl0
                , ssNoProgress = 0
                , ssTmg = tmg0
                }
        _dbgInit =
            unsafePerformIO $ do
                want <- lookupEnv "LP_P1_DUMP"
                case want of
                    Just _ -> do
                        writeFile "/tmp/hs_p1_bounds.txt" (unlines [show nIdx ++ " " ++ show (bbX0 bb) ++ " " ++ show (bbX1 bb) ++ " " ++ show (bbY0 bb) ++ " " ++ show (bbY1 bb) ++ " " ++ show (hpwlBB bb) | (nIdx, bb) <- IM.toAscList bbMap0])
                        writeFile "/tmp/hs_p1_tot.txt" (show currWl0 ++ " " ++ show currTim0 ++ " nnets=" ++ show (length (netsIter d0)) ++ "\n")
                    Nothing -> pure ()
     in _dbgInit `seq` _dbgAuto `seq` goIter sc 1 st0

-- | One full sweep: every autoplaced cell then every chain root.
sweep :: SaConst -> SaState -> (SaState, Int, Int)
sweep sc st0 =
    let (st1, nM1, nA1) = foldl' (\acc c -> moveCell sc acc Nothing c) (st0, 0, 0) (scAuto sc)
        (st2, nM2, nA2) = foldl' (\acc c -> moveCell sc acc (Just 0) c) (st1, 0, 0) (scChain sc)
     in (st2, nM1 + nM2, nA1 + nA2)
  where
    moveCell sc acc forceZ cell =
        let (st, nM, nA) = acc
            e = ssE st
            Just ci = lookupCell cell (ssD st)
            diameter = ssDiameter st
            fz = case forceZ of
                Nothing -> Nothing
                Just _ -> let Just cb = cellBel ci in let Loc _ _ z = getBelLocation e cb in Just z
            (tryBel, rng') = randomBelForCell sc e (ssD st) diameter fz cell (ssRng st)
            stR = st{ssRng = rng'}
         in case forceZ of
                Nothing ->
                    if cellBel ci == Just tryBel
                        then (stR, nM, nA)
                        else
                            let (st2, nM', nA') = trySwapPosition sc stR cell tryBel
                             in (st2, nM + nM', nA + nA')
                Just _ ->
                    if cellBel ci == Just tryBel
                        then (stR, nM, nA)
                        else
                            let (st2, nM', nA') = trySwapChain sc stR cell tryBel
                             in (st2, nM + nM', nA + nA')
-- | The main loop (placer1.cc:261-389).
goIter :: SaConst -> Int -> SaState -> (Ecp5, Design BelId WireId PipId, Rng)
goIter sc iter st =
    let e = ssE st
        _dbgIT =
            unsafePerformIO $ do
                want <- lookupEnv "LP_P1_DUMP"
                case want of
                    Just _ -> do
                        writeIORef p1DumpIterRef iter
                        if iter == 17 then writeIORef p1MoveCntRef 0 else pure ()
                        appendFile "/tmp/hs_p1_iter.txt" ("IT " ++ show iter ++ " wl " ++ show (ssCurrWl st) ++ " tim " ++ printf "%.17g" (ssCurrTim st) ++ " temp " ++ printf "%.17g" (realToFrac (ssTemp st) :: Double) ++ " avg " ++ show (ssAvgWl st) ++ " min " ++ show (ssMinWl st) ++ " nop " ++ show (ssNoProgress st) ++ " rng " ++ printf "%016x" (rngState (ssRng st)) ++ "\n")
                    Nothing -> pure ()
        (st1, nMoves, nAccepts) = foldl' (\acc@(stA, nMA, nAA) _ -> let (stB, nMB, nAB) = sweep sc stA in (stB, nMA + nMB, nAA + nAB)) (st, 0, 0) [1 .. 15 :: Int]
        _dbgMV =
            unsafePerformIO $ do
                want <- lookupEnv "LP_P1_DUMP"
                case want of
                    Just _ -> appendFile "/tmp/hs_p1_moves.txt" ("MV " ++ show iter ++ " nmove " ++ show nMoves ++ " naccept " ++ show nAccepts ++ "\n")
                    Nothing -> pure ()
        improved = ssCurrWl st1 < ssMinWl st
        minWl' = if improved then ssCurrWl st1 else ssMinWl st
        noProgress' = if improved then 0 else ssNoProgress st + 1
     in _dbgIT `seq` _dbgMV `seq` if realToFrac (ssTemp st1) <= 1.0e-7 && noProgress' >= 1
            then
                let _dbg = unsafePerformIO (hPutStrLn stderr ("  at iteration #" ++ show iter ++ ": temp = 0.000000, timing cost = " ++ printf "%.0f" (ssCurrTim st1) ++ ", wirelen = " ++ printf "%.0f" (fromIntegral (ssCurrWl st1) :: Double) ++ " "))
                 in st1 `seq` _dbg `seq` (ssE st1, ssD st1, ssRng st1)
            else
                let racc = fromIntegral nAccepts / fromIntegral nMoves :: Double
                    m = max (scMaxX sc) (scMaxY sc) + 1
                    avgUpdate = fromIntegral (ssCurrWl st1) < (0.95 :: Double) * fromIntegral (ssAvgWl st) && ssCurrWl st1 > 0
                    st2 =
                        if avgUpdate
                            then st1{ssAvgWl = truncate ((0.8 :: Double) * fromIntegral (ssAvgWl st) + (0.2 :: Double) * fromIntegral (ssCurrWl st1))}
                            else
                                let diamNext = fromIntegral (ssDiameter st1) * ((1.0 :: Double) - 0.44 + racc)
                                    diameter' = max 1 (min m (truncate (diamNext + 0.5)))
                                    temp' =
                                        if racc > 0.96
                                            then tempMul 0.5 (ssTemp st1)
                                            else
                                                if racc > 0.8
                                                    then tempMul 0.9 (ssTemp st1)
                                                    else
                                                        if racc > 0.15 && diameter' > 1
                                                            then tempMul 0.95 (ssTemp st1)
                                                            else tempMul 0.8 (ssTemp st1)
                                 in st1{ssDiameter = diameter', ssTemp = temp'}
                    st2' = st2{ssMinWl = minWl', ssNoProgress = noProgress'}
                    tmg' = runTimingAnalyser e (scIsGlobalNet sc) True (ssTmg st2') (ssD st2')
                    _dbgCrit17 =
                        unsafePerformIO $ do
                            want <- lookupEnv "LP_P1_DUMP"
                            case want of
                                Just _ | iter == 17 ->
                                    writeFile "/tmp/hs_crit17.txt" (unlines [show (unIdString c) ++ " " ++ show (unIdString p) ++ " " ++ printf "%08x" (castFloatToWord32 (ppWorstCrit pp)) | (CellPortKey c p, pp) <- M.toAscList (taPorts tmg')])
                                _ -> pure ()
                    _dbgPort17 =
                        unsafePerformIO $ do
                            want <- lookupEnv "LP_P1_DUMP"
                            case want of
                                Just _ | iter == 17 -> do
                                    let dumpOne (c, p) = case M.lookup (CellPortKey (IdString c) (IdString p)) (taPorts tmg') of
                                            Nothing -> ""
                                            Just pp ->
                                                let route = ppRouteDelay pp
                                                    arrs = ["arr[" ++ show d ++ "]=" ++ show (dpMin (artValue a)) ++ "/" ++ show (dpMax (artValue a)) ++ " path=" ++ show (artPathLength a) | (d, a) <- M.toAscList (ppArrival pp)]
                                                    reqs = ["req[" ++ show d ++ "]=" ++ show (dpMin (artValue r)) ++ "/" ++ show (dpMax (artValue r)) ++ " path=" ++ show (artPathLength r) | (d, r) <- M.toAscList (ppRequired pp)]
                                                    dps = ["dp[" ++ show d ++ "] slack=" ++ show (pdpSetupSlack pd) | (d, pd) <- M.toAscList (ppDomainPairs pp)]
                                                 in unlines (("cell=" ++ show c ++ " port=" ++ show p) : ("route=" ++ show (dpMin route) ++ "/" ++ show (dpMax route)) : arrs ++ reqs ++ dps)
                                        ls = concatMap dumpOne [(19593, 1218), (19932, 1316), (20600, 16), (20600, 1675), (20600, 1717)]
                                    writeFile "/tmp/hs_port_dump.txt" ls
                                _ -> pure ()
                    (bbMap', arcMap') = setupCosts sc e (ssD st2') tmg'
                    currWl' = totalWirelenCost bbMap'
                    currTim' = totalTimingCost arcMap'
                    st3 =
                        st2'
                            { ssTmg = tmg'
                            , ssNetBounds = bbMap'
                            , ssNetArc = arcMap'
                            , ssCurrWl = currWl'
                            , ssCurrTim = currTim'
                            , ssLastWl = currWl'
                            , ssLastTim = currTim'
                            }
                    _dbg =
                        if iter `mod` 5 == 0 || iter == 1
                            then unsafePerformIO (hPutStrLn stderr ("  at iteration #" ++ show iter ++ ": temp = " ++ printf "%f" (ssTemp st3) ++ ", timing cost = " ++ printf "%.0f" (ssCurrTim st3) ++ ", wirelen = " ++ printf "%.0f" (fromIntegral (ssCurrWl st3) :: Double)))
                            else ()
                 in _dbgCrit17 `seq` _dbgPort17 `seq` st3 `seq` _dbg `seq` goIter sc (iter + 1) st3

-- | @float(double(temp) * c)@ — mirrors the C++ @temp *= c@ (float promoted
-- to double for the multiply, then rounded back to float).
tempMul :: Double -> Float -> Float
tempMul c t = realToFrac (realToFrac t * c)
