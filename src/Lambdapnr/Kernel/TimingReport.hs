{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

{- | The timing report — a port of the report half of
@common\/kernel\/timing.cc@ (@build_critical_path_report@,
@build_crit_path_reports@, @build_slack_histogram_report@,
@get_min_delay_violations@, @walk_crit_path@, @get_worst_eps@,
@max_delay_by_domain_pairs@) plus the printers in
@common\/kernel\/timing_log.cc@ (@log_crit_paths@, @log_fmax@,
@log_histogram@, @log_timing_results@) and the post-pack
@print_utilisation@ of @design_utils.cc@.

The analyser proper lives in "Lambdapnr.Kernel.TimingAnalyser"; this
module adds the @TimingResult@ layer: per-clock critical paths, Fmax,
cross-domain max delays and the slack histogram, byte-comparable with
the C++ log output.
-}
module Lambdapnr.Kernel.TimingReport (
    ClockEvent (..),
    ClockPair (..),
    SegmentType (..),
    Segment (..),
    CriticalPath (..),
    TimingResult (..),
    timingAnalysisReport,
    logTimingResults,
    printUtilisation,
) where

import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Text.Printf (printf)
import System.IO (hPutStrLn, stderr)

import Lambdapnr.Kernel.Arch (Arch (..), Bel, Loc (..), Pip, Wire)
import Lambdapnr.Kernel.Delay (DelayPair (..), DelayQuad (..), DelayT, dqMaxDelay, dqMinDelay)
import Lambdapnr.Kernel.IdString (IdString, IdTable, emptyId, idByName, idToText, intern)
import Lambdapnr.Kernel.Netlist (CellInfo, Design (..), NetInfo (..), PipMap (..), PortDir (..), PortInfo (..), PortRef (..), cellBel, cellPorts, cellType, portNet)
import Lambdapnr.Kernel.Timing (ClockEdge (..), TimingClockingInfo (..), TimingPortClass (..))
import Lambdapnr.Kernel.Property (Property, propAsString, propIsString)
import Lambdapnr.Kernel.TimingAnalyser (
    ArrivReqTime (..),
    CellArc (..),
    CellArcType (..),
    CellPortKey (..),
    PerDomain (..),
    PerDomainPair (..),
    PerPort (..),
    PortDomainPairData (..),
    TimingAnalyser (..),
    buildTimingAnalyserRaw,
    runTimingAnalyserBy,
 )


-- | A clock event (clock net + edge), mirroring @ClockEvent@.
data ClockEvent = ClockEvent
    { ceClock :: !IdString
    , ceEdge :: !ClockEdge
    }
    deriving (Eq, Ord, Show)

data ClockPair = ClockPair
    { cpStart :: !ClockEvent
    , cpEnd :: !ClockEvent
    }
    deriving (Eq, Ord, Show)

-- | @CriticalPath::Segment::Type@.
data SegmentType
    = SegClkToClk -- ^ Clock to clock delay
    | SegClkSkew -- ^ Clock skew
    | SegClkToQ -- ^ Clock-to-Q delay
    | SegSource -- ^ Delayless source
    | SegLogic -- ^ Combinational logic delay
    | SegRouting -- ^ Routing delay
    | SegSetup -- ^ Setup time in sink
    | SegHold -- ^ Hold time in sink
    deriving (Eq, Ord, Show)

segmentTypeStr :: SegmentType -> String
segmentTypeStr SegClkToClk = "clk-to-clk"
segmentTypeStr SegClkSkew = "clk-skew"
segmentTypeStr SegClkToQ = "clk-to-q"
segmentTypeStr SegSource = "source"
segmentTypeStr SegLogic = "logic"
segmentTypeStr SegRouting = "routing"
segmentTypeStr SegSetup = "setup"
segmentTypeStr SegHold = "hold"

data Segment = Segment
    { segType :: !SegmentType
    , segNet :: !IdString -- ^ Net name (routing only)
    , segFrom :: !(IdString, IdString)
    , segTo :: !(IdString, IdString)
    , segDelay :: !DelayT
    }
    deriving (Show)

data CriticalPath = CriticalPath
    { pathClockPair :: !ClockPair
    , pathMaxDelay :: !DelayT
    , pathSegments :: ![Segment]
    }
    deriving (Show)

data TimingResult = TimingResult
    { trClockFmax :: !(M.Map IdString (Double, Double))
    , -- | Single-domain critical paths, insertion order (the C++ dict
      -- iterates in reverse insertion order; the printers reverse).
      trClockPaths :: ![(IdString, CriticalPath)]
    , trXclockPaths :: ![CriticalPath]
    , trEmptyPaths :: ![IdString]
    , trSlackHistogram :: !(M.Map Int Int)
    , trMinDelayViolations :: ![CriticalPath]
    }
    deriving (Show)

-- ---------------------------------------------------------------------
-- Route delays (Context::getNetinfoRouteDelay, the ECP5 flavour)
-- ---------------------------------------------------------------------

-- | @getNetinfoSourceWire@: ECP5 has no pseudo cells, so this is the
-- driver bel's pin wire for the driver port.
netinfoSourceWire :: Arch a => a -> M.Map IdString (CellInfo (Bel a) (Wire a) (Pip a)) -> NetInfo (Bel a) (Wire a) (Pip a) -> Maybe (Wire a)
netinfoSourceWire arch cells ni = do
    drvCell <- prCell (netDriver ni)
    srcBel <- cellBel =<< M.lookup drvCell cells
    getBelPinWire arch srcBel (prPort (netDriver ni))

-- | @getNetinfoSinkWires@: the sink bel's pin wire (one pin for ECP5).
netinfoSinkWires :: Arch a => a -> M.Map IdString (CellInfo (Bel a) (Wire a) (Pip a)) -> PortRef -> [Maybe (Wire a)]
netinfoSinkWires arch cells u = do
    c <- maybeToListId (prCell u)
    dstBel <- maybeToListId (cellBel =<< M.lookup c cells)
    pure (getBelPinWire arch dstBel (prPort u))
  where
    maybeToListId Nothing = []
    maybeToListId (Just x) = [x]

-- | @predictArcDelay@: the placement-oracle prediction for an (unrouted)
-- arc, via the arch @predictDelay@.
predictArcDelay :: Arch a => a -> M.Map IdString (CellInfo (Bel a) (Wire a) (Pip a)) -> NetInfo (Bel a) (Wire a) (Pip a) -> PortRef -> DelayT
predictArcDelay arch cells ni u =
    case (prCell (netDriver ni) >>= \c -> M.lookup c cells >>= cellBel, prCell u >>= \c -> M.lookup c cells >>= cellBel) of
        (Just srcBel, Just dstBel) -> predictDelay arch srcBel (prPort (netDriver ni)) dstBel (prPort u)
        _ -> 0

-- | @Context::getNetinfoRouteDelay@: the real routed delay for ECP5
-- (0 for globals; walk the bound pips from each sink wire back to the
-- source wire, falling back to @predictArcDelay@ when unrouted).
netinfoRouteDelay ::
    (Arch a, Ord (Wire a)) =>
    a ->
    (NetInfo (Bel a) (Wire a) (Pip a) -> Bool) ->
    M.Map IdString (CellInfo (Bel a) (Wire a) (Pip a)) ->
    M.Map IdString (NetInfo (Bel a) (Wire a) (Pip a)) ->
    NetInfo (Bel a) (Wire a) (Pip a) ->
    PortRef ->
    DelayT
netinfoRouteDelay arch isGlobalNet cells nets ni u
    | isGlobalNet ni = 0
    | M.null (netWires ni) = predictArcDelay arch cells ni u
    | otherwise =
        case netinfoSourceWire arch cells ni of
            Nothing -> 0
            Just srcWire ->
                foldl (\acc sw -> max acc (walkSink sw)) 0 (netinfoSinkWires arch cells u)
              where
                walkSink Nothing = predictArcDelay arch cells ni u
                walkSink (Just dstWire) =
                    let (reached, delay) = go dstWire 0
                     in if reached
                            then delay + dqMaxDelay (getWireDelay arch srcWire)
                            else predictArcDelay arch cells ni u
                go cursor acc
                    | cursor == srcWire = (True, acc)
                    | otherwise =
                        case M.lookup cursor (netWires ni) of
                            Nothing -> (False, acc)
                            Just pm -> case pmPip pm of
                                Nothing -> (False, acc)
                                Just pip ->
                                    let acc' = acc + dqMaxDelay (getPipDelay arch pip) + dqMaxDelay (getWireDelay arch cursor)
                                     in go (getPipSrcWire arch pip) acc'

-- ---------------------------------------------------------------------
-- Report builders
-- ---------------------------------------------------------------------

cellAt :: M.Map IdString (CellInfo bel wire pip) -> IdString -> CellInfo bel wire pip
cellAt cells c = fromMaybe (error "timing report: missing cell") (M.lookup c cells)

cpkCell :: CellPortKey -> IdString
cpkCell (CellPortKey c _) = c

portAt :: TimingAnalyser -> CellPortKey -> PerPort
portAt tmg key = fromMaybe (error "timing report: missing port") (M.lookup key (taPorts tmg))

pairIdOf :: TimingAnalyser -> Int -> Int -> Int
pairIdOf tmg launchId captureId =
    fromMaybe (error "timing report: missing domain pair") $
        fst <$> foldl' (\acc (i, p) -> if pdpLaunch p == launchId && pdpCapture p == captureId then Just (i, p) else acc) Nothing (zip [0 ..] (taDomainPairs tmg))

-- | @walk_crit_path@: walk the min/max path backwards from the endpoint
-- to the startpoint, returning the reversed list of input ports.
walkCritPath ::
    Arch a =>
    a ->
    (CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> (TimingPortClass, Int)) ->
    M.Map IdString (CellInfo (Bel a) (Wire a) (Pip a)) ->
    TimingAnalyser ->
    Int ->
    CellPortKey ->
    Bool ->
    [PortRef]
walkCritPath arch ptc cells tmg pairId endpoint longest =
    go endpoint S.empty []
  where
    launchId = pdpLaunch (taDomainPairs tmg !! pairId)
    go cursor visited acc
        | S.member cursor visited = acc -- combinational loop
        | otherwise =
            let visited' = S.insert cursor visited
                (cursorCell, cursorPort) = case cursor of
                    CellPortKey c p -> (c, p)
                ci = cellAt cells cursorCell
                ptype = portType (cellPorts ci M.! cursorPort)
                (cls, _) = ptc ci cursorPort
                isInput = cls /= TmgClockInput && cls /= TmgIgnore && ptype == PortIn
                acc' = if isInput then acc ++ [PortRef (Just cursorCell) cursorPort] else acc
             in case M.lookup launchId (ppArrival (portAt tmg cursor)) of
                    Nothing -> acc'
                    Just art ->
                        let cursor' = if longest then artBwdMax art else artBwdMin art
                         in if cls == TmgStartpoint
                                then acc'
                                else go cursor' visited' acc'

-- | @get_worst_eps@: the @count@ endpoints with the worst (lowest)
-- setup slack for a domain pair, strictly increasing.
getWorstEps :: TimingAnalyser -> Int -> Int -> [CellPortKey]
getWorstEps tmg pairId count =
    let capD = taDomains tmg !! pdpCapture (taDomainPairs tmg !! pairId)
        eps = pdEndpoints capD
        slackOf (ep, _) =
            case M.lookup pairId (ppDomainPairs (portAt tmg ep)) of
                Just pdp -> Just (pdpSetupSlack pdp)
                Nothing -> Nothing
        go n lastSlack acc
            | n >= count = acc
            | otherwise =
                case foldl' step Nothing eps of
                    Nothing -> acc
                    Just (e, s) -> go (n + 1) s (acc ++ [e])
          where
            step best ep =
                case slackOf ep of
                    Nothing -> best
                    Just sl
                        | sl <= lastSlack -> best
                        | otherwise -> case best of
                            Nothing -> Just (fst ep, sl)
                            Just (_, bsl) -> if sl < bsl then Just (fst ep, sl) else best
     in go 0 minBound []

-- | @max_delay_by_domain_pairs@ — note the C++ reads @domains.at(capture_id)@
-- for the launch domain (a release quirk); replicated exactly.
maxDelayByDomainPairs ::
    Arch a =>
    a ->
    (CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> (TimingPortClass, Int)) ->
    M.Map IdString (CellInfo (Bel a) (Wire a) (Pip a)) ->
    M.Map IdString (NetInfo (Bel a) (Wire a) (Pip a)) ->
    TimingAnalyser ->
    M.Map Int DelayT
maxDelayByDomainPairs arch ptc cells nets tmg =
    foldl' capStep M.empty [0 .. length (taDomains tmg) - 1]
  where
    capStep acc captureId =
        let capture = taDomains tmg !! captureId
         in foldl' (\a (ep, _) -> epStep a captureId ep) acc (pdEndpoints capture)
    epStep acc captureId ep =
        let pd = portAt tmg ep
            req = ppRequired pd M.! captureId
         in foldl' (\a (launchId, arr) -> arrStep a captureId ep pd req launchId arr) acc (M.toList (ppArrival pd))
    arrStep acc captureId ep pd req launchId arr =
        let epCellN = cpkCell ep
            launch = taDomains tmg !! captureId -- C++ quirk: capture_id again
            capture = taDomains tmg !! captureId
            dp = pairIdOf tmg launchId captureId
            clocks = (pdClock launch, pdClock capture)
            sameClock = captureId == launchId
            related = M.member clocks (taClockDelays tmg)
            c2c = if related then taClockDelays tmg M.! clocks else 0
            delay0 = dpMax (artValue arr) - dpMin (artValue req) + c2c
            delay1
                | not sameClock && not related =
                    delay0
                        + sum
                            [ dpMin (ppRouteDelay (portAt tmg (CellPortKey epCellN other)))
                            | CellArc ArcSetup other _ _ <- ppArcs pd
                            ]
                        - clkToQAdjust ep launchId captureId
                | otherwise = delay0
         in M.insertWith max dp delay1 acc
    -- walk back to the startpoint and subtract its clock-to-Q clock
    -- route delay (the C++ max_delay_by_domain_pairs correction)
    clkToQAdjust ep launchId captureId =
        case reverse (walkCritPath arch ptc cells tmg (pairIdOf tmg launchId captureId) ep True) of
            [] -> 0
            (firstInp : _) ->
                case do
                    c <- prCell firstInp
                    n <- portNet (cellPorts (cellAt cells c) M.! prPort firstInp)
                    M.lookup n nets
                    of
                        Nothing -> 0
                        Just firstInpNet ->
                            case prCell (netDriver firstInpNet) of
                                Nothing -> 0
                                Just spCell ->
                                    let spPort = prPort (netDriver firstInpNet)
                                        spPd = portAt tmg (CellPortKey spCell spPort)
                                     in sum
                                            [ dpMax (ppRouteDelay (portAt tmg (CellPortKey spCell other)))
                                            | CellArc ArcClkToQ other _ _ <- ppArcs spPd
                                            ]

-- | @build_critical_path_report@.
buildCriticalPathReport ::
    (Arch a, Ord (Wire a)) =>
    a ->
    (CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> (TimingPortClass, Int)) ->
    (CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> Int -> TimingClockingInfo) ->
    (CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> IdString -> Maybe DelayQuad) ->
    (NetInfo (Bel a) (Wire a) (Pip a) -> Bool) ->
    Double ->
    M.Map IdString (CellInfo (Bel a) (Wire a) (Pip a)) ->
    M.Map IdString (NetInfo (Bel a) (Wire a) (Pip a)) ->
    TimingAnalyser ->
    Int ->
    CellPortKey ->
    Bool ->
    CriticalPath
buildCriticalPathReport arch ptc pci getDelay isGlobalNet targetFreq cells nets tmg pairId endpoint longest =
    let dp = taDomainPairs tmg !! pairId
        launchD = taDomains tmg !! pdpLaunch dp
        captureD = taDomains tmg !! pdpCapture dp
        launchClk = pdClock launchD
        launchEdge = pdEdge launchD
        captureClk = pdClock captureD
        captureEdge = pdEdge captureD
        maxDelay0 = getDelayFromNS arch (1.0e9 / targetFreq)
        maxDelay1 = if launchEdge /= captureEdge then maxDelay0 `div` 2 else maxDelay0
        crit = reverse (walkCritPath arch ptc cells tmg pairId endpoint longest)
        firstInp = head crit
        firstInpNetId = do
            c <- prCell firstInp
            n <- portNet (cellPorts (cellAt cells c) M.! prPort firstInp)
            pure n
        sp = netDriver (nets M.! fromMaybe (error "timing report: startpoint net") firstInpNetId)
        spCell = fromMaybe (error "timing report: startpoint cell") (prCell sp)
        spPort = prPort sp
        (spCls, spClocks) = ptc (cellAt cells spCell) spPort
        (registerStart, spClkInfo, spClkNet) = registerInfoOf TmgRegisterOutput pci cells nets spCell spPort spCls spClocks launchClk launchEdge
        (epCell, epPort) = case endpoint of
            CellPortKey c p -> (c, p)
        (epCls, epClocks) = ptc (cellAt cells epCell) epPort
        (registerEnd, epClkInfo, epClkNet) = registerInfoOf TmgRegisterInput pci cells nets epCell epPort epCls epClocks captureClk captureEdge
        clockPairKey = (launchClk, captureClk)
        related = M.member clockPairKey (taClockDelays tmg)
        sameClock = launchClk == captureClk
        segsC2C =
            if related
                then
                    let cd = taClockDelays tmg M.! clockPairKey
                     in if cd /= 0
                            then [Segment SegClkToClk emptyId (spCell, tciClockPort spClkInfo) (epCell, tciClockPort epClkInfo) cd]
                            else []
                else []
        segsSkew =
            if registerStart && registerEnd && (sameClock || related)
                then
                    let cdl = clockRouteDelay spClkNet (spCell, tciClockPort spClkInfo)
                        cdc = clockRouteDelay epClkNet (epCell, tciClockPort epClkInfo)
                        skew = cdl - cdc
                     in if skew /= 0
                            then segsC2C ++ [Segment SegClkSkew (if sameClock then launchClk else emptyId) (spCell, tciClockPort spClkInfo) (epCell, tciClockPort epClkInfo) skew]
                            else segsC2C
                else segsC2C
        (segsWalked, (prevCellF, prevPortF, _)) =
            foldl' (walkStep arch getDelay isGlobalNet longest spClkInfo registerStart cells nets) (segsSkew, (spCell, spPort, True)) crit
        segsFinal =
            if registerEnd
                then
                    let seg
                            | longest = Segment SegSetup emptyId (prevCellF, prevPortF) (prevCellF, prevPortF) (dpMax (tciSetup epClkInfo))
                            | otherwise = Segment SegHold emptyId (prevCellF, prevPortF) (prevCellF, prevPortF) (-dpMax (tciHold epClkInfo))
                     in segsWalked ++ [seg]
                else segsWalked
        clockRouteDelay Nothing _ = 0
        clockRouteDelay (Just clkNet) (c, p) = netinfoRouteDelay arch isGlobalNet cells nets clkNet (PortRef (Just c) p)
     in CriticalPath
            (ClockPair (ClockEvent launchClk launchEdge) (ClockEvent captureClk captureEdge))
            maxDelay1
            segsFinal
  where
    -- register_start / register_end resolution (the C++ loop keeps the
    -- last iteration's verdict)
    registerInfoOf expectedCls pci' cells' nets' cell port cls count clk edge =
        if cls /= expectedCls
            then (False, defInfo, Nothing)
            else
                if count <= 0
                    then (False, defInfo, Nothing)
                    else
                        let verdicts =
                                [ let info = pci' (cellAt cells' cell) port i
                                      clkNet = portNetLookup cells' nets' cell (tciClockPort info)
                                      ok = maybe False (\n -> netName n == clk && tciEdge info == edge) clkNet
                                   in (ok, info, clkNet)
                                | i <- [0 .. count - 1]
                                ]
                            (okL, infoL, netL) = last verdicts
                         in (okL, infoL, netL)
    defInfo = TimingClockingInfo emptyId RisingEdge (DelayPair 0 0) (DelayPair 0 0) (DelayQuad (DelayPair 0 0) (DelayPair 0 0))
    portNetLookup cells' nets' cell p = do
        n <- portNet (cellPorts (cellAt cells' cell) M.! p)
        M.lookup n nets'

-- The main per-sink walk of build_critical_path_report: a logic segment
-- then a routing segment per input port on the path.
walkStep ::
    (Arch a, Ord (Wire a)) =>
    a ->
    (CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> IdString -> Maybe DelayQuad) ->
    (NetInfo (Bel a) (Wire a) (Pip a) -> Bool) ->
    Bool ->
    TimingClockingInfo ->
    Bool ->
    M.Map IdString (CellInfo (Bel a) (Wire a) (Pip a)) ->
    M.Map IdString (NetInfo (Bel a) (Wire a) (Pip a)) ->
    ([Segment], (IdString, IdString, Bool)) ->
    PortRef ->
    ([Segment], (IdString, IdString, Bool))
walkStep arch getDelay isGlobalNet longest spClkInfo registerStart cells nets (acc, (prevCell, prevPort, isStart)) sink =
    let sinkCell = fromMaybe (error "timing report: sink cell") (prCell sink)
        sinkPort = prPort sink
        net = nets M.! fromMaybe (error "timing report: sink net") (portNet (cellPorts (cellAt cells sinkCell) M.! sinkPort))
        driver = netDriver net
        driverCell = fromMaybe (error "timing report: driver cell") (prCell driver)
        driverPort = prPort driver
        (segT, combDelay)
            | isStart && registerStart = (SegClkToQ, tciClockToQ spClkInfo)
            | isStart = (SegSource, DelayQuad (DelayPair 0 0) (DelayPair 0 0))
            | otherwise = case getDelay (cellAt cells driverCell) prevPort driverPort of
                Just dq -> (SegLogic, dq)
                Nothing -> (SegLogic, DelayQuad (DelayPair 0 0) (DelayPair 0 0))
        segLogic =
            Segment
                segT
                emptyId
                (prevCell, prevPort)
                (driverCell, driverPort)
                (if longest then dqMaxDelay combDelay else dqMinDelay combDelay)
        netDelay = netinfoRouteDelay arch isGlobalNet cells nets net sink
        segRoute = Segment SegRouting (netName net) (driverCell, driverPort) (sinkCell, sinkPort) netDelay
     in (acc ++ [segLogic, segRoute], (sinkCell, sinkPort, False))

-- | @build_crit_path_reports@.
buildCritPathReports ::
    (Arch a, Ord (Wire a)) =>
    a ->
    IdTable ->
    (CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> (TimingPortClass, Int)) ->
    (CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> Int -> TimingClockingInfo) ->
    (CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> IdString -> Maybe DelayQuad) ->
    (NetInfo (Bel a) (Wire a) (Pip a) -> Bool) ->
    Double ->
    M.Map IdString (CellInfo (Bel a) (Wire a) (Pip a)) ->
    M.Map IdString (NetInfo (Bel a) (Wire a) (Pip a)) ->
    TimingAnalyser ->
    (M.Map IdString (Double, Double), [(IdString, CriticalPath)], [CriticalPath], [IdString])
buildCritPathReports arch tbl ptc pci getDelay isGlobalNet targetFreq cells nets tmg =
    let delayByDomain = maxDelayByDomainPairs arch ptc cells nets tmg
        empty0 = dedup [pdClock d | d <- taDomains tmg]
        (fmaxAcc, pathsAcc, emptyAcc) =
            foldl'
                (clockStep delayByDomain)
                (M.empty, [], empty0)
                (zip [0 ..] (taDomainPairs tmg))
        xclock =
            sortBy (cmpCritPath tbl)
                [ buildCriticalPathReport arch ptc pci getDelay isGlobalNet targetFreq cells nets tmg i (head worst) True
                | (i, dp) <- zip [0 ..] (taDomainPairs tmg)
                , let launchD = taDomains tmg !! pdpLaunch dp
                      captureD = taDomains tmg !! pdpCapture dp
                , pdClock launchD /= pdClock captureD || pdClock launchD == emptyId
                , let worst = getWorstEps tmg i 1
                , not (null worst)
                ]
     in (fmaxAcc, pathsAcc, xclock, emptyAcc)
  where
    dedup = foldl' (\acc x -> if x `elem` acc then acc else acc ++ [x]) []
    clockStep delayByDomain (fmaxAcc, pathsAcc, emptyAcc) (i, dp) =
        let launchD = taDomains tmg !! pdpLaunch dp
            captureD = taDomains tmg !! pdpCapture dp
            launchClk = pdClock launchD
            captureClk = pdClock captureD
         in if launchClk /= captureClk || launchClk == emptyId
                then (fmaxAcc, pathsAcc, emptyAcc)
                else
                    let pathDelay = M.findWithDefault 0 i delayByDomain
                        fmax
                            | pdEdge launchD == pdEdge captureD = realToFrac (1000 / (realToFrac (nsOf pathDelay) :: Float) :: Float)
                            | otherwise = realToFrac (500 / (realToFrac (nsOf pathDelay) :: Float) :: Float)
                     in case M.lookup launchClk fmaxAcc of
                            Just (achieved, _) | fmax >= achieved -> (fmaxAcc, pathsAcc, emptyAcc)
                            _ ->
                                let target = realToFrac (realToFrac (targetFreq / 1e6) :: Float)
                                    worst = getWorstEps tmg i 1
                                 in if null worst
                                        then (fmaxAcc, pathsAcc, emptyAcc)
                                        else
                                            ( M.insert launchClk (fmax, target) fmaxAcc
                                            , pathsAcc ++ [(launchClk, buildCriticalPathReport arch ptc pci getDelay isGlobalNet targetFreq cells nets tmg i (head worst) True)]
                                            , filter (/= launchClk) emptyAcc
                                            )
    nsOf d = getDelayNS arch d

-- | The @cmp_crit_path@ ordering of cross-domain reports.
cmpCritPath :: IdTable -> CriticalPath -> CriticalPath -> Ordering
cmpCritPath tbl a b =
    let ca = pathClockPair a
        cb = pathClockPair b
        keyOf cp =
            ( T.unpack (idToText tbl (ceClock (cpStart cp)))
            , ceEdge (cpStart cp)
            , T.unpack (idToText tbl (ceClock (cpEnd cp)))
            , ceEdge (cpEnd cp)
            )
     in compare (keyOf ca) (keyOf cb)

-- | @build_slack_histogram_report@.
buildSlackHistogram :: Arch a => a -> Double -> TimingAnalyser -> M.Map Int Int
buildSlackHistogram arch targetFreq tmg =
    foldl' domStep M.empty (taDomains tmg)
  where
    domStep acc dom = foldl' (\a (ep, _) -> epStep a ep) acc (pdEndpoints dom)
    epStep acc ep =
        let pd = portAt tmg ep
         in foldl' (\a (capId, req) -> reqStep a capId req pd) acc (M.toList (ppRequired pd))
    reqStep acc capId req pd =
        let capture = taDomains tmg !! capId
         in foldl' (\a (launchId, arr) -> arrStep a capture req launchId arr) acc (M.toList (ppArrival pd))
    arrStep acc capture req launchId arr =
        let launch = taDomains tmg !! launchId
         in if pdClock launch /= pdClock capture || pdClock launch == emptyId
                then acc
                else
                    -- the C++ keeps clk_period as a FLOAT here (float
                    -- subtraction against the delay, truncated back to
                    -- delay_t): delay_t slack = clk_period - delay;
                    let clkPeriodF = realToFrac (getDelayFromNS arch (1.0e9 / targetFreq)) :: Float
                        clkPeriodF' = if pdEdge launch /= pdEdge capture then clkPeriodF / 2 else clkPeriodF
                        delay = dpMax (artValue arr) - dpMin (artValue req)
                        slack = truncate (clkPeriodF' - fromIntegral delay)
                        slackPs = truncate ((realToFrac (getDelayNS arch slack) :: Float) * 1000)
                     in M.insertWith (+) slackPs 1 acc

-- | @get_min_delay_violations@.
getMinDelayViolations ::
    (Arch a, Ord (Wire a)) =>
    a ->
    (CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> (TimingPortClass, Int)) ->
    (CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> Int -> TimingClockingInfo) ->
    (CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> IdString -> Maybe DelayQuad) ->
    (NetInfo (Bel a) (Wire a) (Pip a) -> Bool) ->
    Double ->
    M.Map IdString (CellInfo (Bel a) (Wire a) (Pip a)) ->
    M.Map IdString (NetInfo (Bel a) (Wire a) (Pip a)) ->
    TimingAnalyser ->
    [CriticalPath]
getMinDelayViolations arch ptc pci getDelay isGlobalNet targetFreq cells nets tmg =
    sortBy (\a b -> compare (pathTotal a) (pathTotal b)) violations
  where
    pathTotal cp = sum (map segDelay (pathSegments cp))
    violations =
        [ buildCriticalPathReport arch ptc pci getDelay isGlobalNet targetFreq cells nets tmg pairId ep False
        | captureId <- [0 .. length (taDomains tmg) - 1]
        , let capture = taDomains tmg !! captureId
        , let captureClk = pdClock capture
        , (ep, _) <- pdEndpoints capture
        , let (epC, epP) = case ep of
                CellPortKey c p -> (c, p)
        , let ci = cellAt cells epC
        , fst (ptc ci epP) == TmgRegisterInput
        , let pd = portAt tmg ep
        , let req = ppRequired pd M.! captureId
        , (launchId, arr) <- M.toList (ppArrival pd)
        , let launch = taDomains tmg !! launchId
        , let launchClk = pdClock launch
        , let clocks = (launchClk, captureClk)
        , let related = M.member clocks (taClockDelays tmg)
        , launchId /= 0 && (launchId == captureId || related)
        , let c2c = if related then taClockDelays tmg M.! clocks else 0
        , dpMin (artValue arr) - dpMax (artValue req) + c2c <= 0
        , let pairId = pairIdOf tmg launchId captureId
        ]

-- | @timing_analysis()@: the full report build (setup_only=false,
-- with_clock_skew=true, real route delays).
timingAnalysisReport ::
    (Arch a, Ord (Wire a)) =>
    a ->
    IdTable ->
    (CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> (TimingPortClass, Int)) ->
    (CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> Int -> TimingClockingInfo) ->
    (CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> IdString -> Maybe DelayQuad) ->
    (NetInfo (Bel a) (Wire a) (Pip a) -> Bool) ->
    Double ->
    Design (Bel a) (Wire a) (Pip a) ->
    TimingResult
timingAnalysisReport arch tbl ptc pci getDelay isGlobalNet targetFreq d =
    let cells = designCells d
        nets = designNets d
        tmg0 = buildTimingAnalyserRaw arch ptc pci getDelay isGlobalNet False d
        tmg = runTimingAnalyserBy arch isGlobalNet True (netinfoRouteDelay arch isGlobalNet cells nets) tmg0 d
        violations = getMinDelayViolations arch ptc pci getDelay isGlobalNet targetFreq cells nets tmg
        (fmax, paths, xclock, empty) = buildCritPathReports arch tbl ptc pci getDelay isGlobalNet targetFreq cells nets tmg
        hist = buildSlackHistogram arch targetFreq tmg
     in TimingResult fmax paths xclock empty hist violations

-- ---------------------------------------------------------------------
-- Printing
-- ---------------------------------------------------------------------

-- | @clock_event_name@.
clockEventName :: IdTable -> ClockEvent -> Int -> String
clockEventName tbl e fieldWidth =
    let value
            | ceClock e == emptyId = "<async>"
            | otherwise = (if ceEdge e == FallingEdge then "negedge " else "posedge ") ++ T.unpack (idToText tbl (ceClock e))
     in value ++ replicate (max 0 (fieldWidth - length value)) ' '

emit :: [String] -> IO ()
emit = mapM_ (hPutStrLn stderr) . collapse
  where
    -- log_break() collapses consecutive newline runs (log_newline_count < 2)
    collapse ("" : "" : rest) = collapse ("" : rest)
    collapse (l : rest) = l : collapse rest
    collapse [] = []

-- | @log_crit_paths@.
logCritPaths :: Arch a => a -> IdTable -> M.Map IdString (CellInfo (Bel a) (Wire a) (Pip a)) -> M.Map IdString (NetInfo (Bel a) (Wire a) (Pip a)) -> TimingResult -> [String]
logCritPaths arch tbl cells nets result =
    clockLines ++ xclockLines ++ violationLines
  where
    edgeStr :: ClockEdge -> String
    edgeStr RisingEdge = "posedge"
    edgeStr FallingEdge = "negedge"
    clockLines =
        concat
            [ [ ""
              , "Info: " ++ printf "Critical path report for clock '%s' (%s -> %s):" (T.unpack (idToText tbl clock)) (edgeStr (ceEdge (cpStart (pathClockPair report)))) (edgeStr (ceEdge (cpEnd (pathClockPair report))))
              ]
                ++ printPathReport arch tbl cells nets report
            | (clock, report) <- reverse (trClockPaths result)
            ]
    xclockLines =
        concat
            [ [ ""
              , "Info: " ++ printf "Critical path report for cross-domain path '%s' -> '%s':" (clockEventName tbl (cpStart (pathClockPair report)) 0) (clockEventName tbl (cpEnd (pathClockPair report)) 0)
              ]
                ++ printPathReport arch tbl cells nets report
            | report <- trXclockPaths result
            ]
    violationLines =
        let numMin = length (trMinDelayViolations result)
         in if numMin > 0
                then
                    [ ""
                    , "Info: " ++ printf "%d Hold/min time violations (showing 10 worst paths):" numMin
                    ]
                        ++ concat
                            [ [ ""
                              , let start' = clockEventName tbl (cpStart (pathClockPair report)) 0
                                    end' = clockEventName tbl (cpEnd (pathClockPair report)) 0
                                    msg
                                        | cpStart (pathClockPair report) == cpEnd (pathClockPair report) =
                                            "Hold/min time violation for clock '" ++ start' ++ "':"
                                        | otherwise =
                                            "Hold/min time violation for path '" ++ start' ++ "' -> '" ++ end' ++ "':"
                                 in "Error: " ++ msg
                              ]
                                ++ printPathReport arch tbl cells nets report
                            | report <- take 10 (trMinDelayViolations result)
                            ]
                else []

-- | @print_path_report@ (the per-path segment dump).
printPathReport :: Arch a => a -> IdTable -> M.Map IdString (CellInfo (Bel a) (Wire a) (Pip a)) -> M.Map IdString (NetInfo (Bel a) (Wire a) (Pip a)) -> CriticalPath -> [String]
printPathReport arch tbl cells nets report =
    ("Info:       type curr  total name" : segmentLines)
        ++ ["Info: " ++ printf "%.2f ns logic, %.2f ns routing" (getDelayNS arch logicTotal) (getDelayNS arch routeTotal)]
  where
    (logicTotal, routeTotal, segmentLines) = foldl' step (0, 0, []) (pathSegments report)
    step (lT, rT, ls) seg =
        case segType seg of
            SegClkToQ -> logicLine seg (lT + segDelay seg) rT ls
            SegSource -> logicLine seg (lT + segDelay seg) rT ls
            SegLogic -> logicLine seg (lT + segDelay seg) rT ls
            SegSetup -> logicLine seg (lT + segDelay seg) rT ls
            SegHold -> logicLine seg (lT + segDelay seg) rT ls
            _ -> routeLine seg lT (rT + segDelay seg) ls
    logicLine seg lT' rT ls =
        let total = lT' + rT
            ln =
                "Info: "
                    ++ printf "%10s % 5.2f % 5.2f Source %s.%s" (segmentTypeStr (segType seg)) (getDelayNS arch (segDelay seg)) (getDelayNS arch total) (T.unpack (idToText tbl (fst (segTo seg)))) (T.unpack (idToText tbl (snd (segTo seg))))
         in (lT', rT, ls ++ [ln])
    routeLine seg lT rT' ls =
        let total = lT + rT'
            driverCell = cells M.! fst (segFrom seg)
            sinkCell = cells M.! fst (segTo seg)
            driverLoc = getBelLocation arch (fromMaybe (error "timing report: driver bel") (cellBel driverCell))
            sinkLoc = getBelLocation arch (fromMaybe (error "timing report: sink bel") (cellBel sinkCell))
            ln1 =
                "Info: "
                    ++ printf "%10s % 5.2f % 5.2f Net %s (%d,%d) -> (%d,%d)" (segmentTypeStr (segType seg)) (getDelayNS arch (segDelay seg)) (getDelayNS arch total) (T.unpack (idToText tbl (segNet seg))) (locX' driverLoc) (locY' driverLoc) (locX' sinkLoc) (locY' sinkLoc)
            ln2 = "Info: " ++ printf "                         Sink %s.%s" (T.unpack (idToText tbl (fst (segTo seg)))) (T.unpack (idToText tbl (snd (segTo seg))))
            srcLines = case M.lookup (segNet seg) nets of
                Just ni -> netSourceLines tbl ni
                Nothing -> []
         in (lT, rT', ls ++ [ln1, ln2] ++ srcLines)
    locX' (Loc x _ _) = x
    locY' (Loc _ y _) = y
    netSourceLines tbl' ni =
        case idByName tbl' "src" >>= \srcId -> M.lookup srcId (netAttrs ni) of
            Just p
                | propIsString p ->
                    ["Info:                          Defined in:"]
                        ++ [ "Info:                               " ++ T.unpack entry
                           | entry <- T.splitOn "|" (propAsString p)
                           ]
            _ -> []

-- | @log_fmax@.
logFmax :: Arch a => a -> IdTable -> Bool -> Bool -> TimingResult -> [String]
logFmax arch tbl warnOnFailure allowFail result =
    "" : fmaxLines
        ++ [""]
        ++ xclockFmaxLines
        ++ [""]
        ++ c2cLines
        ++ [""]
        ++ emptyClockLines
        ++ [""]
        ++ maxDelayLines
        ++ [""]
  where
    nsOf d = getDelayNS arch d
    fmaxLines
        | null (trClockPaths result) = ["Info: No Fmax available; no interior timing paths found in design."]
        | otherwise =
            let maxWidth = maximum (0 : [length (T.unpack (idToText tbl clock)) | (clock, _) <- trClockPaths result])
             in concat
                    [ let name = T.unpack (idToText tbl clock)
                          width = maxWidth - length name
                          (fmax, target) = M.findWithDefault (0, 0) clock (trClockFmax result)
                          passed = target < fmax
                          body = printf "Max frequency for clock %*s'%s': %.02f MHz (%s at %.02f MHz)" width ("" :: String) name fmax ((if passed then "PASS" else "FAIL") :: String) target
                       in [ if not warnOnFailure || passed
                                then "Info: " ++ body
                                else
                                    if allowFail
                                        then "Warning: " ++ body
                                        else "Error: " ++ body
                          ]
                    | (clock, _) <- reverse (trClockPaths result)
                    ]
    xclockDelays =
        [ (pathClockPair report, clockDelay)
        | report <- trXclockPaths result
        , let clockDelay = sum [segDelay seg | seg <- pathSegments report, segType seg == SegClkToClk]
        , clockDelay /= 0
        ]
    maxWidthXca = maximum (0 : [length (clockEventName tbl (cpStart (pathClockPair r)) 0) | r <- trXclockPaths result])
    maxWidthXcb = maximum (0 : [length (clockEventName tbl (cpEnd (pathClockPair r)) 0) | r <- trXclockPaths result])
    xclockFmaxLines
        | null (trXclockPaths result) = []
        | otherwise =
            concat
                [ let cp = pathClockPair report
                      clockA = ceClock (cpStart cp)
                      clockB = ceClock (cpEnd cp)
                   in case lookup cp xclockDelays of
                        Nothing -> []
                        Just clockDelay ->
                            let pathDelay = sum (map segDelay (pathSegments report))
                                fmax
                                    | pathDelay < 0 = realToFrac (1e3 / (realToFrac (nsOf clockDelay) :: Float) :: Float)
                                    | pathDelay > 0 = realToFrac (1e3 / (realToFrac (nsOf pathDelay) :: Float) :: Float)
                                    | otherwise = 1 / 0
                                target =
                                    case (M.lookup clockA (trClockFmax result), M.lookup clockB (trClockFmax result)) of
                                        (Just (_, ta), Nothing) -> ta
                                        (Nothing, Just (_, tb)) -> tb
                                        (Just (_, ta), Just (_, tb)) -> min ta tb
                                        _ -> 0
                                passed = target < fmax
                                evA = clockEventName tbl (cpStart cp) maxWidthXca
                                evB = clockEventName tbl (cpEnd cp) maxWidthXcb
                                body = printf "Max frequency for %s -> %s: %.02f MHz (%s at %.02f MHz)" evA evB fmax ((if passed then "PASS" else "FAIL") :: String) target
                             in [ if not warnOnFailure || passed
                                    then "Info: " ++ body
                                    else
                                        if allowFail
                                            then "Warning: " ++ printf "Max frequency for  %s -> %s: %.02f MHz (%s at %.02f MHz)" evA evB fmax ((if passed then "PASS" else "FAIL") :: String) target
                                            else "Error: " ++ body
                                ]
                | report <- trXclockPaths result
                ]
    c2cLines
        | null xclockDelays = []
        | otherwise =
            concat
                [ let evA = clockEventName tbl (cpStart pair) maxWidthXca
                      evB = clockEventName tbl (cpEnd pair) maxWidthXcb
                      delay = if ceEdge (cpStart pair) /= ceEdge (cpEnd pair) then d `div` 2 else d
                   in ["Info: " ++ printf "Clock to clock delay %s -> %s: %0.02f ns" evA evB (nsOf delay)]
                | (pair, d) <- reverse xclockDelays
                ]
    emptyClockLines =
        [ "Info: " ++ printf "Clock '%s' has no interior paths" (T.unpack (idToText tbl c))
        | c <- trEmptyPaths result
        , c /= emptyId
        ]
    startFieldWidth = maximum (0 : [length (clockEventName tbl (cpStart (pathClockPair r)) 0) | r <- trXclockPaths result])
    endFieldWidth = maximum (0 : [length (clockEventName tbl (cpEnd (pathClockPair r)) 0) | r <- trXclockPaths result])
    maxDelayLines =
        [ "Info: "
            ++ printf "Max delay %s -> %s: %0.02f ns" (clockEventName tbl (cpStart (pathClockPair report)) startFieldWidth) (clockEventName tbl (cpEnd (pathClockPair report)) endFieldWidth) (nsOf (sum (map segDelay (pathSegments report))))
        | report <- trXclockPaths result
        ]

-- | @log_histogram@.
logHistogram :: TimingResult -> [String]
logHistogram result =
    let hist = trSlackHistogram result
        minSlack = minimum (M.keys hist)
        maxSlack = maximum (M.keys hist)
        binSize = max 1 (ceiling (fromIntegral (maxSlack - minSlack + 1) / (20 :: Float)))
        bins0 = replicate 20 (0 :: Int)
        bins = foldl' (\acc (slackPs, n) -> let i = max 0 (min 19 ((slackPs - minSlack) `div` binSize)) in updAt i (+ n) acc) bins0 (M.toList hist)
        maxFreq = maximum (1 : bins)
        barWidth = min 60 maxFreq
        star :: Int -> String
        star i = replicate (bins !! i * barWidth `div` maxFreq) '*'
        tailCh :: Int -> String
        tailCh i = if (bins !! i * barWidth) `mod` maxFreq > 0 then "+" else " "
     in [ ""
        , "Info: Slack histogram:"
        , "Info: " ++ printf " legend: * represents %d endpoint(s)" (maxFreq `div` barWidth)
        , "Info: " ++ printf "         + represents [1,%d) endpoint(s)" (maxFreq `div` barWidth)
        ]
            ++ [ "Info: " ++ printf "[%6d, %6d) |%s%s" (minSlack + binSize * i) (minSlack + binSize * (i + 1)) (star i) (tailCh i)
               | i <- [0 .. 19]
               ]
  where
    updAt i f xs = case splitAt i xs of
        (before, x : after) -> before ++ [f x] ++ after
        _ -> xs

-- | @log_timing_results@.
logTimingResults ::
    Arch a =>
    a ->
    IdTable ->
    M.Map IdString (CellInfo (Bel a) (Wire a) (Pip a)) ->
    M.Map IdString (NetInfo (Bel a) (Wire a) (Pip a)) ->
    Bool ->
    Bool ->
    Bool ->
    Bool ->
    TimingResult ->
    IO ()
logTimingResults arch tbl cells nets printPath printFmax printHist warnOnFailure result = do
    _ <- intern tbl "$async$"
    emit
        ( (if printPath then logCritPaths arch tbl cells nets result else [])
            ++ (if printFmax then logFmax arch tbl warnOnFailure False result else [])
            ++ (if printHist && not (M.null (trSlackHistogram result)) then logHistogram result else [])
        )

-- | @print_utilisation@: the post-pack device utilisation table.
-- The C++ counts cells by their TYPE's bucket (@getBelBucketForCellType@,
-- the type name for ECP5), so no bel bindings are needed.
printUtilisation :: Arch a => a -> IdTable -> Design (Bel a) (Wire a) (Pip a) -> IO ()
printUtilisation arch tbl d =
    let usedCounts = M.fromListWith (+) [ (cellType ci, 1 :: Int) | ci <- map snd (M.toList (designCells d)) ]
        availCounts = M.fromListWith (+) [ (getBelType arch b, 1 :: Int) | b <- getBels arch ]
        allTypes = S.toAscList (M.keysSet availCounts `S.union` M.keysSet usedCounts)
        lines' =
            [ "Info: "
                ++ printf "\t%20s: %7d/%7d %5d%%" (T.unpack (idToText tbl t)) used avail (100 * used `div` avail)
            | t <- allTypes
            , let avail = M.findWithDefault 0 t availCounts
                  used = M.findWithDefault 0 t usedCounts
            ]
     in emit (["", "Info: Device utilisation:"] ++ lines' ++ [""])
