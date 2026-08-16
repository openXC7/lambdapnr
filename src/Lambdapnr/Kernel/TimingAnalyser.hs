{- | Static timing analysis engine, mirror of nextpnr's
@common\/kernel\/timing.cc@ (@TimingAnalyser@).

The placer consumes only @get_criticality@ (worst criticality across
domain pairs) and, for that, only the following are order-sensitive:

- the topological order (DFS over the port keys in @std::map@ key order,
  post-order push);
- the per-port cell-arc vectors (build sequence: register arcs, then
  combinational arcs in the cell's @ports@ dict iteration order, which
  is the reverse of the port insertion order);
- the domain-id assignment order (first touch during the domain
  propagation loop).

Everything else (@arrival@/@required@/@domain_pairs@ dict iteration,
start/endpoint vector order) only feeds max\/min accumulations and is
order-independent; the C++ dicts are therefore mirrored with plain
@Map@s here.

@setup_only@ mirrors the placer's @tmg.setup_only = true@: min-delay
arrival and max-delay required propagation are disabled (hold analysis
off).
-}
module Lambdapnr.Kernel.TimingAnalyser (
    ArrivReqTime (..),
    CellArc (..),
    CellArcType (..),
    CellPortKey (..),
    PerDomain (..),
    PerDomainPair (..),
    PerPort (..),
    PortDomainPairData (..),
    TimingAnalyser (..),
    buildTimingAnalyser,
    runTimingAnalyser,
    criticalityOf,
) where

import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Control.Monad (unless)
import Control.Monad.ST (runST)
import Data.STRef (modifySTRef', newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector as V
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

import Lambdapnr.Kernel.Arch (Arch (..), Bel, Pip, Wire)
import Lambdapnr.Kernel.Delay (DelayPair (..), DelayQuad (..), DelayT, dpFromDelay, dpMinus, dpPlus, dqDelayPair, dqMaxDelay)
import Lambdapnr.Kernel.IdString (IdString (..), emptyId)
import Lambdapnr.Kernel.Netlist (CellInfo (..), Design (..), NetInfo (..), PortDir (..), PortInfo (..), PortRef (..), cellBel, portNet)
import Lambdapnr.Kernel.Timing (ClockEdge (..), TimingClockingInfo (..), TimingPortClass (..))

-- | A cell-port pair, mirroring the C++ @CellPortKey@ (ordered by id,
-- matching the @std::map@ of @TopoSort@).
data CellPortKey = CellPortKey !IdString !IdString
    deriving (Eq, Ord, Show)

data CellArcType
    = ArcCombinational
    | ArcSetup
    | ArcHold
    | ArcClkToQ
    | ArcStartpoint
    | ArcEndpoint
    deriving (Eq, Show)

data CellArc = CellArc !CellArcType !IdString !DelayQuad !ClockEdge
    deriving (Show)

data ArrivReqTime = ArrivReqTime
    { artValue :: !DelayPair
    , artBwdMin :: !CellPortKey
    , artBwdMax :: !CellPortKey
    , artPathLength :: !Int
    }
    deriving (Show)

emptyArrivReq :: ArrivReqTime
emptyArrivReq = ArrivReqTime (DelayPair maxBound minBound) (CellPortKey emptyId emptyId) (CellPortKey emptyId emptyId) 0

data PortDomainPairData = PortDomainPairData
    { pdpSetupSlack :: !DelayT
    , pdpMaxPathLength :: !Int
    , pdpCriticality :: !Float
    }
    deriving (Show)

data PerPort = PerPort
    { ppType :: !PortDir
    , ppArrival :: !(M.Map Int ArrivReqTime)
    , ppRequired :: !(M.Map Int ArrivReqTime)
    , ppDomainPairs :: !(M.Map Int PortDomainPairData)
    , ppArcs :: ![CellArc]
    , ppRouteDelay :: !DelayPair
    , ppWorstCrit :: !Float
    }
    deriving (Show)

data PerDomain = PerDomain
    { pdClock :: !IdString
    , pdEdge :: !ClockEdge
    , pdStartpoints :: ![(CellPortKey, IdString)]
    , pdEndpoints :: ![(CellPortKey, IdString)]
    }
    deriving (Show)

data PerDomainPair = PerDomainPair
    { pdpLaunch :: !Int
    , pdpCapture :: !Int
    , pdpWorstSetupSlack :: !DelayT
    }
    deriving (Show)

data TimingAnalyser = TimingAnalyser
    { taPorts :: !(M.Map CellPortKey PerPort)
    , taDomains :: ![PerDomain]
    , taDomainPairs :: ![PerDomainPair]
    , taTopoOrder :: ![CellPortKey]
    , taClockDelays :: !(M.Map (IdString, IdString) DelayT)
    , taSetupOnly :: !Bool
    }
    deriving (Show)

asyncDomainId :: Int
asyncDomainId = 0

-- | Build the analyser: @init_ports@, @get_cell_delays@, @topo_sort@,
-- @setup_port_domains@, @identify_related_domains@, then one @run@.
-- The timing-class functions are passed in (the arch-info aware ECP5
-- variants live with the arch); @isGlobalNet@ mirrors
-- @net_info->is_global@.
buildTimingAnalyser ::
    Arch a =>
    a ->
    (CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> (TimingPortClass, Int)) ->
    (CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> Int -> TimingClockingInfo) ->
    (CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> IdString -> Maybe DelayQuad) ->
    (NetInfo (Bel a) (Wire a) (Pip a) -> Bool) ->
    Bool ->
    Design (Bel a) (Wire a) (Pip a) ->
    TimingAnalyser
buildTimingAnalyser arch ptc pci getDelay isGlobalNet setupOnly d =
    let ports0 = initPorts (designCells d)
        ports1 = getCellDelays arch ptc pci getDelay (designCells d) ports0
        (haveLoops, topo) = topoSort (designCells d) (designNets d) ports1
        domains0 = [PerDomain emptyId RisingEdge [] []]
        (ports2, domains1, pairs0) = setupPortDomains (designCells d) (designNets d) haveLoops topo ports1 domains0 M.empty
        clockDelays = identifyRelatedDomains arch ptc pci getDelay (designCells d) (designNets d) domains1
        tmg0 = TimingAnalyser ports2 domains1 pairs0 topo clockDelays setupOnly
     in runTimingAnalyser arch isGlobalNet True tmg0 d

-- | @run@: reset times, refresh route delays, walk both directions,
-- compute slack and criticality.
runTimingAnalyser ::
    Arch a =>
    a ->
    (NetInfo (Bel a) (Wire a) (Pip a) -> Bool) ->
    Bool ->
    TimingAnalyser ->
    Design (Bel a) (Wire a) (Pip a) ->
    TimingAnalyser
runTimingAnalyser arch isGlobalNet updateRouteDelays tmg d =
    let ports0 = resetTimes (taPorts tmg)
        ports1
            | updateRouteDelays = getRouteDelays arch isGlobalNet (designCells d) (designNets d) ports0
            | otherwise = ports0
        ports2 = walkForward (designCells d) (designNets d) (taDomains tmg) (taTopoOrder tmg) (taSetupOnly tmg) ports1
        ports3 = walkBackward (designCells d) (designNets d) (taDomains tmg) (taTopoOrder tmg) (taSetupOnly tmg) ports2
        (ports4, pairs1) = computeSlack (taTopoOrder tmg) (taDomains tmg) (taClockDelays tmg) (taSetupOnly tmg) ports3 (taDomainPairs tmg)
        ports5 = computeCriticality (taTopoOrder tmg) (taDomains tmg) (taSetupOnly tmg) ports4 pairs1
     in tmg{taPorts = ports5, taDomainPairs = pairs1}

-- | @get_criticality@: worst criticality of a cell port.
criticalityOf :: TimingAnalyser -> CellPortKey -> Float
criticalityOf tmg key = maybe 0 ppWorstCrit (M.lookup key (taPorts tmg))

-- init_ports --------------------------------------------------------------

initPorts :: M.Map IdString (CellInfo bel wire pip) -> M.Map CellPortKey PerPort
initPorts cells =
    M.fromList
        [ ( CellPortKey (cellName ci) p
          , PerPort (portType pi) M.empty M.empty M.empty [] (dpFromDelay 0) 0
          )
        | ci <- M.elems cells
        , p <- cellPortOrder ci
        , Just pi <- [M.lookup p (cellPorts ci)]
        ]

-- get_cell_delays ---------------------------------------------------------

getCellDelays ::
    Arch a =>
    a ->
    (CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> (TimingPortClass, Int)) ->
    (CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> Int -> TimingClockingInfo) ->
    (CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> IdString -> Maybe DelayQuad) ->
    M.Map IdString (CellInfo (Bel a) (Wire a) (Pip a)) ->
    M.Map CellPortKey PerPort ->
    M.Map CellPortKey PerPort
getCellDelays arch ptc pci getDelay cells ports0 =
    M.mapWithKey arcsOf ports0
  where
    clockPortLive ci portId = case M.lookup portId (cellPorts ci) of
        Just clkPi -> portNet clkPi /= Nothing
        Nothing -> False

    inputArcs ci port cls clkInfoCount =
        let regArcs
                | cls == TmgRegisterInput =
                    concat
                        [ [ CellArc ArcSetup (tciClockPort info) (DelayQuad (tciSetup info) (tciSetup info)) (tciEdge info)
                          , CellArc ArcHold (tciClockPort info) (DelayQuad (tciHold info) (tciHold info)) (tciEdge info)
                          ]
                        | i <- [0 .. clkInfoCount - 1]
                        , let info = pci ci port i
                        , clockPortLive ci (tciClockPort info)
                        ]
                | otherwise = []
            endpointArcs
                | cls == TmgEndpoint = [CellArc ArcEndpoint emptyId (DelayQuad (dpFromDelay 0) (dpFromDelay 0)) RisingEdge]
                | otherwise = []
            combArcs =
                [ CellArc ArcCombinational other dq RisingEdge
                | other <- reverse (cellPortOrder ci)
                , Just op <- [M.lookup other (cellPorts ci)]
                , portNet op /= Nothing
                , portType op == PortOut
                , Just dq <- [getDelay ci port other]
                ]
         in regArcs ++ endpointArcs ++ combArcs

    outputArcs ci port cls clkInfoCount =
        let regArcs
                | cls == TmgRegisterOutput =
                    [ CellArc ArcClkToQ (tciClockPort info) (tciClockToQ info) (tciEdge info)
                    | i <- [0 .. clkInfoCount - 1]
                    , let info = pci ci port i
                    , clockPortLive ci (tciClockPort info)
                    ]
                | otherwise = []
            startpointArcs
                | cls == TmgStartpoint = [CellArc ArcStartpoint emptyId (DelayQuad (dpFromDelay 0) (dpFromDelay 0)) RisingEdge]
                | otherwise = []
            combArcs =
                [ CellArc ArcCombinational other dq RisingEdge
                | other <- reverse (cellPortOrder ci)
                , Just op <- [M.lookup other (cellPorts ci)]
                , portNet op /= Nothing
                , portType op == PortIn
                , Just dq <- [getDelay ci other port]
                ]
         in regArcs ++ startpointArcs ++ combArcs

    arcsOf key@(CellPortKey cell port) pd =
        case do
            ci <- M.lookup cell cells
            pi <- M.lookup port (cellPorts ci)
            pure (ci, pi) of
            Nothing -> pd
            Just (ci, pi)
                | portNet pi == Nothing -> pd
                | otherwise ->
                    let (cls, clkInfoCount) = ptc ci port
                     in if cls == TmgClockInput || cls == TmgGenClock || cls == TmgIgnore
                            then pd
                            else
                                if portType pi == PortIn
                                    then pd{ppArcs = inputArcs ci port cls clkInfoCount}
                                    else pd{ppArcs = outputArcs ci port cls clkInfoCount}

-- topo_sort ---------------------------------------------------------------

-- | @TopoSort@: nodes = all ports; edges from an input port go to the
-- cell's combinational fanouts, from an output port to the net users.
-- @std::map@ key-order DFS with post-order push.
topoSort ::
    M.Map IdString (CellInfo bel wire pip) ->
    M.Map IdString (NetInfo bel wire pip) ->
    M.Map CellPortKey PerPort ->
    (Bool, [CellPortKey])
topoSort cells nets ports0 =
    let keys = M.keys ports0
        edgeTargets key pd
            | ppType pd == PortIn =
                [ CellPortKey cell other
                | CellArc ArcCombinational other _ _ <- ppArcs pd
                , let CellPortKey cell _ = key
                ]
            | otherwise =
                let CellPortKey cell port = key
                 in case do
                        ci <- M.lookup cell cells
                        pi <- M.lookup port (cellPorts ci)
                        nid <- portNet pi
                        M.lookup nid nets of
                        Nothing -> []
                        Just ni ->
                            [ CellPortKey (fromMaybe emptyId (prCell u)) (prPort u)
                            | Just u <- V.toList (netUsers ni)
                            ]
        -- database[right] = set of lefts (dependencies)
        db =
            M.fromListWith S.union
                [ (target, S.singleton key)
                | key <- keys
                , Just pd <- [M.lookup key ports0]
                , target <- edgeTargets key pd
                ]
        allDb = M.union db (M.fromList [(k, S.empty) | k <- keys, M.notMember k db])
        -- iterative DFS in ascending key order (the C++ TopoSort):
        -- each node's dependency subtree completes before the node
        -- itself is pushed (post-order)
        (loopsFound, sortedRev) = runST $ do
            markedRef <- newSTRef S.empty
            activeRef <- newSTRef S.empty
            loopRef <- newSTRef False
            sortedRef <- newSTRef []
            let rec k = do
                    marked <- readSTRef markedRef
                    active <- readSTRef activeRef
                    if S.member k active
                        then writeSTRef loopRef True
                        else
                            unless (S.member k marked) $ do
                                modifySTRef' activeRef (S.insert k)
                                mapM_ rec (maybe [] S.toList (M.lookup k allDb))
                                modifySTRef' activeRef (S.delete k)
                                modifySTRef' markedRef (S.insert k)
                                modifySTRef' sortedRef (k :)
            mapM_ rec (M.keys allDb)
            lf <- readSTRef loopRef
            sr <- readSTRef sortedRef
            pure (lf, sr)
     in (loopsFound, reverse sortedRev)

-- setup_port_domains ------------------------------------------------------

setupPortDomains ::
    M.Map IdString (CellInfo bel wire pip) ->
    M.Map IdString (NetInfo bel wire pip) ->
    Bool ->
    [CellPortKey] ->
    M.Map CellPortKey PerPort ->
    [PerDomain] ->
    M.Map (IdString, ClockEdge) Int ->
    (M.Map CellPortKey PerPort, [PerDomain], [PerDomainPair])
setupPortDomains cells nets haveLoops topo ports0 domains0 domToId0 = go True ports0 domains0 domToId0
  where
    go firstIter pAcc domsAcc dti =
        let (ports1, domains1, domToId1, upd1) = foldl' (cntStep fwdStep firstIter) (pAcc, domsAcc, dti, False) topo
            (ports2, domains2, _domToId2, upd2) = foldl' (cntStep bwdStep firstIter) (ports1, domains1, domToId1, upd1) (reverse topo)
            (ports3, pairs0, _pairToId) = foldl' pairStep (ports2, [], M.empty) topo
         in if haveLoops && upd2 then go False ports3 domains2 domToId1 else (ports3, domains2, pairs0)
    portInfoOf (CellPortKey cell port) = do
        ci <- M.lookup cell cells
        M.lookup port (cellPorts ci)

    netOf key = do
        pi <- portInfoOf key
        nid <- portNet pi
        M.lookup nid nets

    -- domain_id(cell, clock_port, edge): the clock port's net name
    clockDomId (pAcc, domsAcc, dti, upd) (CellPortKey cell clockPort) edge =
        case do
            ci <- M.lookup cell cells
            pi <- M.lookup clockPort (cellPorts ci)
            nid <- portNet pi
            M.lookup nid nets of
            Nothing -> (pAcc, domsAcc, dti, upd, Nothing)
            Just netInfo ->
                case M.lookup (netName netInfo, edge) dti of
                    Just i -> (pAcc, domsAcc, dti, upd, Just i)
                    Nothing ->
                        let i = length domsAcc
                         in ( pAcc
                            , domsAcc ++ [PerDomain (netName netInfo) edge [] []]
                            , M.insert (netName netInfo, edge) i dti
                            , upd
                            , Just i
                            )

    copyDomains backward fromKey toKey pAcc upd =
        case (M.lookup fromKey pAcc, M.lookup toKey pAcc) of
            (Just f, Just t) ->
                let src = if backward then ppRequired f else ppArrival f
                    dst = if backward then ppRequired t else ppArrival t
                    newKeys = [k | k <- M.keys src, not (M.member k dst)]
                    dst' = foldl (\m k -> M.insert k emptyArrivReq m) dst newKeys
                    t' = if backward then t{ppRequired = dst'} else t{ppArrival = dst'}
                 in (M.insert toKey t' pAcc, upd || not (null newKeys))
            _ -> (pAcc, upd)

    fwdStep st@(pAcc, domsAcc, dti, upd) firstIter key@(CellPortKey cell port) =
        case (M.lookup key pAcc, portInfoOf key) of
            (Just pd, Just pi)
                | portType pi == PortOut ->
                    let -- first iteration: registered outputs are startpoints
                        (p1, d1, dt1, up1) =
                            foldl
                                ( \st' (CellArc typ other _ edge) ->
                                    case typ of
                                        ArcClkToQ ->
                                            case clockDomId st' (CellPortKey cell other) edge of
                                                (pA, dA, dtA, _, Just dom) ->
                                                    let pdA = maybe (error "timing: port vanished") id (M.lookup key pA)
                                                        pdA' = pdA{ppArrival = M.insertWith (\_ o -> o) dom emptyArrivReq (ppArrival pdA)}
                                                        dA' = updDomain dA dom (\dmn -> dmn{pdStartpoints = pdStartpoints dmn ++ [(key, other)]})
                                                     in (M.insert key pdA' pA, dA', dtA, upd)
                                                stA@(pA, dA, dtA, uA, _) -> (pA, dA, dtA, uA)
                                        ArcStartpoint ->
                                            let (pA, dA, dtA, _) = st'
                                                pdA = maybe (error "timing: port vanished") id (M.lookup key pA)
                                                pdA' = pdA{ppArrival = M.insertWith (\_ o -> o) asyncDomainId emptyArrivReq (ppArrival pdA)}
                                                dA' = updDomain dA asyncDomainId (\dmn -> dmn{pdStartpoints = pdStartpoints dmn ++ [(key, emptyId)]})
                                             in (M.insert key pdA' pA, dA', dtA, upd)
                                        _ -> st'
                                )
                                (pAcc, domsAcc, dti, upd)
                                (ppArcs pd)
                        -- copy domains across routing
                        (p2, updF) = case netOf key >>= \ni -> Just [u | Just u <- V.toList (netUsers ni)] of
                            Just usrs ->
                                foldl
                                    ( \(pA, uA) u ->
                                        copyDomains False key (CellPortKey (fromMaybe emptyId (prCell u)) (prPort u)) pA uA
                                    )
                                    (p1, up1)
                                    usrs
                            Nothing -> (p1, up1)
                     in (p2, d1, dt1, updF)
                | otherwise ->
                    let (p1, updF) =
                            foldl
                                ( \(pA, uA) (CellArc typ other _ _) ->
                                    if typ == ArcCombinational
                                        then copyDomains False key (CellPortKey cell other) pA uA
                                        else (pA, uA)
                                )
                                (pAcc, upd)
                                (ppArcs pd)
                     in (p1, domsAcc, dti, updF)
            _ -> st

    bwdStep st@(pAcc, domsAcc, dti, upd) firstIter key@(CellPortKey cell port) =
        case (M.lookup key pAcc, portInfoOf key) of
            (Just pd, Just pi)
                | portType pi == PortOut ->
                    let (p1, updF) =
                            foldl
                                ( \(pA, uA) (CellArc typ other _ _) ->
                                    if typ == ArcCombinational
                                        then copyDomains True key (CellPortKey cell other) pA uA
                                        else (pA, uA)
                                )
                                (pAcc, upd)
                                (ppArcs pd)
                     in (p1, domsAcc, dti, updF)
                | otherwise ->
                    let -- first iteration: registered inputs are endpoints
                        (p1, d1, dt1, up1) =
                            foldl
                                ( \st' (CellArc typ other _ edge) ->
                                    case typ of
                                        ArcSetup ->
                                            case clockDomId st' (CellPortKey cell other) edge of
                                                (pA, dA, dtA, _, Just dom) ->
                                                    let pdA = maybe (error "timing: port vanished") id (M.lookup key pA)
                                                        pdA' = pdA{ppRequired = M.insertWith (\_ o -> o) dom emptyArrivReq (ppRequired pdA)}
                                                        dA' = updDomain dA dom (\dmn -> dmn{pdEndpoints = pdEndpoints dmn ++ [(key, other)]})
                                                     in (M.insert key pdA' pA, dA', dtA, upd)
                                                stA@(pA, dA, dtA, uA, _) -> (pA, dA, dtA, uA)
                                        ArcEndpoint ->
                                            let (pA, dA, dtA, _) = st'
                                                pdA = maybe (error "timing: port vanished") id (M.lookup key pA)
                                                pdA' = pdA{ppRequired = M.insertWith (\_ o -> o) asyncDomainId emptyArrivReq (ppRequired pdA)}
                                                dA' = updDomain dA asyncDomainId (\dmn -> dmn{pdEndpoints = pdEndpoints dmn ++ [(key, emptyId)]})
                                             in (M.insert key pdA' pA, dA', dtA, upd)
                                        _ -> st'
                                )
                                (pAcc, domsAcc, dti, upd)
                                (ppArcs pd)
                        -- copy port to driver
                        (p2, updF) = case netOf key of
                            Just ni ->
                                case prCell (netDriver ni) of
                                    Just drvCell -> copyDomains True key (CellPortKey drvCell (prPort (netDriver ni))) p1 up1
                                    Nothing -> (p1, up1)
                            Nothing -> (p1, up1)
                     in (p2, d1, dt1, updF)
            _ -> st

    -- force the accumulator so the per-step map thunks cannot chain
    cntStep f firstIter st key =
        let st'@(pAcc', dA', dtA', _) = f st firstIter key
         in pAcc' `seq` dA' `seq` dtA' `seq` st'

    pairStep (pAcc, pairsAcc, ptAcc) key =
        case M.lookup key pAcc of
            Nothing -> (pAcc, pairsAcc, ptAcc)
            Just pd ->
                let (pd', pairsAcc', ptAcc') =
                        foldl
                            ( \(pdA, pA, ptA) (arrId, reqId) ->
                                case M.lookup (arrId, reqId) ptA of
                                    Just pairId ->
                                        ( pdA{ppDomainPairs = M.insertWith (\_ o -> o) pairId (PortDomainPairData maxBound 0 0) (ppDomainPairs pdA)}
                                        , pA
                                        , ptA
                                        )
                                    Nothing ->
                                        let pairId = length pA
                                            ptA' = M.insert (arrId, reqId) pairId ptA
                                            pdA' = pdA{ppDomainPairs = M.insert pairId (PortDomainPairData maxBound 0 0) (ppDomainPairs pdA)}
                                            pA' = pA ++ [PerDomainPair arrId reqId maxBound]
                                         in (pdA', pA', ptA')
                            )
                            (pd, pairsAcc, ptAcc)
                            [(a, r) | a <- M.keys (ppArrival pd), r <- M.keys (ppRequired pd)]
                 in (M.insert key pd' pAcc, pairsAcc', ptAcc')

    updDomain [] _ _ = error "timing: domain index out of range"
    updDomain (x : xs) 0 f = f x : xs
    updDomain (x : xs) i f = x : updDomain xs (i - 1) f

-- identify_related_domains -------------------------------------------------

identifyRelatedDomains ::
    Arch a =>
    a ->
    (CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> (TimingPortClass, Int)) ->
    (CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> Int -> TimingClockingInfo) ->
    (CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> IdString -> Maybe DelayQuad) ->
    M.Map IdString (CellInfo (Bel a) (Wire a) (Pip a)) ->
    M.Map IdString (NetInfo (Bel a) (Wire a) (Pip a)) ->
    [PerDomain] ->
    M.Map (IdString, IdString) DelayT
identifyRelatedDomains arch ptc _pci getDelay cells nets domains =
    let findNetDrivers clk =
            go (M.lookup clk nets) S.empty M.empty 0
          where
            go Nothing _ drivers _ = drivers
            go (Just ni) trace0 drivers acc =
                case prCell (netDriver ni) of
                    Nothing -> drivers
                    Just cell ->
                        case M.lookup cell cells of
                            Nothing -> drivers
                            Just ci ->
                                if S.member (netName ni) trace0
                                    then M.insert (netName ni) acc drivers
                                    else
                                        let trace1 = S.insert (netName ni) trace0
                                            port = prPort (netDriver ni)
                                            (cls, _infoCount) = ptc ci port
                                         in if M.size (cellPorts ci) == 1
                                                then M.insert (netName ni) acc drivers
                                                else
                                                    if cls /= TmgCombOutput
                                                        then M.insert (netName ni) acc drivers
                                                        else
                                                            let (drivers', wentUp) =
                                                                    foldl
                                                                        ( \(drv, up) (pn, pi) ->
                                                                            if portType pi /= PortIn
                                                                                then (drv, up)
                                                                                else
                                                                                    case portNet pi of
                                                                                        Nothing -> (drv, up)
                                                                                        Just upstream ->
                                                                                            let (cls', _) = ptc ci pn
                                                                                             in if cls' /= TmgCombInput
                                                                                                    then (drv, up)
                                                                                                    else
                                                                                                        case getDelay ci pn port of
                                                                                                            Nothing -> (drv, up)
                                                                                                            Just dq -> (go (M.lookup upstream nets) trace1 drv (acc + dqMaxDelay dq), True)
                                                                        )
                                                                        (drivers, False)
                                                                        (M.toList (cellPorts ci))
                                                             in if wentUp then drivers' else M.insert (netName ni) acc drivers'
        clockDrivers = [(pdClock dmn, findNetDrivers (pdClock dmn)) | dmn <- domains, pdClock dmn /= emptyId]
     in M.fromList
            [ ( (c1, c2)
              , M.findWithDefault 0 drv (drivers2) - M.findWithDefault 0 drv (drivers1)
              )
            | (c1, drivers1) <- clockDrivers
            , (c2, drivers2) <- clockDrivers
            , c1 /= c2
            , let common = M.keysSet drivers1 `S.intersection` M.keysSet drivers2
            , S.size common == 1
            , let drv = S.findMin common
            ]

-- reset_times -------------------------------------------------------------

resetTimes :: M.Map CellPortKey PerPort -> M.Map CellPortKey PerPort
resetTimes = M.map resetPort
  where
    resetPort pd =
        pd
            { ppArrival = M.map (const emptyArrivReq) (ppArrival pd)
            , ppRequired = M.map (const emptyArrivReq) (ppRequired pd)
            , ppDomainPairs = M.map (\p -> p{pdpSetupSlack = maxBound, pdpMaxPathLength = 0, pdpCriticality = 0}) (ppDomainPairs pd)
            , ppRouteDelay = dpFromDelay 0
            , ppWorstCrit = 0
            }

-- get_route_delays --------------------------------------------------------

getRouteDelays ::
    Arch a =>
    a ->
    (NetInfo (Bel a) (Wire a) (Pip a) -> Bool) ->
    M.Map IdString (CellInfo (Bel a) (Wire a) (Pip a)) ->
    M.Map IdString (NetInfo (Bel a) (Wire a) (Pip a)) ->
    M.Map CellPortKey PerPort ->
    M.Map CellPortKey PerPort
getRouteDelays arch isGlobalNet cells nets ports0 =
    foldl netStep ports0 (reverse (M.keys nets))
  where
    netStep pAcc netId =
        case M.lookup netId nets of
            Nothing -> pAcc
            Just ni
                | isGlobalNet ni -> pAcc
                | Nothing <- prCell (netDriver ni) -> pAcc
                | Just drvCell <- prCell (netDriver ni), Nothing <- cellBel =<< M.lookup drvCell cells -> pAcc
                | otherwise ->
                    foldl (userStep ni) pAcc [u | Just u <- V.toList (netUsers ni)]

    userStep ni pAcc u =
        let key = CellPortKey (fromMaybe emptyId (prCell u)) (prPort u)
         in case M.lookup key pAcc of
                Just pd
                    | Just dstBel <- prCell u >>= \c -> M.lookup c cells >>= cellBel ->
                        let delay =
                                case prCell (netDriver ni) >>= \c -> M.lookup c cells >>= cellBel of
                                    Just srcBel -> predictDelay arch srcBel (prPort (netDriver ni)) dstBel (prPort u)
                                    Nothing -> 0
                            pd' = pd{ppRouteDelay = dpFromDelay delay}
                         in M.insert key pd' pAcc
                _ -> pAcc

-- walk_forward ------------------------------------------------------------

walkForward ::
    M.Map IdString (CellInfo bel wire pip) ->
    M.Map IdString (NetInfo bel wire pip) ->
    [PerDomain] ->
    [CellPortKey] ->
    Bool ->
    M.Map CellPortKey PerPort ->
    M.Map CellPortKey PerPort
walkForward cells nets domains topo setupOnly ports0 =
    let ports1 = foldl startStep ports0 [0 .. length domains - 1]
        startStep pAcc domId =
            foldl
                ( \pA (sp, clkPort) ->
                    case M.lookup sp pA of
                        Nothing -> pA
                        Just pd ->
                            let initArr = foldl addCkToQ (dpFromDelay 0) (ppArcs pd)
                                addCkToQ acc (CellArc ArcClkToQ other dq _)
                                    | other == clkPort = dpPlus acc (dqDelayPair dq)
                                addCkToQ acc _ = acc
                                clkKey = if clkPort == emptyId then CellPortKey emptyId emptyId else CellPortKey (keyCell sp) clkPort
                                pA' = setArrival setupOnly sp domId initArr 1 clkKey pA
                             in pA'
                )
                pAcc
                (pdStartpoints (domains !! domId))
     in foldl portStep ports1 topo
  where
    keyCell (CellPortKey c _) = c

    portStep pAcc key =
        case M.lookup key pAcc of
            Nothing -> pAcc
            Just pd ->
                if ppType pd == PortOut
                    then case netOf key of
                        Nothing -> pAcc
                        Just ni ->
                            foldl
                                ( \pA (dom, art) ->
                                    foldl
                                        ( \pA' u ->
                                            let key' = CellPortKey (fromMaybe emptyId (prCell u)) (prPort u)
                                             in case M.lookup key' pA' of
                                                    Nothing -> pA'
                                                    Just pd' ->
                                                        let next = dpPlus (artValue art) (ppRouteDelay pd')
                                                         in setArrival setupOnly key' dom next (artPathLength art) key pA'
                                        )
                                        pA
                                        [u | Just u <- V.toList (netUsers ni)]
                                )
                                pAcc
                                (M.toList (ppArrival pd))
                    else
                        foldl
                            ( \pA (dom, art) ->
                                foldl
                                    ( \pA' (CellArc typ other dq _) ->
                                        if typ == ArcCombinational
                                            then
                                                let next = dpPlus (artValue art) (dqDelayPair dq)
                                                 in setArrival setupOnly (CellPortKey (keyCell key) other) dom next (artPathLength art + 1) key pA'
                                            else pA'
                                    )
                                    pA
                                    (ppArcs pd)
                            )
                            pAcc
                            (M.toList (ppArrival pd))

    netOf (CellPortKey cell port) = do
        ci <- M.lookup cell cells
        pi <- M.lookup port (cellPorts ci)
        nid <- portNet pi
        M.lookup nid nets

-- | @set_arrival_time@: max-delay always, min-delay only without
-- @setup_only@.
setArrival :: Bool -> CellPortKey -> Int -> DelayPair -> Int -> CellPortKey -> M.Map CellPortKey PerPort -> M.Map CellPortKey PerPort
setArrival setupOnly target dom arrival pathLength prev ports0 =
    case M.lookup target ports0 of
        Nothing -> ports0
        Just pd ->
            let art0 = M.findWithDefault emptyArrivReq dom (ppArrival pd)
                v0 = artValue art0
                v1 =
                    DelayPair
                        (if not setupOnly && dpMin arrival < dpMin v0 then dpMin arrival else dpMin v0)
                        (if dpMax arrival > dpMax v0 then dpMax arrival else dpMax v0)
                art1 =
                    art0
                        { artValue = v1
                        , artBwdMin = if not setupOnly && dpMin arrival < dpMin v0 then prev else artBwdMin art0
                        , artBwdMax = if dpMax arrival > dpMax v0 then prev else artBwdMax art0
                        , artPathLength = max (artPathLength art0) pathLength
                        }
                pd' = pd{ppArrival = M.insert dom art1 (ppArrival pd)}
             in M.insert target pd' ports0

-- walk_backward -----------------------------------------------------------

walkBackward ::
    M.Map IdString (CellInfo bel wire pip) ->
    M.Map IdString (NetInfo bel wire pip) ->
    [PerDomain] ->
    [CellPortKey] ->
    Bool ->
    M.Map CellPortKey PerPort ->
    M.Map CellPortKey PerPort
walkBackward cells nets domains topo setupOnly ports0 =
    let ports1 = foldl endStep ports0 [0 .. length domains - 1]
        endStep pAcc domId =
            foldl
                ( \pA (ep, clkPort) ->
                    case M.lookup ep pA of
                        Nothing -> pA
                        Just pd ->
                            let initReq = foldl addSetupHold (dpFromDelay 0) (ppArcs pd)
                                addSetupHold acc (CellArc ArcSetup other dq _)
                                    | other == clkPort = acc{dpMin = dpMin acc - dqMaxDelay dq}
                                addSetupHold acc (CellArc ArcHold other dq _)
                                    | other == clkPort = acc{dpMax = dpMax acc + dqMaxDelay dq}
                                addSetupHold acc _ = acc
                                clkKey = if clkPort == emptyId then CellPortKey emptyId emptyId else CellPortKey (keyCell ep) clkPort
                                pA' = setRequired setupOnly ep domId initReq 1 clkKey pA
                             in pA'
                )
                pAcc
                (pdEndpoints (domains !! domId))
     in foldl portStep ports1 (reverse topo)
  where
    keyCell (CellPortKey c _) = c

    portStep pAcc key =
        case M.lookup key pAcc of
            Nothing -> pAcc
            Just pd ->
                if ppType pd == PortIn
                    then case netOf key of
                        Nothing -> pAcc
                        Just ni ->
                            case prCell (netDriver ni) of
                                Nothing -> pAcc
                                Just drvCell ->
                                    foldl
                                        ( \pA (dom, art) ->
                                            let next = dpMinus (artValue art) (dpFromDelay (dpMax (ppRouteDelay pd)))
                                             in setRequired setupOnly (CellPortKey drvCell (prPort (netDriver ni))) dom next (artPathLength art) key pA
                                        )
                                        pAcc
                                        (M.toList (ppRequired pd))
                    else
                        foldl
                            ( \pA (dom, art) ->
                                foldl
                                    ( \pA' (CellArc typ other dq _) ->
                                        if typ == ArcCombinational
                                            then
                                                let next = dpMinus (artValue art) (dpFromDelay (dqMaxDelay dq))
                                                 in setRequired setupOnly (CellPortKey (keyCell key) other) dom next (artPathLength art + 1) key pA'
                                            else pA'
                                    )
                                    pA
                                    (ppArcs pd)
                            )
                            pAcc
                            (M.toList (ppRequired pd))

    netOf (CellPortKey cell port) = do
        ci <- M.lookup cell cells
        pi <- M.lookup port (cellPorts ci)
        nid <- portNet pi
        M.lookup nid nets

-- | @set_required_time@: min-delay always, max-delay only without
-- @setup_only@.
setRequired :: Bool -> CellPortKey -> Int -> DelayPair -> Int -> CellPortKey -> M.Map CellPortKey PerPort -> M.Map CellPortKey PerPort
setRequired setupOnly target dom required pathLength prev ports0 =
    case M.lookup target ports0 of
        Nothing -> ports0
        Just pd ->
            let art0 = M.findWithDefault emptyArrivReq dom (ppRequired pd)
                v0 = artValue art0
                v1 =
                    DelayPair
                        (if dpMin required < dpMin v0 then dpMin required else dpMin v0)
                        (if not setupOnly && dpMax required > dpMax v0 then dpMax required else dpMax v0)
                art1 =
                    art0
                        { artValue = v1
                        , artBwdMin = if dpMin required < dpMin v0 then prev else artBwdMin art0
                        , artBwdMax = if not setupOnly && dpMax required > dpMax v0 then prev else artBwdMax art0
                        , artPathLength = max (artPathLength art0) pathLength
                        }
                pd' = pd{ppRequired = M.insert dom art1 (ppRequired pd)}
             in M.insert target pd' ports0

-- compute_slack -----------------------------------------------------------

computeSlack ::
    [CellPortKey] ->
    [PerDomain] ->
    M.Map (IdString, IdString) DelayT ->
    Bool ->
    M.Map CellPortKey PerPort ->
    [PerDomainPair] ->
    (M.Map CellPortKey PerPort, [PerDomainPair])
computeSlack topo domains clockDelays _setupOnly ports0 pairs0 =
    foldl portStep (ports0, map (\p -> p{pdpWorstSetupSlack = maxBound}) pairs0) topo
  where
    portStep (portsAcc, pairsAcc) key =
        case M.lookup key portsAcc of
            Nothing -> (portsAcc, pairsAcc)
            Just pd ->
                let (pd', pairsAcc') = foldl (pdpStep pd) (pd, pairsAcc) (M.toList (ppDomainPairs pd))
                 in (M.insert key pd' portsAcc, pairsAcc')

    pdpStep pd (pdA, pairsAcc) (pairId, pdp) =
        let pair = pairsAcc !! pairId
            launch = pdpLaunch pair
            capture = pdpCapture pair
            c2c = M.findWithDefault 0 (pdClock (domains !! launch), pdClock (domains !! capture)) clockDelays
            art = M.findWithDefault emptyArrivReq launch (ppArrival pd)
            req = M.findWithDefault emptyArrivReq capture (ppRequired pd)
            slack = 0 - (dpMax (artValue art) - dpMin (artValue req) + c2c)
            pdp' = pdp{pdpSetupSlack = slack, pdpMaxPathLength = artPathLength art + artPathLength req}
            pdA' = pdA{ppDomainPairs = M.insert pairId pdp' (ppDomainPairs pdA)}
            pair' = pair{pdpWorstSetupSlack = min (pdpWorstSetupSlack pair) slack}
            pairsAcc' = updAt pairId pair' pairsAcc
         in (pdA', pairsAcc')

    updAt i x xs = case splitAt i xs of
        (before, _ : after) -> before ++ [x] ++ after
        _ -> xs

-- compute_criticality -----------------------------------------------------

computeCriticality ::
    [CellPortKey] ->
    [PerDomain] ->
    Bool ->
    M.Map CellPortKey PerPort ->
    [PerDomainPair] ->
    M.Map CellPortKey PerPort
computeCriticality topo domains _setupOnly ports0 pairs0 =
    foldl portStep ports0 topo
  where
    portStep pAcc key =
        case M.lookup key pAcc of
            Nothing -> pAcc
            Just pd ->
                let pd' =
                        foldl
                            ( \pdA (pairId, pdp) ->
                                case safeIndex pairs0 pairId of
                                    Nothing -> pdA
                                    Just pair ->
                                        let launchAsync = pdClock (domains !! pdpLaunch pair) == emptyId
                                            captureAsync = pdClock (domains !! pdpCapture pair) == emptyId
                                            crit
                                                | launchAsync || captureAsync = 0
                                                | otherwise =
                                                    let worst = fromIntegral (pdpWorstSetupSlack pair) :: Float
                                                        slack = fromIntegral (pdpSetupSlack pdp) :: Float
                                                        c = 1.0 - (slack - worst) / (-worst)
                                                        -- std::min(c, 1.0f) = (1 < c) ? 1 : c, std::max(c, 0) = (c < 0) ? 0 : c
                                                        -- (the exact forms matter: NaN propagates like C++)
                                                        c1 = if 1 < c then 1 else c
                                                        c2 = if c1 < 0 then 0 else c1
                                                     in c2
                                            pdp' = pdp{pdpCriticality = crit}
                                         in pdA{ppDomainPairs = M.insert pairId pdp' (ppDomainPairs pdA)}
                            )
                            pd
                            (M.toList (ppDomainPairs pd))
                    worstCrit = maximum (0 : [pdpCriticality q | q <- M.elems (ppDomainPairs pd')])
                 in M.insert key pd'{ppWorstCrit = worstCrit} pAcc

    safeIndex xs i = if i < length xs then Just (xs !! i) else Nothing
