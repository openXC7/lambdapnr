{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- | The ECP5 router — the Haskell mirror of @ecp5\/arch.cc@ @Arch::route()@:
@setup_wire_locations@ (@ecp5\/arch_place.cc@), @route_ecp5_globals@
(@ecp5\/globals.cc@, @Ecp5GlobalRouter::route_globals@ +
@route_eclk_sources@) and @router1@ (@common\/route\/router1.cc@).

Call order, mirroring the C++:

* 'setupWireLocations' — the @wire_loc_overrides@ map;
* 'routeEcp5Globals' — binds the DCCA/DCSC clock nets onto the global
  network (STRENGTH_LOCKED bindings);
* 'routeRouter1' — the A* ripup/reroute engine with timing-driven
  criticality (the C++ @Router1@: setup -> main loop; @timing_ripup@ is
  off and @constant_value@ is never set on ECP5, so @route_const_arc@
  is dead).

Determinism contract: the RNG continues the stream left by the placer
(the C++ @ctx->rng@ is shared by every phase); queue tie-breaks use
@rng()@ randtags; @sorted_shuffle@ is applied to net names in setup and
to wire/arc lists in ripups. The arc and A* wire queues replicate
libstdc++'s @std::priority_queue@ push/pop heap mechanics so that
pop order is identical even for equal (score, randtag) keys.

ECP5 simplifications (mirroring @ecp5\/arch.h@): @getConflictingWireWire
w = w@, @getConflictingPipWire = WireId()@, @getWireConstantValue =
IdString()@, @getWireDelay = 0@. The @src_to_net@/@dst_to_arc@ conflict
checks in setup are diagnostics only (they abort the C++ on conflict,
which never happens on this design) and are omitted.
-}
module Lambdapnr.Arch.Ecp5.Router
  ( setupWireLocations
  , routeEcp5Globals
  , routeRouter1
  , estimateDelayR
  , renderWireDump
  ) where

import Control.Exception (evaluate)
import Data.Bits (shiftR, (.&.))
import Control.Monad (foldM, unless, when)
import Data.Foldable (foldl')
import Data.Function (on)
import Data.Int (Int8)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word32)
import System.Environment (lookupEnv)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import System.IO (IOMode (AppendMode, WriteMode), hPutStrLn, openFile, stderr)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

import Lambdapnr.Arch.Ecp5 hiding (estimateDelay)
import Lambdapnr.Arch.Ecp5.ArchCellInfo (ArchInfo, CombInfo (..), assignArchInfo, lookupComb)
import Lambdapnr.Arch.Ecp5.Binding (BindState (..), bindPip, bindWire, boundPipNet, boundWireNet, unbindWire)
import Lambdapnr.Arch.Ecp5.CellTiming (TimingDb (..), getCellDelayAi, getPortClockingInfoAi, getPortTimingClassAi)
import Lambdapnr.Arch.Ecp5.Chipdb
import Lambdapnr.Arch.Ecp5.Types
import Lambdapnr.Kernel.Arch (getBelPinWire, getPipDelay, getPipDstWire, getPipSrcWire, getPipsDownhill, getPipsUphill, getRipupDelayPenalty, getWireDelay, checkPipAvail, checkWireAvail)
import Lambdapnr.Kernel.Delay (DelayT, dqMaxDelay)
import Lambdapnr.Kernel.DeterministicRng
import Lambdapnr.Kernel.IdString (IdString (..), emptyId, idToText)
import Lambdapnr.Kernel.Introsort (stdSortBy)
import Lambdapnr.Kernel.Netlist
import Lambdapnr.Kernel.TimingAnalyser (CellPortKey (..), PerPort (..), PortDomainPairData (..), TimingAnalyser (..), buildTimingAnalyser, criticalityOf, runTimingAnalyser)

-- ---------------------------------------------------------------------------
-- Shared plumbing
-- ---------------------------------------------------------------------------

-- | Constid lookup (mirrors the packer's @cid@).
cidR :: Ecp5 -> T.Text -> IdString
cidR e t = fromMaybe emptyId (M.lookup t (tdConstIdByName (ecp5TimingDb e)))

netOfR :: Design BelId WireId PipId -> IdString -> NetInfo BelId WireId PipId
netOfR d n = fromMaybe (error ("router: missing net " ++ show n)) (lookupNet n d)

cellOfR :: Design BelId WireId PipId -> IdString -> CellInfo BelId WireId PipId
cellOfR d n = fromMaybe (error ("router: missing cell " ++ show n)) (lookupCell n d)

-- | @getNetinfoSourceWire@: the bel pin wire of the net driver.
netinfoSourceWireR :: Ecp5 -> Design BelId WireId PipId -> NetInfo BelId WireId PipId -> Maybe WireId
netinfoSourceWireR e d ni = do
    c <- prCell (netDriver ni)
    ci <- lookupCell c d
    bel <- cellBel ci
    getBelPinWire e bel (prPort (netDriver ni))

-- | @getNetinfoSinkWire(net, sink, 0)@: ECP5 bel pins are 1:1, so the
-- sink wire is the single bel pin wire of the user port.
netinfoSinkWireR :: Ecp5 -> Design BelId WireId PipId -> PortRef -> Maybe WireId
netinfoSinkWireR e d u = do
    c <- prCell u
    ci <- lookupCell c d
    bel <- cellBel ci
    getBelPinWire e bel (prPort u)

-- | @is_clock_port@ (route variant — same predicate as the packer's).
isClockPortR :: Ecp5 -> Design BelId WireId PipId -> PortRef -> Bool
isClockPortR e d u =
    case prCell u of
        Nothing -> False
        Just c ->
            let t = cellType (cellOfR d c)
                p = prPort u
             in (t == cidR e "TRELLIS_FF" && p == cidR e "CLK")
                    || (t == cidR e "TRELLIS_COMB" && p == cidR e "WCK")
                    || ( t == cidR e "DCUA"
                            && p `elem` map (cidR e) ["CH0_FF_RXI_CLK", "CH1_FF_RXI_CLK", "CH0_FF_TXI_CLK", "CH1_FF_TXI_CLK"]
                       )
                    || ((t == cidR e "IOLOGIC" || t == cidR e "SIOLOGIC") && p == cidR e "CLK")

-- | @is_logic_port@.
isLogicPortR :: Ecp5 -> Design BelId WireId PipId -> PortRef -> Bool
isLogicPortR e d u =
    case prCell u of
        Nothing -> False
        Just c ->
            let t = cellType (cellOfR d c)
                p = prPort u
             in (t == cidR e "TRELLIS_FF" && p /= cidR e "CLK")
                    || (t == cidR e "TRELLIS_COMB" && p /= cidR e "WCK")

-- | Bind a wire, threading the arch bind state and the design.
bindWireR :: WireId -> IdString -> PlaceStrength -> Ecp5 -> Design BelId WireId PipId -> (Ecp5, Design BelId WireId PipId)
bindWireR w net strength e d =
    let (bs', d') = bindWire net w strength (ecp5Bind e) d
     in (setEcp5Bind bs' e, d')

-- | Bind a pip (and its dst wire).
bindPipR :: PipId -> IdString -> PlaceStrength -> Ecp5 -> Design BelId WireId PipId -> (Ecp5, Design BelId WireId PipId)
bindPipR p net strength e d =
    let (bs', d') = bindPip (ecp5Chipdb e) net p strength (ecp5Bind e) d
     in (setEcp5Bind bs' e, d')

-- | Unbind a wire (and its uphill pip, if any).
unbindWireR :: WireId -> Ecp5 -> Design BelId WireId PipId -> (Ecp5, Design BelId WireId PipId)
unbindWireR w e d =
    let (bs', d') = unbindWire (ecp5Chipdb e) w (ecp5Bind e) d
     in (setEcp5Bind bs' e, d')

-- | Active user slots with their store indices (@enumerate@).
activeSlots :: V.Vector (Maybe PortRef) -> [(Int, PortRef)]
activeSlots v = [(i, u) | (i, Just u) <- zip [0 ..] (V.toList v)]

-- | Active users in slot order.
activeUsersR :: V.Vector (Maybe PortRef) -> [PortRef]
activeUsersR = foldr (\u acc -> maybe acc (: acc) u) [] . V.toList

-- | @ci->getPort@.
getPortR :: CellInfo bel wire pip -> IdString -> Maybe IdString
getPortR ci p = portNet =<< M.lookup p (cellPorts ci)

-- ---------------------------------------------------------------------------
-- setup_wire_locations (ecp5/arch_place.cc)
-- ---------------------------------------------------------------------------

-- | @Arch::setup_wire_locations@: for the special hard-IP cell types,
-- override each connected bel pin wire's estimated location with the
-- location of its first downhill (output) or uphill (input) pip.
setupWireLocations :: Ecp5 -> Design BelId WireId PipId -> M.Map WireId (Int, Int)
setupWireLocations e d =
    M.fromList (concatMap cellEntry (cellsIter d))
  where
    special = map (cidR e) ["ALU54B", "MULT18X18D", "DCUA", "DDRDLL", "DQSBUFM", "EHXPLLL"]
    cellEntry ci
        | cellBel ci == Nothing = []
        | cellType ci `notElem` special = []
        -- the C++ iterates ci->ports (a nextpnr dict = reverse insertion
        -- order); each port contributes a distinct wire, so the fold order
        -- only matters for the impossible duplicate-wire case, where we
        -- replicate last-write-wins.
        | otherwise = portEntries ci (reverse (cellPortOrder ci))
    portEntries ci [] = []
    portEntries ci (pn : pns) =
        case M.lookup pn (cellPorts ci) of
            Nothing -> portEntries ci pns
            Just pi ->
                case portNet pi of
                    Nothing -> portEntries ci pns
                    Just _ ->
                        case getBelPinWire e (fromMaybe (error "router: cell without bel") (cellBel ci)) pn of
                            Nothing -> portEntries ci pns
                            Just pw ->
                                let entry
                                        | portType pi == PortOut =
                                            case getPipsDownhill e pw of
                                                (dh : _) ->
                                                    let dstW = getPipDstWire e dh
                                                     in Just (pw, (fromIntegral (locX (wireLoc dstW)), fromIntegral (locY (wireLoc dstW))))
                                                [] -> Nothing
                                        | otherwise =
                                            case getPipsUphill e pw of
                                                (uh : _) ->
                                                    let srcW = getPipSrcWire e uh
                                                     in Just (pw, (fromIntegral (locX (wireLoc srcW)), fromIntegral (locY (wireLoc srcW))))
                                                [] -> Nothing
                                 in maybe (portEntries ci pns) (: portEntries ci pns) entry

-- ---------------------------------------------------------------------------
-- estimateDelay with wire_loc_overrides + gsrclk_wire (router variant)
-- ---------------------------------------------------------------------------

-- | @est_location@ with the @gsrclk_wire@ special case.
estLocationR :: Maybe WireId -> Ecp5 -> WireId -> (Int, Int)
estLocationR gsrclk e w
    | Just g <- gsrclk, g == w =
        let uh = head (getPipsUphill e w)
            phys = getPipSrcWire e uh
         in (fromIntegral (locX (wireLoc phys)), fromIntegral (locY (wireLoc phys)))
    | otherwise =
        let wi = wireAt cd w
            (x, y) = (fromIntegral (locX (wireLoc w)), fromIntegral (locY (wireLoc w)))
         in if not (V.null (wiBelPins wi))
                then (x + fromIntegral (bpRelDx (V.head (wiBelPins wi))), y + fromIntegral (bpRelDy (V.head (wiBelPins wi))))
                else
                    if not (V.null (wiPipsDownhill wi))
                        then (x + fromIntegral (plRelDx (V.head (wiPipsDownhill wi))), y + fromIntegral (plRelDy (V.head (wiPipsDownhill wi))))
                        else
                            if not (V.null (wiPipsUphill wi))
                                then (x + fromIntegral (plRelDx (V.head (wiPipsUphill wi))), y + fromIntegral (plRelDy (V.head (wiPipsUphill wi))))
                                else (x, y)
  where
    cd = ecp5Chipdb e

-- | @Arch::estimateDelay@ including the router-time @wire_loc_overrides@
-- and the @num_uh < 6@ direct-pip short-circuit.
estimateDelayR :: M.Map WireId (Int, Int) -> Maybe WireId -> Ecp5 -> WireId -> WireId -> DelayT
estimateDelayR overrides gsrclk e src dst =
    case direct of
        Just d -> d
        Nothing ->
            let (sx, sy) = estLocationR gsrclk e src
                (dx', dy') = maybe (estLocationR gsrclk e dst) id (M.lookup dst overrides)
             in delayFormulaE (abs (sx - dx')) (abs (sy - dy'))
  where
    wi = wireAt (ecp5Chipdb e) dst
    direct
        | V.length (wiPipsUphill wi) < 6 =
            listToMaybe
                [ dqMaxDelay (getPipDelay e uh)
                | uh <- getPipsUphill e dst
                , getPipSrcWire e uh == src
                ]
        | otherwise = Nothing
    delayFormulaE dx dy =
        let base = 80 - 9 * speedToInt (eaSpeed (ecp5Args e))
            f dd = max (dd - 5) 0 + 2 * min dd 5
         in fromIntegral (base * (6 + f dx + f dy))

-- ---------------------------------------------------------------------------
-- route_ecp5_globals (ecp5/globals.cc)
-- ---------------------------------------------------------------------------

quadName :: Int8 -> T.Text
quadName q = case q of
    0 -> "UL"
    1 -> "UR"
    2 -> "LL"
    _ -> "LR"

routeEcp5Globals :: Ecp5 -> Design BelId WireId PipId -> (Ecp5, Design BelId WireId PipId)
routeEcp5Globals e0 d0 =
    let (e1, d1) = routeGlobalsR e0 d0
        (e2, d2) = routeEclkSourcesR e1 d1
     in (e2, d2)

-- | @route_globals@.
routeGlobalsR :: Ecp5 -> Design BelId WireId PipId -> (Ecp5, Design BelId WireId PipId)
routeGlobalsR e0 d0 =
    let (_, _, toroute0, clocks0, e1, d1) = foldl' step init0 (cellsIter d0)
        -- std::sort by priority with the exact libstdc++ introsort
        -- permutation (UNSTABLE: equal-priority users get reordered the
        -- same way GCC does; the ROUTING strings in --write output depend
        -- on it).
        sorted = stdSortBy (\(u1, _) (u2, _) -> globalRoutePriorityR e1 d1 u1 < globalRoutePriorityR e1 d1 u2) toroute0
        _ = lpTorouteDump e1 d1 sorted
        (e2, d2) = foldl' (routeUser clocks0) (e1, d1) sorted
     in (e2, d2)
  where
    dcca = cidR e0 "DCCA"
    dcsc = cidR e0 "DCSC"
    clko = cidR e0 "CLKO"
    dcsout = cidR e0 "DCSOUT"
    clk0 = cidR e0 "CLK0"
    clk1 = cidR e0 "CLK1"
    init0 = (S.fromList [0 .. 15], S.fromList [0 .. 7], [], M.empty, e0, d0)
    step (allG, fabG, toroute, clocks, e, d) ci
        | cellType ci /= dcca && cellType ci /= dcsc = (allG, fabG, toroute, clocks, e, d)
        | otherwise =
            let outP = if cellType ci == dcsc then dcsout else clko
             in case getPortR ci outP of
                    Nothing -> error "route_globals: DCC/DCS cell without an output net"
                    Just clock ->
                        let ni = netOfR d clock
                            drivesFab = any (not . isClockPortR e d) (activeUsersR (netUsers ni))
                         in if drivesFab && S.null fabG
                                then (allG, fabG, toroute, clocks, e, d)
                                else
                                    let glbid = if drivesFab then S.findMin fabG else S.findMin allG
                                        (e', d', routed) = routeOntoGlobalR e d clock glbid
                                        _ = if not routed then error "route_globals: failed to route onto global" else ()
                                        toroute' = toroute ++ [(u, glbid) | u <- activeUsersR (netUsers ni)]
                                     in (S.delete glbid allG, S.delete glbid fabG, toroute', M.insert glbid clock clocks, e', d')
    routeUser clocks0 (e, d) (u, glbid) =
        let ciName = fromMaybe (error "route_globals: dangling user") (prCell u)
            ci = cellOfR d ciName
            net = fromMaybe (error "route_globals: missing clock") (M.lookup glbid clocks0)
         in if cellType ci == dcsc && prPort u `elem` [clk0, clk1]
                then
                    let ni = netOfR d net
                        srcW = fromMaybe (error "route_globals: no source wire") (netinfoSourceWireR e d ni)
                        dstW = fromMaybe (error "route_globals: no sink wire") (netinfoSinkWireR e d u)
                        (e', d', _) = simpleRouterR e d net srcW dstW False
                     in (e', d')
                else routeLogicTileGlobalR e d net glbid u

-- | LPDBG: dump the sorted toroute order (LPCHK_ROUTE_TOROUTE).
lpTorouteDump :: Ecp5 -> Design BelId WireId PipId -> [(PortRef, Int)] -> ()
lpTorouteDump e d sorted = unsafePerformIO $ do
    mf <- lookupEnv "LPCHK_ROUTE_TOROUTE"
    case mf of
        Nothing -> pure ()
        Just f ->
            writeFile f
                ( "N " ++ show (length sorted)
                    ++ "\n"
                    ++ concat
                        [ "U "
                            ++ show (unIdString (fromMaybe (error "toroute: dangling user") (prCell u)))
                            ++ " "
                            ++ show (unIdString (prPort u))
                            ++ " "
                            ++ show glbid
                            ++ " "
                            ++ show (globalRoutePriorityR e d u)
                            ++ "\n"
                        | (u, glbid) <- sorted
                        ]
                )

-- | @global_route_priority@: WCK/WRE loads route first (90 vs 99).
globalRoutePriorityR :: Ecp5 -> Design BelId WireId PipId -> PortRef -> Int
globalRoutePriorityR e _ u
    | prPort u `elem` [cidR e "WCK", cidR e "WRE"] = 90
    | otherwise = 99

-- | @route_onto_global@: route the DCC output onto all four quadrant
-- spine networks of the given global index.
routeOntoGlobalR :: Ecp5 -> Design BelId WireId PipId -> IdString -> Int -> (Ecp5, Design BelId WireId PipId, Bool)
routeOntoGlobalR e0 d0 net network =
    let glbSrc = fromMaybe (error "route_onto_global: no source wire") (netinfoSourceWireR e0 d0 (netOfR d0 net))
        step (e, d, ok) q
            | not ok = (e, d, ok)
            | otherwise =
                let glbDst = getGlobalWireR e q network
                    (e', d', ok') = simpleRouterR e d net glbSrc glbDst False
                 in (e', d', ok')
     in foldl' step (e0, d0, True) [0 .. 3]

-- | @get_global_wire@: the quadrant PCLK spine wire at (0,0).
getGlobalWireR :: Ecp5 -> Int -> Int -> WireId
getGlobalWireR e quad network =
    fromMaybe
        (error "get_global_wire: no such wire")
        (getWireByLocBasename e (Location 0 0) ("G_" <> quadName (fromIntegral quad) <> "PCLK" <> T.pack (show network)))

-- | @simple_router@: BFS downhill from @src@ to @dst@ over available
-- wires, binding the backtrace (LOCKED).
simpleRouterR ::
    Ecp5 ->
    Design BelId WireId PipId ->
    IdString ->
    WireId ->
    WireId ->
    Bool ->
    (Ecp5, Design BelId WireId PipId, Bool)
simpleRouterR e0 d0 net src dst allowFail = search [src] M.empty e0 d0
  where
    search visit bt e d
        | null visit || length visit > 50000 =
            if allowFail
                then (e, d, False)
                else error ("cannot route global from " ++ show src ++ " to " ++ show dst)
        | otherwise =
            let cursor = head visit
                rest = tail visit
                bound = boundWireNet cursor (ecp5Bind e)
             in if bound /= Nothing && bound /= Just net
                    then search rest bt e d
                    else
                        if cursor == dst
                            then let (e', d') = bindBack cursor bt e d in (e', d', True)
                            else
                                let (rest', bt') =
                                        foldl'
                                            (\(q, b) dh ->
                                                let pipDst = getPipDstWire e dh
                                                 in if M.member pipDst b
                                                        then (q, b)
                                                        else (q ++ [pipDst], M.insert pipDst dh b))
                                            (rest, bt)
                                            (getPipsDownhill e cursor)
                                 in search rest' bt' e d
    bindBack cursor bt e d =
        let (e', d') = bindLoop cursor bt e d
         in if boundWireNet src (ecp5Bind e') == Nothing
                then bindWireR src net StrengthLocked e' d'
                else (e', d')
    bindLoop cursor bt e d =
        case M.lookup cursor bt of
            Nothing -> (e, d)
            Just pip ->
                let bound = boundWireNet cursor (ecp5Bind e)
                 in if bound /= Nothing
                        then (e, d) -- C++ asserts bound == net here
                        else
                            let (e', d') = bindPipR pip net StrengthLocked e d
                             in bindLoop (getPipSrcWire e' pip) bt e' d'

-- | @find_tap_pip@.
findTapPipR :: Ecp5 -> WireId -> PipId
findTapPipR e tileGlb =
    let wireName = getWireBasename e tileGlb
        glbName = T.drop 2 wireName
        gi = globalInfoAtLoc (ecp5Chipdb e) (wireLoc tileGlb)
        tapLoc = Location (giTapCol gi) (locY (wireLoc tileGlb))
        prefix = if giTapDir gi == 0 then "L_" else "R_"
        tapWire =
            fromMaybe
                (error "find_tap_pip: no tap wire")
                (getWireByLocBasename e tapLoc (prefix <> glbName))
     in fromMaybe (error "find_tap_pip: no uphill pip") (listToMaybe (getPipsUphill e tapWire))

-- | @find_spine_pip@.
findSpinePipR :: Ecp5 -> WireId -> PipId
findSpinePipR e tapWire =
    let wireName = getWireBasename e tapWire
        gi = globalInfoAtLoc (ecp5Chipdb e) (wireLoc tapWire)
        spineLoc = Location (giSpineCol gi) (giSpineRow gi)
        spineWire =
            fromMaybe
                (error "find_spine_pip: no spine wire")
                (getWireByLocBasename e spineLoc wireName)
     in fromMaybe (error "find_spine_pip: no uphill pip") (listToMaybe (getPipsUphill e spineWire))

-- | @route_logic_tile_global@: search uphill from a user's bel pin wire
-- until the tile's global network, then bind the path (LOCKED).
routeLogicTileGlobalR ::
    Ecp5 ->
    Design BelId WireId PipId ->
    IdString ->
    Int ->
    PortRef ->
    (Ecp5, Design BelId WireId PipId)
routeLogicTileGlobalR e0 d0 net globalIndex user =
    let ciName = fromMaybe (error "route_logic_tile_global: dangling user") (prCell user)
        ci = cellOfR d0 ciName
        userWire =
            fromMaybe
                (error "route_logic_tile_global: no pin wire")
                (getBelPinWire e0 (fromMaybe (error "route_logic_tile_global: unplaced cell") (cellBel ci)) (prPort user))
        (nextW, alreadyRouted, bt, e1, d1) = search [userWire] M.empty e0 d0
        (e2, d2) = bindBack nextW bt e1 d1
        (e3, d3)
            | not alreadyRouted =
                let (eA, dA) = bindWireR nextW net StrengthLocked e2 d2
                    tapPip = findTapPipR eA nextW
                    (eB, dB) =
                        if boundPipNet tapPip (ecp5Bind eA) == Nothing
                            then
                                let (eB1, dB1) = bindPipR tapPip net StrengthLocked eA dA
                                    spinePip = findSpinePipR eB1 (getPipSrcWire eB1 tapPip)
                                    (eB2, dB2) =
                                        if boundPipNet spinePip (ecp5Bind eB1) == Nothing
                                            then bindPipR spinePip net StrengthLocked eB1 dB1
                                            else (eB1, dB1) -- C++ asserts == net here
                                 in (eB2, dB2)
                            else (eA, dA) -- C++ asserts == net here
                 in (eB, dB)
            | otherwise = (e2, d2)
     in (e3, d3)
  where
    globalName = T.pack (printf "G_HPBX%02d00" globalIndex)
    search upstream bt e d
        | null upstream = error ("route_logic_tile_global: no upstream path for " ++ show net)
        | length upstream > 30000 = error ("failed to route HPBX" ++ show globalIndex ++ "00")
        | otherwise =
            let nextW = head upstream
                rest = tail upstream
             in if boundWireNet nextW (ecp5Bind e) == Just net
                    then (nextW, True, bt, e, d)
                    else
                        if getWireBasename e nextW == globalName
                            then (nextW, False, bt, e, d)
                            else
                                if checkWireAvail e nextW
                                    then
                                        let (rest', bt') =
                                                foldl'
                                                    (\(q, b) pip ->
                                                        let srcW = getPipSrcWire e pip
                                                         in if M.member srcW b
                                                                then (q, b)
                                                                else (q ++ [srcW], M.insert srcW pip b))
                                                    (rest, bt)
                                                    (getPipsUphill e nextW)
                                         in search rest' bt' e d
                                    else search rest bt e d
    bindBack cursor bt e d =
        case M.lookup cursor bt of
            Nothing -> (e, d)
            Just pip ->
                let (e', d') = bindPipR pip net StrengthLocked e d
                 in bindBack (getPipDstWire e' pip) bt e' d'

-- | @route_eclk_sources@: best-effort dedicated routing for edge clock
-- sources (none present in the reference design; ported for completeness).
routeEclkSourcesR :: Ecp5 -> Design BelId WireId PipId -> (Ecp5, Design BelId WireId PipId)
routeEclkSourcesR e0 d0 = foldl' cellStep (e0, d0) (cellsIter d0)
  where
    eclksyncb = cidR e0 "ECLKSYNCB"
    eclkbuf = cidR e0 "TRELLIS_ECLKBUF"
    bridge = cidR e0 "ECLKBRIDGECS"
    cellStep (e, d) ci
        | cellType ci `notElem` [eclksyncb, eclkbuf, bridge] = (e, d)
        | otherwise =
            let pins =
                    if cellType ci `elem` [eclksyncb, eclkbuf]
                        then [cidR e "ECLKI"]
                        else [cidR e "CLK0", cidR e "CLK1"]
             in foldl' (pinStep ci) (e, d) pins
    pinStep ci (e, d) pin =
        case getPortR ci pin of
            Nothing -> (e, d)
            Just net ->
                case netinfoSourceWireR e d (netOfR d net) of
                    Nothing -> (e, d)
                    Just srcW ->
                        let dstW = fromMaybe (error "route_eclk_sources: no pin wire") (getBelPinWire e (fromMaybe (error "route_eclk_sources: unplaced cell") (cellBel ci)) pin)
                         in eclkSearch e d net srcW dstW
    eclkSearch e0' d0' net srcW dstW = go 0 [dstW] M.empty e0' d0'
      where
        iterMax = 1000
        go iter visit bt e d
            | iter >= iterMax || null visit = (e, d)
            | otherwise =
                let cursor = head visit
                    rest = tail visit
                    bound = boundWireNet cursor (ecp5Bind e)
                 in if bound /= Nothing
                        then
                            if bound == Just net
                                then bindPhase cursor bt e d
                                else go (iter + 1) rest bt e d
                        else
                            if cursor == srcW
                                then let (e', d') = bindWireR cursor net StrengthLocked e d in bindPhase cursor bt e' d'
                                else
                                    let (rest', bt') =
                                            foldl'
                                                (\(q, b) uh ->
                                                    if not (checkPipAvail e uh)
                                                        then (q, b)
                                                        else
                                                            let src' = getPipSrcWire e uh
                                                             in if M.member src' b
                                                                    then (q, b)
                                                                    else
                                                                        if "ECLKCIB" `T.isInfixOf` getWireBasename e src'
                                                                            then (q, b)
                                                                            else (q ++ [src'], M.insert src' uh b))
                                                (rest, bt)
                                                (getPipsUphill e cursor)
                                     in go (iter + 1) rest' bt' e d
        bindPhase cursor bt e d =
            let (e', d') = bindPips cursor bt e d
             in (e', d')
        bindPips cursor bt e d
            | cursor == dstW = (e, d)
            | otherwise =
                case M.lookup cursor bt of
                    Nothing -> error "route_eclk_sources: broken backtrace"
                    Just pip ->
                        let (e', d') = bindPipR pip net StrengthLocked e d
                         in bindPips (getPipDstWire e' pip) bt e' d'

-- ---------------------------------------------------------------------------
-- router1 (common/route/router1.cc)
-- ---------------------------------------------------------------------------

-- | @Router1Cfg@ (the unused @maxIterCnt@/cleanup flags are omitted).
data RouterCfg = RouterCfg
    { rcWireRipupPenalty :: !DelayT
    , rcNetRipupPenalty :: !DelayT
    , rcReuseBonus :: !DelayT
    , rcEstimatePrecision :: !DelayT
    , rcUseEstimate :: !Bool
    }

-- | @arc_key@: (net, user slot, phys index).
data ArcKey = ArcKey !IdString !Int !Int
    deriving (Eq, Ord, Show)

akNet :: ArcKey -> IdString
akNet (ArcKey n _ _) = n

akSlot :: ArcKey -> Int
akSlot (ArcKey _ s _) = s

akPhys :: ArcKey -> Int
akPhys (ArcKey _ _ p) = p

-- | @arc_entry@.
data ArcEntry = ArcEntry !ArcKey !DelayT !Word32
    deriving (Show)

-- | @arc_entry::Less@ (top = max pri, tie max randtag).
arcEntryLess :: ArcEntry -> ArcEntry -> Bool
arcEntryLess (ArcEntry _ p1 t1) (ArcEntry _ p2 t2)
    | p1 /= p2 = p1 < p2
    | otherwise = t1 < t2

-- | @QueuedWire@.
data QueuedWire = QueuedWire
    { qwWire :: !WireId
    , qwPip :: !(Maybe PipId)
    , qwDelay :: !DelayT
    , qwPenalty :: !DelayT
    , qwBonus :: !DelayT
    , qwTogo :: !DelayT
    , qwRandtag :: !Word32
    }
    deriving (Show)

qwScore :: QueuedWire -> DelayT
qwScore q = qwDelay q + qwPenalty q + qwTogo q - qwBonus q

-- | @QueuedWire::Greater@ (top = min score, tie min randtag).
queuedWireGreater :: QueuedWire -> QueuedWire -> Bool
queuedWireGreater l r =
    let ls = qwScore l
        rs = qwScore r
     in if ls == rs then qwRandtag l > qwRandtag r else ls > rs

-- ---------------------------------------------------------------------------
-- libstdc++ std::priority_queue heap mechanics
-- (push_heap / pop_heap with the given comparator)
-- ---------------------------------------------------------------------------

heapPush :: (a -> a -> Bool) -> Seq.Seq a -> a -> Seq.Seq a
heapPush comp sq x =
    let n = Seq.length sq
        sq0 = sq Seq.|> x
        path = takeWhile (> 0) (iterate (\h -> (h - 1) `quot` 2) n)
        steps = takeWhile (\h -> comp (Seq.index sq0 ((h - 1) `quot` 2)) x) path
        sq1 = foldl' (\s h -> Seq.update h (Seq.index s ((h - 1) `quot` 2)) s) sq0 steps
        finalHole = case steps of
            [] -> n
            _ -> (last steps - 1) `quot` 2
     in Seq.update finalHole x sq1

heapPop :: (a -> a -> Bool) -> Seq.Seq a -> (a, Seq.Seq a)
heapPop comp sq
    | Seq.null sq = error "heapPop: empty heap"
    | n == 1 = (Seq.index sq 0, Seq.empty)
    | otherwise =
        let top = Seq.index sq 0
            val = Seq.index sq (n - 1)
            rest0 = Seq.take (n - 1) sq
            len = n - 1
            (hole, rest1) = adjust comp len rest0 0 0
         in (top, pushUp comp hole rest1 val)
  where
    n = Seq.length sq
    adjust comp len sq' hole sc
        | sc < (len - 1) `quot` 2 =
            let sc' = 2 * (sc + 1)
                scSel = if comp (Seq.index sq' sc') (Seq.index sq' (sc' - 1)) then sc' - 1 else sc'
                sq'' = Seq.update hole (Seq.index sq' scSel) sq'
             in adjust comp len sq'' scSel scSel
        | otherwise =
            if (len .&. 1) == 0 && sc == (len - 2) `quot` 2
                then
                    let sc' = 2 * (sc + 1)
                     in (sc' - 1, Seq.update hole (Seq.index sq' (sc' - 1)) sq')
                else (hole, sq')
    pushUp comp hole sq' x =
        let path = takeWhile (> 0) (iterate (\h -> (h - 1) `quot` 2) hole)
            steps = takeWhile (\h -> comp (Seq.index sq' ((h - 1) `quot` 2)) x) path
            sq'' = foldl' (\s h -> Seq.update h (Seq.index s ((h - 1) `quot` 2)) s) sq' steps
            finalHole = case steps of
                [] -> hole
                _ -> (last steps - 1) `quot` 2
         in Seq.update finalHole x sq''

-- ---------------------------------------------------------------------------
-- Router1 state
-- ---------------------------------------------------------------------------

data RState = RState
    { rsArch :: !Ecp5
    , rsDesign :: !(Design BelId WireId PipId)
    , rsRng :: !Rng
    , rsCfg :: !RouterCfg
    , rsArcQueue :: !(Seq.Seq ArcEntry)
    , rsWireToArcs :: !(M.Map WireId (S.Set ArcKey))
    , rsArcToWires :: !(M.Map ArcKey [WireId])
    -- ^ per-arc wire pool (the C++ pool: insertion order, swap-erase,
    -- iterated in REVERSE by the arc-ripup unbind loop)
    , rsQueuedArcs :: !(S.Set ArcKey)
    , rsWireScores :: !(M.Map WireId Int)
    , rsNetScores :: !(M.Map IdString Int)
    , rsArcsWithRipup :: !Int
    , rsArcsWithoutRipup :: !Int
    , rsRipupFlag :: !Bool
    , rsTmg :: !TimingAnalyser
    , rsOverrides :: !(M.Map WireId (Int, Int))
    , rsGsrclk :: !(Maybe WireId)
    , rsWireQueue :: !(Seq.Seq QueuedWire)
    , rsVisited :: !(M.Map WireId QueuedWire)
    }

-- | Sorted shuffle that stores the rng back into the state.
sortedShuffleSt :: (Ord a) => Rng -> V.Vector a -> RState -> (V.Vector a, RState)
sortedShuffleSt r v st =
    let (v', r') = sortedShuffle r v
     in (v', st{rsRng = r'})

-- | @pool::insert@: append-if-absent (insertion order preserved).
poolInsert :: Eq a => [a] -> [a] -> [a]
poolInsert new old = foldl' (\acc x -> if x `elem` acc then acc else acc ++ [x]) old new

-- | @skip_net@ (ECP5: @constant_value@ is never set).
skipNetS :: RState -> NetInfo BelId WireId PipId -> Bool
skipNetS st ni =
    isGlobal || prCell (netDriver ni) == Nothing
  where
    isGlobal = isJust (M.lookup (cidR (rsArch st) "ECP5_IS_GLOBAL") (netAttrs ni))

-- | @arc_queue_insert(arc, src, dst)@.
arcQueueInsertWithS :: ArcKey -> WireId -> WireId -> RState -> RState
arcQueueInsertWithS arc srcW dstW st
    | S.member arc (rsQueuedArcs st) = st
    | otherwise =
        let pri = arcPriS st arc srcW dstW
            (tag, r') = rng30 (rsRng st)
         in st
                { rsArcQueue = heapPush arcEntryLess (rsArcQueue st) (ArcEntry arc pri tag)
                , rsQueuedArcs = S.insert arc (rsQueuedArcs st)
                , rsRng = r'
                }

-- | @arc_queue_insert(arc)@.
arcQueueInsertS :: ArcKey -> RState -> RState
arcQueueInsertS arc st
    | S.member arc (rsQueuedArcs st) = st
    | otherwise =
        let ni = netOfR (rsDesign st) (akNet arc)
            srcW = fromMaybe (error "arc_queue_insert: no source wire") (netinfoSourceWireR (rsArch st) (rsDesign st) ni)
            u = fromMaybe (error "arc_queue_insert: inactive user") (netUsers ni V.! akSlot arc)
            dstW = fromMaybe (error "arc_queue_insert: no sink wire") (netinfoSinkWireR (rsArch st) (rsDesign st) u)
            pri = arcPriS st arc srcW dstW
            (tag, r') = rng30 (rsRng st)
         in st
                { rsArcQueue = heapPush arcEntryLess (rsArcQueue st) (ArcEntry arc pri tag)
                , rsQueuedArcs = S.insert arc (rsQueuedArcs st)
                , rsRng = r'
                }

-- | The queue priority: @estimateDelay * (100 * criticality)@ (float
-- math truncated to int, exactly like the C++).
arcPriS :: RState -> ArcKey -> WireId -> WireId -> DelayT
arcPriS st arc srcW dstW =
    let u = fromMaybe (error "arcPriS: inactive user") (netUsers (netOfR (rsDesign st) (akNet arc)) V.! akSlot arc)
        crit = criticalityOf (rsTmg st) (CellPortKey (fromMaybe emptyId (prCell u)) (prPort u))
        est = estimateDelayR (rsOverrides st) (rsGsrclk st) (rsArch st) srcW dstW
     in truncate (fromIntegral est * (100 * crit))

-- | @ripup_net@.
ripupNetS :: IdString -> RState -> RState
ripupNetS net st0 =
    let st1 = st0{rsNetScores = M.insertWith (+) net 1 (rsNetScores st0), rsRipupFlag = True}
        ni = netOfR (rsDesign st1) net
        (wiresV, st2) = sortedShuffleSt (rsRng st1) (V.fromList (M.keys (netWires ni))) st1
     in foldl' ripupWireOfNet st2 (V.toList wiresV)
  where
    ripupWireOfNet st w =
        let arcsSet = M.findWithDefault S.empty w (rsWireToArcs st)
            st1 =
                st
                    { rsArcToWires = foldl' (\m a -> M.adjust (swapRemovePort w) a m) (rsArcToWires st) (S.toList arcsSet)
                    , rsWireToArcs = M.insert w S.empty (rsWireToArcs st)
                    }
            (arcsV, st2) = sortedShuffleSt (rsRng st1) (V.fromList (S.toAscList arcsSet)) st1
            st3 = foldl' (flip arcQueueInsertS) st2 (V.toList arcsV)
            (bs', d') = unbindWire (ecp5Chipdb (rsArch st3)) w (ecp5Bind (rsArch st3)) (rsDesign st3)
         in st3
                { rsArch = setEcp5Bind bs' (rsArch st3)
                , rsDesign = d'
                , rsWireScores = M.insertWith (+) w 1 (rsWireScores st3)
                }

-- | @ripup_wire@ (ECP5: @getConflictingWireWire w = w@ — always the
-- wire-level branch).
ripupWireS :: WireId -> RState -> RState
ripupWireS wire st0 =
    let st1 = st0{rsRipupFlag = True}
        arcsSet = M.findWithDefault S.empty wire (rsWireToArcs st1)
        st2 =
            st1
                { rsArcToWires = foldl' (\m a -> M.adjust (swapRemovePort wire) a m) (rsArcToWires st1) (S.toList arcsSet)
                , rsWireToArcs = M.insert wire S.empty (rsWireToArcs st1)
                }
        (arcsV, st3) = sortedShuffleSt (rsRng st2) (V.fromList (S.toAscList arcsSet)) st2
        st4 = foldl' (flip arcQueueInsertS) st3 (V.toList arcsV)
        (bs', d') = unbindWire (ecp5Chipdb (rsArch st4)) wire (ecp5Bind (rsArch st4)) (rsDesign st4)
     in st4
            { rsArch = setEcp5Bind bs' (rsArch st4)
            , rsDesign = d'
            , rsWireScores = M.insertWith (+) wire 1 (rsWireScores st4)
            }

-- | @ripup_pip@ (ECP5: @getConflictingPipWire = WireId()@ — rip up the
-- net bound to the pip).
ripupPipS :: PipId -> RState -> RState
ripupPipS pip st =
    let st' = st{rsRipupFlag = True}
     in case boundPipNet pip (ecp5Bind (rsArch st)) of
            Nothing -> st'
            Just n -> ripupNetS n st'

-- | @setup@.
setupS :: RState -> RState
setupS st0 =
    let netNames = V.fromList (map netName (netsIter (rsDesign st0)))
        (namesV, st1) = sortedShuffleSt (rsRng st0) netNames st0
     in foldl' setupNet st1 (V.toList namesV)
  where
    setupNet st nm =
        let ni = netOfR (rsDesign st) nm
         in if skipNetS st ni
                then st
                else
                    case netinfoSourceWireR (rsArch st) (rsDesign st) ni of
                        Nothing -> error ("No wire found for port on source cell of net " ++ show nm)
                        Just srcW ->
                            let (stUsers, _dstMap) = foldl' (setupUser ni srcW) (st, M.empty) (activeSlots (netUsers ni))
                                weakWires =
                                    [ w
                                    | (w, PipMap _ strength) <- M.toList (netWires (netOfR (rsDesign stUsers) nm))
                                    , strength < StrengthLocked
                                    , M.notMember w (rsWireToArcs stUsers)
                                    ]
                                st2 =
                                    foldl'
                                        (\s w ->
                                            let (bs', d') = unbindWire (ecp5Chipdb (rsArch s)) w (ecp5Bind (rsArch s)) (rsDesign s)
                                             in s{rsArch = setEcp5Bind bs' (rsArch s), rsDesign = d'})
                                        stUsers
                                        weakWires
                             in st2
    setupUser ni srcW (st, dstMap) (slot, u) =
        case netinfoSinkWireR (rsArch st) (rsDesign st) u of
            Nothing -> error ("No wire found for port on destination cell of net " ++ show (netName ni))
            Just dstW ->
                let arc = ArcKey (netName ni) slot 0
                    dstMap' = M.insert dstW arc dstMap
                 in case M.lookup dstW dstMap of
                        -- dst_to_arc duplicate: same net -> skip the arc
                        Just existing | akNet existing == netName ni -> (st, dstMap)
                        Just existing -> error ("setup: two arcs share sink wire " ++ show dstW ++ " (" ++ show (akNet existing) ++ " vs " ++ show (netName ni) ++ ")")
                        Nothing ->
                            if M.notMember dstW (netWires ni)
                                then (arcQueueInsertWithS arc srcW dstW st, dstMap')
                                else
                                    let st1 =
                                            st
                                                { rsWireToArcs = M.insertWith S.union dstW (S.singleton arc) (rsWireToArcs st)
                                                , rsArcToWires = M.insertWith poolInsert arc [dstW] (rsArcToWires st)
                                                }
                                     in (walkBack ni srcW arc dstW dstW st1, dstMap')
    walkBack ni srcW arc dstW cursor st
        | srcW == cursor = st
        | otherwise =
            case M.lookup cursor (netWires ni) of
                Nothing -> arcQueueInsertWithS arc srcW dstW st
                Just (PipMap Nothing _) -> error "setup: wire without uphill pip in net"
                Just (PipMap (Just pip) _) ->
                    let src' = getPipSrcWire (rsArch st) pip
                        st1 =
                            st
                                { rsWireToArcs = M.insertWith S.union src' (S.singleton arc) (rsWireToArcs st)
                                , rsArcToWires = M.insertWith poolInsert arc [src'] (rsArcToWires st)
                                }
                     in walkBack ni srcW arc dstW src' st1

-- | Full wire name ("X<x>/Y<y>/<basename>"), mirroring @nameOfWire@.
wireNameR :: Ecp5 -> WireId -> String
wireNameR e w =
    "X"
        ++ show (locX (wireLoc w))
        ++ "/Y"
        ++ show (locY (wireLoc w))
        ++ "/"
        ++ T.unpack (getWireBasename e w)

-- | Full pip name, mirroring @nameOfPip@.
pipNameR :: Ecp5 -> PipId -> String
pipNameR e p =
    let pi' = pipAt (ecp5Chipdb e) p
        srcN = wiName (wireAt (ecp5Chipdb e) (getPipSrcWire e p))
        dstN = wiName (wireAt (ecp5Chipdb e) (getPipDstWire e p))
        base =
            show (piSrcRelDx pi')
                ++ "_" ++ show (piSrcRelDy pi') ++ "_"
                ++ T.unpack srcN
                ++ "->"
                ++ show (piDstRelDx pi') ++ "_" ++ show (piDstRelDy pi') ++ "_"
                ++ T.unpack dstN
     in "X" ++ show (locX (pipLoc p)) ++ "/Y" ++ show (locY (pipLoc p)) ++ "/" ++ base

-- | One-shot guard for the per-arc A* trace (first matching arc wins).
lpTraceDoneRef :: IORef Bool
lpTraceDoneRef = unsafePerformIO (newIORef False)
{-# NOINLINE lpTraceDoneRef #-}

-- | @route_arc@ (ripup = True always; @route_const_arc@ is dead on ECP5).
routeArcS :: ArcKey -> RState -> RState
routeArcS arc st0
    | srcW == dstW = finishArc stB
    | otherwise = aStarPhase st2
  where
    ni = netOfR (rsDesign st0) (akNet arc)
    srcW = fromMaybe (error "route_arc: no source wire") (netinfoSourceWireR (rsArch st0) (rsDesign st0) ni)
    u = fromMaybe (error "route_arc: inactive user") (netUsers ni V.! akSlot arc)
    dstW = fromMaybe (error "route_arc: no sink wire") (netinfoSinkWireR (rsArch st0) (rsDesign st0) u)
    crit = criticalityOf (rsTmg st0) (CellPortKey (fromMaybe emptyId (prCell u)) (prPort u))
    oldWires = M.findWithDefault [] arc (rsArcToWires st0)
    st1 = st0{rsRipupFlag = False, rsArcToWires = M.insert arc [] (rsArcToWires st0)}
    st2 = foldl' (\s w -> unbindOld arc w s) st1 (reverse oldWires)
    -- LPDBG: per-arc A* trace (LPCHK_TRACE_NET/SLOT/FILE)
    traceH = unsafePerformIO $ do
        tn <- lookupEnv "LPCHK_TRACE_NET"
        ts <- lookupEnv "LPCHK_TRACE_SLOT"
        case (tn, ts) of
            (Just a, Just b)
                | read a == unIdString (akNet arc) && read b == akSlot arc -> do
                    done <- atomicModifyIORef' lpTraceDoneRef (\d -> (True, d))
                    if done
                        then pure Nothing
                        else do
                            mf <- lookupEnv "LPCHK_TRACE_FILE"
                            h <- openFile (fromMaybe "trace_hs.txt" mf) WriteMode
                            pure (Just h)
            _ -> pure Nothing
    {-# NOINLINE traceH #-}
    rngTraceH = unsafePerformIO $ do
        mf <- lookupEnv "LPCHK_TRACE_RNG"
        case mf of
            Nothing -> pure Nothing
            Just f -> do
                h <- openFile f AppendMode
                pure (Just h)
    {-# NOINLINE rngTraceH #-}
    -- special case src == dst
    stA =
        case boundWireNet srcW (ecp5Bind (rsArch st2)) of
            Just n | n /= akNet arc -> error "route_arc: wire bound to another net"
            Just _ -> st2
            Nothing ->
                let (bs', d') = bindWire (akNet arc) srcW StrengthWeak (ecp5Bind (rsArch st2)) (rsDesign st2)
                 in st2{rsArch = setEcp5Bind bs' (rsArch st2), rsDesign = d'}
    stB =
        stA
            { rsWireToArcs = M.insertWith S.union srcW (S.singleton arc) (rsWireToArcs stA)
            , rsArcToWires = M.insertWith poolInsert arc [srcW] (rsArcToWires stA)
            }
    unbindOld arc wire st =
        let arcWires = M.findWithDefault S.empty wire (rsWireToArcs st)
            st1' = st{rsWireToArcs = M.insert wire (S.delete arc arcWires) (rsWireToArcs st)}
         in if S.null (S.delete arc arcWires)
                then
                    let (bs', d') = unbindWire (ecp5Chipdb (rsArch st1')) wire (ecp5Bind (rsArch st1')) (rsDesign st1')
                     in st1'{rsArch = setEcp5Bind bs' (rsArch st1'), rsDesign = d'}
                else st1'
    finishArc st =
        let stF
                | rsRipupFlag st = st{rsArcsWithRipup = rsArcsWithRipup st + 1}
                | otherwise = st{rsArcsWithoutRipup = rsArcsWithoutRipup st + 1}
            _trace =
                case rngTraceH of
                    Just h -> unsafePerformIO (hPutStrLn h ("R " ++ show (unIdString (akNet arc)) ++ " " ++ show (akSlot arc) ++ " " ++ printf "%x" (rngState (rsRng stF))))
                    Nothing -> ()
         in _trace `seq` stF
    -- A* phase: reset the wire queue and visited dict, seed the source,
    -- search, then bind the found path.
    aStarPhase st =
        let (tag, r1) = rng30 (rsRng st)
            togo = estimateDelayR (rsOverrides st) (rsGsrclk st) (rsArch st) srcW dstW
            qw0 = QueuedWire srcW Nothing 0 0 0 togo tag
            bestEst = 0 + togo
            st1' = st{rsWireQueue = heapPush queuedWireGreater Seq.empty qw0, rsVisited = M.singleton srcW qw0, rsRng = r1}
            st2' = search 0 (maxBound :: Int) bestEst (-1 :: DelayT) st1'
            searchEndLine =
                case traceH of
                    Just h -> unsafePerformIO (hPutStrLn h ("SEARCHEND " ++ printf "%x" (rngState (rsRng st2'))))
                    Nothing -> ()
         in searchEndLine `seq` if M.notMember dstW (rsVisited st2')
                then error ("Failed to find a route for arc " ++ show (akSlot arc) ++ " of net " ++ show (akNet arc))
                else bindPath st2'
    -- C++: @while (visitCnt++ < maxVisitCnt && !queue.empty())@ — the
    -- pre-increment value is compared; the body sees the incremented one.
    search visitCnt maxVisit bestEst bestScore st
        | visitCnt >= maxVisit || Seq.null (rsWireQueue st) = st
        | otherwise =
            let (qw, q') = heapPop queuedWireGreater (rsWireQueue st)
                st' = st{rsWireQueue = q'}
                traceLine =
                    case traceH of
                        Just h ->
                            unsafePerformIO
                                ( do
                                    hPutStrLn
                                        h
                                        ( "POPW "
                                            ++ show (visitCnt + 1)
                                            ++ " "
                                            ++ wireNameR (rsArch st) (qwWire qw)
                                            ++ " "
                                            ++ show (qwDelay qw)
                                            ++ " "
                                            ++ show (qwPenalty qw)
                                            ++ " "
                                            ++ show (qwBonus qw)
                                            ++ " "
                                            ++ show (qwTogo qw)
                                            ++ " "
                                            ++ show (qwRandtag qw)
                                            ++ " "
                                            ++ maybe "NONE" (pipNameR (rsArch st)) (qwPip qw)
                                        )
                                    hPutStrLn h ("DHP " ++ unwords (map (pipNameR (rsArch st)) (getPipsDownhill (rsArch st) (qwWire qw))))
                                )
                        Nothing -> ()
                (st'', mvc', be', bs') = expand qw (visitCnt + 1) maxVisit bestEst bestScore st'
             in traceLine `seq` search (visitCnt + 1) mvc' be' bs' st''
    expand qw visitCnt maxVisit bestEst bestScore st =
        foldl'
            (\(s, mvc, be, bs) pip -> stepPip qw visitCnt mvc be bs s pip)
            (st, maxVisit, bestEst, bestScore)
            (getPipsDownhill (rsArch st) (qwWire qw))
    stepPip qw visitCnt mvc be bs st pip =
        let arch = rsArch st
            cfg = rsCfg st
            nextWire = getPipDstWire arch pip
            nextDelay0 = qwDelay qw + dqMaxDelay (getPipDelay arch pip)
            nextDelay = nextDelay0 + dqMaxDelay (getWireDelay arch nextWire)
            netWiresNow = netWires (netOfR (rsDesign st) (akNet arc))
            reused =
                case M.lookup nextWire netWiresNow of
                    Just (PipMap (Just p') _) -> p' == pip
                    _ -> False
         in if reused
                then
                    let bonusDelta = truncate (fromIntegral (rcReuseBonus cfg) * (1 - realToFrac crit :: Double))
                     in candidateStep qw visitCnt mvc be bs st pip nextWire (qwPenalty qw) (qwBonus qw + bonusDelta) (qwPenalty qw) nextDelay
                else
                    let wireAvail = checkWireAvail arch nextWire
                        pipAvail = checkPipAvail arch pip
                        -- wire conflict: the bound net must not hold the wire locked
                        wireBlocked =
                            case boundWireNet nextWire (ecp5Bind arch) of
                                Nothing -> False
                                Just n ->
                                    case M.lookup nextWire (netWires (netOfR (rsDesign st) n)) of
                                        Just (PipMap _ strength) -> strength > StrengthStrong
                                        Nothing -> False
                        mbPipNet = if pipAvail then Nothing else boundPipNet pip (ecp5Bind arch)
                        pipBlocked =
                            case mbPipNet of
                                Nothing -> False
                                Just n ->
                                    case M.lookup nextWire (netWires (netOfR (rsDesign st) n)) of
                                        Just (PipMap _ strength) -> strength > StrengthStrong
                                        Nothing -> False
                     in if (not wireAvail && wireBlocked) || (not pipAvail && (pipBlocked || mbPipNet == Nothing))
                            then (st, mvc, be, bs)
                            else
                                let -- dedup: if the pip-conflicting net owns next_wire, drop the wire penalty
                                    conflictWire =
                                        if not wireAvail && maybe False (\n -> M.member nextWire (netWires (netOfR (rsDesign st) n))) mbPipNet
                                            then Nothing
                                            else (if wireAvail then Nothing else Just nextWire)
                                    wirePen =
                                        case conflictWire of
                                            Just w ->
                                                let sc = M.findWithDefault 0 w (rsWireScores st)
                                                 in fromIntegral (sc + 1) * rcWireRipupPenalty cfg
                                            Nothing -> 0
                                    netPen =
                                        case mbPipNet of
                                            Just n ->
                                                let sc = M.findWithDefault 0 n (rsNetScores st)
                                                    sz = M.size (netWires (netOfR (rsDesign st) n))
                                                 in fromIntegral (sc + 1) * rcNetRipupPenalty cfg + fromIntegral sz * rcWireRipupPenalty cfg
                                            Nothing -> 0
                                    pd = wirePen + netPen
                                    factor = max 0.05 (1 - realToFrac crit :: Double) -- timing_driven = true
                                    nextPenalty = qwPenalty qw + truncate (fromIntegral pd * factor)
                                 in candidateStep qw visitCnt mvc be bs st pip nextWire nextPenalty (qwBonus qw) nextPenalty nextDelay
    candidateStep qw visitCnt mvc be bs st pip nextWire nextPenalty nextBonus penaltyNow nextDelay =
        let nextScore = nextDelay + nextPenalty
            prunedBest = bs >= 0 && (nextScore - nextBonus - rcEstimatePrecision (rsCfg st)) > bs
            prunedVisited =
                case M.lookup nextWire (rsVisited st) of
                    Nothing -> False
                    Just old ->
                        let oldScore = qwDelay old + qwPenalty old
                         in nextScore + 20 >= oldScore -- getDelayEpsilon = 20
         in if prunedBest || prunedVisited
                then (st, mvc, be, bs)
                else
                    let (mbTogo, beUpd, prunedEst)
                            | rcUseEstimate (rsCfg st) =
                                let togo = estimateDelayR (rsOverrides st) (rsGsrclk st) (rsArch st) nextWire dstW
                                    thisEst = nextDelay + togo
                                 in ( Just togo
                                    , if be > thisEst then thisEst else be
                                    , thisEst `quot` 2 - rcEstimatePrecision (rsCfg st) > be
                                    )
                            | otherwise = (Nothing, be, False)
                     in if prunedEst
                            then (st, mvc, be, bs) -- C++ continues without updating best_est
                            else
                                let (tag, r') = rng30 (rsRng st)
                                    qw' = QueuedWire nextWire (Just pip) nextDelay nextPenalty nextBonus (fromMaybe 0 mbTogo) tag
                                    st1' =
                                        st
                                            { rsVisited = M.insert nextWire qw' (rsVisited st)
                                            , rsWireQueue = heapPush queuedWireGreater (rsWireQueue st) qw'
                                            , rsRng = r'
                                            }
                                    (mvc', bs') =
                                        if nextWire == dstW
                                            then (min mvc (2 * visitCnt + (if penaltyNow > 0 then 100 else 0)), nextScore - nextBonus)
                                            else (mvc, bs)
                                    traceLine =
                                        case traceH of
                                            Just h ->
                                                unsafePerformIO
                                                    ( hPutStrLn
                                                        h
                                                        ( "PUSHW "
                                                            ++ wireNameR (rsArch st) nextWire
                                                            ++ " "
                                                            ++ show nextDelay
                                                            ++ " "
                                                            ++ show nextPenalty
                                                            ++ " "
                                                            ++ show nextBonus
                                                            ++ " "
                                                            ++ show (fromMaybe 0 mbTogo)
                                                            ++ " "
                                                            ++ show tag
                                                            ++ " "
                                                            ++ pipNameR (rsArch st) pip
                                                        )
                                                    )
                                            Nothing -> ()
                                 in traceLine `seq` (st1', mvc', beUpd, bs')
    bindPath st =
        let (st', _) = bindLoop dstW st
            bindEndLine =
                case traceH of
                    Just h -> unsafePerformIO (hPutStrLn h ("BINDEND " ++ printf "%x" (rngState (rsRng st'))))
                    Nothing -> ()
         in bindEndLine `seq` finishArc st'
      where
        bindLoop cursor st =
            let qw = rsVisited st M.! cursor
                mbPip = qwPip qw
                _ = if mbPip == Nothing && cursor /= srcW then error "route_arc: path broken" else ()
                needs =
                    case M.lookup cursor (netWires (netOfR (rsDesign st) (akNet arc))) of
                        Nothing -> True
                        Just (PipMap p' _) -> p' /= mbPip
                st1 =
                    if needs
                        then
                            let stA = if checkWireAvail (rsArch st) cursor then st else ripupWireS cursor st
                                stB =
                                    case mbPip of
                                        Just p -> if checkPipAvail (rsArch stA) p then stA else ripupPipS p stA
                                        Nothing -> stA
                             in case mbPip of
                                    Just p ->
                                        let (bs', d') = bindPip (ecp5Chipdb (rsArch stB)) (akNet arc) p StrengthWeak (ecp5Bind (rsArch stB)) (rsDesign stB)
                                         in stB{rsArch = setEcp5Bind bs' (rsArch stB), rsDesign = d'}
                                    Nothing ->
                                        let (bs', d') = bindWire (akNet arc) cursor StrengthWeak (ecp5Bind (rsArch stB)) (rsDesign stB)
                                         in stB{rsArch = setEcp5Bind bs' (rsArch stB), rsDesign = d'}
                        else st
                st2' =
                    st1
                        { rsWireToArcs = M.insertWith S.union cursor (S.singleton arc) (rsWireToArcs st1)
                        , rsArcToWires = M.insertWith poolInsert arc [cursor] (rsArcToWires st1)
                        }
             in case mbPip of
                    Nothing -> (st2', ())
                    Just p -> bindLoop (getPipSrcWire (rsArch st2') p) st2'

router1Main :: RState -> RState
router1Main st0 = go 0 st0
  where
    prog = unsafePerformIO $ do
        mf <- lookupEnv "LP_ROUTE_PROGRESS"
        pure (mf == Just "1")
    {-# NOINLINE prog #-}
    popTrace = unsafePerformIO $ do
        mf <- lookupEnv "LPCHK_ROUTE_ARCS"
        case mf of
            Nothing -> pure Nothing
            Just f -> do
                h <- openFile f WriteMode
                pure (Just h)
    {-# NOINLINE popTrace #-}
    go iter st
        | Seq.null (rsArcQueue st) = st
        | otherwise =
            let (ArcEntry arc pri tag, q') = heapPop arcEntryLess (rsArcQueue st)
                progLine =
                    if prog && iter `mod` 1000 == 0
                        then unsafePerformIO (hPutStrLn stderr ("  arc " ++ show iter ++ " net " ++ show (akNet arc)))
                        else ()
                traceLine =
                    case popTrace of
                        Just h -> unsafePerformIO (hPutStrLn h ("POP " ++ show (unIdString (akNet arc)) ++ " " ++ show (akSlot arc) ++ " " ++ show (akPhys arc) ++ " " ++ show pri ++ " " ++ show tag))
                        Nothing -> ()
                st1 = st{rsArcQueue = q', rsQueuedArcs = S.delete arc (rsQueuedArcs st)}
                st2 = routeArcS arc st1
             in traceLine `seq` progLine `seq` go (iter + 1) st2

-- | @router1(ctx, Router1Cfg(ctx))@.
routeRouter1 ::
    Ecp5 ->
    (T.Text -> Maybe IdString) ->
    M.Map WireId (Int, Int) ->
    Maybe WireId ->
    Design BelId WireId PipId ->
    Rng ->
    IO (Ecp5, Design BelId WireId PipId, Rng)
routeRouter1 e cidOf overrides gsrclk d0 rng0 = do
    let db = ecp5TimingDb e
        ai = assignArchInfo cidOf d0
        cidT x = fromMaybe emptyId (cidOf x)
        isGlobalNet ni = isJust (M.lookup (cidT "ECP5_IS_GLOBAL") (netAttrs ni))
        tmg0 = buildTimingAnalyser e (getPortTimingClassAi db ai) (getPortClockingInfoAi db ai) (getCellDelayAi db ai) isGlobalNet False d0
        tmg = runTimingAnalyser e isGlobalNet True tmg0 d0
        penalty = getRipupDelayPenalty e
        cfg = RouterCfg penalty (10 * penalty) (penalty `quot` 2) (100 * penalty) True
        st0 =
            RState
                { rsArch = e
                , rsDesign = d0
                , rsRng = rng0
                , rsCfg = cfg
                , rsArcQueue = Seq.empty
                , rsWireToArcs = M.empty
                , rsArcToWires = M.empty
                , rsQueuedArcs = S.empty
                , rsWireScores = M.empty
                , rsNetScores = M.empty
                , rsArcsWithRipup = 0
                , rsArcsWithoutRipup = 0
                , rsRipupFlag = False
                , rsTmg = tmg
                , rsOverrides = overrides
                , rsGsrclk = gsrclk
                , rsWireQueue = Seq.empty
                , rsVisited = M.empty
                }
        st1 = setupS st0
        lutpermDump = unsafePerformIO $ do
            mf <- lookupEnv "LPCHK_LUTPERM"
            case mf of
                Nothing -> pure ()
                Just f -> do
                    let cd = ecp5Chipdb e
                        n = cdWidth cd * cdHeight cd * 4
                        rows = [show i ++ " " ++ show (M.findWithDefault 0 i (bsLutperm (ecp5Bind e))) | i <- [0 .. n - 1]]
                    writeFile f (unlines rows)
        tmgDump = unsafePerformIO $ do
            mf <- lookupEnv "LP_TMG_DUMP"
            case mf of
                Nothing -> pure ()
                Just f -> do
                    let crits =
                            [ ( unIdString (akNet arc)
                              , akSlot arc
                              , show (criticalityOf tmg (CellPortKey (fromMaybe emptyId (prCell u)) (prPort u)))
                              , show (M.keys (ppDomainPairs (fromMaybe (error "port missing") (M.lookup (CellPortKey (fromMaybe emptyId (prCell u)) (prPort u)) (taPorts tmg)))))
                              , show (ppRouteDelay (fromMaybe (error "port missing") (M.lookup (CellPortKey (fromMaybe emptyId (prCell u)) (prPort u)) (taPorts tmg))))
                              )
                            | (ArcEntry arc _pri _tag) <- take 20 (foldr (:) [] (rsArcQueue st1))
                            , let u = fromMaybe (error "tmg dump: inactive user") (netUsers (netOfR d0 (akNet arc)) V.! akSlot arc)
                            ]
                    writeFile f (unlines [show x | x <- crits])
        arcCountLine = unsafePerformIO (hPutStrLn stderr ("Routing " ++ show (Seq.length (rsArcQueue st1)) ++ " arcs."))
        st2 = lutpermDump `seq` tmgDump `seq` arcCountLine `seq` router1Main st1
    -- LPDBG: dump the post-router1 net wire state when asked (same format
    -- as the oracle's LPCHK_ROUTE_FINAL).
    mf <- lookupEnv "LPCHK_ROUTE_FINAL"
    case mf of
        Nothing -> pure ()
        Just f -> do
            _ <- evaluate (Seq.length (rsArcQueue st2))
            writeFile f (renderWireDump (rsArch st2) (rsDesign st2))
    pure (rsArch st2, rsDesign st2, rsRng st2)

-- | Render the sorted per-net wire state in the LPCHK dump format.
renderWireDump :: Ecp5 -> Design BelId WireId PipId -> String
renderWireDump e d =
    unlines
        (concatMap netRows (M.elems (designNets d)))
  where
    netRows ni =
        case M.toList (netWires ni) of
            [] -> []
            rows ->
                ("NET " ++ show (unIdString (netName ni)))
                    : [ "W "
                            ++ show (locX (wireLoc w))
                            ++ " "
                            ++ show (locY (wireLoc w))
                            ++ " "
                            ++ show (wireIdx w)
                            ++ " "
                            ++ case pip of
                                Nothing -> "-1 -1 -1"
                                Just p ->
                                    show (locX (pipLoc p))
                                        ++ " "
                                        ++ show (locY (pipLoc p))
                                        ++ " "
                                        ++ show (pipIdx p)
                            ++ " "
                            ++ show (fromEnum strength)
                            ++ " "
                            ++ T.unpack (getWireBasename e w)
                      | (w, PipMap pip strength) <- rows
                      ]
