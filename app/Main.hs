{-# LANGUAGE OverloadedStrings #-}

{- | lambdapnr entry point.

Mirrors nextpnr's @main.cc@ + @CommandHandler::exec@: @-h\/--help@ and
@-V\/--version@ print to stderr and exit 0; otherwise the options are
parsed and their semantics applied — device\/package\/speed resolution,
RNG seeding, the placer\/router\/timing settings and their defaults,
then @--test@ runs the architecture database integrity check. The
design flow (JSON ingest, pack/place/route) arrives with the next
milestone; everything exits 125 like the C++ @log_error@ path.
-}
module Main (main) where

import Control.Monad (foldM, forM_)
import Control.Exception (SomeException, evaluate, try)
import Text.Printf (printf)
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as TIO
import System.Environment (getArgs, getProgName, lookupEnv)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr)

import Lambdapnr.Arch.Ecp5
import Lambdapnr.Arch.Ecp5.ArchCellInfo (assignArchInfo)
import Lambdapnr.Arch.Ecp5.Binding (BindState (..), bindBel, emptyBindState)
import Lambdapnr.Arch.Ecp5.Bitgen (buildConfig)
import Lambdapnr.Arch.Ecp5.Config (renderChipConfig)
import Lambdapnr.Arch.Ecp5.Pack (archInfoToAttributes, designOf, packDesign, pkGsrclkWire)
import Lambdapnr.Arch.Ecp5.PlacerHeap (CellLoc (..), PlacerState (..), emptyPlacerState, placeHeapInitialIters, placeHeapMain, placeHeapSeed)
import Lambdapnr.Arch.Ecp5.Placer1 (place1Refine)
import Lambdapnr.Arch.Ecp5.Router (renderWireDump, routeEcp5Globals, routeRouter1, setupWireLocations)
import Lambdapnr.Arch.Ecp5.CellTiming (TimingDb (..), getCellDelayAi, getPortClockingInfoAi, getPortTimingClassAi)
import Lambdapnr.Arch.Ecp5.Types (BelId (..), Location (..), PipId (..), WireId (..), eaDevice)
import Lambdapnr.CLI (Command (..), applyGeneralOpts, checkSingleDevice, ecp5ArgsFromOpts, ecp5Options, generalOptions, parseArgs, renderHelp, versionLine)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isJust)
import Lambdapnr.Kernel.Arch (getBelName, getChipName)
import Lambdapnr.Kernel.Checksum (checksum, checksumCell, checksumNet)
import Lambdapnr.Kernel.ArchCheck (archcheck)
import Lambdapnr.Kernel.Context (Context (..), newContextWith)
import Lambdapnr.Kernel.JsonFrontend (loadJsonDesign)
import Lambdapnr.Kernel.Netlist (CellInfo (..), Design, NetInfo (..), PipMap (..), PlaceStrength, PortInfo (..), PortRef (..), cellAttrs, cellBel, cellBelStrength, cellName, cellParams, cellPorts, cellType, cellsIter, designCellOrder, designCells, designNetOrder, designNets, netAttrs, netDriver, netName, netUsers, netsIter, portNet, portType, prCell, prPort)
import Lambdapnr.Kernel.DeterministicRng (Rng, rngFromState, rngState)
import Lambdapnr.Kernel.Property (Property (..), propAsInt64, propAsString, propIsString)
import Lambdapnr.Kernel.TimingReport (logTimingResults, printUtilisation, timingAnalysisReport)
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (forM_)
import Data.Int (Int16, Int32)
import Data.Word (Word64)
import Lambdapnr.Kernel.IdString (IdString (..), IdTable, emptyId, idToText, intern)

-- | @setting<float>@: read a numeric setting (@Property::as_double@).
settingDouble :: M.Map IdString Property -> IdString -> Double -> Double
settingDouble settings' key def =
    case M.lookup key settings' of
        Nothing -> def
        Just p -> if propIsString p then read (T.unpack (propAsString p)) else fromIntegral (propAsInt64 p)

-- | The serialized property string (C++ @Property::str@).
propStrD :: Property -> Text
propStrD (PropNum s _) = s
propStrD (PropStr s) = s

-- | TEMPORARY debug: dump the design state in the C++ LPDBG format.
stateDump :: Ecp5 -> IdTable -> Design BelId WireId PipId -> IO ()
stateDump arch tbl d = do
    let nm = T.unpack . idToText tbl
    forM_ (M.toList (designCells d)) $ \(_, ci) -> do
        hPutStrLn stderr ("LPCHKX cell " ++ nm (cellName ci) ++ " " ++ printf "%08x" (checksumCell (const 0) ci))
        hPutStrLn stderr ("LPDBG cell " ++ nm (cellName ci) ++ " type=" ++ nm (cellType ci))
        hPutStrLn stderr ("LPDBG cellbel " ++ nm (cellName ci) ++ " " ++ maybe "-" (intercalate "/" . map (T.unpack . idToText tbl) . getBelName arch) (cellBel ci) ++ " " ++ show (fromEnum (cellBelStrength ci)))
        forM_ (M.toList (cellPorts ci)) $ \(p, pi) -> do
            hPutStrLn stderr ("LPDBG cellport " ++ nm (cellName ci) ++ " " ++ nm p ++ " " ++ maybe "-" nm (portNet pi) ++ " " ++ show (fromEnum (portType pi)))
        forM_ (M.toList (cellAttrs ci)) $ \(k, v) ->
            hPutStrLn stderr ("LPDBG cellattr " ++ nm (cellName ci) ++ " " ++ nm k ++ " " ++ T.unpack (propStrD v))
        forM_ (M.toList (cellParams ci)) $ \(k, v) ->
            hPutStrLn stderr ("LPDBG cellparam " ++ nm (cellName ci) ++ " " ++ nm k ++ " " ++ T.unpack (propStrD v))
    forM_ (M.toList (designNets d)) $ \(_, ni) -> do
        forM_ (M.toList (netAttrs ni)) $ \(k, v) ->
            hPutStrLn stderr ("LPDBG netattr " ++ nm (netName ni) ++ " " ++ nm k ++ " " ++ T.unpack (propStrD v))
        hPutStrLn stderr ("LPDBG net " ++ nm (netName ni) ++ " drv=" ++ maybe "-" nm (prCell (netDriver ni)) ++ "." ++ nm (prPort (netDriver ni)))
        forM_ [u | Just u <- V.toList (netUsers ni)] $ \u ->
            hPutStrLn stderr ("LPDBG netuser " ++ nm (netName ni) ++ " " ++ maybe "-" nm (prCell u) ++ "." ++ nm (prPort u))
        hPutStrLn stderr ("LPCHKX net " ++ nm (netName ni) ++ " " ++ printf "%08x" (checksumNet (const 0) (const 0) ni))

-- | Serialize the post-heap placement + RNG state (the placer1 resume
-- debug path — lets the SA refine be iterated without re-running the
-- 5-minute heap loop).
writePlacer1State :: FilePath -> Rng -> BindState -> Design BelId WireId PipId -> IO ()
writePlacer1State f rng bs d = do
    let ls =
            ("RNG " ++ show (rngState rng)) :
            ["L " ++ show i ++ " " ++ show r | (i, r) <- M.toList (bsLutperm bs)] ++
            [ show (unIdString (cellName ci))
                ++ " "
                ++ show (fromIntegral (locX (belLoc bel)) :: Int)
                ++ " "
                ++ show (fromIntegral (locY (belLoc bel)) :: Int)
                ++ " "
                ++ show (belIdx bel)
                ++ " "
                ++ show (fromEnum (cellBelStrength ci))
            | ci <- cellsIter d
            , Just bel <- [cellBel ci]
            ]
    writeFile f (unlines ls)

-- | Read a saved post-heap state: the RNG word and the cell->bel bindings.
readPlacer1State :: FilePath -> IO (Rng, M.Map Int Int, [(IdString, BelId, PlaceStrength)])
readPlacer1State f = do
    ls <- lines <$> readFile f
    let rngW = case ls of
            (('R' : 'N' : 'G' : ' ' : rest) : _) -> rngFromState (read rest :: Word64)
            _ -> error "bad placer1 save: no RNG line"
        lut =
            M.fromList
                [ (read i, read r)
                | l <- ls
                , let ws = words l
                , case ws of
                    ["L", i, r] -> True
                    _ -> False
                , let ["L", i, r] = ws
                ]
        binds =
            [ ( IdString (read c)
              , BelId (Location (fromIntegral (read x :: Int)) (fromIntegral (read y :: Int))) (fromIntegral (read b :: Int))
              , toEnum (read s)
              )
            | l <- ls
            , let ws = words l
            , case ws of
                [c, x, y, b, s] -> True
                _ -> False
            , let [c, x, y, b, s] = ws
            ]
    pure (rngW, lut, binds)

-- | Active users in slot order (the C++ indexed_store iterate).
activeUsersV :: V.Vector (Maybe PortRef) -> [PortRef]
activeUsersV v = [u | Just u <- V.toList v]

-- | Apply saved bindings to the packed design (rebuilds the bel->cell map
-- and restores the lutperm map).
applyPlacer1Bindings :: Ecp5 -> Design BelId WireId PipId -> (M.Map Int Int, [(IdString, BelId, PlaceStrength)]) -> (Ecp5, Design BelId WireId PipId)
applyPlacer1Bindings arch d (lut, binds) =
    let (bs, d') = foldl' (\(b, dd) (c, bel, s) -> bindBel c bel s b dd) (emptyBindState{bsLutperm = lut}, d) binds
     in (setEcp5Bind bs arch, d')

main :: IO ()
main = do
    prog <- getProgName
    args <- getArgs
    case parseArgs (generalOptions ++ ecp5Options) args of
        Left err -> do
            hPutStrLn stderr (prog ++ ": " ++ err)
            hPutStrLn stderr ("Try '" ++ prog ++ " --help' for more information.")
            exitWith (ExitFailure 125)
        Right cmd -> case cmd of
            Help -> do
                hPutStrLn stderr (renderHelp prog generalOptions ecp5Options)
                exitSuccess
            Version -> do
                hPutStrLn stderr (versionLine prog)
                exitSuccess
            Run opts -> runFlow prog opts

-- | The @--test@-and-context part of @CommandHandler::exec@.
runFlow :: String -> [(String, Maybe String)] -> IO ()
runFlow prog opts = case checkSingleDevice opts of
    Left err -> die prog err
    Right () -> case ecp5ArgsFromOpts opts of
        Left err -> die prog err
        Right (args, warnings) -> do
            forM_ warnings (hPutStrLn stderr)
            r <- loadEcp5 args (chipdbFileFor (eaDevice args))
            case r of
                Left err -> die prog err
                Right arch -> do
                    let ctx = newContextWith (ecp5IdTable arch) arch
                    r2 <- applyGeneralOpts args opts ctx
                    case r2 of
                        Left err -> die prog err
                        Right ctx' ->
                            if "test" `elem` map fst opts
                                then do
                                    hPutStrLn stderr "Running architecture database integrity check."
                                    hPutStrLn stderr ("Checking device " ++ show (getChipName arch) ++ ".")
                                    let failures = archcheck arch
                                    if null failures
                                        then do
                                            hPutStrLn stderr "Architecture check passed."
                                            exitSuccess
                                        else do
                                            forM_ failures (hPutStrLn stderr . ("ERROR: " ++))
                                            exitWith (ExitFailure 125)
                                else
                                    case lookup "json" opts of
                                        Just (Just jsonFile) -> do
                                            jsrc <- TIO.readFile jsonFile
                                            r <- loadJsonDesign (ctxIdTable ctx') Nothing jsrc
                                            case r of
                                                Left err -> die prog err
                                                Right d -> do
                                                    (settings', d') <- applyLpfs prog arch opts (ctxSettings ctx') d
                                                    reportDesign prog jsonFile d'
                                                    hPutStrLn stderr "Packing design..."
                                                    pk <- packDesign arch d' ("verbose" `elem` map fst opts) settings'
        
                                                    _ <- do
                                                        stp <- lookupEnv "LAMBDAPNR_PACK_STOP"
                                                        case stp of
                                                            Just _ -> stateDump arch (ctxIdTable ctx') (designOf pk)
                                                            Nothing -> pure ()
                                                    let dPacked = designOf pk
                                                        ai = assignArchInfo (\t -> M.lookup t (tdConstIdByName (ecp5TimingDb arch))) dPacked
                                                        cksum = checksum
                                                            (fromIntegral . belIdx)
                                                            (fromIntegral . wireIdx)
                                                            (fromIntegral . pipIdx)
                                                            dPacked
                                                    hPutStrLn stderr (printf "Checksum: 0x%08x" cksum)
                                                    _ <- evaluate (designCells dPacked)
                                                    -- Arch::pack() tail: archInfoToAttributes() (after the
                                                    -- checksum print, mirroring pack.cc:3035-3038)
                                                    dPackedAttr <- archInfoToAttributes arch dPacked
                                                    let dPacked' = dPackedAttr
                                                    -- command.cc executeMain: print_utilisation after pack
                                                    printUtilisation arch (ctxIdTable ctx') dPacked'
                                                    -- placer stage 1: constraints + seed + initial HPWL
                                                    hPutStrLn stderr (printf "LPDBG rngstate %016x" (rngState (ctxRng ctx')))
                                                    let cidOfT = \t -> M.lookup t (tdConstIdByName (ecp5TimingDb arch))
                                                    -- LP_ROUTE_RESUME: skip the whole placer on reruns by
                                                    -- loading the post-place state (RNG + bel bindings)
                                                    -- written by LP_PLACE_SAVE.
                                                    routeResumeEarly <- lookupEnv "LP_ROUTE_RESUME"
                                                    (arch3, dRefined, rngRefined) <-
                                                        case routeResumeEarly of
                                                            Just f -> do
                                                                (rngW, lutW, binds) <- readPlacer1State f
                                                                let (archR0, dR0) = applyPlacer1Bindings arch dPacked' (lutW, binds)
                                                                pure (archR0, dR0, rngW)
                                                            Nothing -> do
                                                                resumeFile <- lookupEnv "LP_PLACER1_RESUME"
                                                                (arch2, dPlaced, rngPlaced) <-
                                                                    case resumeFile of
                                                                        Just f -> do
                                                                            (rngW, lutW, binds) <- readPlacer1State f
                                                                            let (archR, dR) = applyPlacer1Bindings arch dPacked' (lutW, binds)
                                                                            pure (archR, dR, rngW)
                                                                        Nothing -> do
                                                                            let (_, d2, ps, nConstr, hpwl0) = placeHeapSeed arch cidOfT dPacked' (emptyPlacerState (ctxRng ctx'))
                                                                            _ <- evaluate (rngState (psRng ps))
                                                                            hPutStrLn stderr ("Placed " ++ show nConstr ++ " cells based on constraints.")
                                                                            hPutStrLn stderr ("Creating initial analytic placement for " ++ show (length (psPlaceCells ps)) ++ " cells, random placement wirelen = " ++ show hpwl0 ++ ".")
                                                                            ps1 <- placeHeapInitialIters arch cidOfT dPacked' ps
                                                                            writeFile "/tmp/hs_seed_dump.txt" (unlines ([ "SEED " ++ T.unpack (idToText (ctxIdTable ctx') c) ++ " " ++ show (plcX l) ++ " " ++ show (plcY l) | c <- psPlaceCells ps, Just l <- [M.lookup c (psLocs ps)]] ++ [ "LOCK " ++ T.unpack (idToText (ctxIdTable ctx') n) ++ " " ++ show (plcX l) ++ " " ++ show (plcY l) | (n, l) <- M.toList (psLocs ps), plcLocked l ]))
                                                                            (arch2h, dPlacedh, ps2h) <- placeHeapMain arch cidOfT d2 ps1
                                                                            pure (arch2h, dPlacedh, psRng ps2h)
                                                                saveFile <- lookupEnv "LP_PLACER1_SAVE"
                                                                case saveFile of
                                                                    Just f -> writePlacer1State f rngPlaced (ecp5Bind arch2) dPlaced
                                                                    Nothing -> pure ()
                                                                let (arch3x, dRefinedx, rngRefinedx) = place1Refine arch2 cidOfT dPlaced rngPlaced
                                                                placeSaveFile <- lookupEnv "LP_PLACE_SAVE"
                                                                case placeSaveFile of
                                                                    Just f -> writePlacer1State f rngRefinedx (ecp5Bind arch3x) dRefinedx
                                                                    Nothing -> pure ()
                                                                placeCkFile <- lookupEnv "LP_PLACE_CK"
                                                                case placeCkFile of
                                                                    Just f -> do
                                                                        let cellLns =
                                                                                [ "C " ++ show (unIdString (cellName ci)) ++ " " ++ printf "%08x" (checksumCell (fromIntegral . belIdx) ci)
                                                                                | ci <- cellsIter dRefinedx
                                                                                ]
                                                                            netLns =
                                                                                [ "N " ++ show (unIdString (netName ni)) ++ " " ++ printf "%08x" (checksumNet (fromIntegral . wireIdx) (fromIntegral . pipIdx) ni)
                                                                                | ni <- netsIter dRefinedx
                                                                                ]
                                                                        writeFile f (unlines (cellLns ++ netLns))
                                                                    Nothing -> pure ()
                                                                pure (arch3x, dRefinedx, rngRefinedx)
                                                    -- SAPlacer::place() tail: timing_analysis() before the
                                                    -- placer1 checksum print (default args: fmax + histogram,
                                                    -- no critical paths).
                                                    let dbP = ecp5TimingDb arch3
                                                        aiP = assignArchInfo cidOfT dRefined
                                                        cidPT = \x -> fromMaybe emptyId (cidOfT x)
                                                        isGlobalP ni = isJust (M.lookup (cidPT "ECP5_IS_GLOBAL") (netAttrs ni))
                                                        tgtP = settingDouble settings' (cidPT "target_freq") 1.2e7
                                                        placeReport = timingAnalysisReport arch3 (ctxIdTable ctx') (getPortTimingClassAi dbP aiP) (getPortClockingInfoAi dbP aiP) (getCellDelayAi dbP aiP) isGlobalP tgtP dRefined
                                                    logTimingResults arch3 (ctxIdTable ctx') (designCells dRefined) (designNets dRefined) False True True False placeReport
                                                    let cksum2 =
                                                            checksum
                                                                (fromIntegral . belIdx)
                                                                (fromIntegral . wireIdx)
                                                                (fromIntegral . pipIdx)
                                                                dRefined
                                                    let ck2 = cksum2 in ck2 `seq` hPutStrLn stderr (printf "Post-place checksum: 0x%08x" ck2)
                                                    -- Arch::place() tail: archInfoToAttributes() after the placer1
                                                    -- checksum print (sets NEXTPNR_BEL/BEL_STRENGTH on placed cells,
                                                    -- ROUTING="" on nets — both are part of the router1-end checksum).
                                                    dRefinedAttr <- archInfoToAttributes arch3 dRefined
                                                    -- Arch::route(): setup_wire_locations -> route_ecp5_globals
                                                    -- -> router1 (mirroring arch.cc:686-708; the tail
                                                    -- archInfoToAttributes is checksum-invisible here and skipped)
                                                    hPutStrLn stderr "Routing.."
                                                    let overrides = setupWireLocations arch3 dRefinedAttr
                                                        (archG, dGlobals) = routeEcp5Globals arch3 dRefinedAttr
                                                    gDump <- lookupEnv "LPCHK_ROUTE_GLOBALS"
                                                    case gDump of
                                                        Just f -> do
                                                            _ <- evaluate (designNets dGlobals)
                                                            writeFile f (renderWireDump archG dGlobals)
                                                        Nothing -> pure ()
                                                    stopAfterGlobals <- lookupEnv "LAMBDAPNR_GLOBALS_STOP"
                                                    case stopAfterGlobals of
                                                        Just _ -> die prog "stopped after globals routing (LAMBDAPNR_GLOBALS_STOP)"
                                                        Nothing -> pure ()
                                                    (arch4, dRouted, rngRouted) <- routeRouter1 archG cidOfT overrides (pkGsrclkWire pk) dGlobals rngRefined
                                                    let cksum3 =
                                                            checksum
                                                                (fromIntegral . belIdx)
                                                                (fromIntegral . wireIdx)
                                                                (fromIntegral . pipIdx)
                                                                dRouted
                                                    let ck3 = cksum3 in ck3 `seq` hPutStrLn stderr (printf "Post-route checksum: 0x%08x" ck3)
                                                    -- router1 tail: timing_analysis(ctx, true, true, true, true, true)
                                                    let dbR = ecp5TimingDb arch4
                                                        aiR = assignArchInfo cidOfT dRouted
                                                        cidRT = \x -> fromMaybe emptyId (cidOfT x)
                                                        isGlobalR ni = isJust (M.lookup (cidRT "ECP5_IS_GLOBAL") (netAttrs ni))
                                                        tgtR = settingDouble settings' (cidRT "target_freq") 1.2e7
                                                        routeReport = timingAnalysisReport arch4 (ctxIdTable ctx') (getPortTimingClassAi dbR aiR) (getPortClockingInfoAi dbR aiR) (getCellDelayAi dbR aiR) isGlobalR tgtR dRouted
                                                    logTimingResults arch4 (ctxIdTable ctx') (designCells dRouted) (designNets dRouted) True True True True routeReport
                                                    routeCkFile <- lookupEnv "LP_ROUTE_CK"
                                                    case routeCkFile of
                                                        Just f -> do
                                                            let cellLns =
                                                                    [ "C " ++ show (unIdString (cellName ci)) ++ " " ++ printf "%08x" (checksumCell (fromIntegral . belIdx) ci)
                                                                    | ci <- cellsIter dRouted
                                                                    ]
                                                                cellAttrLns =
                                                                    [ "CA " ++ show (unIdString (cellName ci)) ++ " " ++ show (unIdString k) ++ " " ++ T.unpack (pStr v)
                                                                    | ci <- cellsIter dRouted
                                                                    , (k, v) <- M.toList (cellAttrs ci)
                                                                    ]
                                                                netLns =
                                                                    [ "N " ++ show (unIdString (netName ni)) ++ " " ++ printf "%08x" (checksumNet (fromIntegral . wireIdx) (fromIntegral . pipIdx) ni)
                                                                    | ni <- netsIter dRouted
                                                                    ]
                                                                attrLns =
                                                                    [ "A " ++ show (unIdString (netName ni)) ++ " " ++ show (unIdString k) ++ " " ++ T.unpack (pStr v)
                                                                    | ni <- netsIter dRouted
                                                                    , (k, v) <- M.toList (netAttrs ni)
                                                                    ]
                                                                drvLns =
                                                                    [ "D " ++ show (unIdString (netName ni)) ++ " " ++ show (maybe (-1) unIdString (prCell (netDriver ni))) ++ " " ++ show (unIdString (prPort (netDriver ni)))
                                                                    | ni <- netsIter dRouted
                                                                    ]
                                                                usrLns =
                                                                    [ "U " ++ show (unIdString (netName ni)) ++ " " ++ show (maybe (-1) unIdString (prCell u)) ++ " " ++ show (unIdString (prPort u))
                                                                    | ni <- netsIter dRouted
                                                                    , u <- activeUsersV (netUsers ni)
                                                                    ]

                                                            writeFile f (unlines (cellLns ++ cellAttrLns ++ netLns ++ attrLns ++ drvLns ++ usrLns))
                                                        Nothing -> pure ()
                                                    case lookup "textcfg" opts of
                                                        Just (Just cfgFile) -> do
                                                            let cc = buildConfig arch4 ai dRouted settings'
                                                            TIO.writeFile cfgFile (renderChipConfig cc)
                                                        _ -> pure ()
                                                    -- CommandHandler::exec() tail: printFooter (empty: 0
                                                    -- warnings/errors) + log_break + the final line
                                                    hPutStrLn stderr ""
                                                    hPutStrLn stderr "Info: Program finished normally."
                                        _ -> die prog "no JSON design file specified"

-- | @ECP5CommandHandler::customAfterLoad@: parse every @--lpf@ file via
-- @apply_lpf@, then verify every nextpnr buffer has a LOC (unless
-- @--lpf-allow-unconstrained@). The per-cell type checks intern
-- @$nextpnr_ibuf@\/@$nextpnr_obuf@\/@$nextpnr_iobuf@ in the C++ evaluation
-- order (short-circuit) — that interning is part of the contract.
applyLpfs :: String -> Ecp5 -> [(String, Maybe String)] -> M.Map IdString Property -> Design BelId WireId PipId -> IO (M.Map IdString Property, Design BelId WireId PipId)
applyLpfs prog arch opts settings d =
    case [f | ("lpf", Just f) <- opts] of
        [] -> pure (settings, d)
        files -> do
            (st, d') <- foldM (applyOne prog arch) (settings, d) files
            _ <- checkIoConstrained prog arch opts d'
            pure (st, d')
  where
    applyOne :: String -> Ecp5 -> (M.Map IdString Property, Design BelId WireId PipId) -> FilePath -> IO (M.Map IdString Property, Design BelId WireId PipId)
    applyOne prog arch (st, d) file = do
        lsrc <- try (TIO.readFile file) :: IO (Either SomeException Text)
        case lsrc of
            Left _ -> die prog ("failed to open LPF file '" ++ file ++ "'")
            Right contents -> do
                r <- applyLpf arch (T.pack file) contents st d
                case r of
                    Left err -> die prog ("failed to parse LPF file '" ++ file ++ "': " ++ err)
                    Right ok -> pure ok

-- | The unconstrained-IO check loop of @customAfterLoad@.
checkIoConstrained :: String -> Ecp5 -> [(String, Maybe String)] -> Design BelId WireId PipId -> IO ()
checkIoConstrained prog arch opts d = do
    let tbl = ecp5IdTable arch
        cidOf t = maybe emptyId id (M.lookup t (tdConstIdByName (ecp5TimingDb arch)))
        allowUncon = "lpf-allow-unconstrained" `elem` map fst opts
        locId = cidOf "LOC"
        act ci =
            case M.lookup locId (cellAttrs ci) of
                Nothing
                    | allowUncon ->
                        hPutStrLn stderr ("Warning: IO '" ++ T.unpack (idToText tbl (cellName ci)) ++ "' is unconstrained in LPF and will be automatically placed")
                    | otherwise ->
                        die prog ("IO '" ++ T.unpack (idToText tbl (cellName ci)) ++ "' is unconstrained in LPF (override this error with --lpf-allow-unconstrained)")
                Just _ -> pure ()
        go [] = pure ()
        go (ci : rest) = do
            ibuf <- intern tbl "$nextpnr_ibuf"
            if cellType ci == ibuf
                then act ci >> go rest
                else do
                    obuf <- intern tbl "$nextpnr_obuf"
                    if cellType ci == obuf
                        then act ci >> go rest
                        else do
                            iobuf <- intern tbl "$nextpnr_iobuf"
                            if cellType ci == iobuf then act ci >> go rest else go rest
    go (cellsIter d)

die :: String -> String -> IO a
die prog err = do
    hPutStrLn stderr (prog ++ ": " ++ err)
    exitWith (ExitFailure 125)

-- | Print the imported design summary (the comparison point against
-- the nextpnr reference run).
reportDesign :: String -> FilePath -> Design bel wire pip -> IO ()
reportDesign prog jsonFile d = do
    hPutStrLn stderr (prog ++ ": imported " ++ show (M.size (designCells d)) ++ " cells, " ++ show (M.size (designNets d)) ++ " nets from " ++ jsonFile)
