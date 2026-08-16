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
import Lambdapnr.Arch.Ecp5.Bitgen (buildConfig)
import Lambdapnr.Arch.Ecp5.Config (renderChipConfig)
import Lambdapnr.Arch.Ecp5.Pack (Packer, designOf, packDesign)
import Lambdapnr.Arch.Ecp5.PlacerHeap (CellLoc (..), PlacerState (..), emptyPlacerState, placeHeapInitialIters, placeHeapMain, placeHeapSeed)
import Lambdapnr.Arch.Ecp5.CellTiming (TimingDb (..))
import Lambdapnr.Arch.Ecp5.Types (BelId (..), PipId (..), WireId (..), eaDevice)
import Lambdapnr.CLI (Command (..), applyGeneralOpts, checkSingleDevice, ecp5ArgsFromOpts, ecp5Options, generalOptions, parseArgs, renderHelp, versionLine)
import Data.List (intercalate)
import Lambdapnr.Kernel.Arch (getBelName, getChipName)
import Lambdapnr.Kernel.Checksum (checksum, checksumCell, checksumNet)
import Lambdapnr.Kernel.ArchCheck (archcheck)
import Lambdapnr.Kernel.Context (Context (..), newContextWith)
import Lambdapnr.Kernel.JsonFrontend (loadJsonDesign)
import Lambdapnr.Kernel.Netlist (CellInfo (..), Design, NetInfo (..), PortInfo (..), PortRef (..), cellAttrs, cellName, cellParams, cellPorts, cellType, cellsIter, designCellOrder, designCells, designNetOrder, designNets, netAttrs, netDriver, netName, netUsers, portNet, portType, prCell, prPort)
import Lambdapnr.Kernel.Property (Property (..))
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (forM_)
import Lambdapnr.Kernel.DeterministicRng (rngState)
import Lambdapnr.Kernel.IdString (IdString, IdTable, emptyId, idToText, intern, tableSlice)

-- | TEMPORARY debug: dump the id table slice after settings interning.
lambdapnrDebugDump :: IdTable -> IO ()
lambdapnrDebugDump tbl = do
    xs <- tableSlice tbl 1969 46000
    mapM_ (hPutStrLn stderr . ("TBL " ++)) (zipWith (\i s -> show i ++ ": " ++ s) [1969 ..] xs)

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
  where
    propStrD (PropNum s _) = s
    propStrD (PropStr s) = s

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
                                            _ <- case r of
                                                Left _ -> pure ()
                                                Right _ -> do
                                                    stp <- lookupEnv "LAMBDAPNR_PACK_STOP"
                                                    case stp of
                                                        Just _ -> do
                                                            lambdapnrDebugDump (ctxIdTable ctx')
                                                            pure ()
                                                        Nothing -> pure ()
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
                                                    -- placer stage 1: constraints + seed + initial HPWL
                                                    hPutStrLn stderr (printf "LPDBG rngstate %016x" (rngState (ctxRng ctx')))
                                                    let (_, d2, ps, nConstr, hpwl0) = placeHeapSeed arch (\t -> M.lookup t (tdConstIdByName (ecp5TimingDb arch))) dPacked (emptyPlacerState (ctxRng ctx'))
                                                    _ <- evaluate (rngState (psRng ps))
                                                    hPutStrLn stderr ("Placed " ++ show nConstr ++ " cells based on constraints.")
                                                    hPutStrLn stderr ("Creating initial analytic placement for " ++ show (length (psPlaceCells ps)) ++ " cells, random placement wirelen = " ++ show hpwl0 ++ ".")
                                                    ps1 <- placeHeapInitialIters arch (\t -> M.lookup t (tdConstIdByName (ecp5TimingDb arch))) dPacked ps
                                                    writeFile "/tmp/hs_seed_dump.txt" (unlines ([ "SEED " ++ T.unpack (idToText (ctxIdTable ctx') c) ++ " " ++ show (plcX l) ++ " " ++ show (plcY l) | c <- psPlaceCells ps, Just l <- [M.lookup c (psLocs ps)]] ++ [ "LOCK " ++ T.unpack (idToText (ctxIdTable ctx') n) ++ " " ++ show (plcX l) ++ " " ++ show (plcY l) | (n, l) <- M.toList (psLocs ps), plcLocked l ]))
                                                    (arch2, dPlaced, _ps2) <- placeHeapMain arch (\t -> M.lookup t (tdConstIdByName (ecp5TimingDb arch))) d2 ps1
                                                    let cksum2 = checksum
                                                            (fromIntegral . belIdx)
                                                            (fromIntegral . wireIdx)
                                                            (fromIntegral . pipIdx)
                                                            dPlaced
                                                    hPutStrLn stderr (printf "Post-place checksum: 0x%08x" cksum2)
                                                    case lookup "textcfg" opts of
                                                        Just (Just cfgFile) -> do
                                                            let cc = buildConfig arch2 ai dPlaced settings'
                                                            TIO.writeFile cfgFile (renderChipConfig cc)
                                                            hPutStrLn stderr (prog ++ ": wrote text config to " ++ cfgFile)
                                                            die prog "router not yet implemented"
                                                        _ -> die prog "router not yet implemented"
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
