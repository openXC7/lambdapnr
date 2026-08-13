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

import Control.Monad (forM_)
import Control.Exception (evaluate)
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
import Lambdapnr.Arch.Ecp5.CellTiming (TimingDb (..))
import Lambdapnr.Arch.Ecp5.Types (BelId (..), PipId (..), WireId (..), eaDevice)
import Lambdapnr.CLI (Command (..), applyGeneralOpts, checkSingleDevice, ecp5ArgsFromOpts, ecp5Options, generalOptions, parseArgs, renderHelp, versionLine)
import Lambdapnr.Kernel.Arch (getChipName)
import Lambdapnr.Kernel.Checksum (checksum, checksumCell, checksumNet)
import Lambdapnr.Kernel.ArchCheck (archcheck)
import Lambdapnr.Kernel.Context (Context (..), newContextWith)
import Lambdapnr.Kernel.JsonFrontend (loadJsonDesign)
import Lambdapnr.Kernel.Netlist (CellInfo (..), Design, NetInfo (..), PortInfo (..), PortRef (..), cellAttrs, cellName, cellParams, cellPorts, cellType, cellsIter, designCellOrder, designCells, designNetOrder, designNets, netAttrs, netDriver, netName, netUsers, portNet, portType, prCell, prPort)
import Lambdapnr.Kernel.Property (Property (..))
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Control.Monad (forM_)
import Lambdapnr.Kernel.IdString (IdTable, idToText, tableSlice)

-- | TEMPORARY debug: dump the id table slice after settings interning.
lambdapnrDebugDump :: IdTable -> IO ()
lambdapnrDebugDump tbl = do
    xs <- tableSlice tbl 1969 46000
    mapM_ (hPutStrLn stderr . ("TBL " ++)) (zipWith (\i s -> show i ++ ": " ++ s) [1969 ..] xs)

-- | TEMPORARY debug: dump the design state in the C++ LPDBG format.
stateDump :: IdTable -> Design BelId WireId PipId -> IO ()
stateDump tbl d = do
    let nm = T.unpack . idToText tbl
    forM_ (M.toList (designCells d)) $ \(_, ci) -> do
        hPutStrLn stderr ("LPCHKX cell " ++ nm (cellName ci) ++ " " ++ printf "%08x" (checksumCell (const 0) ci))
        hPutStrLn stderr ("LPDBG cell " ++ nm (cellName ci) ++ " type=" ++ nm (cellType ci))
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
                                                    reportDesign prog jsonFile d
                                                    hPutStrLn stderr "Packing design..."
                                                    pk <- packDesign arch d ("verbose" `elem` map fst opts) (ctxSettings ctx')
        
                                                    _ <- do
                                                        stp <- lookupEnv "LAMBDAPNR_PACK_STOP"
                                                        case stp of
                                                            Just _ -> stateDump (ctxIdTable ctx') (designOf pk)
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
                                                    case lookup "textcfg" opts of
                                                        Just (Just cfgFile) -> do
                                                            let cc = buildConfig arch ai dPacked (ctxSettings ctx')
                                                            TIO.writeFile cfgFile (renderChipConfig cc)
                                                            hPutStrLn stderr (prog ++ ": wrote text config to " ++ cfgFile)
                                                            die prog "placing not yet implemented"
                                                        _ -> die prog "placing not yet implemented"
                                        _ -> die prog "no JSON design file specified"

die :: String -> String -> IO a
die prog err = do
    hPutStrLn stderr (prog ++ ": " ++ err)
    exitWith (ExitFailure 125)

-- | Print the imported design summary (the comparison point against
-- the nextpnr reference run).
reportDesign :: String -> FilePath -> Design bel wire pip -> IO ()
reportDesign prog jsonFile d = do
    hPutStrLn stderr (prog ++ ": imported " ++ show (M.size (designCells d)) ++ " cells, " ++ show (M.size (designNets d)) ++ " nets from " ++ jsonFile)
