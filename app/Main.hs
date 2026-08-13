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
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr)

import Lambdapnr.Arch.Ecp5
import Lambdapnr.Arch.Ecp5.Types (eaDevice)
import Lambdapnr.CLI (Command (..), applyGeneralOpts, checkSingleDevice, ecp5ArgsFromOpts, ecp5Options, generalOptions, parseArgs, renderHelp, versionLine)
import Lambdapnr.Kernel.Arch (getChipName)
import Lambdapnr.Kernel.ArchCheck (archcheck)
import Lambdapnr.Kernel.Context (newContextWith)

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
                        Right _ctx' ->
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
                                    if any ((== "json") . fst) opts
                                        then die prog "JSON design loading not yet implemented"
                                        else die prog "no JSON design file specified"

die :: String -> String -> IO a
die prog err = do
    hPutStrLn stderr (prog ++ ": " ++ err)
    exitWith (ExitFailure 125)
