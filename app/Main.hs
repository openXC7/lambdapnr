{-# LANGUAGE OverloadedStrings #-}

{- | lambdapnr entry point.

Mirrors nextpnr's @main.cc@ + @CommandHandler::exec@: no arguments or
@-h\/--help@ prints the help to stderr, @-V\/--version@ prints the
version banner, both exiting 0. Anything else parses against the option
table and then fails with a clear error until the flow stages land (the
exit code 125 matches the C++ @log_error@ path).
-}
module Main (main) where

import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr)

import Lambdapnr.CLI (Command (..), checkSingleDevice, ecp5Options, generalOptions, parseArgs, renderHelp, versionLine)

main :: IO ()
main = do
    prog <- getProgName
    args <- getArgs
    case parseArgs (generalOptions ++ ecp5Options) args of
        Left err -> do
            hPutStrLn stderr (prog ++ ": " ++ err)
            hPutStrLn stderr ("Try '" ++ prog ++ " --help' for more information.")
            -- the C++ log_error path also exits 125
            exitWith (ExitFailure 125)
        Right cmd -> case cmd of
            Help -> do
                hPutStrLn stderr (renderHelp prog generalOptions ecp5Options)
                exitSuccess
            Version -> do
                hPutStrLn stderr (versionLine prog)
                exitSuccess
            Run opts -> case checkSingleDevice opts of
                Left err -> do
                    hPutStrLn stderr (prog ++ ": " ++ err)
                    exitWith (ExitFailure 125)
                Right () ->
                    if null opts
                    then do
                        -- no arguments at all: nextpnr also prints the help first
                        hPutStrLn stderr (renderHelp prog generalOptions ecp5Options)
                        hPutStrLn stderr (prog ++ ": no JSON design file specified")
                        exitWith (ExitFailure 125)
                    else do
                        hPutStrLn stderr (prog ++ ": design flow not yet implemented (" ++ show (length opts) ++ " option(s) parsed)")
                        exitWith (ExitFailure 125)
