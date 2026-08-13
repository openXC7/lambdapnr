{-# LANGUAGE OverloadedStrings #-}

-- | CLI parsing tests: the option table, argument parsing and the
-- device validation mirror nextpnr's CommandHandler.
module Lambdapnr.CliTest (cliTests) where

import Data.List (isInfixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Lambdapnr.CLI

cliTests :: TestTree
cliTests =
  testGroup
    "CLI"
    [ testCase "help and version win over other options" $ do
        assertEqual "-h" Help (mustParse ["--json", "d.json", "-h"])
        assertEqual "--help" Help (mustParse ["--help"])
        assertEqual "-V" Version (mustParse ["-V"])
        assertEqual "--version" Version (mustParse ["--version"])
    , testCase "device flags parse" $ do
        forDevice ["12k", "25k", "45k", "85k", "um-25k", "um-45k", "um-85k", "um5g-25k", "um5g-45k", "um5g-85k"] $ \dev ->
          assertEqual ("--" ++ dev) (Run [(dev, Nothing)]) (mustParse ["--" ++ dev])
    , testCase "value options consume the next argument" $ do
        assertEqual "--json" (Run [("json", Just "d.json")]) (mustParse ["--json", "d.json"])
        assertEqual "--json=" (Run [("json", Just "d.json")]) (mustParse ["--json=d.json"])
        assertEqual "order preserved" (Run [("json", Just "a.json"), ("speed", Just "6"), ("25k", Nothing)]) (mustParse ["--json", "a.json", "--speed", "6", "--25k"])
    , testCase "missing and unexpected arguments are errors" $ do
        assertLeft "--json without value" ["--json"]
        assertLeft "unknown long" ["--bogus"]
        assertLeft "unknown short" ["-z"]
        assertLeft "value on a flag" ["--25k=x"]
    , testCase "at most one device type" $ do
        assertEqual "single ok" (Right ()) (checkSingleDevice [("25k", Nothing)])
        assertEqual "two devices rejected" (Left "Only one device type can be set") (checkSingleDevice [("12k", Nothing), ("85k", Nothing)])
        assertEqual "device + other options ok" (Right ()) (checkSingleDevice [("25k", Nothing), ("json", Just "d.json")])
    , testCase "help renders all option groups and devices" $ do
        let help = renderHelp "lambdapnr" generalOptions ecp5Options
        assertBool "general group header" ("General options:" `elem` lines help)
        assertBool "arch group header" ("Architecture specific options:" `elem` lines help)
        assertBool "help option shown" ("-h [ --help ]" `isInfixOf` help)
        forDevice ["12k", "25k", "45k", "85k", "um5g-85k"] $ \dev ->
          assertBool ("--" ++ dev ++ " advertised") (("--" ++ dev) `isInfixOf` help)
    ]
  where
    forDevice = flip mapM_
    lines = Prelude.lines
    mustParse args =
      case parseArgs (generalOptions ++ ecp5Options) args of
        Left err -> error ("parse failed: " ++ err)
        Right c -> c
    assertLeft label args =
      case parseArgs (generalOptions ++ ecp5Options) args of
        Left _ -> pure ()
        Right _ -> assertFailure (label ++ ": expected parse error")
