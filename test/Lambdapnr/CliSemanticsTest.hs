{-# LANGUAGE OverloadedStrings #-}

-- | Option-semantics tests: device/package/speed resolution and the
-- settings application mirror @ECP5CommandHandler::createContext@ and
-- @CommandHandler::setupContext@; plus the archcheck integrity oracle
-- on the 25k chipdb.
module Lambdapnr.CliSemanticsTest (cliSemanticsTests, archcheckTests) where

import Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Lambdapnr.Arch.Ecp5
import Lambdapnr.Arch.Ecp5.Types
import Lambdapnr.CLI (applyGeneralOpts, ecp5ArgsFromOpts)
import Lambdapnr.Kernel.Arch (getGridDimX)
import Lambdapnr.Kernel.ArchCheck (archcheck)
import Lambdapnr.Kernel.Context (Context, ctxRng, getSetting, newContextWith)
import Lambdapnr.Kernel.DeterministicRng (Rng, newRng, rngSeed, rngState)
import Lambdapnr.Kernel.IdString (newIdTable)

cliSemanticsTests :: TestTree
cliSemanticsTests =
    testGroup
        "CLI semantics"
        [ testCase "device defaults and flags" $ do
            -- no device flag: LFE5U-45F, like the C++ default
            assertArgs "no options" (Lfe5u45f, "CABGA381", Speed6) []
            assertArgs "--12k" (Lfe5u12f, "CABGA381", Speed6) [("12k", Nothing)]
            assertArgs "--um5g-85k" (Lfe5um5g85f, "CABGA381", Speed85g) [("um5g-85k", Nothing)]
            assertArgs "--package" (Lfe5u45f, "CABGA100", Speed6) [("package", Just "CABGA100")]
            assertArgs "--speed 7" (Lfe5u45f, "CABGA381", Speed7) [("speed", Just "7")]
        , testCase "5G parts force speed grade 8" $ do
            -- 5G without --speed: SPEED_8 then the 5G rule -> SPEED_8_5G
            assertArgs "--um5g-25k default speed" (Lfe5um5g25f, "CABGA381", Speed85g) [("um5g-25k", Nothing)]
            assertArgs "--um5g-25k --speed 8" (Lfe5um5g25f, "CABGA381", Speed85g) [("um5g-25k", Nothing), ("speed", Just "8")]
            assertLeft "5G with speed 7" [("um5g-25k", Nothing), ("speed", Just "7")]
            assertLeft "unsupported speed 9" [("speed", Just "9")]
        , testCase "default package warning" $ do
            case ecp5ArgsFromOpts [] of
                Left err -> assertFailure err
                Right (_, warns) -> assertBool "warning mentions --package" (any ("--package" `isInfixOf`) warns)
            case ecp5ArgsFromOpts [("package", Just "CABGA381")] of
                Left err -> assertFailure err
                Right (_, warns) -> assertEqual "no warning when package given" [] warns
        , testCase "seed reseeds the context RNG" $ do
            ctx <- testContext
            r <- applyGeneralOpts (Ecp5Args Lfe5u45f "" Speed6) [("seed", Just "42")] ctx
            case r of
                Left err -> assertFailure err
                Right ctx' -> assertEqual "rng reseeded" (rngState (rngSeed 42 newRng)) (rngState (ctxRng ctx'))
        , testCase "placer and router validation" $ do
            ctx <- testContext
            r <- applyGeneralOpts (Ecp5Args Lfe5u45f "" Speed6) [("placer", Just "bogus")] ctx
            case r of
                Left err -> assertBool "unknown placer rejected" ("Placer algorithm 'bogus' is not supported" `isInfixOf` err)
                Right _ -> assertFailure "expected placer error"
            r2 <- applyGeneralOpts (Ecp5Args Lfe5u45f "" Speed6) [("router", Just "router2")] ctx
            case r2 of
                Left err -> assertFailure err
                Right ctx' -> do
                    (placer, _) <- getSetting ctx' "placer" ("?" :: String)
                    (router, _) <- getSetting ctx' "router" ("?" :: String)
                    assertEqual "placer default" "heap" placer
                    assertEqual "router option applied" "router2" router
        , testCase "settings defaults mirror the C++" $ do
            ctx <- testContext
            r <- applyGeneralOpts (Ecp5Args Lfe5u45f "CABGA381" Speed6) [] ctx
            case r of
                Left err -> assertFailure err
                Right ctx' -> do
                    (freq, _) <- getSetting ctx' "target_freq" (0.0 :: Double)
                    (tmdriv, _) <- getSetting ctx' "timing_driven" (1 :: Int)
                    (alpha, _) <- getSetting ctx' "placerHeap/alpha" (0.0 :: Double)
                    (archName, _) <- getSetting ctx' "arch.name" ""
                    (archType, _) <- getSetting ctx' "arch.type" ""
                    (pkg, _) <- getSetting ctx' "arch.package" ""
                    assertEqual "target_freq default" 12e6 freq
                    assertEqual "timing_driven default" 1 tmdriv
                    assertEqual "placerHeap/alpha default" 0.1 alpha
                    assertEqual "arch.name" "ARCHNAME" archName
                    assertEqual "arch.type" "lfe5u_45f" archType
                    assertEqual "arch.package" "CABGA381" pkg
        ]
  where
    assertArgs label (dev, pkg, spd) opts =
        case ecp5ArgsFromOpts opts of
            Left err -> assertFailure (label ++ ": " ++ err)
            Right (args, _) ->
                assertEqual label (dev, pkg, spd) (eaDevice args, eaPackage args, eaSpeed args)
    assertLeft label opts =
        case ecp5ArgsFromOpts opts of
            Left _ -> pure ()
            Right _ -> assertFailure (label ++ ": expected error")
    testContext :: IO (Context ())
    testContext = newContextWith <$> newIdTable <*> pure ()

archcheckTests :: TestTree
archcheckTests =
    testGroup
        "ArchCheck"
        [ testCase "25k chipdb passes the integrity check" $ do
            -- slow (~25s): full name/connectivity/bucket walks
            e <- loadEcp5 (Ecp5Args Lfe5um5g25f "" Speed6) "data/ecp5/chipdb-25k.bin" >>= either (error . show) pure
            assertEqual "no archcheck failures" [] (archcheck e)
            assertBool "grid sane" (getGridDimX e > 0)
        ]
