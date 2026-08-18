{-# LANGUAGE OverloadedStrings #-}

{- | Command-line interface, mirroring nextpnr's @CommandHandler@
(@common\/kernel\/command.cc@ + per-arch @main.cc@): the same option
inventory, grouped the same way, rendered in the same
@boost::program_options@ layout, and printed to stderr like the C++
(@std::cerr@).

The option table is the source of truth for both the help text and the
argument parser, so what the help advertises is exactly what the parser
accepts. Options that nextpnr only shows when compiled with python
support (@--run@, @--pre-pack@, ...) are omitted here until the python
hook arrives; same for GUI-only options.
-}
module Lambdapnr.CLI (
    Command (..),
    Option (..),
    generalOptions,
    ecp5Options,
    deviceOptions,
    parseArgs,
    checkSingleDevice,
    renderHelp,
    versionLine,
    ecp5ArgsFromOpts,
    applyGeneralOpts,
) where

import qualified Data.ByteString as BS
import Control.Monad (foldM)
import Data.List (find, intercalate)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.IO (IOMode (ReadMode), hClose, openBinaryFile)

import Lambdapnr.Arch.Ecp5.Types (Ecp5Args (..), Ecp5Device (..), SpeedGrade (..))
import Lambdapnr.Kernel.Context (Context (..), setSetting)
import Lambdapnr.Kernel.IdString (intern)
import Lambdapnr.Kernel.DeterministicRng (rngSeed, rngState)
import Lambdapnr.Kernel.Property (propFromInt, propFromString)

-- | An option: short flag, long name, whether it takes a value, and the
-- help text.
data Option = Option
    { optShort :: Maybe Char
    , optLong :: String
    , optHasArg :: Bool
    , optDesc :: String
    }

-- | Parsed command line.
data Command
    = Help -- ^ -h\/--help: show help, exit 0
    | Version -- ^ -V\/--version: show version line, exit 0
    | Run [(String, Maybe String)]
    -- ^ otherwise: the options as (long name, value) pairs, in order
    deriving (Eq, Show)

-- | General options, in nextpnr's @getGeneralOptions@ order.
generalOptions :: [Option]
generalOptions =
    [ flag 'h' "help" "show help"
    , flag 'v' "verbose" "verbose output"
    , flag 'q' "quiet" "quiet mode, only errors and warnings displayed"
    , nflag "Werror" "Turn warnings into errors"
    , arg 'l' "log" "log file, all log messages are written to this file regardless of -q"
    , nflag "debug" "debug output"
    , nflag "debug-placer" "debug output from placer only"
    , nflag "debug-router" "debug output from router only"
    , narg "threads" "number of threads for passes where this is configurable"
    , flag 'f' "force" "keep running after errors"
    , narg "json" "JSON design file to ingest"
    , narg "write" "JSON design file to write"
    , narg "top" "name of top module"
    , narg "seed" "seed value for random number generator"
    , flag 'r' "randomize-seed" "randomize seed value for random number generator"
    , narg "placer" "placer algorithm to use; available: sa, heap, static; default: heap"
    , narg "router" "router algorithm to use; available: router1, router2; default: router1"
    , narg "cstrweight" "placer weighting for relative constraint satisfaction"
    , narg "starttemp" "placer SA start temperature"
    , nflag "pack-only" "pack design only without placement or routing"
    , nflag "no-route" "process design without routing"
    , nflag "no-place" "process design without placement"
    , nflag "no-pack" "process design without packing"
    , nflag "ignore-loops" "ignore combinational loops in timing analysis"
    , nflag "ignore-rel-clk" "ignore clock-to-clock relations in timing checks"
    , flag 'V' "version" "show version"
    , nflag "test" "check architecture database integrity"
    , narg "freq" "set target frequency for design in MHz"
    , narg "slack_redist_iter" "number of iterations between slack redistribution"
    , nflag "timing-allow-fail" "allow timing to fail in design"
    , nflag "no-tmdriv" "disable timing-driven placement"
    , narg "sdc" "Generic timing constraints SDC file to load"
    , narg "sdf" "SDF delay back-annotation file to write"
    , nflag "sdf-cvc" "enable tweaks for SDF file compatibility with the CVC simulator"
    , nflag "no-print-critical-path-source" "disable printing of the line numbers associated with each net in the critical path"
    , narg "placer-heap-alpha" "placer heap alpha value (float, default: 0.1)"
    , narg "placer-heap-beta" "placer heap maximum placement density (float, default: 0.9)"
    , narg "placer-heap-critexp" "placer heap criticality exponent (int, default: 2)"
    , narg "placer-heap-timingweight" "placer heap timing weight (int, default: 10)"
    , narg "placer-heap-cell-placement-timeout" "allow placer to attempt up to max(10000, total cells^2 / N) iterations to place a cell (int N, default: 8, 0 for no timeout)"
    , nflag "placer-heap-no-ctrl-set" "disable control set awareness in placer heap"
    , nflag "static-dump-density" "write density csv files during placer-static flow"
    , nflag "parallel-refine" "use new experimental parallelised engine for placement refinement"
    , narg "router2-heatmap" "prefix for router2 resource congestion heatmaps"
    , nflag "tmg-ripup" "enable experimental timing-driven ripup in router"
    , nflag "router2-tmg-ripup" "enable experimental timing-driven ripup in router (deprecated; use --tmg-ripup instead)"
    , nflag "router2-alt-weights" "use alternate router2 weights"
    , narg "report" "write timing and utilization report in JSON format to file"
    , nflag "detailed-timing-report" "Append detailed net timing data to the JSON report"
    , narg "placed-svg" "write render of placement to SVG file"
    , narg "routed-svg" "write render of routing to SVG file"
    ]
  where
    flag s l d = Option (Just s) l False d
    nflag l d = Option Nothing l False d
    arg s l d = Option (Just s) l True d
    narg l d = Option Nothing l True d

-- | ECP5 architecture options, mirroring @ECP5CommandHandler::getArchOptions@.
-- Only the devices with a chipdb present are listed (nextpnr does the
-- same via @Arch::is_available@; the 25k family shares one chipdb).
ecp5Options :: [Option]
ecp5Options =
    [ nflag "12k" "set device type to LFE5U-12F"
    , nflag "25k" "set device type to LFE5U-25F"
    , nflag "45k" "set device type to LFE5U-45F"
    , nflag "85k" "set device type to LFE5U-85F"
    , nflag "um-25k" "set device type to LFE5UM-25F"
    , nflag "um-45k" "set device type to LFE5UM-45F"
    , nflag "um-85k" "set device type to LFE5UM-85F"
    , nflag "um5g-25k" "set device type to LFE5UM5G-25F"
    , nflag "um5g-45k" "set device type to LFE5UM5G-45F"
    , nflag "um5g-85k" "set device type to LFE5UM5G-85F"
    , narg "package" "select device package (defaults to CABGA381)"
    , narg "speed" "select device speedgrade (6, 7 or 8)"
    , narg "basecfg" "base chip configuration in Trellis text format (deprecated)"
    , narg "override-basecfg" "base chip configuration in Trellis text format"
    , narg "textcfg" "textual configuration in Trellis format to write"
    , narg "lpf" "LPF pin constraint file(s)"
    , nflag "lpf-allow-unconstrained" "don't require LPF file(s) to constrain all IO"
    , nflag "no-promote-globals" "disable all global promotion"
    , nflag "out-of-context" "disable IO buffer insertion and global promotion/routing, for building pre-routed blocks (experimental)"
    , nflag "disable-router-lutperm" "don't allow the router to permute LUT inputs"
    , nflag "allow-fabric-eclk" "allow fabric routing of ECLKs"
    ]
  where
    flag s l d = Option (Just s) l False d
    nflag l d = Option Nothing l False d
    arg s l d = Option (Just s) l True d
    narg l d = Option Nothing l True d

-- | The long names of the mutually exclusive device-selector flags
-- (@ECP5CommandHandler::validate@ errors when more than one is set).
deviceOptions :: [String]
deviceOptions = map optLong (filter ((`elem` ["12k", "25k", "45k", "85k", "um-25k", "um-45k", "um-85k", "um5g-25k", "um5g-45k", "um5g-85k"]) . optLong) ecp5Options)

-- | Mirror of @ECP5CommandHandler::validate@: at most one device type.
checkSingleDevice :: [(String, Maybe String)] -> Either String ()
checkSingleDevice opts =
    case filter (`elem` deviceOptions) (map fst opts) of
        (_ : _ : _) -> Left "Only one device type can be set"
        _ -> Right ()

-- | The version banner, shaped like nextpnr's.
versionLine :: String -> String
versionLine prog = prog ++ " -- Next Generation Place and Route (Version " ++ version ++ ")"
  where
    version = "0.1.0.0"

{- | Render the help text in boost::program_options layout: a two-space
indent, the option spec padded to a shared column, the description
wrapped at 78 characters with continuation lines aligned under the
description. Group headers separate the general and arch sections.
-}
renderHelp :: String -> [Option] -> [Option] -> String
renderHelp prog general arch = unlines (versionLine prog : "" : renderGroup "General options" general ++ renderGroup "Architecture specific options" arch)
  where
    renderGroup title opts = (title ++ ":") : concatMap (renderOption colWidth) opts
      where
        colWidth = maximum (map (length . spec) opts)

    spec o =
        let longPart = "--" ++ optLong o
            namePart = case optShort o of
                Just s -> "-" ++ [s] ++ " [ " ++ longPart ++ " ]"
                Nothing -> longPart
            argPart = if optHasArg o then " arg" else ""
         in namePart ++ argPart

    -- one option: the first line has the spec + description, continuation
    -- lines wrap at word boundaries aligned under the description, with the
    -- whole line capped at 80 columns (boost's default terminal width)
    renderOption w o =
        let indent = replicate (w + 6) ' '
            descWidth = 80 - (w + 6)
            (first, rest) = spanAccum descWidth (words (optDesc o))
            firstLine =
                "  "
                    ++ spec o
                    ++ replicate (w - length (spec o)) ' '
                    ++ "  "
                    ++ unwords first
         in firstLine : wrap indent descWidth rest
      where
        -- greedily take words while the line stays within the width
        spanAccum dw = go 0 []
          where
            go _ acc [] = (reverse acc, [])
            go n acc (w : ws)
                | null acc || n + length w + 1 <= dw = go (n + length w + 1) (w : acc) ws
                | otherwise = (reverse acc, w : ws)
        wrap _ _ [] = []
        wrap ind dw ws =
            let (line, rest) = spanAccum dw ws
             in (ind ++ unwords line) : wrap ind dw rest

-- | Parse the command line against the option table. @-h\/--help@ and
-- @-V\/--version@ win over everything else, like nextpnr's
-- @vm.count("help")@ check after the full parse.
parseArgs :: [Option] -> [String] -> Either String Command
parseArgs table args
    | "-h" `elem` args || "--help" `elem` args = Right Help
    | "-V" `elem` args || "--version" `elem` args = Right Version
    | otherwise = Run <$> go args
  where
    go [] = pure []
    go (a : rest)
        | Just (name, val) <- splitEq a = do
            o <- lookupLong name
            case val of
                Just v
                    | optHasArg o -> ((name, Just v) :) <$> go rest
                    | otherwise -> Left ("option '--" ++ name ++ "' does not take an argument")
                Nothing
                    | optHasArg o -> Left ("option '--" ++ name ++ "' requires an argument")
                    | otherwise -> ((name, Nothing) :) <$> go rest
        | Just name <- stripLong a = do
            o <- lookupLong name
            if optHasArg o
                then case rest of
                    (v : rest') -> ((name, Just v) :) <$> go rest'
                    [] -> Left ("option '--" ++ name ++ "' requires an argument")
                else ((name, Nothing) :) <$> go rest
        | Just s <- stripShort a = do
            o <- lookupShort s
            if optHasArg o
                then case rest of
                    (v : rest') -> ((optLong o, Just v) :) <$> go rest'
                    [] -> Left ("option '-" ++ [s] ++ "' requires an argument")
                else ((optLong o, Nothing) :) <$> go rest
        | otherwise = Left ("unrecognised option '" ++ a ++ "'")
    splitEq a = case break (== '=') a of
        (n, '=' : v) | Just n' <- stripLong n -> Just (n', Just v)
        _ -> Nothing
    stripLong ('-' : '-' : n) | not (null n) = Just n
    stripLong _ = Nothing
    stripShort ('-' : [s]) | s /= '-' = Just s
    stripShort _ = Nothing
    lookupLong n = case filter ((== n) . optLong) table of
        o : _ -> Right o
        [] -> Left ("unrecognised option '--" ++ n ++ "'")
    lookupShort s = case filter ((== Just s) . optShort) table of
        o : _ -> Right o
        [] -> Left ("unrecognised option '-" ++ [s] ++ "'")

-- ---------------------------------------------------------------------------
-- Option semantics (mirrors ECP5CommandHandler::createContext +
-- CommandHandler::setupContext)

-- | Is the device a 5G part (only speed grade 8 available)?
is5g :: Ecp5Device -> Bool
is5g d = d `elem` [Lfe5um5g25f, Lfe5um5g45f, Lfe5um5g85f]

-- | The value of an option, if given.
optValue :: [(String, Maybe String)] -> String -> Maybe String
optValue opts n = snd =<< find ((== n) . fst) opts

-- | Is the option present?
optSet :: [(String, Maybe String)] -> String -> Bool
optSet opts n = any ((== n) . fst) opts

{- | Build the architecture arguments from the parsed options, mirroring
@ECP5CommandHandler::createContext@: device flags select the part
(default LFE5U-45F), @--package@ defaults to CABGA381 (with the
deprecation warning), @--speed@ accepts 6\/7\/8, and 5G parts force
speed grade 8 (@SPEED_8_5G@). The second component of the result is
the list of warnings to print.
-}
ecp5ArgsFromOpts :: [(String, Maybe String)] -> Either String (Ecp5Args, [String])
ecp5ArgsFromOpts opts = do
    let dev = deviceFromOpts opts
        pkg = optValue opts "package"
        spd = case optValue opts "speed" of
            Nothing -> Right (if is5g dev then Speed8 else Speed6)
            Just s -> case reads s of
                [(6, "")] -> Right Speed6
                [(7, "")] -> Right Speed7
                [(8, "")] -> Right Speed8
                _ -> Left ("Unsupported speed grade '" ++ s ++ "'")
        warnings
            | optSet opts "package" && maybe False (not . null) (optValue opts "package") = []
            | otherwise = ["Use of default value for --package is deprecated. Please add '--package " ++ T.unpack defaultPkg ++ "' to arguments."]
    speed <- spd
    if is5g dev && speed /= Speed8
        then Left "Only speed grade 8 is available for 5G parts"
        else
            Right
                ( Ecp5Args
                    dev
                    (case pkg of
                        Just p | not (null p) -> T.pack p
                        _ -> defaultPkg
                    )
                    (if is5g dev then Speed85g else speed)
                , warnings
                )
  where
    defaultPkg = "CABGA381"

-- | The device selected by the device flags (default LFE5U-45F).
deviceFromOpts :: [(String, Maybe String)] -> Ecp5Device
deviceFromOpts opts
    | optSet opts "12k" = Lfe5u12f
    | optSet opts "25k" = Lfe5u25f
    | optSet opts "45k" = Lfe5u45f
    | optSet opts "85k" = Lfe5u85f
    | optSet opts "um-25k" = Lfe5um25f
    | optSet opts "um-45k" = Lfe5um45f
    | optSet opts "um-85k" = Lfe5um85f
    | optSet opts "um5g-25k" = Lfe5um5g25f
    | optSet opts "um5g-45k" = Lfe5um5g45f
    | optSet opts "um5g-85k" = Lfe5um5g85f
    | otherwise = Lfe5u45f -- the C++ default when no device flag is set

-- | The @arch.speed@ setting string (SPEED_8_5G reports as "8").
speedString :: SpeedGrade -> String
speedString Speed6 = "6"
speedString Speed7 = "7"
speedString _ = "8" -- SPEED_8 and SPEED_8_5G

-- | The @arch.type@ setting string (@archArgsToId@).
deviceTypeName :: Ecp5Device -> String
deviceTypeName d = case d of
    Lfe5u12f -> "lfe5u_12f"
    Lfe5u25f -> "lfe5u_25f"
    Lfe5u45f -> "lfe5u_45f"
    Lfe5u85f -> "lfe5u_85f"
    Lfe5um25f -> "lfe5um_25f"
    Lfe5um45f -> "lfe5um_45f"
    Lfe5um85f -> "lfe5um_85f"
    Lfe5um5g25f -> "lfe5um5g_25f"
    Lfe5um5g45f -> "lfe5um5g_45f"
    Lfe5um5g85f -> "lfe5um5g_85f"

-- | Available placer/router algorithms (the ecp5 statics).
availablePlacers :: [String]
availablePlacers = ["sa", "heap", "static"]

availableRouters :: [String]
availableRouters = ["router1", "router2"]

{- | Apply the general option semantics to a context, mirroring
@CommandHandler::setupContext@ + the ecp5 @arch.*@ settings from
@createContext@: RNG seeding (@--seed@\/@--randomize-seed@), the
placer\/router\/placer-heap\/timing settings, and the defaults
(@target_freq@, @timing_driven@, @placer@, @router@, @arch.*@, ...).
All settings are stored as strings (read back through the typed
'getSetting' accessors), except booleans and the seed, which are
numeric properties like the C++.
-}
applyGeneralOpts :: Ecp5Args -> [(String, Maybe String)] -> Context arch -> IO (Either String (Context arch))
applyGeneralOpts args opts ctx0 = do
    -- createContext (ecp5 main.cc): intern arch.package / arch.speed
    -- BEFORE anything else (the interning ORDER matters: the checksum
    -- hashes id indices, so it must match the C++ exactly)
    ctx1 <- setSetting ctx0 "arch.package" (propFromString (eaPackage args))
    ctx2 <- setSetting ctx1 "arch.speed" (propFromString (T.pack (speedString (eaSpeed args))))
    -- setupContext first line: settings.find(id("seed")) — interns "seed"
    -- here, but the C++ only INSERTS the setting later (after the flag
    -- section, in the defaults tail), so only the interning happens now.
    _ <- intern (ctxIdTable ctx2) "seed"
    let ctx3 = ctx2
    -- flag section in the C++ setupContext order (NOT command-line order)
    r1 <- foldM applyOne (Right ctx3) flagOrder
    case r1 of
        Left err -> pure (Left err)
        Right ctx4 -> Right <$> applyDefaults ctx4
  where
    -- the C++ setupContext processing order (0.10)
    flagOrder =
        [ "seed"
        , "threads"
        , "slack_redist_iter"
        , "ignore-loops"
        , "ignore-rel-clk"
        , "timing-allow-fail"
        , "placer"
        , "router"
        , "cstrweight"
        , "starttemp"
        , "freq"
        , "no-tmdriv"
        , "placer-heap-alpha"
        , "placer-heap-beta"
        , "placer-heap-critexp"
        , "placer-heap-timingweight"
        , "placer-heap-cell-placement-timeout"
        , "placer-heap-no-ctrl-set"
        , "parallel-refine"
        , "router2-heatmap"
        , "tmg-ripup"
        , "router2-tmg-ripup"
        , "router2-alt-weights"
        , "static-dump-density"
        ]
    val :: String -> Maybe String
    val n = lookup n opts >>= id

    applyOne (Left err) _ = pure (Left err)
    applyOne (Right ctx) name
        | name `notElem` map fst opts = pure (Right ctx) -- C++ skips absent flags
        | otherwise = case name of
        "seed" -> case val "seed" of
            Just s -> case reads s of
                [(n, "")] -> pure (Right ctx{ctxRng = rngSeed (fromIntegral n) (ctxRng ctx)})
                _ -> pure (Left ("invalid seed '" ++ s ++ "'"))
            Nothing -> pure (Right ctx) -- no --seed flag: nothing to do
        "randomize-seed" -> do
            n <- randomSeed
            putStrLn ("Generated random seed: " ++ show n)
            pure (Right ctx{ctxRng = rngSeed (fromIntegral n) (ctxRng ctx)})
        "threads" -> setNum "threads" ctx (val "threads")
        "slack_redist_iter" -> do
            r <- setNum "slack_redist_iter" ctx (val "slack_redist_iter")
            case r of
                Left err -> pure (Left err)
                Right ctx'
                    | Just f <- val "freq" >>= readMaybeDouble
                    , f == 0 -> setBool "auto_freq" ctx'
                    | otherwise -> pure (Right ctx')
        "ignore-loops" -> setBool "timing/ignoreLoops" ctx
        "ignore-rel-clk" -> setBool "timing/ignoreRelClk" ctx
        "timing-allow-fail" -> setBool "timing/allowFail" ctx
        "placer" -> do
            case val "placer" of
                Just p
                    | p `elem` availablePlacers -> setStr "placer" ctx p
                    | otherwise ->
                        pure (Left ("Placer algorithm '" ++ p ++ "' is not supported (available options: " ++ intercalate ", " availablePlacers ++ ")"))
                Nothing -> pure (Right ctx)
        "router" -> do
            case val "router" of
                Just r
                    | r `elem` availableRouters -> setStr "router" ctx r
                    | otherwise ->
                        pure (Left ("Router algorithm '" ++ r ++ "' is not supported (available options: " ++ intercalate ", " availableRouters ++ ")"))
                Nothing -> pure (Right ctx)
        "cstrweight" -> setStrF "placer1/constraintWeight" ctx (val "cstrweight")
        "starttemp" -> setStrF "placer1/startTemp" ctx (val "starttemp")
        "freq" -> case val "freq" >>= readMaybeDouble of
            Just f | f > 0 -> setStr "target_freq" ctx (show (f * 1e6))
            _ -> pure (Right ctx)
        "no-tmdriv" -> setBoolFalse "timing_driven" ctx
        "placer-heap-alpha" -> setStrF "placerHeap/alpha" ctx (val "placer-heap-alpha")
        "placer-heap-beta" -> setStrF "placerHeap/beta" ctx (val "placer-heap-beta")
        "placer-heap-critexp" -> case val "placer-heap-critexp" of
            Just v -> setStr "placerHeap/criticalityExponent" ctx v
            Nothing -> pure (Right ctx)
        "placer-heap-timingweight" -> case val "placer-heap-timingweight" of
            Just v -> setStr "placerHeap/timingWeight" ctx v
            Nothing -> pure (Right ctx)
        "placer-heap-cell-placement-timeout" -> case val "placer-heap-cell-placement-timeout" >>= readMaybeInt of
            Just n -> setStr "placerHeap/cellPlacementTimeout" ctx (show (max 0 n))
            _ -> pure (Right ctx)
        "placer-heap-no-ctrl-set" -> setBool "placerHeap/noCtrlSet" ctx
        "parallel-refine" -> setBool "placerHeap/parallelRefine" ctx
        "router2-heatmap" -> case val "router2-heatmap" of
            Just v -> setStr "router2/heatmap" ctx v
            Nothing -> pure (Right ctx)
        "tmg-ripup" -> setBool "router/tmg_ripup" ctx
        "router2-tmg-ripup" -> setBool "router/tmg_ripup" ctx
        "router2-alt-weights" -> setBool "router2/alt-weights" ctx
        "static-dump-density" -> setBool "static/dump_density" ctx
        "top" -> case val "top" of
            Just v -> setStr "frontend/top" ctx v
            Nothing -> pure (Right ctx)
        "no-promote-globals" -> setBool "arch.no-promote-globals" ctx
        "out-of-context" -> setBool "arch.ooc" ctx
        -- accepted, no-op until the corresponding machinery lands:
        -- verbose, debug, debug-placer, debug-router, quiet, Werror,
        -- log, force, help, version, test, json, write, sdc, sdf,
        -- sdf-cvc, no-print-critical-path-source, report,
        -- detailed-timing-report, placed-svg, routed-svg, pack-only,
        -- no-pack, no-place, no-route, basecfg, override-basecfg,
        -- textcfg, lpf, lpf-allow-unconstrained, disable-router-lutperm,
        -- allow-fabric-eclk, 12k..um5g-85k (device handled above)
        _ -> pure (Right ctx)

    -- defaults, mirroring the tail of setupContext: only set when
    -- absent, so user options win. placer/router are constids in the
    -- chipdb (no table growth); ARCHNAME is interned by archId(). The
    -- values match the C++ stores exactly (to_string(12e6), the 32-bit
    -- int Property for bools/ints, to_string(0.1) etc.).
    applyDefaults :: Context arch -> IO (Context arch)
    applyDefaults ctx =
        foldM setDefault ctx
            [ ("target_freq", propFromString "12000000.000000")
            , ("timing_driven", propFromInt 1 32)
            , ("slack_redist_iter", propFromInt 0 32)
            , ("auto_freq", propFromInt 0 32)
            , ("placer", propFromString "heap")
            , ("router", propFromString "router1")
            , ("ARCHNAME", propFromString "ARCHNAME")
            , ("arch.name", propFromString "ARCHNAME")
            , ("arch.type", propFromString (T.pack (deviceTypeName (eaDevice args))))
            , ("seed", propFromInt (fromIntegral (rngState (ctxRng ctx))) 64)
            , ("placerHeap/alpha", propFromString "0.100000")
            , ("placerHeap/beta", propFromString "0.900000")
            , ("placerHeap/criticalityExponent", propFromString "2")
            , ("placerHeap/timingWeight", propFromString "10")
            ]
      where
        setDefault c (k, p) = do
            key <- intern (ctxIdTable c) (T.pack k)
            if M.member key (ctxSettings c) || k == "ARCHNAME"
                -- ARCHNAME is interned by archId() but never stored as a
                -- setting
                then pure c
                else setSetting c (T.pack k) p

    -- stored as numeric properties (the C++ assigns @Property(bool)@,
    -- a 32-bit int Property; bool_or_default reads the integer value)
    propTrue = propFromInt 1 32
    propFalse = propFromInt 0 32

    setStr :: String -> Context arch -> String -> IO (Either String (Context arch))
    setStr k ctx v = Right <$> setSetting ctx (T.pack k) (propFromString (T.pack v))
    setStrF :: String -> Context arch -> Maybe String -> IO (Either String (Context arch))
    setStrF k ctx (Just v) = setStr k ctx v
    setStrF k _ Nothing = pure (Left ("option '--" ++ k ++ "' requires an argument"))
    setNum :: String -> Context arch -> Maybe String -> IO (Either String (Context arch))
    setNum k ctx (Just v) = case reads v of
        [(n, "")] -> Right <$> setSetting ctx (T.pack k) (propFromInt (fromIntegral n) 64)
        _ -> pure (Left ("invalid --" ++ k ++ " value '" ++ v ++ "'"))
    setNum k _ Nothing = pure (Left ("option '--" ++ k ++ "' requires an argument"))
    setBool :: String -> Context arch -> IO (Either String (Context arch))
    setBool k ctx = Right <$> setSetting ctx (T.pack k) propTrue
    setBoolFalse :: String -> Context arch -> IO (Either String (Context arch))
    setBoolFalse k ctx = Right <$> setSetting ctx (T.pack k) propFalse
    readMaybeDouble s = case reads s of
        [(d, "")] -> Just d
        _ -> Nothing
    readMaybeInt s = case reads s of
        [(i, "")] -> Just i
        _ -> Nothing

-- | A non-zero random seed from the OS entropy source (the C++
-- @std::random_device@).
randomSeed :: IO Integer
randomSeed = do
    h <- openBinaryFile "/dev/urandom" ReadMode
    bs <- BS.hGet h 8
    hClose h
    pure (BS.foldl' (\acc w -> acc * 256 + fromIntegral w) 0 bs)
