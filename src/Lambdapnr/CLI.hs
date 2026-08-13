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
) where

import Data.List (intercalate)

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
