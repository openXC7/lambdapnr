{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

{- | The @--sdf@ delay back-annotation writer — a port of
@common\/kernel\/sdf.cc@ (@Context::writeSDF@), byte-comparable with the
oracle. Delay values go through the float @getDelayNS@ times 1000 and
are printed with the C++ @std::ostream@ default format (C @%g@ with
precision 6).
-}
module Lambdapnr.Kernel.Sdf (
    writeSdf,
) where

import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Lambdapnr.Kernel.Arch (Arch (..), Bel, Pip, Wire)
import Lambdapnr.Kernel.CFormat (formatG)
import Lambdapnr.Kernel.Delay (DelayPair (..), DelayQuad (..))
import Lambdapnr.Kernel.IdString (IdString, IdTable, idToText, intern)
import Lambdapnr.Kernel.Netlist (CellInfo, Design (..), NetInfo (..), PortDir (..), PortInfo (..), cellName, cellPortOrder, cellPorts, cellType, portNet, portType, prCell, prPort)
import Lambdapnr.Kernel.Property (Property, propAsString)
import Lambdapnr.Kernel.Timing (ClockEdge (..), TimingClockingInfo (..), TimingPortClass (..))
import Lambdapnr.Kernel.TimingReport (netinfoRouteDelayQuad)

-- | @escape_name@.
escapeName :: Bool -> String -> String
escapeName cvc = concatMap esc
  where
    esc c
        | c == '$' || c == '\\' || c == '[' || c == ']' || c == ':' || (cvc && c == '.') = ['\\', c]
        | otherwise = [c]

-- | @format_name@.
formatName :: String -> String
formatName name = "\"" ++ concatMap (\c -> if c == '\\' || c == '"' then ['"', c] else [c]) name ++ "\""

data MinMaxTyp = MinMaxTyp !Double !Double !Double

data RiseFallDelay = RiseFallDelay {rfRise :: !MinMaxTyp, rfFall :: !MinMaxTyp}

data IOPath = IOPath {ioFrom :: !String, ioTo :: !String, ioDelay :: !RiseFallDelay}

data TimingCheck = TimingCheck
    { tcSetuphold :: !Bool
    , tcFrom :: !(String, ClockEdge)
    , tcTo :: !(String, ClockEdge)
    , tcDelay :: !RiseFallDelay
    }

data SdfCell = SdfCell
    { scCelltype :: !String
    , scInstance :: !String
    , scIopaths :: ![IOPath]
    , scChecks :: ![TimingCheck]
    }

data Interconnect = Interconnect
    { icFrom :: !(String, String)
    , icTo :: !(String, String)
    , icDelay :: !RiseFallDelay
    }

writeDelay :: Bool -> MinMaxTyp -> String
writeDelay cvc (MinMaxTyp mn ty mx)
    | cvc = "(" ++ show (truncate mn :: Int) ++ ":" ++ show (truncate ty :: Int) ++ ":" ++ show (truncate mx :: Int) ++ ")"
    | otherwise = "(" ++ formatG 6 mn ++ ":" ++ formatG 6 ty ++ ":" ++ formatG 6 mx ++ ")"

writeRfDelay :: Bool -> RiseFallDelay -> String
writeRfDelay cvc rf = writeDelay cvc (rfRise rf) ++ " " ++ writeDelay cvc (rfFall rf)

-- | @write_port@.
writePort :: Bool -> (String, String) -> String
writePort cvc (cell, port)
    | cvc = escapeName cvc cell ++ "." ++ escapeName cvc port
    | otherwise = escapeName cvc (cell ++ "/" ++ port)

-- | @write_portedge@.
writePortedge :: (String, ClockEdge) -> String
writePortedge (port, edge) = "(" ++ (if edge == RisingEdge then "posedge" else "negedge") ++ " " ++ escapeName False port ++ ")"

-- | @Context::writeSDF@.
writeSdf ::
    (Arch a, Ord (Wire a)) =>
    a ->
    IdTable ->
    -- | module attrs (for the DESIGN name, @module@ key default @top@)
    M.Map IdString Property ->
    Bool {- cvc mode -} ->
    (CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> (TimingPortClass, Int)) ->
    (CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> Int -> TimingClockingInfo) ->
    (CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> IdString -> Maybe DelayQuad) ->
    (NetInfo (Bel a) (Wire a) (Pip a) -> Bool) ->
    Design (Bel a) (Wire a) (Pip a) ->
    IO String
writeSdf arch tbl attrs cvc ptc pci getDelay isGlobalNet d = do
    moduleId <- intern tbl "module"
    pure (go moduleId)
  where
    go moduleId =
        let design = if M.member moduleId attrs then T.unpack (propAsString (attrs M.! moduleId)) else "top"
         in renderTop design cellEntries renderConns
    idT = T.unpack . idToText tbl
    delayScale = 1000 :: Double
    cellsMap = designCells d
    netsMap = designNets d
    cellEntries = map renderCellOf (reverse (designCellOrder d))

    convertDelay :: DelayQuad -> RiseFallDelay
    convertDelay dly =
        let rMin = realToFrac (getDelayNS arch (dpMin (dqRise dly))) * delayScale
            rMax = realToFrac (getDelayNS arch (dpMax (dqRise dly))) * delayScale
            rTyp = realToFrac (getDelayNS arch ((dpMin (dqRise dly) + dpMax (dqRise dly)) `div` 2)) * delayScale
            fMin = realToFrac (getDelayNS arch (dpMin (dqFall dly))) * delayScale
            fMax = realToFrac (getDelayNS arch (dpMax (dqFall dly))) * delayScale
            fTyp = realToFrac (getDelayNS arch ((dpMin (dqFall dly) + dpMax (dqFall dly)) `div` 2)) * delayScale
         in RiseFallDelay (MinMaxTyp rMin rTyp rMax) (MinMaxTyp fMin fTyp fMax)

    convertSetuphold :: DelayPair -> DelayPair -> RiseFallDelay
    convertSetuphold setup hold =
        let sMin = realToFrac (getDelayNS arch (dpMin setup)) * delayScale
            sMax = realToFrac (getDelayNS arch (dpMax setup)) * delayScale
            sTyp = realToFrac (getDelayNS arch ((dpMin setup + dpMax setup) `div` 2)) * delayScale
            hMin = realToFrac (getDelayNS arch (dpMin hold)) * delayScale
            hMax = realToFrac (getDelayNS arch (dpMax hold)) * delayScale
            hTyp = realToFrac (getDelayNS arch ((dpMin hold + dpMax hold) `div` 2)) * delayScale
         in RiseFallDelay (MinMaxTyp sMin sTyp sMax) (MinMaxTyp hMin hTyp hMax)

    renderTop design cellEntries conns =
        "(DELAYFILE\n"
            ++ "  (SDFVERSION " ++ formatName "3.0" ++ ")\n"
            ++ "  (DESIGN " ++ formatName design ++ ")\n"
            ++ "  (VENDOR " ++ formatName "nextpnr" ++ ")\n"
            ++ "  (PROGRAM " ++ formatName "nextpnr" ++ ")\n"
            ++ "  (DIVIDER " ++ (if cvc then "." else "/") ++ ")\n"
            ++ "  (TIMESCALE 1ps)\n"
            ++ "  (CELL\n"
            ++ "    (CELLTYPE " ++ formatName design ++ ")\n"
            ++ "    (INSTANCE )\n"
            ++ "    (DELAY\n"
            ++ "      (ABSOLUTE\n"
            ++ concatMap (\ic -> "        (INTERCONNECT " ++ writePort cvc (icFrom ic) ++ " " ++ writePort cvc (icTo ic) ++ " " ++ writeRfDelay cvc (icDelay ic) ++ ")\n") conns
            ++ "      )\n"
            ++ "    )\n"
            ++ "  )\n"
            ++ concatMap renderCell cellEntries
            ++ ")\n"

    renderCell (SdfCell celltype instance' iopaths checks) =
        "  (CELL\n"
            ++ "    (CELLTYPE " ++ formatName celltype ++ ")\n"
            ++ "    (INSTANCE " ++ escapeName cvc instance' ++ ")\n"
            ++ ( if null iopaths
                    then ""
                    else "    (DELAY\n"
                        ++ "      (ABSOLUTE\n"
                        ++ concatMap (\p -> "        (IOPATH " ++ escapeName cvc (ioFrom p) ++ " " ++ escapeName cvc (ioTo p) ++ " " ++ writeRfDelay cvc (ioDelay p) ++ ")\n") iopaths
                        ++ "      )\n"
                        ++ "    )\n"
               )
            ++ ( if null checks
                    then ""
                    else "    (TIMINGCHECK\n"
                        ++ concatMap renderCheck checks
                        ++ "    )\n"
               )
            ++ "    )\n"

    renderCheck chk =
        "      (SETUPHOLD "
            ++ writePortedge (tcFrom chk)
            ++ " "
            ++ writePortedge (tcTo chk)
            ++ " "
            ++ writeRfDelay cvc (tcDelay chk)
            ++ ")\n"

    -- the per-cell build (writeSDF's cell loop)
    renderCellOf :: IdString -> SdfCell
    renderCellOf k =
        let ci = fromMaybe (error "sdf: missing cell") (M.lookup k cellsMap)
            portsOrder = reverse (cellPortOrder ci)
            (iopaths, checks) = foldl' (portStep ci) ([], []) portsOrder
         in SdfCell (idT (cellType ci)) (idT k) iopaths checks

    portStep ci (iopaths, checks) p =
        case M.lookup p (cellPorts ci) of
            Nothing -> (iopaths, checks)
            Just pi ->
                let (cls, clockCount) = ptc ci p
                 in if cls == TmgIgnore || portNet pi == Nothing
                        then (iopaths, checks)
                        else
                            let portsOrder = reverse (cellPortOrder ci)
                                combPaths =
                                    [ IOPath (idT o) (idT p) (convertDelay dly')
                                    | o <- portsOrder
                                    , Just opi <- [M.lookup o (cellPorts ci)]
                                    , portNet opi /= Nothing
                                    , portType opi /= PortOut
                                    , Just dly' <- [getDelay ci o p]
                                    ]
                                c2qPaths =
                                    [ IOPath (idT (tciClockPort clkInfo)) (idT p) (convertDelay (tciClockToQ clkInfo))
                                    | cls == TmgRegisterOutput
                                    , i <- [0 .. clockCount - 1]
                                    , let clkInfo = pci ci p i
                                    ]
                                setupChecks =
                                    [ TimingCheck True (idT p, RisingEdge) (idT (tciClockPort clkInfo), tciEdge clkInfo) (convertSetuphold (tciSetup clkInfo) (tciHold clkInfo))
                                    | portType pi /= PortOut
                                    , cls == TmgRegisterInput
                                    , i <- [0 .. clockCount - 1]
                                    , let clkInfo = pci ci p i
                                    ]
                                fallChecks =
                                    [ TimingCheck True (idT p, FallingEdge) (idT (tciClockPort clkInfo), tciEdge clkInfo) (convertSetuphold (tciSetup clkInfo) (tciHold clkInfo))
                                    | portType pi /= PortOut
                                    , cls == TmgRegisterInput
                                    , i <- [0 .. clockCount - 1]
                                    , let clkInfo = pci ci p i
                                    ]
                                iopaths' = if portType pi == PortIn then iopaths else iopaths ++ combPaths ++ c2qPaths
                                checks' = checks ++ setupChecks ++ fallChecks
                             in (iopaths', checks')

    -- the interconnect section: every driver-net user (dict order,
    -- users in slot order)
    renderConns :: [Interconnect]
    renderConns =
        [ Interconnect (idT drvCell, idT (prPort (netDriver ni))) (idT uCell, idT (prPort u)) (convertDelay (netinfoRouteDelayQuad arch isGlobalNet cellsMap netsMap ni u))
        | k <- reverse (designNetOrder d)
        , Just ni <- [M.lookup k netsMap]
        , Just drvCell <- [prCell (netDriver ni)]
        , u <- [x | Just x <- V.toList (netUsers ni)]
        , Just uCell <- [prCell u]
        ]
