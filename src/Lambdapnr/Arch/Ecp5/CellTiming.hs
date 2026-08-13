{-# LANGUAGE OverloadedStrings #-}

{- | ECP5 cell timing: port classification, combinational arcs and
clocking info, mirroring @ecp5\/arch.cc@ (@getCellDelay@,
@getPortTimingClass@, @getPortClockingInfo@, @get_delay_from_tmg_db@,
@get_setuphold_from_tmg_db@).

The timing database (cell timings per speed grade) is looked up by
constid indices, so the functions here are pure over a small 'TimingDb'
view of the arch. Everything payload-free is ported exactly; the
corners that depend on the packed-cell @ArchCellInfo@ payload (carry
variant selection, @FF_M_USED@, clock-inversion edges, @multInfo@ and
@ramInfo@ timing ids) use the conservative default until the packer
lands — each is marked with a @TODO(payload)@ note.
-}
module Lambdapnr.Arch.Ecp5.CellTiming (
    TimingDb (..),
    getDelayFromTmgDb,
    getSetupholdFromTmgDb,
    getCellDelayFor,
    getPortTimingClassFor,
    getPortClockingInfoFor,
) where

import Data.Char (isDigit)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Lambdapnr.Arch.Ecp5.Chipdb (CellPropDelay (..), CellSetupHold (..), CellTiming (..), SpeedGrade (..))
import Lambdapnr.Arch.Ecp5.Types (BelId, PipId, WireId)
import Lambdapnr.Kernel.Delay (DelayPair (..), DelayQuad, DelayT, dpFromDelay, dqFromDelay, dqScalar)
import Lambdapnr.Kernel.IdString (IdString, IdTable, emptyId, idToText)
import Lambdapnr.Kernel.Netlist (CellInfo, PortDir (..), PortInfo (..), cellPorts, cellType, portNet, portType)
import Lambdapnr.Kernel.Timing (ClockEdge (..), TimingClockingInfo (..), TimingPortClass (..))

-- | The arch view the timing functions need.
data TimingDb = TimingDb
    { tdIdTable :: !IdTable
    -- ^ name resolution for the name-based port classification
    , tdConstIdIndex :: !(Map IdString Int)
    -- ^ interned id -> chipdb constid index
    , tdConstIdByName :: !(Map Text IdString)
    -- ^ constid name -> interned id (the @ID_*@ constants)
    , tdSpeedGrade :: !SpeedGrade
    -- ^ the selected speed grade's cell timings
    }

-- | The @ID_*@ constant for a constid name (empty id when absent).
cid :: TimingDb -> Text -> IdString
cid db name = M.findWithDefault emptyId name (tdConstIdByName db)

-- | Constid index of an interned id (Nothing if not in the table).
constIdIndex :: TimingDb -> IdString -> Maybe Int
constIdIndex db x = M.lookup x (tdConstIdIndex db)

-- | Resolve an id to its text ("" for the empty id).
resolve :: TimingDb -> IdString -> Text
resolve db = idToText (tdIdTable db)

-- | Interned-id equality helper: the C++ compares @IdString@ indices,
-- which is exactly what id equality is here.
(=:) :: IdString -> IdString -> Bool
(=:) = (==)

{- | @get_delay_from_tmg_db(tctype, from, to)@: combinational arc delay
lookup. The C++ asserts when the timing cell is missing; the Haskell
port returns 'Nothing'.
-}
getDelayFromTmgDb :: TimingDb -> IdString -> IdString -> IdString -> Maybe DelayQuad
getDelayFromTmgDb db tctype from to = do
    tci <- constIdIndex db tctype
    f <- constIdIndex db from
    t <- constIdIndex db to
    ct <- V.find ((== tci) . fromIntegral . ctCellType) (sgCellTimings (tdSpeedGrade db))
    a <- V.find (\x -> cpdFrom x == fromIntegral f && cpdTo x == fromIntegral t) (ctPropDelays ct)
    pure (dqScalar (fromIntegral (cpdMin a)) (fromIntegral (cpdMax a)))

{- | @get_setuphold_from_tmg_db(tctype, clock, port)@: input setup/hold
checks of a register port. 'Nothing' when absent.
-}
getSetupholdFromTmgDb :: TimingDb -> IdString -> IdString -> IdString -> Maybe (DelayPair, DelayPair)
getSetupholdFromTmgDb db tctype clock port = do
    tci <- constIdIndex db tctype
    c <- constIdIndex db clock
    p <- constIdIndex db port
    ct <- V.find ((== tci) . fromIntegral . ctCellType) (sgCellTimings (tdSpeedGrade db))
    sh <- V.find (\x -> cshClockPort x == fromIntegral c && cshSigPort x == fromIntegral p) (ctSetupHolds ct)
    pure
        ( DelayPair (fromIntegral (cshMinSetup sh)) (fromIntegral (cshMaxSetup sh))
        , DelayPair (fromIntegral (cshMinHold sh)) (fromIntegral (cshMaxHold sh))
        )

-- | Is a cell port disconnected (absent or with no net)? The C++
-- @disconnected@ lambda.
disconnected :: CellInfo BelId WireId PipId -> IdString -> Bool
disconnected cell p =
    case M.lookup p (cellPorts cell) of
        Nothing -> True
        Just pi -> portNet pi == Nothing

-- | Port direction of a cell port (defaults to inout when absent).
portDirOf :: CellInfo BelId WireId PipId -> IdString -> PortDir
portDirOf cell p = maybe PortInout portType (M.lookup p (cellPorts cell))

-- | The C++ @port.in(...)@ idiom: @port `anyOf` ports@.
anyOf :: (Eq a) => a -> [a] -> Bool
anyOf = elem

{- | @getCellDelay@: combinational arcs of a cell. Register arcs return
'Nothing' (they are clock-to-Q, reported by 'getPortClockingInfo').
-}
getCellDelayFor :: TimingDb -> CellInfo BelId WireId PipId -> IdString -> IdString -> Maybe DelayQuad
getCellDelayFor db cell from to
    | cellType cell =: cid db "TRELLIS_COMB" =
        if from `anyOf` [cid db "A", cid db "B", cid db "C", cid db "D", cid db "M", cid db "F1", cid db "FXA", cid db "FXB", cid db "FCI"]
            then getDelayFromTmgDb db (cid db "TRELLIS_COMB") from to
            -- TODO(payload): carry chains select TRELLIS_COMB_CARRY0/1 via
            -- combInfo.flags and constr_z
            else Nothing
    | cellType cell =: cid db "TRELLIS_FF" = Nothing
    | cellType cell =: cid db "TRELLIS_RAMW" =
        if
            (from =: cid db "A0" && to =: cid db "WADO3")
                || (from =: cid db "A1" && to =: cid db "WDO1")
                || (from =: cid db "B0" && to =: cid db "WADO1")
                || (from =: cid db "B1" && to =: cid db "WDO3")
                || (from =: cid db "C0" && to =: cid db "WADO2")
                || (from =: cid db "C1" && to =: cid db "WDO0")
                || (from =: cid db "D0" && to =: cid db "WADO0")
                || (from =: cid db "D1" && to =: cid db "WDO2")
            then Just (dqFromDelay 0)
            else Nothing
    | cellType cell =: cid db "DCCA" =
        if from =: cid db "CLKI" && to =: cid db "CLKO" then Just (dqFromDelay 0) else Nothing
    | cellType cell =: cid db "DCSC" =
        if from `anyOf` [cid db "CLK0", cid db "CLK1"] && to =: cid db "DCSOUT" then Just (dqFromDelay 0) else Nothing
    | otherwise = Nothing
    -- TODO(payload): MULT18X18D unclocked A/Bn -> Pn arcs need
    -- multInfo.timing_id; DP16KD/IOLOGIC have no combinational arcs.

{- | @getPortTimingClass@. Unknown cell types: the C++ @log_error@s; the
Haskell port ignores them (they have no timing paths).
-}
getPortTimingClassFor :: TimingDb -> CellInfo BelId WireId PipId -> IdString -> (TimingPortClass, Int)
getPortTimingClassFor db cell port
    | cellType cell =: cid db "TRELLIS_COMB" =
        if
            port =: cid db "WCK"
            then (TmgClockInput, 0)
            else
                if port `anyOf` [cid db "A", cid db "B", cid db "C", cid db "D", cid db "FCI", cid db "FXA", cid db "FXB", cid db "F1"]
                    then (TmgCombInput, 0)
                    else
                        if port =: cid db "F"
                            && and [disconnected cell p | p <- [cid db "A", cid db "B", cid db "C", cid db "D", cid db "FCI"]]
                            then (TmgIgnore, 0) -- LUT with no inputs is a constant
                            else
                                if port `anyOf` [cid db "F", cid db "FCO", cid db "OFX"]
                                    then (TmgCombOutput, 0)
                                    else
                                        if port =: cid db "M"
                                            then (TmgCombInput, 0)
                                            else
                                                if port `anyOf` [cid db "WD", cid db "WAD0", cid db "WAD1", cid db "WAD2", cid db "WAD3", cid db "WRE"]
                                                    then (TmgRegisterInput, 1)
                                                    else (TmgIgnore, 0)
    | cellType cell =: cid db "TRELLIS_FF" =
        -- TODO(payload): with FF_M_USED, port M is also a register input
        if
            port =: cid db "CLK"
            then (TmgClockInput, 0)
            else
                if port `anyOf` [cid db "DI", cid db "CE", cid db "LSR"]
                    then (TmgRegisterInput, 1)
                    else
                        if port =: cid db "Q"
                            then (TmgRegisterOutput, 1)
                            else (TmgIgnore, 0)
    | cellType cell =: cid db "TRELLIS_RAMW" =
        if
            port `anyOf` [cid db "A0", cid db "A1", cid db "B0", cid db "B1", cid db "C0", cid db "C1", cid db "D0", cid db "D1"]
            then (TmgCombInput, 0)
            else
                if port `anyOf` [cid db "WDO0", cid db "WDO1", cid db "WDO2", cid db "WDO3", cid db "WADO0", cid db "WADO1", cid db "WADO2", cid db "WADO3"]
                    then (TmgCombOutput, 0)
                    else (TmgIgnore, 0)
    | cellType cell =: cid db "TRELLIS_IO" =
        if
            port `anyOf` [cid db "T", cid db "I"]
            then (TmgEndpoint, 0)
            else
                if port =: cid db "O"
                    then (TmgStartpoint, 0)
                    else (TmgIgnore, 0)
    | cellType cell =: cid db "DCCA" =
        if
            port =: cid db "CLKI"
            then (TmgCombInput, 0)
            else
                if port =: cid db "CLKO"
                    then (TmgCombOutput, 0)
                    else (TmgIgnore, 0)
    | cellType cell =: cid db "DCSC" =
        if
            port `anyOf` [cid db "CLK0", cid db "CLK1"]
            then (TmgCombInput, 0)
            else
                if port =: cid db "DCSOUT"
                    then (TmgCombOutput, 0)
                    else (TmgIgnore, 0)
    | cellType cell =: cid db "DP16KD" =
        if
            port `anyOf` [cid db "CLKA", cid db "CLKB"]
            then (TmgClockInput, 0)
            else case lastNonDigit (resolve db port) of
                'A' -> (TmgRegisterInput, 1)
                'B' -> (TmgRegisterInput, 1)
                _ -> (TmgIgnore, 0)
    | cellType cell =: cid db "MULT18X18D" =
        -- TODO(payload): is_clocked selects REGISTER_* with clockInfoCount=1
        if
            port `anyOf` [cid db "CLK0", cid db "CLK1", cid db "CLK2", cid db "CLK3"]
            then (TmgClockInput, 0)
            else
                if port `anyOf` [cid db "CE0", cid db "CE1", cid db "CE2", cid db "CE3", cid db "RST0", cid db "RST1", cid db "RST2", cid db "RST3", cid db "SIGNEDA", cid db "SIGNEDB"]
                    then (TmgCombInput, 0)
                    else case resolve db port of
                        t
                            | T.length t > 1 && (T.head t == 'A' || T.head t == 'B') && isDigit (T.index t 1) ->
                                (TmgCombInput, 0)
                            | T.length t > 1 && T.head t == 'P' && isDigit (T.index t 1) -> (TmgCombOutput, 0)
                            | otherwise -> (TmgIgnore, 0)
    | cellType cell =: cid db "ALU54B" = (TmgIgnore, 0)
    | cellType cell =: cid db "EHXPLLL" = (TmgIgnore, 0)
    | cellType cell `anyOf` [cid db "DCUA", cid db "EXTREFB", cid db "PCSCLKDIV"] =
        if
            port `anyOf` [cid db "CH0_FF_TXI_CLK", cid db "CH0_FF_RXI_CLK", cid db "CH1_FF_TXI_CLK", cid db "CH1_FF_RXI_CLK"]
            then (TmgClockInput, 0)
            else
                if port `anyOf` [cid db "CH0_FF_TX_PCLK", cid db "CH0_FF_RX_PCLK", cid db "CH1_FF_TX_PCLK", cid db "CH1_FF_RX_PCLK"]
                    then (TmgGenClock, 0)
                    else
                        if T.take 9 (resolve db port) `elem` ["CH0_FF_TX", "CH0_FF_RX", "CH1_FF_TX", "CH1_FF_RX"]
                            then (if portDirOf cell port == PortOut then TmgRegisterOutput else TmgRegisterInput, 1)
                            else (TmgIgnore, 0)
    | cellType cell `anyOf` [cid db "IOLOGIC", cid db "SIOLOGIC"] =
        if
            port `anyOf` [cid db "CLK", cid db "ECLK"]
            then (TmgClockInput, 0)
            else
                if port `anyOf` [cid db "IOLDO", cid db "IOLDOI", cid db "IOLDOD", cid db "IOLTO", cid db "PADDI", cid db "DQSR90", cid db "DQSW", cid db "DQSW270"]
                    then (TmgIgnore, 0)
                    else (if portDirOf cell port == PortOut then TmgRegisterOutput else TmgRegisterInput, 1)
    | cellType cell `anyOf` [cid db "DTR", cid db "USRMCLK", cid db "SEDGA", cid db "GSR", cid db "JTAGG"] =
        (if portDirOf cell port == PortOut then TmgStartpoint else TmgEndpoint, 0)
    | cellType cell =: cid db "OSCG" =
        if port =: cid db "OSC" then (TmgGenClock, 0) else (TmgIgnore, 0)
    | cellType cell =: cid db "CLKDIVF" =
        if
            port =: cid db "CLKI"
            then (TmgClockInput, 0)
            else
                if port `anyOf` [cid db "RST", cid db "ALIGNWD"]
                    then (TmgEndpoint, 0)
                    else
                        if port =: cid db "CDIVX"
                            then (TmgGenClock, 0)
                            else (TmgIgnore, 0)
    | cellType cell =: cid db "DQSBUFM" =
        if
            port `anyOf` [cid db "READ0", cid db "READ1"]
            then (TmgRegisterInput, 1)
            else
                if port =: cid db "DATAVALID"
                    then (TmgRegisterOutput, 1)
                    else
                        if port `anyOf` [cid db "SCLK", cid db "ECLK", cid db "DQSI"]
                            then (TmgClockInput, 0)
                            else
                                if port `anyOf` [cid db "DQSR90", cid db "DQSW", cid db "DQSW270"]
                                    then (TmgGenClock, 0)
                                    else (if portDirOf cell port == PortOut then TmgStartpoint else TmgEndpoint, 0)
    | cellType cell =: cid db "DDRDLL" =
        if
            port =: cid db "CLK"
            then (TmgClockInput, 0)
            else (if portDirOf cell port == PortOut then TmgStartpoint else TmgEndpoint, 0)
    | cellType cell =: cid db "TRELLIS_ECLKBUF" =
        (if portDirOf cell port == PortOut then TmgCombOutput else TmgCombInput, 0)
    | cellType cell =: cid db "ECLKSYNCB" =
        if
            port =: cid db "STOP"
            then (TmgEndpoint, 0)
            else (if portDirOf cell port == PortOut then TmgCombOutput else TmgCombInput, 0)
    | cellType cell =: cid db "ECLKBRIDGECS" =
        if
            port =: cid db "SEL"
            then (TmgEndpoint, 0)
            else (if portDirOf cell port == PortOut then TmgCombOutput else TmgCombInput, 0)
    | otherwise = (TmgIgnore, 0)

{- | @getPortClockingInfo@. The payload-dependent cells (DP16KD,
MULT18X18D, TRELLIS_COMB/FF clock-inversion edges) return the default
record until the packer lands.
-}
getPortClockingInfoFor :: TimingDb -> CellInfo BelId WireId PipId -> IdString -> Int -> TimingClockingInfo
getPortClockingInfoFor db cell port _index = chosen
  where
    info =
        TimingClockingInfo
            { tciClockPort = cid db "CLK"
            , tciEdge = RisingEdge
            , tciSetup = dpFromDelay 0
            , tciHold = dpFromDelay 0
            , tciClockToQ = dqFromDelay 0
            }
    fromNS :: Double -> DelayT
    fromNS ns = fromIntegral (floor (ns * 1000) :: Int) -- ecp5 getDelayFromNS: ps = ns*1000
    toQ :: Double -> DelayQuad
    toQ ns = dqFromDelay (fromNS ns)
    pair :: Double -> DelayPair
    pair ns = dpFromDelay (fromNS ns)

    -- TRELLIS_COMB: WCK clocked ports use the SDPRAME timing cell; the
    -- WD port is renamed to WD0 in the DB
    combInfo
        | cellType cell =: cid db "TRELLIS_COMB" && port `anyOf` [cid db "WD", cid db "WAD0", cid db "WAD1", cid db "WAD2", cid db "WAD3", cid db "WRE"] =
            let p' = if port =: cid db "WD" then cid db "WD0" else port
                -- TODO(payload): COMB_RAM_WCKINV selects FALLING_EDGE
                (sh0, sh1) = fromMaybe (pair 0, pair 0) (getSetupholdFromTmgDb db (cid db "SDPRAME") (cid db "WCK") p')
             in info{tciClockPort = cid db "WCK", tciSetup = sh0, tciHold = sh1}
        | otherwise = info

    -- TRELLIS_FF: SLOGICB timing cell; DI -> DI0, Q -> Q0 renames
    ffInfo
        | cellType cell =: cid db "TRELLIS_FF" && port `anyOf` [cid db "DI", cid db "CE", cid db "LSR"] =
            let p' = if port =: cid db "DI" then cid db "DI0" else port
                -- TODO(payload): FF_CLKINV selects FALLING_EDGE
                (sh0, sh1) = fromMaybe (pair 0, pair 0) (getSetupholdFromTmgDb db (cid db "SLOGICB") (cid db "CLK") p')
             in info{tciSetup = sh0, tciHold = sh1}
        | cellType cell =: cid db "TRELLIS_FF" && port =: cid db "Q" =
            -- TODO(payload): FF_CLKINV selects FALLING_EDGE
            info{tciClockToQ = fromMaybe (toQ 0) (getDelayFromTmgDb db (cid db "SLOGICB") (cid db "CLK") (cid db "Q0"))}
        | otherwise = info

    -- DCUA: fixed delays from the C++ (ns -> delay units)
    dcuaInfo
        | cellType cell =: cid db "DCUA" =
            let prefix = T.take 9 (resolve db port)
                clk
                    | prefix == "CH0_FF_TX" = cid db "CH0_FF_TXI_CLK"
                    | prefix == "CH0_FF_RX" = cid db "CH0_FF_RXI_CLK"
                    | prefix == "CH1_FF_TX" = cid db "CH1_FF_TXI_CLK"
                    | prefix == "CH1_FF_RX" = cid db "CH1_FF_RXI_CLK"
                    | otherwise = cid db "CLK"
             in if portDirOf cell port == PortOut
                    then info{tciClockPort = clk, tciClockToQ = toQ 0.7}
                    else info{tciClockPort = clk, tciSetup = pair 1, tciHold = pair 0}
        | otherwise = info

    -- IOLOGIC: fixed delays (0.5ns clock-to-Q / 0.1ns setup)
    ioInfo
        | cellType cell `anyOf` [cid db "IOLOGIC", cid db "SIOLOGIC"] =
            if portDirOf cell port == PortOut
                then info{tciClockPort = cid db "CLK", tciClockToQ = toQ 0.5}
                else info{tciClockPort = cid db "CLK", tciSetup = pair 0.1, tciHold = pair 0}
        | otherwise = info

    -- DQSBUFM: SCLK clocked, fixed delays
    dqsInfo
        | cellType cell =: cid db "DQSBUFM" =
            if port =: cid db "DATAVALID"
                then info{tciClockPort = cid db "SCLK", tciClockToQ = toQ 0.2}
                else
                    if port `anyOf` [cid db "READ0", cid db "READ1"]
                        then info{tciClockPort = cid db "SCLK", tciSetup = pair 0.5, tciHold = pair (-0.4)}
                        else info
        | otherwise = info

    -- TODO(payload): DP16KD and MULT18X18D clocking info need
    -- ramInfo/multInfo timing ids.

    -- first branch wins, mirroring the C++ if/else chain
    chosen =
        if cellType cell =: cid db "TRELLIS_COMB" && port `anyOf` [cid db "WD", cid db "WAD0", cid db "WAD1", cid db "WAD2", cid db "WAD3", cid db "WRE"]
            then combInfo
            else
                if cellType cell =: cid db "TRELLIS_FF" && port `anyOf` [cid db "DI", cid db "CE", cid db "LSR", cid db "Q"]
                    then ffInfo
                    else
                        if cellType cell =: cid db "DCUA"
                            then dcuaInfo
                            else
                                if cellType cell `anyOf` [cid db "IOLOGIC", cid db "SIOLOGIC"]
                                    then ioInfo
                                    else
                                        if cellType cell =: cid db "DQSBUFM"
                                            then dqsInfo
                                            else info

-- helpers ----------------------------------------------------------------


-- | The last non-digit character of a name (the C++ iterates the port
-- name in reverse, skipping digits).
lastNonDigit :: Text -> Char
lastNonDigit t =
    case dropWhile isDigit (reverse (T.unpack t)) of
        c : _ -> c
        [] -> '\0'
