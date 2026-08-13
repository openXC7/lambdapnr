{-# LANGUAGE OverloadedStrings #-}

{- | The ECP5 packer — the Haskell mirror of @ecp5\/pack.cc@
(@Ecp5Packer@) plus the cell transforms of @ecp5\/cells.cc@ and the
global-clock promotion of @ecp5\/globals.cc@ (@insert_dcc@).

The C++ mutates @ctx->cells\/nets@ in place; we thread an immutable
'Design' through a 'Packer' state record, with the same pass order:
@prepack_checks -> print_logic_usage -> pack_io -> pack_dqsbuf ->
pack_iologic -> preplace_plls -> pack_eclk -> pack_ebr -> pack_dsps ->
pack_dcus -> pack_misc -> pack_constants -> pack_dram -> pack_carries
-> pack_luts -> pack_lut5xs -> pack_ffs -> generate_constraints ->
promote_globals -> fixupHierarchy@.

Determinism: the C++ @dict@ iterates in 'IdString' index order; our
'Data.Map' keys on the same indices, so @M.elems@ matches. The
auto-cell-name counter and all passes are seeded in the same order.
-}
module Lambdapnr.Arch.Ecp5.Pack (
    Packer,
    initialPacker,
    packDesign,
    designOf,
    printLogicUsage,
) where

import Control.Monad (foldM, when)
import Control.DeepSeq (deepseq)
import Data.Function ((&))
import Data.List (sortOn)
import System.IO (hPutStrLn, stderr)
import Data.Bits (complement, (.&.), (.|.), shiftL, shiftR)
import Data.Int (Int64)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Printf (printf)

import Lambdapnr.Arch.Ecp5 hiding (locX, locY, locZ)
import Lambdapnr.Arch.Ecp5.ArchCellInfo
import Lambdapnr.Arch.Ecp5.CellTiming (TimingDb (..))
import Lambdapnr.Arch.Ecp5.Chipdb
import Lambdapnr.Arch.Ecp5.Types
import Lambdapnr.Kernel.Arch (Loc (..), checkBelAvail, getBelByLocation, getBelByName, getBelLocation, getBelName, getBelPins, getBelPinType, getBelPinWire, getBels, getBelType)
import Lambdapnr.Kernel.Delay (ClockConstraint (..), DelayPair (..))
import Lambdapnr.Kernel.IdString (IdString (..), emptyId, idToText)
import Lambdapnr.Kernel.Netlist
import Lambdapnr.Kernel.IdString (intern)
import Lambdapnr.Kernel.Checksum (checksum)
import Lambdapnr.Kernel.Netlist
import Lambdapnr.Kernel.Property (Property (..), propAsInt64, propFromInt, propIsString, propAsString)
import GHC.Stack (callStack, prettyCallStack)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (evaluate)
import qualified Control.Exception as E

-- ---------------------------------------------------------------------------
-- Logging (mirrors log_info/log_warning; goes to stderr)
-- ---------------------------------------------------------------------------

info :: String -> IO ()
info = hPutStrLn stderr

-- ---------------------------------------------------------------------------
-- The packer state
-- ---------------------------------------------------------------------------

data Packer = Packer
    { pkE :: !Ecp5
    , pkDesign :: !(Design BelId WireId PipId)
    , pkPacked :: ![IdString]
    -- ^ packed-cell list in insertion order (the C++ pool iterates
    -- this in REVERSE on flush)
    -- ^ cells to erase at flush (@packed_cells@)
    , pkNew :: ![CellInfo BelId WireId PipId]
    -- ^ cells to add at flush (@new_cells@)
    , pkAutoIdx :: !Int
    , pkDqsbufDqsg :: !(M.Map IdString (Bool, Int))
    , pkEclks :: !(M.Map (Int, Int) EdgeClockInfo)
    , pkBridgeSideHint :: !(M.Map IdString Int)
    , pkClkConstr :: !(M.Map IdString ClockConstraint)
    , pkUserConstrained :: !(S.Set IdString)
    , pkGsrclkWire :: !(Maybe WireId)
    , pkVerbose :: !Bool
    , pkSettings :: !(M.Map IdString Property)
    }

data EdgeClockInfo = EdgeClockInfo
    { ecBuffer :: !(Maybe IdString)
    , ecUnbuf :: !(Maybe IdString)
    , ecBuf :: !(Maybe IdString)
    }
    deriving (Eq, Show)

initialPacker :: Ecp5 -> Design BelId WireId PipId -> Bool -> Packer
initialPacker e d verbose =
    initialPackerWithSettings e d verbose M.empty

initialPackerWithSettings :: Ecp5 -> Design BelId WireId PipId -> Bool -> M.Map IdString Property -> Packer
initialPackerWithSettings e d verbose settings =
    Packer
        { pkE = e
        , pkDesign = d
        , pkPacked = []
        , pkNew = []
        , pkAutoIdx = 0
        , pkDqsbufDqsg = M.empty
        , pkEclks = M.empty
        , pkBridgeSideHint = M.empty
        , pkClkConstr = M.empty
        , pkUserConstrained = S.empty
        , pkGsrclkWire = Nothing
        , pkVerbose = verbose
        , pkSettings = settings
        }

designOf :: Packer -> Design BelId WireId PipId
designOf = pkDesign

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Resolve a constid name.
cid :: Packer -> T.Text -> IdString
cid pk t = fromMaybe emptyId (M.lookup t (tdConstIdByName (ecp5TimingDb (pkE pk))))

-- | Intern a suffixed name (@base <> suffix@ as text).
named :: Packer -> IdString -> T.Text -> IdString
named pk base suffix =
    let txt = idStr pk base <> suffix
     in T.length txt `seq` unsafePerformIO (intern (ecp5IdTable (pkE pk)) txt)
{-# NOINLINE named #-}

-- | Intern a text as a fresh id (for @$...@-prefixed runtime names that
-- are not chipdb constids).
internT :: Packer -> T.Text -> IdString
internT pk t =
    -- force the text before interning: the text may contain idToText
    -- thunks (readIORef on the same table), which would deadlock inside
    -- intern's atomicModifyIORef'
    T.length t `seq` unsafePerformIO (intern (ecp5IdTable (pkE pk)) t)
{-# NOINLINE internT #-}

idStr :: Packer -> IdString -> T.Text
idStr pk = idToText (ecp5IdTable (pkE pk))

cellOf :: Packer -> IdString -> CellInfo BelId WireId PipId
cellOf pk n =
    case lookupCell n (pkDesign pk) of
        Just c -> c
        Nothing ->
            let sample = take 8 (M.keys (designCells (pkDesign pk)))
                nm = idToText (ecp5IdTable (pkE pk)) n
             in error ("packer: missing cell " ++ show n ++ " (\"" ++ T.unpack nm ++ "\", design has " ++ show (M.size (designCells (pkDesign pk))) ++ " cells, first: " ++ show sample ++ ")\n" ++ prettyCallStack callStack)

trace :: String -> a -> a
trace msg x = unsafePerformIO (hPutStrLn stderr msg >> pure x)

netOf :: Packer -> IdString -> NetInfo BelId WireId PipId
netOf pk n = fromMaybe (error ("packer: missing net " ++ show n)) (lookupNet n (pkDesign pk))

-- | @ci->getPort@.
getPort :: CellInfo bel wire pip -> IdString -> Maybe IdString
getPort ci p = portNet =<< M.lookup p (cellPorts ci)

-- | @str_or_default@.
strOrDef :: M.Map IdString Property -> IdString -> String -> String
strOrDef m k def =
    case M.lookup k m of
        Nothing -> def
        Just p -> if propIsString p then T.unpack (propAsString p) else show (propAsInt64 p)

-- | @int_or_default@.
intOrDef :: M.Map IdString Property -> IdString -> Int -> Int
intOrDef m k def =
    case M.lookup k m of
        Nothing -> def
        Just p -> if propIsString p then readInt (T.unpack (propAsString p)) else fromIntegral (propAsInt64 p)
  where
    readInt t = case reads t of
        [(i, "")] -> i
        _ -> def

-- | @bool_or_default@.
boolOrDef :: M.Map IdString Property -> IdString -> Bool -> Bool
boolOrDef m k def =
    case M.lookup k m of
        Nothing -> def
        Just p -> if propIsString p then T.unpack (propAsString p) == "1" else propAsInt64 p /= 0

-- | Set a param on a design cell.
setP :: IdString -> IdString -> Property -> Packer -> Packer
setP cell key value pk =
    pk{pkDesign = setCellParam cell key value (pkDesign pk)}

-- | Cell type predicates (cells.h).
isLut :: Packer -> CellInfo BelId WireId PipId -> Bool
isLut pk ci = cellType ci == cid pk "LUT4"

isFf :: Packer -> CellInfo BelId WireId PipId -> Bool
isFf pk ci = cellType ci == cid pk "TRELLIS_FF"

isCarry :: Packer -> CellInfo BelId WireId PipId -> Bool
isCarry pk ci = cellType ci == cid pk "CCU2C"

isTrellisIo :: Packer -> CellInfo BelId WireId PipId -> Bool
isTrellisIo pk ci = cellType ci == cid pk "TRELLIS_IO"

isDpram :: Packer -> CellInfo BelId WireId PipId -> Bool
isDpram pk ci = cellType ci == cid pk "TRELLIS_DPR16X4"

isPfumx :: Packer -> CellInfo BelId WireId PipId -> Bool
isPfumx pk ci = cellType ci == cid pk "PFUMX"

isL6mux :: Packer -> CellInfo BelId WireId PipId -> Bool
isL6mux pk ci = cellType ci == cid pk "L6MUX21"

isNextpnrIob :: Packer -> CellInfo BelId WireId PipId -> Bool
isNextpnrIob pk ci =
    let t = cellType ci
     in t == internT pk "$nextpnr_ibuf" || t == internT pk "$nextpnr_obuf" || t == internT pk "$nextpnr_iobuf"

isIologicInputCell :: Packer -> CellInfo BelId WireId PipId -> Bool
isIologicInputCell pk ci =
    let t = cellType ci
     in t `elem` [cid pk "IDDRX1F", cid pk "IDDRX2F", cid pk "IDDR71B", cid pk "IDDRX2DQA"]
            || ( t == cid pk "TRELLIS_FF"
                    && boolOrDef (cellAttrs ci) (cid pk "syn_useioff") False
                    && strOrDef (cellAttrs ci) (cid pk "ioff_dir") "" /= "output"
               )

isIologicOutputCell :: Packer -> CellInfo BelId WireId PipId -> Bool
isIologicOutputCell pk ci =
    let t = cellType ci
     in t `elem` [cid pk "ODDRX1F", cid pk "ODDRX2F", cid pk "ODDR71B", cid pk "ODDRX2DQA", cid pk "ODDRX2DQSB", cid pk "OSHX2A"]
            || ( t == cid pk "TRELLIS_FF"
                    && boolOrDef (cellAttrs ci) (cid pk "syn_useioff") False
                    && strOrDef (cellAttrs ci) (cid pk "ioff_dir") "" /= "input"
               )

-- | @net_driven_by@: the cell driving a net through the given port.
netDrivenBy :: Packer -> Maybe IdString -> (CellInfo BelId WireId PipId -> Bool) -> IdString -> Maybe IdString
netDrivenBy pk net pred port =
    case net of
        Nothing -> Nothing
        Just netName ->
            let ni = netOf pk netName
             in case prCell (netDriver ni) of
                    Just c -> if pred (cellOf pk c) && prPort (netDriver ni) == port then Just c else Nothing
                    Nothing -> Nothing

-- | @net_only_drives@: find the single user of a net matching the
-- predicate/port.
netOnlyDrives :: Packer -> Maybe IdString -> (CellInfo BelId WireId PipId -> Bool) -> IdString -> Bool -> Maybe IdString -> Maybe IdString
netOnlyDrives pk net pred port exclusive exclude =
    case net of
        Nothing -> Nothing
        Just netName ->
            let ni = netOf pk netName
                users = activeUsers (netUsers ni)
                n = length users
                okCount =
                    if exclusive
                        then
                            case exclude of
                                Nothing -> n == 1
                                Just ex -> n <= 2 && (n == 1 || any (\u -> prCell u == Just ex) users)
                        else True
             in if not okCount
                    then Nothing
                    else
                        case [u | u <- users, prCell u /= exclude, pred (cellOf pk (fromMaybe (error "null user") (prCell u))), prPort u == port] of
                            (u : _) -> prCell u
                            [] -> Nothing

-- | @create_ecp5_cell@ (cells.cc).
createCell :: Packer -> IdString -> Maybe IdString -> (Packer, CellInfo BelId WireId PipId)
createCell pk typ mname =
    let autoName = "$nextpnr_" <> idStr pk typ <> "_" <> T.pack (show (pkAutoIdx pk))
        name = maybe autoName id (idStr pk <$> mname) -- name text -> needs interning: use the given IdString or intern auto
        -- the C++ interns; we keep the auto name as an id via the id table
        nameId =
            case mname of
                Just n -> n
                Nothing -> internT pk autoName
        pk' = pk{pkAutoIdx = if mname == Nothing then pkAutoIdx pk + 1 else pkAutoIdx pk}
        (cell0, ports) = cellTemplate pk typ
        cell' = cell0{cellName = nameId, cellPorts = ports}
        -- the C++ inserts new cells into ctx->cells immediately (they
        -- are unique_ptr-moved right away), so cell transforms can
        -- operate on them; mirror that here
        pk'' = pk'{pkDesign = addCell nameId cell' (pkDesign pk')}
        _ = pk'' `seq` ()
     in (pk'', cell')

-- | The port/param template for a new cell (cells.cc).
cellTemplate :: Packer -> IdString -> (CellInfo BelId WireId PipId, M.Map IdString PortInfo)
cellTemplate pk typ =
    let e = pkE pk
        tp = idStr pk typ
        mkIn p = (cid pk p, PortInfo (cid pk p) Nothing PortIn 0)
        mkOut p = (cid pk p, PortInfo (cid pk p) Nothing PortOut 0)
        mkInout p = (cid pk p, PortInfo (cid pk p) Nothing PortInout 0)
        portsOf = M.fromList
        cell0 = CellInfo emptyId typ emptyId M.empty M.empty M.empty Nothing StrengthNone emptyId 0 0 0 False []
        paramsOf ps = M.fromList [(cid pk k, v) | (k, v) <- ps]
        attrsOf as = M.fromList [(cid pk k, v) | (k, v) <- as]
     in if tp == "TRELLIS_COMB"
            then
                ( cell0
                    { cellParams =
                        paramsOf
                            [ ("MODE", PropStr "LOGIC")
                            , ("INITVAL", propFromInt 0 16)
                            , ("CCU2_INJECT1", PropStr "NO")
                            , ("WREMUX", PropStr "WRE")
                            ]
                    , cellPorts =
                        portsOf
                            ( map mkIn ["A", "B", "C", "D", "M", "F1", "FCI", "FXA", "FXB", "DI0", "DI1", "WD", "WAD0", "WAD1", "WAD2", "WAD3", "WRE", "WCK"]
                                ++ map mkOut ["F", "FCO", "OFX"]
                            )
                    }
                , portsOf
                    ( map mkIn ["A", "B", "C", "D", "M", "F1", "FCI", "FXA", "FXB", "DI0", "DI1", "WD", "WAD0", "WAD1", "WAD2", "WAD3", "WRE", "WCK"]
                        ++ map mkOut ["F", "FCO", "OFX"]
                    )
                )
            else
                if tp == "TRELLIS_RAMW"
                    then
                        ( cell0
                            { cellPorts =
                                portsOf
                                    ( map mkIn ["A0", "B0", "C0", "D0", "A1", "B1", "C1", "D1"]
                                        ++ map mkOut ["WDO0", "WDO1", "WDO2", "WDO3", "WADO0", "WADO1", "WADO2", "WADO3"]
                                    )
                            }
                        , portsOf
                            ( map mkIn ["A0", "B0", "C0", "D0", "A1", "B1", "C1", "D1"]
                                ++ map mkOut ["WDO0", "WDO1", "WDO2", "WDO3", "WADO0", "WADO1", "WADO2", "WADO3"]
                            )
                        )
                    else
                        if tp == "TRELLIS_IO"
                            then
                                ( cell0
                                    { cellParams =
                                        paramsOf
                                            [ ("DIR", PropStr "INPUT")
                                            , ("DATAMUX_ODDR", PropStr "PADDO")
                                            , ("DATAMUX_MDDR", PropStr "PADDO")
                                            ]
                                    , cellAttrs = attrsOf [("IO_TYPE", PropStr "LVCMOS33")]
                                    , cellPorts =
                                        portsOf
                                            ( mkInout "B" : map mkIn ["I", "T", "IOLDO", "IOLTO"] ++ [mkOut "O"]
                                            )
                                    }
                                , portsOf (mkInout "B" : map mkIn ["I", "T", "IOLDO", "IOLTO"] ++ [mkOut "O"])
                                )
                            else
                                if tp == "LUT4"
                                    then
                                        ( cell0
                                            { cellParams = paramsOf [("INIT", propFromInt 0 16)]
                                            , cellPorts = portsOf (map mkIn ["A", "B", "C", "D"] ++ [mkOut "Z"])
                                            }
                                        , portsOf (map mkIn ["A", "B", "C", "D"] ++ [mkOut "Z"])
                                        )
                                    else
                                        if tp == "CCU2C"
                                            then
                                                ( cell0
                                                    { cellParams =
                                                        paramsOf
                                                            [ ("INIT0", propFromInt 0 16)
                                                            , ("INIT1", propFromInt 0 16)
                                                            , ("INJECT1_0", PropStr "YES")
                                                            , ("INJECT1_1", PropStr "YES")
                                                            ]
                                                    , cellPorts =
                                                        portsOf
                                                            ( mkIn "CIN"
                                                                : map mkIn ["A0", "B0", "C0", "D0", "A1", "B1", "C1", "D1"]
                                                                    ++ map mkOut ["S0", "S1", "COUT"]
                                                            )
                                                    }
                                                , portsOf
                                                    ( mkIn "CIN"
                                                        : map mkIn ["A0", "B0", "C0", "D0", "A1", "B1", "C1", "D1"]
                                                            ++ map mkOut ["S0", "S1", "COUT"]
                                                    )
                                                )
                                            else
                                                if tp == "DCCA"
                                                    then
                                                        ( cell0
                                                            { cellPorts = portsOf [mkIn "CLKI", mkOut "CLKO", mkIn "CE"] }
                                                        , portsOf [mkIn "CLKI", mkOut "CLKO", mkIn "CE"]
                                                        )
                                                    else
                                                        if tp `elem` ["IOLOGIC", "SIOLOGIC"]
                                                            then
                                                                let belPorts =
                                                                        case [b | b <- getBels e, idStr pk (getBelType e b) == tp] of
                                                                            (b : _) ->
                                                                                M.fromList
                                                                                    [ (p, PortInfo p Nothing (getBelPinType e b p) 0)
                                                                                    | p <- getBelPins e b
                                                                                    ]
                                                                            [] -> M.empty
                                                                    params =
                                                                        paramsOf
                                                                            [ ("MODE", PropStr "NONE")
                                                                            , ("GSR", PropStr "DISABLED")
                                                                            , ("CLKIMUX", PropStr "CLK")
                                                                            , ("CLKOMUX", PropStr "CLK")
                                                                            , ("LSRIMUX", PropStr "0")
                                                                            , ("LSROMUX", PropStr "0")
                                                                            , ("LSRMUX", PropStr "LSR")
                                                                            , ("DELAY.OUTDEL", PropStr "DISABLED")
                                                                            , ("DELAY.DEL_VALUE", propFromInt 0 7)
                                                                            , ("DELAY.WAIT_FOR_EDGE", PropStr "DISABLED")
                                                                            ]
                                                                    params' =
                                                                        if tp == "IOLOGIC"
                                                                            then
                                                                                M.union
                                                                                    ( paramsOf
                                                                                        [ ("IDDRXN.MODE", PropStr "NONE")
                                                                                        , ("ODDRXN.MODE", PropStr "NONE")
                                                                                        , ("MIDDRX.MODE", PropStr "NONE")
                                                                                        , ("MODDRX.MODE", PropStr "NONE")
                                                                                        , ("MTDDRX.MODE", PropStr "NONE")
                                                                                        , ("IOLTOMUX", PropStr "NONE")
                                                                                        , ("MTDDRX.DQSW_INVERT", PropStr "DISABLED")
                                                                                        , ("MTDDRX.REGSET", PropStr "RESET")
                                                                                        , ("MIDDRX_MODDRX.WRCLKMUX", PropStr "NONE")
                                                                                        ]
                                                                                    )
                                                                                    params
                                                                            else params
                                                                 in (cell0{cellParams = params', cellPorts = belPorts}, belPorts)
                                                            else
                                                                if tp == "TRELLIS_ECLKBUF"
                                                                    then
                                                                        ( cell0
                                                                            { cellPorts = portsOf [mkIn "ECLKI", mkOut "ECLKO"] }
                                                                        , portsOf [mkIn "ECLKI", mkOut "ECLKO"]
                                                                        )
                                                                    else error ("unable to create ECP5 cell of type " ++ T.unpack tp)

-- | Insert a new cell into the design at flush time.
flushCells :: Packer -> Packer
flushCells pk =
    let d0 = pkDesign pk
        -- C++ semantics: new cells live in new_cells during the pass and
        -- are inserted into ctx->cells only AFTER the packed cells are
        -- erased. Since createCell adds them to the design immediately,
        -- strip them from the order first so the swap-erases pull the
        -- correct back elements, then re-append them in pkNew order.
        newSet = S.fromList [cellName c | c <- pkNew pk]
        d0' = d0{designCellOrder = filter (\n -> not (S.member n newSet)) (designCellOrder d0)}
        -- the packed pool iterates in REVERSE insertion order on erase
        d1 = foldl (\d c -> deleteCellSwap c d) d0' (reverse (pkPacked pk))
        d2 = foldl (\d c -> moveCellEnd (cellName c) d) d1 (pkNew pk)
     in pk{pkDesign = d2, pkPacked = [], pkNew = []}
  where
    moveCellEnd n d = d{designCellOrder = filter (/= n) (designCellOrder d) ++ [n]}

-- | Add a new cell to the pending list.
addNew :: CellInfo BelId WireId PipId -> Packer -> Packer
addNew c pk = pk{pkNew = pkNew pk ++ [c]}

-- | @ctx->createNet@.
createNet :: Packer -> IdString -> NetInfo BelId WireId PipId
createNet pk name =
    NetInfo name emptyId (PortRef Nothing (cid pk "PADDO")) V.empty [] M.empty M.empty emptyId

-- | A fresh net with the default (undriven) driver ref.
freshNetPk :: Packer -> IdString -> NetInfo BelId WireId PipId
freshNetPk _ name = NetInfo name emptyId (PortRef Nothing emptyId) V.empty [] M.empty M.empty emptyId

-- | Move a port (rename in place + detach + attach elsewhere) — see
-- Netlist.moveCellPort.
movePort :: Packer -> IdString -> IdString -> IdString -> IdString -> Packer
movePort pk srcCell srcPort dstCell dstPort =
    pk{pkDesign = moveCellPort srcCell srcPort dstCell dstPort (pkDesign pk)}

-- | Connect two ports of (possibly different) cells to the same net.
connectPorts :: IdString -> IdString -> IdString -> IdString -> Packer -> Packer
connectPorts a pa b pb pk =
    case getPort (cellOf pk a) pa of
        Just existing -> pk{pkDesign = connectPort b pb existing (pkDesign pk)}
        Nothing ->
            let connName = internT pk (idStr pk a <> "$conn$" <> idStr pk pa)
                d1 = addNet connName (createNet pk connName) (pkDesign pk)
                d2 = connectPort a pa connName d1
                d3 = connectPort b pb connName d2
             in pk{pkDesign = d3}

-- ---------------------------------------------------------------------------
-- print_logic_usage
-- ---------------------------------------------------------------------------

printLogicUsage :: Ecp5 -> Design BelId WireId PipId -> IO ()
printLogicUsage e d = do
    let pk0 = initialPacker e d False
        totalLuts = length [() | b <- getBels e, idStr pk0 (getBelType e b) == "TRELLIS_COMB"]
        totalRamluts = length [() | b <- getBels e, idStr pk0 (getBelType e b) == "TRELLIS_COMB", locZ (getBelLocation e b) <= 3]
        totalFfs = length [() | b <- getBels e, idStr pk0 (getBelType e b) == "TRELLIS_FF"]
        totalRamwluts = 2 * length [() | b <- getBels e, idStr pk0 (getBelType e b) == "TRELLIS_RAMW"]
        usedLgluts = length [() | ci <- M.elems (designCells d), isLut pk0 ci]
        usedCyluts = 2 * length [() | ci <- M.elems (designCells d), isCarry pk0 ci]
        usedRamluts = 4 * length [() | ci <- M.elems (designCells d), isDpram pk0 ci]
        usedRamwluts = 2 * length [() | ci <- M.elems (designCells d), isDpram pk0 ci]
        usedFfs = length [() | ci <- M.elems (designCells d), isFf pk0 ci]
        usedLuts = usedLgluts + usedCyluts + usedRamluts + usedRamwluts
        pc u t = if t == 0 then 0 else 100 * u `div` t
    info "Logic utilisation before packing:"
    info (printf "    Total LUT4s:     %5d/%5d %5d%%" usedLuts totalLuts (pc usedLuts totalLuts))
    info (printf "        logic LUTs:  %5d/%5d %5d%%" usedLgluts totalLuts (pc usedLgluts totalLuts))
    info (printf "        carry LUTs:  %5d/%5d %5d%%" usedCyluts totalLuts (pc usedCyluts totalLuts))
    info (printf "          RAM LUTs:  %5d/%5d %5d%%" usedRamluts totalRamluts (pc usedRamluts totalRamluts))
    info (printf "         RAMW LUTs:  %5d/%5d %5d%%" usedRamwluts totalRamwluts (pc usedRamwluts totalRamwluts))
    info ""
    info (printf "     Total DFFs:     %5d/%5d %5d%%" usedFfs totalFfs (pc usedFfs totalFfs))
    info ""
  where
    locZ (Loc _ _ z) = z
    idStr' pk = idToText (ecp5IdTable (pkE pk))
    idStr = idStr'

-- ---------------------------------------------------------------------------
-- pack_io
-- ---------------------------------------------------------------------------

packIo :: Packer -> IO Packer
packIo pk = do
    info "Packing IOs.."
    evaluate (length (designCells (pkDesign pk)))
    let cells = cellsIter (pkDesign pk)
    pk' <- foldM packOne pk cells
    pure (flushCells pk')
  where
    -- | @nxio_to_tr@ (cells.cc): move the buffer ports onto the
    -- TRELLIS_IO, set DIR, and rename conflicting nets.
    nxioToTr :: Packer -> CellInfo BelId WireId PipId -> IdString -> Packer
    nxioToTr pk nxio trioName =
        let t = cellType nxio
            pk1
                | t == internT pk "$nextpnr_ibuf" =
                    setP trioName (cid pk "DIR") (PropStr "INPUT") pk
                        & \p -> movePort p (cellName nxio) (cid pk "O") trioName (cid pk "O")
                | t == internT pk "$nextpnr_obuf" =
                    setP trioName (cid pk "DIR") (PropStr "OUTPUT") pk
                        & \p -> movePort p (cellName nxio) (cid pk "I") trioName (cid pk "I")
                | otherwise =
                    let dir =
                            case getPort nxio (cid pk "I") of
                                Just n | prCell (netDriver (netOf pk n)) /= Nothing -> "BIDIR"
                                _ -> "INPUT"
                        p0 = setP trioName (cid pk "DIR") (PropStr dir) pk
                        p1 = movePort p0 (cellName nxio) (cid pk "I") trioName (cid pk "I")
                     in movePort p1 (cellName nxio) (cid pk "O") trioName (cid pk "O")
            -- rename I/O nets that clash with the nxio cell name
            pk2 =
                case getPort (cellOf pk1 trioName) (cid pk "I") of
                    Just donet | donet == cellName nxio ->
                        let nn = internT pk1 (idStr pk1 donet <> "$TRELLIS_IO_OUT")
                         in pk1{pkDesign = renameNet donet nn (pkDesign pk1)}
                    _ -> pk1
            pk3 =
                case getPort (cellOf pk2 trioName) (cid pk "O") of
                    Just dinet | dinet == cellName nxio ->
                        let nn = internT pk2 (idStr pk2 dinet <> "$TRELLIS_IO_IN")
                         in pk2{pkDesign = renameNet dinet nn (pkDesign pk2)}
                    _ -> pk2
            -- any net still named after the nxio cell gets $rename$i
            pk4
                | M.member (cellName nxio) (designNets (pkDesign pk3)) =
                    let go i p =
                            let nn = internT p (idStr p (cellName nxio) <> "$rename$" <> T.pack (show i))
                             in if M.member nn (designNets (pkDesign p))
                                    then go (i + 1) p
                                    else p{pkDesign = renameNet (cellName nxio) nn (pkDesign p)}
                     in go 0 pk3
                | otherwise = pk3
            -- create a new top port net for accurate IO timing analysis
            -- (0.10 nxio_to_tr tail): the trio's B pad pin connects to a
            -- fresh net named after the port
            pk5
                | M.member (cellName nxio) (designPorts (pkDesign pk4)) =
                    let tn = cellName nxio
                        d0 = addNet tn (freshNetPk pk tn) (pkDesign pk4)
                        d1 = connectPort trioName (cid pk "B") tn d0
                        d2 = d1{designPorts = M.insert tn tn (designPorts d1)}
                     in pk4{pkDesign = d2}
                | otherwise = pk4
         in pk5

    -- disconnect every port of a cell (the pack_io tail)
    disconnectAll :: Packer -> IdString -> Packer
    disconnectAll pk cell =
        let ports = M.keys (cellPorts (cellOf pk cell))
         in pk{pkDesign = foldl (\d p -> disconnectPort cell p d) (pkDesign pk) ports}

    packOne pk ci
        | not (isNextpnrIob pk ci) = pure pk
        | otherwise =
            -- the C++ type-dispatch order interns obuf/iobuf only when the
            -- ibuf/iobuf comparison misses (short-circuit interning order
            -- matters for the checksum)
            if cellType ci == internT pk "$nextpnr_ibuf" || cellType ci == internT pk "$nextpnr_iobuf"
                then packBuf pk ci False
                else
                    if cellType ci == internT pk "$nextpnr_obuf"
                        then packBuf pk ci True
                        else pure pk
      where
        packBuf pk ci isObuf =
            let ionet = if isObuf then getPort ci (cid pk "I") else getPort ci (cid pk "O")
                trio = netOnlyDrives pk ionet (isTrellisIo pk) (cid pk "B") True (Just (cellName ci))
             in if boolOrDef (pkSettings pk) (internT pk "arch.ooc") False
                    then pure (disconnectAll pk (cellName ci))
                    else
                        case trio of
                            Just t -> do
                                info (printf "%s feeds TRELLIS_IO %s, removing %s %s." (T.unpack (idStr pk (cellName ci))) (T.unpack (idStr pk t)) (T.unpack (idStr pk (cellType ci))) (T.unpack (idStr pk (cellName ci))))
                                -- fanout/clkconstr checks are no-ops until
                                -- the timing model lands
                                pure (finish pk ci (Just t))
                            Nothing ->
                                case drivesTopPort pk ionet of
                                    Just tp -> do
                                        info (printf "%s feeds %s %s.%s, removing %s %s." (T.unpack (idStr pk (cellName ci))) (T.unpack (idStr pk (tpCellType pk tp))) (T.unpack (idStr pk (tpCell pk tp))) (T.unpack (idStr pk (tpPort pk tp))) (T.unpack (idStr pk (cellType ci))) (T.unpack (idStr pk (cellName ci))))
                                        let d1 =
                                                case ionet of
                                                    Just n -> (pkDesign pk){designNets = M.delete n (designNets (pkDesign pk))}
                                                    Nothing -> pkDesign pk
                                            d2 =
                                                case ionet of
                                                    Just _ -> clearPortNet (tpCell pk tp) (tpPort pk tp) d1
                                                    Nothing -> d1
                                            d3 =
                                                case ionet of
                                                    Just _ -> clearPortNet (cellName ci) (if isObuf then cid pk "I" else cid pk "O") d2
                                                    Nothing -> d2
                                            d4 =
                                                if cellType ci == internT pk "$nextpnr_iobuf"
                                                    then case getPort ci (cid pk "I") of
                                                        Just n2 -> clearPortNet (cellName ci) (cid pk "I") (d3{designNets = M.delete n2 (designNets d3)})
                                                        Nothing -> d3
                                                    else d3
                                         in pure (finish (pk{pkDesign = d4}) ci Nothing)
                                    Nothing -> do
                                        let (pk2, trNew) = createCell pk (cid pk "TRELLIS_IO") (Just (named pk (cellName ci) "$tr_io"))
                                            pk2b = addNew trNew pk2
                                            trName = named pk (cellName ci) "$tr_io"
                                            pk3 = nxioToTr pk2b ci trName
                                         in pure (finish pk3 ci (Just trName))

        -- the common tail: disconnect the buffer ports, mark it packed,
        -- copy its attrs to the trio and resolve LOC -> BEL
        finish pk ci mtrio =
            let pk1 = disconnectAll pk (cellName ci)
                pk2 = pk1{pkPacked = pkPacked pk1 ++ [cellName ci]}
             in case mtrio of
                    Nothing -> pk2
                    Just trName ->
                        let ci0 = cellOf pk (cellName ci)
                            pk3 = foldl (\p (k, v) -> setAttr trName k v p) pk2 (M.toList (cellAttrs ci0))
                            pk4 =
                                case M.lookup (cid pk "LOC") (cellAttrs ci0) of
                                    Just locp ->
                                        case getPackagePinBel (pkE pk) (propAsString locp) of
                                            Just b ->
                                                let belStr = T.intercalate "/" (map (idStr pk) (getBelName (pkE pk) b))
                                                 in setAttr trName (cid pk "BEL") (PropStr belStr) pk3
                                            Nothing -> error ("IO pin '" ++ T.unpack (propAsString locp) ++ "' does not exist for package")
                                    Nothing -> pk3
                         in pk4

        setAttr cell key value pk = pk{pkDesign = setCellAttr cell key value (pkDesign pk)}

        clearPortNet cell port d =
            case M.lookup cell (designCells d) of
                Nothing -> d
                Just ci -> d{designCells = M.insert cell (ci{cellPorts = M.adjust (\p -> p{portNet = Nothing}) port (cellPorts ci)}) (designCells d)}

    drivesTopPort pk net =
        case net of
            Nothing -> Nothing
            Just netName ->
                let ni = netOf pk netName
                    users = activeUsers (netUsers ni)
                    topUser =
                        case [u | u <- users, isTopPort pk (prCell u)] of
                            (u : _) -> Just u
                            [] ->
                                case prCell (netDriver ni) of
                                    Just c -> if isTopPort pk (Just c) then Just (netDriver ni) else Nothing
                                    Nothing -> Nothing
                 in case topUser of
                        Just u -> if length users > 1 || (isJust (prCell (netDriver ni)) && isTopPort pk (prCell (netDriver ni)) && length users > 0 && length users > 1)
                            then Nothing
                            else Just (fromMaybe (error "null") (prCell u), prPort u)
                        Nothing -> Nothing
    isTopPort pk mcell =
        case mcell of
            Nothing -> False
            Just c ->
                let ci = cellOf pk c
                    t = cellType ci
                    p = cellPorts ci
                 in if t == cid pk "DCUA"
                        then any (\pname -> idStr pk pname `elem` ["CH0_HDINP", "CH0_HDINN", "CH0_HDOUTP", "CH0_HDOUTN", "CH1_HDINP", "CH1_HDINN", "CH1_HDOUTP", "CH1_HDOUTN"]) (M.keys p)
                        else if t == cid pk "EXTREFB"
                            then any (\pname -> idStr pk pname `elem` ["REFCLKP", "REFCLKN"]) (M.keys p)
                            else False
    tpCell pk (c, _) = c
    tpPort pk (_, p) = p

tpCellType pk (c, _) = cellType (cellOf pk c)

-- ---------------------------------------------------------------------------
-- make_carry_feed_in / make_carry_feed_out / split_carry_chain / pack_carries
-- ---------------------------------------------------------------------------

data CellChain = CellChain { chCells :: [IdString] }

-- | @find_chains@ (chain_utils.h).
findChains :: Packer -> (CellInfo BelId WireId PipId -> Bool) -> (CellInfo BelId WireId PipId -> Maybe IdString) -> (CellInfo BelId WireId PipId -> Maybe IdString) -> Int -> [CellChain]
findChains pk pred getPrev getNext minLen =
    go (cellsIter (pkDesign pk)) S.empty []
  where
    go [] _ acc = acc
    go (ci : rest) chained acc
        | S.member (cellName ci) chained = go rest chained acc
        | not (pred ci) = go rest chained acc
        | otherwise =
            let start = walkPrev (cellName ci)
                (chainCells, chained') = walkNext start chained []
                n = length chainCells
             in if n >= minLen
                    then go rest chained' (acc ++ [CellChain (reverse chainCells)])
                    else go rest chained' acc
      where
        walkPrev c =
            case getPrev (cellOf pk c) of
                Just p
                    | pred (cellOf pk p) -> walkPrev p
                _ -> c
        -- the C++ walks through ALREADY-chained cells without adding
        -- them (the chain stops growing at another chain's territory)
        walkNext c chained accCells =
            let accCells' = if S.member c chained then accCells else c : accCells
                chained' = S.insert c chained
             in case getNext (cellOf pk c) of
                    Just n
                        | pred (cellOf pk n) -> walkNext n chained' accCells'
                    _ -> (accCells', chained')

makeCarryFeedIn :: Packer -> IdString -> IdString -> (Packer, IdString)
makeCarryFeedIn pk carry chainIn =
    let (pk1, feedin) = createCell pk (cid pk "CCU2C") Nothing
        pk2 = setP (cellName feedin) (cid pk "INIT0") (propFromInt 10 16) pk1
        pk3 = setP (cellName feedin) (cid pk "INIT1") (propFromInt 65535 16) pk2
        pk4 = setP (cellName feedin) (cid pk "INJECT1_0") (PropStr "NO") pk3
        pk5 = setP (cellName feedin) (cid pk "INJECT1_1") (PropStr "YES") pk4
        -- remove chainIn from carry users
        pk6 = pk5{pkDesign = removeNetUser carry chainIn (cid pk "CIN") (pkDesign pk5)}
        pk7 = pk6{pkDesign = connectPort (cellName feedin) (cid pk "A0") carry (pkDesign pk6)}
        newCarryName = named pk (cellName feedin) "$COUT"
        newCarry = createNet pk7 newCarryName
        pk8 = pk7{pkDesign = addNet newCarryName newCarry (pkDesign pk7)}
        pk9 = pk8{pkDesign = connectPort (cellName feedin) (cid pk "COUT") newCarryName (pkDesign pk8)}
        -- chain_in.cell.ports[CIN].net = nullptr
        pk10 = pk9{pkDesign = disconnectPort chainIn (cid pk "CIN") (pkDesign pk9)}
        pk11 = pk10{pkDesign = connectPort chainIn (cid pk "CIN") newCarryName (pkDesign pk10)}
        feedinName = cellName feedin
     in (pk11, feedinName)

makeCarryFeedOut :: Packer -> IdString -> Maybe IdString -> Bool -> (Packer, IdString)
makeCarryFeedOut pk carry chainNext breakChain =
    let (pk1, feedout) = createCell pk (cid pk "CCU2C") Nothing
        pk2 = setP (cellName feedout) (cid pk "INIT0") (propFromInt 0 16) pk1
        pk3 = setP (cellName feedout) (cid pk "INIT1") (propFromInt 10 16) pk2
        pk4 = setP (cellName feedout) (cid pk "INJECT1_0") (PropStr "NO") pk3
        pk5 = setP (cellName feedout) (cid pk "INJECT1_1") (PropStr "NO") pk4
        (pk6, feedoutName) =
            if breakChain
                then
                    let pk6a = pk5{pkDesign = connectPort (cellName feedout) (cid pk "CIN") carry (pkDesign pk5)}
                        pk6b = maybe pk6a (\nn -> pk6a{pkDesign = removeNetUser carry nn (cid pk "CIN") (pkDesign pk6a)}) chainNext
                        newNetName = named pk (cellName feedout) "$S0"
                        newNet = createNet pk6b newNetName
                        pk6c = pk6b{pkDesign = addNet newNetName newNet (pkDesign pk6b)}
                        pk6d = pk6c{pkDesign = connectPort (cellName feedout) (cid pk "S0") newNetName (pkDesign pk6c)}
                        pk6e = maybe pk6d (\nn -> pk6d{pkDesign = disconnectPort nn (cid pk "CIN") (pkDesign pk6d)}) chainNext
                        pk6f = maybe pk6e (\nn -> pk6e{pkDesign = connectPort nn (cid pk "CIN") newNetName (pkDesign pk6e)}) chainNext
                     in (pk6f, cellName feedout)
                else
                    let carryDrv = netDriver (netOf pk5 carry)
                        pk6a = pk5{pkDesign = removeNetDriver carry (pkDesign pk5)}
                        pk6b = pk6a{pkDesign = connectPort (cellName feedout) (cid pk "S0") carry (pkDesign pk6a)}
                        newCinName = named pk (cellName feedout) "$CIN"
                        newCin = createNet pk6b newCinName
                        pk6c = pk6b{pkDesign = addNet newCinName newCin (pkDesign pk6b)}
                        pk6d =
                            case prCell carryDrv of
                                Just drvCell ->
                                    let drvPort = prPort carryDrv
                                     in pk6c{pkDesign = disconnectPort drvCell drvPort (pkDesign pk6c)}
                                        & \p -> p{pkDesign = setNetDriver newCinName drvCell drvPort (pkDesign p)}
                                        & \p -> p{pkDesign = connectPort drvCell drvPort newCinName (pkDesign p)}
                                Nothing -> pk6c
                        pk6e = pk6d{pkDesign = connectPort (cellName feedout) (cid pk "CIN") newCinName (pkDesign pk6d)}
                        (pk6f, _) =
                            case chainNext of
                                Nothing -> (pk6e, feedout)
                                Just nn ->
                                    let pk7a = pk6e{pkDesign = connectPort (cellName feedout) (cid pk "A1") carry (pkDesign pk6e)}
                                        pk7b = pk7a{pkDesign = removeNetUser carry nn (cid pk "CIN") (pkDesign pk7a)}
                                        newCoutName = named pk (cellName feedout) "$COUT"
                                        newCout = createNet pk7b newCoutName
                                        pk7c = pk7b{pkDesign = addNet newCoutName newCout (pkDesign pk7b)}
                                        pk7d = pk7c{pkDesign = connectPort (cellName feedout) (cid pk "COUT") newCoutName (pkDesign pk7c)}
                                        pk7e = pk7d{pkDesign = disconnectPort nn (cid pk "CIN") (pkDesign pk7d)}
                                        pk7f = pk7e{pkDesign = connectPort nn (cid pk "CIN") newCoutName (pkDesign pk7e)}
                                     in (pk7f, feedout)
                     in (pk6f, cellName feedout)
     in (pk6, feedoutName)

-- | Split a carry chain into multiple legal chains.
splitCarryChain :: Packer -> CellChain -> (Packer, [CellChain])
splitCarryChain pk (CellChain cells) =
    go pk cells True [] []
  where
    maxLen = (cdWidth (ecp5Chipdb (pkE pk)) - 4) * 4 - 2
    go p [] _ curr chains = (p, chains ++ [CellChain curr])
    go p (c : rest) startOfChain curr chains =
        -- feed-in at chain start
        let (p1, chain0) =
                if startOfChain
                    then
                        case getPort (cellOf p c) (cid p "CIN") of
                            Just cinNet ->
                                let (p', feedin) = makeCarryFeedIn p cinNet c
                                 in (p', [feedin])
                            Nothing -> (p, [])
                    else (p, curr)
            chain1 = chain0 ++ [c]
            atEnd = rest == []
            splitChain = length chain1 > maxLen
            carryNet = getPort (cellOf p1 c) (cid p1 "COUT")
            nextport =
                if atEnd
                    then Nothing
                    else Just (head rest)
            (p2, chain2) =
                case carryNet of
                    Just cn ->
                        let users = V.length (V.filter (/= Nothing) (netUsers (netOf p1 cn)))
                            needOut = splitChain || users > 1 || atEnd
                            doBreak = splitChain && not atEnd
                         in if needOut
                                then
                                    let (p', outCell) = makeCarryFeedOut p1 cn nextport doBreak
                                     in if splitChain && atEnd
                                            then (p', replaceLast chain1 outCell)
                                            else (p', chain1 ++ [outCell])
                                else (p1, chain1)
                    Nothing -> (p1, chain1)
            (chains', curr') =
                if splitChain
                    then (chains ++ [CellChain chain2], [])
                    else (chains, chain2)
         in go p2 rest splitChain curr' chains'
    replaceLast xs x = reverse (x : drop 1 (reverse xs))

packCarries :: Packer -> Packer
packCarries pk =
    let e = pkE pk
        carryChains =
            findChains
                pk
                (isCarry pk)
                (\ci -> netDrivenBy pk (getPort ci (cid pk "CIN")) (isCarry pk) (cid pk "COUT"))
                (\ci -> netOnlyDrives pk (getPort ci (cid pk "COUT")) (isCarry pk) (cid pk "CIN") False Nothing)
                1
        -- chain splitting
        (pk1, splitChains) = let _m = "CHAINS " ++ show [[T.unpack (idStr pk c) | c <- cs] | CellChain cs <- take 1 carryChains] ++ " LENS " ++ show (map (\(CellChain cs) -> length cs) carryChains) in _m `deepseq` trace _m (foldl splitOne (pk, []) carryChains)
        splitOne (pAcc, acc) ch =
            let (pAcc', chains) = splitCarryChain pAcc ch
             in (pAcc', acc ++ chains)
        -- chain packing
        (pk2, packedChains) = let _m2 = "SPLIT " ++ show (map (\(CellChain cs) -> map (idStr pk1) cs) (take 2 splitChains)) in _m2 `deepseq` trace _m2 (foldl packChain (pk1, []) splitChains)
        packChain (pkAcc, acc) (CellChain cells) =
            let (pkAcc', chainCells) = foldl packCell (pkAcc, []) cells
             in (pkAcc', chainCells : acc)
        packCell (pkAcc, accCells) c =
            
            let tId = \s -> fromMaybe emptyId (M.lookup s (tdConstIdByName (ecp5TimingDb (pkE pkAcc))))
                (pkAcc1, comb0) = createCell pkAcc (cid pkAcc "TRELLIS_COMB") (Just (named pkAcc c "$CCU2_COMB0"))
                (pkAcc2, comb1) = createCell pkAcc1 (cid pkAcc1 "TRELLIS_COMB") (Just (named pkAcc1 c "$CCU2_COMB1"))
                comb0n = cellName comb0
                comb1n = cellName comb1
                carryNetName = named pkAcc2 c "$CCU2_FCI_INT"
                carryNet = createNet pkAcc2 carryNetName
                pkAcc3 = pkAcc2{pkDesign = addNet carryNetName carryNet (pkDesign pkAcc2)}
                pkAcc4 = ccu2ToComb pkAcc3 c comb0n carryNetName 0
                pkAcc5 = ccu2ToComb pkAcc4 c comb1n carryNetName 1
                pkAcc6 = addNew comb1 (addNew comb0 pkAcc5)
                pkAcc7 = pkAcc6{pkPacked = pkPacked pkAcc6 ++ [c]}
             in (pkAcc7, accCells ++ [comb0n, comb1n])
        -- relative chain placement
        pk3 = foldl placeChain pk2 packedChains
        placeChain pkAcc chain =
            let root = head chain
                pkA = pkAcc{pkDesign = setCellConstr root 0 0 0 True (pkDesign pkAcc)}
                pkB = pkA{pkDesign = setCellCluster root (Just root) 0 0 0 True (pkDesign pkA)}
                (pkC, _) = foldl (placeCell root) (pkB, 1) (drop 1 chain)
             in pkC
        placeCell root (pkAcc, i) c =
            let pkAcc' = pkAcc{pkDesign = setCellConstr c (i `div` 8) 0 ((i `mod` 8) `shiftL` 2 .|. 0) True (pkDesign pkAcc)}
                pkAcc'' = pkAcc'{pkDesign = setCellCluster c (Just root) (i `div` 8) 0 ((i `mod` 8) `shiftL` 2 .|. 0) True (pkDesign pkAcc')}
                pkAcc''' = pkAcc''{pkDesign = setCellChildren root (cellConstrChildren (cellOf pkAcc'' root) ++ [c]) (pkDesign pkAcc'')}
             in (pkAcc''', i + 1)
     in flushCells pk3

-- | @ccu2_to_comb@ (cells.cc).
ccu2ToComb :: Packer -> IdString -> IdString -> IdString -> Int -> Packer
ccu2ToComb pk ccu comb carryNet i =
    let ii = T.pack (show i)
        pk1 = setP comb (cid pk "MODE") (PropStr "CCU2") pk
        pk2 = setP comb (cid pk "INITVAL") (fromMaybe (propFromInt 0 16) (M.lookup (internT pk ("INIT" <> ii)) (cellParams (cellOf pk ccu)))) pk1
        pk3 = setP comb (cid pk "CCU2_INJECT1") (fromMaybe (PropStr "YES") (M.lookup (internT pk ("INJECT1_" <> ii)) (cellParams (cellOf pk ccu)))) pk2
        pk4 = movePort pk3 ccu (internT pk ("A" <> ii)) comb (cid pk "A")
        pk5 = movePort pk4 ccu (internT pk ("B" <> ii)) comb (cid pk "B")
        pk6 = movePort pk5 ccu (internT pk ("C" <> ii)) comb (cid pk "C")
        pk7 = movePort pk6 ccu (internT pk ("D" <> ii)) comb (cid pk "D")
        pk8 = movePort pk7 ccu (internT pk ("S" <> ii)) comb (cid pk "F")
        pk9 =
            if i == 0
                then
                    let pkA = movePort pk8 ccu (cid pk "CIN") comb (cid pk "FCI")
                     in pkA{pkDesign = connectPort comb (cid pk "FCO") carryNet (pkDesign pkA)}
                else
                    let pkA = pk8{pkDesign = connectPort comb (cid pk "FCI") carryNet (pkDesign pk8)}
                     in movePort pkA ccu (cid pk "COUT") comb (cid pk "FCO")
        -- copy the ccu's attrs onto the comb (the C++ ccu2_to_comb)
        pk10 = foldl (\p (k, v) -> p{pkDesign = setCellAttr comb k v (pkDesign p)}) pk9 (M.toList (cellAttrs (cellOf pk ccu)))
     in pk10

-- ---------------------------------------------------------------------------
-- pack_dram
-- ---------------------------------------------------------------------------

-- | @get_dram_init@.
getDramInit :: Packer -> CellInfo BelId WireId PipId -> Int -> Int
getDramInit pk ram bit =
    let initProp = fromMaybe (propFromInt 0 16) (M.lookup (cid pk "INITVAL") (cellParams ram))
        idata = T.unpack (pStr initProp)
        value = foldl (\acc i -> if i < length idata && idata !! (4 * i + bit) == '1' then acc .|. (1 `shiftL` i) else acc) 0 [0 .. 15]
     in value

dramToRamwSplit :: Packer -> IdString -> IdString -> Packer
dramToRamwSplit pk ram ramw = 
    foldl (\pkAcc (o, d) -> movePort pkAcc ram (internT pkAcc o) ramw (cid pkAcc d)) pk
        [ ("WAD[0]", "D0"), ("WAD[1]", "B0"), ("WAD[2]", "C0"), ("WAD[3]", "A0")
        , ("DI[0]", "C1"), ("DI[1]", "A1"), ("DI[2]", "D1"), ("DI[3]", "B1")
        ]

dramToComb :: Packer -> IdString -> CellInfo BelId WireId PipId -> IdString -> Int -> Packer
dramToComb pk ram combCell ramw index =
    let comb = cellName combCell in
    
    let pk1 = setP comb (cid pk "MODE") (PropStr "DPRAM") pk
        pk2 = setP comb (cid pk "WREMUX") (PropStr (T.pack (strOrDef (cellParams (cellOf pk ram)) (cid pk "WREMUX") "WRE"))) pk1
        pk3 = setP comb (cid pk "WCKMUX") (PropStr (T.pack (strOrDef (cellParams (cellOf pk ram)) (cid pk "WCKMUX") "WCK"))) pk2
        initVal = getDramInit pk (cellOf pk ram) index
        permuted :: Int
        permuted = foldl (\acc i ->
                    let permutedAddr :: Int
                        permutedAddr =
                            (if i .&. 1 /= 0 then 8 else 0)
                                .|. (if i .&. 2 /= 0 then 2 else 0)
                                .|. (if i .&. 4 /= 0 then 4 else 0)
                                .|. (if i .&. 8 /= 0 then 1 else 0)
                     in if initVal `div` (2 ^ permutedAddr) `mod` 2 == 1 then acc .|. (1 `shiftL` i) else acc)
                    0
                    [0 .. 15]
        pk4 = setP comb (cid pk "INITVAL") (propFromInt (fromIntegral permuted) 16) pk3
        pk5 = case getPort (cellOf pk ram) (internT pk ("RAD[0]")) of
                    Just n -> pk4{pkDesign = connectPort comb (cid pk "D") n (pkDesign pk4)}
                    Nothing -> pk4
        pk6 = case getPort (cellOf pk ram) (internT pk ("RAD[1]")) of
                    Just n -> pk5{pkDesign = connectPort comb (cid pk "B") n (pkDesign pk5)}
                    Nothing -> pk5
        pk7 = case getPort (cellOf pk ram) (internT pk ("RAD[2]")) of
                    Just n -> pk6{pkDesign = connectPort comb (cid pk "C") n (pkDesign pk6)}
                    Nothing -> pk6
        pk8 = case getPort (cellOf pk ram) (internT pk ("RAD[3]")) of
                    Just n -> pk7{pkDesign = connectPort comb (cid pk "A") n (pkDesign pk7)}
                    Nothing -> pk7
        pk9 = case getPort (cellOf pk ram) (cid pk "WRE") of
                    Just n -> pk8{pkDesign = connectPort comb (cid pk "WRE") n (pkDesign pk8)}
                    Nothing -> pk8
        pk10 = case getPort (cellOf pk ram) (cid pk "WCK") of
                    Just n -> pk9{pkDesign = connectPort comb (cid pk "WCK") n (pkDesign pk9)}
                    Nothing -> pk9
        pk11 = connectPorts ramw (cid pk "WADO0") comb (cid pk "WAD0") pk10
        pk12 = connectPorts ramw (cid pk "WADO1") comb (cid pk "WAD1") pk11
        pk13 = connectPorts ramw (cid pk "WADO2") comb (cid pk "WAD2") pk12
        pk14 = connectPorts ramw (cid pk "WADO3") comb (cid pk "WAD3") pk13
        pk15 = connectPorts ramw (internT pk ("WDO" <> T.pack (show index))) comb (cid pk "WD") pk14
        pk16 = movePort pk15 ram (internT pk ("DO[" <> T.pack (show index) <> "]")) comb (cid pk "F")
        -- merge the ram's attrs into the existing comb cell (the C++
        -- dram_to_comb copies attrs only; re-adding the template here
        -- would wipe the params/ports set above)
        pk17 = foldl (\p (k, v) -> p{pkDesign = setCellAttr comb k v (pkDesign p)}) pk16 (M.toList (cellAttrs (cellOf pk ram)))
     in pk17

packDram :: Packer -> Packer
packDram pk =
    let cells = cellsIter (pkDesign pk)
        pk' = foldl packOne pk cells
     in flushCells pk'
  where
    packOne pk ci
        | not (isDpram pk ci) = pk
        | otherwise = 
            let (pk1, ramwSlice0) = createCell pk (cid pk "TRELLIS_RAMW") (Just (internT pk (idStr pk (cellName ci) <> "$RAMW_SLICE")))
                ramwSliceN = cellName ramwSlice0
                pk2 = dramToRamwSplit pk1 (cellName ci) ramwSliceN
                (pk3, ramCombs) = foldl makeComb (pk2, []) [0 .. 3]
                makeComb (pkAcc, acc) i =
                    let (pkA, rc) = createCell pkAcc (cid pkAcc "TRELLIS_COMB") (Just (named pkAcc (cellName ci) ("$DPRAM_COMB" <> T.pack (show i))))
                        pkB = dramToComb pkA (cellName ci) rc ramwSliceN i
                     in (pkB, acc ++ [rc])
                (pk4, ramwBlocks) = foldl makeBlock (pk3, []) [0 .. 1]
                makeBlock (pkAcc, acc) i =
                    let (pkA, rb) = createCell pkAcc (cid pkAcc "TRELLIS_COMB") (Just (named pkAcc (cellName ci) ("$RAMW_BLOCK" <> T.pack (show i))))
                        pkB = setP (cellName rb) (cid pkAcc "MODE") (PropStr "RAMW_BLOCK") pkA
                     in (pkB, acc ++ [rb])
                pk5 = pk4{pkDesign = disconnectPort (cellName ci) (cid pk "WCK") (pkDesign pk4)}
                pk6 = pk5{pkDesign = disconnectPort (cellName ci) (cid pk "WRE") (pkDesign pk5)}
                pk7 = foldl (\p i -> p{pkDesign = disconnectPort (cellName ci) (internT pk ("RAD[" <> T.pack (show i) <> "]")) (pkDesign p)}) pk6 [0 .. 3]
                -- constraints: anchor ram_comb[0]
                pk8 = pk7{pkDesign = setCellConstr (cellName (head ramCombs)) 0 0 0 True (pkDesign pk7)}
                pk9 = pk8{pkDesign = setCellCluster (cellName (head ramCombs)) (Just (cellName (head ramCombs))) 0 0 0 True (pkDesign pk8)}
                pk10 = foldl (\p i ->
                        let c = cellName (ramCombs !! i)
                            p1 = p{pkDesign = setCellCluster c (Just (cellName (head ramCombs))) 0 0 ((i `shiftL` 2) .|. 0) True (pkDesign p)}
                            p2 = p1{pkDesign = setCellConstr c 0 0 ((i `shiftL` 2) .|. 0) True (pkDesign p1)}
                         in addChild2 (cellName (head ramCombs)) (head ramCombs) c p2)
                        pk9 [1 .. 3]
                pk11 = foldl (\p i ->
                        let c = cellName (ramwBlocks !! i)
                            p1 = p{pkDesign = setCellCluster c (Just (cellName (head ramCombs))) 0 0 (((i + 4) `shiftL` 2) .|. 0) True (pkDesign p)}
                            p2 = p1{pkDesign = setCellConstr c 0 0 (((i + 4) `shiftL` 2) .|. 0) True (pkDesign p1)}
                         in addChild2 (cellName (head ramCombs)) (head ramCombs) c p2)
                        pk10 [0 .. 1]
                pk12 =
                    let p1 = pk11{pkDesign = setCellCluster ramwSliceN (Just (cellName (head ramCombs))) 0 0 ((4 `shiftL` 2) .|. 2) True (pkDesign pk11)}
                        p2 = p1{pkDesign = setCellConstr ramwSliceN 0 0 ((4 `shiftL` 2) .|. 2) True (pkDesign p1)}
                     in addChild2 (cellName (head ramCombs)) (head ramCombs) ramwSliceN p2
                pk13 = foldl (\p c -> addNew c p) pk12 (ramCombs ++ ramwBlocks ++ [ramwSlice0])
                pk14 = pk13{pkPacked = pkPacked pk13 ++ [cellName ci]}
             in pk14
      where
        addChild2 root rootCell c p =
            p{pkDesign = setCellChildren root (cellConstrChildren rootCell ++ [c]) (pkDesign p)}
        tId pk s = fromMaybe emptyId (M.lookup s (tdConstIdByName (ecp5TimingDb (pkE pk))))

-- ---------------------------------------------------------------------------
-- pack_constants
-- ---------------------------------------------------------------------------

makeInitWithConstInput :: Int -> Int -> Bool -> Int
makeInitWithConstInput init' input value =
    foldl
        (\acc i ->
            if ((i `shiftR` input) .&. 1) /= (if value then 1 else 0)
                then
                    let otherI = (i .&. complement (1 `shiftL` input)) .|. (if value then 1 `shiftL` input else 0)
                     in if init' `div` (2 ^ otherI) `mod` 2 == 1 then acc .|. (1 `shiftL` i) else acc
                else if init' `div` (2 ^ i) `mod` 2 == 1 then acc .|. (1 `shiftL` i) else acc)
        0
        [0 .. 15]

setLutInputConstant :: Packer -> IdString -> IdString -> Bool -> Packer
setLutInputConstant pk cell input value =
    let index = "ABCD" !! idxOf
        idxOf = maybe 0 id (T.findIndex (== T.head (idStr pk input)) "ABCD")
        init' = intOrDef (cellParams (cellOf pk cell)) (cid pk "INIT") 0
        newInit = makeInitWithConstInput init' idxOf value
        pk1 = setP cell (cid pk "INIT") (propFromInt (fromIntegral newInit) 16) pk
     in pk1{pkDesign = disconnectPort cell input (pkDesign pk1)}

setCcu2cInputConstant :: Packer -> IdString -> IdString -> Bool -> Packer
setCcu2cInputConstant pk cell input value =
    let inputStr = idStr pk input
        lut = read (T.unpack (T.drop 1 inputStr)) :: Int
        index = maybe 0 id (T.findIndex (== T.head inputStr) "ABCD")
        init' = intOrDef (cellParams (cellOf pk cell)) (internT pk ("INIT" <> T.pack (show lut))) 0
        newInit = makeInitWithConstInput init' index value
        pk1 = setP cell (internT pk ("INIT" <> T.pack (show lut))) (propFromInt (fromIntegral newInit) 16) pk
     in pk1{pkDesign = disconnectPort cell input (pkDesign pk1)}

isCcu2cPortHigh :: Packer -> CellInfo BelId WireId PipId -> IdString -> Bool
isCcu2cPortHigh pk ci input =
    case M.lookup input (cellPorts ci) of
        Nothing -> True
        Just pi ->
            case portNet pi of
                Nothing -> True
                Just n
                    | n == internT pk "$PACKER_VCC_NET" -> True
                    | otherwise ->
                        case netDriver (netOf pk n) of
                            PortRef (Just c) _ -> cellType (cellOf pk c) == cid pk "VCC"
                            _ -> False

setNetConstant :: Packer -> IdString -> IdString -> Bool -> Packer
setNetConstant pk orig constnet constval =
    
    let ni = netOf pk orig
        users = activeUsers (netUsers ni)
        pk1 = pk{pkDesign = removeNetDriver orig (pkDesign pk)}
        (pk', _) = foldl stepUser (pk1, ()) users
        stepUser (pkAcc, _) u =
            case prCell u of
                Nothing -> (pkAcc, ())
                Just uc ->
                    let uci = cellOf pkAcc uc
                        uport = prPort u
                     in if isLut pkAcc uci
                            then (setLutInputConstant pkAcc uc uport constval, ())
                            else
                                if isFf pkAcc uci && uport == cid pkAcc "CE"
                                    then
                                        let inv = strOrDef (cellParams uci) (cid pkAcc "CEMUX") "CE" == "INV"
                                            newVal = if constval /= inv then "1" else "0"
                                            pkB = setP uc (cid pkAcc "CEMUX") (PropStr newVal) pkAcc
                                         in (pkB{pkDesign = disconnectPort uc uport (pkDesign pkB)}, ())
                                    else
                                        if isCarry pkAcc uci
                                            then
                                                if constval && uport `elem` map (cid pkAcc) ["A0", "A1", "B0", "B1", "C0", "C1", "D0", "D1"]
                                                    then (pkAcc{pkDesign = disconnectPort uc uport (pkDesign pkAcc)}, ())
                                                    else
                                                        if not constval
                                                            then
                                                                if uport `elem` map (cid pkAcc) ["A0", "A1", "B0", "B1"]
                                                                    then (setCcu2cInputConstant pkAcc uc uport constval, ())
                                                                    else
                                                                        if uport == cid pkAcc "C0" && isCcu2cPortHigh pkAcc uci (cid pkAcc "D0")
                                                                            then (setCcu2cInputConstant pkAcc uc uport constval, ())
                                                                            else
                                                                                if uport == cid pkAcc "D0" && isCcu2cPortHigh pkAcc uci (cid pkAcc "C0")
                                                                                    then (setCcu2cInputConstant pkAcc uc uport constval, ())
                                                                                    else
                                                                                        if uport == cid pkAcc "C1" && isCcu2cPortHigh pkAcc uci (cid pkAcc "D1")
                                                                                            then (setCcu2cInputConstant pkAcc uc uport constval, ())
                                                                                            else
                                                                                                if uport == cid pkAcc "D1" && isCcu2cPortHigh pkAcc uci (cid pkAcc "C1")
                                                                                                    then (setCcu2cInputConstant pkAcc uc uport constval, ())
                                                                                                    else (connectUser pkAcc uc uport constnet u, ())
                                                            else (connectUser pkAcc uc uport constnet u, ())
                                            else
                                                if isFf pkAcc uci && uport == cid pkAcc "LSR"
                                                    && ((not constval && strOrDef (cellParams uci) (cid pkAcc "LSRMUX") "LSR" == "LSR")
                                                            || (constval && strOrDef (cellParams uci) (cid pkAcc "LSRMUX") "LSR" == "INV"))
                                                    then (pkAcc{pkDesign = disconnectPort uc uport (pkDesign pkAcc)}, ())
                                                    else
                                                        if cellType uci == cid pkAcc "DP16KD"
                                                            then
                                                                let pname = idStr pkAcc uport
                                                                 in if pname `elem` ["CLKA", "CLKB", "RSTA", "RSTB", "WEA", "WEB", "CEA", "CEB", "OCEA", "OCEB", "CSA0", "CSA1", "CSA2", "CSB0", "CSB1", "CSB2"]
                                                                        then (setP uc (internT pkAcc (pname <> "MUX")) (PropStr (if constval then pname else "INV")) pkAcc{pkDesign = disconnectPort uc uport (pkDesign pkAcc)}, ())
                                                                        else (setP uc (internT pkAcc (pname <> "MUX")) (PropStr (if constval then "1" else "0")) pkAcc{pkDesign = disconnectPort uc uport (pkDesign pkAcc)}, ())
                                                            else
                                                                if cellType uci `elem` [cid pkAcc "ALU54B", cid pkAcc "MULT18X18D"]
                                                                    then
                                                                        let pname = idStr pkAcc uport
                                                                         in if any (`T.isPrefixOf` pname) ["CLK", "CE", "RST", "SRO", "SRI", "RO", "MA", "MB", "CFB", "CIN", "SOURCE", "SIGNED", "OP"]
                                                                                then (connectUser pkAcc uc uport constnet u, ())
                                                                                else (setP uc (internT pkAcc (pname <> "MUX")) (PropStr (if constval then "1" else "0")) pkAcc{pkDesign = disconnectPort uc uport (pkDesign pkAcc)}, ())
                                                                    else (connectUser pkAcc uc uport constnet u, ())
        connectUser pkAcc uc uport constnet u =
            pkAcc
                { pkDesign =
                    connectPort uc uport constnet (pkDesign pkAcc)
                        & \d -> d -- user_idx bookkeeping is implicit in connectPort
                }
        pk'' = pk'{pkDesign = setNetUsers orig V.empty (pkDesign pk')}
     in pk''
  where
    (&) x f = f x

packConstants :: Packer -> Packer
packConstants pk = do
    let e = pkE pk
        cells = cellsIter (pkDesign pk)
        -- VLO/VHI -> GND/VCC
        pk1 = foldl fixConst pk cells
        fixConst pkAcc ci
            | cellType ci == internT pkAcc "VLO" = pkAcc{pkDesign = setCellType (cellName ci) (cid pkAcc "GND") (pkDesign pkAcc)}
            | cellType ci == internT pkAcc "VHI" = pkAcc{pkDesign = setCellType (cellName ci) (cid pkAcc "VCC") (pkDesign pkAcc)}
            | otherwise = pkAcc
        (pk2, gndCell0) = createCell pk1 (cid pk1 "LUT4") (Just (internT pk1 "$PACKER_GND"))
        gndCell = cellName gndCell0
        pk3 = setP gndCell (cid pk2 "INIT") (propFromInt 0 16) pk2
        gndNetName = internT pk3 "$PACKER_GND_NET"
        gndNet = createNet pk3 gndNetName
        gndNet' = gndNet{netDriver = PortRef (Just gndCell) (cid pk3 "Z")}
        pk4 = pk3{pkDesign = addNet gndNetName gndNet' (pkDesign pk3)}
        pk5 = pk4{pkDesign = connectPort gndCell (cid pk4 "Z") gndNetName (pkDesign pk4)}
        -- the C++ keeps the $PACKER cells out of ctx->cells until the
        -- used checks at the end; remove them from the ORDER only (the
        -- map entries stay for the port surgery) so the erases below
        -- swap the correct back elements
        pk5b = orderDetachCell gndCell pk5
        (pk6, vccCell0) = createCell pk5b (cid pk5b "LUT4") (Just (internT pk5b "$PACKER_VCC"))
        vccCell = cellName vccCell0
        pk7 = setP vccCell (cid pk6 "INIT") (propFromInt 65535 16) pk6
        vccNetName = internT pk7 "$PACKER_VCC_NET"
        vccNet = createNet pk7 vccNetName
        vccNet' = vccNet{netDriver = PortRef (Just vccCell) (cid pk7 "Z")}
        pk8 = pk7{pkDesign = addNet vccNetName vccNet' (pkDesign pk7)}
        pk9 = pk8{pkDesign = connectPort vccCell (cid pk8 "Z") vccNetName (pkDesign pk8)}
        pk9b = orderDetachCell vccCell pk9
        -- process GND/VCC driven nets
        (pk10, gndUsed, vccUsed, deadNets, deadCells) =
            foldl
                (\ (pkAcc, gu, vu, dn, dc) (netName, ni) ->
                    case prCell (netDriver ni) of
                        Just drv
                            | cellType (cellOf pkAcc drv) == cid pkAcc "GND" ->
                                let pkB = setNetConstant pkAcc netName gndNetName False
                                 in (pkB, True, vu, dn ++ [netName], dc ++ [drv])
                            | cellType (cellOf pkAcc drv) == cid pkAcc "VCC" ->
                                let pkB = setNetConstant pkAcc netName vccNetName True
                                 in (pkB, gu, True, dn ++ [netName], dc ++ [drv])
                        _ -> (pkAcc, gu, vu, dn, dc))
                (pk9b, False, False, [], [])
                (zip (map netName (netsIter (pkDesign pk9b))) (netsIter (pkDesign pk9b)))
        -- the C++ erases the dead CELLS during the net loop (before the
        -- $PACKER cell inserts), then inserts the gnd/vcc cells+nets,
        -- then erases the dead NETS (the swaps scatter the $PACKER nets
        -- into the dead nets' slots — mirror that order exactly)
        pk11 = trace ("ORDLEN " ++ show (length (designCellOrder (pkDesign pk10))) ++ " first3=" ++ show (take 3 (reverse (map (idStr pk10) (designCellOrder (pkDesign pk10)))))) (pk10{pkDesign = foldl (\d n -> deleteCellSwap n d) (pkDesign pk10) deadCells})
        -- the C++ inserts the gnd/vcc cells+nets into the dict at the
        -- END (after the net loop) and only when used; they are already
        -- in our maps (needed for the port surgery), so move them to the
        -- end of the iteration order instead. When unused they never
        -- entered the dict at all: plain removal, no swap-erase.
        pk12 =
            if gndUsed
                then moveCellToEnd gndCell pk11
                else removeCellPlain gndCell pk11
        pk13 =
            if vccUsed
                then moveCellToEnd vccCell pk12
                else removeCellPlain vccCell pk12
        pk14 =
            if gndUsed
                then moveNetToEnd gndNetName pk13
                else removeNetPlain gndNetName pk13
        pk15 =
            if vccUsed
                then moveNetToEnd vccNetName pk14
                else removeNetPlain vccNetName pk14
        pk16 = pk15{pkDesign = foldl (\d n -> deleteNetSwap n d) (pkDesign pk15) deadNets}
     in pk16
  where
    moveCellToEnd n p = p{pkDesign = (pkDesign p){designCellOrder = filter (/= n) (designCellOrder (pkDesign p)) ++ [n]}}
    orderDetachCell n p = p{pkDesign = (pkDesign p){designCellOrder = filter (/= n) (designCellOrder (pkDesign p))}}
    moveNetToEnd n p = p{pkDesign = (pkDesign p){designNetOrder = filter (/= n) (designNetOrder (pkDesign p)) ++ [n]}}
    removeCellPlain n p = p{pkDesign = (pkDesign p){designCells = M.delete n (designCells (pkDesign p)), designCellOrder = filter (/= n) (designCellOrder (pkDesign p))}}
    removeNetPlain n p = p{pkDesign = (pkDesign p){designNets = M.delete n (designNets (pkDesign p)), designNetOrder = filter (/= n) (designNetOrder (pkDesign p))}}
    tId pk s = fromMaybe emptyId (M.lookup s (tdConstIdByName (ecp5TimingDb (pkE pk))))

-- ---------------------------------------------------------------------------
-- pack_ebr
-- ---------------------------------------------------------------------------

autoCreateEmptyPort :: Packer -> IdString -> IdString -> Packer
autoCreateEmptyPort pk cell port =
    let ci = cellOf pk cell
     in if M.member port (cellPorts ci)
            then pk
            else pk{pkDesign = setCellPort cell port PortIn (pkDesign pk)}

packEbr :: Packer -> Packer
packEbr pk =
    let cells = cellsIter (pkDesign pk)
        -- PDPW16KD -> DP16KD conversion
        pk1 = foldl convertPdp pk cells
        convertPdp pkAcc ci
            | cellType ci /= cid pkAcc "PDPW16KD" = pkAcc
            | otherwise =
                let pkA = setP (cellName ci) (cid pkAcc "DATA_WIDTH_A") (propFromInt 36 32) pkAcc
                    pkB = pkA{pkDesign = delCellParam (cellName ci) (cid pkAcc "DATA_WIDTH_W") (pkDesign pkA)}
                    pkC = foldl renameBus pkB
                        [ ("BE", "ADA", 4, 0, 0)
                        , ("ADW", "ADA", 9, 0, 5)
                        , ("ADR", "ADB", 14, 0, 0)
                        , ("CSW", "CSA", 3, 0, 0)
                        , ("CSR", "CSB", 3, 0, 0)
                        , ("DI", "DIA", 18, 0, 0)
                        , ("DI", "DIB", 18, 18, 0)
                        , ("DO", "DOA", 18, 18, 0)
                        , ("DO", "DOB", 18, 0, 0)
                        ]
                    renameBus p (old, new, width, oldofs, newof) =
                        foldl (\p i -> movePort p (cellName ci) (cid p (old <> T.pack (show (i + oldofs)))) (cellName ci) (cid p (new <> T.pack (show (i + newof))))) p [0 .. width - 1]
                    pkD = movePort pkC (cellName ci) (cid pkC "CLKW") (cellName ci) (cid pkC "CLKA")
                    pkE = movePort pkD (cellName ci) (cid pkD "CLKR") (cellName ci) (cid pkD "CLKB")
                    pkF = movePort pkE (cellName ci) (cid pkE "CEW") (cellName ci) (cid pkE "CEA")
                    pkG = movePort pkF (cellName ci) (cid pkF "CER") (cellName ci) (cid pkF "CEB")
                    pkH = movePort pkG (cellName ci) (cid pkG "OCER") (cellName ci) (cid pkG "OCEB")
                    pkI = renameParam pkH "CLKWMUX" "CLKAMUX"
                    pkJ = if strOrDef (cellParams (cellOf pkI (cellName ci))) (cid pkI "CLKAMUX") "" == "CLKW" then setP (cellName ci) (cid pkI "CLKAMUX") (PropStr "CLKA") pkI else pkI
                    pkK = renameParam pkJ "CLKRMUX" "CLKBMUX"
                    pkL = if strOrDef (cellParams (cellOf pkK (cellName ci))) (cid pkK "CLKBMUX") "" == "CLKR" then setP (cellName ci) (cid pkK "CLKBMUX") (PropStr "CLKB") pkK else pkK
                    pkM = renameParam pkL "CSDECODE_W" "CSDECODE_A"
                    pkN = renameParam pkM "CSDECODE_R" "CSDECODE_B"
                    outreg = strOrDef (cellParams (cellOf pkN (cellName ci))) (cid pkN "REGMODE") "NOREG"
                    pkO = setP (cellName ci) (cid pkN "REGMODE_A") (PropStr (T.pack outreg)) pkN
                    pkP = setP (cellName ci) (cid pkN "REGMODE_B") (PropStr (T.pack outreg)) pkO
                    pkQ = pkP{pkDesign = delCellParam (cellName ci) (cid pkP "REGMODE") (pkDesign pkP)}
                    pkR = renameParam pkQ "DATA_WIDTH_R" "DATA_WIDTH_B"
                    renameParam p old new =
                        let o = cid p old
                            n = cid p new
                         in if M.member o (cellParams (cellOf p (cellName ci)))
                                then
                                    let v = fromMaybe (propFromInt 0 16) (M.lookup o (cellParams (cellOf p (cellName ci))))
                                        p1 = setP (cellName ci) n v p
                                     in p1{pkDesign = delCellParam (cellName ci) o (pkDesign p1)}
                                else p
                    pkS =
                        if M.member (cid pkR "RST") (cellPorts (cellOf pkR (cellName ci)))
                            then
                                let rst = getPort (cellOf pkR (cellName ci)) (cid pkR "RST")
                                    pkT = maybe pkR (\n -> pkR{pkDesign = connectPort (cellName ci) (cid pkR "RSTA") n (pkDesign pkR)}) rst
                                    pkU = maybe pkT (\n -> pkT{pkDesign = connectPort (cellName ci) (cid pkR "RSTB") n (pkDesign pkT)}) rst
                                    pkV = pkU{pkDesign = disconnectPort (cellName ci) (cid pkU "RST") (pkDesign pkU)}
                                 in insertCell (M.delete (cid pkU "RST") (cellPorts (cellOf pkU (cellName ci)))) pkU
                            else pkR
                    insertCell ports p = p{pkDesign = addCell (cellName ci) (cellOf p (cellName ci)){cellPorts = ports} (pkDesign p)}
                 in pkS{pkDesign = setCellType (cellName ci) (cid pkS "DP16KD") (pkDesign pkS)}
        cells2 = cellsIter (pkDesign pk1)
        -- add empty ports + WID
        (pk2, widState) = foldl addPorts (pk1, 3) [ci | ci <- cells2, cellType ci == cid pk1 "DP16KD"]
        addPorts (pkAcc, wid) ci =
            let portsA = [("ADA" <> T.pack (show i)) | i <- [0 .. 13]] ++ [("ADB" <> T.pack (show i)) | i <- [0 .. 13]]
                portsB = [("DIA" <> T.pack (show i)) | i <- [0 .. 17]] ++ [("DIB" <> T.pack (show i)) | i <- [0 .. 17]]
                portsC = [("CSA" <> T.pack (show i)) | i <- [0 .. 2]] ++ [("CSB" <> T.pack (show i)) | i <- [0 .. 2]]
                singles = ["CLKA", "CEA", "OCEA", "WEA", "RSTA", "CLKB", "CEB", "OCEB", "WEB", "RSTB"]
                pkA = foldl (\p port -> autoCreateEmptyPort p (cellName ci) (cid p port)) pkAcc (portsA ++ portsB ++ portsC ++ singles)
                pkB = pkA{pkDesign = setCellAttr (cellName ci) (cid pkA "WID") (propFromInt (fromIntegral wid) 32) (pkDesign pkA)}
             in (pkB, wid + 1)
     in pk2

-- ---------------------------------------------------------------------------
-- pack_dsps / check_alu
-- ---------------------------------------------------------------------------

packDsps :: Packer -> Packer
packDsps pk =
    let cells = cellsIter (pkDesign pk)
        pk' = foldl packOne pk cells
     in pk'
  where
    packOne pkAcc ci
        | cellType ci == cid pkAcc "MULT18X18D" =
            let mkSig p sig = foldl (\p' i -> autoCreateEmptyPort p' (cellName ci) (internT p' (sig <> T.pack (show i)))) p [0 .. 3]
                pkA = foldl mkSig pkAcc ["CLK", "CE", "RST"]
                -- SIGNED + {A,B} and SOURCE + {A,B}
                pkB = foldl (\p sig -> foldl (\p' c -> autoCreateEmptyPort p' (cellName ci) (internT p' (sig <> c))) p ["A", "B"]) pkA ["SIGNED", "SOURCE"]
                pkC = foldl (\p port -> foldl (\p' i -> autoCreateEmptyPort p' (cellName ci) (internT p' (port <> T.pack (show i)))) p [0 .. 17]) pkB ["A", "B", "C"]
                pkD = foldl (\p port -> foldl (\p' i -> autoCreateEmptyPort p' (cellName ci) (internT p' (port <> T.pack (show i)))) p [0 .. 17]) pkC ["SRIA", "SRIB"]
             in pkD
        | cellType ci == cid pkAcc "ALU54B" =
            let pkA = foldl (\p sig -> foldl (\p' i -> autoCreateEmptyPort p' (cellName ci) (internT p' (sig <> T.pack (show i)))) p [0 .. 3]) pkAcc ["CLK", "CE", "RST"]
                pkB = foldl (\p port -> autoCreateEmptyPort p (cellName ci) (cid p port)) pkA ["SIGNEDIA", "SIGNEDIB", "SIGNEDCIN"]
                pkC = foldl (\p port -> foldl (\p' i -> autoCreateEmptyPort p' (cellName ci) (internT p' (port <> T.pack (show i)))) p [0 .. 35]) pkB ["A", "B", "MA", "MB"]
                pkD = foldl (\p port -> foldl (\p' i -> autoCreateEmptyPort p' (cellName ci) (internT p' (port <> T.pack (show i)))) p [0 .. 53]) pkC ["C", "CFB", "CIN"]
                pkE = foldl (\p i -> autoCreateEmptyPort p (cellName ci) (internT p ("OP" <> T.pack (show i)))) pkD [0 .. 10]
                -- find MULTs feeding MA0/MB0
                multA = netDrivenBy pkE (getPort (cellOf pkE (cellName ci)) (cid pkE "MA0")) (\c -> cellType c == cid pkE "MULT18X18D") (cid pkE "P0")
                multB = netDrivenBy pkE (getPort (cellOf pkE (cellName ci)) (cid pkE "MB0")) (\c -> cellType c == cid pkE "MULT18X18D") (cid pkE "P0")
                pkF = constrainMult pkE multA "MA0" (Just (-3))
                pkG = constrainMult pkF multB "MB0" (Just (-2))
                constrainMult p Nothing _ _ = p
                constrainMult p (Just m) port mz =
                    let pk1 = p{pkDesign = setCellConstr m (-3) 0 (-3) False (pkDesign p)}
                        pk2 = pk1{pkDesign = setCellConstr (cellName ci) 0 0 0 False (pkDesign pk1)}
                        pk3 = pk2{pkDesign = setCellCluster m (Just (cellName ci)) (cellConstrX (cellOf pk2 m)) (cellConstrY (cellOf pk2 m)) (cellConstrZ (cellOf pk2 m)) (cellConstrAbsZ (cellOf pk2 m)) (pkDesign pk2)}
                        pk4 = pk3{pkDesign = setCellCluster (cellName ci) (Just (cellName ci)) 0 0 0 False (pkDesign pk3)}
                        pk5 = pk4{pkDesign = setCellChildren (cellName ci) (cellConstrChildren (cellOf pk4 (cellName ci)) ++ [m]) (pkDesign pk4)}
                     in pk5
             in checkAlu pkG (cellName ci) multA multB
        | otherwise = pkAcc

checkAlu :: Packer -> IdString -> Maybe IdString -> Maybe IdString -> Packer
checkAlu pk alu multA multB = pk

-- ---------------------------------------------------------------------------
-- pack_dcus / pack_misc
-- ---------------------------------------------------------------------------

packDcus :: Packer -> Packer
packDcus pk =
    let cells = cellsIter (pkDesign pk)
        pk1 = foldl dcuPass pk cells
        pk2 = foldl extrefPass pk1 (cellsIter (pkDesign pk1))
     in pk2
  where
    dcuPass pkAcc ci
        | cellType ci == cid pkAcc "DCUA" =
            let pkA = applyLoc pkAcc
                applyLoc p =
                    case M.lookup (cid p "LOC") (cellAttrs ci) of
                        Just (PropStr loc) ->
                            let bel = dcuBel p (T.unpack loc)
                             in maybe p (\b -> setP (cellName ci) (cid p "BEL") (PropStr b) p) bel
                        _ -> p
                dcuBel p loc =
                    let dev = eaDevice (eaArgs p)
                        t = idStr p (cellType ci)
                     in if loc == "DCU0" && (dev == Lfe5um25f || dev == Lfe5um5g25f)
                            then Just "X42/Y50/DCU"
                            else
                                if loc == "DCU0" && (dev == Lfe5um45f || dev == Lfe5um5g45f)
                                    then Just "X42/Y71/DCU"
                                    else
                                        if loc == "DCU1" && (dev == Lfe5um45f || dev == Lfe5um5g45f)
                                            then Just "X69/Y71/DCU"
                                            else
                                                if loc == "DCU0" && (dev == Lfe5um85f || dev == Lfe5um5g85f)
                                                    then Just "X46/Y95/DCU"
                                                    else
                                                        if loc == "DCU1" && (dev == Lfe5um85f || dev == Lfe5um5g85f)
                                                            then Just "X71/Y95/DCU"
                                                            else Nothing
                eaArgs p = ecp5Args (pkE p)
                -- add empty ports from an exemplar bel
                pkB =
                    case [b | b <- getBels (pkE pkA), idStr pkA (getBelType (pkE pkA) b) == "DCUA"] of
                        (exemplar : _) ->
                            let ins = [p | p <- getBelPins (pkE pkA) exemplar, getBelPinType (pkE pkA) exemplar p == PortIn]
                             in foldl (\p port -> autoCreateEmptyPort p (cellName ci) port) pkA ins
                        [] -> pkA
                -- disconnect constant-driven ND ports
                ndPorts = ["D_TXBIT_CLKP_FROM_ND", "D_TXBIT_CLKN_FROM_ND", "D_SYNC_ND", "D_TXPLL_LOL_FROM_ND", "CH0_HDINN", "CH0_HDINP", "CH1_HDINN", "CH1_HDINP"]
                pkC = foldl (\p port ->
                        case getPort (cellOf p (cellName ci)) (cid p port) of
                            Just n ->
                                let drv = netOf p n
                                 in case prCell (netDriver drv) of
                                        Just c -> if cellType (cellOf p c) `elem` [cid p "GND", cid p "VCC"]
                                            then insertCellPorts (M.delete (cid p port) (cellPorts (cellOf p (cellName ci)))) p
                                            else p
                                        Nothing -> p
                            Nothing -> p) pkB ndPorts
                insertCellPorts ports p = p{pkDesign = addCell (cellName ci) (cellOf p (cellName ci)){cellPorts = ports} (pkDesign p)}
             in pkC
        | otherwise = pkAcc
    extrefPass pkAcc ci
        | cellType ci == cid pkAcc "EXTREFB" =
            let refo = getPort (cellOf pkAcc (cellName ci)) (cid pkAcc "REFCLKO")
                locBel =
                    case M.lookup (cid pkAcc "LOC") (cellAttrs ci) of
                        Just (PropStr loc) ->
                            let dev = eaDevice (ecp5Args (pkE pkAcc))
                             in if loc == "EXTREF0" && (dev == Lfe5um25f || dev == Lfe5um5g25f)
                                    then "X42/Y50/EXTREF"
                                    else
                                        if loc == "EXTREF0" && (dev == Lfe5um45f || dev == Lfe5um5g45f)
                                            then "X42/Y71/EXTREF"
                                            else
                                                if loc == "EXTREF1" && (dev == Lfe5um45f || dev == Lfe5um5g45f)
                                                    then "X69/Y71/EXTREF"
                                                    else
                                                        if loc == "EXTREF0" && (dev == Lfe5um85f || dev == Lfe5um5g85f)
                                                            then "X46/Y95/EXTREF"
                                                            else
                                                                if loc == "EXTREF1" && (dev == Lfe5um85f || dev == Lfe5um5g85f)
                                                                    then "X71/Y95/EXTREF"
                                                                    else "NONE"
                        _ -> "NONE"
                dcu =
                    case refo of
                        Just n ->
                            let users = activeUsers (netUsers (netOf pkAcc n))
                             in case [u | u <- users, prCell u /= Nothing, cellType (cellOf pkAcc (fromMaybe (error "x") (prCell u))) == cid pkAcc "DCUA"] of
                                    (u : _) -> prCell u
                                    [] -> Nothing
                        Nothing -> Nothing
                dcuBel = case dcu of
                    Just d ->
                        let belStr = strOrDef (cellAttrs (cellOf pkAcc d)) (cid pkAcc "BEL") ""
                         in if T.isSuffixOf "DCU" (T.pack belStr)
                                then T.dropEnd 3 (T.pack belStr) <> "EXTREF"
                                else ""
                    Nothing -> ""
                resolved =
                    if dcuBel /= T.pack locBel
                        then
                            if dcuBel == "" && locBel == "NONE"
                                then Nothing
                                else
                                    if dcuBel == ""
                                        then Just (T.pack locBel)
                                        else
                                            if locBel == "NONE"
                                                then Just dcuBel
                                                else Nothing
                        else
                            if dcuBel == ""
                                then Nothing
                                else Just dcuBel
             in case resolved of
                    Just bel -> setP (cellName ci) (cid pkAcc "BEL") (PropStr bel) pkAcc
                    Nothing -> pkAcc
        | cellType ci == cid pkAcc "PCSCLKDIV" =
            case getPort (cellOf pkAcc (cellName ci)) (cid pkAcc "CLKI") of
                Just n ->
                    let drv = netOf pkAcc n
                     in case prCell (netDriver drv) of
                            Just c
                                | cellType (cellOf pkAcc c) == cid pkAcc "DCUA" ->
                                    let belStr = strOrDef (cellAttrs (cellOf pkAcc c)) (cid pkAcc "BEL") ""
                                        bel = getBelByNameStr pkAcc (T.pack belStr)
                                     in case bel of
                                            Just b ->
                                                let Loc bx _ _ = getBelLocation (pkE pkAcc) b
                                                    z = if bx >= 69 then 1 else 0
                                                 in pkAcc{pkDesign = setCellConstr (cellName ci) 0 0 z True (pkDesign pkAcc)}
                                            Nothing -> pkAcc
                            _ -> pkAcc
                Nothing -> pkAcc
        | otherwise = pkAcc

getBelByNameStr :: Packer -> T.Text -> Maybe BelId
getBelByNameStr pk name =
    let parts = T.splitOn "/" name
     in case parts of
            [x, y, bname] ->
                let xi = readInt (T.unpack (T.drop 1 x))
                    yi = readInt (T.unpack (T.drop 1 y))
                 in getBelByName (pkE pk) [xId, yId, bId]
              where
                xId = fromMaybe emptyId (M.lookup x (tdConstIdByName (ecp5TimingDb (pkE pk))))
                yId = fromMaybe emptyId (M.lookup y (tdConstIdByName (ecp5TimingDb (pkE pk))))
                bId = fromMaybe emptyId (M.lookup bname (tdConstIdByName (ecp5TimingDb (pkE pk))))
            _ -> Nothing
  where
    readInt t = case reads t of
        [(i, "")] -> i
        _ -> 0

packMisc :: Packer -> Packer
packMisc pk =
    let cells = cellsIter (pkDesign pk)
     in foldl miscOne pk cells
  where
    miscOne pkAcc ci
        | cellType ci == cid pkAcc "USRMCLK" =
            movePort pkAcc (cellName ci) (cid pkAcc "USRMCLKI") (cellName ci) (cid pkAcc "PADDO")
                |> \p -> movePort p (cellName ci) (cid p "USRMCLKTS") (cellName ci) (cid p "PADDT")
                |> \p -> movePort p (cellName ci) (cid p "USRMCLKO") (cellName ci) (cid p "PADDI")
        | cellType ci `elem` [cid pkAcc "GSR", cid pkAcc "SGSR"] =
            let isSgsr = cellType ci == cid pkAcc "SGSR"
                pkA = setP (cellName ci) (cid pkAcc "MODE") (PropStr "ACTIVE_LOW") pkAcc
                pkB = setP (cellName ci) (cid pkAcc "SYNCMODE") (PropStr (if isSgsr then "SYNC" else "ASYNC")) pkA
                pkC = pkB{pkDesign = setCellType (cellName ci) (cid pkB "GSR") (pkDesign pkB)}
                pkD =
                    case [b | b <- getBels (pkE pkC), idStr pkC (getBelType (pkE pkC) b) == "GSR"] of
                        (b : _) ->
                            let belName = T.intercalate "/" (map (idToText (ecp5IdTable (pkE pkC))) (getBelName (pkE pkC) b))
                                pkD1 = setP (cellName ci) (cid pkC "BEL") (PropStr belName) pkC
                             in pkD1{pkGsrclkWire = getBelPinWire (pkE pkC) b (cid pkC "CLK")}
                        [] -> pkC
             in pkD
        | otherwise = pkAcc
    (|>) x f = f x

-- ---------------------------------------------------------------------------
-- preplace_plls
-- ---------------------------------------------------------------------------

findEclkUserX :: Packer -> Maybe IdString -> Int
findEclkUserX pk net =
    case net of
        Nothing -> -1
        Just n ->
            let users = activeUsers (netUsers (netOf pk n))
                go [] = -1
                go (u : rest) =
                    case prCell u of
                        Just c ->
                            let ci = cellOf pk c
                             in if cellType ci == cid pk "ECLKSYNCB" && prPort u == cid pk "ECLKI"
                                    then
                                        let r = findEclkUserX pk (getPort ci (cid pk "ECLKO"))
                                         in if r /= -1 then r else go rest
                                    else
                                        if cellType ci `elem` [cid pk "IOLOGIC", cid pk "DQSBUFM"] && prPort u == cid pk "ECLK"
                                            then
                                                let belStr = strOrDef (cellAttrs ci) (cid pk "BEL") ""
                                                 in if belStr == ""
                                                        then go rest
                                                        else
                                                            case getBelByNameStr pk (T.pack belStr) of
                                                                Just b ->
                                                                    let Loc bx _ _ = getBelLocation (pkE pk) b
                                                                     in bx
                                                                Nothing -> go rest
                                            else go rest
                        Nothing -> go rest
             in go users

getPllEclkX :: Packer -> CellInfo BelId WireId PipId -> Int
getPllEclkX pk ci =
    foldl (\acc port ->
        let r = findEclkUserX pk (getPort ci (cid pk port))
         in if r /= -1 then r else acc)
        (-1)
        ["CLKOP", "CLKOS", "CLKOS2", "CLKOS3"]

preplacePlls :: Packer -> Packer
preplacePlls pk = do
    let e = pkE pk
        allBels = getBels e
        available0 = S.fromList [b | b <- allBels, idStr pk (getBelType e b) == "EHXPLLL", checkBelAvail e b]
        fixed = S.fromList [b | b <- allBels, idStr pk (getBelType e b) == "EHXPLLL", not (checkBelAvail e b)]
        cells = cellsIter (pkDesign pk)
        fixedPlls = [b | ci <- cells, cellType ci == cid pk "EHXPLLL", M.member (cid pk "BEL") (cellAttrs ci), b <- maybeToList (getBelByNameStr pk (T.pack (strOrDef (cellAttrs ci) (cid pk "BEL") "")))]
        available = foldl (flip S.delete) available0 fixedPlls
        -- ordered plls: (cell, eclk x)
        ordered = [ (cellName ci, getPllEclkX pk ci)
                  | ci <- cells
                  , cellType ci == cid pk "EHXPLLL"
                  , not (M.member (cid pk "BEL") (cellAttrs ci))
                  ]
        orderedSorted = sortOn (negate . snd) ordered
        -- phase 1: place near fixed drivers
        (pk1, available1) = placeNearDriver pk available orderedSorted
        -- phase 2: ECLK then random
        pk2 = placeRemaining pk1 available1 orderedSorted
     in pk2
  where
    maybeToList Nothing = []
    maybeToList (Just x) = [x]
    placeNearDriver pkAcc avail ordered =
        go pkAcc avail ordered False
      where
        go p avail' [] did = (p, avail')
        go p avail' ((cell, eclkX) : rest) did =
            let drvNet = getPort (cellOf p cell) (cid p "CLKI")
             in case drvNet of
                    Just n ->
                        let drv = netOf p n
                         in case prCell (netDriver drv) of
                                Just c
                                    | M.member (cid p "BEL") (cellAttrs (cellOf p c)) ->
                                        let belStr = strOrDef (cellAttrs (cellOf p c)) (cid p "BEL") ""
                                            drvBel = getBelByNameStr p (T.pack belStr)
                                         in case drvBel of
                                                Just db ->
                                                    let Loc dx dy _ = getBelLocation (pkE p) db
                                                        closest = foldl
                                                            (\ (best, bestD) b ->
                                                                let Loc px py _ = getBelLocation (pkE p) b
                                                                    distance = 2 * abs (dx - px) + abs (dy - py)
                                                                    distance' = if eclkX /= -1 then distance + 20 * abs (eclkX - px) else distance
                                                                 in if distance' < bestD then (Just b, distance') else (best, bestD))
                                                            (Nothing, maxBound :: Int)
                                                            (S.toList avail')
                                                     in case closest of
                                                            (Just b, _) ->
                                                                let p' = p{pkDesign = setCellAttr cell (cid p "BEL") (PropStr (belNameStr p b)) (pkDesign p)}
                                                                 in go p' (S.delete b avail') rest True
                                                            _ -> go p avail' rest did
                                                Nothing -> go p avail' rest did
                                _ -> go p avail' rest did
                    Nothing -> go p avail' rest did
    belNameStr p b =
        let tbl = ecp5IdTable (pkE p)
            ns = getBelName (pkE p) b
            -- force the names (running the lazy bel-name intern) BEFORE
            -- the idToText reads, so a shared PAP snapshot cannot miss
            -- the fresh entry
            -- force each name id (running the lazy bel-name intern)
            -- BEFORE the idToText reads: a shared read snapshot would
            -- otherwise miss the fresh entry
            forced = foldl' (flip seq) () ns
            nm = forced `seq` T.intercalate "/" (map (idToText tbl) ns)
         in nm
    placeRemaining pkAcc avail ordered =
        foldl
            (\p (cell, eclkX) ->
                if M.member (cid p "BEL") (cellAttrs (cellOf p cell))
                    then p
                    else
                        let (b, avail') =
                                if S.null avail
                                    then (error ("failed to place PLL '" ++ show cell ++ "'"), avail)
                                    else
                                        if eclkX == -1
                                            then (S.findMin avail, S.deleteMin avail)
                                            else
                                                let best = foldl
                                                        (\ (best, bestD) b ->
                                                            let Loc px _ _ = getBelLocation (pkE p) b
                                                                distance = abs (eclkX - px)
                                                             in if distance < bestD then (Just b, distance) else (best, bestD))
                                                        (Nothing, maxBound :: Int)
                                                        (S.toList avail)
                                                 in case best of
                                                        (Just bb, _) -> (bb, S.delete bb avail)
                                                        (Nothing, _) -> (S.findMin avail, S.deleteMin avail)
                         in p{pkDesign = setCellAttr cell (cid p "BEL") (PropStr (belNameStr p b)) (pkDesign p)})
            pkAcc
            ordered

-- ---------------------------------------------------------------------------
-- pack_dqsbuf
-- ---------------------------------------------------------------------------

tieZero :: Packer -> IdString -> IdString -> Packer
tieZero pk ci port =
    let pk1 = autoCreateEmptyPort pk ci port
        name = named pk1 ci ("$zero$" <> idStr pk1 port)
        (pk2, zeroCell) = createCell pk1 (cid pk1 "GND") (Just name)
        pk3 = pk2{pkDesign = addNet name (createNet pk2 name) (pkDesign pk2)}
        pk4 = pk3{pkDesign = setCellPort (cellName zeroCell) (cid pk3 "GND") PortOut (pkDesign pk3)}
        pk5 = pk4{pkDesign = connectPort (cellName zeroCell) (cid pk4 "GND") name (pkDesign pk4)}
        pk6 = pk5{pkDesign = connectPort ci port name (pkDesign pk5)}
     in addNew zeroCell pk6

packDqsbuf :: Packer -> Packer
packDqsbuf pk =
    let cells = cellsIter (pkDesign pk)
        pk1 = foldl packOne pk cells
        -- tie zero ports
        pk2 = foldl tieZeros pk1 (cellsIter (pkDesign pk1))
        tieZeros p ci
            | cellType ci == cid p "DQSBUFM" =
                foldl (\p' zport -> if getPort ci (cid p' zport) == Nothing then tieZero p' (cellName ci) (cid p' zport) else p') p
                    ["RDMOVE", "RDDIRECTION", "WRMOVE", "WRDIRECTION", "READ0", "READ1", "READCLKSEL0", "READCLKSEL1", "READCLKSEL2"
                    , "DYNDELAY0", "DYNDELAY1", "DYNDELAY2", "DYNDELAY3", "DYNDELAY4", "DYNDELAY5", "DYNDELAY6", "DYNDELAY7"
                    ]
            | otherwise = p
     in flushCells pk2
  where
    packOne pkAcc ci
        | cellType ci == cid pkAcc "DQSBUFM" =
            let pio = netDrivenBy pkAcc (getPort ci (cid pkAcc "DQSI")) (isTrellisIo pkAcc) (cid pkAcc "O")
                pkA = case pio of
                    Nothing -> pkAcc
                    Just pioCell ->
                        let belStr = strOrDef (cellAttrs (cellOf pkAcc pioCell)) (cid pkAcc "BEL") ""
                         in case getBelByNameStr pkAcc (T.pack belStr) of
                                Just pioBel ->
                                    let Loc px py pz = getBelLocation (pkE pkAcc) pioBel
                                        pioLoc = Loc px py 8
                                        dqsbuf = getBelByLocation (pkE pkAcc) pioLoc
                                     in case dqsbuf of
                                            Just db -> setP (cellName ci) (cid pkAcc "BEL") (PropStr (belNameStr pkAcc db)) pkAcc
                                            Nothing -> pkAcc
                                Nothing -> pkAcc
                -- globals marking
                pkB = foldl markGlobal pkA ["DQSR90", "RDPNTR0", "RDPNTR1", "RDPNTR2", "WRPNTR0", "WRPNTR1", "WRPNTR2", "DQSW270", "DQSW"]
                markGlobal p port =
                    case getPort (cellOf p (cellName ci)) (cid p port) of
                        Just n ->
                            let ni = netOf p n
                             in p{pkDesign = addNet n (ni{netAttrs = M.insert (cid p "ECP5_IS_GLOBAL") (propFromInt 1 32) (netAttrs ni)}) (pkDesign p)}
                        Nothing -> p
             in pkB
        | otherwise = pkAcc
    belNameStr p b =
        let tbl = ecp5IdTable (pkE p)
            ns = getBelName (pkE p) b
            -- force the names (running the lazy bel-name intern) BEFORE
            -- the idToText reads, so a shared PAP snapshot cannot miss
            -- the fresh entry
            -- force each name id (running the lazy bel-name intern)
            -- BEFORE the idToText reads: a shared read snapshot would
            -- otherwise miss the fresh entry
            forced = foldl' (flip seq) () ns
            nm = forced `seq` T.intercalate "/" (map (idToText tbl) ns)
         in nm

-- ---------------------------------------------------------------------------
-- pack_iologic (port of the C++; exercised by DDR/SERDES designs)
-- ---------------------------------------------------------------------------

lookupDelay :: String -> Int
lookupDelay delMode =
    case delMode of
        "USER_DEFINED" -> 0
        "DQS_ALIGNED_X2" -> 6
        "DQS_CMD_CLK" -> 9
        "ECLK_ALIGNED" -> 21
        "ECLK_CENTERED" -> 11
        "ECLKBRIDGE_ALIGNED" -> 39
        "ECLKBRIDGE_CENTERED" -> 29
        "SCLK_ALIGNED" -> 50
        "SCLK_CENTERED" -> 39
        "SCLK_ZEROHOLD" -> 59
        _ -> error ("Unsupported DEL_MODE '" ++ delMode ++ "'")

equalConstant :: Packer -> Maybe IdString -> Maybe IdString -> Bool
equalConstant pk a b =
    case (a, b) of
        (Nothing, Nothing) -> True
        (Nothing, _) -> False
        (_, Nothing) -> False
        (Just na, Just nb) ->
            let da = netDriver (netOf pk na)
                db = netDriver (netOf pk nb)
             in case (prCell da, prCell db) of
                    (Just ca, Just cb) ->
                        let ta = cellType (cellOf pk ca)
                            tb = cellType (cellOf pk cb)
                         in (ta == cid pk "GND" || ta == cid pk "VCC") && ta == tb
                    _ -> prCell da == Nothing && prCell db == Nothing

packIologic :: Packer -> Packer
packIologic pk =
    let -- pass 1: DELAYF/DELAYG
        pk1 = foldl delayPass pk (cellsIter (pkDesign pk))
        -- pass 2: DDR/IDDR/TSH primitives
        pk2 = foldl ddrPass pk1 (cellsIter (pkDesign pk1))
     in flushCells pk2
  where
    getPioBel pkAcc pio curr =
        let belStr = strOrDef (cellAttrs (cellOf pkAcc pio)) (cid pkAcc "BEL") ""
         in case getBelByNameStr pkAcc (T.pack belStr) of
                Just b -> Just b
                Nothing -> error ("IOLOGIC functionality can only be used with pin-constrained PIO (while processing '" ++ T.unpack (idStr pkAcc curr) ++ "')")
    createPioIologic pkAcc pio curr =
        let bel0 = getPioBel pkAcc pio curr
            bel = fromMaybe (error "createPioIologic: no bel") bel0
            Loc bx by z = getBelLocation (pkE pkAcc) bel
            s = by == 0 || by == cdHeight (ecp5Chipdb (pkE pkAcc)) - 1
            iolType = cid pkAcc (if s then "SIOLOGIC" else "IOLOGIC")
            (pkB, iol) = createCell pkAcc iolType (Just (named pkB pio "$IOL"))
            loc' = Loc bx by (z + (if s then 2 else 4))
            iolBel = getBelByLocation (pkE pkB) loc'
            pkC = case iolBel of
                Just b -> setP (cellName iol) (cid pkB "BEL") (PropStr (belNameStr pkB b)) pkB
                Nothing -> pkB
         in (addNew iol pkC, cellName iol)
    belNameStr p b =
        let tbl = ecp5IdTable (pkE p)
            ns = getBelName (pkE p) b
            -- force the names (running the lazy bel-name intern) BEFORE
            -- the idToText reads, so a shared PAP snapshot cannot miss
            -- the fresh entry
            -- force each name id (running the lazy bel-name intern)
            -- BEFORE the idToText reads: a shared read snapshot would
            -- otherwise miss the fresh entry
            forced = foldl' (flip seq) () ns
            nm = forced `seq` T.intercalate "/" (map (idToText tbl) ns)
         in nm
    setIologicSclk pkAcc iol prim port input =
        let sclk = getPort (cellOf pkAcc prim) (cid pkAcc port)
            pkA =
                case sclk of
                    Nothing -> setP iol (cid pkAcc (if input then "CLKIMUX" else "CLKOMUX")) (PropStr "0") pkAcc
                    Just s ->
                        let pk1 = setP iol (cid pkAcc (if input then "CLKIMUX" else "CLKOMUX")) (PropStr "CLK") pkAcc
                            iolClk = getPort (cellOf pkAcc iol) (cid pkAcc "CLK")
                         in case iolClk of
                                Just ic
                                    | ic /= s && not (equalConstant pkAcc (Just ic) (Just s)) ->
                                        error ("IOLOGIC '" ++ show iol ++ "' has conflicting clocks")
                                _ ->
                                    case iolClk of
                                        Nothing -> pk1{pkDesign = connectPort iol (cid pkAcc "CLK") s (pkDesign pk1)}
                                        Just _ -> pk1
         in if M.member (cid pkAcc port) (cellPorts (cellOf pkAcc prim))
                then pkA{pkDesign = disconnectPort prim (cid pkAcc port) (pkDesign pkA)}
                else pkA
    setIologicEclk pkAcc iol prim port =
        let eclk = getPort (cellOf pkAcc prim) (cid pkAcc port)
            pkA = case eclk of
                Nothing -> error (T.unpack (idStr pkAcc (cellType (cellOf pkAcc prim))) ++ " '" ++ show prim ++ "' cannot have disconnected ECLK")
                Just e ->
                    case getPort (cellOf pkAcc iol) (cid pkAcc "ECLK") of
                        Just iolEclk
                            | iolEclk /= e -> error ("IOLOGIC '" ++ show iol ++ "' has conflicting ECLKs")
                        _ ->
                            case getPort (cellOf pkAcc iol) (cid pkAcc "ECLK") of
                                Nothing -> pkAcc{pkDesign = connectPort iol (cid pkAcc "ECLK") e (pkDesign pkAcc)}
                                Just _ -> pkAcc
         in if M.member (cid pkAcc port) (cellPorts (cellOf pkAcc prim))
                then pkA{pkDesign = disconnectPort prim (cid pkAcc port) (pkDesign pkA)}
                else pkA
    setIologicLsr pkAcc iol prim port input =
        let lsr = getPort (cellOf pkAcc prim) (cid pkAcc port)
            pkA =
                case lsr of
                    Nothing -> setP iol (cid pkAcc (if input then "LSRIMUX" else "LSROMUX")) (PropStr "0") pkAcc
                    Just s ->
                        let pk1 = setP iol (cid pkAcc (if input then "LSRIMUX" else "LSROMUX")) (PropStr "LSRMUX") pkAcc
                            iolLsr = getPort (cellOf pkAcc iol) (cid pkAcc "LSR")
                         in case iolLsr of
                                Just il
                                    | il /= s && not (equalConstant pkAcc (Just il) (Just s)) ->
                                        error ("IOLOGIC '" ++ show iol ++ "' has conflicting LSR signals")
                                _ ->
                                    case iolLsr of
                                        Nothing -> pk1{pkDesign = connectPort iol (cid pkAcc "LSR") s (pkDesign pk1)}
                                        Just _ -> pk1
         in if M.member (cid pkAcc port) (cellPorts (cellOf pkAcc prim))
                then pkA{pkDesign = disconnectPort prim (cid pkAcc port) (pkDesign pkA)}
                else pkA
    setIologicMode pkAcc iol mode =
        let ci = cellOf pkAcc iol
            currMode = strOrDef (cellParams ci) (cid pkAcc "MODE") "NONE"
         in if currMode /= "NONE" && mode == "IREG_OREG"
                then pkAcc
                else
                    if (currMode == "IDDRXN" && mode == "ODDRXN") || (currMode == "ODDRXN" && mode == "IDDRXN")
                        then setP iol (cid pkAcc "MODE") (PropStr "ODDRXN") pkAcc
                        else
                            if currMode /= "NONE" && currMode /= "IREG_OREG" && currMode /= mode
                                then error ("IOLOGIC '" ++ show iol ++ "' has conflicting modes '" ++ currMode ++ "' and '" ++ mode ++ "'")
                                else
                                    if cellType ci == cid pkAcc "SIOLOGIC" && mode /= "IREG_OREG" && mode /= "IDDRX1_ODDRX1" && mode /= "NONE"
                                        then error ("IOLOGIC '" ++ show iol ++ "' is set to mode '" ++ mode ++ "', but this is only supported for left and right IO")
                                        else setP iol (cid pkAcc "MODE") (PropStr (T.pack mode)) pkAcc
    processDqsPort pkAcc prim pio iol port =
        let sig = getPort (cellOf pkAcc prim) (cid pkAcc port)
            pkA =
                case sig of
                    Nothing -> error ("Port " ++ T.unpack port ++ " of cell '" ++ show prim ++ "' cannot be disconnected, it must be driven by a DQSBUFM")
                    Just s ->
                        case getPort (cellOf pkAcc iol) (cid pkAcc port) of
                            Just iolSig
                                | iolSig /= s -> error ("IOLOGIC '" ++ show iol ++ "' has conflicting " ++ T.unpack port ++ " signals")
                                | otherwise -> pkAcc{pkDesign = disconnectPort prim (cid pkAcc port) (pkDesign pkAcc)}
                            Nothing ->
                                let drv = netOf pkAcc s
                                 in case prCell (netDriver drv) of
                                        Just dc
                                            | cellType (cellOf pkAcc dc) == cid pkAcc "DQSBUFM" && prPort (netDriver drv) == cid pkAcc port ->
                                                pkAcc{pkDesign = disconnectPort prim (cid pkAcc port) (pkDesign pkAcc)}
                                        _ -> error ("Port " ++ T.unpack port ++ " of cell '" ++ show prim ++ "' must be driven by port " ++ T.unpack port ++ " of a DQSBUFM")
         in pkA
    delayPass pkAcc ci
        | cellType ci `elem` [cid pkAcc "DELAYF", cid pkAcc "DELAYG"] =
            let aNet = getPort ci (cid pkAcc "A")
                zNet = getPort ci (cid pkAcc "Z")
                iPio = netDrivenBy pkAcc aNet (isTrellisIo pkAcc) (cid pkAcc "O")
                oPio = netOnlyDrives pkAcc zNet (isTrellisIo pkAcc) (cid pkAcc "I") True Nothing
                pk1 =
                    case iPio of
                        Just ip
                            | V.length (V.filter (/= Nothing) (netUsers (netOf pkAcc (maybe emptyId id aNet)))) == 1 ->
                                let (pkB, iol) = createPioIologic pkAcc ip (cellName ci)
                                    pkC = setIologicMode pkB iol "IREG_OREG"
                                    drivesIologic = any (\u -> isIologicInputCell pkAcc (cellOf pkAcc (fromMaybe (error "u") (prCell u))) && (prPort u == cid pkAcc "D" || (cellType (cellOf pkAcc (fromMaybe (error "u2") (prCell u))) == cid pkAcc "TRELLIS_FF" && prPort u == cid pkAcc "DI"))) (activeUsers (netUsers (netOf pkAcc (maybe emptyId id zNet))))
                                    pkD =
                                        if drivesIologic
                                            then
                                                let inputNet = aNet
                                                    dlyNet = zNet
                                                    pk1 = pkC{pkDesign = disconnectPort ip (cid pkC "O") (pkDesign pkC)}
                                                    pk2 = pk1{pkDesign = disconnectPort (cellName ci) (cid pk1 "A") (pkDesign pk1)}
                                                    pk3 = pk2{pkDesign = disconnectPort (cellName ci) (cid pk2 "Z") (pkDesign pk2)}
                                                    pk4 = pk3{pkDesign = connectPort ip (cid pk3 "O") (maybe emptyId id dlyNet) (pkDesign pk3)}
                                                    pk5 = pk4{pkDesign = connectPort iol (cid pk4 "INDD") (maybe emptyId id inputNet) (pkDesign pk4)}
                                                    pk6 = pk5{pkDesign = connectPort iol (cid pk5 "DI") (maybe emptyId id inputNet) (pkDesign pk5)}
                                                 in pk6
                                            else
                                                let pk1 = movePort pkC (cellName ci) (cid pkC "A") iol (cid pkC "PADDI")
                                                    pk2 = movePort pk1 (cellName ci) (cid pk1 "Z") iol (cid pk1 "INDD")
                                                 in pk2
                                 in pkD{pkPacked = pkPacked pkD ++ [cellName ci]}
                        _ -> pkAcc
                pk2 =
                    case oPio of
                        Just op ->
                            let (pkB, iol) = createPioIologic pkAcc op (cellName ci)
                                pkC = setP iol (cid pkB "DELAY.OUTDEL") (PropStr "ENABLED") pkB
                                inputNet = aNet
                                dlyNet = zNet
                                drivenByIol =
                                    case inputNet of
                                        Just n ->
                                            let drv = netDriver (netOf pkC n)
                                             in case prCell drv of
                                                    Just c -> isIologicOutputCell pkC (cellOf pkC c) && prPort drv == cid pkC "Q"
                                                    Nothing -> False
                                        Nothing -> False
                                pkD =
                                    if drivenByIol
                                        then
                                            let pk1 = pkC{pkDesign = disconnectPort op (cid pkC "I") (pkDesign pkC)}
                                                pk2 = pk1{pkDesign = disconnectPort (cellName ci) (cid pk1 "A") (pkDesign pk1)}
                                                pk3 = pk2{pkDesign = disconnectPort (cellName ci) (cid pk2 "Z") (pkDesign pk2)}
                                                pk4 = pk3{pkDesign = connectPort op (cid pk3 "I") (maybe emptyId id inputNet) (pkDesign pk3)}
                                                pk5 = maybe pk4 (\dn -> pk4{pkDesign = deleteNet dn (pkDesign pk4)}) dlyNet
                                             in pk5
                                        else
                                            let pk1 = movePort pkC (cellName ci) (cid pkC "A") iol (cid pkC "TXDATA0")
                                                pk2 = movePort pk1 (cellName ci) (cid pk1 "Z") iol (cid pk1 "IOLDO")
                                                pk3 = pk2{pkDesign = setCellPort op (cid pk2 "IOLDO") PortIn (pkDesign pk2)}
                                                pk4 = movePort pk3 op (cid pk3 "I") op (cid pk3 "IOLDO")
                                             in pk4
                             in pkD{pkPacked = pkPacked pkD ++ [cellName ci]}
                        Nothing -> pkAcc
                pk3 =
                    if iPio == Nothing && oPio == Nothing
                        then error (T.unpack (idStr pkAcc (cellType ci)) ++ " '" ++ show (cellName ci) ++ "' must be connected directly to top level input or output")
                        else pk2
                -- DELAY params
                pk4 =
                    case pk3 `iolOf` pkAcc of
                        Just iol ->
                            let delMode = strOrDef (cellParams ci) (cid pkAcc "DEL_MODE") "USER_DEFINED"
                                pkA = setP iol (cid pkAcc "DELAY.DEL_VALUE") (PropNum "" (fromIntegral (lookupDelay delMode))) pk3
                                pkB =
                                    if M.member (cid pkAcc "DEL_VALUE") (cellParams ci)
                                        then
                                            let pv = fromMaybe (PropNum "" 0) (M.lookup (cid pkAcc "DEL_VALUE") (cellParams ci))
                                             in if propIsString pv && "DELAY" `T.isPrefixOf` propAsString pv
                                                    then pkA
                                                    else setP iol (cid pkAcc "DELAY.DEL_VALUE") pv pkA
                                        else pkA
                                pkC =
                                    if M.member (cid pkAcc "LOADN") (cellPorts ci)
                                        then movePort pkB (cellName ci) (cid pkAcc "LOADN") iol (cid pkAcc "LOADN")
                                        else tieZero pkB iol (cid pkB "LOADN")
                                pkD =
                                    if M.member (cid pkAcc "MOVE") (cellPorts ci)
                                        then movePort pkC (cellName ci) (cid pkC "MOVE") iol (cid pkC "MOVE")
                                        else tieZero pkC iol (cid pkC "MOVE")
                                pkE =
                                    if M.member (cid pkAcc "DIRECTION") (cellPorts ci)
                                        then movePort pkD (cellName ci) (cid pkD "DIRECTION") iol (cid pkD "DIRECTION")
                                        else tieZero pkD iol (cid pkD "DIRECTION")
                                pkF =
                                    if M.member (cid pkAcc "CFLAG") (cellPorts ci)
                                        then movePort pkE (cellName ci) (cid pkE "CFLAG") iol (cid pkE "CFLAG")
                                        else pkE
                             in pkF
                        Nothing -> pk3
             in pk4
        | otherwise = pkAcc
    iolOf p orig =
        -- find the iologic created for this cell: scan new cells with "$IOL" suffix matching the pio
        case [c | c <- pkNew p, "$IOL" `T.isSuffixOf` idStr p (cellName c)] of
            (c : _) -> Just (cellName c)
            [] -> Nothing
    ddrPass pkAcc ci = pkAcc -- the DDR primitive pass (see pack.cc); TODO(pack-ddr): ported in a follow-up

-- ---------------------------------------------------------------------------
-- pack_eclk
-- ---------------------------------------------------------------------------

packEclk :: Packer -> Packer
packEclk pk = pk -- TODO(pack-eclk): edge-clock promotion pass (needs make_eclk + routing)

-- ---------------------------------------------------------------------------
-- generate_constraints
-- ---------------------------------------------------------------------------

generateConstraints :: Packer -> Packer
generateConstraints pk =
    let pk0 = pk{pkUserConstrained = S.fromList [n | (n, ni) <- M.toList (designNets (pkDesign pk)), netClkConstrOf ni /= Nothing]}
        changed0 = S.fromList (M.keys (designNets (pkDesign pk)))
        (pk', _) = iterateConstraints pk0 changed0 0
     in pk'
  where
    netClkConstrOf ni = M.lookup (netName ni) (pkClkConstr pk)

    iterateConstraints p changed iter
        | S.null changed || iter >= 5000 = (p, iter)
        | otherwise =
            let changedCells = S.fromList
                    [ c
                    | n <- S.toList changed
                    , u <- activeUsers (netUsers (netOf p n))
                    , prCell u /= Nothing
                    , prPort u `elem` map (cid p) ["CLKI", "ECLKI", "CLK0", "CLK1"]
                    , let c = fromMaybe (error "c") (prCell u)
                    ]
                changedCells' =
                    if iter == 0
                        then
                            let oscCells =
                                    [ c
                                    | (n, ni) <- M.toList (designNets (pkDesign p))
                                    , prPort (netDriver ni) == cid p "OSC"
                                    , let c = prCell (netDriver ni)
                                    , c /= Nothing
                                    , fromMaybe (error "o") c `S.member` changedCells || True
                                    ]
                             in changedCells `S.union` S.fromList [fromMaybe (error "o2") c | c <- [prCell (netDriver ni) | (_, ni) <- M.toList (designNets (pkDesign p)), prPort (netDriver ni) == cid p "OSC"], c /= Nothing]
                        else changedCells
                (p', _) = foldl (stepCell iter) (p, ()) (S.toList changedCells')
             in iterateConstraints p' S.empty (iter + 1)
    stepCell iter (p, _) c =
        let ci = cellOf p c
         in if cellType ci == cid p "CLKDIVF"
                then
                    let div = strOrDef (cellParams ci) (cid p "DIV") "2.0"
                        ratio = if div == "2.0" then 1 / 2.0 else if div == "3.5" then 1 / 3.5 else error ("Unsupported divider ratio '" ++ div ++ "' on CLKDIVF")
                     in (copyConstraint p c "CLKI" "CDIVX" ratio, ())
                else
                    if cellType ci `elem` [cid p "ECLKSYNCB", cid p "TRELLIS_ECLKBUF"]
                        then (copyConstraint p c "ECLKI" "ECLKO" 1, ())
                        else
                            if cellType ci == cid p "ECLKBRIDGECS"
                                then (copyConstraint (copyConstraint p c "CLK0" "ECSOUT" 1) c "CLK1" "ECSOUT" 1, ())
                                else
                                    if cellType ci == cid p "DCCA"
                                        then (copyConstraint p c "CLKI" "CLKO" 1, ())
                                        else
                                            if cellType ci == cid p "DCSC"
                                                then (p, ()) -- TODO(pack-dcsc): merged constraint
                                                else
                                                    if cellType ci == cid p "EHXPLLL"
                                                        then
                                                            case getPort ci (cid p "CLKI") of
                                                                Just clkiNet
                                                                    | M.member clkiNet (pkClkConstr p) ->
                                                                        let periodIn = dpMin (ccPeriod (pkClkConstr p M.! clkiNet))
                                                                            periodInDiv = periodIn * fromIntegral (intOrDef (cellParams ci) (cid p "CLKI_DIV") 1)
                                                                            path = strOrDef (cellParams ci) (cid p "FEEDBK_PATH") "CLKOP"
                                                                            feedbackDiv0 = intOrDef (cellParams ci) (cid p "CLKFB_DIV") 1
                                                                            feedbackDiv =
                                                                                if path `elem` ["CLKOP", "INT_OP"]
                                                                                    then feedbackDiv0 * intOrDef (cellParams ci) (cid p "CLKOP_DIV") 1
                                                                                    else
                                                                                        if path `elem` ["CLKOS", "INT_OS"]
                                                                                            then feedbackDiv0 * intOrDef (cellParams ci) (cid p "CLKOS_DIV") 1
                                                                                            else
                                                                                                if path `elem` ["CLKOS2", "INT_OS2"]
                                                                                                    then feedbackDiv0 * intOrDef (cellParams ci) (cid p "CLKOS2_DIV") 1
                                                                                                    else
                                                                                                        if path `elem` ["CLKOS3", "INT_OS3"]
                                                                                                            then feedbackDiv0 * intOrDef (cellParams ci) (cid p "CLKOS3_DIV") 1
                                                                                                            else 0
                                                                            vcoPeriod = if feedbackDiv == 0 then 0 else periodInDiv `div` fromIntegral feedbackDiv
                                                                            setOut pOut muxPort outPort defDiv =
                                                                                if strOrDef (cellParams ci) (cid p muxPort) defDiv == "REFCLK"
                                                                                    then copyConstraint p c "CLKI" outPort 1
                                                                                    else setConstraint p c outPort (ClockConstraint (DelayPair (vcoPeriod `div` 2) (vcoPeriod `div` 2)) (DelayPair (vcoPeriod `div` 2) (vcoPeriod `div` 2)) (DelayPair vcoPeriod vcoPeriod))
                                                                         in if feedbackDiv == 0
                                                                                then (p, ())
                                                                                else
                                                                                    ( setOut p "OUTDIVIDER_MUXA" "CLKOP" "DIVA"
                                                                                        |> \p1 -> setOut p1 "OUTDIVIDER_MUXB" "CLKOS" "DIVB"
                                                                                        |> \p2 -> setOut p2 "OUTDIVIDER_MUXC" "CLKOS2" "DIVC"
                                                                                        |> \p3 -> setOut p3 "OUTDIVIDER_MUXD" "CLKOS3" "DIVD"
                                                                                    , ()
                                                                                    )
                                                                _ -> (p, ())
                                                        else
                                                            if cellType ci == cid p "OSCG"
                                                                then
                                                                    let divv = intOrDef (cellParams ci) (cid p "DIV") 128
                                                                        period = fromIntegral ((divv * 1000000) `div` (2 * 155)) :: Int64
                                                                        half = period `div` 2
                                                                     in (setConstraint p c "OSC" (ClockConstraint (DelayPair half half) (DelayPair half half) (DelayPair period period)), ())
                                                                else (p, ())
      where
        (|>) x f = f x
    copyConstraint p c fromPort toPort ratio =
        let ci = cellOf p c
         in case (getPort ci (cid p fromPort), getPort ci (cid p toPort)) of
                (Just from, Just to)
                    | M.member from (pkClkConstr p) ->
                        case M.lookup to (pkClkConstr p) of
                            Just _ -> p
                            Nothing ->
                                let cc = pkClkConstr p M.! from
                                    nlow = fromIntegral (floor (fromIntegral (dpMin (ccLow cc)) / ratio) :: Int64)
                                    nhigh = fromIntegral (floor (fromIntegral (dpMin (ccHigh cc)) / ratio) :: Int64)
                                    nperiod = fromIntegral (floor (fromIntegral (dpMin (ccPeriod cc)) / ratio) :: Int64)
                                 in p{pkClkConstr = M.insert to (ClockConstraint (DelayPair nlow nlow) (DelayPair nhigh nhigh) (DelayPair nperiod nperiod)) (pkClkConstr p)}
                _ -> p
    setConstraint p c port cc =
        case getPort (cellOf p c) (cid p port) of
            Nothing -> p
            Just to ->
                case M.lookup to (pkClkConstr p) of
                    Just _ -> p
                    Nothing -> p{pkClkConstr = M.insert to cc (pkClkConstr p)}

-- ---------------------------------------------------------------------------
-- promote_ecp5_globals (globals.cc: get_clocks + insert_dcc)
-- ---------------------------------------------------------------------------

promoteGlobals :: Packer -> Packer
promoteGlobals pk = do
    let pk0 = pk
        disable = boolOrDef (pkSettings pk0) (cid pk0 "arch.no-promote-globals") False
        isOoc = boolOrDef (pkSettings pk0) (cid pk0 "arch.ooc") False
        clocks = getClocks pk0
        (pk1, _) = foldl (promoteClock disable isOoc) (pk0, ()) clocks
        -- DCS inputs
        dcsCells = [ci | ci <- cellsIter (pkDesign pk1), cellType ci == cid pk1 "DCSC"]
        (pk2, _) = foldl dcsDcc (pk1, ()) dcsCells
     in pk2
  where
    getClocks p =
        [ n
        | (n, ni) <- M.toList (designNets (pkDesign p))
        , M.member n (pkClkConstr p)
        ]
    promoteClock disable isOoc (p, _) clockNet =
        let ni = netOf p clockNet
            isNoglobal =
                disable
                    || boolOrDef (netAttrs ni) (cid p "noglobal") False
                    || boolOrDef (netAttrs ni) (cid p "ECP5_IS_GLOBAL") False
         in if isNoglobal
                then (p, ())
                else
                    if isOoc
                        then (p{pkDesign = addNet clockNet (ni{netAttrs = M.insert (cid p "ECP5_IS_GLOBAL") (propFromInt 1 32) (netAttrs ni)}) (pkDesign p)}, ())
                        else (insertDcc p clockNet Nothing, ())
    dcsDcc (p, _) ci =
        (foldl (\p' port -> case getPort ci (cid p' port) of
                    Just n -> insertDcc p' n (Just (cellName ci))
                    Nothing -> p') p ["CLK0", "CLK1"], ())

-- | @insert_dcc@: create a DCCA, a glbnet, rewire users, place the DCC.
insertDcc :: Packer -> IdString -> Maybe IdString -> Packer
insertDcc pk net mdcsCell =
    let ni = netOf pk net
        already =
            case prCell (netDriver ni) of
                Just c -> cellType (cellOf pk c) `elem` [cid pk "DCCA", cid pk "DCSC"]
                Nothing -> False
     in if already
            then markGlobal pk net
            else
                let (pk1, dcc) = createCell pk (cid pk "DCCA") (Just (internT pk ("$gbuf$" <> idStr pk net)))
                    glbName = internT pk1 ("$glbnet$" <> idStr pk1 net)
                    glbNet0 = createNet pk1 glbName
                    glbNet = glbNet0{netDriver = PortRef (Just (cellName dcc)) (cid pk1 "CLKO")}
                    pk2 = pk1{pkDesign = addNet glbName glbNet (pkDesign pk1)}
                    pk3 = pk2{pkDesign = connectPort (cellName dcc) (cid pk2 "CLKO") glbName (pkDesign pk2)}
                    users = activeUsers (netUsers (netOf pk3 net))
                    isLogicPort u =
                        let ci = cellOf pk3 (fromMaybe (error "u") (prCell u))
                            pname = idStr pk3 (prPort u)
                         in cellType ci == cid pk3 "TRELLIS_FF"
                                || pname `elem` ["CLKI", "ECLKI", "CLK0", "CLK1", "DCSOUT", "CLKO", "CDIVX", "ECLKO", "OSC"]
                    (pk4, keepUsers) =
                        foldl
                            (\ (pAcc, keep) u ->
                                case prCell u of
                                    Nothing -> (pAcc, keep)
                                    Just uc ->
                                        let uci = cellOf pAcc uc
                                            keepIt =
                                                (case mdcsCell of
                                                    Just dc -> cellType uci /= cid pAcc "DCSC"
                                                    Nothing -> False)
                                                    || prPort u == cid pAcc "CLKFB"
                                                    || (prPort (netDriver (netOf pAcc net)) == cid pAcc "OSC" && False)
                                                    || (prPort (netDriver (netOf pAcc net)) == cid pAcc "REFCLKO" && cellType uci == cid pAcc "DCUA")
                                                    || isLogicPort u
                                         in if keepIt
                                                then (pAcc, keep ++ [u])
                                                else
                                                    let p1 = pAcc{pkDesign = connectPort uc (prPort u) glbName (pkDesign pAcc)}
                                                        -- remove the old user entry
                                                        p2 = p1{pkDesign = removeNetUser net uc (prPort u) (pkDesign p1)}
                                                     in (p2, keep))
                            (pk3, [])
                            users
                    -- rebuild net users from keep list
                    pk5 = pk4{pkDesign = setNetUsers net (V.fromList keepUsers) (pkDesign pk4)}
                    -- re-add keep users with correct user_idx (connectPort appends)
                    pk6 = foldl (\p u -> p{pkDesign = connectPort (fromMaybe (error "k") (prCell u)) (prPort u) net (pkDesign p)}) pk5 keepUsers
                    pk7 = pk6{pkDesign = setNetUsers net V.empty (pkDesign pk6)}
                    pk8 = foldl (\p u -> p{pkDesign = connectPort (fromMaybe (error "k2") (prCell u)) (prPort u) net (pkDesign p)}) pk7 keepUsers
                    pk9 = pk8{pkDesign = connectPort (cellName dcc) (cid pk8 "CLKI") net (pkDesign pk8)}
                    -- clock constraint copy
                    pk10 =
                        case M.lookup net (pkClkConstr pk8) of
                            Just cc -> pk9{pkClkConstr = M.insert glbName cc (pkClkConstr pk9)}
                            Nothing -> pk9
                    pk11 = addNew dcc pk10
                    pk12 = markGlobal pk11 glbName
                    dccCell = cellOf pk12 (cellName dcc)
                    belSet =
                        if strOrDef (cellAttrs dccCell) (cid pk12 "BEL") "" == ""
                            then placeDccDcs pk12 (cellName dcc)
                            else pk12
                 in belSet
  where
    markGlobal p netName =
        let ni = netOf p netName
         in p{pkDesign = addNet netName (ni{netAttrs = M.insert (cid p "ECP5_IS_GLOBAL") (propFromInt 1 32) (netAttrs ni)}) (pkDesign p)}

-- | @place_dcc_dcs@: try every DCCA bel, pick the best metric.
placeDccDcs :: Packer -> IdString -> Packer
placeDccDcs pk dcc = do
    let e = pkE pk
        ci = cellOf pk dcc
        usingCe = getPort ci (cid pk "CE") /= Nothing
        candidates =
            [ b
            | b <- getBels e
            , idStr pk (getBelType e b) == idStr pk (cellType ci)
            , checkBelAvail e b
            , let belname = belNameOf b
            , not (T.head belname == 'D' && usingCe)
            ]
        best =
            foldl
                (\bestSoFar b ->
                    let metric = dccMetric pk b dcc
                     in case bestSoFar of
                            Nothing -> Just (b, metric)
                            Just (bb, m) -> if metric < m then Just (b, metric) else bestSoFar)
                Nothing
                candidates
     in case best of
            Just (b, _) -> setP dcc (cid pk "BEL") (PropStr (belNameStr b)) pk
            Nothing -> error "place_dcc_dcs: no valid DCC bel"
  where
    belNameOf b = biName (belAt (ecp5Chipdb (pkE pk)) b)
    belNameStr b = T.intercalate "/" (map (idToText (ecp5IdTable (pkE pk))) (getBelName (pkE pk) b))

-- | The DCC placement metric (globals.cc @get_dcc_metric@, simplified:
-- the driver-locked case and the general wirelength case).
dccMetric :: Packer -> BelId -> IdString -> (Int, Bool)
dccMetric pk dccBel dcc =
    let ci = cellOf pk dcc
        drvNet = getPort ci (cid pk "CLKI")
        e = pkE pk
     in case drvNet of
            Nothing -> (9999999, False)
            Just n ->
                let ni = netOf pk n
                    drv = netDriver ni
                 in case prCell drv of
                        Just drvCell
                            | M.member (cid pk "BEL") (cellAttrs (cellOf pk drvCell)) ->
                                let belStr = strOrDef (cellAttrs (cellOf pk drvCell)) (cid pk "BEL") ""
                                 in case getBelByNameStr pk (T.pack belStr) of
                                        Just drvBel ->
                                            let Loc dx dy _ = getBelLocation e drvBel
                                                Loc cx cy _ = getBelLocation e dccBel
                                             in (abs (cx - dx) + abs (cy - dy), False)
                                        Nothing -> (9999999, False)
                        _ ->
                            -- wirelength metric over placed users
                            let Loc dx dy _ = getBelLocation e dccBel
                                (xmin, xmax, ymin, ymax) =
                                    foldl
                                        (\ (x0, x1, y0, y1) u ->
                                            case prCell u of
                                                Just uc
                                                    | M.member (cid pk "BEL") (cellAttrs (cellOf pk uc)) ->
                                                        case getBelByNameStr pk (T.pack (strOrDef (cellAttrs (cellOf pk uc)) (cid pk "BEL") "")) of
                                                            Just ub ->
                                                                let Loc ux uy _ = getBelLocation e ub
                                                                 in (min x0 ux, max x1 ux, min y0 uy, max y1 uy)
                                                            Nothing -> (x0, x1, y0, y1)
                                                _ -> (x0, x1, y0, y1))
                                        (dx, dx, dy, dy)
                                        (activeUsers (netUsers ni))
                             in ((ymax - ymin) + (xmax - xmin), False)

-- ---------------------------------------------------------------------------
-- fixupHierarchy (no-op: our design is already flattened)
-- ---------------------------------------------------------------------------

fixupHierarchy :: Packer -> Packer
fixupHierarchy pk = pk

-- ---------------------------------------------------------------------------
-- The main pack() mirror
-- ---------------------------------------------------------------------------

-- | Per-pass checksum oracle (compare against the patched C++ LPCHK lines).
pkCk :: String -> Packer -> Packer
pkCk tag pk =
    let c = checksum
            (fromIntegral . belIdx)
            (fromIntegral . wireIdx)
            (fromIntegral . pipIdx)
            (pkDesign pk)
        -- force the full message before writing: the checksum computation
        -- triggers the whole pack chain, whose traces would interleave
        -- with a lazily-written string
        m = T.unpack (T.pack ("LPCHK " ++ tag ++ ": 0x" ++ printf "%08x" c))
     in trace m pk

packDesign :: Ecp5 -> Design BelId WireId PipId -> Bool -> M.Map IdString Property -> IO Packer
packDesign e d verbose settings = do
    printLogicUsage e d
    let pk0 = initialPackerWithSettings e d verbose settings
    stop <- lookupEnv "LAMBDAPNR_PACK_STOP"
    let dumpOrd lbl pkk = do
            e <- lookupEnv "LP_DUMP_ORDER"
            when (isJust e) $
                writeFile ("/tmp/lp_order_" ++ lbl ++ ".txt") (unlines [T.unpack (idStr pkk (cellName ci)) | ci <- cellsIter (pkDesign pkk)])
    let pk1 = pkCk "after-load" pk0
    dumpOrd "load" pk1
    -- pass order mirrors nextpnr-0.10's Ecp5Packer::pack() (the binary the
    -- reference artifacts were built with): preplace_plls runs BEFORE
    -- pack_iologic, and 0.10 has no pack_eclk pass.
    pk2 <- packIo pk1
    let pk3 = pkCk "io" pk2
    dumpOrd "io" pk3
    let pk4 = packDqsbuf pk3
        pk5 = pkCk "dqsbuf" pk4
        pk6 = preplacePlls pk5
        pk7 = pkCk "plls" pk6
    dumpOrd "plls" pk7
    let pk8 = packIologic pk7
        pk9 = pkCk "iologic" pk8
        pk10 = packEbr pk9
        pk11 = pkCk "ebr" pk10
    dumpOrd "ebr" pk11
    let pk12 = packDsps pk11
        pk13 = pkCk "dsps" pk12
        pk14 = packDcus pk13
        pk15 = pkCk "dcus" pk14
        pk16 = packMisc pk15
        pk17 = pkCk "misc" pk16
    dumpOrd "misc" pk17
    let pk18 = packConstants pk17
        pk19 = pkCk "constants" pk18
    dumpOrd "constants" pk19
    let pk20 = packDram pk19
        pk21 = pkCk "dram" pk20
    dumpOrd "dram" pk21
    let pk22 = packCarries pk21
        pk23 = pkCk "carries" pk22
    dumpOrd "carries" pk23
    let pk24 = packLuts pk23
        pk25 = pkCk "luts" pk24
    dumpOrd "luts" pk25
    let pk26 = packLut5xs pk25
        pk27 = pkCk "lut5xs" pk26
    dumpOrd "lut5xs" pk27
    let pk28 = packFfs pk27
        pk29 = pkCk "ffs" pk28
        pk30 = generateConstraints pk29
        pk31 = pkCk "constraints" pk30
        pk32 = promoteGlobals pk31
        pk33 = pkCk "globals" pk32
        pk34 = fixupHierarchy pk33
        pk35 = pkCk "fixup" pk34
    let pkRes =
            case stop of
                Just "io" -> pk3
                Just "dqsbuf" -> pk5
                Just "iologic" -> pk9
                Just "plls" -> pk7
                Just "ebr" -> pk11
                Just "dsps" -> pk13
                Just "dcus" -> pk15
                Just "misc" -> pk17
                Just "constants" -> pk19
                Just "dram" -> pk21
                Just "carries" -> pk23
                Just "luts" -> pk25
                Just "lut5xs" -> pk27
                Just "ffs" -> pk29
                Just "constraints" -> pk31
                Just "globals" -> pk33
                _ -> pk35
    evaluate (pkDesign pkRes)
    pure pkRes

-- | @pack_luts@: LUT4 -> TRELLIS_COMB.
packLuts :: Packer -> Packer
packLuts pk =
    let cells = cellsIter (pkDesign pk)
        pk' = foldl lutOne pk cells
     in pk'
  where
    lutOne pkAcc ci
        | isLut pkAcc ci = lutToComb pkAcc (cellName ci)
        | otherwise = pkAcc

-- | @lut_to_comb@ (cells.cc).
lutToComb :: Packer -> IdString -> Packer
lutToComb pk lut =
    let pk1 = pk{pkDesign = setCellType lut (cid pk "TRELLIS_COMB") (pkDesign pk)}
        pk2 = setP lut (cid pk1 "INITVAL") (fromMaybe (PropNum "" 0) (M.lookup (cid pk1 "INIT") (cellParams (cellOf pk1 lut)))) pk1
        pk3 = pk2{pkDesign = delCellParam lut (cid pk2 "INIT") (pkDesign pk2)}
        pk4 = movePort pk3 lut (cid pk3 "Z") lut (cid pk3 "F")
     in pk4

-- | @pack_lut5xs@: PFUMX -> LUT5, L6MUX21 -> LUT6/LUT7.
packLut5xs :: Packer -> Packer
packLut5xs pk = do
    let e = pkE pk
        -- pack LUT5s (PFUMX)
        (pk1, lut5Roots) = foldl packPfumx (pk, M.empty) (cellsIter (pkDesign pk))
        pk2 = flushCells pk1
        -- pack LUT6s
        (pk3, lut6Roots) = foldl packL6 (pk2, M.empty) (cellsIter (pkDesign pk2))
        pk4 = flushCells pk3
        -- pack LUT7s
        (pk5, lut7Roots) = foldl packL7 (pk4, M.empty) (cellsIter (pkDesign pk4))
        -- constraints
        pk6 = foldl (\p (_, (a, b)) -> relConstr p b a (4 `shiftL` 2)) pk5 (M.toList lut7Roots)
        pk7 = foldl (\p (_, (a, b)) -> relConstr p b a (2 `shiftL` 2)) pk6 (M.toList lut6Roots)
        pk8 = foldl (\p (_, (a, b)) -> relConstr p a b (1 `shiftL` 2)) pk7 (M.toList lut5Roots)
     in flushCells pk8
  where
    getComb1FromLut5 p lut5 =
        let f1 = getPort (cellOf p lut5) (cid p "F1")
         in case netDrivenBy p f1 (\_ -> True) (cid p "F") of
                Just c -> c
                Nothing -> error "PFUMX F1 not driven"
    packPfumx (pAcc, roots) ci
        | isPfumx pAcc ci =
            let f0 = getPort ci (cid pAcc "BLUT")
                f1 = getPort ci (cid pAcc "ALUT")
                lut0 = netDrivenBy pAcc f0 (\c -> cellType c == cid pAcc "TRELLIS_COMB") (cid pAcc "F")
                lut1 = netDrivenBy pAcc f1 (\c -> cellType c == cid pAcc "TRELLIS_COMB") (cid pAcc "F")
                p1 = case (lut0, lut1) of
                    (Just l0, Just l1) ->
                        let pkA = pAcc{pkDesign = setCellPort l0 (cid pAcc "F1") PortIn (pkDesign pAcc)}
                            pkB = pkA{pkDesign = setCellPort l0 (cid pAcc "M") PortIn (pkDesign pkA)}
                            pkC = pkB{pkDesign = setCellPort l0 (cid pAcc "OFX") PortOut (pkDesign pkB)}
                            pkD = movePort pkC (cellName ci) (cid pkC "Z") l0 (cid pkC "OFX")
                            pkE = movePort pkD (cellName ci) (cid pkD "ALUT") l0 (cid pkD "F1")
                            pkF = movePort pkE (cellName ci) (cid pkE "C0") l0 (cid pkE "M")
                            pkG = pkF{pkDesign = disconnectPort (cellName ci) (cid pkF "BLUT") (pkDesign pkF)}
                         in (pkG{pkPacked = pkPacked pkG ++ [cellName ci]}, M.insert l0 (l0, l1) roots)
                    _ -> error "PFUMX driven by cell other than a LUT"
             in p1
        | otherwise = (pAcc, roots)
    packL6 (pAcc, roots) ci
        | isL6mux pAcc ci =
            let d0 = getPort ci (cid pAcc "D0")
                d1 = getPort ci (cid pAcc "D1")
                comb0 = netDrivenBy pAcc d0 (\c -> cellType c == cid pAcc "TRELLIS_COMB") (cid pAcc "OFX")
                comb1 = netDrivenBy pAcc d1 (\c -> cellType c == cid pAcc "TRELLIS_COMB") (cid pAcc "OFX")
                p1 =
                    case comb0 of
                        Nothing -> (pAcc, roots)
                        Just c0
                            | M.member c0 roots -> (pAcc, roots)
                            | otherwise ->
                                case comb1 of
                                    Nothing -> (pAcc, roots)
                                    Just c1
                                        | M.member c1 roots -> (pAcc, roots)
                                        | otherwise ->
                                            let c0' = getComb1FromLut5 pAcc c0
                                                c1' = getComb1FromLut5 pAcc c1
                                                pkA = pAcc{pkDesign = setCellPort c1' (cid pAcc "FXA") PortIn (pkDesign pAcc)}
                                                pkB = pkA{pkDesign = setCellPort c1' (cid pAcc "FXB") PortIn (pkDesign pkA)}
                                                pkC = pkB{pkDesign = setCellPort c1' (cid pAcc "M") PortIn (pkDesign pkB)}
                                                pkD = pkC{pkDesign = setCellPort c1' (cid pAcc "OFX") PortOut (pkDesign pkC)}
                                                pkE = movePort pkD (cellName ci) (cid pkD "D0") c1' (cid pkD "FXA")
                                                pkF = movePort pkE (cellName ci) (cid pkE "D1") c1' (cid pkE "FXB")
                                                pkG = movePort pkF (cellName ci) (cid pkF "SD") c1' (cid pkF "M")
                                                pkH = movePort pkG (cellName ci) (cid pkG "Z") c1' (cid pkG "OFX")
                                             in (pkH{pkPacked = pkPacked pkH ++ [cellName ci]}, M.insert c1' (c0', c1') roots)
             in p1
        | otherwise = (pAcc, roots)
    packL7 (pAcc, roots) ci
        | isL6mux pAcc ci =
            let d0 = getPort ci (cid pAcc "D0")
                d1 = getPort ci (cid pAcc "D1")
                comb1 = netDrivenBy pAcc d0 (\c -> cellType c == cid pAcc "TRELLIS_COMB") (cid pAcc "OFX")
                comb3 = netDrivenBy pAcc d1 (\c -> cellType c == cid pAcc "TRELLIS_COMB") (cid pAcc "OFX")
             in case (comb1, comb3) of
                    (Just c1, Just c3) ->
                        let fxa1 = getPort (cellOf pAcc c3) (cid pAcc "FXA")
                            comb2 = netDrivenBy pAcc fxa1 (\c -> cellType c == cid pAcc "TRELLIS_COMB") (cid pAcc "OFX")
                         in case comb2 of
                                Just c2 ->
                                    let c2' = getComb1FromLut5 pAcc c2
                                        pkA = pAcc{pkDesign = setCellPort c2' (cid pAcc "FXA") PortIn (pkDesign pAcc)}
                                        pkB = pkA{pkDesign = setCellPort c2' (cid pAcc "FXB") PortIn (pkDesign pkA)}
                                        pkC = pkB{pkDesign = setCellPort c2' (cid pAcc "M") PortIn (pkDesign pkB)}
                                        pkD = pkC{pkDesign = setCellPort c2' (cid pAcc "OFX") PortOut (pkDesign pkC)}
                                        pkE = movePort pkD (cellName ci) (cid pkD "D0") c2' (cid pkD "FXA")
                                        pkF = movePort pkE (cellName ci) (cid pkE "D1") c2' (cid pkE "FXB")
                                        pkG = movePort pkF (cellName ci) (cid pkF "SD") c2' (cid pkF "M")
                                        pkH = movePort pkG (cellName ci) (cid pkG "Z") c2' (cid pkG "OFX")
                                     in (pkH{pkPacked = pkPacked pkH ++ [cellName ci]}, M.insert c2' (c1, c3) roots)
                                Nothing -> error "SLICE FXA driven by cell other than a SLICE OFX0"
                    _ -> error "L6MUX21 driven by cell other than a SLICE OFX"
        | otherwise = (pAcc, roots)
    relConstr p base rel dz =
        let pkA = p{pkDesign = setCellCluster base (Just base) 0 0 0 True (pkDesign p)}
            pkB = pkA{pkDesign = setCellCluster rel (Just base) 0 0 0 False (pkDesign pkA)}
            pkC = pkB{pkDesign = setCellConstr rel 0 0 dz False (pkDesign pkB)}
            pkD = pkC{pkDesign = setCellChildren base (cellConstrChildren (cellOf pkC base) ++ [rel]) (pkDesign pkC)}
         in pkD

-- | @pack_ffs@.
packFfs :: Packer -> Packer
packFfs pk =
    let cells = cellsIter (pkDesign pk)
        dumpDone = unsafePerformIO $
            lookupEnv "LP_DUMP_FFS_ORDER" >>= \e ->
                when (isJust e) $
                    writeFile "/tmp/lp_ffs_iter.txt" (unlines [T.unpack (idStr pk (cellName ci)) | ci <- cells])
        (pk', pairs) = foldl packOne (pk, 0 :: Int) cells
        _ = info (printf "    %d FFs paired with LUTs." pairs)
     in dumpDone `seq` pk'
  where
    ffZ = belFfZ - belCombZ
    sd0Rename p ci = setP (cellName ci) (cid p "SD") (PropStr "0") (movePort p (cellName ci) (cid p "DI") (cellName ci) (cid p "M"))
    packOne (pAcc, pairs) ci
        | isFf pAcc ci =
            let di = getPort ci (cid pAcc "DI")
                m = getPort ci (cid pAcc "M")
                p0 = (if M.member (cid pAcc "M") (cellPorts ci)
                    && (m == Nothing || prCell (netDriver (netOf pAcc (fromMaybe (error "m") m))) == Nothing)
                    then
                        let p1 = pAcc{pkDesign = disconnectPort (cellName ci) (cid pAcc "M") (pkDesign pAcc)}
                         in p1{pkDesign = addCell (cellName ci) (cellOf p1 (cellName ci)){cellPorts = M.delete (cid pAcc "M") (cellPorts (cellOf p1 (cellName ci)))} (pkDesign p1)}
                    else pAcc)
             in case di of
                    Just diNet ->
                        case prCell (netDriver (netOf p0 diNet)) of
                            Just comb
                                | cellType (cellOf p0 comb) == cid p0 "TRELLIS_COMB"
                                    && prPort (netDriver (netOf p0 diNet)) == cid p0 "F" ->
                                    let p1 = setP (cellName ci) (cid p0 "SD") (PropStr "1") p0
                                     in if cellCluster (cellOf p1 comb) /= emptyId
                                            then
                                                -- macro case: check slice compatibility
                                                if canAddFlipflopToMacro p1 comb (cellName ci)
                                                    then (relConstrCells p1 comb (cellName ci) ffZ, pairs + 1)
                                                    else (sd0Rename p1 ci, pairs)
                                            else
                                                let p2 = addChild comb (cellName ci) p1
                                                    p3 = setCluster comb (cellName ci) p2
                                                    p4 = setConstr (cellName ci) 0 0 1 False p3
                                                 in (p4, pairs + 1)
                            _ -> (sd0Rename p0 ci, pairs)
                    Nothing -> (pAcc, pairs)
        | otherwise = (pAcc, pairs)
      where
        addChild r c p = p{pkDesign = setCellChildren r (cellConstrChildren (cellOf p r) ++ [c]) (pkDesign p)}
        setCluster r c p = p{pkDesign = setCellCluster r (Just r) (cellConstrX (cellOf p r)) (cellConstrY (cellOf p r)) (cellConstrZ (cellOf p r)) (cellConstrAbsZ (cellOf p r)) (pkDesign p)}
            & \p' -> p'{pkDesign = setCellCluster c (Just r) 0 0 1 False (pkDesign p')}
        setConstr c cx cy cz absz p = p{pkDesign = setCellConstr c cx cy cz absz (pkDesign p)}
        (&) x f = f x

-- | The macro z-position of a cell (@get_macro_cell_z@).
getMacroCellZ :: Packer -> IdString -> Int
getMacroCellZ pk ci =
    let c = cellOf pk ci
     in if cellConstrAbsZ c
            then cellConstrZ c
            else
                if cellCluster c /= emptyId && cellCluster c /= ci
                    then cellConstrZ c + getMacroCellZ pk (cellCluster c)
                    else 0

-- | The macro xy-position of a cell (@get_macro_cell_xy@).
getMacroCellXy :: Packer -> IdString -> (Int, Int)
getMacroCellXy pk ci =
    let c = cellOf pk ci
     in if cellCluster c /= emptyId then (cellConstrX c, cellConstrY c) else (0, 0)

-- | @rel_constr_cells@ (all four branches).
relConstrCells :: Packer -> IdString -> IdString -> Int -> Packer
relConstrCells pk a b dz =
    let ca = cellOf pk a
        cb = cellOf pk b
        aNotRoot = cellCluster ca /= emptyId && cellCluster ca /= a
        bNotRoot = cellCluster cb /= emptyId && cellCluster cb /= b
        res
            | aNotRoot =
                let root = cellCluster ca
                    d1 = setCellChildren root (cellConstrChildren (cellOf pk root) ++ [b]) (pkDesign pk)
                    d2 = setCellCluster b (Just root) (cellConstrX ca) (cellConstrY ca) (getMacroCellZ pk a + dz) (cellConstrAbsZ ca) d1
                 in pk{pkDesign = d2}
            | bNotRoot =
                let root = cellCluster cb
                    d1 = setCellChildren root (cellConstrChildren (cellOf pk root) ++ [a]) (pkDesign pk)
                    d2 = setCellCluster a (Just root) (cellConstrX cb) (cellConstrY cb) (getMacroCellZ pk b - dz) (cellConstrAbsZ cb) d1
                 in pk{pkDesign = d2}
            | not (null (cellConstrChildren cb)) =
                let d1 = setCellChildren b (cellConstrChildren cb ++ [a]) (pkDesign pk)
                    d2 = setCellCluster a (Just (cellCluster cb)) 0 0 (getMacroCellZ pk b - dz) (cellConstrAbsZ cb) d1
                 in pk{pkDesign = d2}
            | otherwise =
                let d1 = setCellChildren a (cellConstrChildren ca ++ [b]) (pkDesign pk)
                    d2 = setCellCluster a (Just a) (cellConstrX ca) (cellConstrY ca) (cellConstrZ ca) (cellConstrAbsZ ca) d1
                    d3 = setCellCluster b (Just a) 0 0 (getMacroCellZ pk a + dz) (cellConstrAbsZ ca) d2
                 in pk{pkDesign = d3}
     in res

-- | @can_add_flipflop_to_macro@: is it legal to add an FF to a macro?
canAddFlipflopToMacro :: Packer -> IdString -> IdString -> Bool
canAddFlipflopToMacro pk comb ff =
    trace ("CANADD " ++ T.unpack (idStr pk comb)) $
    let _a = trace "CANADD ai-eval" (assignArchInfo (\t -> M.lookup t (tdConstIdByName (ecp5TimingDb (pkE pk)))) (pkDesign pk))
        _b = trace "CANADD ai-forced" (length (show _a) `seq` _a)
        ai = _b
        cells0 = V.replicate 32 Nothing
        rootCell = case cellCluster (cellOf pk comb) of
            cl | cl /= emptyId -> cellOf pk cl
            _ -> cellOf pk comb
        processCell ci cs =
            trace ("CANADD proc " ++ T.unpack (idStr pk (cellName ci))) $
            if getMacroCellXy pk (cellName ci) /= getMacroCellXy pk comb
                then cs
                else let z = trace ("CANADD z " ++ show (getMacroCellZ pk (cellName ci))) (getMacroCellZ pk (cellName ci)) in cs V.// [(z, Just ci)]
        cells1 =
            trace "CANADD cells1-start" $
            if cellCluster (cellOf pk comb) /= emptyId
                then let r = foldl (\cs ch -> processCell (cellOf pk ch) cs) (processCell rootCell cells0) (cellConstrChildren rootCell) in trace ("CANADD cells1-cl nch=" ++ show (length (cellConstrChildren rootCell))) r
                else let r = foldl (\cs ch -> processCell (cellOf pk ch) cs) (processCell (cellOf pk comb) cells0) (cellConstrChildren (cellOf pk comb)) in trace "CANADD cells1-nocl" r
        ffZ = trace ("CANADD ffz " ++ show (getMacroCellZ pk comb + (belFfZ - belCombZ))) (getMacroCellZ pk comb + (belFfZ - belCombZ))
        ffCell = trace "CANADD ffcell-x" (cellOf pk ff)
        idxRes = trace "CANADD idx-done" (cells1 V.! ffZ)
     in case trace "CANADD pre-case" idxRes of
            Just _ -> trace "CANADD slot-occupied" False
            Nothing -> unsafePerformIO $
                let cells2 = cells1 V.// [(ffZ, Just ffCell)]
                    slotCell z = trace ("CANADD slot " ++ show z) $ case cells2 V.! z of
                        Just c -> Just (c, ai)
                        Nothing -> Nothing
                    step :: String -> IO () -> IO ()
                    step lbl act =
                        E.catch
                            (act >> hPutStrLn stderr ("CANADD OK " ++ lbl))
                            ( \e -> do
                                hPutStrLn stderr ("CANADD LOOP-IN " ++ lbl ++ ": " ++ show (e :: E.SomeException))
                                E.throwIO e
                            )
                 in do
                    _ <- step "cells2-spine" (evaluate (V.length cells2) >> pure ())
                    _ <- step "cells2-elems" (evaluate (foldr (\e a -> a + maybe 0 (const 1) e) 0 cells2) >> pure ())
                    _ <- step "cells2-names" (evaluate (length [T.length (idStr pk nm) | Just c <- V.toList cells2, let nm = cellName c]) >> pure ())
                    _ <- step "cells1-names" (evaluate (length [T.length (idStr pk nm) | Just c <- V.toList cells1, let nm = cellName c]) >> pure ())
                    _ <- step "ffcell" (evaluate (T.length (idStr pk (cellName ffCell))) >> pure ())
                    _ <- step "ai" (evaluate (length (show ai)) >> pure ())
                    _ <- step "slices" (evaluate (slicesCompatible slotCell) >> pure ())
                    pure (slicesCompatible slotCell)

-- | The live users of a net in slot order (@users@ iteration).
activeUsers :: V.Vector (Maybe PortRef) -> [PortRef]
activeUsers = foldr (\u acc -> maybe acc (: acc) u) [] . V.toList
