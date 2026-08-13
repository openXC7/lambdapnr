{-# LANGUAGE OverloadedStrings #-}

{- | The yosys JSON netlist frontend — the Haskell mirror of
@frontend\/json_frontend.cc@ + @frontend\/frontend_base.h@
(@GenericFrontend@), for the flat single-module case.

Import pipeline (matching the C++):

1. find the top module: the @frontend\/top@ override, else the module
   with the @top@ attribute, else autodetection (a non-box module no
   other module instantiates);
2. @netnames@: create a net per signal bit, named by the preferred
   label (@get_bit_name@: plain name for width 1, @name[i]@ for buses
   with offset\/upto; port names win, then fewer @$@, then fewer
   @.@, ties alphabetical) or @$frontend$<idx>@;
3. cells: skip @$scopeinfo@\/@$print@\/@$check@; create a cell per
   instance with per-bit ports (@A[0]@...), parameters and attributes
   via @Property::from_string@; connections resolve signal bits to
   nets (merging same-bit connections), @x@ bits are left
   unconnected, string bits create constant nets (@<cell>.<port>$const@
   with the constant value); output ports drive the net (a second
   driver is an error), other ports are appended as users;
4. top-level ports: create the port nets and the
   @$nextpnr_ibuf@\/@obuf@\/@iobuf@ cells (inout ports bifurcated,
   like @split_io@).

Deviations from the C++ (documented): no hierarchy flattening (leaf
cells only), and the top-level port direction is not recorded on the
design (the Netlist model has no port table yet).
-}
module Lambdapnr.Kernel.JsonFrontend (
    loadJsonDesign,
) where

import Control.Monad (foldM)
import qualified Data.IntMap.Strict as IM
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Lambdapnr.Kernel.IdString (IdString, IdTable, emptyId, intern)
import Lambdapnr.Kernel.Json
import Lambdapnr.Kernel.Netlist
import Lambdapnr.Kernel.Property (Property, propFromStr)

-- | Import state: net per bit index + candidate names per bit.
data ImportState = ImportState
    { stNetByBit :: !(IM.IntMap IdString)
    -- ^ module bit index -> net name (created nets)
    , stNetNames :: !(IM.IntMap [Text])
    -- ^ module bit index -> candidate names from netnames/ports
    , stPortNames :: ![Text]
    -- ^ top-level port names (they win the label preference)
    , stUsedCells :: !(M.Map IdString ())
    -- ^ cell names in use (unique_name for cells)
    , stUsedNets :: !(M.Map IdString ())
    -- ^ net names in use (unique_name for nets; the C++ keeps the
    -- @ctx->cells@ and @ctx->nets@ namespaces separate)
    }

emptyState :: ImportState
emptyState = ImportState IM.empty IM.empty [] M.empty M.empty

-- | The import result type (IO for interning, Either for errors).
type Import a = IO (Either String a)

-- | Load a yosys netlist document. The optional override selects the
-- top module by name (the @frontend\/top@ setting); @Nothing@ uses the
-- @top@ attribute, then autodetection.
loadJsonDesign :: IdTable -> Maybe Text -> Text -> Import (Design bel wire pip)
loadJsonDesign tbl topOverride src = do
    case parseJson src of
        Left err -> pure (Left ("JSON parse error: " ++ err))
        Right root -> do
            let mods = case objLookup "modules" root of
                    Just (JObj ms) -> ms
                    _ -> []
            case selectTop topOverride mods of
                Left err -> pure (Left err)
                Right topName -> do
                    let topMod = maybe (JObj []) id (lookup topName mods)
                        st0 = emptyState{stPortNames = map fst (portsOf topMod)}
                        st1 = foldl (addNetnameNames tbl) st0 (netnamesOf topMod)
                        st2 = foldl (addPortNames tbl) st1 (portsOf topMod)
                    r <- createNamedNets tbl st2
                    case r of
                        Left err -> pure (Left err)
                        Right (d1, st3) -> do
                            r2 <- foldM (importCell tbl) (Right (d1, st3)) (cellsOf topMod)
                            case r2 of
                                Left err -> pure (Left err)
                                Right (d2, st4) -> do
                                    r3 <- foldM (importPort tbl) (Right (d2, st4)) (portsOf topMod)
                                    pure (fmap fst r3)

-- | Select the top module: override, then the @top@ attribute, then
-- autodetection (mirrors @find_top_module@).
selectTop :: Maybe Text -> [(Text, Json)] -> Either String Text
selectTop (Just t) mods
    | any ((== t) . fst) mods = Right t
    | otherwise = Left ("Top module '" ++ T.unpack t ++ "' not found!")
selectTop Nothing mods =
    case [n | (n, m) <- mods, attrTop m] of
        [t] -> Right t
        [] -> autodetect
        t : _ -> Left ("Found multiple modules with (* top *) set (including " ++ T.unpack t ++ ")")
  where
    autodetect =
        let candidates = [(n, m) | (n, m) <- mods, not (isBox m)]
            instantiated = [ct | (_, m) <- mods, (_, c) <- cellsOf m, let ct = cellTypeOf c, ct /= "$scopeinfo"]
            tops = [n | (n, _) <- candidates, n `notElem` instantiated]
         in case tops of
                [t] -> Right t
                [] -> Left "Failed to autodetect top module, please specify using --top."
                ts -> Left ("Failed to autodetect top module, please specify using --top (candidates: " ++ T.unpack (T.intercalate ", " ts) ++ ")")
    -- the C++ parses the attribute via Property::from_string and tests
    -- @intval != 0@; yosys writes it as a 32-bit bit string
    attrTop m = case objLookup "attributes" m >>= objLookup "top" of
        Just (JStr s) | not (T.null s), T.all (`elem` ("01" :: String)) s -> T.any (== '1') s
        _ -> False
    isBox m =
        let attrs = objLookupDef (JObj []) "attributes" m
         in objLookup "blackbox" attrs == Just (JStr "1") || objLookup "whitebox" attrs == Just (JStr "1")

-- sections ---------------------------------------------------------------

netnamesOf :: Json -> [(Text, Json)]
netnamesOf mod = pairsOf "netnames" mod

portsOf :: Json -> [(Text, Json)]
portsOf mod = pairsOf "ports" mod

cellsOf :: Json -> [(Text, Json)]
cellsOf mod = pairsOf "cells" mod

pairsOf :: Text -> Json -> [(Text, Json)]
pairsOf key mod = case objLookup key mod of
    Just (JObj kv) -> kv
    _ -> []

cellTypeOf :: Json -> Text
cellTypeOf = strValue . objLookupDef (JStr "") "type"

-- names ------------------------------------------------------------------

-- | @get_bit_name@ from frontend_base.h.
getBitName :: Text -> Int -> Int -> Int -> Bool -> Text
getBitName base index length' offset upto
    | length' == 1 && offset == 0 = base
    | otherwise = base <> "[" <> T.pack (show realIndex) <> "]"
  where
    realIndex
        | upto = offset + length' - index - 1
        | otherwise = offset + index

-- | Register the candidate names of one netname's bits.
addNetnameNames :: IdTable -> ImportState -> (Text, Json) -> ImportState
addNetnameNames _ st (base, nn) = foldl step st [0 .. width - 1]
  where
    bits = arrItems (objLookupDef (JArr []) "bits" nn)
    width = length bits
    offset = maybe 0 fromInteger (objLookup "offset" nn >>= intOf)
    upto = objLookup "upto" nn == Just (JBool True)
    step st' i = case bits !! i of
        JInt b | b >= 0 ->
            let idx = fromInteger b
                nm = getBitName base i width offset upto
             in st'{stNetNames = IM.insertWith (++) idx [nm] (stNetNames st')}
        _ -> st'

-- | Register top-port names (they win the label preference).
addPortNames :: IdTable -> ImportState -> (Text, Json) -> ImportState
addPortNames = addNetnameNames

-- | The preferred label for a bit: port names first, then fewer $,
-- then fewer ., then alphabetical (mirrors @prefer_netlabel@).
preferLabel :: [Text] -> [Text] -> Text -> Text
preferLabel _ _ "" = ""
preferLabel ports _ b
    | b `elem` ports = b
preferLabel ports names _ = foldl1 pick (sortOn rank names)
  where
    pick a b = if rank a <= rank b then a else b
    rank nm = (T.count "$" nm, T.count "." nm, nm)

-- | Create the named nets (one per bit with candidates).
createNamedNets :: IdTable -> ImportState -> Import (Design bel wire pip, ImportState)
createNamedNets tbl st = foldM step (Right (emptyDesign, st)) (IM.toList (stNetNames st))
  where
    step (Right (d, st')) (idx, names) =
        if IM.member idx (stNetByBit st')
            then pure (Right (d, st'))
            else do
                let nm = preferLabel (stPortNames st') names ("$frontend$" <> T.pack (show idx))
                netId <- intern tbl nm
                pure (Right (addNet netId (freshNet netId) d, st'{stNetByBit = IM.insert idx netId (stNetByBit st'), stUsedNets = M.insert netId () (stUsedNets st')}))
    step (Left err) _ = pure (Left err)

-- | A fresh, unconnected net.
freshNet :: IdString -> NetInfo bel wire pip
freshNet name =
    NetInfo
        { netName = name
        , netHierpath = emptyId
        , netDriver = PortRef Nothing emptyId
        , netUsers = V.empty
        , netWires = M.empty
        , netAttrs = M.empty
        , netConstantValue = emptyId
        }

-- | Look up (creating if needed) the net for a module bit index.
getOrCreateNet :: IdTable -> ImportState -> Int -> Import (Design bel wire pip, ImportState, IdString)
getOrCreateNet tbl st idx =
    case IM.lookup idx (stNetByBit st) of
        Just netId -> pure (Right (emptyDesign, st, netId))
        Nothing -> do
            let nm = case IM.lookup idx (stNetNames st) of
                    Just names -> preferLabel (stPortNames st) names ("$frontend$" <> T.pack (show idx))
                    Nothing -> "$frontend$" <> T.pack (show idx)
            netId <- intern tbl nm
            pure (Right (addNet netId (freshNet netId) emptyDesign, st{stNetByBit = IM.insert idx netId (stNetByBit st), stUsedNets = M.insert netId () (stUsedNets st)}, netId))

-- cells ------------------------------------------------------------------

-- | Import one leaf cell (mirrors @import_leaf_cell@).
importCell :: IdTable -> Either String (Design bel wire pip, ImportState) -> (Text, Json) -> Import (Design bel wire pip, ImportState)
importCell tbl acc (name, cd) = do
    case acc of
        Left err -> pure (Left err)
        Right (d, st) -> do
            let ctype = cellTypeOf cd
            if ctype `elem` ["$scopeinfo", "$print", "$check"]
                then pure (Right (d, st))
                else do
                    attrs <- attrsOf tbl cd
                    params <- paramsOf tbl cd
                    cellId <- uniqueCell tbl (stUsedCells st) (T.unpack name)
                    typeId <- intern tbl ctype
                    let dirs = [(p, dirOf v) | (p, v) <- pairsOf "port_directions" cd]
                        conns = [(p, arrItems v) | (p, v) <- pairsOf "connections" cd]
                        cell0 =
                                CellInfo
                                    { cellName = cellId
                                    , cellType = typeId
                                    , cellHierpath = emptyId
                                    , cellPorts = M.empty
                                    , cellAttrs = attrs
                                    , cellParams = params
                                    , cellBel = Nothing
                                    , cellBelStrength = StrengthNone
                                    }
                        st' = st{stUsedCells = M.insert cellId () (stUsedCells st)}
                        d1 = addCell cellId cell0 d
                    r <- foldM (importConn tbl dirs name cellId) (Right (d1, st')) conns
                    pure r

dirOf :: Json -> PortDir
dirOf v = case strValue v of
    "input" -> PortIn
    "output" -> PortOut
    _ -> PortInout

-- | The cell attributes (values via from_string, like the C++
-- @Property::from_string@).
attrsOf :: IdTable -> Json -> IO (Map IdString Property)
attrsOf tbl cd = do
    kvs <- mapM (\(k, v) -> (,) <$> intern tbl k <*> pure (propFromStr (strValue v))) (pairsOf "attributes" cd)
    pure (M.fromList kvs)

-- | The cell parameters (values via from_string).
paramsOf :: IdTable -> Json -> IO (Map IdString Property)
paramsOf tbl cd = do
    kvs <- mapM (\(k, v) -> (,) <$> intern tbl k <*> pure (propFromStr (strValue v))) (pairsOf "parameters" cd)
    pure (M.fromList kvs)

-- | Import one port connection of a cell.
importConn :: IdTable -> [(Text, PortDir)] -> Text -> IdString -> Either String (Design bel wire pip, ImportState) -> (Text, [Json]) -> Import (Design bel wire pip, ImportState)
importConn tbl dirs cellName cellId acc (port, bits) = do
    case acc of
        Left err -> pure (Left err)
        Right (d, st) ->
            case lookup port dirs of
                Nothing -> pure (Right (d, st))
                Just dir ->
                    let width = length bits
                     in foldM (importBit tbl cellName cellId port dir width) (Right (d, st)) (zip [0 ..] bits)

-- | Import one bit of a cell port.
importBit :: IdTable -> Text -> IdString -> Text -> PortDir -> Int -> Either String (Design bel wire pip, ImportState) -> (Int, Json) -> Import (Design bel wire pip, ImportState)
importBit tbl cellName cellId port dir width acc (i, bit) = do
    case acc of
        Left err -> pure (Left err)
        Right (d, st) -> case bit of
            JStr "x" -> pure (Right (d, st)) -- undef: leave unconnected
            JInt b | b >= 0 -> do
                let idx = fromInteger b
                r <- getOrCreateNet tbl st idx
                case r of
                    Left err -> pure (Left err)
                    Right (dN, st', netId) -> do
                        portId <- intern tbl (getBitName port i width 0 False)
                        let dP = setCellPort cellId portId dir (dN <> d)
                        -- multiple drivers are an error, like the C++
                        case dir of
                            PortOut
                                | Just ni <- lookupNet netId dP
                                , prCell (netDriver ni) /= Nothing ->
                                    pure (Left ("Net is multiply driven by cell ports " ++ show (prCell (netDriver ni)) ++ "." ++ show (prPort (netDriver ni)) ++ " and " ++ show cellId ++ "." ++ show portId))
                                | otherwise -> pure (Right (connectPort cellId portId netId dP, st'))
                            _ -> pure (Right (connectPort cellId portId netId dP, st'))
            _ -> do
                -- constant bit: a constant net named <cell>.<port>$const
                let constVal = strValue bit
                constId <- intern tbl constVal
                netId <- uniqueNet tbl (stUsedNets st) (T.unpack cellName <> "." <> T.unpack port <> "$const")
                portId <- intern tbl (getBitName port i width 0 False)
                let constNet = (freshNet netId){netConstantValue = constId}
                    st' = st{stUsedNets = M.insert netId () (stUsedNets st)}
                    dP = setCellPort cellId portId dir (addNet netId constNet d)
                pure (Right (connectPort cellId portId netId dP, st'))

-- | Is this bit a signal (a non-negative integer)?
isSignalBit :: Json -> Bool
isSignalBit (JInt i) = i >= 0
isSignalBit _ = False

intOf :: Json -> Maybe Integer
intOf (JInt i) = Just i
intOf _ = Nothing

-- top ports --------------------------------------------------------------

-- | Import one top-level port: the port net + the iobuf cell
-- (mirrors @import_toplevel_ports@ + @create_iobuf@ with split_io).
importPort :: IdTable -> Either String (Design bel wire pip, ImportState) -> (Text, Json) -> Import (Design bel wire pip, ImportState)
importPort tbl acc (pname, port) = do
    case acc of
        Left err -> pure (Left err)
        Right (d, st) -> do
            let dir = dirOf (objLookupDef (JStr "inout") "direction" port)
                bits = arrItems (objLookupDef (JArr []) "bits" port)
                width = length bits
            foldM (importPortBit tbl pname dir width) (Right (d, st)) (zip [0 ..] bits)

importPortBit :: IdTable -> Text -> PortDir -> Int -> Either String (Design bel wire pip, ImportState) -> (Int, Json) -> Import (Design bel wire pip, ImportState)
importPortBit tbl pname dir width acc (i, bit) = do
    case acc of
        Left err -> pure (Left err)
        Right (d, st) -> case bit of
            JInt b | b >= 0 -> do
                let idx = fromInteger b
                r <- getOrCreateNet tbl st idx
                case r of
                    Left err -> pure (Left err)
                    Right (dN, st', netId) -> do
                        let pbit = getBitName pname i width 0 False
                        iobufId <- uniqueCell tbl (stUsedCells st') (T.unpack pbit)
                        iobufType <- intern tbl (case dir of
                            PortIn -> "$nextpnr_ibuf"
                            PortOut -> "$nextpnr_obuf"
                            PortInout -> "$nextpnr_iobuf")
                        let cell0 =
                                CellInfo
                                    { cellName = iobufId
                                    , cellType = iobufType
                                    , cellHierpath = emptyId
                                    , cellPorts = M.empty
                                    , cellAttrs = M.empty
                                    , cellParams = M.empty
                                    , cellBel = Nothing
                                    , cellBelStrength = StrengthNone
                                    }
                            st1 = st'{stUsedCells = M.insert iobufId () (stUsedCells st')}
                            d1 = addCell iobufId cell0 (dN <> d)
                        case dir of
                            PortIn -> do
                                -- the ibuf output drives the port net
                                portId <- intern tbl "O"
                                let d2 = setCellPort iobufId portId PortOut d1
                                pure (Right (connectPort iobufId portId netId d2, st1))
                            PortOut -> do
                                -- the port net drives the obuf input
                                portId <- intern tbl "I"
                                let d2 = setCellPort iobufId portId PortIn d1
                                pure (Right (connectPort iobufId portId netId d2, st1))
                            PortInout -> do
                                -- bifurcate: split net for the driver side, the
                                -- iobuf O drives the original net
                                splitNetId <- uniqueNet tbl (stUsedNets st1) ("$" <> T.unpack pname <> "$iobuf_i")
                                portI <- intern tbl "I"
                                portO <- intern tbl "O"
                                let st2 = st1{stUsedNets = M.insert splitNetId () (stUsedNets st1)}
                                    d2 = addNet splitNetId (freshNet splitNetId) d1
                                (d3, st3) <-
                                    case lookupNet netId d2 of
                                        Just ni
                                            | Just drvCell <- prCell (netDriver ni) ->
                                                -- move the driver to the split net
                                                let drvPort = prPort (netDriver ni)
                                                    dA = disconnectPort drvCell drvPort d2
                                                    dB = connectPort drvCell drvPort splitNetId dA
                                                 in pure (dB, st2)
                                        _ -> pure (d2, st2)
                                let d3b = setCellPort iobufId portI PortIn d3
                                    d4 = connectPort iobufId portI splitNetId d3b
                                    d4b = setCellPort iobufId portO PortOut d4
                                    d5 = connectPort iobufId portO netId d4b
                                pure (Right (d5, st3))
            _ -> pure (Right (d, st))

-- unique names -----------------------------------------------------------

-- | A cell name not yet used (appends __unique__N on collision).
uniqueCell :: IdTable -> Map IdString () -> String -> IO IdString
uniqueCell tbl used base = go 0
  where
    go n = do
        let nm = if n == 0 then T.pack base else T.pack (base ++ "__unique__" ++ show n)
        i <- intern tbl nm
        if M.member i used then go (n + 1) else pure i

-- | A net name not yet used (nets live in a separate namespace).
uniqueNet :: IdTable -> Map IdString () -> String -> IO IdString
uniqueNet = uniqueCell
