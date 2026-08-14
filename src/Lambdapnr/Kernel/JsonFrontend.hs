{-# LANGUAGE OverloadedStrings #-}
{- | The yosys JSON netlist frontend — the Haskell mirror of
@frontend\/json_frontend.cc@ + @frontend\/frontend_base.h@.

Two structural facts make this a faithful port rather than a
reimplementation:

1. json11 stores JSON objects as @std::map<std::string, Json>@, so the
   C++ iterates every object (modules, cells, netnames, ports,
   attributes, parameters, port directions, connections) in *sorted*
   key order — not document order. Id interning order (which the
   checksum hashes) therefore follows sorted order, and so does this
   port.
2. Nets are created *lazily*: the netnames section only records
   candidate names; a @NetInfo@ comes into existence the first time a
   cell port or top-level port references its bit index. Nets never
   referenced by anything are dropped. Constant bits get a net named
   @<cell>.<port>$const@ plus a VCC/GND driver cell named
   @<net>$VCC$<n>@\/@<net>$GND$<n>@ (@add_constant_driver@).
-}
module Lambdapnr.Kernel.JsonFrontend (
    loadJsonDesign,
    parseJson,
    Json (..),
) where

import Control.Monad (foldM, forM_, when)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', sort, sortOn)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word32)

import Lambdapnr.Kernel.IdString (IdString, IdTable, emptyId, idToText, intern)
import Lambdapnr.Kernel.Netlist
import Lambdapnr.Kernel.Property (Property, propFromStr)

-- | @parseJson@ from json_frontend.cc (COMMENTS enabled).
parseJson :: Text -> Either String Json
parseJson src = jsonParse (T.unpack src)

-- json11 minimal model ----------------------------------------------------

data Json
    = JObj [(Text, Json)]
    | JArr [Json]
    | JStr Text
    | JNum Double
    | JBool Bool
    | JNull
    deriving (Eq, Show)

jsonParse :: String -> Either String Json
jsonParse s = do
    (v, rest) <- pValue (dropWhile isSpace s)
    if null (dropWhile isSpace rest)
        then Right v
        else Left "trailing characters in JSON input"
  where
    isSpace c = c `elem` (" \t\r\n" :: String)
    pValue s0 = case s0 of
        '{' : s1 -> pObj s1
        '[' : s1 -> pArr s1
        '"' : s1 -> do
            (t, s2) <- pStr ('"' : s1)
            pure (JStr t, s2)
        't' : s1 -> do
            let (kw, rest) = splitAt 4 s1
            if kw == "rue" then Right (JBool True, rest) else Left "invalid JSON"
        'f' : s1 -> do
            let (kw, rest) = splitAt 5 s1
            if kw == "alse" then Right (JBool False, rest) else Left "invalid JSON"
        'n' : s1 -> do
            let (kw, rest) = splitAt 4 s1
            if kw == "ull" then Right (JNull, rest) else Left "invalid JSON"
        c : _
            | c == '-' || c >= '0' && c <= '9' ->
                let (num, rest) = span (\d -> d `elem` ("-+.eE0123456789" :: String)) s0
                 in case reads num of
                        [(d, "")] -> Right (JNum d, rest)
                        _ -> Left ("invalid number " ++ num)
        c : _ -> Left ("unexpected character '" ++ [c] ++ "'")
        [] -> Left "unexpected end of input"
    pObj s0 = go (dropWhile isSpace s0) []
      where
        go ('}' : s1) acc = Right (JObj (reverse acc), s1)
        go ('"' : s1) acc = do
            (k, s2) <- pStr ('"' : s1)
            let s3 = dropWhile isSpace s2
            case s3 of
                ':' : s4 -> do
                    (v, s5) <- pValue (dropWhile isSpace s4)
                    let s6 = dropWhile isSpace s5
                    case s6 of
                        ',' : s7 -> go (dropWhile isSpace s7) ((k, v) : acc)
                        '}' : s7 -> Right (JObj (reverse ((k, v) : acc)), s7)
                        _ -> Left "expected ',' or '}' in object"
                _ -> Left "expected ':' in object"
        go _ _ = Left "expected '\"' or '}' in object"
    pArr s0 = go (dropWhile isSpace s0) []
      where
        go (']' : s1) acc = Right (JArr (reverse acc), s1)
        go s1 acc = do
            (v, s2) <- pValue (dropWhile isSpace s1)
            let s3 = dropWhile isSpace s2
            case s3 of
                ',' : s4 -> go (dropWhile isSpace s4) (v : acc)
                ']' : s4 -> Right (JArr (reverse (v : acc)), s4)
                _ -> Left "expected ',' or ']' in array"
    pStr ('"' : s0) = go s0 []
      where
        go ('"' : s1) acc = Right (T.pack (reverse acc), s1)
        go ('\\' : c : s1) acc = case c of
            '"' -> go s1 ('"' : acc)
            '\\' -> go s1 ('\\' : acc)
            '/' -> go s1 ('/' : acc)
            'b' -> go s1 ('\b' : acc)
            'f' -> go s1 ('\f' : acc)
            'n' -> go s1 ('\n' : acc)
            'r' -> go s1 ('\r' : acc)
            't' -> go s1 ('\t' : acc)
            'u' ->
                let (hx, s2) = splitAt 4 s1
                 in case reads ("0x" ++ hx) of
                        [(n, "")] -> go s2 (toEnum n : acc)
                        _ -> Left "invalid \\u escape"
            _ -> Left "invalid escape"
        go (c : s1) acc = go s1 (c : acc)
        go [] _ = Left "unterminated string"
    pStr _ = Left "expected string"

objLookup :: Text -> Json -> Maybe Json
objLookup k (JObj kv) = lookup k kv
objLookup _ _ = Nothing

objLookupDef :: Json -> Text -> Json -> Json
objLookupDef def k j = maybe def id (objLookup k j)

strValue :: Json -> Text
strValue (JStr t) = t
strValue _ = T.empty

intOf :: Json -> Maybe Integer
intOf (JNum d) = Just (truncate d)
intOf _ = Nothing

arrItems :: Json -> [Json]
arrItems (JArr xs) = xs
arrItems _ = []

-- | Sorted object members (json11 iterates std::map — sorted keys).
sortedPairs :: Text -> Json -> [(Text, Json)]
sortedPairs key j = sortOn fst (pairsOf key j)

pairsOf :: Text -> Json -> [(Text, Json)]
pairsOf key j = case objLookup key j of
    Just (JObj kv) -> kv
    _ -> []

netnamesOf :: Json -> [(Text, Json)]
netnamesOf = sortedPairs "netnames"

portsOf :: Json -> [(Text, Json)]
portsOf = sortedPairs "ports"

cellsOf :: Json -> [(Text, Json)]
cellsOf = sortedPairs "cells"

cellTypeOf :: Json -> Text
cellTypeOf = strValue . objLookupDef (JStr "") "type"

-- | Top-level ports, inouts last (@import_toplevel_ports@ iterates
-- non-inouts first, then inouts).
sortPorts :: Json -> [(Text, Json)]
sortPorts m = sortOn rank (portsOf m)
  where
    rank (_, p) = case strValue (objLookupDef (JStr "inout") "direction" p) of
        "inout" -> (1 :: Int, "")
        _ -> (0, "")

-- | The top module selection (mirrors @find_top_module@: override, then
-- the @top@ attribute, then autodetection). Module names are interned
-- by the caller BEFORE this runs, exactly like the C++ pre-pass.
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

-- import state ------------------------------------------------------------

-- | The frontend state (net candidate names per module bit index, the
-- created-net index, used namespaces, top port names, const counter).
data ImportState = ImportState
    { stNetNames :: !(IntMap [Text])
    , stNetByBit :: !(IntMap IdString)
    , stUsedNets :: !(M.Map IdString ())
    , stUsedCells :: !(M.Map IdString ())
    , stPortNames :: ![Text]
    , stConstIdx :: !Int
    }

emptyState :: ImportState
emptyState = ImportState IM.empty IM.empty M.empty M.empty [] 0

-- | The import result type (IO for interning, Either for errors).
type Import a = IO (Either String a)

-- | Load a yosys netlist document. The optional override selects the
-- top module by name (the @frontend\/top@ setting); @Nothing@ uses the
-- @top@ attribute, then autodetection.
loadJsonDesign :: IdTable -> Maybe Text -> Text -> Import (Design bel wire pip)
loadJsonDesign tbl topOverride src = do
    let pv = parseJson src
    pv `seq` case pv of
        Left err -> pure (Left ("JSON parse error: " ++ err))
        Right root -> do
            -- json11 stores objects as std::map: module iteration is
            -- sorted by key (the backslash-prefixed \$__ABC9_* modules
            -- sort after the plain $-prefixed ones)
            let mods = sortOn fst (pairsOf "modules" root)
            -- find_top_module pre-pass: module names and instantiated
            -- cell types are interned for every module (sorted order)
            forM_ mods $ \(n, m) -> do
                _ <- intern tbl n
                forM_ (cellsOf m) $ \(_, cd) -> do
                    _ <- intern tbl (cellTypeOf cd)
                    pure ()
            _ <- intern tbl "frontend/top"
            case selectTop topOverride mods of
                Left err -> pure (Left err)
                Right topName -> do
                    let topMod = maybe (JObj []) id (lookup topName mods)
                    -- import_module (top): port names first, then module
                    -- attributes (interned, in sorted order)
                    st0 <- foldM (internPortName tbl) emptyState (portsOf topMod)
                    forM_ (attrsOf topMod) $ \(k, _) -> do
                        _ <- intern tbl k
                        pure ()
                    -- the netnames section: record candidate names only
                    let st1 = foldl (addNetnameNames tbl) st0 (netnamesOf topMod)
                    -- cells (sorted): nets are created lazily here
                    r2 <- foldM (importCell tbl) (Right (emptyDesign, st1)) (cellsOf topMod)
                    case r2 of
                        Left err -> pure (Left err)
                        Right (d2, st2) -> do
                            -- net attributes (sorted netnames × attrs):
                            -- imported into each created net of the netname,
                            -- mirroring import_net_attrs
                            d2' <- foldM (applyNetAttrs st2) d2 (netnamesOf topMod)
                            -- top-level ports (non-inout first)
                            r3 <- foldM (importPort tbl) (Right (d2', st2)) (sortPorts topMod)
                            case r3 of
                                Left err -> pure (Left err)
                                Right (d3, st3) -> do
                                    -- mark the design as loaded through
                                    -- nextpnr, then process nextpnr
                                    -- attributes (interned unconditionally
                                    -- by the find calls)
                                    _ <- intern tbl "synth"
                                    _ <- intern tbl "NEXTPNR_BEL"
                                    _ <- intern tbl "ROUTING"
                                    pure (Right d3)
  where
    bitExists (JNum b) = b >= 0
    bitExists _ = False

    -- @import_net_attrs@: for each netname, for each bit, if the net
    -- exists, merge the attrs into it (values via from_string)
    applyNetAttrs :: ImportState -> Design bel wire pip -> (Text, Json) -> IO (Design bel wire pip)
    applyNetAttrs st d (_, nn) = do
        kvs <- mapM (\(k, v) -> (,) <$> intern tbl k <*> pure (propFromStr (strValue v))) (attrsOf nn)
        pure
            ( foldl'
                (\d' b -> case b of
                    JNum bi | bi >= 0
                        , Just netId <- IM.lookup (truncate bi) (stNetByBit st) ->
                            d'{designNets = M.adjust (\ni -> ni{netAttrs = M.union (M.fromList kvs) (netAttrs ni)}) netId (designNets d')}
                    _ -> d')
                d
                (arrItems (objLookupDef (JArr []) "bits" nn))
            )

attrsOf :: Json -> [(Text, Json)]
attrsOf j = case objLookup "attributes" j of
    Just (JObj kv) -> sortOn fst kv
    _ -> []

-- | Intern a top-level port name (the @port_to_bus@ keys).
internPortName :: IdTable -> ImportState -> (Text, Json) -> IO ImportState
internPortName tbl st (n, _) = do
    _ <- intern tbl n
    pure st{stPortNames = n : stPortNames st}

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

-- | Register the candidate names of one netname's bits (no interning).
addNetnameNames :: IdTable -> ImportState -> (Text, Json) -> ImportState
addNetnameNames _ st (base, nn) = foldl step st [0 .. width - 1]
  where
    bits = arrItems (objLookupDef (JArr []) "bits" nn)
    width = length bits
    offset = maybe 0 fromInteger (objLookup "offset" nn >>= intOf)
    upto = objLookup "upto" nn == Just (JBool True)
    step st' i = case bits !! i of
        JNum b | b >= 0 ->
            let idx = truncate b
                nm = getBitName base i width offset upto
             in st'{stNetNames = IM.insertWith (flip (++)) idx [nm] (stNetNames st')}
        _ -> st'

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

-- | A fresh, unconnected net.
freshNet :: IdString -> NetInfo bel wire pip
freshNet name =
    NetInfo
        { netName = name
        , netHierpath = emptyId
        , netDriver = PortRef Nothing emptyId
        , netUsers = V.empty
        , netUserFree = []
        , netWires = M.empty
        , netAttrs = M.empty
        , netConstantValue = emptyId
        }

-- | Look up (creating if needed) the net for a module bit index
-- (@create_or_get_net@). All candidate names are interned (in order) —
-- the prefer_netlabel comparisons call @ctx->id@ on both arguments.
getOrCreateNet :: IdTable -> ImportState -> Int -> Import (Design bel wire pip, ImportState, IdString)
getOrCreateNet tbl st idx =
    case IM.lookup idx (stNetByBit st) of
        Just netId -> pure (Right (emptyDesign, st, netId))
        Nothing -> do
            let names = maybe [] id (IM.lookup idx (stNetNames st))
            chosen <-
                if null names
                    then pure ("$frontend$" <> T.pack (show idx))
                    else do
                        -- mirror @prefer_netlabel@: each comparison interns
                        -- a (= the next candidate), then b (= the running
                        -- best) only when a is not a top port
                        let pickFrom cur [] = pure cur
                            pickFrom cur (x : xs) = do
                                _ <- intern tbl x
                                if x `elem` stPortNames st
                                    then pure x -- a wins; b not interned
                                    else do
                                        _ <- intern tbl cur
                                        if cur `elem` stPortNames st
                                            then pure cur
                                            else
                                                let better =
                                                        (T.count "$" x, T.count "." x, x)
                                                            < (T.count "$" cur, T.count "." cur, cur)
                                                 in pickFrom (if better then x else cur) xs
                        pickFrom (head names) (tail names)
            netId <- uniqueNet tbl (stUsedNets st) (T.unpack chosen)
            pure (Right (addNet netId (freshNet netId) emptyDesign, st{stNetByBit = IM.insert idx netId (stNetByBit st), stUsedNets = M.insert netId () (stUsedNets st)}, netId))

-- cells ------------------------------------------------------------------

-- | Import one leaf cell (mirrors @import_leaf_cell@: cell name, type,
-- port directions, connections (nets created lazily), then attributes
-- and parameters — all in sorted key order).
importCell :: IdTable -> Either String (Design bel wire pip, ImportState) -> (Text, Json) -> Import (Design bel wire pip, ImportState)
importCell tbl acc (name, cd) = do
    case acc of
        Left err -> pure (Left err)
        Right (d, st) -> do
            let ctype = cellTypeOf cd
            if ctype `elem` ["$scopeinfo", "$print", "$check"]
                then pure (Right (d, st))
                else do
                    cellId <- uniqueCell tbl (stUsedCells st) (T.unpack name)
                    typeId <- intern tbl ctype
                    -- foreach_port_dir interns the port names first (sorted)
                    mapM_ (\(p, _) -> intern tbl p >> pure ()) (sortOn fst (pairsOf "port_directions" cd))
                    let dirs = [(p, dirOf v) | (p, v) <- sortOn fst (pairsOf "port_directions" cd)]
                        conns = [(p, arrItems v) | (p, v) <- sortOn fst (pairsOf "connections" cd)]
                        cell0 =
                                CellInfo
                                    { cellName = cellId
                                    , cellType = typeId
                                    , cellHierpath = emptyId
                                    , cellPorts = M.empty
                                    , cellPortOrder = []
                                    , cellAttrs = M.empty
                                    , cellParams = M.empty
                                    , cellBel = Nothing
                                    , cellBelStrength = StrengthNone
                                    , cellCluster = emptyId
                                    , cellConstrX = 0
                                    , cellConstrY = 0
                                    , cellConstrZ = 0
                                    , cellConstrAbsZ = False
                                    , cellConstrChildren = []
                                    }
                        st' = st{stUsedCells = M.insert cellId () (stUsedCells st)}
                        d1 = addCell cellId cell0 d
                    r <- foldM (importConn tbl dirs name cellId) (Right (d1, st')) conns
                    case r of
                        Left err -> pure (Left err)
                        Right (d2, st2) -> do
                            attrs <- attrsOf2 tbl cd
                            params <- paramsOf2 tbl cd
                            let d3 = d2{designCells = M.adjust (\ci -> ci{cellAttrs = attrs, cellParams = params}) cellId (designCells d2)}
                            pure (Right (d3, st2))

dirOf :: Json -> PortDir
dirOf v = case strValue v of
    "input" -> PortIn
    "output" -> PortOut
    _ -> PortInout

-- | The cell attributes (values via from_string, like the C++
-- @Property::from_string@) — interned AFTER the connections.
attrsOf2 :: IdTable -> Json -> IO (M.Map IdString Property)
attrsOf2 tbl cd = do
    kvs <- mapM (\(k, v) -> (,) <$> intern tbl k <*> pure (propFromStr (strValue v))) (attrsOf cd)
    pure (M.fromList kvs)

-- | The cell parameters (values via from_string).
paramsOf2 :: IdTable -> Json -> IO (M.Map IdString Property)
paramsOf2 tbl cd = do
    kvs <- mapM (\(k, v) -> (,) <$> intern tbl k <*> pure (propFromStr (strValue v))) (sortedPairs "parameters" cd)
    pure (M.fromList kvs)

-- | Import one port connection of a cell.
importConn :: IdTable -> [(Text, PortDir)] -> Text -> IdString -> Either String (Design bel wire pip, ImportState) -> (Text, [Json]) -> Import (Design bel wire pip, ImportState)
importConn tbl dirs cellName cellId acc (port, bits) = do
    case acc of
        Left err -> pure (Left err)
        Right (d, st) -> do
            case lookup port dirs of
                Nothing -> pure (Right (d, st))
                Just dir -> do
                    let width = length bits
                    foldM (importBit tbl cellName cellId port dir width) (Right (d, st)) (zip [0 ..] bits)

-- | Import one bit of a cell port.
importBit :: IdTable -> Text -> IdString -> Text -> PortDir -> Int -> Either String (Design bel wire pip, ImportState) -> (Int, Json) -> Import (Design bel wire pip, ImportState)
importBit tbl cellName cellId port dir width acc (i, bit) = do
    case acc of
        Left err -> pure (Left err)
        Right (d, st) -> do
            case bit of
                JStr "x" -> do
                    -- undef bit: the port is still created, just
                    -- without a net (foreach_port_conn creates the port
                    -- before the x check)
                    portId <- intern tbl (getBitName port i width 0 False)
                    pure (Right (setCellPort cellId portId dir d, st))
                JStr c
                    -- constant bit: net named <cell>.<port>$const plus a
                    -- VCC/GND driver cell (x/z get the net but no driver)
                    | c == "z" -> mkConstNet d st c Nothing
                    | otherwise -> mkConstNet d st c (Just c)
                JNum b | b >= 0 -> do
                    let idx = truncate b
                    -- the C++ interns the port bit name BEFORE resolving
                    -- the net (get_bit_name first, then create_or_get_net)
                    portId <- intern tbl (getBitName port i width 0 False)
                    r <- getOrCreateNet tbl st idx
                    case r of
                        Left err -> pure (Left err)
                        Right (dN, st', netId) -> do
                            let dP = setCellPort cellId portId dir (dN <> d)
                            -- multiple drivers are an error, like the C++
                            case dir of
                                PortOut
                                    | Just ni <- lookupNet netId dP
                                    , prCell (netDriver ni) /= Nothing ->
                                        pure (Left ("Net is multiply driven by cell ports " ++ show (prCell (netDriver ni)) ++ "." ++ show (prPort (netDriver ni)) ++ " and " ++ show cellId ++ "." ++ show portId))
                                    | otherwise -> pure (Right (connectPort cellId portId netId dP, st'))
                                _ -> pure (Right (connectPort cellId portId netId dP, st'))
                _ -> pure (Right (d, st))
  where
    -- create the constant net + optional VCC/GND driver cell
    mkConstNet d st constVal mdrv = do
        -- port bit name first, like the C++ get_bit_name ordering
        portId <- intern tbl (getBitName port i width 0 False)
        let hint = T.unpack cellName <> "." <> T.unpack (getBitName port i width 0 False) <> "$const"
        netId <- uniqueNet tbl (stUsedNets st) hint
        let st1 = st{stUsedNets = M.insert netId () (stUsedNets st)}
            d1 = addNet netId (freshNet netId) d
        case mdrv of
            Nothing -> do
                -- 'z': the net exists but is undriven
                let dP = setCellPort cellId portId dir d1
                pure (Right (connectPort cellId portId netId dP, st1))
            Just cv -> do
                -- add_constant_driver: VCC/GND cell named
                -- <netname>$VCC$<n> / <netname>$GND$<n>
                let suffix = if cv == "1" then "$VCC$" else "$GND$"
                    -- resolve the net's text BEFORE interning: the hint
                    -- is forced inside intern's atomicModifyIORef' and
                    -- idToText reads the same IORef (re-entrancy deadlock)
                    hintName = idToText tbl netId
                    cellHint = T.unpack hintName <> suffix <> show (stConstIdx st1)
                    st2 = st1{stConstIdx = stConstIdx st1 + 1}
                -- force the hint before interning: intern's atomicModify
                -- must not re-enter the table via idToText (deadlock)
                cellId2 <- cellHint `seq` uniqueCell tbl (stUsedCells st2) cellHint
                typeId <- intern tbl (if cv == "1" then "VCC" else "GND")
                yId <- intern tbl "Y"
                let cc =
                        CellInfo
                            { cellName = cellId2
                            , cellType = typeId
                            , cellHierpath = emptyId
                            , cellPorts = M.singleton yId (PortInfo yId Nothing PortOut 0)
                            , cellPortOrder = [yId]
                            , cellAttrs = M.empty
                            , cellParams = M.empty
                            , cellBel = Nothing
                            , cellBelStrength = StrengthNone
                            , cellCluster = emptyId
                            , cellConstrX = 0
                            , cellConstrY = 0
                            , cellConstrZ = 0
                            , cellConstrAbsZ = False
                            , cellConstrChildren = []
                            }
                    st3 = st2{stUsedCells = M.insert cellId2 () (stUsedCells st2)}
                    d2 = addCell cellId2 cc d1
                    d3 = connectPort cellId2 yId netId d2
                    dP = setCellPort cellId portId dir d3
                pure (Right (connectPort cellId portId netId dP, st3))

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
            JNum b | b >= 0 -> do
                let idx = truncate b
                r <- getOrCreateNet tbl st idx
                case r of
                    Left err -> pure (Left err)
                    Right (dN, st', netId) -> do
                        let pbit = getBitName pname i width 0 False
                        -- create_iobuf: the synth settings check interns
                        -- first, then the cell name, then unknown_iob
                        _ <- intern tbl "synth"
                        iobufId <- uniqueCell tbl (stUsedCells st') (T.unpack pbit)
                        _ <- intern tbl "unknown_iob"
                        iobufType <- intern tbl (case dir of
                            PortIn -> "$nextpnr_ibuf"
                            PortOut -> "$nextpnr_obuf"
                            PortInout -> "$nextpnr_iobuf")
                        -- create_iobuf copies the net's attrs onto the iobuf
                        let netAttrs' = maybe M.empty netAttrs (lookupNet netId d)
                        let cell0 =
                                CellInfo
                                    { cellName = iobufId
                                    , cellType = iobufType
                                    , cellHierpath = emptyId
                                    , cellPorts = M.empty
                                    , cellPortOrder = []
                                    , cellAttrs = netAttrs'
                                    , cellParams = M.empty
                                    , cellBel = Nothing
                                    , cellBelStrength = StrengthNone
                                    , cellCluster = emptyId
                                    , cellConstrX = 0
                                    , cellConstrY = 0
                                    , cellConstrZ = 0
                                    , cellConstrAbsZ = False
                                    , cellConstrChildren = []
                                    }
                            st1 = st'{stUsedCells = M.insert iobufId () (stUsedCells st')}
                            d1 = addCell iobufId cell0 (dN <> d)
                        case dir of
                            PortIn -> do
                                -- the ibuf output drives the port net
                                portId <- intern tbl "O"
                                let d2 = setCellPort iobufId portId PortOut d1
                                    d3 = connectPort iobufId portId netId d2
                                    d4 = d3{designPorts = M.insert (iobufId) netId (designPorts d3)}
                                pure (Right (d4, st1))
                            PortOut -> do
                                -- the port net drives the obuf input
                                portId <- intern tbl "I"
                                let d2 = setCellPort iobufId portId PortIn d1
                                    d3 = connectPort iobufId portId netId d2
                                    d4 = d3{designPorts = M.insert iobufId netId (designPorts d3)}
                                pure (Right (d4, st1))
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
                                    d6 = d5{designPorts = M.insert iobufId netId (designPorts d5)}
                                pure (Right (d6, st3))
            _ -> pure (Right (d, st))

-- unique names -----------------------------------------------------------

-- | A cell name not yet used (appends __unique__N on collision).
uniqueCell :: IdTable -> M.Map IdString () -> String -> IO IdString
uniqueCell tbl used base = go 0
  where
    go n = do
        let nm = if n == 0 then T.pack base else T.pack (base ++ "__unique__" ++ show n)
        i <- intern tbl nm
        if M.member i used then go (n + 1) else pure i

-- | A net name not yet used (nets live in a separate namespace).
uniqueNet :: IdTable -> M.Map IdString () -> String -> IO IdString
uniqueNet = uniqueCell
