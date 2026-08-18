{-# LANGUAGE OverloadedStrings #-}

{- | The @--write@ JSON design dump — a port of
@json\/jsonwrite.cc@ (@write_json_file@), byte-comparable with the
oracle. Iteration orders mirror the C++ dict (reverse insertion, with
swap-erase); disconnected port bits are dummy indices starting at
@idstring_count + 1000@, so the interning count must match the C++'s
at the write point.
-}
module Lambdapnr.Kernel.JsonWrite (
    writeJsonFile,
) where

import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

import Lambdapnr.Kernel.IdString (IdString (..), IdTable, idTableSize, idToText, intern)
import Lambdapnr.Kernel.Netlist (CellInfo, Design (..), NetInfo (..), PortDir (..), cellAttrOrder, cellAttrs, cellParamOrder, cellParams, cellPortOrder, cellPorts, cellType, portNet, portType)
import Lambdapnr.Kernel.Property (Property, propAsString, propToStr)

-- | @get_string@: backslashes double, quotes are NOT escaped (the C++
-- only doubles backslashes).
quoteString :: String -> String
quoteString s = "\"" ++ concatMap (\c -> if c == '\\' then "\\\\" else [c]) s ++ "\""

quoteText :: Text -> String
quoteText = quoteString . T.unpack

-- | @write_parameters@: iterate in dict order (reverse insertion).
writeParameters :: IdTable -> [IdString] -> (IdString -> Property) -> String -> String
writeParameters tbl order lookupP indent =
    concatMap entry (zip [0 :: Int ..] order)
  where
    entry (i, k) =
        (if i == 0 then "\n" else ",\n")
            ++ indent
            ++ quoteString (T.unpack (idToText tbl k))
            ++ ": "
            ++ quoteString (T.unpack (propToStr (lookupP k)))

-- | A port group (the C++ @PortGroup@).
data PortGroup = PortGroup
    { pgName :: !Text
    , pgDir :: !PortDir
    , pgBits :: ![Int]
    -- ^ bit per index (offset applied), -1 for disconnected
    , pgOffset :: !Int
    }
    deriving (Show)

-- | @group_ports@: iterate the ports in dict order (reverse insertion);
-- @[i]@-suffixed names group by base name (first-seen order). The C++
-- uses the port NAME index for single top ports and the net index for
-- bracketed ones; cells use the net index.
groupPorts :: (IdString -> Text) -> (IdString -> Int) -> (IdString -> Int) -> (IdString -> PortDir) -> [IdString] -> [PortGroup]
groupPorts nameOf bitSingle bitBracketed dirOf order = map finish (collect order M.empty [])
  where
    collect [] _ acc = acc
    collect (p : rest) baseToGrp acc =
        let nm = T.unpack (nameOf p)
         in if null nm || last nm /= ']' || '[' `notElem` nm
                then collect rest baseToGrp (acc ++ [(T.pack nm, dirOf p, [(0, bitSingle p)])])
                else
                    let off1 = lastBracket nm
                        base = take off1 nm
                        idx = read (take (length nm - (off1 + 2)) (drop (off1 + 1) nm)) :: Int
                     in case M.lookup base baseToGrp of
                            Just gi ->
                                let (bs, dir, gbs) = acc !! gi
                                 in collect rest baseToGrp (updAt gi (bs, dir, gbs ++ [(idx, bitBracketed p)]) acc)
                            Nothing ->
                                let gi = length acc
                                 in collect rest (M.insert base gi baseToGrp) (acc ++ [(T.pack base, dirOf p, [(idx, bitBracketed p)])])
    lastBracket nm = go (length nm - 1)
      where
        go i = if i >= 0 && nm !! i /= '[' then go (i - 1) else i
    finish (bs, dir, gbs) =
        let offset = minimum (map fst gbs)
            size = maximum (map (\g -> fst g - offset) gbs) + 1
            bits0 = replicate size (-1)
            bits1 = foldl' (\v (ix, b) -> updAt (ix - offset) b v) bits0 gbs
         in PortGroup bs dir bits1 offset
    updAt i x xs = case splitAt i xs of
        (before, _ : after) -> before ++ [x] ++ after
        _ -> xs

-- | @format_port_bits@: @-1@ bits take successive dummy indices (the
-- counter starts at @idstring_count + 1000@ and is shared across the
-- whole file).
formatPortBits :: Int -> [Int] -> (String, Int)
formatPortBits counter bits
    | length bits == 1 && head bits == -1 = ("[  ]", counter)
    | otherwise =
        let (revParts, counter') =
                foldl'
                    (\(acc, c) b -> case b of
                        -1 -> (show c : acc, c + 1)
                        _ -> (show b : acc, c)
                    )
                    ([], counter)
                    bits
         in ("[ " ++ intercalate ", " (reverse revParts) ++ " ]", counter')
  where
    intercalate sep xs = case xs of
        [] -> ""
        (a : rest) -> a ++ concatMap (sep ++) rest

dirStr :: PortDir -> String
dirStr PortIn = "input"
dirStr PortOut = "output"
dirStr PortInout = "inout"

hideNameOf :: Text -> String
hideNameOf nm = if T.null nm then "1" else if T.head nm == '$' then "1" else "0"

-- | Render the full file.
writeJsonFile ::
    IdTable ->
    -- | settings map + insertion order
    (M.Map IdString Property, [IdString]) ->
    -- | module attrs + insertion order
    (M.Map IdString Property, [IdString]) ->
    Design bel wire pip ->
    IO String
writeJsonFile tbl (settings, settingsOrd) (attrs, attrsOrd) d = do
    -- write_module: ctx->attrs.find(ctx->id("module")) interns "module"
    -- before the dummy index count
    moduleId <- intern tbl "module"
    nIdstrings <- idTableSize tbl
    let nameOf = idToText tbl
        dummy0 = nIdstrings + 1000
        modName = if M.member moduleId attrs then propAsString (attrs M.! moduleId) else "top"
        settingsS = writeParameters tbl (reverse settingsOrd) (settings M.!) "        "
        attrsS = writeParameters tbl (reverse attrsOrd) (attrs M.!) "        "
        -- ports: top ports use the port name index for single bits
        topBitSingle p = unIdString p
        topBitBracketed p = maybe (unIdString p) unIdString (M.lookup p (designPorts d))
        portGroups = groupPorts nameOf topBitSingle topBitBracketed (maybe PortInout id . flip M.lookup (designPortDirs d)) (reverse (designPortOrder d))
        (portsS, dummy1) = renderPorts dummy0 portGroups
        (cellsS, dummy2) = renderCells tbl dummy1 nameOf (reverse (designCellOrder d)) (designCells d)
        (netsS, _) = renderNets tbl dummy2 nameOf (reverse (designNetOrder d)) (designNets d)
     in pure $
            "{\n"
                ++ "  \"creator\": "
                ++ quoteString "Next Generation Place and Route (Version 10c14c8)"
                ++ ",\n"
                ++ "  \"modules\": {\n"
                ++ "    " ++ quoteText modName ++ ": {\n"
                ++ "      \"settings\": {" ++ settingsS ++ "\n      },\n"
                ++ "      \"attributes\": {" ++ attrsS ++ "\n      },\n"
                ++ "      \"ports\": {" ++ portsS ++ "\n      },\n"
                ++ "      \"cells\": {" ++ cellsS ++ "\n      },\n"
                ++ "      \"netnames\": {" ++ netsS ++ "\n      }\n"
                ++ "    }"
                ++ "\n  }"
                ++ "\n}\n"

-- | The ports section.
renderPorts :: Int -> [PortGroup] -> (String, Int)
renderPorts dummy0 groups =
    foldl'
        (\(acc, d) (i, g) ->
            let (bitsS, d') = formatPortBits d (pgBits g)
                body =
                    (if i == (0 :: Int) then "\n" else ",\n")
                        ++ "        " ++ quoteText (pgName g) ++ ": {\n"
                        ++ "          \"direction\": \"" ++ dirStr (pgDir g) ++ "\",\n"
                        ++ (if pgOffset g /= 0 then "          \"offset\": " ++ show (pgOffset g) ++ ",\n" else "")
                        ++ "          \"bits\": " ++ bitsS ++ "\n"
                        ++ "        }"
             in (acc ++ body, d')
        )
        ("", dummy0)
        (zip [0 ..] groups)

-- | The cells section.
renderCells :: IdTable -> Int -> (IdString -> Text) -> [IdString] -> M.Map IdString (CellInfo bel wire pip) -> (String, Int)
renderCells tbl dummy0 nameOf order cells =
    foldl'
        (\(acc, d) (i, k) ->
            let ci = fromJustOrError "write: missing cell" (M.lookup k cells)
                cellBitSingle p = maybe (-1) unIdString (portNet =<< M.lookup p (cellPorts ci))
                groups = groupPorts nameOf cellBitSingle cellBitSingle (portTypeOf ci) (reverse (cellPortOrder ci))
                (dirsS, connsS, d') = renderCellGroups d groups
                paramsS = writeParameters tbl (reverse (cellParamOrder ci)) (\k -> M.findWithDefault (error "write: missing param") k (cellParams ci)) "            "
                attrsS = writeParameters tbl (reverse (cellAttrOrder ci)) (\k -> M.findWithDefault (error "write: missing attr") k (cellAttrs ci)) "            "
                body =
                    (if i == 0 then "\n" else ",\n")
                        ++ "        " ++ quoteText (nameOf k) ++ ": {\n"
                        ++ "          \"hide_name\": " ++ hideNameOf (nameOf k) ++ ",\n"
                        ++ "          \"type\": " ++ quoteText (cellTypeText ci) ++ ",\n"
                        ++ "          \"parameters\": {" ++ paramsS ++ "\n          },\n"
                        ++ "          \"attributes\": {" ++ attrsS ++ "\n          },\n"
                        ++ "          \"port_directions\": {" ++ dirsS ++ "\n          },\n"
                        ++ "          \"connections\": {" ++ connsS ++ "\n          }\n"
                        ++ "        }"
             in (acc ++ body, d')
        )
        ("", dummy0)
        (zip [0 :: Int ..] order)
  where
    portTypeOf ci p = maybe PortInout portType (M.lookup p (cellPorts ci))
    cellTypeText ci = idToText tbl (cellType ci)
    fromJustOrError _ (Just x) = x
    fromJustOrError msg Nothing = error msg

-- | The per-cell port_directions + connections blocks (the dummy counter
-- threads through both).
renderCellGroups :: Int -> [PortGroup] -> (String, String, Int)
renderCellGroups dummy0 groups =
    let step (dirsAcc, connsAcc, d) (i, g) =
            let (bitsS, d') = formatPortBits d (pgBits g)
                dirsAcc' = dirsAcc ++ (if i == (0 :: Int) then "\n" else ",\n") ++ "            " ++ quoteText (pgName g) ++ ": \"" ++ dirStr (pgDir g) ++ "\""
                connsAcc' = connsAcc ++ (if i == 0 then "\n" else ",\n") ++ "            " ++ quoteText (pgName g) ++ ": " ++ bitsS
             in (dirsAcc', connsAcc', d')
     in foldl' step ("", "", dummy0) (zip [0 ..] groups)

-- | The netnames section.
renderNets :: IdTable -> Int -> (IdString -> Text) -> [IdString] -> M.Map IdString (NetInfo bel wire pip) -> (String, Int)
renderNets tbl dummy0 nameOf order nets =
    foldl'
        (\(acc, d) (i, k) ->
            let ni = fromJustOrError "write: missing net" (M.lookup k nets)
                attrsS = writeParameters tbl (reverse (netAttrOrder ni)) (\k -> M.findWithDefault (error "write: missing net attr") k (netAttrs ni)) "            "
                body =
                    (if i == 0 then "\n" else ",\n")
                        ++ "        " ++ quoteText (nameOf k) ++ ": {\n"
                        ++ "          \"hide_name\": " ++ hideNameOf (nameOf k) ++ ",\n"
                        ++ "          \"bits\": [ " ++ show (unIdString k) ++ " ] ,\n"
                        ++ "          \"attributes\": {" ++ attrsS ++ "\n          }\n"
                        ++ "        }"
             in (acc ++ body, d)
        )
        ("", dummy0)
        (zip [0 :: Int ..] order)
  where
    fromJustOrError _ (Just x) = x
    fromJustOrError msg Nothing = error msg
