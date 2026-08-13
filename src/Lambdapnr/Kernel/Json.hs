{- | A minimal JSON parser for the yosys netlist format.

The yosys @write_json@ output used by nextpnr's frontend is a small
JSON subset: objects, arrays, strings (with escapes), integers, and the
literals. Rather than pull in a full JSON library, this module parses
exactly that subset with a hand-rolled recursive-descent parser —
deterministic, dependency-free, and fast enough for multi-megabyte
netlists.
-}
module Lambdapnr.Kernel.Json (
    Json (..),
    parseJson,
    objLookup,
    objLookupDef,
    arrItems,
    strValue,
) where

import Data.Char (chr, digitToInt, isDigit)
import Data.Text (Text)
import qualified Data.Text as T

-- | A parsed JSON value.
data Json
    = JNull
    | JBool Bool
    | JInt Integer
    | JFloat Double
    | JStr Text
    | JArr [Json]
    | JObj [(Text, Json)] -- ^ keys in document order
    deriving (Eq, Show)

-- | Look up a key in an object.
objLookup :: Text -> Json -> Maybe Json
objLookup k (JObj kv) = lookup k kv
objLookup _ _ = Nothing

-- | Look up a key with a default: @objLookupDef def key obj@.
objLookupDef :: Json -> Text -> Json -> Json
objLookupDef def k j = maybe def id (objLookup k j)

-- | The items of an array (empty for non-arrays).
arrItems :: Json -> [Json]
arrItems (JArr xs) = xs
arrItems _ = []

-- | The string value of a JSON string ("" for non-strings).
strValue :: Json -> Text
strValue (JStr s) = s
strValue _ = T.empty

-- | Parse a JSON document. Returns the value or an error with the
-- position.
parseJson :: Text -> Either String Json
parseJson = parseValue . T.unpack

-- | Parser state: remaining string + position.
type P = (String, Int)

parseValue :: String -> Either String Json
parseValue s0 = do
    (v, rest) <- value (skipWs s0)
    case skipWs rest of
        [] -> Right v
        _ -> Left "trailing data after JSON value"

skipWs :: String -> String
skipWs (c : cs)
    | c == ' ' || c == '\t' || c == '\n' || c == '\r' = skipWs cs
skipWs s = s

value :: String -> Either String (Json, String)
value ('{' : cs) = object cs
value ('[' : cs) = array cs
value ('"' : cs) = do
    (t, rest) <- string cs
    pure (JStr t, rest)
value ('t' : 'r' : 'u' : 'e' : rest) = pure (JBool True, rest)
value ('f' : 'a' : 'l' : 's' : 'e' : rest) = pure (JBool False, rest)
value ('n' : 'u' : 'l' : 'l' : rest) = pure (JNull, rest)
value (c : rest) | c == '-' || isDigit c = number tok (drop (length tok - 1) rest)
  where
    tok = c : takeWhile isNumChar rest
    isNumChar ch = isDigit ch || ch == '.' || ch == 'e' || ch == 'E' || ch == '+' || ch == '-'
value _ = Left "unexpected character in JSON value"

object :: String -> Either String (Json, String)
object cs0 = do
    let cs1 = skipWs cs0
    case cs1 of
        '}' : rest -> pure (JObj [], rest)
        _ -> go [] cs1
  where
    go acc cs = do
        let cs' = skipWs cs
        (k, cs1) <- case cs' of
            '"' : rest -> string rest
            _ -> Left "expected object key string"
        let cs2 = skipWs cs1
        cs3 <- case cs2 of
            ':' : rest -> Right (skipWs rest)
            _ -> Left "expected ':' in object"
        (v, cs4) <- value cs3
        let cs5 = skipWs cs4
        case cs5 of
            ',' : rest -> go ((k, v) : acc) rest
            '}' : rest -> pure (JObj (reverse ((k, v) : acc)), rest)
            _ -> Left "expected ',' or '}' in object"

array :: String -> Either String (Json, String)
array cs0 = do
    let cs1 = skipWs cs0
    case cs1 of
        ']' : rest -> pure (JArr [], rest)
        _ -> go [] cs1
  where
    go acc cs = do
        (v, cs1) <- value (skipWs cs)
        let cs2 = skipWs cs1
        case cs2 of
            ',' : rest -> go (v : acc) rest
            ']' : rest -> pure (JArr (reverse (v : acc)), rest)
            _ -> Left "expected ',' or ']' in array"

-- | A JSON string body (after the opening quote).
string :: String -> Either String (Text, String)
string = go []
  where
    go acc [] = Left "unterminated string"
    go acc ('"' : rest) = Right (T.pack (reverse acc), rest)
    go acc ('\\' : esc : rest) = case esc of
        '"' -> go ('"' : acc) rest
        '\\' -> go ('\\' : acc) rest
        '/' -> go ('/' : acc) rest
        'b' -> go ('\b' : acc) rest
        'f' -> go ('\f' : acc) rest
        'n' -> go ('\n' : acc) rest
        'r' -> go ('\r' : acc) rest
        't' -> go ('\t' : acc) rest
        'u' ->
            let (hex, rest') = splitAt 4 rest
             in if length hex == 4 && all isHexDigit hex
                    then go (chr (foldl (\n h -> n * 16 + digitToInt h) 0 hex) : acc) rest'
                    else Left "bad \\u escape"
    go acc (c : rest) = go (c : acc) rest
    isHexDigit ch = isDigit ch || ch `elem` ("abcdefABCDEF" :: String)

-- | A number token (already separated): parse int or float.
number :: String -> String -> Either String (Json, String)
number tok rest
    | '.' `elem` tok || 'e' `elem` tok || 'E' `elem` tok =
        case reads tok of
            [(d, "")] -> Right (JFloat d, rest)
            _ -> Left ("bad number: " ++ tok)
    | otherwise = case reads tok of
        [(i, "")] -> Right (JInt i, rest)
        _ -> Left ("bad number: " ++ tok)
