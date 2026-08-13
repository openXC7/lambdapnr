{-# LANGUAGE OverloadedStrings #-}

{- | The ECP5 bitstream configuration — the prjtrellis "text config"
format that @--textcfg@ writes and @ecppack@ consumes.

Mirror of @ecp5\/config.h@\/@config.cc@: a chip-level configuration
holds per-tile routing arcs and non-routing settings (words and
enums), plus metadata, sysconfig entries, BRAM init data and tile
groups. The text format is:

@
.device LFE5U-12F

.comment Part: LFE5U-12F-6CABGA256

.tile CIB_R10C1:CIB_LR
arc: E1_H02E0201 S1_V02N0201
enum: CIB.JC1MUX 0
word: SLICEA.K0.INIT 0b0000000000000000

.tile_group T1 T2
arc: ...
@

Bit vectors are LSB-first in memory and printed MSB-first (the C++
@to_string@ reverses), so a @word:@ value is the bit string with the
MSB on the left.
-}
module Lambdapnr.Arch.Ecp5.Config (
    ConfigArc (..),
    ConfigWord (..),
    ConfigEnum (..),
    ConfigUnknown (..),
    TileConfig (..),
    TileGroup (..),
    ChipConfig (..),
    emptyTileConfig,
    emptyChipConfig,
    addArc,
    addWord,
    addEnum,
    addUnknown,
    renderChipConfig,
    parseChipConfig,
) where

import Data.Char (isDigit, isHexDigit, isSpace)
import Data.Map.Strict (Map)
import Numeric (showHex)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

-- | A connection in a tile (sink receives from source).
data ConfigArc = ConfigArc
    { caSink :: !Text
    , caSource :: !Text
    }
    deriving (Eq, Show)

-- | A multi-bit configuration setting (such as a LUT init).
data ConfigWord = ConfigWord
    { cwName :: !Text
    , cwValue :: ![Bool] -- ^ LSB first, like the C++ vector
    }
    deriving (Eq, Show)

-- | An enumeration configuration setting (such as an IO type).
data ConfigEnum = ConfigEnum
    { ceName :: !Text
    , ceValue :: !Text
    }
    deriving (Eq, Show)

-- | An unknown bit, specified by position only.
data ConfigUnknown = ConfigUnknown
    { cuFrame :: !Int
    , cuBit :: !Int
    }
    deriving (Eq, Show)

-- | The configuration of one tile.
data TileConfig = TileConfig
    { tcArcs :: ![ConfigArc]
    , tcWords :: ![ConfigWord]
    , tcEnums :: ![ConfigEnum]
    , tcUnknowns :: ![ConfigUnknown]
    }
    deriving (Eq, Show)

-- | A group of tiles configured together (non-routing only).
data TileGroup = TileGroup
    { tgTiles :: ![Text]
    , tgConfig :: !TileConfig
    }
    deriving (Eq, Show)

-- | The whole chip configuration.
data ChipConfig = ChipConfig
    { ccChipName :: !Text
    , ccMetadata :: ![Text]
    , ccTiles :: !(Map Text TileConfig)
    , ccTileGroups :: ![TileGroup]
    , ccSysconfig :: !(Map Text Text)
    , ccBramData :: !(Map Int [Int])
    }
    deriving (Eq, Show)

emptyTileConfig :: TileConfig
emptyTileConfig = TileConfig [] [] [] []

emptyChipConfig :: ChipConfig
emptyChipConfig = ChipConfig "" [] M.empty [] M.empty M.empty

addArc :: Text -> Text -> TileConfig -> TileConfig
addArc sink source tc = tc{tcArcs = tcArcs tc ++ [ConfigArc sink source]}

addWord :: Text -> [Bool] -> TileConfig -> TileConfig
addWord name value tc = tc{tcWords = tcWords tc ++ [ConfigWord name value]}

addEnum :: Text -> Text -> TileConfig -> TileConfig
addEnum name value tc = tc{tcEnums = tcEnums tc ++ [ConfigEnum name value]}

addUnknown :: Int -> Int -> TileConfig -> TileConfig
addUnknown frame bit tc = tc{tcUnknowns = tcUnknowns tc ++ [ConfigUnknown frame bit]}

-- writing -----------------------------------------------------------------

-- | The word bits, MSB first (the C++ reverses the LSB-first vector).
wordBits :: ConfigWord -> Text
wordBits = T.pack . map (\b -> if b then '1' else '0') . reverse . cwValue

-- | Render a tile's entries: arcs, then words, then enums, then
-- unknowns (the C++ @TileConfig@ stream order).
renderTileConfig :: TileConfig -> Text
renderTileConfig tc =
    T.unlines
        ( concat
            [ ["arc: " <> caSink a <> " " <> caSource a | a <- tcArcs tc]
            , ["word: " <> cwName w <> " " <> wordBits w | w <- tcWords tc]
            , ["enum: " <> ceName e <> " " <> ceValue e | e <- tcEnums tc]
            , ["unknown: F" <> tshow (cuFrame u) <> "B" <> tshow (cuBit u) | u <- tcUnknowns tc]
            ]
        )
  where
    tshow = T.pack . show

-- | Render the whole chip configuration.
renderChipConfig :: ChipConfig -> Text
renderChipConfig cc =
    T.concat
        ( [".device " <> ccChipName cc <> "\n\n"]
            ++ [".comment " <> m <> "\n" | m <- ccMetadata cc]
            ++ [".sysconfig " <> k <> " " <> v <> "\n" | (k, v) <- M.toList (ccSysconfig cc)]
            ++ ["\n"]
            ++ [renderTile (k, v) | (k, v) <- M.toList (ccTiles cc), not (tileEmpty v)]
            ++ concatMap renderBram (M.toList (ccBramData cc))
            ++ concatMap renderGroup (ccTileGroups cc)
        )
  where
    tileEmpty tc = null (tcArcs tc) && null (tcWords tc) && null (tcEnums tc) && null (tcUnknowns tc)
    renderTile (name, tc) = ".tile " <> name <> "\n" <> renderTileConfig tc <> "\n"
    renderBram (key, bytes) =
        [ ".bram_init " <> T.pack (show key) <> "\n"
            <> T.intercalate "\n" (map T.unwords (chunksOf 8 [T.pack (pad3 (showHex b)) | b <- bytes]))
            <> "\n"
        ]
    showHex b = Numeric.showHex b ""
    pad3 s = replicate (3 - length s) '0' ++ s
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)
    renderGroup tg = [".tile_group" <> T.concat [" " <> t | t <- tgTiles tg] <> "\n" <> renderTileConfig (tgConfig tg) <> "\n"]

-- reading -----------------------------------------------------------------

-- | Parse the config text (the C++ @operator>>@).
parseChipConfig :: Text -> Either String ChipConfig
parseChipConfig = go emptyChipConfig . T.lines
  where
    go cc [] = Right cc
    go cc (l : ls)
        | T.null (T.strip l) = go cc ls
        | Just rest <- T.stripPrefix ".device " l =
            go cc{ccChipName = T.strip rest} ls
        | Just rest <- T.stripPrefix ".comment " l = go cc{ccMetadata = ccMetadata cc ++ [rest]} ls
        | Just rest <- T.stripPrefix ".sysconfig " l =
            case T.words rest of
                [k, v] -> go cc{ccSysconfig = M.insert k v (ccSysconfig cc)} ls
                _ -> Left "bad .sysconfig line"
        | Just rest <- T.stripPrefix ".tile " l =
            case parseTileLines ls of
                Left err -> Left err
                Right (tc, ls') -> go cc{ccTiles = M.insert (T.strip rest) tc (ccTiles cc)} ls'
        | Just rest <- T.stripPrefix ".tile_group" l = do
            let tiles = T.words rest
            case parseTileLines ls of
                Left err -> Left err
                Right (tc, ls') -> go cc{ccTileGroups = ccTileGroups cc ++ [TileGroup tiles tc]} ls'
        | Just rest <- T.stripPrefix ".bram_init " l =
            case readInt (T.strip rest) of
                Nothing -> Left "bad .bram_init key"
                Just key -> case parseBramLines ls of
                    Left err -> Left err
                    Right (bytes, ls') -> go cc{ccBramData = M.insert key bytes (ccBramData cc)} ls'
        | otherwise = Left ("unexpected line: " ++ T.unpack l)
    parseTileLines ls = go2 [] [] [] [] ls
      where
        go2 arcs words' enums unks [] = Right (TileConfig arcs words' enums unks, [])
        go2 arcs words' enums unks (l : ls)
            | T.null (T.strip l) = Right (TileConfig arcs words' enums unks, ls)
            | Just rest <- T.stripPrefix "arc: " l =
                case T.words rest of
                    [sink, source] -> go2 (arcs ++ [ConfigArc sink source]) words' enums unks ls
                    _ -> Left "bad arc line"
            | Just rest <- T.stripPrefix "word: " l =
                case T.words rest of
                    [name, bits] -> go2 arcs (words' ++ [ConfigWord name (parseBits bits)]) enums unks ls
                    _ -> Left "bad word line"
            | Just rest <- T.stripPrefix "enum: " l =
                case T.words rest of
                    [name, value] -> go2 arcs words' (enums ++ [ConfigEnum name value]) unks ls
                    _ -> Left "bad enum line"
            | Just rest <- T.stripPrefix "unknown: " l = case parseUnknown rest of
                Just u -> go2 arcs words' enums (unks ++ [u]) ls
                Nothing -> Left "bad unknown line"
            | otherwise = Left ("unexpected tile line: " ++ T.unpack l)
    parseBramLines [] = Left "unterminated .bram_init"
    parseBramLines (l : ls)
        | T.null (T.strip l) = Right ([], ls)
        | otherwise =
            let bytes = map readHexByte (T.words l)
             in if all (>= 0) bytes
                    then fmap (\(bs, ls') -> (bytes ++ bs, ls')) (parseBramLines ls)
                    else Left "bad bram line"
    parseUnknown t =
        case T.stripPrefix "F" t >>= (\r -> case T.breakOn "B" r of (f, b) -> Just (f, T.drop 1 b)) of
            Just (f, b) -> ConfigUnknown <$> readInt f <*> readInt b
            Nothing -> Nothing
    readInt t = case reads (T.unpack t) of
        [(i, "")] -> Just i
        _ -> Nothing
    -- the C++ reads the MSB-first string into an LSB-first vector
    parseBits = reverse . map (== '1') . T.unpack
    readHexByte t = case reads ("0x" ++ T.unpack t) of
        [(b, "")] -> b
        _ -> -1
