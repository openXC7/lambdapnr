-- | ECP5 chipdb: binary parser.
--
-- Parses the chipdb blob produced by @bbasm --le@ from the
-- @trellis_import.py@ output. The layout mirrors @ecp5\/arch.h@ exactly
-- (packed little-endian PODs with @RelPtr@\/@RelSlice@ relative offsets).
-- All slices are resolved eagerly into 'V.Vector's; the chipdb is
-- immutable after parsing and shared read-only by the arch.
module Lambdapnr.Arch.Ecp5.Chipdb
  ( Chipdb (..)
  , LocationType (..)
  , BelInfo (..)
  , BelWire (..)
  , WireInfo (..)
  , PipLocator (..)
  , BelPort (..)
  , PipInfo (..)
  , PackageInfo (..)
  , PackagePin (..)
  , PioInfo (..)
  , TileInfo (..)
  , TileName (..)
  , SpeedGrade (..)
  , CellTiming (..)
  , CellPropDelay (..)
  , CellSetupHold (..)
  , PipDelay (..)
  , parseChipdb
  ) where

import Control.Monad (unless, when)
import Data.Bits (shiftL, (.&.), (.|.))
import Data.Int (Int8, Int16, Int32)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Vector as V
import Data.Word (Word8)

-- | A parsed chipdb.
data Chipdb = Chipdb
  { cdWidth :: !Int
  , cdHeight :: !Int
  , cdNumTiles :: !Int
  , cdConstIdCount :: !Int
  , cdLocations :: !(V.Vector LocationType)
  , cdLocationType :: !(V.Vector Int)
  , cdTiletypeNames :: !(V.Vector Text)
  , cdPackages :: !(V.Vector PackageInfo)
  , cdPios :: !(V.Vector PioInfo)
  , cdTileInfos :: !(V.Vector TileInfo)
  , cdSpeedGrades :: !(V.Vector SpeedGrade)
  }

data LocationType = LocationType
  { ltBels :: !(V.Vector BelInfo)
  , ltWires :: !(V.Vector WireInfo)
  , ltPips :: !(V.Vector PipInfo)
  }

data BelWire = BelWire
  { bwRelDx :: !Int16
  , bwRelDy :: !Int16
  , bwWireIndex :: !Int32
  , bwPort :: !Int32
  , bwType :: !Int32
  }
  deriving (Eq, Show)

data BelInfo = BelInfo
  { biName :: !Text
  , biType :: !Int32
  , biZ :: !Int32
  , biBelWires :: !(V.Vector BelWire)
  }
  deriving (Eq, Show)

data PipInfo = PipInfo
  { piSrcRelDx :: !Int16
  , piSrcRelDy :: !Int16
  , piDstRelDx :: !Int16
  , piDstRelDy :: !Int16
  , piSrcIdx :: !Int16
  , piDstIdx :: !Int16
  , piTimingClass :: !Int16
  , piTileType :: !Int8
  , piPipType :: !Int8
  , piLutpermFlags :: !Int16
  }
  deriving (Eq, Show)

data PipLocator = PipLocator
  { plRelDx :: !Int16
  , plRelDy :: !Int16
  , plIndex :: !Int32
  }
  deriving (Eq, Show)

data BelPort = BelPort
  { bpRelDx :: !Int16
  , bpRelDy :: !Int16
  , bpBelIndex :: !Int32
  , bpPort :: !Int32
  }
  deriving (Eq, Show)

data WireInfo = WireInfo
  { wiName :: !Text
  , wiType :: !Int16
  , wiTileWire :: !Int16
  , wiPipsUphill :: !(V.Vector PipLocator)
  , wiPipsDownhill :: !(V.Vector PipLocator)
  , wiBelPins :: !(V.Vector BelPort)
  }
  deriving (Eq, Show)

data PioInfo = PioInfo
  { pioAbsDx :: !Int16
  , pioAbsDy :: !Int16
  , pioBelIndex :: !Int32
  , pioFunctionName :: !Text
  , pioBank :: !Int16
  , pioDqsGroup :: !Int16
  }
  deriving (Eq, Show)

data PackagePin = PackagePin
  { ppName :: !Text
  , ppAbsDx :: !Int16
  , ppAbsDy :: !Int16
  , ppBelIndex :: !Int32
  }
  deriving (Eq, Show)

data PackageInfo = PackageInfo
  { pkgName :: !Text
  , pkgPins :: !(V.Vector PackagePin)
  }
  deriving (Eq, Show)

data TileName = TileName
  { tnName :: !Text
  , tnTypeIdx :: !Int16
  }
  deriving (Eq, Show)

data TileInfo = TileInfo
  { tiTileNames :: !(V.Vector TileName)
  }
  deriving (Eq, Show)

data CellPropDelay = CellPropDelay
  { cpdFrom :: !Int32
  , cpdTo :: !Int32
  , cpdMin :: !Int32
  , cpdMax :: !Int32
  }
  deriving (Eq, Show)

data CellSetupHold = CellSetupHold
  { cshSigPort :: !Int32
  , cshClockPort :: !Int32
  , cshMinSetup :: !Int32
  , cshMaxSetup :: !Int32
  , cshMinHold :: !Int32
  , cshMaxHold :: !Int32
  }
  deriving (Eq, Show)

data CellTiming = CellTiming
  { ctCellType :: !Int32
  , ctPropDelays :: !(V.Vector CellPropDelay)
  , ctSetupHolds :: !(V.Vector CellSetupHold)
  }
  deriving (Eq, Show)

data PipDelay = PipDelay
  { pdMinBase :: !Int32
  , pdMaxBase :: !Int32
  , pdMinFanout :: !Int32
  , pdMaxFanout :: !Int32
  }
  deriving (Eq, Show)

data SpeedGrade = SpeedGrade
  { sgCellTimings :: !(V.Vector CellTiming)
  , sgPipClasses :: !(V.Vector PipDelay)
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Little-endian field readers over a strict ByteString at absolute offsets.

type Off = Int

u8 :: BS.ByteString -> Off -> Word8
u8 bs p = BS.index bs p

i8 :: BS.ByteString -> Off -> Int8
i8 bs p = fromIntegral (u8 bs p)

i16 :: BS.ByteString -> Off -> Int16
i16 bs p = fromIntegral (u8 bs p) .|. (fromIntegral (u8 bs (p + 1)) `shiftL` 8)

i32 :: BS.ByteString -> Off -> Int32
i32 bs p =
  -- The low word must be masked: a negative Int16 sign-extends into
  -- bits 16..31, which would clobber the high word when OR'd in (e.g.
  -- bytes [88 E3 22 FE] would read as 0xFFFFE388 instead of 0xFE22E388).
  -- Misread slice offsets/counts used to ask 'V.generate' for billions of
  -- elements and OOM the machine.
  (fromIntegral (i16 bs p) .&. 0xFFFF) .|. (fromIntegral (i16 bs (p + 2)) `shiftL` 16)

u32 :: BS.ByteString -> Off -> Int32
u32 bs p = i32 bs p

-- | A @RelSlice@ at offset p: its absolute element offset and length.
slice :: BS.ByteString -> Off -> (Off, Int)
slice bs p = (p + fromIntegral (i32 bs p), fromIntegral (u32 bs (p + 4)))

-- | A NUL-terminated @RelPtr<char>@ at offset p. A null pointer (offset
-- 0) yields the empty text; invalid UTF-8 is replaced (the C++ reads raw
-- chars; such names are never queried in practice).
cstr :: BS.ByteString -> Off -> Text
cstr bs p
  | rel == 0 = T.empty
  | otherwise =
      let start = p + fromIntegral rel
          len = BS.length (BS.takeWhile (/= 0) (BS.drop start bs))
       in decodeUtf8With lenientDecode (BS.take len (BS.drop start bs))
  where
    rel = i32 bs p

-- | Parse @count@ structs of size @sz@ starting at @base@ with element
-- parser @f@.
vec :: (Off -> a) -> Int -> Int -> Off -> V.Vector a
vec f sz count base = V.generate count (\i -> f (base + i * sz))

-- | Validate a slice lies inside the blob.
checkBounds :: BS.ByteString -> Off -> Int -> Either String ()
checkBounds bs off len =
  unless (off >= 0 && off + len <= BS.length bs) $
    Left ("slice out of bounds: offset=" ++ show off ++ " len=" ++ show len)

-- ---------------------------------------------------------------------------
-- Struct parsers. Sizes are the packed sizes from ecp5/arch.h.

parseBelWire :: BS.ByteString -> Off -> BelWire
parseBelWire bs p =
  BelWire
    { bwRelDx = i16 bs p
    , bwRelDy = i16 bs (p + 2)
    , bwWireIndex = i32 bs (p + 4)
    , bwPort = i32 bs (p + 8)
    , bwType = i32 bs (p + 12)
    }

parseBelInfo :: BS.ByteString -> Off -> BelInfo
parseBelInfo bs p =
  BelInfo
    { biName = cstr bs p
    , biType = i32 bs (p + 4)
    , biZ = i32 bs (p + 8)
    , biBelWires = parseSlice bs (p + 12) parseBelWire 16
    }

parsePipInfo :: BS.ByteString -> Off -> PipInfo
parsePipInfo bs p =
  PipInfo
    { piSrcRelDx = i16 bs p
    , piSrcRelDy = i16 bs (p + 2)
    , piDstRelDx = i16 bs (p + 4)
    , piDstRelDy = i16 bs (p + 6)
    , piSrcIdx = i16 bs (p + 8)
    , piDstIdx = i16 bs (p + 10)
    , piTimingClass = i16 bs (p + 12)
    , piTileType = i8 bs (p + 14)
    , piPipType = i8 bs (p + 15)
    , piLutpermFlags = i16 bs (p + 16)
    }

parsePipLocator :: BS.ByteString -> Off -> PipLocator
parsePipLocator bs p =
  PipLocator
    { plRelDx = i16 bs p
    , plRelDy = i16 bs (p + 2)
    , plIndex = i32 bs (p + 4)
    }

parseBelPort :: BS.ByteString -> Off -> BelPort
parseBelPort bs p =
  BelPort
    { bpRelDx = i16 bs p
    , bpRelDy = i16 bs (p + 2)
    , bpBelIndex = i32 bs (p + 4)
    , bpPort = i32 bs (p + 8)
    }

parseWireInfo :: BS.ByteString -> Off -> WireInfo
parseWireInfo bs p =
  WireInfo
    { wiName = cstr bs p
    , wiType = i16 bs (p + 4)
    , wiTileWire = i16 bs (p + 6)
    , wiPipsUphill = parseSlice bs (p + 8) parsePipLocator 8
    , wiPipsDownhill = parseSlice bs (p + 16) parsePipLocator 8
    , wiBelPins = parseSlice bs (p + 24) parseBelPort 12
    }

parseLocationType :: BS.ByteString -> Off -> LocationType
parseLocationType bs p =
  LocationType
    { ltBels = parseSlice bs (p + 0) parseBelInfo 20
    , ltWires = parseSlice bs (p + 8) parseWireInfo 32
    , ltPips = parseSlice bs (p + 16) parsePipInfo 20
    }

parsePioInfo :: BS.ByteString -> Off -> PioInfo
parsePioInfo bs p =
  PioInfo
    { pioAbsDx = i16 bs p
    , pioAbsDy = i16 bs (p + 2)
    , pioBelIndex = i32 bs (p + 4)
    , pioFunctionName = cstr bs (p + 8)
    , pioBank = i16 bs (p + 12)
    , pioDqsGroup = i16 bs (p + 14)
    }

parsePackagePin :: BS.ByteString -> Off -> PackagePin
parsePackagePin bs p =
  PackagePin
    { ppName = cstr bs p
    , ppAbsDx = i16 bs (p + 4)
    , ppAbsDy = i16 bs (p + 6)
    , ppBelIndex = i32 bs (p + 8)
    }

parsePackageInfo :: BS.ByteString -> Off -> PackageInfo
parsePackageInfo bs p =
  PackageInfo
    { pkgName = cstr bs p
    , pkgPins = parseSlice bs (p + 4) parsePackagePin 12
    }

parseTileName :: BS.ByteString -> Off -> TileName
parseTileName bs p =
  TileName
    { tnName = cstr bs p
    , tnTypeIdx = i16 bs (p + 4)
    }

parseTileInfo :: BS.ByteString -> Off -> TileInfo
parseTileInfo bs p =
  TileInfo
    { tiTileNames = parseSlice bs (p + 0) parseTileName 8
    }

parseCellPropDelay :: BS.ByteString -> Off -> CellPropDelay
parseCellPropDelay bs p =
  CellPropDelay
    { cpdFrom = i32 bs p
    , cpdTo = i32 bs (p + 4)
    , cpdMin = i32 bs (p + 8)
    , cpdMax = i32 bs (p + 12)
    }

parseCellSetupHold :: BS.ByteString -> Off -> CellSetupHold
parseCellSetupHold bs p =
  CellSetupHold
    { cshSigPort = i32 bs p
    , cshClockPort = i32 bs (p + 4)
    , cshMinSetup = i32 bs (p + 8)
    , cshMaxSetup = i32 bs (p + 12)
    , cshMinHold = i32 bs (p + 16)
    , cshMaxHold = i32 bs (p + 20)
    }

parseCellTiming :: BS.ByteString -> Off -> CellTiming
parseCellTiming bs p =
  CellTiming
    { ctCellType = i32 bs p
    , ctPropDelays = parseSlice bs (p + 4) parseCellPropDelay 16
    , ctSetupHolds = parseSlice bs (p + 12) parseCellSetupHold 24
    }

parsePipDelay :: BS.ByteString -> Off -> PipDelay
parsePipDelay bs p =
  PipDelay
    { pdMinBase = i32 bs p
    , pdMaxBase = i32 bs (p + 4)
    , pdMinFanout = i32 bs (p + 8)
    , pdMaxFanout = i32 bs (p + 12)
    }

parseSpeedGrade :: BS.ByteString -> Off -> SpeedGrade
parseSpeedGrade bs p =
  SpeedGrade
    { sgCellTimings = parseSlice bs (p + 0) parseCellTiming 20
    , sgPipClasses = parseSlice bs (p + 8) parsePipDelay 16
    }

-- | Parse a @RelSlice@ field: element offset relative to the slice field.
--
-- The slice is validated against the blob *before* any allocation: a
-- corrupt count (e.g. from a damaged blob) must fail with a clean error
-- instead of asking 'V.generate' for a billions-of-elements vector
-- (observed in the wild: a 4e9-element garbage count made the test
-- suite allocate tens of GB and OOM the machine).
parseSlice :: BS.ByteString -> Off -> (BS.ByteString -> Off -> a) -> Int -> V.Vector a
parseSlice bs p f sz =
  let (start, len) = slice bs p
      blobLen = BS.length bs
      total = toInteger start + toInteger len * toInteger sz
   in if len == 0
        -- bbasm emits (rel=garbage, len=0) for empty slices ("ref None");
        -- the offset is never dereferenced, so accept it.
        then V.empty
        else
          if start < 0 || total > toInteger blobLen
            then
              error
                ( "lambdapnr: chipdb slice out of bounds: field="
                    ++ show p ++ " start=" ++ show start
                    ++ " len=" ++ show len ++ " elemSize=" ++ show sz
                    ++ " blob=" ++ show blobLen
                )
            else vec (f bs) sz len start

-- | Parse the whole chipdb blob. The header is @ChipInfoPOD@ (80 bytes);
-- all child slices are resolved recursively.
parseChipdb :: BS.ByteString -> Either String Chipdb
parseChipdb bs = do
  -- The blob starts with a @RelPtr<ChipInfoPOD>@ (the C++ dereferences it
  -- via @get_chip_info@); the ChipInfoPOD itself is at that offset.
  checkBounds bs 0 4
  let hdr = fromIntegral (i32 bs 0)
  checkBounds bs hdr 80
  let width = fromIntegral (i32 bs (hdr + 0))
      height = fromIntegral (i32 bs (hdr + 4))
      numTiles = fromIntegral (i32 bs (hdr + 8))
      constIdCount = fromIntegral (i32 bs (hdr + 12))
  checkBounds bs hdr (width * height * 4)
  let (locStart, locLen) = slice bs (hdr + 16)
      (ltStart, ltLen) = slice bs (hdr + 24)
      (glbStart, glbLen) = slice bs (hdr + 32)
      (ttnStart, ttnLen) = slice bs (hdr + 40)
      (pkgStart, pkgLen) = slice bs (hdr + 48)
      (pioStart, pioLen) = slice bs (hdr + 56)
      (tilStart, tilLen) = slice bs (hdr + 64)
      (sgStart, sgLen) = slice bs (hdr + 72)
  checkBounds bs locStart (locLen * 24)
  checkBounds bs ltStart (ltLen * 4)
  checkBounds bs glbStart (glbLen * 8)
  checkBounds bs ttnStart (ttnLen * 4)
  checkBounds bs pkgStart (pkgLen * 12)
  checkBounds bs pioStart (pioLen * 16)
  checkBounds bs tilStart (tilLen * 8)
  checkBounds bs sgStart (sgLen * 16)
  when (ltLen /= numTiles) $
    Left ("num_tiles mismatch: header says " ++ show numTiles ++ ", location_type " ++ show ltLen)
  pure
    Chipdb
      { cdWidth = width
      , cdHeight = height
      , cdNumTiles = numTiles
      , cdConstIdCount = constIdCount
      , cdLocations = vec (parseLocationType bs) 24 locLen locStart
      , cdLocationType = V.map fromIntegral (vec (i32 bs) 4 ltLen ltStart)
      , cdTiletypeNames = vec (cstr bs) 4 ttnLen ttnStart
      , cdPackages = vec (parsePackageInfo bs) 12 pkgLen pkgStart
      , cdPios = vec (parsePioInfo bs) 16 pioLen pioStart
      , cdTileInfos = vec (parseTileInfo bs) 8 tilLen tilStart
      , cdSpeedGrades = vec (parseSpeedGrade bs) 16 sgLen sgStart
      }
