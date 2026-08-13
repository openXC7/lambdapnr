{-# LANGUAGE OverloadedStrings #-}

-- | ECP5 architecture: id types and arguments.
--
-- Mirror of @ecp5\/archdefs.h@ and the id definitions in @ecp5\/arch.h@:
-- every bel/wire/pip id is a (location, index) pair, where the index is
-- into the data of the location's tile *type* (the chipdb deduplicates
-- tile types).
module Lambdapnr.Arch.Ecp5.Types
  ( Location (..)
  , BelId (..)
  , WireId (..)
  , PipId (..)
  , GroupId (..)
  , Ecp5Device (..)
  , SpeedGrade (..)
  , Ecp5Args (..)
  , locAdd
  , deviceName
  , speedToInt
  ) where

import Data.Int (Int16, Int32)
import Data.Text (Text)

-- | Tile location (@LocationPOD@: two signed 16-bit coordinates).
data Location = Location
  { locX :: !Int16
  , locY :: !Int16
  }
  deriving (Eq, Ord, Show)

locAdd :: Location -> Location -> Location
locAdd (Location x y) (Location dx dy) = Location (x + dx) (y + dy)

-- | @BelId@: location + index into the tile type's bel data.
data BelId = BelId
  { belLoc :: !Location
  , belIdx :: !Int32
  }
  deriving (Eq, Ord, Show)

-- | @WireId@.
data WireId = WireId
  { wireLoc :: !Location
  , wireIdx :: !Int32
  }
  deriving (Eq, Ord, Show)

-- | @PipId@.
data PipId = PipId
  { pipLoc :: !Location
  , pipIdx :: !Int32
  }
  deriving (Eq, Ord, Show)

-- | @GroupId@ (switchbox groups; only the tag is needed for now).
data GroupId = GroupId
  deriving (Eq, Ord, Show)

-- | Device family (@ArchArgs::ArchArgsTypes@).
data Ecp5Device
  = Lfe5u12f
  | Lfe5u25f
  | Lfe5u45f
  | Lfe5u85f
  | Lfe5um25f
  | Lfe5um45f
  | Lfe5um85f
  | Lfe5um5g25f
  | Lfe5um5g45f
  | Lfe5um5g85f
  deriving (Eq, Ord, Show, Enum)

-- | Speed grade (@SPEED_6 = 0 .. SPEED_8_5G@).
data SpeedGrade = Speed6 | Speed7 | Speed8 | Speed85g
  deriving (Eq, Ord, Show, Enum)

speedToInt :: SpeedGrade -> Int
speedToInt = fromEnum
{-# INLINE speedToInt #-}

-- | Device name as printed by @getChipName@.
deviceName :: Ecp5Device -> Text
deviceName d = case d of
  Lfe5u12f -> "LFE5U-12F"
  Lfe5u25f -> "LFE5U-25F"
  Lfe5u45f -> "LFE5U-45F"
  Lfe5u85f -> "LFE5U-85F"
  Lfe5um25f -> "LFE5UM-25F"
  Lfe5um45f -> "LFE5UM-45F"
  Lfe5um85f -> "LFE5UM-85F"
  Lfe5um5g25f -> "LFE5UM5G-25F"
  Lfe5um5g45f -> "LFE5UM5G-45F"
  Lfe5um5g85f -> "LFE5UM5G-85F"

-- | Architecture arguments (device, package, speed).
data Ecp5Args = Ecp5Args
  { eaDevice :: !Ecp5Device
  , eaPackage :: !Text
  , eaSpeed :: !SpeedGrade
  }
  deriving (Eq, Show)
