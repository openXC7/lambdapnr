{- | Property values: cell parameters and attributes.

Mirror of nextpnr's @Property@ (@common\/kernel\/property.h@). Numeric
values keep a canonical four-state bit string (@[01xz]@, LSB first) plus
the derived lower-64-bit integer; string values keep the literal text.
The bit string is the serialization used by the checksum and the JSON
frontend, so it must match the C++ construction exactly.
-}
module Lambdapnr.Kernel.Property (
    State (..),
    Property (..),
    propFromInt,
    propFromString,
    propFromState,
    propAsInt64,
    propAsString,
    propExtract,
    propToStr,
    propFromStr,
    propSize,
    propIsString,
) where

import Data.Bits (shiftL, (.&.), (.|.))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T

-- | Four-state logic bit (mirrors @Property::State@).
data State = S0 | S1 | Sx | Sz
    deriving (Eq, Ord, Show)

{- | A property value.

@PropNum@ carries the canonical LSB-first bit string and its lower-64-bit
integer (derived, as in C++). @PropStr@ carries a literal string.
Equality compares the constructor and bit string, matching the C++
@operator==@ (which compares @is_string@ and @str@).
-}
data Property
    = PropNum {pStr :: !Text, pInt :: !Int64}
    | PropStr {pStr :: !Text}
    deriving (Eq, Show)

propIsString :: Property -> Bool
propIsString PropStr{} = True
propIsString PropNum{} = False

{- | @Property(intval, width)@: bit @i@ of the string is bit @i@ of the
integer (LSB first), @width@ bits total.
-}
propFromInt :: Int64 -> Int -> Property
propFromInt intval width =
    PropNum (T.pack [if testBit intval i then '1' else '0' | i <- [0 .. width - 1]]) intval

testBit :: Int64 -> Int -> Bool
testBit v i = (v .&. (1 `shiftL` i)) /= 0

-- | @Property(const std::string&)@: a literal string value.
propFromString :: Text -> Property
propFromString = PropStr

-- | @Property(State)@: a one-bit numeric property.
propFromState :: State -> Property
propFromState S1 = PropNum (T.singleton '1') 1
propFromState S0 = PropNum (T.singleton '0') 0
propFromState Sx = PropNum (T.singleton 'x') 0
propFromState Sz = PropNum (T.singleton 'z') 0

-- | @as_int64()@: the integer value of a numeric property.
propAsInt64 :: Property -> Int64
propAsInt64 (PropNum _ i) = i
propAsInt64 PropStr{} = error "Property.as_int64: string property"

-- | @as_string()@: the literal of a string property.
propAsString :: Property -> Text
propAsString (PropStr s) = s
propAsString PropNum{} = error "Property.as_string: numeric property"

{- | @extract(offset, len, padding)@: slice of the bit string (numeric
result, padding used past the end).
-}
propExtract :: Int -> Int -> State -> Property -> Property
propExtract offset len pad p@(PropNum s _) =
    let padCh = stateChar pad
        chars = [T.index s i | i <- [offset .. offset + len - 1], i < T.length s]
        padded = T.pack chars <> T.replicate (max 0 (len - length chars)) (T.singleton padCh)
     in PropNum padded (updateIntval padded)
propExtract _ _ _ PropStr{} = error "Property.extract: string property"

{- | @to_string()@: numeric values print MSB first; string values get a
trailing space when they look like binary strings (disambiguation rule,
matching the C++ state machine: escape iff the string is @[01xz]*@ or
@[01xz]* + trailing spaces@).
-}
propToStr :: Property -> Text
propToStr (PropNum s _) = T.reverse s
propToStr (PropStr s)
    | isBinaryLike s = s <> T.singleton ' '
    | otherwise = s
  where
    isBinaryLike t =
        T.all isBinCh t
            || let (_, rest) = T.span isBinCh t in T.all (== ' ') rest

{- | @from_string()@: inverse of 'propToStr'. Matches the C++ exactly:
all-binary input becomes a numeric property; binary-plus-trailing-spaces
becomes a string property with exactly one trailing character stripped;
anything else is a literal string.
-}
propFromStr :: Text -> Property
propFromStr s
    | T.all isBinCh s = PropNum (T.reverse s) (updateIntval (T.reverse s))
    | T.all isBinCh (T.dropWhileEnd (== ' ') s) && T.any (== ' ') s && not (T.null s) =
        PropStr (T.init s)
    | otherwise = PropStr s

-- | @size()@: bits for numeric values, bytes for strings.
propSize :: Property -> Int
propSize (PropNum s _) = T.length s
propSize (PropStr s) = 8 * T.length s

isBinCh :: Char -> Bool
isBinCh c = c == '0' || c == '1' || c == 'x' || c == 'z'

stateChar :: State -> Char
stateChar S0 = '0'
stateChar S1 = '1'
stateChar Sx = 'x'
stateChar Sz = 'z'

{- | Recompute the lower-64-bit integer from the bit string (LSB first);
only @1@ bits count, mirroring @update_intval@.
-}
updateIntval :: Text -> Int64
updateIntval s = foldl' step 0 (zip [0 ..] (T.unpack s))
  where
    step acc (i, c)
        | c == '1' && i < 64 = acc .|. (1 `shiftL` fromIntegral i)
        | otherwise = acc
