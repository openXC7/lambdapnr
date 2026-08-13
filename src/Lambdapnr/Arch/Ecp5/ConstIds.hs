{-# LANGUAGE OverloadedStrings #-}

-- | ECP5 constant-id table.
--
-- The chipdb stores bel/wire/pip types and ports as direct indices into
-- the constid table generated from @ecp5\/constids.inc@ (the C++ @ID_*
-- enum). The table must be interned in exactly that order (id 0 is the
-- empty string, ids 1..N follow the @X(...)@ lines), because the chipdb
-- encodes indices, not names.
module Lambdapnr.Arch.Ecp5.ConstIds
  ( constIdCount
  , parseConstIds
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

-- | Number of ids in the table, including the leading empty id
-- (@ID_NONE@ + @constids.inc@ entries). The chipdb records this as
-- @const_id_count@; the parsed table must match it.
constIdCount :: Int
constIdCount = 1846

-- | Parse @constids.inc@: the returned vector has the empty string at
-- index 0 followed by one entry per @X(NAME)@ line, in file order.
parseConstIds :: Text -> V.Vector Text
parseConstIds src =
  V.fromList ("" : [T.strip name | l <- T.lines src, Just name <- [parseLine l]])
  where
    parseLine l =
      case T.stripPrefix "X(" (T.strip l) of
        Just rest | Just name <- T.stripSuffix ")" rest -> Just name
        _ -> Nothing
