{- | Interned string identifiers.

Mirror of nextpnr's @IdString@ (@common\/kernel\/idstring.h@): an @Int@
index into a per-context table of unique strings. All netlist names,
cell types, port names and attribute keys are 'IdString's, so equality
and hashing are index comparisons — the port's equivalent of the C++
pointer-into-string-table design.

Safety of 'idToText': the table is append-only and every update is a
single atomic swap, so any index ever returned by 'intern' resolves in
every later snapshot of the table. The @unsafePerformIO@ read is
therefore sound; interning itself stays in 'IO' (mirroring the C++
mutex-protected context).
-}
module Lambdapnr.Kernel.IdString (
    IdString (..),
    IdStringList,
    IdTable,
    newIdTable,
    intern,
    internStr,
    idToText,
    idToStr,
    idFromIndex,
    emptyId,
) where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.IO.Unsafe (unsafePerformIO)

-- | An interned string: a table index.
newtype IdString = IdString {unIdString :: Int}
    deriving (Eq, Ord, Show)

-- | Hierarchical name path (delimiter is arch-specific).
type IdStringList = [IdString]

-- | The empty id (index 0), interned at table creation like C++.
emptyId :: IdString
emptyId = IdString 0

-- | Per-context interning table.
data IdTable = IdTable (IORef (Map Text Int, V.Vector Text))

{- | Create a fresh table; @""@ is interned as index 0, matching the C++
@BaseCtx@ constructor.
-}
newIdTable :: IO IdTable
newIdTable = IdTable <$> newIORef (M.singleton T.empty 0, V.singleton T.empty)

-- | Intern a string, returning its (stable) index. Idempotent.
intern :: IdTable -> Text -> IO IdString
intern (IdTable ref) t = atomicModifyIORef' ref $ \(m, v) ->
    case M.lookup t m of
        Just i -> ((m, v), IdString i)
        Nothing ->
            let i = M.size m
             in ((M.insert t i m, V.snoc v t), IdString i)

-- | 'intern' over 'String'.
internStr :: IdTable -> String -> IO IdString
internStr tbl = intern tbl . T.pack

-- | Resolve an id back to its text.
idToText :: IdTable -> IdString -> Text
idToText (IdTable ref) (IdString i) =
    unsafePerformIO $ do
        (_, v) <- readIORef ref
        pure (v V.! i)

-- | 'idToText' over 'String'.
idToStr :: IdTable -> IdString -> String
idToStr tbl = T.unpack . idToText tbl

{- | Build an id from a raw index (used by chipdb loaders and arch code
that stores pre-interned indices).
-}
idFromIndex :: Int -> IdString
idFromIndex = IdString
