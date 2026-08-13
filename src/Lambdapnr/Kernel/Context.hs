{- | The context: id table + architecture + design + settings + RNG.

Mirror of @Context@ (@common\/kernel\/context.h@), in pure form: phases
(pack/place/route) are pure transformations on this record; the mutable
front-end (CLI, python, GUI) will wrap it in an 'IORef'. Setting names
and design keys are 'IdString's interned through the context's
'IdTable'.
-}
module Lambdapnr.Kernel.Context (
    Context (..),
    newContext,
    newContextWith,
    getSetting,
    setSetting,
    ctxChecksum,
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)

import Lambdapnr.Kernel.Arch (Arch (..), Bel, Pip, Wire)
import Lambdapnr.Kernel.Checksum (checksum)
import Lambdapnr.Kernel.DeterministicRng (Rng, newRng)
import Lambdapnr.Kernel.IdString (IdString, IdTable, intern, newIdTable)
import Lambdapnr.Kernel.Netlist (Design, emptyDesign)
import Lambdapnr.Kernel.Property (Property (..), propFromStr)
import Text.Read (readMaybe)

-- | Everything the kernel algorithms need, threaded explicitly.
data Context a = Context
    { ctxIdTable :: IdTable
    , ctxArch :: a
    , ctxSettings :: Map IdString Property
    , ctxDesign :: Design (Bel a) (Wire a) (Pip a)
    , ctxRng :: Rng
    }

-- | A fresh context with a fresh id table.
newContext :: (Arch a) => a -> IO (Context a)
newContext arch = newContextWith <$> newIdTable <*> pure arch

{- | A context built over an existing id table (chipdbs intern their
names up front and share the table).
-}
newContextWith :: IdTable -> a -> Context a
newContextWith tbl arch =
    Context
        { ctxIdTable = tbl
        , ctxArch = arch
        , ctxSettings = M.empty
        , ctxDesign = emptyDesign
        , ctxRng = newRng
        }

{- | Typed setting access, mirroring @setting<T>(name, default)@: the
default is stored on first read, exactly like the C++ non-const
overload. The setting name is interned on first use.
-}
getSetting :: (Read a, Show a) => Context arch -> Text -> a -> IO (a, Context arch)
getSetting ctx name def = do
    key <- intern (ctxIdTable ctx) name
    case M.lookup key (ctxSettings ctx) of
        Just p -> pure (readSetting p, ctx)
        Nothing ->
            let p = defaultSetting def
                ctx' = ctx{ctxSettings = M.insert key p (ctxSettings ctx)}
             in pure (def, ctx')

-- | Raw setting write (mirrors @ctx->settings[id] = value@).
setSetting :: Context arch -> Text -> Property -> IO (Context arch)
setSetting ctx name p = do
    key <- intern (ctxIdTable ctx) name
    pure (ctx{ctxSettings = M.insert key p (ctxSettings ctx)})

{- | The design checksum — the determinism oracle (see
'Lambdapnr.Kernel.Checksum').
-}
ctxChecksum :: (Arch a) => Context a -> Word32
ctxChecksum ctx =
    checksum
        (getBelChecksum (ctxArch ctx))
        (getWireChecksum (ctxArch ctx))
        (getPipChecksum (ctxArch ctx))
        (ctxDesign ctx)

-- helpers ----------------------------------------------------------------

-- | Parse a stored setting back to a typed value: numeric properties
-- decode through their integer value (like the C++ @as_int64@), string
-- properties through 'read'.
readSetting :: (Read a) => Property -> a
readSetting (PropNum _ i) =
    maybe (error ("lambdapnr: cannot read numeric setting " ++ show i)) id (readMaybe (show i))
readSetting (PropStr s) =
    -- string properties: numeric targets parse the bare text; String/Text
    -- targets need the quoted literal form, so fall back to 'show'
    maybe
        ( maybe
            (error ("lambdapnr: cannot read setting " ++ show s))
            id
            (readMaybe (show s))
        )
        id
        (readMaybe (T.unpack s))

-- | Store a default as a string property.
defaultSetting :: (Show a) => a -> Property
defaultSetting = propFromStr . T.pack . show
