{- | Design checksum.

Mirror of @Context::checksum()@ (@common\/kernel\/context.cc@). The C++
implementation folds xorshift32 over nets and cells; all sub-folds are
sums, so iteration order does not affect the result — the Haskell
port's ordered maps are therefore exactly equivalent. The checksum is
the determinism oracle for the whole port (regression goldens compare
it), so the fold sequence must match the C++ bit-for-bit.
-}
module Lambdapnr.Kernel.Checksum (
    xorshift32,
    checksum,
    checksumNet,
    checksumCell,
) where

import Data.Bits (shiftL, shiftR, xor)
import Data.Char (ord)
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word32)

import Lambdapnr.Kernel.IdString (IdString (..))
import Lambdapnr.Kernel.Netlist
import Lambdapnr.Kernel.Property

-- | @xorshift32@ from context.cc.
xorshift32 :: Word32 -> Word32
xorshift32 x = x5
  where
    x1 = x `xor` (x `shiftL` 13)
    x2 = x1 `xor` (x1 `shiftR` 17)
    x5 = x2 `xor` (x2 `shiftL` 5)

hashIndex :: IdString -> Word32
hashIndex = fromIntegral . unIdString

hashChar :: Char -> Word32
hashChar = fromIntegral . ord

{- | The serialized property string (the C++ @Property::str@: LSB-first
bit string for numerics, literal for strings) — what the checksum and
JSON both hash.
-}
propStr :: Property -> Text
propStr (PropNum s _) = s
propStr (PropStr s) = s

{- | Per-net checksum fold, mirroring the C++ loop exactly: every raw
index enters as @xs32(x + xs32(index))@, the users chain into @x@
directly (order matters), and the attrs/wires are separate sums.
-}
checksumNet ::
    (wire -> Word32) ->
    (pip -> Word32) ->
    NetInfo bel wire pip ->
    Word32
checksumNet wireCk pipCk ni =
    let x1 = xorshift32 (123456789 + xorshift32 (hashIndex (netName ni))) -- net key
        x2 = xorshift32 (x1 + xorshift32 (hashIndex (netName ni))) -- ni.name
        x3 = maybe x2 (\c -> xorshift32 (x2 + xorshift32 (hashIndex c))) (prCell (netDriver ni))
        x4 = xorshift32 (x3 + xorshift32 (hashIndex (prPort (netDriver ni))))
        x5 = V.foldl' (\acc u -> maybe acc (step acc) u) x4 (netUsers ni)
        x6 = xorshift32 (x5 + xorshift32 (checksumAttrs (netAttrs ni)))
     in xorshift32 (x6 + xorshift32 (checksumWires wireCk pipCk (netWires ni)))
  where
    step acc u =
        let a = maybe acc (\c -> xorshift32 (acc + xorshift32 (hashIndex c))) (prCell u)
         in xorshift32 (a + xorshift32 (hashIndex (prPort u)))

checksumUsers :: Vector PortRef -> Word32
checksumUsers users =
    foldl' step 0 users
  where
    step acc u =
        let a = maybe acc (\c -> xorshift32 (acc + xorshift32 (hashIndex c))) (prCell u)
         in xorshift32 (a + xorshift32 (hashIndex (prPort u)))

{- | C++ pattern:
  attr_x_sum = 0
  for each attr:
    attr_x = xs32(123456789 + xs32(key.index))
    for each char: attr_x = xs32(attr_x + xs32(ch))
    attr_x_sum += attr_x
  returns attr_x_sum
-}
checksumAttrs :: Map IdString Property -> Word32
checksumAttrs attrs =
    foldl' (\acc (k, v) -> acc + attrX k v) 0 (M.toList attrs)
  where
    attrX k v =
        foldl'
            (\acc c -> xorshift32 (acc + xorshift32 (hashChar c)))
            (xorshift32 (123456789 + xorshift32 (hashIndex k)))
            (T.unpack (propStr v))

{- | C++ pattern:
  wire_x_sum = 0
  for each wire:
    wire_x = xs32(123456789 + xs32(wireChecksum(w)))
    wire_x = xs32(wire_x + xs32(pipChecksum(pip)))
    wire_x = xs32(wire_x + xs32(strength))
    wire_x_sum += wire_x
-}
checksumWires :: (wire -> Word32) -> (pip -> Word32) -> Map wire (PipMap pip) -> Word32
checksumWires wireCk pipCk wires =
    foldl' (\acc (w, pm) -> acc + wireX w pm) 0 (M.toList wires)
  where
    wireX w pm =
        let a = xorshift32 (123456789 + xorshift32 (wireCk w))
            -- the default (unbound) pip has index -1, like BelId/WireId
            b = xorshift32 (a + xorshift32 (maybe 0xFFFFFFFF pipCk (pmPip pm)))
         in xorshift32 (b + xorshift32 (fromIntegral (strengthToInt (pmStrength pm))))

{- | Per-cell checksum fold, mirroring the C++ loop exactly: every raw
index enters as @xs32(x + xs32(index))@; ports, attrs and params are
separate sums.
-}
checksumCell ::
    (bel -> Word32) ->
    CellInfo bel wire pip ->
    Word32
checksumCell belCk ci =
    let x1 = xorshift32 (123456789 + xorshift32 (hashIndex (cellName ci))) -- cell key
        x2 = xorshift32 (x1 + xorshift32 (hashIndex (cellName ci))) -- ci.name
        x3 = xorshift32 (x2 + xorshift32 (hashIndex (cellType ci)))
        x4 = xorshift32 (x3 + xorshift32 (checksumPorts (cellPorts ci)))
        x5 = xorshift32 (x4 + xorshift32 (checksumAttrs (cellAttrs ci)))
        x6 = xorshift32 (x5 + xorshift32 (checksumAttrs (cellParams ci)))
        x7 = xorshift32 (x6 + xorshift32 (maybe 0xFFFFFFFF belCk (cellBel ci)))
     in xorshift32 (x7 + xorshift32 (fromIntegral (strengthToInt (cellBelStrength ci))))

checksumPorts :: Map IdString PortInfo -> Word32
checksumPorts ports =
    foldl' (\acc (k, p) -> acc + portX k p) 0 (M.toList ports)
  where
    portX k p =
        let a = xorshift32 (123456789 + xorshift32 (hashIndex k))
            b = xorshift32 (a + xorshift32 (hashIndex (portName p)))
            c = maybe b (\n -> xorshift32 (b + xorshift32 (hashIndex n))) (portNet p)
         in xorshift32 (c + xorshift32 (fromIntegral (fromEnum (portType p))))

{- | Whole-design checksum:
  cksum = xs32(123456789)
  cksum = xs32(cksum + xs32(sum of net folds))
  cksum = xs32(cksum + xs32(sum of cell folds))
-}
checksum ::
    (bel -> Word32) ->
    (wire -> Word32) ->
    (pip -> Word32) ->
    Design bel wire pip ->
    Word32
checksum belCk wireCk pipCk d =
    let netsSum = foldl' (\acc ni -> acc + checksumNet wireCk pipCk ni) 0 (M.elems (designNets d))
        cellsSum = foldl' (\acc ci -> acc + checksumCell belCk ci) 0 (M.elems (designCells d))
        c0 = xorshift32 123456789
        c1 = xorshift32 (c0 + xorshift32 netsSum)
     in xorshift32 (c1 + xorshift32 cellsSum)
