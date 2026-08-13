{-# LANGUAGE OverloadedStrings #-}

{- | ECP5 binding state: bel/cell, wire/net and pip/net maps plus the
wire fanout counter, mirroring @BaseArch@ (@base_bel2cell@,
@base_wire2net@, @base_pip2net@) and the ecp5 @wire_fanout@ vector.

The C++ arch API mutates these maps in place; the Haskell port threads
them explicitly. Each operation mirrors the exact C++ side effects
(@ecp5\/arch.h@ @bindBel@\/@unbindBel@\/@bindWire@\/@unbindWire@\/@bindPip@\/@unbindPip@),
including the design-side bookkeeping (@net->wires@), so the two
implementations agree on observable state:

* 'bindPip' also binds the pip's destination wire to the net (the core
  invariant: a bound pip implies a bound dst wire), bumps the source
  wire's fanout (used by 'getPipDelay' fanout adders), and records the
  uphill pip of the dst wire;
* 'unbindWire' of a pip-bound wire additionally unbinds the uphill pip
  and decrements the fanout, exactly like the C++.
-}
module Lambdapnr.Arch.Ecp5.Binding (
    BindState (..),
    emptyBindState,
    bindBel,
    unbindBel,
    bindWire,
    unbindWire,
    bindPip,
    unbindPip,
    wireFanoutOf,
    boundBelCell,
    boundWireNet,
    boundPipNet,
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Lambdapnr.Arch.Ecp5.Chipdb (Chipdb (..), PipInfo (..), pipAt)
import Lambdapnr.Arch.Ecp5.Types (BelId (..), Location (..), PipId (..), WireId (..), locAdd)
import Lambdapnr.Kernel.IdString (IdString)
import Lambdapnr.Kernel.Netlist (Design, PlaceStrength, clearCellBel, removeNetWire, setCellBel, setNetWire)

-- | The binding maps. All keyed by the id types directly (the C++ flat
-- vectors indexed by tile-base offsets are a memory optimization; the
-- semantics are identical).
data BindState = BindState
    { bsBel2Cell :: !(Map BelId IdString)
    , bsWire2Net :: !(Map WireId IdString)
    , bsPip2Net :: !(Map PipId IdString)
    , bsWireFanout :: !(Map WireId Int)
    -- ^ src wire -> number of bound pips leaving it (nonzero entries)
    , bsWirePip :: !(Map WireId PipId)
    -- ^ dst wire -> uphill pip (the arch-side mirror of @net->wires@)
    }
    deriving (Eq, Show)

emptyBindState :: BindState
emptyBindState = BindState M.empty M.empty M.empty M.empty M.empty

-- | Fanout of a wire (0 when nothing is bound).
wireFanoutOf :: WireId -> BindState -> Int
wireFanoutOf w = M.findWithDefault 0 w . bsWireFanout

-- | The cell bound to a bel.
boundBelCell :: BelId -> BindState -> Maybe IdString
boundBelCell b = M.lookup b . bsBel2Cell

-- | The net bound to a wire.
boundWireNet :: WireId -> BindState -> Maybe IdString
boundWireNet w = M.lookup w . bsWire2Net

-- | The net bound to a pip.
boundPipNet :: PipId -> BindState -> Maybe IdString
boundPipNet p = M.lookup p . bsPip2Net

{- | @bindBel(cell, bel, strength)@: records the binding and sets the
cell's bel. C++ asserts the bel is free; the Haskell port is total and
overwrites (callers guard with 'boundBelCell').
-}
bindBel :: IdString -> BelId -> PlaceStrength -> BindState -> Design BelId WireId PipId -> (BindState, Design BelId WireId PipId)
bindBel cell bel strength st d =
    (st{bsBel2Cell = M.insert bel cell (bsBel2Cell st)}, setCellBel cell bel strength d)

-- | @unbindBel(cell, bel)@: clears both sides.
unbindBel :: IdString -> BelId -> BindState -> Design BelId WireId PipId -> (BindState, Design BelId WireId PipId)
unbindBel cell bel st d =
    (st{bsBel2Cell = M.delete bel (bsBel2Cell st)}, clearCellBel cell d)

{- | @bindWire(net, wire, strength)@: binds the wire with no uphill pip
and records the (empty) wire entry in the net.
-}
bindWire :: IdString -> WireId -> PlaceStrength -> BindState -> Design BelId WireId PipId -> (BindState, Design BelId WireId PipId)
bindWire net wire strength st d =
    ( st
        { bsWire2Net = M.insert wire net (bsWire2Net st)
        , bsWirePip = M.delete wire (bsWirePip st)
        },
      setNetWire net wire Nothing strength d
    )

{- | @unbindWire(wire)@: if the wire has an uphill pip, the pip and the
source-wire fanout are also cleared (the C++ reads the pip from
@net->wires@; here from the arch-side mirror, which 'bindPip' keeps in
sync).
-}
unbindWire :: Chipdb -> WireId -> BindState -> Design BelId WireId PipId -> (BindState, Design BelId WireId PipId)
unbindWire cd wire st d =
    let net = boundWireNet wire st
        st1 = st{bsWire2Net = M.delete wire (bsWire2Net st), bsWirePip = M.delete wire (bsWirePip st)}
        st2 = case M.lookup wire (bsWirePip st) of
            Just pip -> pipCleanup cd pip st1
            Nothing -> st1
        d1 = maybe d (\x -> removeNetWire x wire d) net
     in (st2, d1)
  where
    -- shared pip-side cleanup: unbind the pip and decrement the fanout
    -- of its source wire
    pipCleanup cd' pip st' =
        st'
            { bsPip2Net = M.delete pip (bsPip2Net st')
            , bsWireFanout = bump (-1) (srcOf cd' pip) (bsWireFanout st')
            }

{- | @bindPip(net, pip, strength)@: the core binding operation. The
source wire's fanout is incremented, the pip is bound, and the
destination wire is bound to the net with this pip as its uphill pip.
-}
bindPip :: Chipdb -> IdString -> PipId -> PlaceStrength -> BindState -> Design BelId WireId PipId -> (BindState, Design BelId WireId PipId)
bindPip cd net pip strength st d =
    let src = srcOf cd pip
        dst = dstOf cd pip
        st1 =
            st
                { bsWireFanout = bump 1 src (bsWireFanout st)
                , bsPip2Net = M.insert pip net (bsPip2Net st)
                , bsWire2Net = M.insert dst net (bsWire2Net st)
                , bsWirePip = M.insert dst pip (bsWirePip st)
                }
     in (st1, setNetWire net dst (Just pip) strength d)

-- | @unbindPip(pip)@: inverse of 'bindPip'.
unbindPip :: Chipdb -> PipId -> BindState -> Design BelId WireId PipId -> (BindState, Design BelId WireId PipId)
unbindPip cd pip st d =
    let dst = dstOf cd pip
        net = boundPipNet pip st
        st1 =
            st
                { bsPip2Net = M.delete pip (bsPip2Net st)
                , bsWireFanout = bump (-1) (srcOf cd pip) (bsWireFanout st)
                , bsWire2Net = M.delete dst (bsWire2Net st)
                , bsWirePip = M.delete dst (bsWirePip st)
                }
        d1 = maybe d (\x -> removeNetWire x dst d) net
     in (st1, d1)

-- helpers ----------------------------------------------------------------

-- | Source wire of a pip (chipdb lookup).
srcOf :: Chipdb -> PipId -> WireId
srcOf cd p =
    let pi = pipAt cd p
     in WireId{wireLoc = locAdd (pipLoc p) (Location (piSrcRelDx pi) (piSrcRelDy pi)), wireIdx = fromIntegral (piSrcIdx pi)}

-- | Destination wire of a pip (chipdb lookup).
dstOf :: Chipdb -> PipId -> WireId
dstOf cd p =
    let pi = pipAt cd p
     in WireId{wireLoc = locAdd (pipLoc p) (Location (piDstRelDx pi) (piDstRelDy pi)), wireIdx = fromIntegral (piDstIdx pi)}

-- | Add @delta@ to a wire's fanout, dropping zero entries.
bump :: Int -> WireId -> Map WireId Int -> Map WireId Int
bump delta w m =
    let v = M.findWithDefault 0 w m + delta
     in if v == 0 then M.delete w m else M.insert w v m
