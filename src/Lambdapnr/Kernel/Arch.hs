{-# LANGUAGE TypeFamilies #-}

{- | The Arch typeclass: the Haskell mirror of the C++ @ArchAPI@ template
(@common\/kernel\/arch_api.h@). Each family (generic, ice40, ecp5, …)
instantiates it with its own bel/wire/pip/group id types and chipdb.

This first iteration covers the core surface needed by the kernel
algorithms (config, bels, wires, pips, delay model, placement validity,
flow). The GUI/decal, cluster and resource methods (SPECIFICATION.md
§2.2) arrive with the first concrete arch.
-}
module Lambdapnr.Kernel.Arch (
    Arch (..),
    DelayQuad,
    Loc (..),
    BoundingBox (..),
) where

import Data.Text (Text)
import Data.Word (Word32)

import Lambdapnr.Kernel.Delay (DelayPair, DelayQuad, DelayT, dpFromDelay, dqFromDelay)
import Lambdapnr.Kernel.IdString (IdString (..), IdStringList)
import Lambdapnr.Kernel.Netlist (CellInfo, PlaceStrength, PortDir, PortRef)
import Lambdapnr.Kernel.Timing (ClockEdge (..), TimingClockingInfo (..), TimingPortClass (..))

-- | Tile coordinate.
data Loc = Loc
    { locX :: !Int
    , locY :: !Int
    , locZ :: !Int
    }
    deriving (Eq, Ord, Show)

-- | Bounding box in tile coordinates.
data BoundingBox = BoundingBox
    { bbX0 :: !Int
    , bbY0 :: !Int
    , bbX1 :: !Int
    , bbY1 :: !Int
    }
    deriving (Eq, Ord, Show)

{- | The architecture interface (see SPECIFICATION.md §2.2 for the full
method inventory).
-}
class Arch a where
    -- Architecture-specific id types
    type Bel a
    type Wire a
    type Pip a
    type Group a

    -- Basic config --------------------------------------------------------
    archId :: a -> IdString
    getChipName :: a -> Text
    getGridDimX :: a -> Int
    getGridDimY :: a -> Int
    getTileBelDimZ :: a -> Int -> Int -> Int
    getTilePipDimZ :: a -> Int -> Int -> Int
    getNameDelimiter :: a -> Char

    -- Bels ----------------------------------------------------------------
    getBels :: a -> [Bel a]
    getBelName :: a -> Bel a -> IdStringList
    getBelByName :: a -> IdStringList -> Maybe (Bel a)
    getBelLocation :: a -> Bel a -> Loc
    getBelByLocation :: a -> Loc -> Maybe (Bel a)
    getBelsByTile :: a -> Int -> Int -> [Bel a]
    getBelType :: a -> Bel a -> IdString
    getBelPins :: a -> Bel a -> [IdString]
    getBelPinWire :: a -> Bel a -> IdString -> Maybe (Wire a)
    getBelPinType :: a -> Bel a -> IdString -> PortDir
    getBelChecksum :: a -> Bel a -> Word32
    getBelGlobalBuf :: a -> Bel a -> Bool
    getBelHidden :: a -> Bel a -> Bool
    checkBelAvail :: a -> Bel a -> Bool
    getBoundBelCell :: a -> Bel a -> Maybe IdString
    getConflictingBelCell :: a -> Bel a -> Maybe IdString

    -- Wires ---------------------------------------------------------------
    getWires :: a -> [Wire a]
    getWireName :: a -> Wire a -> IdStringList
    getWireByName :: a -> IdStringList -> Maybe (Wire a)
    getWireType :: a -> Wire a -> IdString
    getWireChecksum :: a -> Wire a -> Word32
    getWireDelay :: a -> Wire a -> DelayQuad
    getPipsDownhill :: a -> Wire a -> [Pip a]
    getPipsUphill :: a -> Wire a -> [Pip a]
    getWireBelPins :: a -> Wire a -> [(Bel a, IdString)]
    checkWireAvail :: a -> Wire a -> Bool
    getBoundWireNet :: a -> Wire a -> Maybe IdString

    -- Pips ----------------------------------------------------------------
    getPips :: a -> [Pip a]
    getPipName :: a -> Pip a -> IdStringList
    getPipByName :: a -> IdStringList -> Maybe (Pip a)
    getPipType :: a -> Pip a -> IdString
    getPipChecksum :: a -> Pip a -> Word32
    getPipDelay :: a -> Pip a -> DelayQuad
    getPipSrcWire :: a -> Pip a -> Wire a
    getPipDstWire :: a -> Pip a -> Wire a
    getPipLocation :: a -> Pip a -> Loc
    isPipInverting :: a -> Pip a -> Bool
    checkPipAvail :: a -> Pip a -> Bool
    checkPipAvailForNet :: a -> Pip a -> IdString -> Bool
    getBoundPipNet :: a -> Pip a -> Maybe IdString

    -- Delay model ---------------------------------------------------------

    {- | Placement cost oracle: predicted delay between a source bel pin and
    a sink bel pin.
    -}
    predictDelay :: a -> Bel a -> IdString -> Bel a -> IdString -> DelayT

    -- | A* heuristic: estimated routing delay between two wires.
    estimateDelay :: a -> Wire a -> Wire a -> DelayT

    getDelayEpsilon :: a -> DelayT
    getRipupDelayPenalty :: a -> DelayT
    getDelayNS :: a -> DelayT -> Double
    getDelayFromNS :: a -> Double -> DelayT
    getRouteBoundingBox :: a -> Wire a -> Wire a -> BoundingBox
    expandBoundingBox :: a -> BoundingBox -> BoundingBox

    -- Placement validity --------------------------------------------------
    isValidBelForCellType :: a -> IdString -> Bel a -> Bool
    isBelLocationValid :: a -> Bel a -> Bool
    getCellTypes :: a -> [IdString]
    getBelBucketForCellType :: a -> IdString -> IdString
    getBelBucketForBel :: a -> Bel a -> IdString
    getBelsInBucket :: a -> IdString -> [Bel a]

    -- Cell timing ---------------------------------------------------------

    {- | Combinational delay of a cell arc, if the cell has one. The C++
    @getCellDelay@ returns false for register arcs (those are
    clock-to-Q, reported via 'getPortClockingInfo').
    -}
    getCellDelay :: a -> CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> IdString -> Maybe DelayQuad

    -- | Timing class of a cell port + number of associated clocks.
    getPortTimingClass :: a -> CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> (TimingPortClass, Int)

    -- | Clocking information (setup/hold/clock-to-Q) of a register port.
    getPortClockingInfo :: a -> CellInfo (Bel a) (Wire a) (Pip a) -> IdString -> Int -> TimingClockingInfo

    -- Flow ----------------------------------------------------------------
    pack :: a -> IO Bool
    place :: a -> IO Bool
    route :: a -> IO Bool

    -- Defaults (mirroring @BaseArch@ trivial implementations) -------------
    getTilePipDimZ _ _ _ = 1
    getNameDelimiter _ = ' '
    getBelGlobalBuf _ _ = False
    getBelHidden _ _ = False
    getBelChecksum _ _ = 0
    getWireType _ _ = emptyIdString
    getWireChecksum _ _ = 0
    getPipType _ _ = emptyIdString
    getPipChecksum _ _ = 0
    isPipInverting _ _ = False
    checkPipAvailForNet a p n = checkPipAvail a p || getBoundPipNet a p == Just n
    getConflictingBelCell a b = getBoundBelCell a b
    expandBoundingBox a (BoundingBox x0 y0 x1 y1) =
        BoundingBox
            (max 0 (x0 - 1))
            (max 0 (y0 - 1))
            (min (getGridDimX a) (x1 + 1))
            (min (getGridDimY a) (y1 + 1))
    getCellTypes a = []
    getBelBucketForCellType _ t = t
    getBelBucketForBel a b = getBelBucketForCellType a (getBelType a b)
    getBelsInBucket a b = filter (\bel -> getBelBucketForBel a bel == b) (getBels a)
    -- @BaseArch@ defaults: no combinational arcs, all ports ignored, and
    -- an empty clocking info record.
    getCellDelay _ _ _ _ = Nothing
    getPortTimingClass _ _ _ = (TmgIgnore, 0)
    getPortClockingInfo _ _ _ _ =
        TimingClockingInfo
            emptyIdString
            RisingEdge
            (dpFromDelay 0)
            (dpFromDelay 0)
            (dqFromDelay 0)
    pack _ = pure False
    place _ = pure False
    route _ = pure False

-- | Re-exported port direction (the Arch API uses it for bel pins).
emptyIdString :: IdString
emptyIdString = Lambdapnr.Kernel.IdString.IdString 0
