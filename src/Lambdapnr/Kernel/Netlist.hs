{- | The netlist model: cells, nets, ports and connections.

Mirror of nextpnr's @CellInfo@\/@NetInfo@\/@PortInfo@\/@PortRef@
(@common\/kernel\/nextpnr_types.h@). The design is keyed by 'IdString'
like the C++ @dict@, and parameterized over the architecture's bel,
wire and pip id types (the C++ template does the same via
@archdefs.h@). Everything is immutable and threaded explicitly so
pack/place/route phases can be pure whole-design transformations
(SPECIFICATION.md §7.2).
-}
module Lambdapnr.Kernel.Netlist (
    PlaceStrength (..),
    PortDir (..),
    PortRef (..),
    PortInfo (..),
    PipMap (..),
    NetInfo (..),
    CellInfo (..),
    Design (..),
    emptyDesign,
    lookupCell,
    lookupNet,
    addCell,
    addNet,
    connectPort,
    disconnectPort,
    setCellBel,
    clearCellBel,
    setCellPort,
    setNetWire,
    removeNetWire,
    strengthToInt,
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Vector (Vector)
import qualified Data.Vector as V

import Lambdapnr.Kernel.IdString (IdString (..), emptyId)
import Lambdapnr.Kernel.Property (Property)

-- | 'IdString' with no text (the empty id, index 0).
emptyIdString :: IdString
emptyIdString = emptyId

{- | Binding strength, matching the C++ enum order exactly (it is part of
the checksum serialization).
-}
data PlaceStrength
    = StrengthNone
    | StrengthWeak
    | StrengthStrong
    | StrengthPlacer
    | StrengthFixed
    | StrengthLocked
    | StrengthUser
    deriving (Eq, Ord, Show, Enum)

strengthToInt :: PlaceStrength -> Int
strengthToInt = fromEnum

-- | Bind a cell to a bel (@cell->bel = bel@, @cell->belStrength@).
setCellBel :: IdString -> bel -> PlaceStrength -> Design bel wire pip -> Design bel wire pip
setCellBel cell bel strength d =
    case M.lookup cell (designCells d) of
        Nothing -> d
        Just ci -> d{designCells = M.insert cell (ci{cellBel = Just bel, cellBelStrength = strength}) (designCells d)}

-- | Create (or overwrite) a cell port entry, mirroring the C++
-- @ci->ports[id] = {name, type}@ before @connectPort@.
setCellPort :: IdString -> IdString -> PortDir -> Design bel wire pip -> Design bel wire pip
setCellPort cell port dir d =
    case M.lookup cell (designCells d) of
        Nothing -> d
        Just ci ->
            let pi = PortInfo{portName = port, portNet = Nothing, portType = dir, portUserIdx = 0}
             in d{designCells = M.insert cell (ci{cellPorts = M.insert port pi (cellPorts ci)}) (designCells d)}

-- | Clear a cell's bel binding (@cell->bel = BelId()@).
clearCellBel :: IdString -> Design bel wire pip -> Design bel wire pip
clearCellBel cell d =
    case M.lookup cell (designCells d) of
        Nothing -> d
        Just ci -> d{designCells = M.insert cell (ci{cellBel = Nothing, cellBelStrength = StrengthNone}) (designCells d)}

{- | Insert (or replace) a wire entry in a net's wires map, mirroring
@net->wires[wire] = {pip, strength}@.
-}
setNetWire :: (Ord wire) => IdString -> wire -> Maybe pip -> PlaceStrength -> Design bel wire pip -> Design bel wire pip
setNetWire net wire pip strength d =
    case M.lookup net (designNets d) of
        Nothing -> d
        Just ni -> d{designNets = M.insert net (ni{netWires = M.insert wire (PipMap pip strength) (netWires ni)}) (designNets d)}

-- | Remove a wire from a net's wires map (@net->wires.erase(wire)@).
removeNetWire :: (Ord wire) => IdString -> wire -> Design bel wire pip -> Design bel wire pip
removeNetWire net wire d =
    case M.lookup net (designNets d) of
        Nothing -> d
        Just ni -> d{designNets = M.insert net (ni{netWires = M.delete wire (netWires ni)}) (designNets d)}

{- | Port direction. The 'Enum' order matches the C++ @PortType@
(@PORT_IN = 0, PORT_OUT = 1, PORT_INOUT = 2@) — it is part of the
checksum serialization.
-}
data PortDir = PortIn | PortOut | PortInout
    deriving (Eq, Ord, Show, Enum)

{- | A reference to a cell port (driver or user). @prCell = Nothing@ is a
dangling reference (C++ nullptr).
-}
data PortRef = PortRef
    { prCell :: !(Maybe IdString)
    , prPort :: !IdString
    }
    deriving (Eq, Show)

-- | One port of a cell.
data PortInfo = PortInfo
    { portName :: !IdString
    , portNet :: !(Maybe IdString)
    , portType :: !PortDir
    , portUserIdx :: !Int
    -- ^ index into the owning net's users vector
    }
    deriving (Eq, Show)

-- | A wire binding within a net: the uphill pip (if any) and strength.
data PipMap pip = PipMap
    { pmPip :: !(Maybe pip)
    , pmStrength :: !PlaceStrength
    }
    deriving (Eq, Show)

-- | A routed net.
data NetInfo bel wire pip = NetInfo
    { netName :: !IdString
    , netHierpath :: !IdString
    , netDriver :: !PortRef
    , netUsers :: !(Vector PortRef)
    , netWires :: !(Map wire (PipMap pip))
    -- ^ wire -> uphill pip
    , netAttrs :: !(Map IdString Property)
    , netConstantValue :: !IdString
    }
    deriving (Eq, Show)

-- | A cell (technology-mapped or packed).
data CellInfo bel wire pip = CellInfo
    { cellName :: !IdString
    , cellType :: !IdString
    , cellHierpath :: !IdString
    , cellPorts :: !(Map IdString PortInfo)
    , cellAttrs :: !(Map IdString Property)
    , cellParams :: !(Map IdString Property)
    , cellBel :: !(Maybe bel)
    , cellBelStrength :: !PlaceStrength
    }
    deriving (Eq, Show)

-- | The whole netlist.
data Design bel wire pip = Design
    { designNets :: !(Map IdString (NetInfo bel wire pip))
    , designCells :: !(Map IdString (CellInfo bel wire pip))
    }
    deriving (Eq, Show)

emptyDesign :: Design bel wire pip
emptyDesign = Design M.empty M.empty

-- | Left-biased merge (fresh entries win), used by the frontend when
-- splicing newly created nets/cells into the design.
instance Semigroup (Design bel wire pip) where
    Design a b <> Design c d = Design (M.union a c) (M.union b d)

instance Monoid (Design bel wire pip) where
    mempty = emptyDesign

lookupCell :: IdString -> Design bel wire pip -> Maybe (CellInfo bel wire pip)
lookupCell n = M.lookup n . designCells

lookupNet :: IdString -> Design bel wire pip -> Maybe (NetInfo bel wire pip)
lookupNet n = M.lookup n . designNets

-- | Insert (or replace) a cell.
addCell :: IdString -> CellInfo bel wire pip -> Design bel wire pip -> Design bel wire pip
addCell n ci d = d{designCells = M.insert n ci (designCells d)}

-- | Insert (or replace) a net.
addNet :: IdString -> NetInfo bel wire pip -> Design bel wire pip -> Design bel wire pip
addNet n ni d = d{designNets = M.insert n ni (designNets d)}

{- | Connect a cell port to a net, mirroring @CellInfo::connectPort@:
output ports become the net driver; input/inout ports are appended to
the net's users. Returns the updated design (no-op on unknown
cell/port).
-}
connectPort ::
    -- | cell name
    IdString ->
    -- | port name
    IdString ->
    -- | net name
    IdString ->
    Design bel wire pip ->
    Design bel wire pip
connectPort cell port net d =
    case M.lookup cell (designCells d) of
        Nothing -> d
        Just ci ->
            case M.lookup port (cellPorts ci) of
                Nothing -> d
                Just pi ->
                    let ci' = ci{cellPorts = M.insert port (pi{portNet = Just net}) (cellPorts ci)}
                        d1 = d{designCells = M.insert cell ci' (designCells d)}
                     in case portType pi of
                            PortOut ->
                                -- net must be undriven; set driver
                                case M.lookup net (designNets d1) of
                                    Nothing -> d1
                                    Just ni ->
                                        let ni' = ni{netDriver = PortRef (Just cell) port}
                                         in d1{designNets = M.insert net ni' (designNets d1)}
                            _ ->
                                let ni =
                                        M.findWithDefault
                                            (NetInfo net emptyIdString (PortRef Nothing port) V.empty M.empty M.empty emptyIdString)
                                            net
                                            (designNets d1)
                                    ni' = ni{netUsers = V.snoc (netUsers ni) (PortRef (Just cell) port)}
                                    d2 = d1{designNets = M.insert net ni' (designNets d1)}
                                 in d2

{- | Disconnect a cell port from its net (mirrors @disconnectPort@):
removes the user (or driver) entry and clears the port's net.
-}
disconnectPort :: IdString -> IdString -> Design bel wire pip -> Design bel wire pip
disconnectPort cell port d =
    case M.lookup cell (designCells d) of
        Nothing -> d
        Just ci ->
            case M.lookup port (cellPorts ci) of
                Nothing -> d
                Just pi ->
                    let ci' = ci{cellPorts = M.insert port (pi{portNet = Nothing}) (cellPorts ci)}
                        d1 = d{designCells = M.insert cell ci' (designCells d)}
                     in case portNet pi of
                            Nothing -> d1
                            Just net ->
                                case M.lookup net (designNets d1) of
                                    Nothing -> d1
                                    Just ni ->
                                        let ni'
                                                | portType pi == PortOut = ni{netDriver = PortRef Nothing port}
                                                | otherwise =
                                                    ni
                                                        { netUsers =
                                                            V.filter
                                                                (\u -> not (prCell u == Just cell && prPort u == port))
                                                                (netUsers ni)
                                                        }
                                         in d1{designNets = M.insert net ni' (designNets d1)}
