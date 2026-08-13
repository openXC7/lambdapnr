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
    deleteNetSwap,
    deleteCellSwap,
    cellsIter,
    netsIter,
    connectPort,
    disconnectPort,
    setCellBel,
    clearCellBel,
    setCellPort,
    setNetWire,
    removeNetWire,
    strengthToInt,
    setCellType,
    setCellParam,
    delCellParam,
    setCellAttr,
    renameCellPort,
    moveCellPort,
    connectCellPorts,
    addCellPort,
    removeCellPort,
    renameNet,
    deleteNet,
    setNetDriver,
    setNetUserIdx,
    removeNetUser,
    removeNetDriver,
    setTopPort,
    getTopPortNet,
    setCellConstr,
    setCellCluster,
    setCellChildren,
    setNetUsers,
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
--
-- Users are slots (@indexed_store<PortRef>@ semantics): removal frees a
-- slot, the next add reuses the most recently freed slot (LIFO), so
-- remove+add keeps the user's position in the iteration order — which
-- the checksum chains over. @netUserFree@ is the LIFO free list.
data NetInfo bel wire pip = NetInfo
    { netName :: !IdString
    , netHierpath :: !IdString
    , netDriver :: !PortRef
    , netUsers :: !(Vector (Maybe PortRef))
    , netUserFree :: ![Int]
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
    , cellCluster :: !IdString
    -- ^ cluster root cell name ('ClusterId', empty when none)
    , cellConstrX :: !Int
    , cellConstrY :: !Int
    , cellConstrZ :: !Int
    , cellConstrAbsZ :: !Bool
    , cellConstrChildren :: ![IdString]
    }
    deriving (Eq, Show)

-- | The whole netlist, plus the top-level port table (the C++
-- @ctx->ports@): a top port's name maps to the top-level net it
-- connects to.
data Design bel wire pip = Design
    { designNets :: !(Map IdString (NetInfo bel wire pip))
    , designCells :: !(Map IdString (CellInfo bel wire pip))
    , designPorts :: !(Map IdString IdString)
    -- ^ top-level port name -> top-level net name
    , designCellOrder :: ![IdString]
    -- ^ cell insertion order (the C++ dict iterates this in REVERSE,
    -- with swap-erase on removal); pack passes iterate that way
    , designNetOrder :: ![IdString]
    -- ^ net insertion order, same semantics
    }
    deriving (Eq, Show)

emptyDesign :: Design bel wire pip
emptyDesign = Design M.empty M.empty M.empty [] []

-- | Left-biased merge (fresh entries win), used by the frontend when
-- splicing newly created nets/cells into the design.
instance Semigroup (Design bel wire pip) where
    Design a b p oo no <> Design c d q rr nr =
        Design
            (M.union a c)
            (M.union b d)
            (M.union p q)
            -- the fresh (first) entries are created after the second's;
            -- they keep the second's positions when replacing, and the
            -- genuinely new ones append at the END of the order
            (rr ++ filter (\n -> not (M.member n d)) oo)
            (nr ++ filter (\n -> not (M.member n c)) no)

instance Monoid (Design bel wire pip) where
    mempty = emptyDesign

lookupCell :: IdString -> Design bel wire pip -> Maybe (CellInfo bel wire pip)
lookupCell n = M.lookup n . designCells

lookupNet :: IdString -> Design bel wire pip -> Maybe (NetInfo bel wire pip)
lookupNet n = M.lookup n . designNets

-- | Set the top-level net for a top port (@ctx->ports[name].net@).
setTopPort :: IdString -> IdString -> Design bel wire pip -> Design bel wire pip
setTopPort port net d = d{designPorts = M.insert port net (designPorts d)}

-- | The top-level net for a top port, if any.
getTopPortNet :: IdString -> Design bel wire pip -> Maybe IdString
getTopPortNet port d = M.lookup port (designPorts d)

-- | Insert (or replace) a cell.
addCell :: IdString -> CellInfo bel wire pip -> Design bel wire pip -> Design bel wire pip
addCell n ci d =
    d
        { designCells = M.insert n ci (designCells d)
        , designCellOrder = if M.member n (designCells d) then designCellOrder d else designCellOrder d ++ [n]
        }

-- | Remove a cell (the C++ @cells.erase@: the dict swap-erases, so the
-- last-inserted cell takes the erased slot in the iteration order).
deleteCellSwap :: IdString -> Design bel wire pip -> Design bel wire pip
deleteCellSwap n d =
    d
        { designCells = M.delete n (designCells d)
        , designCellOrder =
            case break (== n) (designCellOrder d) of
                (pre, _ : post) ->
                    -- the back element moves into the hole
                    case reverse post of
                        (b : bp) -> pre ++ [b] ++ reverse bp
                        [] -> pre
                _ -> designCellOrder d
        }

-- | Iterate the cells the way the C++ dict does: reverse insertion
-- order (swap-erase applied on removal).
cellsIter :: Design bel wire pip -> [CellInfo bel wire pip]
cellsIter d =
    [ ci
    | n <- reverse (designCellOrder d)
    , Just ci <- [M.lookup n (designCells d)]
    ]

-- | Insert (or replace) a net.
addNet :: IdString -> NetInfo bel wire pip -> Design bel wire pip -> Design bel wire pip
addNet n ni d =
    d
        { designNets = M.insert n ni (designNets d)
        , designNetOrder = if M.member n (designNets d) then designNetOrder d else designNetOrder d ++ [n]
        }

-- | Remove a net (swap-erase semantics, like the C++ dict erase).
deleteNetSwap :: IdString -> Design bel wire pip -> Design bel wire pip
deleteNetSwap n d =
    d
        { designNets = M.delete n (designNets d)
        , designNetOrder =
            case break (== n) (designNetOrder d) of
                (pre, _ : post) ->
                    case reverse post of
                        (b : bp) -> pre ++ [b] ++ reverse bp
                        [] -> pre
                _ -> designNetOrder d
        }

-- | Iterate the nets the way the C++ dict does: reverse insertion order.
netsIter :: Design bel wire pip -> [NetInfo bel wire pip]
netsIter d =
    [ ni
    | n <- reverse (designNetOrder d)
    , Just ni <- [M.lookup n (designNets d)]
    ]

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
                                            (NetInfo net emptyIdString (PortRef Nothing port) V.empty [] M.empty M.empty emptyIdString)
                                            net
                                            (designNets d1)
                                    (slots, idx) =
                                        case netUserFree ni of
                                            i : rest ->
                                                ( (netUsers ni) V.// [(i, Just (PortRef (Just cell) port))]
                                                , i
                                                )
                                            [] ->
                                                ( V.snoc (netUsers ni) (Just (PortRef (Just cell) port))
                                                , V.length (netUsers ni)
                                                )
                                    ni' = ni{netUsers = slots, netUserFree = drop 1 (netUserFree ni)}
                                    -- record the user index on the cell port
                                    d1b =
                                        d1
                                            { designCells =
                                                M.adjust (\c -> c{cellPorts = M.adjust (\p -> p{portUserIdx = idx}) port (cellPorts c)}) cell (designCells d1)
                                            }
                                    d2 = d1b{designNets = M.insert net ni' (designNets d1b)}
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
                                                        { netUsers = (netUsers ni) V.// [(portUserIdx pi, Nothing)]
                                                        , netUserFree = portUserIdx pi : netUserFree ni
                                                        }
                                         in d1{designNets = M.insert net ni' (designNets d1)}

-- ---------------------------------------------------------------------------
-- Netlist surgery (the packer's cell/net transformations, mirroring
-- CellInfo/NetInfo member functions)
-- ---------------------------------------------------------------------------

-- | Change a cell's type (@ci->type = type@).
setCellType :: IdString -> IdString -> Design bel wire pip -> Design bel wire pip
setCellType cell typ d =
    case M.lookup cell (designCells d) of
        Nothing -> d
        Just ci -> d{designCells = M.insert cell (ci{cellType = typ}) (designCells d)}

-- | Set a cell parameter (mirrors @ci->params[key] = value@).
setCellParam :: IdString -> IdString -> Property -> Design bel wire pip -> Design bel wire pip
setCellParam cell key value d =
    case M.lookup cell (designCells d) of
        Nothing -> d
        Just ci -> d{designCells = M.insert cell (ci{cellParams = M.insert key value (cellParams ci)}) (designCells d)}

-- | Delete a cell parameter (@ci->params.erase(key)@).
delCellParam :: IdString -> IdString -> Design bel wire pip -> Design bel wire pip
delCellParam cell key d =
    case M.lookup cell (designCells d) of
        Nothing -> d
        Just ci -> d{designCells = M.insert cell (ci{cellParams = M.delete key (cellParams ci)}) (designCells d)}

-- | Set a cell attribute (@ci->attrs[key] = value@).
setCellAttr :: IdString -> IdString -> Property -> Design bel wire pip -> Design bel wire pip
setCellAttr cell key value d =
    case M.lookup cell (designCells d) of
        Nothing -> d
        Just ci -> d{designCells = M.insert cell (ci{cellAttrs = M.insert key value (cellAttrs ci)}) (designCells d)}

-- | Rename a cell port (@ci->renamePort(old, new)@): the port and its
-- net connection move to the new name.
renameCellPort :: IdString -> IdString -> IdString -> Design bel wire pip -> Design bel wire pip
renameCellPort cell old new d =
    case M.lookup cell (designCells d) of
        Nothing -> d
        Just ci ->
            case M.lookup old (cellPorts ci) of
                Nothing -> d
                Just pi ->
                    let ci1 = ci{cellPorts = M.delete old (cellPorts ci)}
                        ci2 = ci1{cellPorts = M.insert new (pi{portName = new}) (cellPorts ci1)}
                        d1 = d{designCells = M.insert cell ci2 (designCells d)}
                     in case portNet pi of
                            Nothing -> d1
                            Just net ->
                                case M.lookup net (designNets d1) of
                                    Nothing -> d1
                                    Just ni ->
                                        let fixDriver u = if prPort u == old then u{prPort = new} else u
                                            ni'
                                                | portType pi == PortOut = ni{netDriver = fixDriver (netDriver ni)}
                                                | otherwise =
                                                    ni
                                                        { netUsers =
                                                            V.map
                                                                (fmap (\u -> if prCell u == Just cell && prPort u == old then u{prPort = new} else u))
                                                                (netUsers ni)
                                                        }
                                         in d1{designNets = M.insert net ni' (designNets d1)}

-- | Move a port from one cell to another (@ci->movePortTo(port, other, newport)@):
-- the net connection follows the port.
moveCellPort :: IdString -> IdString -> IdString -> IdString -> Design bel wire pip -> Design bel wire pip
moveCellPort srcCell srcPort dstCell dstPort d =
    case M.lookup srcCell (designCells d) of
        Nothing -> d
        Just ci ->
            case M.lookup srcPort (cellPorts ci) of
                Nothing -> d
                Just pi ->
                    -- detach the source port (keeping its net in view), then
                    -- create/overwrite the destination port and attach it to
                    -- the same net, then drop the source port
                    let d1 = disconnectPort srcCell srcPort d
                        d2 = setCellPort dstCell dstPort (portType pi) d1
                        d3 = case portNet pi of
                            Nothing -> d2
                            Just net -> connectPort dstCell dstPort net d2
                        d4 = removeCellPort srcCell srcPort d3
                     in d4

-- | Connect two cell ports to the same net (@a->connectPorts(pa, b, pb)@):
-- b's port net is set to a's port net.
connectCellPorts :: IdString -> IdString -> IdString -> IdString -> Design bel wire pip -> Design bel wire pip
connectCellPorts a pa b pb d =
    case M.lookup a (designCells d) of
        Nothing -> d
        Just ci ->
            case M.lookup pa (cellPorts ci) of
                Nothing -> d
                Just pi -> case portNet pi of
                    Nothing -> d
                    Just net -> connectPort b pb net d

-- | Add a cell port (@ci->ports[port] = {port, nullptr, type}@).
addCellPort :: IdString -> IdString -> PortDir -> Design bel wire pip -> Design bel wire pip
addCellPort = setCellPort

-- | Remove a cell port entirely (@ci->ports.erase(port)@).
removeCellPort :: IdString -> IdString -> Design bel wire pip -> Design bel wire pip
removeCellPort cell port d =
    case M.lookup cell (designCells d) of
        Nothing -> d
        Just ci ->
            case M.lookup port (cellPorts ci) of
                Nothing -> d
                Just pi ->
                    let d1 = disconnectPort cell port d
                        ci' = maybe ci id (lookupCell cell d1)
                     in d1{designCells = M.insert cell (ci'{cellPorts = M.delete port (cellPorts ci')}) (designCells d1)}

-- | Rename a net (@ctx->renameNet(old, new)@): all references follow.
renameNet :: IdString -> IdString -> Design bel wire pip -> Design bel wire pip
renameNet old new d =
    case M.lookup old (designNets d) of
        Nothing -> d
        Just ni ->
            let d1 = d{designNets = M.insert new (ni{netName = new}) (M.delete old (designNets d))}
                -- the C++ renameNet's swap+erase round-trips the entry
                -- back to its original position (the swap moves it to
                -- the new key's slot, the erase of the old key swaps
                -- the back — which is the just-moved entry — into the
                -- old slot), so the iteration position is preserved
                d2 = d1{designNetOrder = map (\n -> if n == old then new else n) (designNetOrder d1)}
                renameInCell c ci =
                    let fixPort pn pi = if portNet pi == Just old then pi{portNet = Just new} else pi
                     in ci{cellPorts = M.mapWithKey fixPort (cellPorts ci)}
                d2b = d2{designCells = M.map (renameInCell c) (designCells d2)}
                d3 = d2b{designPorts = M.map (\n -> if n == old then new else n) (designPorts d2b)}
             in d3
  where
    c = old

-- | Delete a net (@ctx->nets.erase(name)@).
deleteNet :: IdString -> Design bel wire pip -> Design bel wire pip
deleteNet net d = d{designNets = M.delete net (designNets d)}

-- | Set the net driver (@net->driver = {cell, port}@).
setNetDriver :: IdString -> IdString -> IdString -> Design bel wire pip -> Design bel wire pip
setNetDriver net cell port d =
    case M.lookup net (designNets d) of
        Nothing -> d
        Just ni -> d{designNets = M.insert net (ni{netDriver = PortRef (Just cell) port}) (designNets d)}

-- | Clear the net driver (@net->driver.cell = nullptr@).
removeNetDriver :: IdString -> Design bel wire pip -> Design bel wire pip
removeNetDriver net d =
    case M.lookup net (designNets d) of
        Nothing -> d
        Just ni -> d{designNets = M.insert net (ni{netDriver = PortRef Nothing (prPort (netDriver ni))}) (designNets d)}

-- | Set a user's index into its net's users vector.
setNetUserIdx :: IdString -> IdString -> IdString -> Int -> Design bel wire pip -> Design bel wire pip
setNetUserIdx net cell port idx d =
    case M.lookup net (designNets d) of
        Nothing -> d
        Just ni ->
            case M.lookup cell (designCells d) of
                Nothing -> d
                Just ci ->
                    let ci' = ci{cellPorts = M.adjust (\pi -> pi{portUserIdx = idx}) port (cellPorts ci)}
                        d1 = d{designCells = M.insert cell ci' (designCells d)}
                     in case M.lookup port (cellPorts ci) of
                            Nothing -> d
                            Just pi ->
                                case portNet pi of
                                    Nothing -> d
                                    Just pn
                                        | pn == net -> d1
                                        | otherwise -> d1

-- | Remove a net user (@net->users.remove(port.user_idx)@).
removeNetUser :: IdString -> IdString -> IdString -> Design bel wire pip -> Design bel wire pip
removeNetUser net cell port d =
    case M.lookup net (designNets d) of
        Nothing -> d
        Just ni ->
            case V.findIndex (\u -> u == Just (PortRef (Just cell) port)) (netUsers ni) of
                Nothing -> d
                Just i ->
                    let ni' = ni{netUsers = (netUsers ni) V.// [(i, Nothing)], netUserFree = i : netUserFree ni}
                     in d{designNets = M.insert net ni' (designNets d)}

-- | Set a cell's cluster/constr fields in one go.
setCellConstr :: IdString -> Int -> Int -> Int -> Bool -> Design bel wire pip -> Design bel wire pip
setCellConstr cell cx cy cz absz d =
    case M.lookup cell (designCells d) of
        Nothing -> d
        Just ci ->
            d
                { designCells =
                    M.insert
                        cell
                        (ci{cellConstrX = cx, cellConstrY = cy, cellConstrZ = cz, cellConstrAbsZ = absz})
                        (designCells d)
                }

-- | Set a cell's cluster root.
setCellCluster :: IdString -> Maybe IdString -> Int -> Int -> Int -> Bool -> Design bel wire pip -> Design bel wire pip
setCellCluster cell mroot cx cy cz absz d =
    case M.lookup cell (designCells d) of
        Nothing -> d
        Just ci ->
            d
                { designCells =
                    M.insert
                        cell
                        (ci{cellCluster = maybe emptyId id mroot, cellConstrX = cx, cellConstrY = cy, cellConstrZ = cz, cellConstrAbsZ = absz})
                        (designCells d)
                }

-- | Set a cluster root's children list.
setCellChildren :: IdString -> [IdString] -> Design bel wire pip -> Design bel wire pip
setCellChildren cell children d =
    case M.lookup cell (designCells d) of
        Nothing -> d
        Just ci -> d{designCells = M.insert cell (ci{cellConstrChildren = children}) (designCells d)}

-- | Replace a net's users wholesale (the C++ @users.clear()@ + adds:
-- fresh dense slots, empty free list).
setNetUsers :: IdString -> Vector PortRef -> Design bel wire pip -> Design bel wire pip
setNetUsers net users d =
    case M.lookup net (designNets d) of
        Nothing -> d
        Just ni -> d{designNets = M.insert net (ni{netUsers = V.map Just users, netUserFree = []}) (designNets d)}
