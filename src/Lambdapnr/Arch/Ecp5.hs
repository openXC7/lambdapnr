{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | The ECP5 architecture: 'Arch' instance over the parsed chipdb.
--
-- Query surface mirror of @ecp5\/arch.cc@\/@arch.h@: ids, names, types,
-- locations, pip/wire connectivity, the delay model (timing classes with
-- fanout adders, estimate/predict formulas, unit conversions), bel
-- buckets and validity. Binding state (bel2cell/wire2net/pip2net/fanout)
-- arrives with the router port; queries are pure.
--
-- Interning discipline: all chipdb names are interned at load time in a
-- fixed order (constids first, then bel/wire basenames per location type),
-- so id indices are deterministic for a given chipdb blob.
module Lambdapnr.Arch.Ecp5
  ( Ecp5
  , loadEcp5
  , chipdbFileFor
  , ecp5Chipdb
  , ecp5Args
  , ecp5Bind
  , setEcp5Bind
  , ecp5IdTable
  , ecp5TimingDb
  , tileIndex
  , tileXY
  , locTypeOfTile
  , belAt
  , wireAt
  , pipAt
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.Functor ((<&>))
import Data.Int (Int16, Int32)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.IO.Unsafe (unsafePerformIO)

import Lambdapnr.Arch.Ecp5.Binding (BindState (..), boundBelCell, boundPipNet, boundWireNet, emptyBindState, wireFanoutOf)
import Lambdapnr.Arch.Ecp5.CellTiming (TimingDb (..), getCellDelayFor, getPortClockingInfoFor, getPortTimingClassFor)
import Lambdapnr.Arch.Ecp5.Chipdb
import Lambdapnr.Arch.Ecp5.ConstIds
import Lambdapnr.Arch.Ecp5.Types (BelId (..), Ecp5Args (..), Ecp5Device (..), GroupId, Location (..), PipId (..), SpeedGrade (..), WireId (..), deviceName, locAdd, speedToInt)
import Lambdapnr.Kernel.Arch hiding (locX, locY, locZ)
import Lambdapnr.Kernel.Delay
import Lambdapnr.Kernel.IdString
import Lambdapnr.Kernel.Netlist (CellInfo (..), PortDir (..))
import Lambdapnr.Kernel.Timing (TimingClockingInfo, TimingPortClass)

-- | The ECP5 architecture instance: chipdb + args + interned name ids.
data Ecp5 = Ecp5
  { e5Chipdb :: !Chipdb
  , e5Args :: !Ecp5Args
  , e5IdTable :: IdTable
  , e5ConstIds :: !(V.Vector IdString) -- ^ ids 0..constIdCount-1 (index = chipdb index)
  , e5ConstIdByName :: !(M.Map Text IdString) -- ^ constid name -> id
  , e5ConstIdIdx :: !(M.Map IdString Int) -- ^ constid id -> chipdb index (timing DB lookups)
  , e5XIds :: !(V.Vector IdString) -- ^ "0".."width-1"
  , e5YIds :: !(V.Vector IdString)
  , e5BelNameIds :: !(M.Map Text IdString) -- ^ bel basename -> id
  , e5WireNameIds :: !(M.Map Text IdString)
  , e5Bind :: !BindState
  }

ecp5Chipdb :: Ecp5 -> Chipdb
ecp5Chipdb = e5Chipdb

ecp5Args :: Ecp5 -> Ecp5Args
ecp5Args = e5Args

-- | The chipdb blob for a device, mirroring @get_chip_info@: the 12k
-- part shares the 25k database (nextpnr treats it as 25k silicon with
-- the @LFE5U-12F@ name and 12k-specific bitstream handling).
chipdbFileFor :: Ecp5Device -> FilePath
chipdbFileFor dev = case dev of
    Lfe5u12f -> "data/ecp5/chipdb-25k.bin"
    Lfe5u25f -> "data/ecp5/chipdb-25k.bin"
    Lfe5um25f -> "data/ecp5/chipdb-25k.bin"
    Lfe5um5g25f -> "data/ecp5/chipdb-25k.bin"
    Lfe5u45f -> "data/ecp5/chipdb-45k.bin"
    Lfe5um45f -> "data/ecp5/chipdb-45k.bin"
    Lfe5um5g45f -> "data/ecp5/chipdb-45k.bin"
    Lfe5u85f -> "data/ecp5/chipdb-85k.bin"
    Lfe5um85f -> "data/ecp5/chipdb-85k.bin"
    Lfe5um5g85f -> "data/ecp5/chipdb-85k.bin"

-- | The binding maps (bel/cell, wire/net, pip/net, wire fanout).
ecp5Bind :: Ecp5 -> BindState
ecp5Bind = e5Bind

-- | Install updated binding maps (the bind/unbind operations in
-- 'Lambdapnr.Arch.Ecp5.Binding' return a 'BindState'; this puts it
-- back into the arch record).
setEcp5Bind :: BindState -> Ecp5 -> Ecp5
setEcp5Bind bs e = e{e5Bind = bs}

-- | The arch's id table (share it with the context via 'newContextWith').
ecp5IdTable :: Ecp5 -> IdTable
ecp5IdTable = e5IdTable

-- | The timing-database view (cell timings of the selected speed grade
-- + constid tables).
ecp5TimingDb :: Ecp5 -> TimingDb
ecp5TimingDb e =
    TimingDb
        { tdIdTable = e5IdTable e
        , tdConstIdIndex = e5ConstIdIdx e
        , tdConstIdByName = e5ConstIdByName e
        , tdSpeedGrade = cdSpeedGrades (e5Chipdb e) V.! speedToInt (eaSpeed (e5Args e))
        }

-- | Load the architecture: parse the blob, intern names, build the id
-- maps. Returns the arch with a *fresh* id table (share it with the
-- context via 'newContextWith').
--
-- Retries on parse failure: this development machine has an
-- intermittently flaky storage controller that occasionally returns
-- corrupted reads; the parser's structural checks turn those into
-- clean errors, and a retry almost always succeeds. The retry is cheap
-- (a 32MB read + parse) and bounds the blast radius of bad hardware.
loadEcp5 :: Ecp5Args -> FilePath -> IO (Either String Ecp5)
loadEcp5 = loadEcp5WithRetries 5

loadEcp5WithRetries :: Int -> Ecp5Args -> FilePath -> IO (Either String Ecp5)
loadEcp5WithRetries 0 _ _ = pure (Left "chipdb load failed after 5 attempts (storage read corruption?)")
loadEcp5WithRetries n args path = do
  bs <- BS.readFile path
  case parseChipdb bs of
    Left err -> pure (Left err)
    Right cd -> do
      r <- try (buildEcp5 args cd)
      case r of
        Right e -> pure (Right e)
        Left (e :: SomeException) -> do
          putStrLn ("lambdapnr: chipdb parse attempt failed (" ++ show e ++ "); retrying...")
          loadEcp5WithRetries (n - 1) args path
-- | Intern everything in the deterministic load order.
buildEcp5 :: Ecp5Args -> Chipdb -> IO Ecp5
buildEcp5 args cd = do
  tbl <- newIdTable
  -- 1. constids (indices 1..N; index 0 is the empty string); the count
  --    must match what the chipdb records
  let constidNames = parseConstIds bundledConstIds
  unless (V.length constidNames == cdConstIdCount cd) $
    error
      ( "lambdapnr ecp5: constids.inc has "
          ++ show (V.length constidNames)
          ++ " entries but chipdb expects "
          ++ show (cdConstIdCount cd)
      )
  constIds <- V.mapM (intern tbl) constidNames
  let constIdByName = M.fromList (zip (V.toList constidNames) (V.toList constIds))
      constIdIdx = M.fromList (zip (V.toList constIds) [0 ..])
  -- 2. x/y coordinate ids
  xIds <- V.mapM (intern tbl) (V.generate (cdWidth cd) (T.pack . show))
  yIds <- V.mapM (intern tbl) (V.generate (cdHeight cd) (T.pack . show))
  -- 3. bel/wire basenames, per location type (the chipdb deduplicates
  --    tile types; interning per type keeps the load at the blob's own
  --    scale instead of expanding every tile instance)
  belNames <- internTileNames tbl cd ltBels biName
  wireNames <- internTileNames tbl cd ltWires wiName
  pure
    Ecp5
      { e5Chipdb = cd
      , e5Args = args
      , e5IdTable = tbl
      , e5ConstIds = constIds
      , e5ConstIdByName = constIdByName
      , e5ConstIdIdx = constIdIdx
      , e5XIds = xIds
      , e5YIds = yIds
      , e5BelNameIds = belNames
      , e5WireNameIds = wireNames
      , e5Bind = emptyBindState
      }

internTileNames :: IdTable -> Chipdb -> (LocationType -> V.Vector a) -> (a -> Text) -> IO (M.Map Text IdString)
internTileNames tbl cd proj nameOf = do
  let names = [nameOf x | lt <- V.toList (cdLocations cd), x <- V.toList (proj lt)]
  M.fromList <$> mapM (\n -> (,) n <$> intern tbl n) names

-- | The bundled constids.inc (kept in sync with nextpnr/ecp5/constids.inc
-- by the submodule pin).
bundledConstIds :: Text
bundledConstIds =
  unsafePerformIO $
    T.pack
      <$> readFile "nextpnr/ecp5/constids.inc"
{-# NOINLINE bundledConstIds #-}

-- ---------------------------------------------------------------------------
-- Arch instance

instance Arch Ecp5 where
  type Bel Ecp5 = BelId
  type Wire Ecp5 = WireId
  type Pip Ecp5 = PipId
  type Group Ecp5 = GroupId

  archId e = unsafePerformIO (intern (e5IdTable e) "ecp5")
  getChipName = deviceName . eaDevice . e5Args
  getGridDimX = cdWidth . e5Chipdb
  getGridDimY = cdHeight . e5Chipdb
  -- the C++ constant @max_loc_bels = 32@: the maximum bel z over all
  -- tile types (z indices are sparse per tile, e.g. 0,1,4,5,...)
  getTileBelDimZ _ _ _ = 32
  getNameDelimiter _ = '/'

  -- Bels ---------------------------------------------------------------
  getBels e =
    [ BelId (tileXY cd t) (fromIntegral i)
    | t <- [0 .. cdNumTiles cd - 1]
    , i <- [0 .. V.length (ltBels (locTypeOfTile cd t)) - 1]
    ]
    where
      cd = e5Chipdb e
  getBelName e b =
    [ e5XIds e V.! fromIntegral (locX (belLoc b))
    , e5YIds e V.! fromIntegral (locY (belLoc b))
    , belNameId e (belAt (e5Chipdb e) b)
    ]
  getBelByName e [x, y, name] = do
    xi <- idToCoord e x
    yi <- idToCoord e y
    let loc = Location xi yi
        lt = locTypeOfTile (e5Chipdb e) (tileIndex (e5Chipdb e) loc)
    V.findIndex ((== name) . belNameId e) (ltBels lt)
      <&> \i -> BelId loc (fromIntegral i)
  getBelByName _ _ = Nothing
  getBelLocation e b =
    let info = belAt (e5Chipdb e) b
     in Loc (fromIntegral (locX (belLoc b))) (fromIntegral (locY (belLoc b))) (fromIntegral (biZ info))
  getBelByLocation e (Loc x y z) =
    let loc = Location (fromIntegral x) (fromIntegral y)
        lt = locTypeOfTile (e5Chipdb e) (tileIndex (e5Chipdb e) loc)
     in V.findIndex ((== fromIntegral z) . biZ) (ltBels lt)
          <&> \i -> BelId loc (fromIntegral i)
  getBelsByTile e x y =
    let loc = Location (fromIntegral x) (fromIntegral y)
        _ = loc
        lt = locTypeOfTile (e5Chipdb e) (tileIndex (e5Chipdb e) loc)
     in [BelId loc (fromIntegral i) | i <- [0 .. V.length (ltBels lt) - 1]]
  getBelType e b = e5ConstIds e V.! fromIntegral (biType (belAt (e5Chipdb e) b))
  getBelPins e b =
    [e5ConstIds e V.! fromIntegral (bwPort bw) | bw <- V.toList (biBelWires (belAt (e5Chipdb e) b))]
  getBelPinWire e b pin = do
    bw <- V.find ((== pin) . constId e . bwPort) (biBelWires (belAt (e5Chipdb e) b))
    pure
      WireId
        { wireLoc = locAdd (belLoc b) (Location (bwRelDx bw) (bwRelDy bw))
        , wireIdx = bwWireIndex bw
        }
  getBelPinType e b pin =
    case V.find ((== pin) . constId e . bwPort) (biBelWires (belAt (e5Chipdb e) b)) of
      Just bw -> toPortDir (bwType bw)
      Nothing -> PortInout
  getBelChecksum _ b = fromIntegral (belIdx b)
  getBelGlobalBuf e b = getBelType e b == M.findWithDefault (IdString 0) "DCCA" (e5ConstIdByName e)
  getBelHidden _ _ = False
  checkBelAvail e b = not (M.member b (bsBel2Cell (e5Bind e)))
  getBoundBelCell e b = boundBelCell b (e5Bind e)
  getConflictingBelCell e b = boundBelCell b (e5Bind e)

  -- Wires ---------------------------------------------------------------
  getWires e =
    [ WireId (tileXY cd t) (fromIntegral i)
    | t <- [0 .. cdNumTiles cd - 1]
    , i <- [0 .. V.length (ltWires (locTypeOfTile cd t)) - 1]
    ]
    where
      cd = e5Chipdb e
  getWireName e w =
    [ e5XIds e V.! fromIntegral (locX (wireLoc w))
    , e5YIds e V.! fromIntegral (locY (wireLoc w))
    , wireNameId e (wireAt (e5Chipdb e) w)
    ]
  getWireByName e [x, y, name] = do
    xi <- idToCoord e x
    yi <- idToCoord e y
    let loc = Location xi yi
        lt = locTypeOfTile (e5Chipdb e) (tileIndex (e5Chipdb e) loc)
    V.findIndex ((== name) . wireNameId e) (ltWires lt)
      <&> \i -> WireId loc (fromIntegral i)
  getWireByName _ _ = Nothing
  getWireType e w = e5ConstIds e V.! fromIntegral (wiType (wireAt (e5Chipdb e) w))
  getWireChecksum _ w = fromIntegral (wireIdx w)
  getWireDelay _ _ = dqFromDelay 0
  getPipsDownhill e w =
    [ PipId (locAdd (wireLoc w) (Location (plRelDx pl) (plRelDy pl))) (plIndex pl)
    | pl <- V.toList (wiPipsDownhill (wireAt (e5Chipdb e) w))
    ]
  getPipsUphill e w =
    [ PipId (locAdd (wireLoc w) (Location (plRelDx pl) (plRelDy pl))) (plIndex pl)
    | pl <- V.toList (wiPipsUphill (wireAt (e5Chipdb e) w))
    ]
  getWireBelPins e w =
    [ ( BelId (locAdd (wireLoc w) (Location (bpRelDx bp) (bpRelDy bp))) (bpBelIndex bp)
      , e5ConstIds e V.! fromIntegral (bpPort bp)
      )
    | bp <- V.toList (wiBelPins (wireAt (e5Chipdb e) w))
    ]
  checkWireAvail e w = not (M.member w (bsWire2Net (e5Bind e)))
  getBoundWireNet e w = boundWireNet w (e5Bind e)

  -- Pips ----------------------------------------------------------------
  getPips e =
    [ PipId (tileXY cd t) (fromIntegral i)
    | t <- [0 .. cdNumTiles cd - 1]
    , i <- [0 .. V.length (ltPips (locTypeOfTile cd t)) - 1]
    ]
    where
      cd = e5Chipdb e
  getPipName e p =
    let cd = e5Chipdb e
     in [ e5XIds e V.! fromIntegral (locX (pipLoc p))
        , e5YIds e V.! fromIntegral (locY (pipLoc p))
        , pipNameBasename e p
        ]
  getPipByName e [x, y, name] = do
    xi <- idToCoord e x
    yi <- idToCoord e y
    let loc = Location xi yi
        lt = locTypeOfTile (e5Chipdb e) (tileIndex (e5Chipdb e) loc)
    V.findIndex ((== name) . pipNameBasename e) (V.map (PipId loc) (V.imap (\i _ -> fromIntegral i) (ltPips lt)))
      <&> \i -> PipId loc (fromIntegral i)
  getPipByName _ _ = Nothing
  getPipType _ _ = IdString 0 -- BaseArch default: no pip types in ecp5
  getPipChecksum _ p = fromIntegral (pipIdx p)
  getPipDelay e p =
    let cd = e5Chipdb e
        pi = pipAt cd p
        cls = sgPipClasses (cdSpeedGrades cd V.! speedToInt (eaSpeed (e5Args e))) V.! fromIntegral (piTimingClass pi)
        -- C++: delay = base + fanout * adder, with the fanout of the
        -- pip's source wire (number of bound pips leaving it)
        fanout = wireFanoutOf (getPipSrcWire e p) (e5Bind e)
     in dqScalar
          (fromIntegral (pdMinBase cls) + fromIntegral fanout * fromIntegral (pdMinFanout cls))
          (fromIntegral (pdMaxBase cls) + fromIntegral fanout * fromIntegral (pdMaxFanout cls))
  getPipSrcWire e p =
    let pi = pipAt (e5Chipdb e) p
     in WireId
          { wireLoc = locAdd (pipLoc p) (Location (piSrcRelDx pi) (piSrcRelDy pi))
          , wireIdx = fromIntegral (piSrcIdx pi)
          }
  getPipDstWire e p =
    let pi = pipAt (e5Chipdb e) p
     in WireId
          { wireLoc = locAdd (pipLoc p) (Location (piDstRelDx pi) (piDstRelDy pi))
          , wireIdx = fromIntegral (piDstIdx pi)
          }
  getPipLocation e p =
    Loc (fromIntegral (locX (pipLoc p))) (fromIntegral (locY (pipLoc p))) 0
  isPipInverting _ _ = False
  checkPipAvail e p = not (M.member p (bsPip2Net (e5Bind e)))
  getBoundPipNet e p = boundPipNet p (e5Bind e)

  -- Delay model ---------------------------------------------------------
  predictDelay e srcB _srcPin dstB _dstPin =
    let (Loc sx sy _) = getBelLocation e srcB
        (Loc dx dy _) = getBelLocation e dstB
     in delayFormula e (abs (dx - sx)) (abs (dy - sy))
  estimateDelay e srcW dstW =
    let (sx, sy) = estLocation e srcW
        (dx, dy) = estLocation e dstW
     in delayFormula e (abs (dx - sx)) (abs (dy - sy))
  getDelayEpsilon _ = 20
  getRipupDelayPenalty e = fromIntegral (550 - 50 * speedToInt (eaSpeed (e5Args e)))
  getDelayNS _ v = fromIntegral v * 0.001
  getDelayFromNS _ ns = fromIntegral (floor (ns * 1000) :: Int)
  getRouteBoundingBox e srcW dstW =
    let (sx, sy) = estLocation e srcW
        (dx, dy) = estLocation e dstW
     in BoundingBox (min sx dx) (min sy dy) (max sx dx) (max sy dy)
  expandBoundingBox e (BoundingBox x0 y0 x1 y1) =
    BoundingBox
      (max 0 (x0 - 1))
      (max 0 (y0 - 1))
      (min (getGridDimX e - 1) (x1 + 1))
      (min (getGridDimY e - 1) (y1 + 1))

  -- Placement validity --------------------------------------------------
  isValidBelForCellType e cellType b = getBelType e b == cellType
  isBelLocationValid _ _ = True
  getCellTypes e =
    S.toList $
      S.fromList [e5ConstIds e V.! fromIntegral (biType (belAt (e5Chipdb e) b)) | b <- getBels e]
  getBelBucketForCellType _ t = t
  getBelBucketForBel e b = getBelType e b
  getBelsInBucket e bucket = [b | b <- getBels e, getBelBucketForBel e b == bucket]

  -- Cell timing -----------------------------------------------------------
  getCellDelay e cell from to = getCellDelayFor (ecp5TimingDb e) cell from to
  getPortTimingClass e cell port = getPortTimingClassFor (ecp5TimingDb e) cell port
  getPortClockingInfo e cell port idx = getPortClockingInfoFor (ecp5TimingDb e) cell port idx

  pack _ = pure False
  place _ = pure False
  route _ = pure False

-- ---------------------------------------------------------------------------
-- helpers

-- | Constid id for a chipdb index.
constId :: Ecp5 -> Int32 -> IdString
constId e i = e5ConstIds e V.! fromIntegral i

-- | The interned basename id of a bel.
belNameId :: Ecp5 -> BelInfo -> IdString
belNameId e bi = M.findWithDefault (IdString 0) (biName bi) (e5BelNameIds e)

-- | The interned basename id of a wire.
wireNameId :: Ecp5 -> WireInfo -> IdString
wireNameId e wi = M.findWithDefault (IdString 0) (wiName wi) (e5WireNameIds e)

-- | The ECP5 delay formula: (80 - 9*speed) * (6 + max(d-5,0) + 2*min(d,5)).
delayFormula :: Ecp5 -> Int -> Int -> DelayT
delayFormula e dx dy =
  let base = 80 - 9 * speedToInt (eaSpeed (e5Args e))
      f d = max (d - 5) 0 + 2 * min d 5
   in fromIntegral (base * (6 + f dx + f dy))

-- | Estimated physical location of a wire, mirroring the C++ @est_location@.
estLocation :: Ecp5 -> WireId -> (Int, Int)
estLocation e w =
  let cd = e5Chipdb e
      wi = wireAt cd w
      (x, y) = (fromIntegral (locX (wireLoc w)), fromIntegral (locY (wireLoc w)))
   in if not (V.null (wiBelPins wi))
        then (x + fromIntegral (bpRelDx (V.head (wiBelPins wi))), y + fromIntegral (bpRelDy (V.head (wiBelPins wi))))
        else
          if not (V.null (wiPipsDownhill wi))
            then (x + fromIntegral (plRelDx (V.head (wiPipsDownhill wi))), y + fromIntegral (plRelDy (V.head (wiPipsDownhill wi))))
            else
              if not (V.null (wiPipsUphill wi))
                then (x + fromIntegral (plRelDx (V.head (wiPipsUphill wi))), y + fromIntegral (plRelDy (V.head (wiPipsUphill wi))))
                else (x, y)

-- | The third (basename) id of a pip's name, composed like the C++
-- @stringf("%d_%d_%s->%d_%d_%s", ...)@. Interned on first use; the
-- C++ caches these in @pip_by_name@ (a cache lands with binding state).
pipNameBasename :: Ecp5 -> PipId -> IdString
pipNameBasename e p =
  let pi = pipAt (e5Chipdb e) p
      srcName = wiName (wireAt (e5Chipdb e) (getPipSrcWire e p))
      dstName = wiName (wireAt (e5Chipdb e) (getPipDstWire e p))
      nm =
        T.pack
          ( show (piSrcRelDx pi)
              ++ "_" ++ show (piSrcRelDy pi) ++ "_"
              ++ T.unpack srcName ++ "->"
              ++ show (piDstRelDx pi) ++ "_" ++ show (piDstRelDy pi) ++ "_"
              ++ T.unpack dstName
          )
   in unsafePerformIO (intern (e5IdTable e) nm)

-- | Resolve an x or y coordinate id to its integer value.
idToCoord :: Ecp5 -> IdString -> Maybe Int16
idToCoord e idstr =
  case V.findIndex (== idstr) (e5XIds e) of
    Just i -> Just (fromIntegral i)
    Nothing -> fromIntegral <$> V.findIndex (== idstr) (e5YIds e)

toPortDir :: Int32 -> PortDir
toPortDir 0 = PortIn
toPortDir 1 = PortOut
toPortDir _ = PortInout
