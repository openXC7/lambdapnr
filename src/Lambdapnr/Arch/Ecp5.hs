{-# LANGUAGE MultiWayIf #-}
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
  , combCtxOf
  , tileIndex
  , tileXY
  , locTypeOfTile
  , belAt
  , wireAt
  , pipAt
  , getTilesAtLoc
  , getTileByTypeLocSet
  , getTileByTypeLoc
  , getTileByType
  , getWireBasename
  , getWireByLocBasename
  , getPioBelBank
  , getPioFunctionName
  , getPioByFunctionName
  , getPackagePinBel
  , getBelPackagePin
  , getPioDqsGroup
  , applyLpf
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (foldM, unless)
import Data.Char (isSpace)
import qualified Data.ByteString as BS
import Data.Bits (shiftR, (.&.))
import Data.Functor ((<&>))
import Data.Int (Int16, Int32)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.IO.Unsafe (unsafePerformIO)

import Lambdapnr.Arch.Ecp5.Binding (BindState (..), CombCtx (..), boundBelCell, boundPipNet, boundWireNet, emptyBindState, wireFanoutOf)
import Lambdapnr.Arch.Ecp5.CellTiming (TimingDb (..), getCellDelayFor, getPortClockingInfoFor, getPortTimingClassFor)
import Lambdapnr.Arch.Ecp5.Chipdb
import Lambdapnr.Arch.Ecp5.ConstIds
import Lambdapnr.Arch.Ecp5.Types (BelId (..), Ecp5Args (..), Ecp5Device (..), GroupId, Location (..), PipId (..), SpeedGrade (..), WireId (..), deviceName, locAdd, speedToInt)
import Lambdapnr.Kernel.Arch hiding (locX, locY, locZ)
import Lambdapnr.Kernel.Delay
import Lambdapnr.Kernel.IdString
import Lambdapnr.Kernel.Netlist (CellInfo (..), Design, PortDir (..), designCells, setCellAttr)
import Lambdapnr.Kernel.Timing (TimingClockingInfo, TimingPortClass)
import Lambdapnr.Kernel.Property (Property (..), propFromString)
import System.IO (hPutStrLn, stderr)

-- | The ECP5 architecture instance: chipdb + args + interned name ids.
data Ecp5 = Ecp5
  { e5Chipdb :: !Chipdb
  , e5Args :: !Ecp5Args
  , e5IdTable :: IdTable
  , e5ConstIds :: !(V.Vector IdString) -- ^ ids 0..constIdCount-1 (index = chipdb index)
  , e5ConstIdByName :: !(M.Map Text IdString) -- ^ constid name -> id
  , e5ConstIdIdx :: !(M.Map IdString Int) -- ^ constid id -> chipdb index (timing DB lookups)
  , e5XIds :: !(V.Vector IdString) -- ^ "X0".."X<width-1>"
  , e5YIds :: !(V.Vector IdString) -- ^ "Y0".."Y<height-1>"
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

-- | The @CombCtx@ for @bindBelLut@ (TRELLIS_COMB bel-type index +
-- width + constid name table).
combCtxOf :: Ecp5 -> CombCtx
combCtxOf e =
    let combId = M.findWithDefault emptyId "TRELLIS_COMB" (e5ConstIdByName e)
        idx = fromIntegral (M.findWithDefault (-1) combId (e5ConstIdIdx e)) :: Int32
     in CombCtx idx (cdWidth (e5Chipdb e)) (e5ConstIdByName e)

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
  -- 2. x/y coordinate ids (@idf("X%d", i)@ / @idf("Y%d", i)@): the
  --    interning ORDER here must match the C++ Arch constructor exactly
  --    (constids, then X ids, then Y ids) because the checksum hashes
  --    id indices. Bel/wire names are NOT interned up front in C++
  --    (ECP5 bel/wire types are constid indices, and bel/wire names are
  --    interned lazily on demand), so we must not pre-intern them.
  xIds <- V.mapM (intern tbl) (V.generate (cdWidth cd) (\i -> "X" <> T.pack (show i)))
  yIds <- V.mapM (intern tbl) (V.generate (cdHeight cd) (\i -> "Y" <> T.pack (show i)))
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
      , e5Bind = emptyBindState
      }

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
    let bi0 = belAt (e5Chipdb e) b
        nm = belNameId e bi0
     in [ e5XIds e V.! fromIntegral (locX (belLoc b))
        , e5YIds e V.! fromIntegral (locY (belLoc b))
        , nm
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
  checkPipAvail e p = not (M.member p (bsPip2Net (e5Bind e))) && not (isPipBlockedE e p)
  getBoundPipNet e p = boundPipNet p (e5Bind e)

  -- Delay model ---------------------------------------------------------
  predictDelay e srcB srcPin dstB dstPin =
    let (Loc sx sy sz) = getBelLocation e srcB
        (Loc dx dy dz) = getBelLocation e dstB
        pin n = M.findWithDefault emptyId n (e5ConstIdByName e)
        fco = pin "FCO"
        fci = pin "FCI"
        fxa = pin "FXA"
        fxb = pin "FXB"
        f = pin "F"
        di = pin "DI"
        q = pin "Q"
        lutPins = [pin "A", pin "B", pin "C", pin "D"]
        -- C++ arch.cc predictDelay: direct interconnect (0-delay) cases.
        directCarry = (srcPin == fco && dstPin == fci) || dstPin `elem` [fxa, fxb] || (srcPin == f && dstPin == di)
        sameTile = sx == dx && sy == dy
        qToLut = sameTile && dstPin `elem` lutPins && srcPin == q && (dz `div` 4) == (sz `div` 4)
        fToLut = sameTile && dstPin `elem` lutPins && srcPin == f && (sz `div` 4) `notElem` [1, 6]
     in if directCarry || qToLut || fToLut
            then 0
            else delayFormula e (abs (dx - sx)) (abs (dy - sy))
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

-- | The interned basename id of a bel (interned lazily on demand,
-- mirroring C++ @getBelName@'s @id(...)@ — pre-interning would shift
-- the id table and break checksum equality).
belNameId :: Ecp5 -> BelInfo -> IdString
belNameId e bi = unsafePerformIO (intern (e5IdTable e) (biName bi))
{-# NOINLINE belNameId #-}

-- | The interned basename id of a wire (see 'belNameId').
wireNameId :: Ecp5 -> WireInfo -> IdString
wireNameId e wi = unsafePerformIO (intern (e5IdTable e) (wiName wi))
{-# NOINLINE wireNameId #-}

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

-- ---------------------------------------------------------------------------
-- ECP5-specific arch queries (the C++ Arch methods used by the packer
-- and bitgen; the Arch class keeps only the generic kernel surface)
-- ---------------------------------------------------------------------------

-- | @get_tiles_at_loc@: the names of all tiles at a grid location.
getTilesAtLoc :: Ecp5 -> Int -> Int -> [(Text, Text)]
getTilesAtLoc e y x =
    [ (tnName tn, cdTiletypeNames cd V.! fromIntegral (tnTypeIdx tn))
    | tn <- V.toList (tiTileNames (cdTileInfos cd V.! (y * cdWidth cd + x)))
    ]
  where
    cd = e5Chipdb e

-- | @get_tile_by_type_loc@: the name of the tile at (row, col) whose
-- type is in the set (first match in chipdb order).
getTileByTypeLocSet :: Ecp5 -> Int -> Int -> [Text] -> Maybe Text
getTileByTypeLocSet e row col types =
    let cd = e5Chipdb e
        tileIdx = row * cdWidth cd + col
     in case V.find (\tn -> cdTiletypeNames cd V.! fromIntegral (tnTypeIdx tn) `elem` types) (tiTileNames (cdTileInfos cd V.! tileIdx)) of
            Just tn -> Just (tnName tn)
            Nothing -> Nothing

-- | @get_tile_by_type_loc@ with a single type.
getTileByTypeLoc :: Ecp5 -> Int -> Int -> Text -> Maybe Text
getTileByTypeLoc e row col typ = getTileByTypeLocSet e row col [typ]

-- | @get_tile_by_type@: the first tile in the chip with the type.
getTileByType :: Ecp5 -> Text -> Maybe Text
getTileByType e typ =
    let cd = e5Chipdb e
     in case V.findIndex (\tile -> any (\tn -> cdTiletypeNames cd V.! fromIntegral (tnTypeIdx tn) == typ) (V.toList (tiTileNames tile))) (cdTileInfos cd) of
            Nothing -> Nothing
            Just i -> tnName <$> V.find (\tn -> cdTiletypeNames cd V.! fromIntegral (tnTypeIdx tn) == typ) (tiTileNames (cdTileInfos cd V.! i))

-- | @is_pip_blocked@: LUT-permutation pips are blocked unless the
-- slice's @lutperm_allowed@ rule permits the swap. @disable_router_lutperm@
-- is always false in this flow. Rule: 0 = NONE (blocked), 1 = CARRY
-- (A/B and C/D swappable within pairs), 2 = ALL (any permutation).
isPipBlockedE :: Ecp5 -> PipId -> Bool
isPipBlockedE e p =
    let flags = fromIntegral (piLutpermFlags (pipAt cd p)) :: Int
     in if (flags .&. 0x4000) /= 0
            then
                let lut = (flags `shiftR` 4) .&. 7
                    outPin = (flags `shiftR` 2) .&. 3
                    inPin = flags .&. 3
                    sliceIdx = (fromIntegral (locY (pipLoc p)) * cdWidth cd + fromIntegral (locX (pipLoc p))) * 4 + lut `quot` 2
                    rule = M.findWithDefault 0 sliceIdx (bsLutperm (e5Bind e))
                 in rule == 0 || (rule == 1 && (outPin `quot` 2) /= (inPin `quot` 2))
            else False
  where
    cd = e5Chipdb e

-- | @get_wire_basename@: the raw wire name from the chipdb.
getWireBasename :: Ecp5 -> WireId -> Text
getWireBasename e w = wiName (wireAt (e5Chipdb e) w)

-- | @get_wire_by_loc_basename@: the wire at a location with the given
-- basename (Nothing when absent).
getWireByLocBasename :: Ecp5 -> Location -> Text -> Maybe WireId
getWireByLocBasename e loc basename =
    let lt = locTypeOfTile (e5Chipdb e) (tileIndex (e5Chipdb e) loc)
     in V.findIndex ((== basename) . wiName) (ltWires lt)
          <&> \i -> WireId loc (fromIntegral i)

-- | @get_pio_bel_bank@: the IO bank of a PIO bel.
getPioBelBank :: Ecp5 -> BelId -> Int
getPioBelBank e bel =
    case V.find (\pi -> locAdd (Location (pioAbsDx pi) (pioAbsDy pi)) (Location 0 0) == belLoc bel && pioBelIndex pi == fromIntegral (belIdx bel)) (cdPios (e5Chipdb e)) of
        Just pi -> fromIntegral (pioBank pi)
        Nothing -> error "lambdapnr: getPioBelBank: failed to find PIO"

-- | @get_pio_function_name@: the special function name of a PIO bel.
getPioFunctionName :: Ecp5 -> BelId -> Text
getPioFunctionName e bel =
    case V.find (\pi -> locAdd (Location (pioAbsDx pi) (pioAbsDy pi)) (Location 0 0) == belLoc bel && pioBelIndex pi == fromIntegral (belIdx bel)) (cdPios (e5Chipdb e)) of
        Just pi -> pioFunctionName pi
        Nothing -> ""

-- | @get_pio_by_function_name@: the PIO bel with the special function
-- name (e.g. @VREF1_3@).
getPioByFunctionName :: Ecp5 -> Text -> Maybe BelId
getPioByFunctionName e name =
    case V.find ((== name) . pioFunctionName) (cdPios (e5Chipdb e)) of
        Just pi ->
            Just BelId{belLoc = locAdd (Location (pioAbsDx pi) (pioAbsDy pi)) (Location 0 0), belIdx = fromIntegral (pioBelIndex pi)}
        Nothing -> Nothing

-- | @get_package_pin_bel@: the bel for a package pin name.
getPackagePinBel :: Ecp5 -> Text -> Maybe BelId
getPackagePinBel e pin =
    let cd = e5Chipdb e
     in case V.find (\pkg -> pin `elem` map ppName (V.toList (pkgPins pkg))) (cdPackages cd) of
            Nothing -> Nothing
            Just pkg ->
                case V.find ((== pin) . ppName) (pkgPins pkg) of
                    Just pp -> Just BelId{belLoc = locAdd (Location (ppAbsDx pp) (ppAbsDy pp)) (Location 0 0), belIdx = fromIntegral (ppBelIndex pp)}
                    Nothing -> Nothing

-- | @get_bel_package_pin@: the package pin name for a bel.
getBelPackagePin :: Ecp5 -> BelId -> Text
getBelPackagePin e bel =
    let cd = e5Chipdb e
     in case V.find (\pkg -> any (\pp -> locAdd (Location (ppAbsDx pp) (ppAbsDy pp)) (Location 0 0) == belLoc bel && ppBelIndex pp == fromIntegral (belIdx bel)) (V.toList (pkgPins pkg))) (cdPackages cd) of
            Nothing -> ""
            Just pkg ->
                case V.find (\pp -> locAdd (Location (ppAbsDx pp) (ppAbsDy pp)) (Location 0 0) == belLoc bel && ppBelIndex pp == fromIntegral (belIdx bel)) (pkgPins pkg) of
                    Just pp -> ppName pp
                    Nothing -> ""

-- | @get_pio_dqs_group@: DQS group of a PIO (dqsright, dqsrow).
getPioDqsGroup :: Ecp5 -> BelId -> Maybe (Bool, Int)
getPioDqsGroup e bel =
    case V.find (\pi -> locAdd (Location (pioAbsDx pi) (pioAbsDy pi)) (Location 0 0) == belLoc bel && pioBelIndex pi == fromIntegral (belIdx bel)) (cdPios (e5Chipdb e)) of
        Just pi -> Just (fromIntegral (pioDqsGroup pi) < 0, abs (fromIntegral (pioDqsGroup pi)))
        Nothing -> Nothing


-- ---------------------------------------------------------------------------
-- LPF (@ecp5\/lpf.cc@: @Arch::apply_lpf@)
-- ---------------------------------------------------------------------------

-- | @Arch::apply_lpf@: parse an LPF constraint file (LOCATE \/ IOBUF \/
-- SYSCONFIG \/ FREQUENCY \/ BLOCK). Applied after JSON load, before
-- packing. Interning order must match the C++: command words are not
-- interned, LOCATE interns the cell name (then the [0]-stripped retry),
-- IOBUF interns each attribute key, and @input\/lpf@ is interned last.
applyLpf ::
    Ecp5 ->
    -- | filename (stored in settings verbatim)
    Text ->
    -- | file contents
    Text ->
    M.Map IdString Property ->
    Design BelId WireId PipId ->
    IO (Either String (M.Map IdString Property, Design BelId WireId PipId))
applyLpf e filename contents settings d0 = go 1 "" settings d0 (T.lines contents)
  where
    tbl = e5IdTable e
    cidOf t = maybe emptyId id (M.lookup t (e5ConstIdByName e))
    warn msg = hPutStrLn stderr ("Warning: " ++ msg)
    sysconfigKeys =
        [ "SLAVE_SPI_PORT", "MASTER_SPI_PORT", "SLAVE_PARALLEL_PORT"
        , "BACKGROUND_RECONFIG", "DONE_EX", "DONE_OD"
        , "DONE_PULL", "MCCLK_FREQ", "TRANSFR"
        , "CONFIG_IOVOLTAGE", "CONFIG_SECURE", "WAKE_UP"
        , "COMPRESS_CONFIG", "CONFIG_MODE", "INBUF"
        ]
    iobufKeys =
        [ "IO_TYPE", "BANK", "BANK_VCC", "VREF", "PULLMODE", "DRIVE", "SLEWRATE"
        , "CLAMP", "OPENDRAIN", "DIFFRESISTOR", "DIFFDRIVE", "HYSTERESIS", "TERMINATION"
        ]
    isBlankish c = isSpace c || c == '\r' || c == '\n'
    isEmptyLine s = T.all isBlankish s
    cutComment s = let (a, _) = T.breakOn "#" s in fst (T.breakOn "//" a)
    stripQuotes lineno str
        | T.null str = str
        | T.head str == '"' =
            if T.last str /= '"'
                then errAt lineno ("expected '\"' at end of string '" ++ T.unpack str ++ "'")
                else T.dropEnd 1 (T.drop 1 str)
        | otherwise = str
    errAt lineno m = error ("lpf " ++ show lineno ++ ": " ++ m)
    setAttr cell key val d = setCellAttr cell key (propFromString val) d

    go :: Int -> Text -> M.Map IdString Property -> Design BelId WireId PipId -> [Text] -> IO (Either String (M.Map IdString Property, Design BelId WireId PipId))
    go _ linebuf st d [] =
        if isEmptyLine linebuf
            then do
                inputLpf <- intern tbl "input/lpf"
                pure (Right (M.insert inputLpf (propFromString filename) st, d))
            else pure (Left "unexpected end of LPF file")
    go lineno linebuf st d (line : rest) =
        let line' = cutComment line
         in if isEmptyLine line'
                then go (lineno + 1) linebuf st d rest
                else execCommands lineno (linebuf <> line') st d rest

    execCommands :: Int -> Text -> M.Map IdString Property -> Design BelId WireId PipId -> [Text] -> IO (Either String (M.Map IdString Property, Design BelId WireId PipId))
    execCommands lineno buf st d rest =
        case T.breakOn ";" buf of
            (cmd, after)
                | not (T.null after) -> do
                    r <- try (execCommand lineno cmd st d)
                    case r of
                        Left (err :: SomeException) -> pure (Left (show err))
                        Right (st', d') -> execCommands lineno (T.drop 1 after) st' d' rest
            _ -> go (lineno + 1) buf st d rest

    execCommand :: Int -> Text -> M.Map IdString Property -> Design BelId WireId PipId -> IO (M.Map IdString Property, Design BelId WireId PipId)
    execCommand lineno cmd st d =
        case T.words cmd of
            [] -> pure (st, d)
            (verb : ws) -> do
                if
                    | verb == "BLOCK" ->
                        case ws of
                            [w] | w == "ASYNCPATHS" || w == "RESETPATHS" -> pure (st, d)
                            _ -> warn ("ignoring unsupported LPF command '" ++ T.unpack cmd ++ "' (on line " ++ show lineno ++ ")") >> pure (st, d)
                    | verb == "SYSCONFIG" -> do
                        st' <- foldM (sysconfigOne lineno) st ws
                        pure (st', d)
                    | verb == "FREQUENCY" ->
                        case ws of
                            [] -> errAt lineno "expected object type after FREQUENCY"
                            (etype : rest') ->
                                if etype == "PORT" || etype == "NET"
                                    then
                                        if length rest' < 3
                                            then errAt lineno ("expected frequency value and unit after 'FREQUENCY " ++ T.unpack etype ++ "'")
                                            else
                                                let target = stripQuotes lineno (rest' !! 0)
                                                    freqTxt = rest' !! 1
                                                    unit = T.toUpper (rest' !! 2)
                                                    freqMhz =
                                                        case reads (T.unpack freqTxt) :: [(Double, String)] of
                                                            [] -> Nothing
                                                            ((f, _) : _) ->
                                                                if unit == "MHZ" then Just f
                                                                else if unit == "KHZ" then Just (f / 1.0e3)
                                                                else if unit == "HZ" then Just (f / 1.0e6)
                                                                else Nothing
                                                 in case freqMhz of
                                                        Nothing
                                                            | unit /= "MHZ" && unit /= "KHZ" && unit /= "HZ" ->
                                                                errAt lineno ("unsupported frequency unit '" ++ T.unpack unit ++ "'")
                                                            | otherwise -> errAt lineno ("invalid frequency value '" ++ T.unpack freqTxt ++ "'")
                                                        Just _ -> do
                                                            _ <- intern tbl target
                                                            warn ("FREQUENCY clock constraints not yet supported (ignoring " ++ T.unpack target ++ ")")
                                                            pure (st, d)
                                    else warn ("ignoring unsupported LPF command '" ++ T.unpack cmd ++ " " ++ T.unpack etype ++ "' (on line " ++ show lineno ++ ")") >> pure (st, d)
                    | verb == "LOCATE" -> do
                        if length ws < 4
                            then errAt lineno "expected syntax 'LOCATE COMP <port name> SITE <pin>'"
                            else
                                if ws !! 0 /= "COMP"
                                    then errAt lineno "expected 'COMP' after 'LOCATE'"
                                    else
                                        let cell = stripQuotes lineno (ws !! 1)
                                         in if ws !! 2 /= "SITE"
                                                then errAt lineno ("expected 'SITE' after 'LOCATE COMP " ++ T.unpack cell ++ "'")
                                                else
                                                    if length ws > 4
                                                        then errAt lineno "unexpected input following LOCATE clause"
                                                        else do
                                                            cellId <- intern tbl cell
                                                            (foundId, cellId') <-
                                                                case M.lookup cellId (designCells d) of
                                                                    Just _ -> pure (True, cellId)
                                                                    Nothing
                                                                        | T.length cell >= 3 && T.isSuffixOf "[0]" cell -> do
                                                                            cellId2 <- intern tbl (T.dropEnd 3 cell)
                                                                            pure (M.member cellId2 (designCells d), cellId2)
                                                                        | otherwise -> pure (False, cellId)
                                                            pure
                                                                ( st
                                                                , if foundId then setAttr cellId' (cidOf "LOC") (stripQuotes lineno (ws !! 3)) d else d
                                                                )
                    | verb == "IOBUF" -> do
                        if length ws < 2
                            then errAt lineno "expected syntax 'IOBUF PORT <port name> <attr>=<value>...'"
                            else
                                if ws !! 0 /= "PORT"
                                    then errAt lineno "expected 'PORT' after 'IOBUF'"
                                    else do
                                        let cell = stripQuotes lineno (ws !! 1)
                                        cellId <- intern tbl cell
                                        d' <-
                                            if M.member cellId (designCells d)
                                                then foldM (iobufOne lineno cell cellId) d (drop 2 ws)
                                                else pure d
                                        pure (st, d')
                    | otherwise -> pure (st, d)

    sysconfigOne :: Int -> M.Map IdString Property -> Text -> IO (M.Map IdString Property)
    sysconfigOne lineno st setting =
        let (key, after) = T.breakOn "=" setting
         in if T.null after
                then errAt lineno "expected syntax 'SYSCONFIG <attr>=<value>...'"
                else
                    if key `notElem` sysconfigKeys
                        then errAt lineno ("unexpected SYSCONFIG key '" ++ T.unpack key ++ "'")
                        else do
                            keyId <- intern tbl ("arch.sysconfig." <> key)
                            pure (M.insert keyId (propFromString (T.drop 1 after)) st)

    iobufOne :: Int -> Text -> IdString -> Design BelId WireId PipId -> Text -> IO (Design BelId WireId PipId)
    iobufOne lineno cell cellId d setting =
        let (key, after) = T.breakOn "=" setting
         in if T.null after
                then errAt lineno "expected syntax 'IOBUF PORT <port name> <attr>=<value>...'"
                else do
                    if key `notElem` iobufKeys
                        then warn ("IOBUF '" ++ T.unpack cell ++ "' attribute '" ++ T.unpack key ++ "' is not recognised (on line " ++ show lineno ++ ")")
                        else pure ()
                    keyId <- intern tbl key
                    pure (setAttr cellId keyId (T.drop 1 after) d)
