{-# LANGUAGE OverloadedStrings #-}

{- | The ECP5 bitstream generator — the Haskell mirror of
@ecp5\/bitstream.cc@ (@ECP5Bitgen@) for the @--textcfg@ output: the
prjtrellis text config that @ecppack@ packs into a bitstream.

Pipeline (mirroring @ECP5Bitgen::run@): start from the device's empty
base configuration (or a parsed @--basecfg@ file), add the @Part:@
metadata, clear DCU tie-offs when DCUs are used, then every bound
configurable pip as a routing @arc:@, then per-cell settings (comb,
ff, io, dcc, bram, dsp, pll, iologic, dcu, ...), then SYSCONFIG
settings, then the SERDES tile-name fixups.
-}
module Lambdapnr.Arch.Ecp5.Bitgen (
    buildConfig,
    getTrellisWireName,
    getPipTileName,
    getTileByTypeLoc,
    fullChipName,
) where


import Data.Bits (shiftL, (.&.), (.|.))
import Data.Char (toUpper)
import Text.Printf (printf)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Lambdapnr.Arch.Ecp5 hiding (locX, locY, locZ)
import Lambdapnr.Arch.Ecp5.ArchCellInfo (ArchInfo, CombInfo (..), RamInfo (..), combFlag, hasFlag, lookupComb, lookupRam, CombFlags (CombCarry))
import Lambdapnr.Arch.Ecp5.BaseConfigs (configEmpty)
import Lambdapnr.Arch.Ecp5.Binding (boundPipNet, boundWireNet)
import Lambdapnr.Arch.Ecp5.CellTiming (TimingDb (..))
import Lambdapnr.Arch.Ecp5.Chipdb
import Lambdapnr.Arch.Ecp5.Config
import Lambdapnr.Arch.Ecp5.DcuBitstream (dcuWords)
import Lambdapnr.Arch.Ecp5.Types
import Lambdapnr.Kernel.Arch hiding (locX, locY, locZ)
import Lambdapnr.Kernel.IdString (IdString, emptyId, idByName, idToText)
import Lambdapnr.Kernel.Netlist
import Lambdapnr.Kernel.Property (Property (..), propAsInt64, propIsString, propAsString)

-- | The full chip name (@get_full_chip_name@): @LFE5U-12F-6CABGA256@.
fullChipName :: Ecp5Args -> Text
fullChipName args =
    deviceName (eaDevice args)
        <> "-"
        <> speedGradeStr (eaSpeed args)
        <> eaPackage args
  where
    speedGradeStr Speed6 = "6"
    speedGradeStr Speed7 = "7"
    speedGradeStr Speed8 = "8"
    speedGradeStr Speed85g = "8"

-- | The name of the tile a pip's config lives in (@get_pip_tilename@):
-- the tile at the pip's location whose type matches the pip's
-- @tile_type@.
getPipTileName :: Ecp5 -> PipId -> Text
getPipTileName e p =
    let cd = ecp5Chipdb e
        pi = pipAt cd p
        tileIdx = tileIndex cd (pipLoc p)
        names = tiTileNames (cdTileInfos cd V.! tileIdx)
     in case V.find ((== fromIntegral (piTileType pi)) . tnTypeIdx) names of
            Just tn -> tnName tn
            Nothing -> ""


-- | The trellis wire name of a wire relative to a location
-- (@get_trellis_wirename@): G_/L_/R_ wires and same-tile wires keep
-- their basename; others get an N/S/E/W relative prefix.
getTrellisWireName :: Ecp5 -> Location -> WireId -> Text
getTrellisWireName e loc wire =
    let basename = getWireBasename e wire
        prefix2 = T.take 2 basename
     in if prefix2 == "G_" || prefix2 == "L_" || prefix2 == "R_"
            then basename
            else
                if loc == wireLoc wire
                    then basename
                    else relPrefix <> "_" <> basename
  where
    relPrefix =
        T.concat
            [ if locY (wireLoc wire) < locY loc then "N" <> tshow (fromIntegral (locY loc - locY (wireLoc wire))) else ""
            , if locY (wireLoc wire) > locY loc then "S" <> tshow (fromIntegral (locY (wireLoc wire) - locY loc)) else ""
            , if locX (wireLoc wire) > locX loc then "E" <> tshow (fromIntegral (locX (wireLoc wire) - locX loc)) else ""
            , if locX (wireLoc wire) < locX loc then "W" <> tshow (fromIntegral (locX loc - locX (wireLoc wire))) else ""
            ]
    tshow = T.pack . show

-- ---------------------------------------------------------------------------
-- IO types (mirror of @ecp5\/pio.cc@ + @iotypes.inc@)
-- ---------------------------------------------------------------------------

data IOType = TypeNone | TypeUnknown | IoType Text

data IOVoltage
    = Vcc3V3
    | Vcc2V5
    | Vcc1V8
    | Vcc1V5
    | Vcc1V35
    | Vcc1V2
    deriving (Eq, Show)

iovoltageToStr :: IOVoltage -> Text
iovoltageToStr Vcc3V3 = "3V3"
iovoltageToStr Vcc2V5 = "2V5"
iovoltageToStr Vcc1V8 = "1V8"
iovoltageToStr Vcc1V5 = "1V5"
iovoltageToStr Vcc1V35 = "1V35"
iovoltageToStr Vcc1V2 = "1V2"

ioTypeFromStr :: Text -> IOType
ioTypeFromStr "NONE" = TypeNone
ioTypeFromStr t
    | t `elem` ioTypes = IoType t
    | otherwise = TypeUnknown

-- | The iotypes.inc list.
ioTypes :: [Text]
ioTypes =
    [ "LVTTL33", "LVCMOS33", "LVCMOS25", "LVCMOS18", "LVCMOS15", "LVCMOS12"
    , "SSTL18_I", "SSTL18_II", "SSTL15_I", "SSTL15_II", "SSTL135_I", "SSTL135_II", "HSUL12"
    , "SSTL18D_I", "SSTL18D_II", "SSTL135D_I", "SSTL135D_II", "SSTL15D_I", "SSTL15D_II", "HSUL12D"
    , "LVCMOS33D", "LVCMOS25D", "LVCMOS15D", "LVCMOS12D"
    , "LVDS", "BLVDS25", "MLVDS25", "LVPECL33", "SLVS", "SUBLVDS", "LVCMOS18D"
    , "LVDS25E", "BLVDS25E", "MLVDS25E", "LVPECL33E"
    ]

getVccio :: IOType -> IOVoltage
getVccio (IoType t)
    | t `elem` ["LVTTL33", "LVCMOS33", "LVCMOS33D", "LVPECL33", "LVPECL33E"] = Vcc3V3
    | t `elem` ["LVCMOS25", "LVCMOS25D", "LVDS", "SLVS", "SUBLVDS", "LVDS25E", "MLVDS25", "MLVDS25E", "BLVDS25"] = Vcc2V5
    | t `elem` ["LVCMOS18", "LVCMOS18D", "SSTL18_I", "SSTL18_II", "SSTL18D_I", "SSTL18D_II"] = Vcc1V8
    | t `elem` ["LVCMOS15", "LVCMOS15D", "SSTL15_I", "SSTL15_II", "SSTL15D_I", "SSTL15D_II"] = Vcc1V5
    | t `elem` ["SSTL135_I", "SSTL135_II", "SSTL135D_I", "SSTL135D_II"] = Vcc1V35
    | otherwise = Vcc1V2
getVccio _ = Vcc1V2

isDifferential :: IOType -> Bool
isDifferential (IoType t) =
    t
        `elem` [ "LVCMOS33D", "LVCMOS25D", "LVCMOS15D", "LVCMOS12D", "LVPECL33", "LVDS", "MLVDS25", "BLVDS25"
               , "SLVS", "SUBLVDS", "LVCMOS18D", "SSTL18D_I", "SSTL18D_II", "SSTL15D_I", "SSTL15D_II"
               , "SSTL135D_I", "SSTL135D_II", "HSUL12D"
               ]
isDifferential _ = False

isReferenced :: IOType -> Bool
isReferenced (IoType t) =
    t
        `elem` [ "SSTL18_I", "SSTL18_II", "SSTL18D_I", "SSTL18D_II", "SSTL15_I", "SSTL15_II"
               , "SSTL15D_I", "SSTL15D_II", "SSTL135_I", "SSTL135_II", "SSTL135D_I", "SSTL135D_II"
               , "HSUL12", "HSUL12D"
               ]
isReferenced _ = False

-- ---------------------------------------------------------------------------
-- The bitgen state
-- ---------------------------------------------------------------------------

data Bitgen = Bitgen
    { bgE :: Ecp5
    , bgArchInfo :: ArchInfo
    , bgDesign :: Design BelId WireId PipId
    , bgSettings :: M.Map IdString Property
    , bgCfg :: ChipConfig
    , bgBankVcc :: M.Map Int IOVoltage
    , bgBankLvds :: S.Set Int
    , bgBankVref :: S.Set Int
    , bgBankDiff :: S.Set Int
    , bgDriveWarning :: Bool
    }

-- | Build the chip configuration from the arch + the (routed, bound)
-- design + its arch info.
buildConfig :: Ecp5 -> ArchInfo -> Design BelId WireId PipId -> M.Map IdString Property -> ChipConfig
buildConfig e ai design settings =
    ccFinal
  where
    args = ecp5Args e
    baseName = if eaDevice args == Lfe5u12f then "LFE5U-25F" else deviceName (eaDevice args)
    cc0 = emptyChipConfig{ccChipName = deviceName (eaDevice args), ccTiles = configEmpty baseName}
    b0 = Bitgen e ai design settings cc0 M.empty S.empty S.empty S.empty False

    -- DCU tie-off clearing (before pips)
    b1 = foldl clearDcuTies b0 (cellsIter design)
    -- metadata
    b2 = b1{bgCfg = (bgCfg b1){ccMetadata = ["Part: " <> fullChipName args]}}
    -- bound pips
    b3 = foldl addPip b2 (getPips e)
    -- IO banks
    b4 = initIoBanks b3
    -- cells
    b5 = foldl addCellConfig b4 (cellsIter design)
    -- SYSCONFIG settings
    b6 = foldl addSysConfig b5 (M.toList settings)
    -- tile name fixups
    ccFinal = fixTileNames (bgCfg b6)

    -- clear DCU tie-offs in the base config: for every DCUA, the enums
    -- and unknowns of the tiles at (y-1, x+i) are dropped
    clearDcuTies b ci
        | cellType ci == cid "DCUA" =
            case cellBel ci of
                Just bel ->
                    let Loc bx by _ = getBelLocation e bel
                        clearTile cc i =
                            foldl
                                (\cc' (tname, _) ->
                                    case M.lookup tname (ccTiles cc') of
                                        Just tc -> cc'{ccTiles = M.insert tname tc{tcEnums = [], tcUnknowns = []} (ccTiles cc')}
                                        Nothing -> cc')
                                cc
                                (getTilesAtLoc e (by - 1) (bx + i))
                     in b{bgCfg = foldl clearTile (bgCfg b) [0 .. 11]}
                Nothing -> b
        | otherwise = b

    -- bound, configurable pips as arcs
    addPip b p =
        case boundPipNet p (ecp5Bind e) of
            Nothing -> b
            Just _ ->
                let pi = pipAt (ecp5Chipdb e) p
                 in if piPipType pi /= 0
                        then b
                        else
                            let source = getTrellisWireName e (pipLoc p) (getPipSrcWire e p)
                             in if "CLKI_PLL" `T.isInfixOf` source
                                    then foldl (\b' ep -> setPip b' ep) b [ep | ep <- getPipsUphill e (getPipDstWire e p), getPipSrcWire e ep == getPipSrcWire e p]
                                    else setPip b p
    setPip b p =
        let tile = getPipTileName e p
            source = getTrellisWireName e (pipLoc p) (getPipSrcWire e p)
            sink = getTrellisWireName e (pipLoc p) (getPipDstWire e p)
            tc = M.findWithDefault emptyTileConfig tile (ccTiles (bgCfg b))
         in b{bgCfg = (bgCfg b){ccTiles = M.insert tile (addArc sink source tc) (ccTiles (bgCfg b))}}

    -- cell configuration dispatch
    addCellConfig b ci =
        case idToType ci of
            "TRELLIS_COMB" -> writeComb b ci
            "TRELLIS_FF" -> writeFf b ci
            "TRELLIS_RAMW" -> writeRamw b ci
            "TRELLIS_IO" -> writeIo b ci
            "DCCA" -> writeDcc b ci
            "DCSC" -> writeDcsc b ci
            "DP16KD" -> writeBram b ci
            "MULT18X18D" -> writeMult18 b ci
            "ALU54B" -> writeAlu54 b ci
            "EHXPLLL" -> writePll b ci
            "IOLOGIC" -> writeIol b ci
            "SIOLOGIC" -> writeIol b ci
            "DCUA" -> writeDcu b ci
            "EXTREFB" -> writeExtrefb b ci
            "PCSCLKDIV" -> writePcsclkdiv b ci
            "DTR" -> writeDtr b ci
            "OSCG" -> writeOscg b ci
            "USRMCLK" -> writeUsrmclk b ci
            "GSR" -> writeGsr b ci
            "JTAGG" -> writeJtagg b ci
            "CLKDIVF" -> writeClkdivf b ci
            "TRELLIS_ECLKBUF" -> b
            "DQSBUFM" -> writeDqsbuf b ci
            "ECLKSYNCB" -> writeEclksyncb b ci
            "ECLKBRIDGECS" -> writeEclkbridgecs b ci
            "DDRDLL" -> writeDdrdll b ci
            _ -> b

    -- the packed cell type as text (the constid name)
    idToType ci = idToText (ecp5IdTable e) (cellType ci)

    -- constid helpers
    cid :: Text -> IdString
    cid t = fromMaybe (fromMaybe emptyId (M.lookup t (tdConstIdByName (ecp5TimingDb e)))) (idByName (ecp5IdTable e) t)

    -- param helpers (str_or_default / int_or_default / intstr_or_default)
    strParam ci name def =
        case M.lookup (cid name) (cellParams ci) of
            Nothing -> def
            Just p -> if propIsString p then propAsString p else T.pack (show (propAsInt64 p))
    intParam ci name def =
        case M.lookup (cid name) (cellParams ci) of
            Nothing -> def
            Just p -> if propIsString p then readInt (T.unpack (propAsString p)) else fromIntegral (propAsInt64 p)
    intstrParam ci name def =
        case M.lookup (cid name) (cellParams ci) of
            Nothing -> def
            Just p -> if propIsString p then propAsString p else T.pack (show (propAsInt64 p))
    attrStr ci name def =
        case M.lookup (cid name) (cellAttrs ci) of
            Nothing -> def
            Just p -> if propIsString p then propAsString p else T.pack (show (propAsInt64 p))
    attrInt ci name def =
        case M.lookup (cid name) (cellAttrs ci) of
            Nothing -> def
            Just p -> if propIsString p then readInt (T.unpack (propAsString p)) else fromIntegral (propAsInt64 p)
    boolParam ci name def =
        case M.lookup (cid name) (cellParams ci) of
            Nothing -> def
            Just p -> if propIsString p then T.unpack (propAsString p) == "1" || T.unpack (propAsString p) == "true" else propAsInt64 p /= 0
    readInt t = case reads t of
        [(i, "")] -> i
        _ -> 0

    -- bit vectors
    intToBits n size = [n `div` (2 ^ i) `mod` 2 == 1 | i <- [0 .. size - 1]]
    bitReverse x size =
        sum [if x `div` (2 ^ i) `mod` 2 == 1 then 2 ^ ((size - 1) - i) else 0 | i <- [0 .. size - 1]]
    chtohex c = case T.findIndex (== toUpper c) "0123456789ABCDEF" of
        Just i -> i
        Nothing -> 99

    -- parse_init_str (hex-string or bit vector property)
    printf' i = T.pack (printf "INITVAL_%02X" i)

    parseInitStr :: CellInfo bel wire pip -> Text -> Int -> [Bool]
    parseInitStr ci pname length' =
        case M.lookup (cid pname) (cellParams ci) of
            Nothing -> replicate length' False
            Just p
                | propIsString p ->
                    let str = T.unpack (propAsString p)
                     in if take 2 str /= "0x"
                            then replicate length' False
                            else parseHex str length'
                | otherwise -> take length' (map (== '1') (T.unpack (pStr p)) ++ repeat False)
    parseHex str length' =
        let hexChars = reverse (drop 2 str)
            nibble c = fromMaybe 0 (T.findIndex (== toUpper c) "0123456789ABCDEF")
            bitsOf i =
                if i * 4 < length'
                    then [ (i * 4 + k < length') && nibble (hexChars !! i) `div` (2 ^ k) `mod` 2 == 1 | k <- [0 .. 3] ]
                    else []
         in concatMap bitsOf [0 .. length hexChars - 1] ++ replicate (length' - length (concatMap bitsOf [0 .. length hexChars - 1])) False

    -- parse_config_str (0b/0x/0d/decimal or bit vector)
    parseConfigStr p length' =
        case p of
            PropStr str ->
                let s = T.unpack str
                    base = take 2 s
                    lsbVal i c = c == '1'
                 in if base == "0b"
                        then [lsbVal i (s !! (length s - 1 - i)) | i <- [0 .. length' - 1]]
                        else
                            if base == "0x"
                                then parseHex s length'
                                else
                                    if base == "0d"
                                        then intToBits (readInt (drop 2 s)) length'
                                        else intToBits (readInt s) length'
            PropNum _ n -> take length' (intToBits (fromIntegral n) length' ++ repeat False)

    -- str_to_bitvector: "0b..." string
    strToBits str length' =
        if T.take 2 str /= "0b"
            then replicate length' False
            else
                let s = T.unpack str
                 in [s !! (length s - 1 - i) == '1' | i <- [0 .. length' - 1]]

    -- tie a wire via the CIB ties
    tieCibSignal :: WireId -> Bool -> Bitgen -> Bitgen
    tieCibSignal wire value b =
        go [wire] b
      where
        cibRe name =
            -- the C++ std::regex_match "J([A-D]|CE|LSR|CLK)[0-7]": a FULL
            -- match — 'J' + one alternative + exactly one digit.
            case T.unpack name of
                [c, a, d] -> c == 'J' && a `elem` ("ABCD" :: String) && d `elem` ("01234567" :: String)
                [c, a, b, d] -> c == 'J' && [a, b] == "CE" && d `elem` ("01234567" :: String)
                [c, a, b, cc, d] -> c == 'J' && [a, b, cc] `elem` ["LSR", "CLK"] && d `elem` ("01234567" :: String)
                _ -> False

        go queue b' =
            case queue of
                [] -> b'
                cibsig : rest ->
                    let basename = getWireBasename e cibsig
                     in if cibRe basename
                        then
                            let outValue
                                    | T.isPrefixOf "JCLK" basename || T.isPrefixOf "JLSR" basename = False
                                    | otherwise = value
                                tiles = getTilesAtLoc e (fromIntegral (locY (wireLoc cibsig))) (fromIntegral (locX (wireLoc cibsig)))
                             in case [tn | (tn, ty) <- tiles, T.isPrefixOf "CIB" ty || T.isPrefixOf "VCIB" ty] of
                                    tile : _ ->
                                        let tc = M.findWithDefault emptyTileConfig tile (ccTiles (bgCfg b'))
                                         in b'{bgCfg = (bgCfg b'){ccTiles = M.insert tile (addEnum ("CIB." <> basename <> "MUX") (if outValue then "1" else "0") tc) (ccTiles (bgCfg b'))}}
                                    [] -> b'
                        else go (rest ++ map (getPipSrcWire e) (getPipsUphill e cibsig)) b'

    -- tile/wire lookup wrappers (the Ecp5.hs variants return Maybe)
    ttl e' row col typ = fromMaybe "" (getTileByTypeLoc e' row col typ)
    ttls e' row col types = fromMaybe "" (getTileByTypeLocSet e' row col types)
    belPinWireOf bel p = fromMaybe (error "lambdapnr: unplaced bel in bitgen") (getBelPinWire e bel p)

    -- addTileEnum / addTileWord helpers
    addTileEnum b tile name value =
        let tc = M.findWithDefault emptyTileConfig tile (ccTiles (bgCfg b))
         in b{bgCfg = (bgCfg b){ccTiles = M.insert tile (addEnum name value tc) (ccTiles (bgCfg b))}}
    addTileWord b tile name value =
        let tc = M.findWithDefault emptyTileConfig tile (ccTiles (bgCfg b))
         in b{bgCfg = (bgCfg b){ccTiles = M.insert tile (addWord name value tc) (ccTiles (bgCfg b))}}
    addTileGroup b tg = b{bgCfg = (bgCfg b){ccTileGroups = ccTileGroups (bgCfg b) ++ [tg]}}

    -- get a cell's port net
    -- The C++ iterates @ci->ports@ (a nextpnr dict = reverse insertion
    -- order); @cellPortOrder@ is the insertion order, so reverse it.
    portOrderRefs :: CellInfo BelId WireId PipId -> [(IdString, PortInfo)]
    portOrderRefs ci = [ (p, fromMaybe (error "bitgen: port missing") (M.lookup p (cellPorts ci))) | p <- reverse (cellPortOrder ci) ]

    cellPortNet ci p = case portNet =<< M.lookup (cid p) (cellPorts ci) of
        Just n -> Just n
        Nothing -> Nothing

    -- ------------------------------------------------------------------
    -- permute_lut
    -- ------------------------------------------------------------------
    permuteLut :: CellInfo BelId WireId PipId -> S.Set Text -> Int -> (S.Set Text, Int)
    permuteLut cell usedPhysPins origInit =
        let ports = [("A", 0), ("B", 1), ("C", 2), ("D", 3)]
            -- C++: for each bound uphill pip of pin i's wire, push i into
            -- phys_to_log[from_pin] (non-permuting: from_pin = i).
            physToLogOf i =
                let pinWire = belPinWireOf (fromMaybe (error "unplaced comb") (cellBel cell)) (cid (fst (ports !! i)))
                 in [ ( if lp .&. 0x4000 == 0 then i else fromIntegral (lp .&. 0x3), i )
                    | p <- getPipsUphill e pinWire
                    , boundPipNet p (ecp5Bind e) /= Nothing
                    , let lp = piLutpermFlags (pipAt (ecp5Chipdb e) p)
                    ]
            contribs = concat [ physToLogOf i | i <- [0 .. 3] ]
            physToLog = [ [ i | (fp, i) <- contribs, fp == j ] | j <- [0 .. 3] ]
            usedPins = S.fromList [ fst (ports !! i) | i <- [0 .. 3], not (null (physToLog !! i)) ]
            -- CCU2 carry: keep the two halves split
            physToLog' =
                if hasFlag (ciFlags (lookupComb (cellName cell) ai)) (combFlag CombCarry)
                    then [ if null (physToLog !! i)
                            then [j | j <- [2 * (i `div` 2) .. 2 * (i `div` 2 + 1) - 1], boundWireNet (belPinWireOf (fromMaybe (error "unplaced") (cellBel cell)) (cid (fst (ports !! j)))) (ecp5Bind e) == Nothing]
                            else physToLog !! i
                         | i <- [0 .. 3] ]
                    else physToLog
            permutedInit = foldl setBit 0 [0 .. 15]
            setBit acc i =
                let logIdx = foldl (\acc' j -> if i `div` (2 ^ j) `mod` 2 == 1 then acc' .|. foldl (\a lp -> a .|. (1 `shiftL` lp)) 0 (physToLog' !! j) else acc') 0 [0 .. 3]
                 in if origInit `div` (2 ^ logIdx) `mod` 2 == 1 then acc .|. (1 `shiftL` i) else acc
         in (usedPins, permutedInit)
      where
        shiftL = \x n -> x * (2 ^ n)
        ciFlags (CombInfo f _ _ _) = f

    -- ------------------------------------------------------------------
    -- write_comb
    -- ------------------------------------------------------------------
    writeComb b ci =
        case cellBel ci of
            Nothing -> b
            Just bel ->
                let Loc bx by _ = getBelLocation e bel
                    tname = ttl e (fromIntegral by) (fromIntegral bx) "PLC2"
                    z = fromIntegral (belZOf bel) `div` 4
                    slice = "SLICE" <> T.singleton ("ABCD" !! (z `div` 2))
                    lc = T.pack (show (z `mod` 2))
                    mode = strParam ci "MODE" "LOGIC"
                 in if mode == "RAMW_BLOCK"
                        then b
                        else
                            let lutInit = intParam ci "INITVAL" 0
                                (usedPins, permuted) = permuteLut ci S.empty lutInit
                                b1 = addTileEnum b tname (slice <> ".MODE") mode
                                b2 = addTileWord b1 tname (slice <> ".K" <> lc <> ".INIT") (intToBits permuted 16)
                                b3 =
                                    if mode == "CCU2"
                                        then addTileEnum b2 tname (slice <> ".CCU2.INJECT1_" <> lc) (strParam ci "CCU2_INJECT1" "YES")
                                        else addTileEnum b2 tname (slice <> ".CCU2.INJECT1_" <> lc) "_NONE_"
                                b4 =
                                    if mode == "DPRAM" && slice == "SLICEA" && lc == "0"
                                        then
                                            let wckmux = strParam ci "WCKMUX" "WCK"
                                                wckmux' = if wckmux == "WCK" then "CLK" else wckmux
                                             in addTileEnum (addTileEnum b3 tname (slice <> ".WREMUX") (strParam ci "WREMUX" "WRE")) tname "CLK1.CLKMUX" wckmux'
                                        else b3
                                b5 = foldl (\b' pin -> if S.member pin usedPins then b' else addTileEnum b' tname (slice <> "." <> pin <> lc <> "MUX") "1") b4 ["A", "B", "C", "D"]
                             in b5
      where
        belZOf bel = biZ (belAt (ecp5Chipdb e) bel)

    -- ------------------------------------------------------------------
    -- write_ff
    -- ------------------------------------------------------------------
    writeFf b ci =
        case cellBel ci of
            Nothing -> b
            Just bel ->
                let Loc bx by _ = getBelLocation e bel
                    tname = ttl e (fromIntegral by) (fromIntegral bx) "PLC2"
                    z = fromIntegral (belZOf bel) `div` 4
                    slice = "SLICE" <> T.singleton ("ABCD" !! (z `div` 2))
                    lc = T.pack (show (z `mod` 2))
                    b1 = addTileEnum b tname (slice <> ".GSR") (strParam ci "GSR" "ENABLED")
                    b2 = addTileEnum b1 tname (slice <> ".REG" <> lc <> ".SD") (intstrParam ci "SD" "0")
                    b3 = addTileEnum b2 tname (slice <> ".REG" <> lc <> ".REGSET") (strParam ci "REGSET" "RESET")
                    b4 = addTileEnum b3 tname (slice <> ".REG" <> lc <> ".LSRMODE") (strParam ci "LSRMODE" "LSR")
                    b5 = addTileEnum b4 tname (slice <> ".CEMUX") (strParam ci "CEMUX" "1")
                    lsrnet = cellPortNet ci "LSR"
                    b6 =
                        case getWireByLocBasename e (Location (fromIntegral bx) (fromIntegral by)) "LSR0" of
                            Just w | getBoundWireNet e w == lsrnet ->
                                addTileEnum (addTileEnum b5 tname "LSR0.SRMODE" (strParam ci "SRMODE" "LSR_OVER_CE")) tname "LSR0.LSRMUX" (strParam ci "LSRMUX" "LSR")
                            _ -> b5
                    b7 =
                        case getWireByLocBasename e (Location (fromIntegral bx) (fromIntegral by)) "LSR1" of
                            Just w | getBoundWireNet e w == lsrnet ->
                                addTileEnum (addTileEnum b6 tname "LSR1.SRMODE" (strParam ci "SRMODE" "LSR_OVER_CE")) tname "LSR1.LSRMUX" (strParam ci "LSRMUX" "LSR")
                            _ -> b6
                    clknet = cellPortNet ci "CLK"
                    b8 =
                        case getWireByLocBasename e (Location (fromIntegral bx) (fromIntegral by)) "CLK0" of
                            Just w | getBoundWireNet e w == clknet ->
                                addTileEnum b7 tname "CLK0.CLKMUX" (strParam ci "CLKMUX" "CLK")
                            _ -> b7
                    b9 =
                        case getWireByLocBasename e (Location (fromIntegral bx) (fromIntegral by)) "CLK1" of
                            Just w | getBoundWireNet e w == clknet ->
                                addTileEnum b8 tname "CLK1.CLKMUX" (strParam ci "CLKMUX" "CLK")
                            _ -> b8
                 in b9
      where
        belZOf bel = biZ (belAt (ecp5Chipdb e) bel)

    -- ------------------------------------------------------------------
    -- write_ramw
    -- ------------------------------------------------------------------
    writeRamw b ci =
        case cellBel ci of
            Nothing -> b
            Just bel ->
                let Loc bx by _ = getBelLocation e bel
                    tname = ttl e (fromIntegral by) (fromIntegral bx) "PLC2"
                    b1 = addTileEnum b tname "SLICEC.MODE" "RAMW"
                    b2 = addTileWord b1 tname "SLICEC.K0.INIT" (replicate 16 False)
                    b3 = addTileWord b2 tname "SLICEC.K1.INIT" (replicate 16 False)
                 in b3

    -- ------------------------------------------------------------------
    -- IO
    -- ------------------------------------------------------------------
    pioAbcdL = ["PICL1", "PICL1_DQS0", "PICL1_DQS3"]
    pioAbcdR = ["PICR1", "PICR1_DQS0", "PICR1_DQS3"]
    pioaB = ["PICB0", "EFB0_PICB0", "EFB2_PICB0", "SPICB0"]
    piobB = ["PICB1", "EFB1_PICB1", "EFB3_PICB1"]
    picAbL = ["PICL0", "PICL0_DQS2"]
    picCdL = ["PICL2", "PICL2_DQS1", "MIB_CIB_LR"]
    picAbR = ["PICR0", "PICR0_DQS2"]
    picCdR = ["PICR2", "PICR2_DQS1", "MIB_CIB_LR_A"]
    picaB = ["PICB0", "EFB0_PICB0", "EFB2_PICB0", "SPICB0"]
    picbB = ["PICB1", "EFB1_PICB1", "EFB3_PICB1"]

    getPioTile bel =
        let pioName = belNameOf bel
            Loc bx by _ = getBelLocation e bel
            height = cdHeight (ecp5Chipdb e)
            width = cdWidth (ecp5Chipdb e)
         in if by == 0
                then
                    if pioName == "PIOA"
                        then ttl e 0 bx "PIOT0"
                        else
                            if pioName == "PIOB"
                                then ttl e 0 (bx + 1) "PIOT1"
                                else error "bad PIO location"
                else
                    if by == height - 1
                        then
                            if pioName == "PIOA"
                                then ttls e by bx pioaB
                                else
                                    if pioName == "PIOB"
                                        then ttls e by (bx + 1) piobB
                                        else error "bad PIO location"
                        else
                            if bx == 0
                                then ttls e (by + 1) bx pioAbcdL
                                else
                                    if bx == width - 1
                                        then ttls e (by + 1) bx pioAbcdR
                                        else error "bad PIO location"

    getPicTile bel =
        let pioName = belNameOf bel
            Loc bx by _ = getBelLocation e bel
            height = cdHeight (ecp5Chipdb e)
            width = cdWidth (ecp5Chipdb e)
         in if by == 0
                then
                    if pioName == "PIOA"
                        then ttl e 1 bx "PICT0"
                        else
                            if pioName == "PIOB"
                                then ttl e 1 (bx + 1) "PICT1"
                                else error "bad PIO location"
                else
                    if by == height - 1
                        then
                            if pioName == "PIOA"
                                then ttls e by bx picaB
                                else
                                    if pioName == "PIOB"
                                        then ttls e by (bx + 1) picbB
                                        else error "bad PIO location"
                        else
                            if bx == 0
                                then
                                    if pioName == "PIOA" || pioName == "PIOB"
                                        then ttls e by bx picAbL
                                        else ttls e (by + 2) bx picCdL
                                else
                                    if bx == width - 1
                                        then
                                            if pioName == "PIOA" || pioName == "PIOB"
                                                then ttls e by bx picAbR
                                                else ttls e (by + 2) bx picCdR
                                        else error "bad PIO location"

    belNameOf bel = belNameText (belAt (ecp5Chipdb e) bel)

    cibTiles = ["CIB", "CIB_LR", "CIB_LR_S", "CIB_EFB0", "CIB_EFB1"]

    writeIo b ci =
        case cellBel ci of
            Nothing -> b
            Just bel ->
                let Loc bx by _ = getBelLocation e bel
                    pio = belNameOf bel
                    iotype = attrStr ci "IO_TYPE" "LVCMOS33"
                    dir = strParam ci "DIR" "INPUT"
                    pioTile = getPioTile bel
                    picTile = getPicTile bel
                    ioType = ioTypeFromStr iotype
                    b1 = addTileEnum (addTileEnum b pioTile (pio <> ".BASE_TYPE") (dir <> "_" <> iotype)) picTile (pio <> ".BASE_TYPE") (dir <> "_" <> iotype)
                    b2 =
                        if isDifferential ioType
                            then
                                if by == 0
                                    then
                                        let cpioTile = ttl e 0 (bx + 1) "PIOT1"
                                            cpicTile = ttl e 1 (bx + 1) "PICT1"
                                         in addTileEnum (addTileEnum b1 cpioTile (pio <> ".BASE_TYPE") (dir <> "_" <> iotype)) cpicTile (pio <> ".BASE_TYPE") (dir <> "_" <> iotype)
                                    else
                                        let other = if pio == "PIOA" then "PIOB" else "PIOD"
                                         in addTileEnum (addTileEnum b1 pioTile (other <> ".PULLMODE") "NONE") pioTile (pio <> ".PULLMODE") "NONE"
                            else
                                if isReferenced ioType
                                    then addTileEnum b1 pioTile (pio <> ".PULLMODE") "NONE"
                                    else b1
                    b3 =
                        if dir /= "INPUT" && portUnconnected "T" && portUnconnected "IOLTO"
                            then
                                let jptWire = getWireByLocBasename e (Location (fromIntegral bx) (fromIntegral by)) ("JPADDT" <> T.pack [T.last pio])
                                 in case jptWire of
                                        Just w ->
                                            case getPipsUphill e w of
                                                (jptPip : _) ->
                                                    let cibWire = getPipSrcWire e jptPip
                                                        cibTile = ttls e (fromIntegral (locY (wireLoc cibWire))) (fromIntegral (locX (wireLoc cibWire))) cibTiles
                                                        cibWirename = getWireBasename e cibWire
                                                     in addTileEnum b2 cibTile ("CIB." <> cibWirename <> "MUX") "0"
                                                [] -> b2
                                        Nothing -> b2
                            else b2
                    b4 =
                        if (dir == "INPUT" || dir == "BIDIR") && not (isDifferential ioType) && not (isReferenced ioType)
                            then addTileEnum b3 pioTile (pio <> ".HYSTERESIS") (attrStr ci "HYSTERESIS" "ON")
                            else b3
                    b5 =
                        if M.member (cid "SLEWRATE") (cellAttrs ci) && not (isReferenced ioType)
                            then addTileEnum b4 pioTile (pio <> ".SLEWRATE") (attrStr ci "SLEWRATE" "SLOW")
                            else b4
                    b6 =
                        if M.member (cid "PULLMODE") (cellAttrs ci)
                            then addTileEnum b5 pioTile (pio <> ".PULLMODE") (attrStr ci "PULLMODE" "NONE")
                            else b5
                    b7 =
                        if M.member (cid "DIFFRESISTOR") (cellAttrs ci)
                            then addTileEnum b6 pioTile (pio <> ".DIFFRESISTOR") (attrStr ci "DIFFRESISTOR" "OFF")
                            else b6
                    b8 =
                        if M.member (cid "CLAMP") (cellAttrs ci)
                            then addTileEnum b7 pioTile (pio <> ".CLAMP") (attrStr ci "CLAMP" "OFF")
                            else b7
                    b9 =
                        if M.member (cid "DRIVE") (cellAttrs ci)
                            then
                                if iotype == "LVCMOS33"
                                    then addTileEnum b8 pioTile (pio <> ".DRIVE") (attrStr ci "DRIVE" "8")
                                    else
                                        if iotype == "LVCMOS33D"
                                            then
                                                if by == 0
                                                    then
                                                        let cpioTile = ttl e 0 (bx + 1) "PIOT1"
                                                         in addTileEnum (addTileEnum b8 pioTile "PIOA.DRIVE" (attrStr ci "DRIVE" "12")) cpioTile "PIOB.DRIVE" (attrStr ci "DRIVE" "12")
                                                    else
                                                        let other = if pio == "PIOA" then "PIOB" else "PIOD"
                                                         in addTileEnum (addTileEnum b8 pioTile (pio <> ".DRIVE") (attrStr ci "DRIVE" "12")) pioTile (other <> ".DRIVE") (attrStr ci "DRIVE" "12")
                                            else b8
                            else b8
                    b10 =
                        if M.member (cid "TERMINATION") (cellAttrs ci)
                            then
                                case getVccio ioType of
                                    Vcc1V8 -> addTileEnum b9 pioTile (pio <> ".TERMINATION_1V8") (attrStr ci "TERMINATION" "OFF")
                                    Vcc1V5 -> addTileEnum b9 pioTile (pio <> ".TERMINATION_1V5") (attrStr ci "TERMINATION" "OFF")
                                    Vcc1V35 -> addTileEnum b9 pioTile (pio <> ".TERMINATION_1V35") (attrStr ci "TERMINATION" "OFF")
                                    _ -> b9
                            else b9
                    b11 =
                        if M.member (cid "OPENDRAIN") (cellAttrs ci)
                            then
                                if isDifferential ioType
                                    then
                                        let other = if pio == "PIOA" then "PIOB" else "PIOD"
                                         in addTileEnum (addTileEnum b10 pioTile (pio <> ".OPENDRAIN") (attrStr ci "OPENDRAIN" "OFF")) pioTile (other <> ".OPENDRAIN") (attrStr ci "OPENDRAIN" "OFF")
                                    else addTileEnum b10 pioTile (pio <> ".OPENDRAIN") (attrStr ci "OPENDRAIN" "OFF")
                            else b10
                    datamuxOddr = strParam ci "DATAMUX_ODDR" "PADDO"
                    b12 = if datamuxOddr /= "PADDO" then addTileEnum b11 picTile (pio <> ".DATAMUX_ODDR") datamuxOddr else b11
                    datamuxOreg = strParam ci "DATAMUX_OREG" "PADDO"
                    b13 = if datamuxOreg /= "PADDO" then addTileEnum b12 picTile (pio <> ".DATAMUX_OREG") datamuxOreg else b12
                    datamuxMddr = strParam ci "DATAMUX_MDDR" "PADDO"
                    b14 = if datamuxMddr /= "PADDO" then addTileEnum b13 picTile (pio <> ".DATAMUX_MDDR") datamuxMddr else b13
                    trimuxTsreg = strParam ci "TRIMUX_TSREG" "PADDT"
                    b15 = if trimuxTsreg /= "PADDT" then addTileEnum b14 picTile (pio <> ".TRIMUX_TSREG") trimuxTsreg else b14
                 in b15
      where
        portUnconnected p = case M.lookup (cid p) (cellPorts ci) of
            Nothing -> True
            Just pi -> portNet pi == Nothing

    -- ------------------------------------------------------------------
    -- write_dcc
    -- ------------------------------------------------------------------
    writeDcc b ci =
        case cellBel ci of
            Nothing -> b
            Just bel ->
                if cellPortNet ci "CE" == Nothing
                    then b
                    else
                        let belname = belNameOf bel
                            Loc bx by _ = getBelLocation e bel
                            tg0 = TileGroup [] emptyTileConfig
                            (tg1, ok) =
                                case T.head belname of
                                    'B' ->
                                        ( tg0{tgTiles = [ttls e by bx ["BMID_0H", "BMID_0V"], ttls e by (bx + 1) ["BMID_2", "BMID_2V"]]}
                                        , True
                                        )
                                    'T' -> (tg0{tgTiles = [ttl e by bx "TMID_0", ttl e by (bx + 1) "TMID_1"]}, True)
                                    'L' -> (tg0{tgTiles = [ttl e by bx "LMID_0"]}, True)
                                    'R' -> (tg0{tgTiles = [ttl e by bx "RMID_0"]}, True)
                                    _ -> (tg0, False)
                         in if not ok
                                then b
                                else
                                    let tg = tg1{tgConfig = addEnum ("DCC_" <> T.singleton (T.head belname) <> T.drop 4 belname <> ".MODE") "DCCA" (tgConfig tg1)}
                                     in addTileGroup b tg

    -- ------------------------------------------------------------------
    -- write_bram
    -- ------------------------------------------------------------------
    getBramTiles bel =
        let Loc bx by z = getBelLocation e bel
            ebr0 = ["MIB_EBR0", "EBR_CMUX_UR", "EBR_CMUX_LR", "EBR_CMUX_LR_25K"]
            ebr8 = [ "MIB_EBR8", "EBR_SPINE_UL1", "EBR_SPINE_UR1", "EBR_SPINE_LL1", "EBR_CMUX_UL", "EBR_SPINE_LL0"
                   , "EBR_CMUX_LL", "EBR_SPINE_LR0", "EBR_SPINE_LR1", "EBR_CMUX_LL_25K", "EBR_SPINE_UL2", "EBR_SPINE_UL0"
                   , "EBR_SPINE_UR2", "EBR_SPINE_LL2", "EBR_SPINE_LR2", "EBR_SPINE_UR0"
                   ]
         in case z of
                0 -> [ttls e by bx ebr0, ttl e by (bx + 1) "MIB_EBR1"]
                1 -> [ttl e by bx "MIB_EBR2", ttl e by (bx + 1) "MIB_EBR3", ttl e by (bx + 2) "MIB_EBR4"]
                2 -> [ttl e by bx "MIB_EBR4", ttl e by (bx + 1) "MIB_EBR5", ttl e by (bx + 2) "MIB_EBR6"]
                3 -> [ttl e by bx "MIB_EBR6", ttl e by (bx + 1) "MIB_EBR7", ttls e by (bx + 2) ebr8]
                _ -> []

    writeBram b ci =
        case cellBel ci of
            Nothing -> b
            Just bel ->
                let Loc bx by z = getBelLocation e bel
                    ebr = "EBR" <> T.pack (show z)
                    isPdp = riIsPdp (lookupRam (cellName ci) ai)
                    tg0 = TileGroup (getBramTiles bel) emptyTileConfig
                    tg1 =
                        if isPdp
                            then tg0{tgConfig = addEnum (ebr <> ".PDPW16KD.DATA_WIDTH_R") (intstrParam ci "DATA_WIDTH_B" "36") (addEnum (ebr <> ".MODE") "PDPW16KD" (tgConfig tg0))}
                            else tg0{tgConfig = addEnum (ebr <> ".DP16KD.WRITEMODE_B") (strParam ci "WRITEMODE_B" "NORMAL") (addEnum (ebr <> ".DP16KD.WRITEMODE_A") (strParam ci "WRITEMODE_A" "NORMAL") (addEnum (ebr <> ".DP16KD.DATA_WIDTH_B") (intstrParam ci "DATA_WIDTH_B" "18") (addEnum (ebr <> ".DP16KD.DATA_WIDTH_A") (intstrParam ci "DATA_WIDTH_A" "18") (addEnum (ebr <> ".MODE") "DP16KD" (tgConfig tg0)))))}
                    csdA0 = strToBits (strParam ci "CSDECODE_A" "0b000") 3
                    csdB0 = strToBits (strParam ci "CSDECODE_B" "0b000") 3
                    tg2 = tg1{tgConfig = addEnum (ebr <> ".GSR") (strParam ci "GSR" "DISABLED") (addEnum (ebr <> ".ASYNC_RESET_RELEASE") (strParam ci "ASYNC_RESET_RELEASE" "SYNC") (addEnum (ebr <> ".RESETMODE") (strParam ci "RESETMODE" "SYNC") (addEnum (ebr <> ".REGMODE_B") (strParam ci "REGMODE_B" "NOREG") (addEnum (ebr <> ".REGMODE_A") (strParam ci "REGMODE_A" "NOREG") (tgConfig tg1)))))}
                    wid = attrInt ci "WID" 0
                    tg3 = tg2{tgConfig = addWord (ebr <> ".WID") (intToBits (bitReverse wid 9) 9) (tgConfig tg2)}
                    -- tie signals (mutating the cell params in the design,
                    -- like the C++ writes into ci->params)
                    (tg4, b') = foldl tiePort (tg3, b) (portOrderRefs ci)
                    tiePort (tgAcc, bAcc) (p, pi) =
                        let pname = idToText (ecp5IdTable e) p
                            portNet' = portNet pi
                            ciSnap = fromMaybe ci (lookupCell (cellName ci) (bgDesign bAcc))
                            setMux v =
                                if not (M.member (cid (pname <> "MUX")) (cellParams ciSnap))
                                    then bAcc{bgDesign = setCellParam (cellName ci) (cid (pname <> "MUX")) (PropStr v) (bgDesign bAcc)}
                                    else bAcc
                         in if isPdp && (pname `elem` ["WEA", "WEB", "ADA4"])
                                then (tgAcc, bAcc)
                                else
                                    if portNet' == Nothing && portType pi == PortIn
                                        then
                                            if pname `elem` ["CLKA", "CLKB", "WEA", "WEB", "RSTA", "RSTB"]
                                                then (tgAcc, tieCibSignal (belPinWireOf bel p) True (setMux "INV"))
                                                else
                                                    if pname `elem` ["CEA", "CEB", "OCEA", "OCEB"]
                                                        then (tgAcc, tieCibSignal (belPinWireOf bel p) True (setMux pname))
                                                        else
                                                            if pname `elem` ["CSA0", "CSA1", "CSA2", "CSB0", "CSB1", "CSB2"]
                                                                then (tgAcc, tieCibSignal (belPinWireOf bel p) True (setMux "INV"))
                                                                else
                                                                    let value = boolParam ciSnap (pname <> "MUX") False
                                                                     in (tgAcc, tieCibSignal (belPinWireOf bel p) value bAcc)
                                        else (tgAcc, bAcc)
                    -- re-read the (possibly mutated) cell
                    ci' = fromMaybe ci (lookupCell (cellName ci) (bgDesign b'))
                    -- invert CSDECODE for INV muxes
                    csdA = [if strParam ci' ("CSA" <> showBit <> "MUX") ("CSA" <> showBit) == "INV" then not b2 else b2 | (showBit, b2) <- zip ["0", "1", "2"] csdA0]
                    csdB = [if strParam ci' ("CSB" <> showBit <> "MUX") ("CSB" <> showBit) == "INV" then not b2 else b2 | (showBit, b2) <- zip ["0", "1", "2"] csdB0]
                    tg5 = tg4{tgConfig = addEnum (ebr <> ".RSTBMUX") (strParam ci "RSTBMUX" "RSTB") (addEnum (ebr <> ".RSTAMUX") (strParam ci "RSTAMUX" "RSTA") (addEnum (ebr <> ".CLKBMUX") (strParam ci "CLKBMUX" "CLKB") (addEnum (ebr <> ".CLKAMUX") (strParam ci "CLKAMUX" "CLKA") (tgConfig tg4))))}
                    tg6 =
                        if not isPdp
                            then tg5{tgConfig = addEnum (ebr <> ".WEBMUX") (strParam ci "WEBMUX" "WEB") (addEnum (ebr <> ".WEAMUX") (strParam ci "WEAMUX" "WEA") (tgConfig tg5))}
                            else tg5
                    tg7 = tg6{tgConfig = addEnum (ebr <> ".OCEBMUX") (strParam ci "OCEBMUX" "OCEB") (addEnum (ebr <> ".OCEAMUX") (strParam ci "OCEAMUX" "OCEA") (addEnum (ebr <> ".CEBMUX") (strParam ci "CEBMUX" "CEB") (addEnum (ebr <> ".CEAMUX") (strParam ci "CEAMUX" "CEA") (tgConfig tg6))))}
                    tg8 = tg7{tgConfig = addWord (ebr <> ".CSDECODE_B") (reverse csdB) (addWord (ebr <> ".CSDECODE_A") (reverse csdA) (tgConfig tg7))}
                    initData = V.fromList [0 :: Int | _ <- [0 .. 2047]]
                    initData' = foldl addInit initData [0 .. 0x3F]
                    addInit acc i =
                        let paramName = printf' i
                            value = parseInitStr ci paramName 320
                            idxOf j k = i * 32 + j * 2 + (k `div` 9)
                         in V.accum (.|.) acc [ (idxOf j k, 1 `shiftL` (k `mod` 9))
                                              | j <- [0 .. 15]
                                              , k <- [0 .. 17]
                                              , value !! (20 * j + k)
                                              ]
                    b1 = addTileGroup b' tg8
                    b2 = b1{bgCfg = (bgCfg b1){ccBramData = M.insert wid (V.toList initData') (ccBramData (bgCfg b1))}}
                 in b2
      where

    -- ------------------------------------------------------------------
    -- write_mult18 / write_alu54
    -- ------------------------------------------------------------------
    getDspTiles bel =
        let Loc bx by z = getBelLocation e bel
            dsp8 = ["MIB_DSP8", "DSP_SPINE_UL0", "DSP_SPINE_UR0", "DSP_SPINE_UR1"]
            mibs = [ ("MIB_DSP0", 0), ("MIB2_DSP0", 0), ("MIB_DSP1", 1), ("MIB2_DSP1", 1), ("MIB_DSP2", 2), ("MIB2_DSP2", 2), ("MIB_DSP3", 3), ("MIB2_DSP3", 3), ("MIB_DSP4", 4), ("MIB2_DSP4", 4) ]
            t0 = [ttl e by (bx + o) m | (m, o) <- mibs]
         in if belTypeOf bel == "MULT18X18D"
                then case z of
                    0 -> t0
                    1 -> [ttl e by (bx - 1 + o) m | (m, o) <- mibs]
                    4 -> [ttl e by (bx + o) m | (m, o) <- [("MIB_DSP4", 0), ("MIB2_DSP4", 0), ("MIB_DSP5", 1), ("MIB2_DSP5", 1), ("MIB_DSP6", 2), ("MIB2_DSP6", 2), ("MIB_DSP7", 3), ("MIB2_DSP7", 3)]] ++ [ttls e by (bx + 4) dsp8, ttl e by (bx + 4) "MIB2_DSP8"]
                    5 -> [ttl e by (bx - 1 + o) m | (m, o) <- [("MIB_DSP4", 0), ("MIB2_DSP4", 0), ("MIB_DSP5", 1), ("MIB2_DSP5", 1), ("MIB_DSP6", 2), ("MIB2_DSP6", 2), ("MIB_DSP7", 3), ("MIB2_DSP7", 3)]] ++ [ttls e by (bx + 3) dsp8, ttl e by (bx + 3) "MIB2_DSP8"]
                    _ -> []
                else
                    if belTypeOf bel == "ALU54B"
                        then case z of
                            3 -> [ttl e by (bx - 3 + o) m | (m, o) <- mibs]
                            7 -> [ttl e by (bx - 3 + o) m | (m, o) <- [("MIB_DSP4", 0), ("MIB2_DSP4", 0), ("MIB_DSP5", 1), ("MIB2_DSP5", 1), ("MIB_DSP6", 2), ("MIB2_DSP6", 2), ("MIB_DSP7", 3), ("MIB2_DSP7", 3)]] ++ [ttls e by (bx + 1) dsp8, ttl e by (bx + 1) "MIB2_DSP8"]
                            _ -> []
                        else []
      where
        belTypeOf bel = idToText (ecp5IdTable e) (getBelType e bel)

    tieoffDspPorts :: CellInfo BelId WireId PipId -> Bitgen -> Bitgen
    tieoffDspPorts ci b =
        foldl tiePort b (portOrderRefs ci)
      where
        skip p = any (`T.isPrefixOf` p) ["CLK", "CE", "RST", "SRO", "SRI", "RO", "MA", "MB", "CFB", "CIN", "SOURCE", "SIGNED", "OP"]
        tiePort bAcc (p, pi) =
            let pname = idToText (ecp5IdTable e) p
             in if portNet pi == Nothing && portType pi == PortIn && not (skip pname)
                    then
                        let value = boolParam ci (pname <> "MUX") False
                         in tieCibSignal (belPinWireOf (fromMaybe (error "unplaced dsp") (cellBel ci)) p) value bAcc
                    else bAcc

    writeMult18 b ci =
        case cellBel ci of
            Nothing -> b
            Just bel ->
                let Loc bx by z = getBelLocation e bel
                    dsp = "MULT18_" <> T.pack (show z)
                    tg0 = TileGroup (getDspTiles bel) emptyTileConfig
                    c = tgConfig tg0
                    c1 = addEnum (dsp <> ".REG_INPUTA_CLK") (strParam ci "REG_INPUTA_CLK" "NONE") c
                    c2 = addEnum (dsp <> ".REG_INPUTA_CE") (strParam ci "REG_INPUTA_CE" "CE0") c1
                    c3 = addEnum (dsp <> ".REG_INPUTA_RST") (strParam ci "REG_INPUTA_RST" "RST0") c2
                    c4 = addEnum (dsp <> ".REG_INPUTB_CLK") (strParam ci "REG_INPUTB_CLK" "NONE") c3
                    c5 = addEnum (dsp <> ".REG_INPUTB_CE") (strParam ci "REG_INPUTB_CE" "CE0") c4
                    c6 = addEnum (dsp <> ".REG_INPUTB_RST") (strParam ci "REG_INPUTB_RST" "RST0") c5
                    c7 = addEnum (dsp <> ".REG_INPUTC_CLK") (strParam ci "REG_INPUTC_CLK" "NONE") c6
                    c8 = addEnum (dsp <> ".REG_PIPELINE_CLK") (strParam ci "REG_PIPELINE_CLK" "NONE") c7
                    c9 = addEnum (dsp <> ".REG_PIPELINE_CE") (strParam ci "REG_PIPELINE_CE" "CE0") c8
                    c10 = addEnum (dsp <> ".REG_PIPELINE_RST") (strParam ci "REG_PIPELINE_RST" "RST0") c9
                    c11 = addEnum (dsp <> ".REG_OUTPUT_CLK") (strParam ci "REG_OUTPUT_CLK" "NONE") c10
                    c12 =
                        if dsp == "MULT18_0" || dsp == "MULT18_4"
                            then addEnum (dsp <> ".REG_OUTPUT_RST") (strParam ci "REG_OUTPUT_RST" "RST0") c11
                            else c11
                    c13 = addEnum (dsp <> ".CLK0_DIV") (strParam ci "CLK0_DIV" "ENABLED") c12
                    c14 = addEnum (dsp <> ".CLK1_DIV") (strParam ci "CLK1_DIV" "ENABLED") c13
                    c15 = addEnum (dsp <> ".CLK2_DIV") (strParam ci "CLK2_DIV" "ENABLED") c14
                    c16 = addEnum (dsp <> ".CLK3_DIV") (strParam ci "CLK3_DIV" "ENABLED") c15
                    c17 = addEnum (dsp <> ".GSR") (strParam ci "GSR" "ENABLED") c16
                    c18 = addEnum (dsp <> ".SOURCEB_MODE") (strParam ci "SOURCEB_MODE" "B_SHIFT") c17
                    c19 = addEnum (dsp <> ".RESETMODE") (strParam ci "RESETMODE" "SYNC") c18
                    c20 = addEnum (dsp <> ".MODE") "MULT18X18D" c19
                    c21 =
                        if strParam ci "REG_OUTPUT_CLK" "NONE" == "NONE" && cellCluster ci == emptyId
                            then addEnum (dsp <> ".CIBOUT_BYP") "ON" c20
                            else c20
                    c22 =
                        if z < 4
                            then addEnum "DSP_LEFT.CIBOUT" "ON" c21
                            else addEnum "DSP_RIGHT.CIBOUT" "ON" c21
                    c23 = foldl (\acc (port, i) -> addEnum (dsp <> "." <> port <> T.pack (show i) <> "MUX") (port <> T.pack (show i)) acc) c22 [(p, i) | p <- ["CLK", "CE", "RST"], i <- [0 .. 3]]
                    tg = tg0{tgConfig = c23}
                    b1 = addTileGroup b tg
                 in tieoffDspPorts ci b1

    writeAlu54 b ci =
        case cellBel ci of
            Nothing -> b
            Just bel ->
                let Loc bx by z = getBelLocation e bel
                    dsp = "ALU54_" <> T.pack (show z)
                    tg0 = TileGroup (getDspTiles bel) emptyTileConfig
                    c = tgConfig tg0
                    c1 = addEnum (dsp <> ".REG_INPUTC0_CLK") (strParam ci "REG_INPUTC0_CLK" "NONE") c
                    c2 = addEnum (dsp <> ".REG_INPUTC1_CLK") (strParam ci "REG_INPUTC1_CLK" "NONE") c1
                    c3 = addEnum (dsp <> ".REG_OPCODEOP0_0_CLK") (strParam ci "REG_OPCODEOP0_0_CLK" "NONE") c2
                    c4 = addEnum (dsp <> ".REG_OPCODEOP0_0_CE") (strParam ci "REG_OPCODEOP0_0_CE" "CE0") c3
                    c5 = addEnum (dsp <> ".REG_OPCODEOP0_0_RST") (strParam ci "REG_OPCODEOP0_0_RST" "RST0") c4
                    c6 = addEnum (dsp <> ".REG_OPCODEOP1_0_CLK") (strParam ci "REG_OPCODEOP1_0_CLK" "NONE") c5
                    c7 = addEnum (dsp <> ".REG_OPCODEOP0_1_CLK") (strParam ci "REG_OPCODEOP0_1_CLK" "NONE") c6
                    c8 = addEnum (dsp <> ".REG_OPCODEOP1_1_CLK") (strParam ci "REG_OPCODEOP1_1_CLK" "NONE") c7
                    c9 = addEnum (dsp <> ".REG_OPCODEOP0_1_CE") (strParam ci "REG_OPCODEOP0_1_CE" "CE0") c8
                    c10 = addEnum (dsp <> ".REG_OPCODEOP0_1_RST") (strParam ci "REG_OPCODEOP0_1_RST" "RST0") c9
                    c11 = addEnum (dsp <> ".REG_OPCODEIN_0_CLK") (strParam ci "REG_OPCODEIN_0_CLK" "NONE") c10
                    c12 = addEnum (dsp <> ".REG_OPCODEIN_0_CE") (strParam ci "REG_OPCODEIN_0_CE" "CE0") c11
                    c13 = addEnum (dsp <> ".REG_OPCODEIN_0_RST") (strParam ci "REG_OPCODEIN_0_RST" "RST0") c12
                    c14 = addEnum (dsp <> ".REG_OPCODEIN_1_CLK") (strParam ci "REG_OPCODEIN_1_CLK" "NONE") c13
                    c15 = addEnum (dsp <> ".REG_OPCODEIN_1_CE") (strParam ci "REG_OPCODEIN_1_CE" "CE0") c14
                    c16 = addEnum (dsp <> ".REG_OPCODEIN_1_RST") (strParam ci "REG_OPCODEIN_1_RST" "RST0") c15
                    c17 = addEnum (dsp <> ".REG_OUTPUT0_CLK") (strParam ci "REG_OUTPUT0_CLK" "NONE") c16
                    c18 = addEnum (dsp <> ".REG_OUTPUT1_CLK") (strParam ci "REG_OUTPUT1_CLK" "NONE") c17
                    c19 = addEnum (dsp <> ".REG_FLAG_CLK") (strParam ci "REG_FLAG_CLK" "NONE") c18
                    c20 = addEnum (dsp <> ".MCPAT_SOURCE") (strParam ci "MCPAT_SOURCE" "STATIC") c19
                    c21 = addEnum (dsp <> ".MASKPAT_SOURCE") (strParam ci "MASKPAT_SOURCE" "STATIC") c20
                    c22 = addWord (dsp <> ".MASK01") (parseInitStr ci "MASK01" 56) c21
                    c23 = addEnum (dsp <> ".CLK0_DIV") (strParam ci "CLK0_DIV" "ENABLED") c22
                    c24 = addEnum (dsp <> ".CLK1_DIV") (strParam ci "CLK1_DIV" "ENABLED") c23
                    c25 = addEnum (dsp <> ".CLK2_DIV") (strParam ci "CLK2_DIV" "ENABLED") c24
                    c26 = addEnum (dsp <> ".CLK3_DIV") (strParam ci "CLK3_DIV" "ENABLED") c25
                    c27 = addWord (dsp <> ".MCPAT") (parseInitStr ci "MCPAT" 56) c26
                    c28 = addWord (dsp <> ".MASKPAT") (parseInitStr ci "MASKPAT" 56) c27
                    c29 = addWord (dsp <> ".RNDPAT") (parseInitStr ci "RNDPAT" 56) c28
                    c30 = addEnum (dsp <> ".GSR") (strParam ci "GSR" "ENABLED") c29
                    c31 = addEnum (dsp <> ".RESETMODE") (strParam ci "RESETMODE" "SYNC") c30
                    c32 = addEnum (dsp <> ".FORCE_ZERO_BARREL_SHIFT") (strParam ci "FORCE_ZERO_BARREL_SHIFT" "DISABLED") c31
                    c33 = addEnum (dsp <> ".LEGACY") (strParam ci "LEGACY" "DISABLED") c32
                    c34 = addEnum (dsp <> ".MODE") "ALU54B" c33
                    c35 =
                        if z < 4
                            then addEnum "DSP_LEFT.CIBOUT" "ON" c34
                            else addEnum "DSP_RIGHT.CIBOUT" "ON" c34
                    c36 =
                        if strParam ci "REG_FLAG_CLK" "NONE" == "NONE" && (dsp == "ALU54_7" || dsp == "ALU54_3")
                            then addEnum "MULT18_5.CIBOUT_BYP" "ON" c35
                            else c35
                    c37 =
                        if strParam ci "REG_OUTPUT0_CLK" "NONE" == "NONE" && (dsp == "ALU54_7" || dsp == "ALU54_3")
                            then addEnum (if dsp == "ALU54_7" then "MULT18_4.CIBOUT_BYP" else "MULT18_0.CIBOUT_BYP") "ON" c36
                            else c36
                    tg = tg0{tgConfig = c37}
                    b1 = addTileGroup b tg
                 in tieoffDspPorts ci b1

    -- ------------------------------------------------------------------
    -- write_pll
    -- ------------------------------------------------------------------
    getPllTiles bel =
        let name = belNameOf bel
            Loc bx by _ = getBelLocation e bel
            pll1Lr = ["PLL1_LR", "BANKREF4"]
         in if name == "EHXPLL_UL"
                then [ttl e by (bx - 1) "PLL0_UL", ttl e (by + 1) (bx - 1) "PLL1_UL"]
                else
                    if name == "EHXPLL_LL"
                        then [ttl e (by + 1) bx "PLL0_LL", ttl e (by + 1) (bx + 1) "BANKREF8"]
                        else
                            if name == "EHXPLL_LR"
                                then [ttl e (by + 1) bx "PLL0_LR", ttls e (by + 1) (bx - 1) pll1Lr]
                                else
                                    if name == "EHXPLL_UR"
                                        then [ttl e by (bx + 1) "PLL0_UR", ttl e (by + 1) (bx + 1) "PLL1_UR"]
                                        else []

    writePll b ci =
        case cellBel ci of
            Nothing -> b
            Just bel ->
                let tg0 = TileGroup (getPllTiles bel) emptyTileConfig
                    c = tgConfig tg0
                    c0 = addEnum "MODE" "EHXPLLL" c
                    c1 = addWord "CLKI_DIV" (intToBits (intParam ci "CLKI_DIV" 1 - 1) 7) c0
                    c2 = addWord "CLKFB_DIV" (intToBits (intParam ci "CLKFB_DIV" 1 - 1) 7) c1
                    c3 = addEnum "CLKOP_ENABLE" (strParam ci "CLKOP_ENABLE" "ENABLED") c2
                    c4 = addEnum "CLKOS_ENABLE" (strParam ci "CLKOS_ENABLE" "ENABLED") c3
                    c5 = addEnum "CLKOS2_ENABLE" (strParam ci "CLKOS2_ENABLE" "ENABLED") c4
                    c6 = addEnum "CLKOS3_ENABLE" (strParam ci "CLKOS3_ENABLE" "ENABLED") c5
                    c7 = foldl (\acc out ->
                            let cd = addWord (out <> "_DIV") (intToBits (intParam ci (out <> "_DIV") 8 - 1) 7) acc
                                cp = addWord (out <> "_CPHASE") (intToBits (intParam ci (out <> "_CPHASE") 0) 7) cd
                             in addWord (out <> "_FPHASE") (intToBits (intParam ci (out <> "_FPHASE") 0) 3) cp
                        ) c6 ["CLKOP", "CLKOS", "CLKOS2", "CLKOS3"]
                    c8 = addEnum "FEEDBK_PATH" (strParam ci "FEEDBK_PATH" "CLKOP") c7
                    c9 = addEnum "CLKOP_TRIM_POL" (strParam ci "CLKOP_TRIM_POL" "RISING") c8
                    c10 = addEnum "CLKOP_TRIM_DELAY" (intstrParam ci "CLKOP_TRIM_DELAY" "0") c9
                    c11 = addEnum "CLKOS_TRIM_POL" (strParam ci "CLKOS_TRIM_POL" "RISING") c10
                    c12 = addEnum "CLKOS_TRIM_DELAY" (intstrParam ci "CLKOS_TRIM_DELAY" "0") c11
                    hasClkop = cellPortNet ci "CLKOP" /= Nothing
                    c13 = addEnum "OUTDIVIDER_MUXA" (strParam ci "OUTDIVIDER_MUXA" (if hasClkop then "DIVA" else "REFCLK")) c12
                    c14 = addEnum "OUTDIVIDER_MUXB" (strParam ci "OUTDIVIDER_MUXB" (if hasClkop then "DIVB" else "REFCLK")) c13
                    c15 = addEnum "OUTDIVIDER_MUXC" (strParam ci "OUTDIVIDER_MUXC" (if hasClkop then "DIVC" else "REFCLK")) c14
                    c16 = addEnum "OUTDIVIDER_MUXD" (strParam ci "OUTDIVIDER_MUXD" (if hasClkop then "DIVD" else "REFCLK")) c15
                    c17 = addWord "PLL_LOCK_MODE" (intToBits (intParam ci "PLL_LOCK_MODE" 0) 3) c16
                    c18 = addEnum "STDBY_ENABLE" (strParam ci "STDBY_ENABLE" "DISABLED") c17
                    c19 = addEnum "REFIN_RESET" (strParam ci "REFIN_RESET" "DISABLED") c18
                    c20 = addEnum "SYNC_ENABLE" (strParam ci "SYNC_ENABLE" "DISABLED") c19
                    c21 = addEnum "INT_LOCK_STICKY" (strParam ci "INT_LOCK_STICKY" "ENABLED") c20
                    c22 = addEnum "DPHASE_SOURCE" (strParam ci "DPHASE_SOURCE" "DISABLED") c21
                    c23 = addEnum "PLLRST_ENA" (strParam ci "PLLRST_ENA" "DISABLED") c22
                    c24 = addEnum "INTFB_WAKE" (strParam ci "INTFB_WAKE" "DISABLED") c23
                    c25 = addWord "KVCO" (intToBits (attrInt ci "KVCO" 0) 3) c24
                    c26 = addWord "LPF_CAPACITOR" (intToBits (attrInt ci "LPF_CAPACITOR" 0) 2) c25
                    c27 = addWord "LPF_RESISTOR" (intToBits (attrInt ci "LPF_RESISTOR" 0) 7) c26
                    c28 = addWord "ICP_CURRENT" (intToBits (attrInt ci "ICP_CURRENT" 0) 5) c27
                    c29 = addWord "FREQ_LOCK_ACCURACY" (intToBits (attrInt ci "FREQ_LOCK_ACCURACY" 0) 2) c28
                    c30 = addWord "MFG_GMC_GAIN" (intToBits (attrInt ci "MFG_GMC_GAIN" 0) 3) c29
                    c31 = addWord "MFG_GMC_TEST" (intToBits (attrInt ci "MFG_GMC_TEST" 14) 4) c30
                    c32 = addWord "MFG1_TEST" (intToBits (attrInt ci "MFG1_TEST" 0) 3) c31
                    c33 = addWord "MFG2_TEST" (intToBits (attrInt ci "MFG2_TEST" 0) 3) c32
                    c34 = addWord "MFG_FORCE_VFILTER" (intToBits (attrInt ci "MFG_FORCE_VFILTER" 0) 1) c33
                    c35 = addWord "MFG_ICP_TEST" (intToBits (attrInt ci "MFG_ICP_TEST" 0) 1) c34
                    c36 = addWord "MFG_EN_UP" (intToBits (attrInt ci "MFG_EN_UP" 0) 1) c35
                    c37 = addWord "MFG_FLOAT_ICP" (intToBits (attrInt ci "MFG_FLOAT_ICP" 0) 1) c36
                    c38 = addWord "MFG_GMC_PRESET" (intToBits (attrInt ci "MFG_GMC_PRESET" 0) 1) c37
                    c39 = addWord "MFG_LF_PRESET" (intToBits (attrInt ci "MFG_LF_PRESET" 0) 1) c38
                    c40 = addWord "MFG_GMC_RESET" (intToBits (attrInt ci "MFG_GMC_RESET" 0) 1) c39
                    c41 = addWord "MFG_LF_RESET" (intToBits (attrInt ci "MFG_LF_RESET" 0) 1) c40
                    c42 = addWord "MFG_LF_RESGRND" (intToBits (attrInt ci "MFG_LF_RESGRND" 0) 1) c41
                    c43 = addWord "MFG_GMCREF_SEL" (intToBits (attrInt ci "MFG_GMCREF_SEL" 0) 2) c42
                    c44 = addWord "MFG_ENABLE_FILTEROPAMP" (intToBits (attrInt ci "MFG_ENABLE_FILTEROPAMP" 0) 1) c43
                 in addTileGroup b tg0{tgConfig = c44}

    -- ------------------------------------------------------------------
    -- write_iol (IOLOGIC / SIOLOGIC)
    -- ------------------------------------------------------------------
    writeIol b ci =
        case cellBel ci of
            Nothing -> b
            Just bel ->
                let Loc bx by z = getBelLocation e bel
                    pioZ = z - (if cellType ci == cid "SIOLOGIC" then 2 else 4)
                    pioBel = getBelByLocation e (Loc bx by pioZ)
                    picTile = case pioBel of
                        Just pb -> getPicTile pb
                        Nothing -> ""
                    prim = "IOLOGIC" <> T.singleton ("ABCD" !! pioZ)
                    (b1, c) = foldl addParam (b, emptyTileConfig) (M.toList (cellParams ci))
                    addParam (bAcc, tcAcc) (p, pv) =
                        let pname = idToText (ecp5IdTable e) p
                            pstr = if propIsString pv then propAsString pv else T.pack (show (propAsInt64 pv))
                         in if pname == "DELAY.DEL_VALUE"
                                then (bAcc, addWord (prim <> "." <> pname) (intToBits (propAsInt64 pv) 7) tcAcc)
                                else (bAcc, addEnum (prim <> "." <> pname) pstr tcAcc)
                    tc' = case cellPortNet ci "LOADN" of
                        Just _ -> addEnum (prim <> ".LOADNMUX") "LOADN" c
                        Nothing -> c
                    b2 =
                        if picTile == ""
                            then b1
                            else b1{bgCfg = (bgCfg b1){ccTiles = M.insert picTile tc' (ccTiles (bgCfg b1))}}
                 in b2

    -- ------------------------------------------------------------------
    -- write_dcu / write_extrefb / write_dqsbuf
    -- ------------------------------------------------------------------
    getDcuTiles bel =
        let Loc bx by _ = getBelLocation e bel
         in [ttl e by (bx + i) ("DCU" <> T.pack (show i)) | i <- [0 .. 8]]

    writeDcu b ci =
        case cellBel ci of
            Nothing -> b
            Just bel ->
                let tg0 = TileGroup (getDcuTiles bel) emptyTileConfig
                    c = tgConfig tg0
                    c' = foldl (\acc (name, width) -> addWord name (parseConfigStr (fromMaybe (PropNum "" 0) (M.lookup (cid (T.drop 4 name)) (cellParams ci))) width) acc) c dcuWords
                    b1 = addTileGroup b tg0{tgConfig = c'}
                    b2 = foldl tiePort b1 (portOrderRefs ci)
                    tiePort bAcc (p, pi) =
                        let pname = idToText (ecp5IdTable e) p
                         in if portNet pi == Nothing && portType pi == PortIn
                                && not ("CLK" `T.isInfixOf` pname)
                                && not ("HDIN" `T.isInfixOf` pname)
                                && not ("HDOUT" `T.isInfixOf` pname)
                                then
                                    let value = boolParam ci (pname <> "MUX") False
                                     in tieCibSignal (belPinWireOf bel p) value bAcc
                                else bAcc
                 in b2

    writeExtrefb b ci =
        case cellBel ci of
            Nothing -> b
            Just bel ->
                let tg = TileGroup (getDcuTiles bel) (foldl addW emptyTileConfig ["REFCK_DCBIAS_EN", "REFCK_RTERM", "REFCK_PWDNB"])
                    addW tc name = addWord ("EXTREF." <> name) (parseConfigStr (fromMaybe (PropNum "" 0) (M.lookup (cid name) (cellParams ci))) 1) tc
                 in addTileGroup b tg

    writePcsclkdiv b ci =
        case cellBel ci of
            Nothing -> b
            Just bel ->
                let Loc bx by z = getBelLocation e bel
                    tname = ttl e (by + 1) bx "BMID_0H"
                 in addTileEnum b tname ("PCSCLKDIV" <> T.pack (show z)) (strParam ci "GSR" "ENABLED")

    writeDtr b _ =
        case getTileByType e "DTR" of
            Just tname -> addTileEnum b tname "DTR.MODE" "DTR"
            Nothing -> b

    writeOscg b ci =
        let div = intParam ci "DIV" 128
            div' = if div == 128 then 127 else div
            divs = T.pack (show div')
            b1 = case getTileByType e "EFB0_PICB0" of
                Just tname -> addTileEnum b tname "OSC.DIV" divs
                Nothing -> b
            b2 = case getTileByType e "EFB1_PICB1" of
                Just tname -> addTileEnum (addTileEnum b1 tname "OSC.DIV" divs) tname "OSC.MODE" "OSCG"
                Nothing -> b1
         in case getTileByType e "EFB1_PICB1" of
                Just tname -> addTileEnum b2 tname "CCLK.MODE" "_NONE_"
                Nothing -> b2

    writeUsrmclk b _ =
        case getTileByType e "EFB3_PICB1" of
            Just tname -> addTileEnum b tname "CCLK.MODE" "USRMCLK"
            Nothing -> b

    writeGsr b ci =
        let b1 = case getTileByType e "EFB0_PICB0" of
                Just tname -> addTileEnum b tname "GSR.GSRMODE" (strParam ci "MODE" "ACTIVE_LOW")
                Nothing -> b
         in case getTileByType e "VIQ_BUF" of
                Just tname -> addTileEnum b1 tname "GSR.SYNCMODE" (strParam ci "SYNCMODE" "ASYNC")
                Nothing -> b1

    writeJtagg b ci =
        case getTileByType e "EFB0_PICB0" of
            Just tname -> addTileEnum (addTileEnum b tname "JTAG.ER1" (strParam ci "ER1" "ENABLED")) tname "JTAG.ER2" (strParam ci "ER2" "ENABLED")
            Nothing -> b

    writeClkdivf b ci =
        case cellBel ci of
            Nothing -> b
            Just bel ->
                let Loc bx _ z = getBelLocation e bel
                    r = bx > 5
                    clkdiv = "CLKDIV_" <> (if r then "R" else "L") <> T.pack (show z)
                 in case getTileByType e (if r then "ECLK_R" else "ECLK_L") of
                        Just tname -> addTileEnum (addTileEnum b tname (clkdiv <> ".DIV") (strParam ci "DIV" "2.0")) tname (clkdiv <> ".GSR") (strParam ci "GSR" "DISABLED")
                        Nothing -> b

    writeDcsc b ci =
        case cellBel ci of
            Nothing -> b
            Just bel ->
                let Loc bx by _ = getBelLocation e bel
                    dcsTiles = ["EBR_CMUX_LL", "EBR_CMUX_UL", "EBR_CMUX_LL_25K", "DSP_CMUX_UL"]
                    tile = ttls e by bx dcsTiles
                    dcs = belNameOf bel
                 in addTileEnum b tile (dcs <> ".DCSMODE") (attrStr ci "DCSMODE" "POS")

    writeEclksyncb b ci =
        case cellBel ci of
            Nothing -> b
            Just bel ->
                let Loc bx _ _ = getBelLocation e bel
                    r = bx > 5
                    eclksync = belNameOf bel
                 in if cellPortNet ci "STOP" /= Nothing
                        then case getTileByType e (if r then "ECLK_R" else "ECLK_L") of
                            Just tname -> addTileEnum b tname (eclksync <> ".MODE") "ECLKSYNCB"
                            Nothing -> b
                        else b

    writeEclkbridgecs b ci =
        case cellBel ci of
            Nothing -> b
            Just bel ->
                let Loc bx _ _ = getBelLocation e bel
                    r = bx > 5
                    eclkb = belNameOf bel
                 in if cellPortNet ci "STOP" /= Nothing
                        then case getTileByType e (if r then "ECLK_R" else "ECLK_L") of
                            Just tname -> addTileEnum b tname (eclkb <> ".MODE") "ECLKBRIDGECS"
                            Nothing -> b
                        else b

    writeDdrdll b ci =
        case cellBel ci of
            Nothing -> b
            Just bel ->
                let Loc bx by _ = getBelLocation e bel
                    u = by < 15
                    r = bx > 15
                    tiletype = "DDRDLL_" <> (if u then "U" else "L") <> (if r then "R" else "L")
                    tiletype' =
                        if (eaDevice args `elem` [Lfe5u12f, Lfe5u25f, Lfe5um25f, Lfe5um5g25f]) && u
                            then tiletype <> "A"
                            else tiletype
                 in case getTileByType e tiletype' of
                        Just tname ->
                            addTileEnum
                                (addTileEnum (addTileEnum b tname "DDRDLL.MODE" "DDRDLLA") tname "DDRDLL.GSR" (strParam ci "GSR" "DISABLED"))
                                tname
                                "DDRDLL.FORCE_MAX_DELAY"
                                (strParam ci "FORCE_MAX_DELAY" "NO")
                        Nothing -> b

    writeDqsbuf b ci =
        case cellBel ci of
            Nothing -> b
            Just bel ->
                let Loc bx by _ = getBelLocation e bel
                    liDel0 = intParam ci "DQS_LI_DEL_VAL" 0 :: Int
                    l = bx < 10
                    pic = if l then "PICL" else "PICR"
                    tg = TileGroup
                            [ ttl e (by - 2) bx (pic <> "1_DQS0")
                            , ttl e (by - 1) bx (pic <> "2_DQS1")
                            , ttl e by bx (pic <> "0_DQS2")
                            , ttl e (by + 1) bx (pic <> "1_DQS3")
                            ]
                            emptyTileConfig
                    liDel = liDel0
                    liDel' = if strParam ci "DQS_LI_DEL_ADJ" "PLUS" == "MINUS" then (256 - liDel) .&. 0xFF else liDel
                    loDel = intParam ci "DQS_LO_DEL_VAL" 0 :: Int
                    loDel' = if strParam ci "DQS_LO_DEL_ADJ" "PLUS" == "MINUS" then (256 - loDel) .&. 0xFF else loDel
                    c = tgConfig tg
                    c1 = addEnum "DQS.MODE" "DQSBUFM" c
                    c2 = addEnum "DQS.DQS_LI_DEL_ADJ" (strParam ci "DQS_LI_DEL_ADJ" "PLUS") c1
                    c3 = addEnum "DQS.DQS_LO_DEL_ADJ" (strParam ci "DQS_LO_DEL_ADJ" "PLUS") c2
                    c4 = addWord "DQS.DQS_LI_DEL_VAL" (intToBits liDel' 8) c3
                    c5 = addWord "DQS.DQS_LO_DEL_VAL" (intToBits loDel' 8) c4
                    c6 = addEnum "DQS.WRLOADN_USED" (if cellPortNet ci "WRLOADN" /= Nothing then "YES" else "NO") c5
                    c7 = addEnum "DQS.RDLOADN_USED" (if cellPortNet ci "RDLOADN" /= Nothing then "YES" else "NO") c6
                    c8 = addEnum "DQS.PAUSE_USED" (if cellPortNet ci "PAUSE" /= Nothing then "YES" else "NO") c7
                    c9 = addEnum "DQS.READ_USED" (if cellPortNet ci "READ0" /= Nothing || cellPortNet ci "READ1" /= Nothing then "YES" else "NO") c8
                    c10 = addEnum "DQS.DDRDEL" (if cellPortNet ci "DDRDEL" /= Nothing then "DDRDEL" else "0") c9
                    c11 = addEnum "DQS.GSR" (strParam ci "GSR" "DISABLED") c10
                 in addTileGroup b tg{tgConfig = c11}

    -- ------------------------------------------------------------------
    -- init_io_banks
    -- ------------------------------------------------------------------
    initIoBanks b =
        let b1 = foldl scanCell b (cellsIter design)
            scanCell bAcc ci =
                case (cellType ci == cid "TRELLIS_IO", cellBel ci) of
                    (True, Just bel) ->
                        let bank = getPioBelBank e bel
                            dir = strParam ci "DIR" "INPUT"
                            iotype = attrStr ci "IO_TYPE" "LVCMOS33"
                            ioType = ioTypeFromStr iotype
                            b1' =
                                if dir /= "INPUT" || isReferenced ioType
                                    then
                                        let vcc = getVccio ioType
                                         in case M.lookup bank (bgBankVcc bAcc) of
                                                Just vcc'
                                                    | vcc' /= vcc -> error ("incompatible IO voltages on bank " ++ show bank)
                                                _ -> bAcc{bgBankVcc = M.insert bank vcc (bgBankVcc bAcc)}
                                    else bAcc
                            b2' =
                                if iotype == "LVDS"
                                    then b1'{bgBankLvds = S.insert bank (bgBankLvds b1')}
                                    else b1'
                            b3' =
                                if (dir == "INPUT" || dir == "BIDIR") && isDifferential ioType
                                    then b2'{bgBankDiff = S.insert bank (bgBankDiff b2')}
                                    else b2'
                         in if (dir == "INPUT" || dir == "BIDIR") && isReferenced ioType
                                then b3'{bgBankVref = S.insert bank (bgBankVref b3')}
                                else b3'
                    _ -> bAcc
            -- BANKREF tiles
            b2 = foldl bankRefTile b1 [0 .. cdHeight (ecp5Chipdb e) - 1]
            bankRefTile bAcc y =
                foldl (\bAcc' x ->
                    let tiles = getTilesAtLoc e y x
                     in foldl (\bAcc'' (tname, typ) ->
                            if "BANKREF" `T.isInfixOf` typ
                                then
                                    let bank = readInt (T.unpack (T.drop 7 typ))
                                        b1'' =
                                            if bank == 8 && M.member (cid "arch.sysconfig.CONFIG_IOVOLTAGE") (bgSettings b)
                                                then
                                                    let vcc = strSetting (cid "arch.sysconfig.CONFIG_IOVOLTAGE") "3V3"
                                                        vcc' = T.pack (T.unpack vcc)
                                                     in addTileEnum bAcc'' tname "BANK.VCCIO" vcc'
                                                else
                                                    case M.lookup bank (bgBankVcc bAcc'') of
                                                        Just Vcc1V35 -> addTileEnum bAcc'' tname "BANK.VCCIO" "1V2"
                                                        Just vcc -> addTileEnum bAcc'' tname "BANK.VCCIO" (iovoltageToStr vcc)
                                                        Nothing -> bAcc''
                                        b2'' =
                                            if S.member bank (bgBankLvds b1'')
                                                then addTileEnum (addTileEnum b1'' tname "BANK.DIFF_REF" "ON") tname "BANK.LVDSO" "ON"
                                                else b1''
                                        b3'' =
                                            if S.member bank (bgBankDiff b2'')
                                                then addTileEnum b2'' tname "BANK.DIFF_REF" "ON"
                                                else b2''
                                     in if S.member bank (bgBankVref b3'')
                                            then addTileEnum (addTileEnum b3'' tname "BANK.DIFF_REF" "ON") tname "BANK.VREF" "ON"
                                            else b3''
                                else bAcc''
                        ) bAcc' tiles
                    ) bAcc [0 .. cdWidth (ecp5Chipdb e) - 1]
            -- VREF dummy outputs
            b3 = foldl vrefOut b2 (S.toList (bgBankVref b2))
            vrefOut bAcc bank =
                    case getPioByFunctionName e ("VREF1_" <> T.pack (show bank)) of
                        Nothing -> error ("unable to find VREF input for bank " ++ show bank)
                        Just vrefIO ->
                            let iotype = case M.lookup bank (bgBankVcc bAcc) of
                                    Just Vcc1V2 -> "HSUL12"
                                    Just Vcc1V35 -> "SSTL18_I"
                                    Just Vcc1V5 -> "SSTL18_I"
                                    Just Vcc1V8 -> "SSTL18_I"
                                    _ -> error "unsupported Vref vccio"
                                pioTile = getPioTile vrefIO
                                pio = belNameOf vrefIO
                             in addTileEnum (addTileEnum bAcc pioTile (pio <> ".BASE_TYPE") ("OUTPUT_" <> iotype)) pioTile (pio <> ".PULLMODE") "NONE"
         in b3
      where
        strSetting k def =
            case M.lookup k (bgSettings b) of
                Just p -> if propIsString p then propAsString p else T.pack (show (propAsInt64 p))
                Nothing -> T.pack def

    -- ------------------------------------------------------------------
    -- SYSCONFIG settings
    -- ------------------------------------------------------------------
    addSysConfig b (key, p) =
        let keystr = idToText (ecp5IdTable e) key
            value = if propIsString p then propAsString p else T.pack (show (propAsInt64 p))
         in if "arch.sysconfig." `T.isPrefixOf` keystr
                then
                    let key' = T.drop (T.length "arch.sysconfig.") keystr
                     in if key' `elem` ["SLAVE_SPI_PORT", "DONE_EX"]
                            then case (getTileByType e "EFB0_PICB0", getTileByType e "EFB2_PICB0") of
                                (Just t0, Just t2) -> addTileEnum (addTileEnum b t0 ("SYSCONFIG." <> key') value) t2 ("SYSCONFIG." <> key') value
                                _ -> b
                            else
                                if key' `elem` ["SLAVE_PARALLEL_PORT", "BACKGROUND_RECONFIG", "WAKE_UP"]
                                    then case getTileByType e "EFB0_PICB0" of
                                        Just t0 -> addTileEnum b t0 ("SYSCONFIG." <> key') value
                                        Nothing -> b
                                    else
                                        if key' == "MASTER_SPI_PORT"
                                            then case getTileByType e "EFB1_PICB1" of
                                                Just t1 -> addTileEnum b t1 ("SYSCONFIG." <> key') value
                                                Nothing -> b
                                            else
                                                if key' == "TRANSFR"
                                                    then case (getTileByType e "EFB0_PICB0", getTileByType e "EFB1_PICB1") of
                                                        (Just t0, Just t1) -> addTileEnum (addTileEnum b t0 ("SYSCONFIG." <> key') value) t1 ("SYSCONFIG." <> key') value
                                                        _ -> b
                                                    else b{bgCfg = (bgCfg b){ccSysconfig = M.insert key' value (ccSysconfig (bgCfg b))}}
                else b

    -- ------------------------------------------------------------------
    -- fix_tile_names (SERDES V transforms)
    -- ------------------------------------------------------------------
    fixTileNames cc =
        if eaDevice args `elem` [Lfe5u12f, Lfe5u25f, Lfe5u45f, Lfe5u85f]
            then
                let xform = M.fromList
                        [ (old, new)
                        | (old, tc) <- M.toList (ccTiles cc)
                        , let new =
                                if "CIB_DCU" `T.isInfixOf` old
                                    then
                                        let pos = T.length (fst (T.breakOn "CIB_DCU" old))
                                         in if pos > 0 && T.index old (pos - 1) == 'V'
                                                then old
                                                else T.take pos old <> "V" <> T.drop pos old
                                    else
                                        if "BMID_0H" `T.isSuffixOf` old
                                            then T.snoc (T.dropEnd 1 old) 'V'
                                            else
                                                if "BMID_2" `T.isSuffixOf` old
                                                    then old <> "V"
                                                    else old
                        , new /= old
                        ]
                 in cc
                        { ccTiles =
                            M.fromListWith (mergeTileConfig)
                                [ (if M.member old xform then xform M.! old else old, tc)
                                | (old, tc) <- M.toList (ccTiles cc)
                                ]
                        , ccTileGroups =
                            [ tg{tgTiles = map fixTileName (tgTiles tg)} | tg <- ccTileGroups cc ]
                        }
            else cc
      where
        fixTileName t
            | "BMID_0H" `T.isSuffixOf` t = T.snoc (T.dropEnd 1 t) 'V'
            | "BMID_2" `T.isSuffixOf` t = t <> "V"
            | otherwise = t
        mergeTileConfig a b =
            a{tcArcs = tcArcs a ++ tcArcs b, tcEnums = tcEnums a ++ tcEnums b, tcWords = tcWords a ++ tcWords b, tcUnknowns = tcUnknowns a ++ tcUnknowns b}



-- | A name for internal use.
belNameText :: BelInfo -> Text
belNameText = biName

