{-# LANGUAGE OverloadedStrings #-}

{- | The ECP5 bitstream generator — the Haskell mirror of
@ecp5\/bitstream.cc@ (@ECP5Bitgen@) for the @--textcfg@ output: the
prjtrellis text config that @ecppack@ packs into a bitstream.

Pipeline (mirroring @ECP5Bitgen::run@): start from the device's empty
base configuration, add the @Part:@ metadata, then every bound
configurable pip as a routing @arc:@, then per-cell settings
(@write_comb@\/@write_ff@\/RAMW). The cell writers cover the logic
slice; the remaining cells (IO, DCC, PLL, DSP, ...) land with the
packer milestone.
-}
module Lambdapnr.Arch.Ecp5.Bitgen (
    buildConfig,
    getTrellisWireName,
    getPipTileName,
    getTileByTypeLoc,
    fullChipName,
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Lambdapnr.Arch.Ecp5.BaseConfigs (configEmpty)
import Lambdapnr.Arch.Ecp5.Binding (boundPipNet)
import Lambdapnr.Arch.Ecp5.CellTiming (TimingDb (..))
import Lambdapnr.Arch.Ecp5.Chipdb
import Lambdapnr.Arch.Ecp5.Config
import Lambdapnr.Arch.Ecp5.Types
import Lambdapnr.Arch.Ecp5 (Ecp5 (..), ecp5Args, ecp5Bind, ecp5Chipdb, ecp5IdTable, ecp5TimingDb)
import Lambdapnr.Kernel.Arch hiding (locX, locY, locZ)
import Lambdapnr.Kernel.IdString (IdString, emptyId, idToText)
import Lambdapnr.Kernel.Netlist
import Lambdapnr.Kernel.Property (Property, propAsInt64, propAsString, propIsString)

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

-- | The name of the tile at (row, col) with the given type
-- (@get_tile_by_type_loc@).
getTileByTypeLoc :: Ecp5 -> Int -> Int -> Text -> Text
getTileByTypeLoc e row col typ =
    let cd = ecp5Chipdb e
        tileIdx = row * cdWidth cd + col
        names = tiTileNames (cdTileInfos cd V.! tileIdx)
     in case V.find (\tn -> cdTiletypeNames cd V.! fromIntegral (tnTypeIdx tn) == typ) names of
            Just tn -> tnName tn
            Nothing -> ""

-- | The trellis wire name of a wire relative to a location
-- (@get_trellis_wirename@): G_/L_/R_ wires and same-tile wires keep
-- their basename; others get an N/S/E/W relative prefix.
getTrellisWireName :: Ecp5 -> Location -> WireId -> Text
getTrellisWireName e loc wire =
    let basename = wiName (wireAt (ecp5Chipdb e) wire)
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

-- | Build the chip configuration from the arch + the (routed, bound)
-- design.
buildConfig :: Ecp5 -> Design BelId WireId PipId -> ChipConfig
buildConfig e design =
    let args = ecp5Args e
        -- the 12k part uses the 25k base config (the C++ run() does the
        -- same and then renames the chip)
        baseName = if eaDevice args == Lfe5u12f then "LFE5U-25F" else deviceName (eaDevice args)
        base = configEmpty baseName
        cc0 = emptyChipConfig{ccChipName = deviceName (eaDevice args), ccTiles = base}
        cc1 = cc0{ccMetadata = ["Part: " <> fullChipName args]}
        -- every bound, configurable pip becomes a routing arc
        cc2 = foldl addPip cc1 (getPips e)
     in foldl addCellConfig cc2 (M.elems (designCells design))
  where
    -- bound pips: pip_class (pip_type) /= 0 are configurable; the
    -- CLKI_PLL special case mirrors the C++ (set the pip in all
    -- equivalent tiles)
    addPip cc p =
        case boundPipNet p (ecp5Bind e) of
            Nothing -> cc
            Just _ ->
                let pi = pipAt (ecp5Chipdb e) p
                 in if piPipType pi == 0
                        then cc
                        else
                            let source = getTrellisWireName e (pipLoc p) (getPipSrcWire e p)
                             in if "CLKI_PLL" `T.isInfixOf` source
                                    then foldl (\cc' ep -> setPip cc' ep) cc [ep | ep <- getPipsUphill e (getPipDstWire e p), getPipSrcWire e ep == getPipSrcWire e p]
                                    else setPip cc p
    setPip cc p =
        let tile = getPipTileName e p
            source = getTrellisWireName e (pipLoc p) (getPipSrcWire e p)
            sink = getTrellisWireName e (pipLoc p) (getPipDstWire e p)
            tc = M.findWithDefault emptyTileConfig tile (ccTiles cc)
         in cc{ccTiles = M.insert tile (addArc sink source tc) (ccTiles cc)}

    -- cell configuration
    addCellConfig cc ci =
        case idToType ci of
            "TRELLIS_COMB" -> writeComb cc ci
            "TRELLIS_FF" -> writeFf cc ci
            "TRELLIS_RAMW" -> writeRamw cc ci
            _ -> cc

    -- the packed cell type as text (the constid name)
    idToType ci = idToText (ecp5IdTable e) (cellType ci)

    -- cell params via str_or_default / int_or_default semantics
    strParam ci name def = case M.lookup (constIdOf name) (cellParams ci) of
        Just p -> if propIsString p then propAsString p else T.pack (show (propAsInt64 p))
        Nothing -> def
    intParam ci name def = case M.lookup (constIdOf name) (cellParams ci) of
        Just p -> if propIsString p then propAsString p else T.pack (show (propAsInt64 p))
        Nothing -> def
    constIdOf name =
        case M.lookup name (tdConstIdByName (ecp5TimingDb e)) of
            Just i -> i
            Nothing -> emptyId

    -- @write_comb@: slice mode, LUT init word, CCU2 inject, input muxes
    writeComb cc ci =
        case cellBel ci of
            Nothing -> cc
            Just bel ->
                let Loc bx by _ = getBelLocation e bel
                    tname = getTileByTypeLoc e (fromIntegral by) (fromIntegral bx) "PLC2"
                    z = fromIntegral (belZOf e bel) `div` 4
                    slice = "SLICE" <> T.singleton ("ABCD" !! (z `mod` 4))
                    lc = T.pack (show (z `mod` 2))
                    mode = strParam ci "MODE" "LOGIC"
                 in if mode == "RAMW_BLOCK"
                        then cc
                        else
                            let lutInit = intParam ci "INITVAL" "0"
                                tc = M.findWithDefault emptyTileConfig tname (ccTiles cc)
                                tc1 = addEnum (slice <> ".MODE") mode tc
                                tc2 = addWord (slice <> ".K" <> lc <> ".INIT") (intToBits (readIntOrZero lutInit) 16) tc1
                                tc3 = if mode == "CCU2"
                                    then addEnum (slice <> ".CCU2.INJECT1_" <> lc) (strParam ci "CCU2_INJECT1" "YES") tc2
                                    else addEnum (slice <> ".CCU2.INJECT1_" <> lc) "_NONE_" tc2
                                -- TODO: DPRAM WREMUX/WCKMUX special case
                                tc4 = foldl (\t pin -> addEnum (slice <> "." <> pin <> lc <> "MUX") "1" t) tc3 ["A", "B", "C", "D"]
                             in cc{ccTiles = M.insert tname tc4 (ccTiles cc)}

    -- @write_ff@: GSR, SD/REGSET/LSRMODE, CEMUX and the LSR/CLK muxes
    writeFf cc ci =
        case cellBel ci of
            Nothing -> cc
            Just bel ->
                let Loc bx by _ = getBelLocation e bel
                    tname = getTileByTypeLoc e (fromIntegral by) (fromIntegral bx) "PLC2"
                    z = fromIntegral (belZOf e bel) `div` 4
                    slice = "SLICE" <> T.singleton ("ABCD" !! (z `mod` 4))
                    lc = T.pack (show (z `mod` 2))
                    tc0 = M.findWithDefault emptyTileConfig tname (ccTiles cc)
                    tc1 = addEnum (slice <> ".GSR") (strParam ci "GSR" "ENABLED") tc0
                    tc2 = addEnum (slice <> ".REG" <> lc <> ".SD") (strParam ci "SD" "0") tc1
                    tc3 = addEnum (slice <> ".REG" <> lc <> ".REGSET") (strParam ci "REGSET" "RESET") tc2
                    tc4 = addEnum (slice <> ".REG" <> lc <> ".LSRMODE") (strParam ci "LSRMODE" "LSR") tc3
                    tc5 = addEnum (slice <> ".CEMUX") (strParam ci "CEMUX" "1") tc4
                    -- TODO: LSR/CLK wire mux selection (needs bound wire comparison)
                 in cc{ccTiles = M.insert tname tc5 (ccTiles cc)}

    -- @write_ramw@: the RAMW slice mode
    writeRamw cc ci =
        case cellBel ci of
            Nothing -> cc
            Just bel ->
                let Loc bx by _ = getBelLocation e bel
                    tname = getTileByTypeLoc e (fromIntegral by) (fromIntegral bx) "PLC2"
                    tc = M.findWithDefault emptyTileConfig tname (ccTiles cc)
                    tc1 = addEnum "SLICEC.MODE" "RAMW" tc
                    tc2 = addWord "SLICEC.K0.INIT" (replicate 16 False) tc1
                    tc3 = addWord "SLICEC.K1.INIT" (replicate 16 False) tc2
                 in cc{ccTiles = M.insert tname tc3 (ccTiles cc)}

    belZOf e' bel = biZ (belAt (ecp5Chipdb e') bel)

    -- a 16-bit LSB-first bit vector from an integer
    intToBits :: Int -> Int -> [Bool]
    intToBits n size = [n `div` (2 ^ i) `mod` 2 == 1 | i <- [0 .. size - 1]]

    readIntOrZero :: Text -> Int
    readIntOrZero t = case reads (T.unpack t) of
        [(i, "")] -> i
        _ -> 0

