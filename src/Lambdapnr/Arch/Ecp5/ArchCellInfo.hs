{-# LANGUAGE OverloadedStrings #-}

{- | The per-cell architecture payload (mirror of @ArchCellInfo@ in
@ecp5\/archdefs.h@): comb\/ff\/ram\/mult flags and signals computed by
@assign_arch_info_for_cell@ (@ecp5\/pack.cc@). These drive slice
compatibility checking, LUT permutation (@COMB_CARRY@), RAM timing
ids, and the bitgen writers.
-}
module Lambdapnr.Arch.Ecp5.ArchCellInfo (
    CombFlags (..),
    FfFlags (..),
    CombInfo (..),
    FfInfo (..),
    RamInfo (..),
    MultInfo (..),
    ArchInfo (..),
    emptyArchInfo,
    combFlag,
    ffFlag,
    lookupComb,
    lookupFf,
    lookupRam,
    lookupMult,
    assignArchInfo,
    slicesCompatible,
    belCombZ,
    belFfZ,
    belRamwZ,
) where

import Data.Bits (shiftL, testBit, (.|.))
import Debug.Trace (trace)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import qualified Data.Text as T

import Lambdapnr.Kernel.IdString (IdString, emptyId)
import Lambdapnr.Kernel.Netlist
import Lambdapnr.Kernel.Property (Property, propAsInt64, propIsString, propAsString)

-- | @ArchCellInfo::CombFlags@.
data CombFlags
    = CombNone
    | CombCarry
    | CombLutram
    | CombMux5
    | CombMux6
    | CombRamWckInv
    | CombRamWreInv
    | CombRamwBlock
    deriving (Eq, Show, Enum)

combFlag :: CombFlags -> Int
combFlag = fromEnum

-- | @ArchCellInfo::FFFlags@.
data FfFlags
    = FfNone
    | FfClkInv
    | FfCeInv
    | FfCeConst
    | FfLsrInv
    | FfGsrEn
    | FfAsync
    | FfMUsed
    deriving (Eq, Show, Enum)

ffFlag :: FfFlags -> Int
ffFlag = fromEnum

-- | The comb payload (@combInfo@).
data CombInfo = CombInfo
    { ciFlags :: !Int
    , ciRamWck :: !IdString
    , ciRamWre :: !IdString
    , ciMuxFxad :: !(Maybe IdString)
    }
    deriving (Eq, Show)

-- | The ff payload (@ffInfo@).
data FfInfo = FfInfo
    { fiFlags :: !Int
    , fiClkSig :: !IdString
    , fiLsSig :: !IdString
    , fiCeSig :: !IdString
    , fiDiSig :: !IdString
    }
    deriving (Eq, Show)

-- | The blockram payload (@ramInfo@).
data RamInfo = RamInfo
    { riIsPdp :: !Bool
    , riIsOutputARegistered :: !Bool
    , riIsOutputBRegistered :: !Bool
    , riRegmodeTimingId :: !IdString
    }
    deriving (Eq, Show)

-- | The mult payload (@multInfo@).
data MultInfo = MultInfo
    { miIsClocked :: !Bool
    , miTimingId :: !IdString
    }
    deriving (Eq, Show)

-- | The per-cell arch info map (the C++ stores this inside each
-- @CellInfo@; we keep it separate so the generic kernel netlist stays
-- architecture-free).
data ArchInfo = ArchInfo
    { aiComb :: !(Map IdString CombInfo)
    , aiFf :: !(Map IdString FfInfo)
    , aiRam :: !(Map IdString RamInfo)
    , aiMult :: !(Map IdString MultInfo)
    }
    deriving (Eq, Show)

emptyArchInfo :: ArchInfo
emptyArchInfo = ArchInfo M.empty M.empty M.empty M.empty

lookupComb :: IdString -> ArchInfo -> CombInfo
lookupComb n ai = M.findWithDefault (CombInfo 0 emptyId emptyId Nothing) n (aiComb ai)

lookupFf :: IdString -> ArchInfo -> FfInfo
lookupFf n ai = M.findWithDefault (FfInfo 0 emptyId emptyId emptyId emptyId) n (aiFf ai)

lookupRam :: IdString -> ArchInfo -> RamInfo
lookupRam n ai = M.findWithDefault (RamInfo False False False emptyId) n (aiRam ai)

lookupMult :: IdString -> ArchInfo -> MultInfo
lookupMult n ai = M.findWithDefault (MultInfo False emptyId) n (aiMult ai)

-- | @str_or_default@.
strOrDef :: Map IdString Property -> IdString -> String -> String
strOrDef m k def =
    case M.lookup k m of
        Nothing -> def
        Just p -> if propIsString p then T.unpack (propAsString p) else show (propAsInt64 p)

-- | @int_or_default@.
intOrDef :: Map IdString Property -> IdString -> Int -> Int
intOrDef m k def =
    case M.lookup k m of
        Nothing -> def
        Just p -> if propIsString p then readInt (T.unpack (propAsString p)) else fromIntegral (propAsInt64 p)
  where
    readInt t = case reads t of
        [(i, "")] -> i
        _ -> def

-- | Resolve a constid name via the arch table (empty id when absent).
cid :: (T.Text -> Maybe IdString) -> T.Text -> IdString
cid resolve t = maybe emptyId id (resolve t)

-- | Get a cell's port net name (empty when unconnected), mirroring the
-- C++ @get_port_net@ lambda.
portNetName :: CellInfo bel wire pip -> IdString -> IdString
portNetName ci p = maybe emptyId id (portNet =<< M.lookup p (cellPorts ci))

-- | @Arch::assign_arch_info_for_cell@ (@ecp5\/pack.cc@). The resolver
-- maps a constid name to its 'IdString' (the arch's constid table).
assignArchInfo :: (T.Text -> Maybe IdString) -> Design bel wire pip -> ArchInfo
assignArchInfo resolve d =
    ArchInfo
        { aiComb = M.mapMaybeWithKey combOf (designCells d)
        , aiFf = M.mapMaybeWithKey ffOf (designCells d)
        , aiRam = M.mapMaybeWithKey ramOf (designCells d)
        , aiMult = M.mapMaybeWithKey multOf (designCells d)
        }
  where
    typeIs ci t = cellType ci == t
    combOf _ ci
        | ci `typeIs` (cid resolve "TRELLIS_COMB") =
            let mode = strOrDef (cellParams ci) (cid resolve "MODE") "LOGIC"
                flags0 = combFlag CombNone
                flags1
                    | mode == "CCU2" = flags0 .|. combFlag CombCarry
                    | otherwise = flags0
                wckmux = strOrDef (cellParams ci) (cid resolve "WCKMUX") "WCK"
                wremux = strOrDef (cellParams ci) (cid resolve "WREMUX") "WRE"
                flags2
                    | mode == "DPRAM" =
                        ( flags1
                            .|. combFlag CombLutram
                            .|. (if wckmux == "INV" then combFlag CombRamWckInv else 0)
                            .|. (if wremux == "INV" || wremux == "0" then combFlag CombRamWreInv else 0)
                        )
                    | otherwise = flags1
                flags3
                    | mode == "RAMW_BLOCK" = flags2 .|. combFlag CombRamwBlock
                    | otherwise = flags2
                flags4
                    | portNetName ci (cid resolve "F1") /= emptyId = flags3 .|. combFlag CombMux5
                    | otherwise = flags3
                (flags5, muxFxad)
                    | portNetName ci (cid resolve "FXA") /= emptyId || portNetName ci (cid resolve "FXB") /= emptyId =
                        let fxa = getPortNet ci (cid resolve "FXA")
                            drvCell
                                | fxa /= emptyId = prCell (netDriver (netOfNet fxa d))
                                | otherwise = Nothing
                         in (flags4 .|. combFlag CombMux6, drvCell)
                    | otherwise = (flags4, Nothing)
             in Just
                    CombInfo
                        { ciFlags = flags5
                        , ciRamWck = if mode == "DPRAM" then portNetName ci (cid resolve "WCK") else emptyId
                        , ciRamWre = if mode == "DPRAM" then portNetName ci (cid resolve "WRE") else emptyId
                        , ciMuxFxad = muxFxad
                        }
        | otherwise = Nothing

    ffOf _ ci
        | ci `typeIs` (cid resolve "TRELLIS_FF") =
            let gsr = strOrDef (cellParams ci) (cid resolve "GSR") "ENABLED"
                srmode = strOrDef (cellParams ci) (cid resolve "SRMODE") "LSR_OVER_CE"
                clkmux = strOrDef (cellParams ci) (cid resolve "CLKMUX") "CLK"
                cemux = strOrDef (cellParams ci) (cid resolve "CEMUX") "CE"
                lsrmux = strOrDef (cellParams ci) (cid resolve "LSRMUX") "LSR"
                flags =
                    0
                        .|. (if gsr == "ENABLED" then ffFlag FfGsrEn else 0)
                        .|. (if srmode == "ASYNC" then ffFlag FfAsync else 0)
                        .|. (if portNetName ci (cid resolve "M") /= emptyId then ffFlag FfMUsed else 0)
                        .|. (if clkmux == "INV" || clkmux == "0" then ffFlag FfClkInv else 0)
                        .|. (if cemux == "INV" || cemux == "0" then ffFlag FfCeInv else 0)
                        .|. (if cemux == "1" || cemux == "0" then ffFlag FfCeConst else 0)
                        .|. (if lsrmux == "INV" then ffFlag FfLsrInv else 0)
             in Just
                    FfInfo
                        { fiFlags = flags
                        , fiClkSig = portNetName ci (cid resolve "CLK")
                        , fiCeSig = portNetName ci (cid resolve "CE")
                        , fiLsSig = portNetName ci (cid resolve "LSR")
                        , fiDiSig = portNetName ci (cid resolve "DI")
                        }
        | otherwise = Nothing

    ramOf _ ci
        | ci `typeIs` (cid resolve "DP16KD") =
            let isPdp = intOrDef (cellParams ci) (cid resolve "DATA_WIDTH_A") 0 == 36
                regA = strOrDef (cellParams ci) (cid resolve "REGMODE_A") "NOREG"
                regB = strOrDef (cellParams ci) (cid resolve "REGMODE_B") "NOREG"
                outA = regA == "OUTREG"
                outB = regB == "OUTREG"
                timingId
                    | not outA && not outB = (cid resolve "DP16KD_REGMODE_A_NOREG_REGMODE_B_NOREG")
                    | not outA && outB = (cid resolve "DP16KD_REGMODE_A_NOREG_REGMODE_B_OUTREG")
                    | outA && not outB = (cid resolve "DP16KD_REGMODE_A_OUTREG_REGMODE_B_NOREG")
                    | otherwise = (cid resolve "DP16KD_REGMODE_A_OUTREG_REGMODE_B_OUTREG")
             in Just (RamInfo isPdp outA outB timingId)
        | otherwise = Nothing

    multOf _ ci
        | ci `typeIs` (cid resolve "MULT18X18D") =
            let regInA = strOrDef (cellParams ci) (cid resolve "REG_INPUTA_CLK") "NONE"
                regInB = strOrDef (cellParams ci) (cid resolve "REG_INPUTB_CLK") "NONE"
                regOut = strOrDef (cellParams ci) (cid resolve "REG_OUTPUT_CLK") "NONE"
                inA = regInA /= "NONE"
                inB = regInB /= "NONE"
                outR = regOut /= "NONE"
                anyIn = inA || inB
                bothIn = inA && inB
                timingId
                    | anyIn && not bothIn = if outR then (cid resolve "MULT18X18D_REGS_OUTPUT") else (cid resolve "MULT18X18D_REGS_NONE")
                    | not bothIn && not outR = (cid resolve "MULT18X18D_REGS_NONE")
                    | bothIn && not outR = (cid resolve "MULT18X18D_REGS_INPUT")
                    | not bothIn && outR = (cid resolve "MULT18X18D_REGS_OUTPUT")
                    | otherwise = (cid resolve "MULT18X18D_REGS_ALL")
             in Just (MultInfo (timingId /= (cid resolve "MULT18X18D_REGS_NONE")) timingId)
        | otherwise = Nothing

    getPortNet ci p = case portNet =<< M.lookup p (cellPorts ci) of
        Just net -> net
        Nothing -> emptyId
    netOfNet n d = M.findWithDefault (NetInfo emptyId emptyId (PortRef Nothing emptyId) V.empty [] M.empty M.empty emptyId) n (designNets d)

-- | The bel z for comb/ff/ramw slots.
belCombZ :: Int
belCombZ = 0

belFfZ :: Int
belFfZ = 1

belRamwZ :: Int
belRamwZ = 2

lcIdxShift :: Int
lcIdxShift = 2

-- | @Arch::slices_compatible@ (@ecp5\/arch_place.cc@): check that the
-- cells occupying the slots of a tile can share slices. The slot lookup
-- returns the cell (with its arch info) at a z slot, or Nothing when free.
slicesCompatible ::
    -- | slot z (0..31) -> cell (Nothing = free)
    (Int -> Maybe (CellInfo bel wire pip, ArchInfo)) ->
    Bool
slicesCompatible slotCell =
    goSlice 0 && goTile
  where
    -- per-slice checks
    goSlice sl
        | sl >= 4 = True
        | otherwise =
            trace ("SC slice " ++ show sl) $
            let ramwUsed = sl == 2 && case slotCell ((sl * 2) `shiftL` lcIdxShift .|. belRamwZ) of
                    Just _ -> True
                    Nothing -> False
             in sliceOk sl ramwUsed && goSlice (sl + 1)
    sliceOk sl ramwUsed = step 0 (True, False, 0, emptyId)
      where
        step l acc
            | l >= 2 = case acc of
                (ok, _, _, _) -> ok
            | otherwise =
                trace ("SC step " ++ show sl ++ " " ++ show l) $
                let comb = slotCell (((sl * 2 + l) `shiftL` lcIdxShift) .|. belCombZ)
                    ff = slotCell (((sl * 2 + l) `shiftL` lcIdxShift) .|. belFfZ)
                    -- comb checks
                    (ok0, combMUsed) = case comb of
                        Nothing -> (True, False)
                        Just (c, ai) ->
                            let flags = ciFlags (lookupComb (cellName c) ai)
                                okRamw = not (ramwUsed && not (testBit flags (combFlag CombRamwBlock)))
                                okMux5 = not (testBit flags (combFlag CombMux5) && l /= 0)
                                okMux6 = not (testBit flags (combFlag CombMux6) && l /= 1)
                                okMuxRoot =
                                    case ciMuxFxad (lookupComb (cellName c) ai) of
                                        Just fxad
                                            | testBit flags (combFlag CombMux6) ->
                                                let fxadFlags = ciFlags (lookupComb fxad ai)
                                                 in not (testBit fxadFlags (combFlag CombMux5) && (sl /= 0 && sl /= 2))
                                        _ -> True
                                okLutram = not (testBit flags (combFlag CombLutram) && sl > 1)
                                okCarryPair =
                                    if l == 1
                                        then case slotCell (((sl * 2 + 0) `shiftL` lcIdxShift) .|. belCombZ) of
                                            Just (c0, ai0) ->
                                                testBit (ciFlags (lookupComb (cellName c0) ai0)) (combFlag CombCarry)
                                                    == testBit flags (combFlag CombCarry)
                                            Nothing -> True
                                        else True
                             in ( okRamw && okMux5 && okMux6 && okMuxRoot && okLutram && okCarryPair
                                , testBit flags (combFlag CombMux5) || testBit flags (combFlag CombMux6)
                                )
                    -- ff checks
                    (okPrev, foundFfPrev, lastFlags, lastCe) = acc
                    (ok1, foundFf1, lastFlags1, lastCe1) = case ff of
                        Nothing -> (ok0, foundFfPrev, lastFlags, lastCe)
                        Just (c, ai) ->
                            let FfInfo flags _ _ ceSig _ = lookupFf (cellName c) ai
                                okM = not (combMUsed && testBit flags (ffFlag FfMUsed))
                             in if foundFfPrev
                                    then
                                        ( okPrev
                                            && okM
                                            && testBit flags (ffFlag FfGsrEn) == testBit lastFlags (ffFlag FfGsrEn)
                                            && testBit flags (ffFlag FfCeConst) == testBit lastFlags (ffFlag FfCeConst)
                                            && testBit flags (ffFlag FfCeInv) == testBit lastFlags (ffFlag FfCeInv)
                                            && ceSig == lastCe
                                        , True
                                        , lastFlags
                                        , lastCe
                                        )
                                    else (okPrev && okM, True, flags, ceSig)
                 in step (l + 1) (ok1, foundFf1, lastFlags1, lastCe1)
    -- per-tile control-set checks
    goTile =
        let (ok, _, _, _, _, _, _, _) = tileStep 0 (True, False, False, False, False, False, emptyId, emptyId)
         in ok
    tileStep i (ok, foundGf, foundGd, clkInv, lsrInv, async, clkSig, lsrSig)
        | i >= 8 = (ok, foundGf, foundGd, clkInv, lsrInv, async, clkSig, lsrSig)
        | otherwise =
            trace ("SC tile " ++ show i) $
            -- DPRAM comb (bottom half of the tile)
            let (ok0, foundGd1, clkInv1, lsrInv1) =
                    if i < 4
                        then case slotCell ((i `shiftL` lcIdxShift) .|. belCombZ) of
                            Just (c, ai)
                                | testBit (ciFlags (lookupComb (cellName c) ai)) (combFlag CombLutram) ->
                                    let flags = ciFlags (lookupComb (cellName c) ai)
                                        thisCkInv = testBit flags (combFlag CombRamWckInv)
                                        thisLsInv = testBit flags (combFlag CombRamWreInv)
                                     in if foundGd
                                            then (ok && thisCkInv == clkInv && thisLsInv == lsrInv, True, clkInv, lsrInv)
                                            else (ok, True, thisCkInv, thisLsInv)
                            _ -> (ok, foundGd, clkInv, lsrInv)
                        else (ok, foundGd, clkInv, lsrInv)
                (ok1, foundGf1, clkInv2, lsrInv2, async1, clkSig1, lsrSig1) =
                    case slotCell ((i `shiftL` lcIdxShift) .|. belFfZ) of
                        Nothing -> (ok0, foundGf, clkInv1, lsrInv1, async, clkSig, lsrSig)
                        Just (c, ai) ->
                            let FfInfo flags clkSigF lsrSigF _ _ = lookupFf (cellName c) ai
                                thisCkInv = testBit flags (ffFlag FfClkInv)
                                thisLsInv = testBit flags (ffFlag FfLsrInv)
                                thisAsync = testBit flags (ffFlag FfAsync)
                             in if foundGf
                                    then
                                        ( ok0
                                            && clkSigF == clkSig
                                            && lsrSigF == lsrSig
                                            && thisCkInv == clkInv1
                                            && thisLsInv == lsrInv1
                                            && thisAsync == async
                                        , True
                                        , clkInv1
                                        , lsrInv1
                                        , async
                                        , clkSig
                                        , lsrSig
                                        )
                                    else
                                        ( ok0
                                            && (not foundGd1 || (thisCkInv == clkInv1 && thisLsInv == lsrInv1))
                                        , True
                                        , thisCkInv
                                        , thisLsInv
                                        , thisAsync
                                        , clkSigF
                                        , lsrSigF
                                        )
             in tileStep (i + 1) (ok1, foundGf1, foundGd1, clkInv2, lsrInv2, async1, clkSig1, lsrSig1)
