{-# LANGUAGE OverloadedStrings #-}

-- | @placer_heap@ (@common\/place\/placer_heap.cc@): the ECP5 HeAP
-- placer. Stage 1 (this module so far): constrain BEL cells, build the
-- fast-bel bounds, seed a random initial placement, and report the
-- initial HPWL — the first deterministic anchor ("Creating initial
-- analytic placement for N cells, random placement wirelen = W").
module Lambdapnr.Arch.Ecp5.PlacerHeap
  ( CellLoc (..)
  , PlacerState (..)
  , emptyPlacerState
  , placeHeapSeed
  , placeHeapInitialIters
  , placeHeapMain
  , isBelLocValidE
  , dspLocationValid
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Foldable as F
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Control.Monad (forM_, when)
import Data.List (maximum, sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Environment (lookupEnv)
import System.CPUTime (getCPUTime)
import System.IO (hPutStrLn, stderr)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

import Lambdapnr.Arch.Ecp5
import Lambdapnr.Arch.Ecp5.Types (BelId, PipId, WireId)
import Lambdapnr.Kernel.Delay (DelayPair (..), DelayQuad (..))
import Lambdapnr.Kernel.TimingAnalyser (ArrivReqTime (..), CellArc (..), CellArcType (..), CellPortKey (..), PerDomain (..), PerDomainPair (..), PerPort (..), PortDomainPairData (..), TimingAnalyser (..), buildTimingAnalyser, criticalityOf, runTimingAnalyser)
import Lambdapnr.Arch.Ecp5.ArchCellInfo (ArchInfo, CombInfo (..), FfInfo (..), assignArchInfo, lookupComb, lookupFf, slicesCompatible)
import Lambdapnr.Arch.Ecp5.CellTiming (getCellDelayAi, getPortClockingInfoAi, getPortTimingClassAi)
import Lambdapnr.Arch.Ecp5.Binding (bindBelLut, boundBelCell, unbindBel)

import Lambdapnr.Arch.Ecp5.Chipdb (belAt, biZ)
import Lambdapnr.Arch.Ecp5.Types (BelId (..), Ecp5Device (..), Location (..), eaDevice)
import Lambdapnr.Kernel.Arch (Loc (..), checkBelAvail, getBelByLocation, getBelGlobalBuf, getBelLocation, getBelByName, getBels, getBelsByTile, getBelType, isValidBelForCellType)
import Lambdapnr.Kernel.DeterministicRng (Rng, rngBounded, rngState, shuffle)
import Foreign (Ptr, mallocArray, peekArray, withArray)
import Foreign.C.Types (CDouble (..), CInt (..))
import Foreign.Storable (peek, poke)
import Lambdapnr.Kernel.IdString (IdString (..), IdTable, emptyId, idToText, intern)
import Lambdapnr.Kernel.Netlist
import Lambdapnr.Kernel.Property (propAsString, propFromString)

-- | bindBel including the C++ lutperm_allowed side effect.
bindBelH :: Ecp5 -> IdString -> BelId -> PlaceStrength -> Ecp5 -> Design BelId WireId PipId -> (Ecp5, Design BelId WireId PipId)
bindBelH e cell bel strength eAcc d = let (bs, d') = bindBelLut (combCtxOf e) (ecp5Chipdb e) cell bel strength (ecp5Bind eAcc) d in (setEcp5Bind bs eAcc, d')


-- | The placer's per-cell location record (@CellLocation@).
data CellLoc = CellLoc
    { plcX :: !Int
    , plcY :: !Int
    , plcLegalX :: !Int
    , plcLegalY :: !Int
    , plcRawX :: !Double
    , plcRawY :: !Double
    , plcLocked :: !Bool
    , plcGlobal :: !Bool
    }
    deriving (Eq, Show)

-- | Placer state threaded through the heap stages.
data PlacerState = PlacerState
    { psLocs :: !(M.Map IdString CellLoc)
    , psPlaceCells :: ![IdString]
    -- ^ cells with connectivity, pushed in seed order
    , psMaxX :: !Int
    , psMaxY :: !Int
    , psRng :: !Rng
    }

emptyPlacerState :: Rng -> PlacerState
emptyPlacerState = PlacerState M.empty [] 0 0

-- | Intern on demand (mirrors the arch's lazy bel/wire name ids).
internOnDemand :: IdTable -> T.Text -> IdString
internOnDemand tbl t = unsafePerformIO (intern tbl t)
{-# NOINLINE internOnDemand #-}

-- | @place()@ stage 1: constrain, seed, report initial HPWL. Returns
-- the updated arch (bind state), design (bel bindings) and placer state.
placeHeapSeed ::
    Ecp5 ->
    (T.Text -> Maybe IdString) ->
    Design BelId WireId PipId ->
    PlacerState ->
    (Ecp5, Design BelId WireId PipId, PlacerState, Int, Int)
placeHeapSeed e cidOf d0 ps0 =
    let (e1, d1, nConstr) = placeConstraints e cidOf d0
        (maxX, maxY) = buildFastBels e1
        (e3, d2, locs, placeCells, rng2) = seedPlacement e1 cidOf d1 ps0
        locs' = updateAllChains d2 locs placeCells maxX maxY
        hpwl = totalHpwl d2 locs'
     in (e3, d2, PlacerState locs' placeCells maxX maxY rng2, nConstr, hpwl)

-- | @place_constraints@: bind every non-pseudo cell with a BEL attr
-- with @STRENGTH_USER@.
placeConstraints ::
    Ecp5 ->
    (T.Text -> Maybe IdString) ->
    Design BelId WireId PipId ->
    (Ecp5, Design BelId WireId PipId, Int)
placeConstraints e cidOf d0 =
    foldl placeOne (e, d0, 0 :: Int) (cellsIter d0)
  where
    ai0 = assignArchInfo cidOf d0
    belAttrId = fromMaybe emptyId (cidOf "BEL")
    placeOne (eAcc, dAcc, n) ci =
        case M.lookup belAttrId (cellAttrs ci) of
            Nothing -> (eAcc, dAcc, n)
            Just locP ->
                case getBelByNameStrE eAcc (propAsString locP) of
                    Nothing -> error ("No Bel named '" ++ T.unpack (propAsString locP) ++ "' located for this chip (processing BEL attribute on '" ++ T.unpack (idToText (ecp5IdTable eAcc) (cellName ci)) ++ "')")
                    Just bel ->
                        if not (isValidBelForCellType eAcc (cellType ci) bel)
                            then error ("Bel '" ++ T.unpack (propAsString locP) ++ "' of a type that does not match cell '" ++ T.unpack (idToText (ecp5IdTable eAcc) (cellName ci)) ++ "'")
                            else
                                case boundBelCell bel (ecp5Bind eAcc) of
                                    Just other -> error ("Cell '" ++ T.unpack (idToText (ecp5IdTable eAcc) (cellName ci)) ++ "' cannot be bound to bel since already bound to cell '" ++ T.unpack (idToText (ecp5IdTable eAcc) other) ++ "'")
                                    Nothing ->
                                        let (bs, d') = bindBelLut (combCtxOf eAcc) (ecp5Chipdb eAcc) (cellName ci) bel StrengthUser (ecp5Bind eAcc) dAcc
                                            e' = setEcp5Bind bs eAcc
                                         in if isBelLocValidE ai0 e' cidOf d' bel
                                                then (e', d', n + 1)
                                                else error ("Bel '" ++ T.unpack (propAsString locP) ++ "' is not valid for cell '" ++ T.unpack (idToText (ecp5IdTable eAcc) (cellName ci)) ++ "'")

    getBelByNameStrE eAcc s =
        case map (internOnDemand (ecp5IdTable eAcc)) (T.splitOn "/" s) of
            [x, y, n] -> getBelByName eAcc [x, y, n]
            _ -> Nothing

-- | @build_fast_bels@: the max bel coordinate over available bels.
buildFastBels :: Ecp5 -> (Int, Int)
buildFastBels e =
    foldl
        ( \(x0, y0) b ->
            if checkBelAvail e b
                then
                    let Loc lx ly _ = getBelLocation e b
                     in (max x0 lx, max y0 ly)
                else (x0, y0)
        )
        (0, 0)
        (getBels e)

-- | @seed_placement@: random initial placement, without legality.
seedPlacement ::
    Ecp5 ->
    (T.Text -> Maybe IdString) ->
    Design BelId WireId PipId ->
    PlacerState ->
    (Ecp5, Design BelId WireId PipId, M.Map IdString CellLoc, [IdString], Rng)
seedPlacement e cidOf d ps =
    let ai0 = assignArchInfo cidOf d
        -- the C++ cell_types pool keeps encounter order; available_bels
        -- shuffles run in forward encounter order (see below)
        cellTypes =
            foldl
                (\ts ci -> if cellType ci `elem` ts then ts else ts ++ [cellType ci])
                []
                (cellsIter d)
        -- bels available per cell type, in getBels order
        available0 =
            foldl
                ( \m b ->
                    if not (checkBelAvail e b)
                        then m
                        else
                            foldl
                                ( \m' t ->
                                    if isValidBelForCellType e t b
                                        then M.insertWith (++) t [b] m'
                                        else m'
                                )
                                m
                                cellTypes
                )
                M.empty
                (getBels e)
        -- shuffle each type's bel list; the C++ shuffles in the
        -- available_bels dict's iteration order: keys are inserted when
        -- the type's first valid bel appears in getBels order, and the
        -- dict iterates in reverse insertion order
        shuffleOrder =
            reverse
                ( foldl
                    ( \acc b ->
                        if not (checkBelAvail e b)
                            then acc
                            else
                                foldl
                                    ( \acc' t ->
                                        if t `elem` acc' || not (isValidBelForCellType e t b)
                                            then acc'
                                            else acc' ++ [t]
                                    )
                                    acc
                                    cellTypes
                    )
                    []
                    (getBels e)
                )
        (available1, rng1) =
            foldl
                ( \(m, r) t ->
                    let (v, r') = shuffle r (V.fromList (M.findWithDefault [] t m))
                        dbg = unsafePerformIO (hPutStrLn stderr ("LPDBG shuf " ++ T.unpack (idToText (ecp5IdTable e) t) ++ " " ++ show (V.length v)))
                     in dbg `seq` (M.insert t (V.toList v) m, r')
                )
                (M.map reverse available0, psRng ps)
                shuffleOrder
        _ =
            case reverse (M.findWithDefault [] (fromMaybe emptyId (cidOf "TRELLIS_FF")) available1) of
                (b : _) ->
                    let Location bx by = belLoc b
                     in unsafePerformIO (hPutStrLn stderr ("LPDBG ffback " ++ show bx ++ " " ++ show by))
                [] -> ()
        ioBuf = fromMaybe emptyId (cidOf "TRELLIS_IO")
        noBelErr ci t = "Unable to place cell '" ++ T.unpack (idToText (ecp5IdTable e) (cellName ci)) ++ "', no BELs remaining to implement cell type '" ++ T.unpack (idToText (ecp5IdTable e) t) ++ "'"
        go (eAcc, dAcc, locsAcc, placeAcc, usedAcc, rngAcc, availAcc) ci =
            case cellBel ci of
                Just bel ->
                    let Loc lx ly _ = getBelLocation eAcc bel
                     in ( eAcc
                        , dAcc
                        , M.insert (cellName ci) (CellLoc lx ly 0 0 0 0 True (getBelGlobalBuf eAcc bel)) locsAcc
                        , placeAcc
                        , usedAcc
                        , rngAcc
                        , availAcc
                        )
                Nothing
                    | cellCluster ci /= emptyId && cellCluster ci /= cellName ci ->
                        (eAcc, dAcc, locsAcc, placeAcc, usedAcc, rngAcc, availAcc)
                    | otherwise ->
                        let t = cellType ci
                            list = M.findWithDefault (error (noBelErr ci t)) t availAcc
                            -- the C++ pops the BACK of the shuffled deque
                            pick cs used'
                                | null cs = error (noBelErr ci t)
                                | S.member (last cs) used' = pick (init cs) used'
                                | otherwise = last cs
                            bel = pick list usedAcc
                            availAcc' = M.insert t (filter (/= bel) list) availAcc
                            Loc lx ly _ = getBelLocation eAcc bel
                            loc = CellLoc lx ly 0 0 0 0 False (getBelGlobalBuf eAcc bel)
                         in if hasConnectivity d ci && cellType ci /= ioBuf
                                then ( eAcc
                                     , dAcc
                                     , M.insert (cellName ci) loc locsAcc
                                     , placeAcc ++ [cellName ci]
                                     , S.insert bel usedAcc
                                     , rngAcc
                                     , availAcc'
                                     )
                                else
                                    let (bs, d') = bindBelLut (combCtxOf eAcc) (ecp5Chipdb eAcc) (cellName ci) bel StrengthStrong (ecp5Bind eAcc) dAcc
                                        e' = setEcp5Bind bs eAcc
                                     in if isBelLocValidE ai0 e' cidOf d' bel
                                            then ( e'
                                                 , d'
                                                 , M.insert (cellName ci) loc{plcLocked = True} locsAcc
                                                 , placeAcc
                                                 , S.insert bel usedAcc
                                                 , rngAcc
                                                 , availAcc'
                                                 )
                                            else
                                                let (bs2, d2) = unbindBel (cellName ci) bel bs d'
                                                 in ( setEcp5Bind bs2 eAcc
                                                    , d2
                                                    , locsAcc
                                                    , placeAcc
                                                    , usedAcc
                                                    , rngAcc
                                                    , M.insert t (bel : list) availAcc
                                                    )
        (e', d', locs, placeCells, _, rng2, _) =
            foldl go (e, d, M.empty, [], S.empty, rng1, available1) (cellsIter d)
     in (e', d', locs, placeCells, rng2)

-- | @has_connectivity@: any port with a driven, used net.
hasConnectivity :: Design BelId WireId PipId -> CellInfo BelId WireId PipId -> Bool
hasConnectivity d ci =
    any
        ( \pi ->
            case portNet pi of
                Nothing -> False
                Just n ->
                    case M.lookup n (designNets d) of
                        Nothing -> False
                        Just ni ->
                            prCell (netDriver ni) /= Nothing
                                && V.length (V.filter (maybe False (const True)) (netUsers ni)) > 0
        )
        (M.elems (cellPorts ci))

-- | @update_all_chains@: propagate cluster-root locations to children.
updateAllChains ::
    Design BelId WireId PipId ->
    M.Map IdString CellLoc ->
    [IdString] ->
    Int ->
    Int ->
    M.Map IdString CellLoc
updateAllChains d locs placeCells maxX maxY =
    foldl updateChain locs placeCells
  where
    clusterChildren root =
        [ (cellName ci, cellConstrX ci, cellConstrY ci)
        | ci <- cellsIter d
        , cellCluster ci == root
        , cellName ci /= root
        ]
    updateChain acc root =
        case M.lookup root acc of
            Nothing -> acc
            Just base ->
                foldl
                    ( \acc' (cname, dx, dy) ->
                        M.insert
                            cname
                            ( (M.findWithDefault (CellLoc 0 0 0 0 0 0 False False) cname acc')
                                { plcX = max 0 (min maxX (plcX base + dx))
                                , plcY = max 0 (min maxY (plcY base + dy))
                                }
                            )
                            acc'
                    )
                    acc
                    (clusterChildren root)

-- | @total_hpwl@: bounding-box half-perimeter wirelength.
totalHpwl :: Design BelId WireId PipId -> M.Map IdString CellLoc -> Int
totalHpwl d locs =
    foldl step 0 (netsIter d)
  where
    step acc ni =
        case prCell (netDriver ni) of
            Nothing -> acc
            Just drv ->
                case M.lookup drv locs of
                    Nothing -> acc
                    Just drvLoc
                        | plcGlobal drvLoc -> acc
                        | otherwise ->
                            let (xmin, xmax, ymin, ymax) =
                                    foldl
                                        ( \(x0, x1, y0, y1) u ->
                                            case prCell u of
                                                Just uc ->
                                                    case M.lookup uc locs of
                                                        Just ul -> (min x0 (plcX ul), max x1 (plcX ul), min y0 (plcY ul), max y1 (plcY ul))
                                                        Nothing -> (x0, x1, y0, y1)
                                                Nothing -> (x0, x1, y0, y1)
                                        )
                                        (plcX drvLoc, plcX drvLoc, plcY drvLoc, plcY drvLoc)
                                        [u | Just u <- V.toList (netUsers ni)]
                             in acc + (xmax - xmin) + (ymax - ymin)


-- | ECP5 @isBelLocationValid@: slice compatibility for COMB/FF/RAMW
-- bels (over the tile's currently bound cells), device check for
-- DCUA/EXTREFB/PCSCLKDIV, DSP signal check for MULT18X18D/ALU54B.
isBelLocValidE ::
    ArchInfo ->
    Ecp5 ->
    (T.Text -> Maybe IdString) ->
    Design BelId WireId PipId ->
    BelId ->
    Bool
isBelLocValidE ai e cidOf d bel =
    let t = getBelType e bel
        cidT x = fromMaybe emptyId (cidOf x)
     in if t == cidT "TRELLIS_COMB" || t == cidT "TRELLIS_FF" || t == cidT "TRELLIS_RAMW"
            then
                let Location x y = belLoc bel
                    slots =
                        [ (biZ (belAt (ecp5Chipdb e) b), c)
                        | b <- getBelsByTile e (fromIntegral x) (fromIntegral y)
                        , Just c <- [boundBelCell b (ecp5Bind e)]
                        ]
                    _dbgSlots =
                        if x == 24 && y == 24
                            then unsafePerformIO (appendFile "/tmp/hs_slots.txt" (concatMap (\(z, c) -> show z ++ ":" ++ show (unIdString c) ++ ":" ++ show (unIdString (cellType (M.findWithDefault (error "slotc") c (designCells d)))) ++ " ") slots ++ "\n"))
                            else ()
                    _dbgFlags =
                        if x == 24 && y == 24
                            then
                                unsafePerformIO
                                    ( appendFile "/tmp/hs_flags.txt"
                                        ( concatMap
                                            ( \(z, c) ->
                                                let bt = z `mod` 4
                                                    ci = M.findWithDefault (error "flc") c (designCells d)
                                                    nm = T.unpack (idToText (ecp5IdTable e) c)
                                                    clu = unIdString (cellCluster ci)
                                                 in (if bt == 0
                                                        then show z ++ "C:" ++ show (unIdString c) ++ ":" ++ nm ++ ":clu" ++ show clu ++ " flags=" ++ show (ciFlags (lookupComb c ai)) ++ " "
                                                        else
                                                            if bt == 1
                                                                then let FfInfo fk clk lsr ce _ = lookupFf c ai in show z ++ "F:" ++ show (unIdString c) ++ ":" ++ nm ++ ":clu" ++ show clu ++ " flags=" ++ show fk ++ " clk=" ++ show (unIdString clk) ++ " lsr=" ++ show (unIdString lsr) ++ " ce=" ++ show (unIdString ce) ++ " "
                                                                else show z ++ "R ")
                                            )
                                            slots
                                            ++ "\n"
                                        )
                                    )
                            else ()
                    slotCell z = case lookup (fromIntegral z) slots of
                        Just cName -> Just (M.findWithDefault (error "slot cell") cName (designCells d), ai)
                        Nothing -> Nothing
                 in let r = slicesCompatible slotCell
                        _dbgV = if x == 24 && y == 24 then unsafePerformIO (appendFile "/tmp/hs_valid.txt" ("cell=" ++ maybe "-" (show . unIdString) (boundBelCell bel (ecp5Bind e)) ++ " z=" ++ show (biZ (belAt (ecp5Chipdb e) bel)) ++ ":" ++ (if r then "1" else "0") ++ "\n")) else ()
                     in r `seq` (_dbgSlots `seq` _dbgFlags `seq` _dbgV `seq` r)
            else
                case boundBelCell bel (ecp5Bind e) of
                    Nothing -> True
                    Just cName ->
                        let tc = cellType (M.findWithDefault (error "dsp cell") cName (designCells d))
                         in if tc == cidT "DCUA" || tc == cidT "EXTREFB" || tc == cidT "PCSCLKDIV"
                                then eaDevice (ecp5Args e) `notElem` [Lfe5u25f, Lfe5u45f, Lfe5u85f]
                                else
                                    if tc == cidT "MULT18X18D" || tc == cidT "ALU54B"
                                        then dspLocationValid e cidOf d (M.findWithDefault (error "dsp cell2") cName (designCells d))
                                        else True

-- | @is_dsp_location_valid@: at most four distinct CLK/CE/RST nets per
-- DSP block of two slices.
dspLocationValid :: Ecp5 -> (T.Text -> Maybe IdString) -> Design BelId WireId PipId -> CellInfo BelId WireId PipId -> Bool
dspLocationValid e cidOf d cell =
    case cellBel cell of
        Nothing -> True
        Just bel ->
            let Loc lx ly lz = getBelLocation e bel
                blockX = lx - lz
                blockY = ly
                cidT x = fromMaybe emptyId (cidOf x)
                groups =
                    [ map cidT ["CLK0", "CLK1", "CLK2", "CLK3"]
                    , map cidT ["CE0", "CE1", "CE2", "CE3"]
                    , map cidT ["RST0", "RST1", "RST2", "RST3"]
                    ]
                netOfPort ci port = portNet =<< M.lookup port (cellPorts ci)
                okGroup bc ports =
                    let nets = foldl (\acc port -> maybe acc (`S.insert` acc) (netOfPort bc port)) S.empty ports
                     in S.size nets <= 4
                go dx =
                    case getBelByLocation e (Loc (blockX + dx) blockY dx) of
                        Nothing -> True
                        Just b ->
                            case boundBelCell b (ecp5Bind e) of
                                Nothing -> True
                                Just bcName -> all (okGroup (M.findWithDefault (error "dsp bc") bcName (designCells d))) groups
             in all go [0, 1, 3, 4, 5, 7]


-- ---------------------------------------------------------------------------
-- Equation solving (placer_heap.cc: EquationSystem + build/solve_equations)
-- ---------------------------------------------------------------------------

-- | The C++ @EquationSystem@: sparse columns (sorted, unique rows) + RHS.
data EqSys = EqSys
    { esCols :: !(Seq [(Int, Double)])
    , esRhs :: !(Seq Double)
    }

emptyEqSys :: Int -> EqSys
emptyEqSys n = EqSys (Seq.fromList (replicate n [])) (Seq.fromList (replicate n 0))

-- | @add_coeff@: accumulate into the sorted column (binary-search insert).
addCoeff :: EqSys -> Int -> Int -> Double -> EqSys
addCoeff es row col val =
    es{esCols = Seq.update col (ins row val (Seq.index (esCols es) col)) (esCols es)}
  where
    ins r v [] = [(r, v)]
    ins r v ((r', x) : rest)
        | r' == r = (r', x + v) : rest
        | r' > r = (r, v) : (r', x) : rest
        | otherwise = (r', x) : ins r v rest

-- | @add_rhs@.
addRhs :: EqSys -> Int -> Double -> EqSys
addRhs es row val = es{esRhs = Seq.adjust (+ val) row (esRhs es)}

-- | Generic O(n) list element update (used by the spreader region lists).
updateNth :: Int -> (a -> a) -> [a] -> [a]
updateNth i f xs = case splitAt i xs of
    (pre, x : post) -> pre ++ (f x : post)
    _ -> xs
-- | The Eigen CG solver (same call as the C++: ConjugateGradient with
-- tolerance, solveWithGuess).
{-# NOINLINE cgDumpRef #-}
cgDumpRef :: IORef Bool
cgDumpRef = unsafePerformIO (newIORef True)

solveEqSys :: Double -> EqSys -> [Double] -> [Double]
solveEqSys tol (EqSys cols rhs) guess =
    let colsL = F.toList cols
        rhsL = F.toList rhs
        colptr = scanl (\acc c -> acc + length c) 0 colsL
        (rows, vals) = unzip [(r, v) | c <- colsL, (r, v) <- c]
     in unsafePerformIO $ do
            first <- readIORef cgDumpRef
            writeIORef cgDumpRef False
            when first $ do
                want <- lookupEnv "LP_DUMP_CG_INPUT"
                case want of
                    Just _ -> writeFile "/tmp/hs_cg_input.txt" (show (colptr, rows, vals, rhsL, guess))
                    Nothing -> pure ()
            withArray (map fromIntegral colptr) $ \cp ->
                withArray (map fromIntegral rows) $ \rp ->
                    withArray (map realToFrac vals) $ \vp ->
                        withArray (map realToFrac rhsL) $ \bp ->
                            withArray (map realToFrac guess) $ \gp -> do
                                out <- mallocArray (length colsL)
                                _ <- c_lpSolveCg (fromIntegral (length colsL)) cp rp vp bp gp (realToFrac tol) out
                                map realToFrac <$> peekArray (length colsL) out

foreign import ccall unsafe "lp_solve_cg"
    c_lpSolveCg :: CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CDouble -> Ptr CDouble -> IO CInt

foreign import ccall unsafe "lp_sort_indices"
    c_lpSortIndices :: CInt -> Ptr CDouble -> Ptr CInt -> IO ()

-- | Sort a list by a key, reproducing the C++ @std::sort@ (unstable
-- introsort) tie order. The pure @sortBy@ is stable, so cells with equal
-- raw positions come out in a different order than nextpnr's @std::sort@,
-- shifting them across bin boundaries downstream.
sortByRaw :: (a -> Double) -> [a] -> [a]
sortByRaw keyOf xs =
    let n = length xs
        arr = V.fromList xs
        keys = map (realToFrac . keyOf) xs
     in unsafePerformIO $
            withArray keys $ \rp -> do
                op <- mallocArray n
                c_lpSortIndices (fromIntegral n) rp op
                idxs <- peekArray n op
                pure [arr V.! fromIntegral i | i <- idxs]

{-# NOINLINE arcDbgRef #-}
arcDbgRef :: IORef Int
arcDbgRef = unsafePerformIO (newIORef (0 :: Int))

-- | @build_equations@ for one axis. @udata@ maps solve cells to rows;
-- any other cell is @dont_solve@.
buildEquations ::
    (IdString -> String) ->
    (IdString -> IdString -> Float) ->
    Bool ->
    Int ->
    Design BelId WireId PipId ->
    M.Map IdString CellLoc ->
    [IdString] ->
    M.Map IdString Int ->
    Bool ->
    EqSys
buildEquations nm critOf dumpArcs anchorIter d locs solveCells udata yaxis =
    let nets = netsIter d
        (es0, nPass) = foldl netEq (emptyEqSys (length solveCells), 0 :: Int) nets
        -- the legal-position anchor term: weight = alpha*iter /
        -- max(1, |l_pos - c_pos|), added only after the first main
        -- iteration (the C++ passes iter == -1 for the initial solve)
        es =
            if anchorIter == 0
                then es0
                else
                    foldl'
                        ( \esA (row, c) ->
                            let l = locOf c
                                lPos = if yaxis then plcLegalY l else plcLegalX l
                                cPos = if yaxis then plcY l else plcX l
                                w = realToFrac ((0.1 :: Float) * fromIntegral anchorIter) / max 1 (fromIntegral (abs (lPos - cPos)))
                             in addRhs (addCoeff esA row row w) row (w * fromIntegral lPos)
                        )
                        es0
                        (zip [0 ..] solveCells)
        dbg =
            unsafePerformIO $ do
                want <- lookupEnv "LP_DUMP_MATRIX"
                case want of
                    Just _ -> do
                        writeFile "/tmp/hs_net_order.txt" (unlines (map (show . unIdString . netName) nets))
                        hPutStrLn stderr ("LPDBG nets " ++ show (length nets) ++ " passed " ++ show nPass)
                    Nothing -> pure ()
     in dbg `seq` es
  where
    locOf c = M.findWithDefault (CellLoc 0 0 0 0 0 0 False False) c locs
    cellPos c = let l = locOf c in if yaxis then plcY l else plcX l
    -- ports of a net in the C++ foreach_port order: driver first (no
    -- user idx), then users in slot order (with idx)
    portsOf ni =
        (case prCell (netDriver ni) of
            Just drv -> [(drv, prPort (netDriver ni), False)]
            Nothing -> [])
            ++ [ (uc, prPort u, True)
               | Just u <- V.toList (netUsers ni)
               , Just uc <- [prCell u]
               ]
    numUsers ni = length [() | Just _ <- V.toList (netUsers ni)]
    clusterOff c =
        case M.lookup c (designCells d) of
            Just ci
                | cellCluster ci /= emptyId -> Just (cellConstrX ci, cellConstrY ci)
            _ -> Nothing
    netEq (es, n) ni
        | prCell (netDriver ni) == Nothing = (es, n)
        | numUsers ni == 0 = (es, n)
        | plcGlobal (locOf (fromMaybe emptyId (prCell (netDriver ni)))) = (es, n)
        | otherwise =
            let ports = portsOf ni
                pos p = cellPos (fst3 p)
                -- strict comparisons keep the FIRST min/max occurrence
                lbp = foldl1 (\a b -> if pos b < pos a then b else a) ports
                ubp = foldl1 (\a b -> if pos b > pos a then b else a) ports
                stamp es' (vc, _, _) (ec, _, _) w =
                    case M.lookup ec udata of
                        Nothing -> es'
                        Just row ->
                            let vPos = fromIntegral (cellPos vc)
                                es1 =
                                    case M.lookup vc udata of
                                        Just vcol -> addCoeff es' row vcol w
                                        Nothing -> addRhs es' row (-vPos * w)
                                es2 =
                                    case clusterOff vc of
                                        Just (ox, oy) ->
                                            let o = if yaxis then fromIntegral oy else fromIntegral ox
                                             in addRhs es1 row (-o * w)
                                        Nothing -> es1
                             in es2
                processArc es' p other =
                    if fst3 p == fst3 other && snd3 p == snd3 other
                        then es'
                        else
                            let oPos = fromIntegral (cellPos (fst3 other))
                                thisPos = fromIntegral (cellPos (fst3 p))
                                base =
                                    1.0
                                        / ( fromIntegral (numUsers ni)
                                                * max 1 ((if yaxis then 1 else 1) * abs (oPos - thisPos))
                                          )
                                _arcDbg =
                                    unsafePerformIO $ do
                                        want <- lookupEnv "LP_DUMP_ARC"
                                        case want of
                                            Just _ ->
                                                when dumpArcs $
                                                    hPutStrLn stderr ("LPDBG arc " ++ nm (fst3 other) ++ "." ++ nm (snd3 other) ++ " -> " ++ nm (fst3 p) ++ "." ++ nm (snd3 p) ++ " o=" ++ show (round oPos) ++ " t=" ++ show (round thisPos) ++ " w=" ++ show base ++ " y=" ++ show yaxis)
                                            Nothing -> pure ()
                                -- cfg.timingWeight * pow(crit, cfg.criticalityExponent),
                                -- ECP5: timingWeight = 10, criticalityExponent = 4;
                                -- user ports only (the C++ checks user_idx)
                                w =
                                    if thd3 p
                                        then base * (1.0 + realToFrac ((10.0 :: Float) * ((critOf (fst3 p) (snd3 p)) ** (4.0 :: Float))))
                                        else base

                             in _arcDbg `seq` stamp (stamp (stamp (stamp es' p p w) p other (-w)) other other w) other p (-w)
                arcEq es' p = processArc (processArc es' p lbp) p ubp
             in (foldl arcEq es ports, n + 1)
    fst3 (c, _, _) = c
    snd3 (_, p, _) = p
    thd3 (_, _, b) = b

-- | @solve_equations@ for one axis: solve, then write the positions
-- back (truncation toward zero + clamp).
solveAxis ::
    Design BelId WireId PipId ->
    Int ->
    Int ->
    M.Map IdString CellLoc ->
    [IdString] ->
    EqSys ->
    Bool ->
    M.Map IdString CellLoc
solveAxis d maxX maxY locs solveCells es yaxis =
    let locOf c = M.findWithDefault (CellLoc 0 0 0 0 0 0 False False) c locs
        guess = [fromIntegral (if yaxis then plcY (locOf c) else plcX (locOf c)) | c <- solveCells]
        out = solveEqSys (realToFrac (1e-5 :: Float)) es guess
        _dbg =
            unsafePerformIO $ do
                want <- lookupEnv "LP_DUMP_SOLVE"
                case want of
                    Just _ -> hPutStrLn stderr ("LPDBG solve y=" ++ show yaxis ++ " out=" ++ show (take 6 out) ++ " guess=" ++ show (take 6 guess))
                    Nothing -> pure ()
        go acc (c, v) =
            let l = locOf c
             in if yaxis
                    then M.insert c l{plcRawY = v, plcY = max 0 (min maxY (truncate v))} acc
                    else M.insert c l{plcRawX = v, plcX = max 0 (min maxX (truncate v))} acc
     in _dbg `seq` foldl go locs (zip solveCells out)

-- | The 4 initial solve iterations of @place()@ (build + solve each
-- axis 5 times, then chain-update and report).
placeHeapInitialIters ::
    Ecp5 ->
    (T.Text -> Maybe IdString) ->
    Design BelId WireId PipId ->
    PlacerState ->
    IO PlacerState
placeHeapInitialIters e cidOf d ps0 = do
    let db = ecp5TimingDb e
        ai = assignArchInfo cidOf d
        isGlobNet ni = maybe False (const True) (M.lookup (fromMaybe emptyId (cidOf "ECP5_IS_GLOBAL")) (netAttrs ni))
        tmg = buildTimingAnalyser e (getPortTimingClassAi db ai) (getPortClockingInfoAi db ai) (getCellDelayAi db ai) isGlobNet True d
        critOf cell port = criticalityOf tmg (CellPortKey cell port)
    go critOf ps0 (0 :: Int)
  where
    go critOf ps i
        | i >= 4 = pure ps
        | otherwise = do
            let (udata, solveCells) = setupSolveCells d (psPlaceCells ps) Nothing
                solveDirection yaxis pAcc =
                    let step (nStep, pAcc') _ =
                            let es = buildEquations (\n -> T.unpack (idToText (ecp5IdTable e) n)) critOf (i == 0 && nStep == 1) 0 d (psLocs pAcc') solveCells udata yaxis
                                dbg =
                                    if i == 0 && not yaxis && nStep <= 2
                                        then
                                            let ls =
                                                    [ show r ++ " " ++ show c ++ " " ++ show v
                                                    | (c, col) <- zip [0 ..] (F.toList (esCols es))
                                                    , (r, v) <- col
                                                    ]
                                                        ++ ["RHS " ++ show r ++ " " ++ show v | (r, v) <- zip [0 ..] (F.toList (esRhs es))]
                                             in unsafePerformIO $ do
                                                    want <- lookupEnv "LP_DUMP_MATRIX"
                                                    case want of
                                                        Just _ -> writeFile ("/tmp/hs_matrix" ++ show nStep ++ ".txt") (unlines ls)
                                                        Nothing -> pure ()
                                        else ()
                             in dbg `seq` ( nStep + 1
                                , pAcc'{psLocs = solveAxis d (psMaxX pAcc') (psMaxY pAcc') (psLocs pAcc') solveCells es yaxis}
                                )
                     in snd (foldl step (1, pAcc) [1 .. 5])
                ps1 = solveDirection False ps
                ps2 = solveDirection True ps1
                ps3 = ps2{psLocs = updateAllChains d (psLocs ps2) solveCells (psMaxX ps2) (psMaxY ps2)}
                hpwl = totalHpwl d (psLocs ps3)
            hPutStrLn stderr ("    at initial placer iter " ++ show i ++ ", wirelen = " ++ show hpwl)
            go critOf ps3 (i + 1)

-- ---------------------------------------------------------------------------
-- Main HeAP loop (placer_heap.cc: place() after the initial iterations)
-- ---------------------------------------------------------------------------

-- | @setup_solve_cells@: assign row ids to the cells being solved
-- (optionally filtered to a set of bel buckets); cluster children
-- inherit the root's row. Returns the udata map and the solve cell list.
setupSolveCells ::
    Design BelId WireId PipId ->
    [IdString] ->
    Maybe [IdString] ->
    (M.Map IdString Int, [IdString])
setupSolveCells d placeCells mbuckets =
    let inBuckets ci = maybe True (\bs -> cellType ci `elem` bs) mbuckets
        solvable =
            [ c
            | c <- placeCells
            , Just ci <- [M.lookup c (designCells d)]
            , inBuckets ci
            ]
        udata0 = M.fromList (zip solvable [0 ..])
        udata =
            udata0
                `M.union` M.fromList
                    [ (cellName ci, row)
                    | (root, row) <- M.toList udata0
                    , ci <- cellsIter d
                    , cellCluster ci == root
                    , cellName ci /= root
                    ]
     in (udata, solvable)

-- | @update_all_chains@'s chain_size map: root cell -> macro size
-- (1 + number of cluster children).
chainSizes :: Design BelId WireId PipId -> [IdString] -> M.Map IdString Int
chainSizes d placeCells =
    foldl' (\m root -> M.insert root (1 + length [() | ci <- cellsIter d, cellCluster ci == root, cellName ci /= root]) m) M.empty placeCells

-- | The @FastBels@ per cell type: bels grouped by (x, y) tile in
-- @getBels@ order, restricted to available bels valid for the type.
-- @fbXSize@ mirrors @fb->size()@ (max x + 1), @fbRowSize x@ mirrors
-- @fb->at(x).size()@ (max y + 1 in that column).
type FastBelsT = (Int, M.Map Int (Int, M.Map Int [BelId]))

buildFastBelsT :: Ecp5 -> IdString -> FastBelsT
buildFastBelsT e t =
    let bels = [b | b <- getBels e, checkBelAvail e b, isValidBelForCellType e t b]
        rows =
            foldl'
                ( \m b ->
                    let Loc x y _ = getBelLocation e b
                     in M.insertWith (\nr o -> M.insertWith (flip (++)) y [b] o) x (M.singleton y [b]) m
                )
                M.empty
                bels
        xSize = maybe 0 ((+ 1) . fst) (M.lookupMax rows)
        rowsSized = M.map (\m -> (maybe 0 ((+ 1) . fst) (M.lookupMax m), m)) rows
     in (xSize, rowsSized)

fbXSize :: FastBelsT -> Int
fbXSize = fst

fbRowSize :: FastBelsT -> Int -> Int
fbRowSize (_, rows) x = maybe 0 fst (M.lookup x rows)

fbAt :: FastBelsT -> Int -> Int -> [BelId]
fbAt (_, rows) x y = maybe [] (M.findWithDefault [] y . snd) (M.lookup x rows)

-- | @place()@ main loop: alternating solve / spread / strict-legalise
-- iterations until stalled or HPWL diverges, then apply the saved best
-- solution.
-- | Debug: measure the CPU time to force a thunk to WHNF (phase timing).
timePhase :: String -> Int -> a -> a
timePhase label it x =
    unsafePerformIO $ do
        t0 <- getCPUTime
        let r = x
        r `seq` do
            t1 <- getCPUTime
            hPutStrLn stderr ("LPTIME it=" ++ show it ++ " " ++ label ++ " " ++ show (((fromIntegral (t1 - t0)) :: Double) / 1e12) ++ "s")
            pure r

placeHeapMain ::
    Ecp5 ->
    (T.Text -> Maybe IdString) ->
    Design BelId WireId PipId ->
    PlacerState ->
    IO (Ecp5, Design BelId WireId PipId, PlacerState)
placeHeapMain e cidOf d0 ps0 = do
    let db = ecp5TimingDb e
        ai = assignArchInfo cidOf d0
        nm n = T.unpack (idToText (ecp5IdTable e) n)
        cidT x = fromMaybe emptyId (cidOf x)
        isGlobalNet ni = maybe False (const True) (M.lookup (cidT "ECP5_IS_GLOBAL") (netAttrs ni))
        tmg0 = buildTimingAnalyser e (getPortTimingClassAi db ai) (getPortClockingInfoAi db ai) (getCellDelayAi db ai) isGlobalNet True d0
        -- ECP5 PlacerHeapCfg: cellGroups are pools of bel buckets
        -- (pool iteration order = reverse insertion)
        groupPools =
            [ reverse [cidT "MULT18X18D", cidT "ALU54B"]
            , reverse [cidT "TRELLIS_COMB", cidT "TRELLIS_FF", cidT "TRELLIS_RAMW"]
            ]
        -- heap_runs: placeAllAtOnce -> a single run with every bucket
        -- encountered in place-cell order (pool order = reverse)
        runTypes =
            reverse
                ( foldl
                    (\ts c -> if c `elem` ts then ts else ts ++ [c])
                    []
                    [ cellType (M.findWithDefault (error "run cell") c (designCells d0))
                    | c <- psPlaceCells ps0
                    ]
                )
        fbMap = M.fromList [(cellType ci, buildFastBelsT e (cellType ci)) | ci <- cellsIter d0]
        chainMap = chainSizes d0 (psPlaceCells ps0)
        numCells = length (designCellOrder d0)
        timeout = max 10000 (numCells * numCells `div` 8)
        cellOf d c = M.findWithDefault (error "main cell") c (designCells d)
        locOf locs c = M.findWithDefault (CellLoc 0 0 0 0 0 0 False False) c locs
        solveBothAxes anchorIt dA psA solveCells udata tmgA =
            let critOfA c p = criticalityOf tmgA (CellPortKey c p)
                stepAxis yaxis locsA =
                    snd
                        ( foldl'
                            ( \(n, l) _ ->
                                ( n + 1
                                , solveAxis dA (psMaxX psA) (psMaxY psA) l solveCells (buildEquations nm critOfA False anchorIt dA l solveCells udata yaxis) yaxis
                                )
                            )
                            (1 :: Int, locsA)
                            [1 .. 5 :: Int]
                        )
             in stepAxis True (stepAxis False (psLocs psA))
        -- apply the saved best solution: unbind everything in it, then
        -- rebind with the saved strengths
        restore eA dA psA solnA =
            let (eU, dU) =
                    foldl'
                        ( \(eX, dX) (c, _mb, _) ->
                            case M.lookup c (designCells dX) >>= cellBel of
                                Nothing -> (eX, dX)
                                Just curBel -> let (bs, d') = unbindBel c curBel (ecp5Bind eX) dX in (setEcp5Bind bs eX, d')
                        )
                        (eA, dA)
                        solnA
                (eB, dB) =
                    foldl'
                        ( \(eX, dX) (c, mb, st) ->
                            case mb of
                                Nothing -> (eX, dX)
                                Just bel -> let (bs, d') = bindBelLut (combCtxOf eX) (ecp5Chipdb eX) c bel st (ecp5Bind eX) dX in (setEcp5Bind bs eX, d')
                        )
                        (eU, dU)
                        solnA
                unbound =
                    [ (nm (cellName ci), nm (cellType ci))
                    | ci <- cellsIter dB
                    , cellBel ci == Nothing
                    ]
                mismatched =
                    [ (nm (cellName ci), nm (cellType ci))
                    | ci <- cellsIter dB
                    , Just bel <- [cellBel ci]
                    , boundBelCell bel (ecp5Bind eB) /= Just (cellName ci)
                    ]
             in if not (null unbound) || not (null mismatched)
                    then error ("placement failed: unbound " ++ show (take 10 unbound) ++ " mismatched " ++ show (take 10 mismatched))
                    else pure (eB, dB, psA)
        go (eA, dA, psA, tmgA, bestA, stalledA, iterA, solvedA, legalA, solnA)
            | stalledA >= 5 || (fromIntegral solvedA :: Double) > fromIntegral legalA * 0.8 = do
                hPutStrLn stderr ("LPDBG main done iter=" ++ show iterA ++ " stalled=" ++ show stalledA ++ " solved=" ++ show solvedA ++ " legal=" ++ show legalA)
                restore eA dA psA solnA
            | otherwise =
                let (udata, solveCells) = setupSolveCells dA (psPlaceCells psA) Nothing
                 in if null solveCells
                        then restore eA dA psA solnA
                        else
                            let anchorIt = if iterA == 0 then 0 else iterA
                                locsS = timePhase "solve" iterA (solveBothAxes anchorIt dA psA solveCells udata tmgA)
                                _dbgSolve =
                                    if iterA == 4
                                        then
                                            unsafePerformIO $
                                                writeFile "/tmp/hs_solve_dump.txt" (unlines [T.unpack (idToText (ecp5IdTable eA) c) ++ " " ++ show (plcRawX l) ++ " " ++ show (plcRawY l) | (c, l) <- M.toList locsS])
                                        else ()
                                locsS1 = timePhase "chain1" iterA (updateAllChains dA locsS (psPlaceCells psA) (psMaxX psA) (psMaxY psA))
                                solvedHpwl = _dbgSolve `seq` totalHpwl dA locsS1
                                locsS2 = timePhase "chain2" iterA (updateAllChains dA locsS1 (psPlaceCells psA) (psMaxX psA) (psMaxY psA))
                                locsSpr = timePhase "spread" iterA (runSpreaders eA dA fbMap groupPools runTypes solveCells (psPlaceCells psA) chainMap (psMaxX psA) (psMaxY psA) locsS2)
                                locsSpr1 = timePhase "chain3" iterA (updateAllChains dA locsSpr (psPlaceCells psA) (psMaxX psA) (psMaxY psA))
                                spreadHpwl = totalHpwl dA locsSpr1
                                _dbgSp =
                                    if iterA == 4
                                        then
                                            unsafePerformIO $
                                                writeFile
                                                    "/tmp/hs_spread_dump.txt"
                                                    ( unlines
                                                        [ T.unpack (idToText (ecp5IdTable eA) c) ++ " " ++ show (plcX l) ++ " " ++ show (plcY l) ++ " " ++ show (plcRawX l) ++ " " ++ show (plcRawY l)
                                                        | (c, l) <- M.toList locsSpr1
                                                        ]
                                                    )
                                        else ()
                                legRes = strictLegalise ai eA cidOf dA fbMap chainMap (psMaxX psA) (psMaxY psA) locsSpr1 solveCells udata timeout (psRng psA)
                                (eL, dL, locsL0, rngL) = legRes
                                locsL = timePhase "legalise" iterA locsL0
                                locsL1 = timePhase "chain4" iterA (updateAllChains dL locsL (psPlaceCells psA) (psMaxX psA) (psMaxY psA))
                                legalHpwl = _dbgSp `seq` totalHpwl dL locsL1
                                tmgL = timePhase "timing" iterA (runTimingAnalyser eL isGlobalNet True tmgA dL)
                                _dbgCrit =
                                    unsafePerformIO $ do
                                        let cs = [ppWorstCrit pd | pd <- M.elems (taPorts tmgL), ppWorstCrit pd > 0]
                                            cnt = length cs
                                            mx = maximum (0 : cs)
                                            sm = sum (map (\x -> realToFrac x :: Double) cs)
                                        appendFile "/tmp/hs_crit.txt" (show (iterA + 1) ++ " " ++ show cnt ++ " " ++ show sm ++ " " ++ show mx ++ "\n")
                                        let big =
                                                [ ( T.unpack (idToText (ecp5IdTable e) c) ++ "." ++ T.unpack (idToText (ecp5IdTable e) p)
                                                  , ppWorstCrit pd
                                                  )
                                                | (CellPortKey c p, pd) <- M.toList (taPorts tmgL)
                                                , ppWorstCrit pd > 0.5
                                                ]
                                            sorted = sortBy (\(a, _) (b, _) -> compare a b) big
                                        when (iterA == 1) $ writeFile "/tmp/hs_crit_ports.txt" (unlines [n ++ " " ++ show c | (n, c) <- sorted])
                                _dbgPort =
                                    unsafePerformIO $
                                        if iterA /= 1
                                            then pure ()
                                            else do
                                                let matches =
                                                        [ (c, p, pd)
                                                        | (CellPortKey c p, pd) <- M.toList (taPorts tmgL)
                                                        , T.unpack (idToText (ecp5IdTable e) c) == "$nextpnr_CCU2C_12$CCU2_COMB0"
                                                        , T.unpack (idToText (ecp5IdTable e) p) == "A"
                                                        ]
                                                case matches of
                                                    [] -> writeFile "/tmp/hs_port_dump.txt" "NOT FOUND\n"
                                                    ((c, _, pd) : _) -> do
                                                        let routeLine = "route=" ++ show (dpMin (ppRouteDelay pd)) ++ "/" ++ show (dpMax (ppRouteDelay pd))
                                                            arrLines = ["arr[" ++ show k ++ "]=" ++ show (dpMin (artValue v)) ++ "/" ++ show (dpMax (artValue v)) | (k, v) <- M.toList (ppArrival pd)]
                                                            reqLines = ["req[" ++ show k ++ "]=" ++ show (dpMin (artValue v)) ++ "/" ++ show (dpMax (artValue v)) | (k, v) <- M.toList (ppRequired pd)]
                                                            arcLines = ["arc[" ++ show t ++ "] other=" ++ T.unpack (idToText (ecp5IdTable e) o) ++ " dq=" ++ show (dpMin (dqRise q)) ++ "/" ++ show (dpMax (dqRise q)) ++ "/" ++ show (dpMin (dqFall q)) ++ "/" ++ show (dpMax (dqFall q)) | CellArc t o q _ <- ppArcs pd]
                                                            dpLines = ["dp[" ++ show k ++ "] slack=" ++ show (pdpSetupSlack v) ++ " crit=" ++ show (pdpCriticality v) | (k, v) <- M.toList (ppDomainPairs pd)]
                                                            fcoId = fromMaybe emptyId (cidOf "FCO")
                                                            fcoNets = [ni | (_, ni) <- M.toList (designNets dL), prCell (netDriver ni) == Just c, prPort (netDriver ni) == fcoId]
                                                            fcoLines = concatMap fcoNetLines fcoNets
                                                            locStr (Loc x y z) = show x ++ "," ++ show y ++ "," ++ show z
                                                            fcoNetLines ni =
                                                                let drvLoc = case prCell (netDriver ni) >>= \dc -> M.lookup dc (designCells dL) >>= cellBel of
                                                                        Just b -> locStr (getBelLocation e b)
                                                                        Nothing -> "?"
                                                                 in ("fco_net=" ++ nm (netName ni) ++ " driver_bel=" ++ drvLoc) : concatMap fcoUserLines [u | Just u <- V.toList (netUsers ni)]
                                                            fcoUserLines u =
                                                                let uc = fromMaybe emptyId (prCell u)
                                                                    up = prPort u
                                                                    locS = case prCell u >>= \x -> M.lookup x (designCells dL) >>= cellBel of
                                                                        Just b -> locStr (getBelLocation e b)
                                                                        Nothing -> "?"
                                                                    sink = M.lookup (CellPortKey uc up) (taPorts tmgL)
                                                                    rtLine = case sink of
                                                                        Just ps -> "    route=" ++ show (dpMin (ppRouteDelay ps)) ++ "/" ++ show (dpMax (ppRouteDelay ps))
                                                                        Nothing -> "    route=?"
                                                                    setupLines = case sink of
                                                                        Just ps -> ["    setup other=" ++ nm o ++ " dq=" ++ show (dpMin (dqRise q)) ++ "/" ++ show (dpMax (dqRise q)) ++ "/" ++ show (dpMin (dqFall q)) ++ "/" ++ show (dpMax (dqFall q)) | CellArc ArcSetup o q _ <- ppArcs ps]
                                                                        Nothing -> []
                                                                 in ("  user=" ++ nm uc ++ "." ++ nm up ++ " bel=" ++ locS) : rtLine : setupLines
                                                        writeFile "/tmp/hs_port_dump.txt" (unlines (routeLine : arrLines ++ reqLines ++ arcLines ++ dpLines ++ fcoLines))
                                (bestN, stalledN, solnN) =
                                    if legalHpwl < bestA
                                        then (legalHpwl, 0, [(cellName ci, cellBel ci, cellBelStrength ci) | ci <- cellsIter dL])
                                        else (bestA, stalledA + 1, solnA)
                                locsFinal = M.map (\l -> l{plcLegalX = plcX l, plcLegalY = plcY l}) locsL1
                                psN = psA{psLocs = locsFinal, psRng = rngL}
                             in do
                                    _dbgCrit `seq` _dbgPort `seq` hPutStrLn stderr ("    at iteration #" ++ show (iterA + 1) ++ ", type ALL: wirelen solved = " ++ show solvedHpwl ++ ", spread = " ++ show spreadHpwl ++ ", legal = " ++ show legalHpwl ++ ".")
                                    go (eL, dL, psN, tmgL, bestN, stalledN, iterA + 1, solvedHpwl, legalHpwl, solnN)
    hPutStrLn stderr ("Running main analytical placer, max placement attempts per cell = " ++ show timeout ++ ".")
    go (e, d0, ps0, tmg0, maxBound :: Int, 0, 0, 0, 0, [])


-- | The CutSpreader runs of one iteration: each cell group first, then
-- each run bucket not covered by any group (single-type spreader).
runSpreaders ::
    Ecp5 ->
    Design BelId WireId PipId ->
    M.Map IdString FastBelsT ->
    [[IdString]] ->
    [IdString] ->
    [IdString] ->
    [IdString] ->
    M.Map IdString Int ->
    Int ->
    Int ->
    M.Map IdString CellLoc ->
    M.Map IdString CellLoc
runSpreaders e d fbMap groupPools runTypes solveCells placeCells chainMap maxX maxY locs =
    foldl' (\l t -> cutSpread e d fbMap [t] solveCells placeCells chainMap maxX maxY l) locsGroups singles
  where
    locsGroups = foldl' (\l grp -> cutSpread e d fbMap grp solveCells placeCells chainMap maxX maxY l) locs groupPools
    inGroup t = any (elem t) groupPools
    singles = [t | t <- runTypes, not (inGroup t)]

-- | @SpreaderRegion@: a rectangular region with per-type cell and bel
-- counts (indexed by the spreader's bucket order).
data SpreaderRegion = SpreaderRegion
    { srId :: !Int
    , srX0 :: !Int
    , srY0 :: !Int
    , srX1 :: !Int
    , srY1 :: !Int
    , srCells :: ![Int]
    , srBels :: ![Int]
    }
    deriving (Show)

data SprState = SprState
    { ssGroups :: !(M.Map (Int, Int) Int)
    , ssRegions :: !(Seq SpreaderRegion)
    , ssMerged :: !(S.Set Int)
    }

-- | @CutSpreader::run@: init, find overused regions, expand them to
-- the beta density, then recursively cut-spread each region.
cutSpread ::
    Ecp5 ->
    Design BelId WireId PipId ->
    M.Map IdString FastBelsT ->
    [IdString] ->
    [IdString] ->
    [IdString] ->
    M.Map IdString Int ->
    Int ->
    Int ->
    M.Map IdString CellLoc ->
    M.Map IdString CellLoc
cutSpread e d fbMap buckets solveCells placeCells chainMap maxX maxY locs0 =
    loopQueue stAfterExpand locs0 cellsAtLoc workQueue0
  where
    nb = length buckets
    typeIdx = M.fromList (zip buckets [0 ..])
    cellOf c = M.findWithDefault (error "spread cell") c (designCells d)
    locOf c = M.findWithDefault (CellLoc 0 0 0 0 0 0 False False) c locs0
    fbs = [(t, M.findWithDefault (buildFastBelsT e t) t fbMap) | t <- buckets]
    fbOf t = maybe (error "spread fb") id (lookup t fbs)
    isFixed ci = not (cellType ci `elem` buckets)
    fixedAt x y t = maybe 0 (\m -> M.findWithDefault 0 t m) (M.lookup (x, y) focc)
    occAt x y t = maybe 0 (\m -> M.findWithDefault 0 t m) (M.lookup (x, y) occ)
    belsAt x y t =
        let (xSize, rows) = fbOf (buckets !! t)
         in if x >= xSize
                then 0
                else case M.lookup x rows of
                    Nothing -> 0
                    Just (ySize, m) -> if y >= ySize then 0 else max 0 (length (M.findWithDefault [] y m) - fixedAt x y t)
    -- init(): occupancy / fixed_occupancy / cell extents over cell_locs
    occ :: M.Map (Int, Int) (M.Map Int Int)
    focc :: M.Map (Int, Int) (M.Map Int Int)
    extents :: M.Map IdString (Int, Int, Int, Int)
    bumpTile :: M.Map (Int, Int) (M.Map Int Int) -> (Int, Int) -> Int -> M.Map (Int, Int) (M.Map Int Int)
    bumpTile m (x, y) t = M.alter (\mb -> Just (M.insertWith (+) t (1 :: Int) (maybe M.empty id mb))) (x, y) m
    stepOcc (o, fo, ext) (cname, cl) =
        let ci = cellOf cname
         in if isFixed ci
                then (o, fo, ext)
                else
                    case cellBelStrength ci of
                        st | st > StrengthStrong -> (o, fo, ext)
                        _ ->
                            let ext' =
                                    if cellCluster ci /= emptyId
                                        then
                                            let root = cellCluster ci
                                                (x0, y0, x1, y1) = M.findWithDefault (plcX cl, plcY cl, plcX cl, plcY cl) root ext
                                             in M.insert root (min x0 (plcX cl), min y0 (plcY cl), max x1 (plcX cl), max y1 (plcY cl)) ext
                                        else ext
                                t = typeIdx M.! cellType ci
                             in if cellCluster ci /= emptyId && isFixed (cellOf (cellCluster ci))
                                    then (o, bumpTile fo (plcX cl, plcY cl) t, ext')
                                    else (bumpTile o (plcX cl, plcY cl) t, fo, ext')
    (occ, focc, extents) = foldl' stepOcc (M.empty, M.empty, M.empty) (M.toList locs0)
    _dbgOcc =
        if nb == 3
            then
                unsafePerformIO $
                    appendFile
                        "/tmp/hs_occ_dump.txt"
                        ( unlines
                            [ show (x, y) ++ " occ=" ++ show [occAt x y t | t <- [0 .. nb - 1]] ++ " focc=" ++ show [fixedAt x y t | t <- [0 .. nb - 1]] ++ " bels=" ++ show [belsAt x y t | t <- [0 .. nb - 1]]
                            | (x, y) <- [(3, 2), (4, 2), (3, 3), (3, 4), (4, 4)]
                            ]
                        )
            else ()
    -- init(): chain extents -> chaines at each cell's location
    chaines =
        foldl'
            ( \ch (cname, cl) ->
                let ci = cellOf cname
                 in if isFixed ci || cellBelStrength ci > StrengthStrong || cellCluster ci == emptyId
                        then ch
                        else
                            let (ce0, ce1, ce2, ce3) = M.findWithDefault (plcX cl, plcY cl, plcX cl, plcY cl) (cellCluster ci) extents
                                (ex0, ey0, ex1, ey1) = M.findWithDefault (plcX cl, plcY cl, plcX cl, plcY cl) (plcX cl, plcY cl) ch
                             in M.insert (plcX cl, plcY cl) (min ex0 ce0, min ey0 ce1, max ex1 ce2, max ey1 ce3) ch
            )
            (M.fromList [((x, y), (x, y, x, y)) | x <- [0 .. maxX], y <- [0 .. maxY]])
            (M.toList locs0)
    -- init(): cells at location (solve-cell order, unfixed only)
    cellsAtLoc =
        foldl'
            ( \m c ->
                let ci = cellOf c
                 in if isFixed ci
                        then m
                        else let cl = locOf c in M.insertWith (flip (++)) (plcX cl, plcY cl) [c] m
            )
            (M.fromList [((x, y), []) | x <- [0 .. maxX], y <- [0 .. maxY]])
            solveCells
    regionOverused r beta =
        or (zipWith (\cells bels -> if bels < 4 then cells > bels else fromIntegral cells > beta * fromIntegral bels) (srCells r) (srBels r))
    -- grow_region: extend r over the box, merging any region hit
    growRegion st r x0 y0 x1 y1 init' =
        if (x0 >= srX0 r && y0 >= srY0 r && x1 <= srX1 r && y1 <= srY1 r) || init'
            then st
            else
                let oldX0 = srX0 r + if init' then 1 else 0
                    oldY0 = srY0 r
                    oldX1 = srX1 r
                    oldY1 = srY1 r
                    r1 = r{srX0 = min (srX0 r) x0, srY0 = min (srY0 r) y0, srX1 = max (srX1 r) x1, srY1 = max (srY1 r) y1}
                    -- the C++ grow_region mutates the region's bounds in
                    -- place; store them back before the tile processing
                    stB0 = st{ssRegions = Seq.adjust (const r1) (srId r1) (ssRegions st)}
                    processLoc st' x y =
                        let g = M.findWithDefault (-1) (x, y) (ssGroups st')
                            st''
                                | g == -1 =
                                    st'
                                        { ssRegions =
                                            Seq.adjust
                                                ( \rr ->
                                                    rr
                                                        { srCells = zipWith (+) (srCells rr) [occAt x y t | t <- [0 .. nb - 1]]
                                                        , srBels = zipWith (+) (srBels rr) [belsAt x y t | t <- [0 .. nb - 1]]
                                                        }
                                                )
                                                (srId r1)
                                                (ssRegions st')
                                        }
                                | otherwise = st'
                            st''' = if g /= -1 && g /= srId r1 then mergeRegions st'' (Seq.index (ssRegions st'') (srId r1)) (Seq.index (ssRegions st'') g) else st''
                            st'''' = st'''{ssGroups = M.insert (x, y) (srId r1) (ssGroups st''')}
                            (cx0, cy0, cx1, cy1) = M.findWithDefault (x, y, x, y) (x, y) chaines
                         in growRegion st'''' (Seq.index (ssRegions st'''') (srId r1)) cx0 cy0 cx1 cy1 False
                    stA = foldl' (\st' x -> foldl' (\st'' y -> processLoc st'' x y) st' [srY0 r1 .. srY1 r1]) stB0 [srX0 r1 .. oldX0 - 1]
                    stB = foldl' (\st' x -> foldl' (\st'' y -> processLoc st'' x y) st' [srY0 r1 .. srY1 r1]) stA [oldX1 + 1 .. x1]
                    stC = foldl' (\st' y -> foldl' (\st'' x -> processLoc st'' x y) st' [srX0 r1 .. srX1 r1]) stB [srY0 r1 .. oldY0 - 1]
                    stD = foldl' (\st' y -> foldl' (\st'' x -> processLoc st'' x y) st' [srX0 r1 .. srX1 r1]) stC [oldY1 + 1 .. y1]
                 in stD
    mergeRegions st merged mergee =
        let st1 =
                foldl'
                    ( \st' x ->
                        foldl'
                            ( \st'' y ->
                                st''
                                    { ssGroups = M.insert (x, y) (srId merged) (ssGroups st'')
                                    , ssRegions =
                                        Seq.adjust
                                            ( \r ->
                                                r
                                                    { srCells = zipWith (+) (srCells r) [occAt x y t | t <- [0 .. nb - 1]]
                                                    , srBels = zipWith (+) (srBels r) [belsAt x y t | t <- [0 .. nb - 1]]
                                                    }
                                            )
                                            (srId merged)
                                            (ssRegions st'')
                                    }
                            )
                            st'
                            [srY0 mergee .. srY1 mergee]
                    )
                    (st{ssMerged = S.insert (srId mergee) (ssMerged st)})
                    [srX0 mergee .. srX1 mergee]
         in growRegion st1 (Seq.index (ssRegions st1) (srId merged)) (srX0 mergee) (srY0 mergee) (srX1 mergee) (srY1 mergee) False
    -- find_overused_regions: scan tiles in x/y order
    findRegions =
        fst
            ( foldl'
                ( \(st, _) (x, y) ->
                    if M.findWithDefault (-1) (x, y) (ssGroups st) /= -1
                        then (st, ())
                        else
                            let over = any (\t -> occAt x y t > belsAt x y t) [0 .. nb - 1]
                             in if not over
                                    then (st, ())
                                    else
                                        let rid = Seq.length (ssRegions st)
                                            st1 = st{ssGroups = M.insert (x, y) rid (ssGroups st)}
                                            reg = SpreaderRegion rid x y x y [occAt x y t | t <- [0 .. nb - 1]] [belsAt x y t | t <- [0 .. nb - 1]]
                                            st2 = st1{ssRegions = ssRegions st1 Seq.|> reg}
                                            st3 = growRegion st2 reg x y x y True
                                            -- expansion in +x / +y while over-occupied
                                            expand stE =
                                                let rE = Seq.index (ssRegions stE) rid
                                                    overOccX =
                                                        srX1 rE < maxX
                                                            && any (\y1 -> any (\t -> occAt (srX1 rE + 1) y1 t > belsAt (srX1 rE + 1) y1 t) [0 .. nb - 1]) [srY0 rE .. srY1 rE]
                                                    stX = if overOccX then growRegion stE rE (srX0 rE) (srY0 rE) (srX1 rE + 1) (srY1 rE) False else stE
                                                    rX = Seq.index (ssRegions stX) rid
                                                    overOccY =
                                                        srY1 rX < maxY
                                                            && any (\x1 -> any (\t -> occAt x1 (srY1 rX + 1) t > belsAt x1 (srY1 rX + 1) t) [0 .. nb - 1]) [srX0 rX .. srX1 rX]
                                                    stY = if overOccY then growRegion stX rX (srX0 rX) (srY0 rX) (srX1 rX) (srY1 rX + 1) False else stX
                                                 in if overOccX || overOccY then expand stY else stE
                                            st4 = expand st3
                                         in (st4, ())
                )
                (SprState (M.fromList [((x, y), -1) | x <- [0 .. maxX], y <- [0 .. maxY]]) Seq.empty S.empty, ())
                [(x, y) | x <- [0 .. maxX], y <- [0 .. maxY]]
            )
    -- expand_regions: grow overused regions to the beta density
    expandRegions st =
        let beta = 0.75 :: Float
            initQueue = [srId r | r <- F.toList (ssRegions st), not (S.member (srId r) (ssMerged st)) && regionOverused r beta]
            growWhile stW rid =
                let regAt stX = Seq.index (ssRegions stX) rid
                    rW = regAt stW
                    -- one pass of the C++ inner loop: an x-phase grow
                    -- (left then right) and a y-phase grow (up then
                    -- down), each re-checking the beta density. Returns
                    -- the new state and whether any grow actually fired
                    -- (the C++ `changed` flag, guarding the infinite loop
                    -- when a region cannot grow but stays overused).
                    stepOne stS =
                        let overused stX = regionOverused (regAt stX) beta
                            growL = overused stS && srX0 (regAt stS) > 0
                            stX1
                                | growL =
                                    growRegion stS (regAt stS) (srX0 (regAt stS) - 1) (srY0 (regAt stS)) (srX1 (regAt stS)) (srY1 (regAt stS)) False
                                | otherwise = stS
                            growR = overused stX1 && srX1 (regAt stX1) < maxX
                            stX2
                                | growR =
                                    growRegion stX1 (regAt stX1) (srX0 (regAt stX1)) (srY0 (regAt stX1)) (srX1 (regAt stX1) + 1) (srY1 (regAt stX1)) False
                                | otherwise = stX1
                            -- the C++ y-phase grows up unconditionally
                            -- (the overuse break only exits the x loop)
                            growU = srY0 (regAt stX2) > 0
                            stY1
                                | growU =
                                    growRegion stX2 (regAt stX2) (srX0 (regAt stX2)) (srY0 (regAt stX2) - 1) (srX1 (regAt stX2)) (srY1 (regAt stX2)) False
                                | otherwise = stX2
                            -- C++ +y grow fires if y1 < max_y and the
                            -- -y grow did not break out of the y-phase
                            -- (i.e. y0 == 0, or still overused after -y).
                            growD = srY1 (regAt stY1) < maxY && (not growU || overused stY1)
                            stY2
                                | growD =
                                    growRegion stY1 (regAt stY1) (srX0 (regAt stY1)) (srY0 (regAt stY1)) (srX1 (regAt stY1)) (srY1 (regAt stY1) + 1) False
                                | otherwise = stY1
                         in (stY2, growL || growR || growU || growD)
                 in if regionOverused rW beta
                        then
                            let (stN, changed) = stepOne stW
                             in if changed then growWhile stN rid else stW
                        else stW
            loop q st' = case q of
                [] -> st'
                (rid : rest) ->
                    if S.member rid (ssMerged st')
                        then loop rest st'
                        else loop rest (growWhile st' rid)
         in loop initQueue st
    -- cut_region: sort cells, pick a cut, spread by interpolation
    cutRegion st r dir locsQ cellsAtLocQ =
        let calAt x y = M.findWithDefault [] (x, y) cellsAtLocQ
            cutCells = [c | x <- [srX0 r .. srX1 r], y <- [srY0 r .. srY1 r], c <- calAt x y]
            chainOf c = M.findWithDefault 1 c chainMap
            locOfQ c = M.findWithDefault (CellLoc 0 0 0 0 0 0 False False) c locsQ
            rawOf c = let cl = locOfQ c in if dir then plcRawY cl else plcRawX cl
            sortedCells = sortByRaw rawOf cutCells
         in if length sortedCells < 2
                then (st, locsQ, cellsAtLocQ, Nothing)
                else
                    let _dbgTie =
                            if srId r == 1
                                then
                                    let pairs = zip sortedCells (drop 1 sortedCells)
                                        ties = [rawOf a | (a, b) <- pairs, rawOf a == rawOf b]
                                        s = "  ntie=" ++ show (length ties) ++ concat [" " ++ T.unpack (idToText (ecp5IdTable e) a) ++ "=" ++ show v | (a, v) <- take 20 (zip [a | (a, b) <- pairs, rawOf a == rawOf b] ties)] ++ "\n"
                                     in unsafePerformIO (length s `seq` appendFile "/tmp/hs_ties.txt" s)
                                else ()
                        totalCells = sum (map chainOf cutCells)
                        _dbgCut = unsafePerformIO (appendFile "/tmp/hs_regions_log.txt" ("cut r" ++ show (srId r) ++ " (" ++ show (srX0 r) ++ "," ++ show (srY0 r) ++ ")|>(" ++ show (srX1 r) ++ "," ++ show (srY1 r) ++ ") dir=" ++ show dir ++ " ncells=" ++ show (length sortedCells) ++ "\\n"
                            ++ (if srId r == 1 && length sortedCells > 100
                                    then "  sorted20:" ++ concat [" " ++ T.unpack (idToText (ecp5IdTable e) c) ++ "=" ++ show (rawOf c) | c <- take 20 sortedCells] ++ "\\n"
                                    else "")))
                        (_, (_, pivot0)) =
                            foldl
                                ( \(acc, (pc, pi)) c ->
                                    let pc' = pc + chainOf c
                                     in if pc' >= totalCells `div` 2
                                            then (acc, (pc', pi))
                                            else (acc + 1, (pc', pi + 1))
                                )
                                (0 :: Int, (0 :: Int, 0 :: Int))
                                sortedCells
                        pivot = _dbgCut `seq` min pivot0 (length sortedCells - 1)
                        extentOf c =
                            case M.lookup c extents of
                                Just (x0, y0, x1, y1) -> if dir then y1 - y0 + 1 else x1 - x0 + 1
                                Nothing -> 1
                        clearanceL = maximum (0 : [extentOf c | (i, c) <- zip [0 ..] sortedCells, i < pivot])
                        clearanceR = maximum (0 : [extentOf c | (i, c) <- zip [0 ..] sortedCells, i >= pivot])
                        _dbgPiv = unsafePerformIO (appendFile "/tmp/hs_regions_log.txt" ("  pivot=" ++ show pivot ++ " cl=" ++ show clearanceL ++ " cr=" ++ show clearanceR ++ "\n"))
                        trimmedL0 = if dir then srY0 r else srX0 r
                        trimmedR0 = if dir then srY1 r else srX1 r
                        rowHasBels bnd =
                            any
                                ( \i ->
                                    any (\t -> belsAt (if dir then i else bnd) (if dir then bnd else i) t > 0) [0 .. nb - 1]
                                )
                                [(if dir then srX0 r else srY0 r) .. (if dir then srX1 r else srY1 r)]
                        trimmedL = head ([b | b <- [trimmedL0 .. (if dir then srY1 r else srX1 r)], rowHasBels b] ++ [if dir then srY1 r else srX1 r])
                        _dbgColsF = _dbgCols `seq` ()
                        trimmedR = head ([b | b <- reverse [(if dir then srY0 r else srX0 r) .. trimmedR0], rowHasBels b] ++ [if dir then srY0 r else srX0 r])
                        _dbgAU i aU lv rv =
                            if srId r == 0 && nb == 2
                                then
                                    unsafePerformIO
                                        ( appendFile "/tmp/hs_regions_log.txt" ("  au i=" ++ show i ++ " aU=" ++ show aU ++ " lv=" ++ show (lv !! 1) ++ " rv=" ++ show (rv !! 1) ++ " pivot=" ++ show pivot ++ "\n")
                                        )
                                else ()
                        _dbgCols =
                            if srId r == 0 && nb == 2
                                then
                                    unsafePerformIO
                                        ( appendFile
                                            "/tmp/hs_regions_log.txt"
                                            ( "  colbels:"
                                                ++ concat
                                                    [ " " ++ show i ++ "=" ++ show (sum [belsAt (if dir then j else i) (if dir then i else j) 1 | j <- [(if dir then srX0 r else srY0 r) .. (if dir then srX1 r else srY1 r)]])
                                                    | i <- [trimmedL .. trimmedR]
                                                    ]
                                                ++ "\n"
                                            )
                                        )
                                else ()
                     in if (trimmedR - trimmedL + 1) <= max clearanceL clearanceR
                            then _dbgPiv `seq` (st, locsQ, cellsAtLocQ, Nothing)
                            else
                                let cellIdx c = typeIdx M.! cellType (cellOf c)
                                    leftCells0 = [sum [chainOf c | (i, c) <- zip [0 ..] sortedCells, i <= pivot, cellIdx c == t] | t <- [0 .. nb - 1]]
                                    rightCells0 = [sum [chainOf c | (i, c) <- zip [0 ..] sortedCells, i > pivot, cellIdx c == t] | t <- [0 .. nb - 1]]
                                    (bestCut, _) =
                                        foldl
                                            ( \(bestC, bestDU) (i, (lv, rv)) ->
                                                if (i - trimmedL + 1) >= clearanceL && (trimmedR - i + 1) >= clearanceR
                                                    then
                                                        let aU =
                                                                sum
                                                                    [ fromIntegral (leftCells0 !! t + rightCells0 !! t)
                                                                        * abs (fromIntegral (leftCells0 !! t) / fromIntegral (max (lv !! t) 1) - fromIntegral (rightCells0 !! t) / fromIntegral (max (rv !! t) 1))
                                                                    | t <- [0 .. nb - 1]
                                                                    ]
                                                         in if aU < bestDU
                                                                then _dbgAU i aU lv rv `seq` (i, aU)
                                                                else (bestC, bestDU)
                                                    else (bestC, bestDU)
                                            )
                                            (-1, (1.0 / 0.0) :: Double)
                                            ( zip
                                                [trimmedL .. trimmedR]
                                                ( tail
                                                    ( scanl
                                                        ( \(lv, rv) i ->
                                                            let slither = [sum [belsAt (if dir then j else i) (if dir then i else j) t | j <- [(if dir then srX0 r else srY0 r) .. (if dir then srX1 r else srY1 r)]] | t <- [0 .. nb - 1]]
                                                             in (zipWith (+) lv slither, zipWith (-) rv slither)
                                                        )
                                                        (replicate nb 0, [sum [belsAt x y t | x <- [srX0 r .. srX1 r], y <- [srY0 r .. srY1 r]] | t <- [0 .. nb - 1]])
                                                        [trimmedL .. trimmedR]
                                                    )
                                                )
                                            )
                                 in if bestCut == -1
                                        then _dbgPiv `seq` (st, locsQ, cellsAtLocQ, Nothing)
                                        else
                                            let _dbgBc = _dbgCols `seq` unsafePerformIO (appendFile "/tmp/hs_regions_log.txt" ("  trimmed=" ++ show trimmedL ++ ".." ++ show trimmedR ++ " bestcut=" ++ show bestCut ++ " pivot'=" ++ show pivot ++ "\n"))
                                                leftBels = [sum [belsAt x y t | x <- [srX0 r .. (if dir then srX1 r else bestCut)], y <- [srY0 r .. (if dir then bestCut else srY1 r)]] | t <- [0 .. nb - 1]]
                                                rightBels = [sum [belsAt x y t | x <- [(if dir then srX0 r else bestCut + 1) .. srX1 r], y <- [(if dir then bestCut + 1 else srY0 r) .. srY1 r]] | t <- [0 .. nb - 1]]
                                             in if sum leftBels == 0 || sum rightBels == 0
                                                    then _dbgBc `seq` (st, locsQ, cellsAtLocQ, Nothing)
                                                    else
                                                        let isOver rSide lc rc =
                                                                let delta = sum [fromIntegral (lc !! t) / fromIntegral (max (leftBels !! t) 1) - fromIntegral (rc !! t) / fromIntegral (max (rightBels !! t) 1) | t <- [0 .. nb - 1]]
                                                                    _dbgO =
                                                                        if srId r == 1
                                                                            then
                                                                                let s = "  isOver rSide=" ++ show rSide ++ " delta=" ++ show delta ++ " lc=" ++ show lc ++ " rc=" ++ show rc ++ " lb=" ++ show leftBels ++ " rb=" ++ show rightBels ++ "\n"
                                                                                 in unsafePerformIO (length s `seq` appendFile "/tmp/hs_isover.txt" s)
                                                                            else ()
                                                                 in _dbgO `seq` if rSide then delta < 0 else delta > 0
                                                            goLeft (piv, lc, rc)
                                                                | piv > 0 && isOver False lc rc =
                                                                    let sz = chainOf (sortedCells !! piv)
                                                                        t = cellIdx (sortedCells !! piv)
                                                                     in goLeft (piv - 1, updateNth t (subtract sz) lc, updateNth t (+ sz) rc)
                                                                | otherwise = (piv, lc, rc)
                                                            goRight (piv, lc, rc)
                                                                | piv < length sortedCells - 1 && isOver True lc rc =
                                                                    let sz = chainOf (sortedCells !! (piv + 1))
                                                                        t = cellIdx (sortedCells !! piv)
                                                                     in goRight (piv + 1, updateNth t (+ sz) lc, updateNth t (subtract sz) rc)
                                                                | otherwise = (piv, lc, rc)
                                                            (pivot', leftCells, rightCells) = goRight (goLeft (pivot, leftCells0, rightCells0))
                                                            -- spread_binlerp
                                                            spreadBins :: Int -> Int -> Double -> Double -> M.Map IdString CellLoc -> M.Map IdString CellLoc
                                                            spreadBins cellsStart cellsEnd areaL areaR locsIn
                                                                | n <= 2 = foldl' spreadSimple locsIn [cellsStart .. cellsEnd - 1]
                                                                | otherwise = foldl' spreadBin locsIn [0 .. nbins - 1]
                                                                where
                                                                    n = cellsEnd - cellsStart
                                                                    nbins = min n 10
                                                                    bs = (cellsStart, areaL) : [(cellsStart + (n * i) `div` nbins, areaL + ((areaR - areaL + 0.99) * fromIntegral i) / fromIntegral nbins) | i <- [1 .. nbins - 1]] ++ [(cellsEnd, areaR + 0.99)]
                                                                    spreadSimple locsA i0 =
                                                                        let c = sortedCells !! i0
                                                                            pos = areaL + fromIntegral i0 * ((areaR - areaL) / fromIntegral n)
                                                                         in M.adjust (\cl -> if dir then cl{plcRawY = pos} else cl{plcRawX = pos}) c locsA
                                                                    spreadBin locsA i =
                                                                        let (bl, br) = (bs !! i, bs !! (i + 1))
                                                                            origLeft = rawOf (sortedCells !! fst bl)
                                                                            origRight = rawOf (sortedCells !! (fst br - 1))
                                                                            m' = (snd br - snd bl) / max 0.00001 (origRight - origLeft)
                                                                            _dbgBin =
                                                                                if srId r == 1 && i < 3
                                                                                    then
                                                                                        unsafePerformIO
                                                                                            ( appendFile "/tmp/hs_regions_log.txt" ("  bin " ++ show i ++ " bl=(" ++ show (fst bl) ++ "," ++ show (snd bl) ++ ") br=(" ++ show (fst br) ++ "," ++ show (snd br) ++ ") origL=" ++ show origLeft ++ " origR=" ++ show origRight ++ " m=" ++ show m' ++ " first=" ++ T.unpack (idToText (ecp5IdTable e) (sortedCells !! fst bl)) ++ "\n")
                                                                                            )
                                                                                    else ()
                                                                         in _dbgBin `seq` foldl'
                                                                                ( \locsB j ->
                                                                                    let posOld = rawOf (sortedCells !! j)
                                                                                        posNew = snd bl + m' * (posOld - origLeft)
                                                                                     in M.adjust (\cl -> if dir then cl{plcRawY = posNew} else cl{plcRawX = posNew}) (sortedCells !! j) locsB
                                                                                )
                                                                                locsA
                                                                                [fst bl .. fst br - 1]
                                                            locsCut =
                                                                spreadBins 0 (pivot' + 1) (fromIntegral trimmedL) (fromIntegral bestCut) (spreadBins (pivot' + 1) (length sortedCells) (fromIntegral bestCut + 1) (fromIntegral trimmedR) locsQ)
                                                            -- rebuild cells_at_location with clamped positions
                                                            cellsAtLoc' =
                                                                foldl'
                                                                    ( \m c ->
                                                                        let cl = M.findWithDefault (error "cut cell") c locsCut
                                                                            nx' = min (srX1 r) (max (srX0 r) (truncate (plcRawX cl)))
                                                                            ny' = min (srY1 r) (max (srY0 r) (truncate (plcRawY cl)))
                                                                         in M.insertWith (flip (++)) (nx', ny') [c] m
                                                                    )
                                                                    (foldl' (\m (x, y) -> M.insert (x, y) [] m) cellsAtLocQ [(x, y) | x <- [srX0 r .. srX1 r], y <- [srY0 r .. srY1 r]])
                                                                    sortedCells
                                                            rlId = Seq.length (ssRegions st)
                                                            rrId = rlId + 1
                                                            rl = SpreaderRegion rlId (srX0 r) (srY0 r) (if dir then srX1 r else bestCut) (if dir then bestCut else srY1 r) leftCells leftBels
                                                            rr = SpreaderRegion rrId (if dir then srX0 r else bestCut + 1) (if dir then bestCut + 1 else srY0 r) (srX1 r) (srY1 r) rightCells rightBels
                                                            st' =
                                                                st
                                                                    { ssRegions = ssRegions st Seq.|> rl Seq.|> rr
                                                                    , ssGroups =
                                                                        foldl'
                                                                            (\g (x, y) -> M.insert (x, y) rrId g)
                                                                            (foldl' (\g (x, y) -> M.insert (x, y) rlId g) (ssGroups st) [(x, y) | x <- [srX0 rl .. srX1 rl], y <- [srY0 rl .. srY1 rl]])
                                                                            [(x, y) | x <- [srX0 rr .. srX1 rr], y <- [srY0 rr .. srY1 rr]]
                                                                    }
                                                            locsOut = foldl' (\l c -> M.adjust (\lc -> lc{plcX = min (srX1 r) (max (srX0 r) (truncate (plcRawX lc))), plcY = min (srY1 r) (max (srY0 r) (truncate (plcRawY lc)))}) c l) locsCut sortedCells
                                                         in _dbgExp `seq` _dbgTie `seq` _dbgPiv `seq` _dbgBc `seq` (st', locsOut, cellsAtLoc', Just (rlId, rrId))
    stAfterFind =
        let r = findRegions
            _dbgR = unsafePerformIO $ do
                appendFile
                    "/tmp/hs_regions_log.txt"
                    ( "== spreader run buckets=" ++ show (map unIdString buckets) ++ "\n"
                        ++ unlines
                            [ "reg " ++ show (srId reg) ++ " (" ++ show (srX0 reg) ++ "," ++ show (srY0 reg) ++ ")|>(" ++ show (srX1 reg) ++ "," ++ show (srY1 reg) ++ ") cells=" ++ unwords (map show (srCells reg)) ++ " bels=" ++ unwords (map show (srBels reg))
                            | reg <- F.toList (ssRegions r)
                            , not (S.member (srId reg) (ssMerged r))
                            ]
                    )
         in _dbgOcc `seq` _dbgR `seq` r
    stAfterExpand = expandRegions stAfterFind
    _dbgExp =
        if nb == 3
            then
                unsafePerformIO $
                    appendFile
                        "/tmp/hs_exp_dump.txt"
                        ( unlines
                            [ "exp " ++ show (srId reg) ++ " (" ++ show (srX0 reg) ++ "," ++ show (srY0 reg) ++ ")|>(" ++ show (srX1 reg) ++ "," ++ show (srY1 reg) ++ ") cells=" ++ unwords (map show (srCells reg)) ++ " bels=" ++ unwords (map show (srBels reg))
                            | reg <- F.toList (ssRegions stAfterExpand)
                            , not (S.member (srId reg) (ssMerged stAfterExpand))
                            ]
                        )
            else ()
    workQueue0 = [(srId r, False) | r <- F.toList (ssRegions stAfterExpand), not (S.member (srId r) (ssMerged stAfterExpand))]
    loopQueue st locsQ cellsAtLocQ q = case q of
        [] -> locsQ
        ((rid, dir) : rest) ->
            let r = Seq.index (ssRegions st) rid
             in if all (== 0) (srCells r)
                    then loopQueue st locsQ cellsAtLocQ rest
                    else
                        case cutRegion st r dir locsQ cellsAtLocQ of
                            (stA, locsA, calA, Just (idL, idR)) -> loopQueue stA locsA calA (rest ++ [(idL, not dir), (idR, not dir)])
                            (stA, locsA, calA, Nothing) ->
                                case cutRegion stA r (not dir) locsA calA of
                                    (stB, locsB, calB, Just (idL2, idR2)) -> loopQueue stB locsB calB (rest ++ [(idL2, dir), (idR2, dir)])
                                    (stB, locsB, calB, Nothing) -> loopQueue stB locsB calB rest


-- ---------------------------------------------------------------------------
-- Strict legalisation (placer_heap.cc: StrictLegaliser)
-- ---------------------------------------------------------------------------

-- | The remaining-cells max-heap: @std::priority_queue@ on
-- @(chain_size * weight, cell name id)@ pairs (largest first).
type RemHeap = M.Map (Int, Int) [IdString]

remHeapPush :: RemHeap -> Int -> IdString -> RemHeap
remHeapPush h w c = M.insertWith (++) (w, unIdString c) [c] h

remHeapPop :: RemHeap -> Maybe (IdString, RemHeap)
remHeapPop h = case M.lookupMax h of
    Nothing -> Nothing
    Just (k, (c : cs)) -> Just (c, if null cs then M.delete k h else M.insert k cs h)
    Just (_, []) -> Nothing

-- | Legaliser state threaded across cells.
data LegState = LegState
    { lgE :: !Ecp5
    , lgD :: !(Design BelId WireId PipId)
    , lgLocs :: !(M.Map IdString CellLoc)
    , lgRng :: !Rng
    , lgHeap :: !RemHeap
    , lgTotalIters :: !Int
    , lgRipupRadius :: !Int
    , lgTotalNoReset :: !Int
    }

-- | The strict legaliser pass: unbind the solve set, then
-- largest-macro-first greedy placement with ripup.
strictLegalise ::
    ArchInfo ->
    Ecp5 ->
    (T.Text -> Maybe IdString) ->
    Design BelId WireId PipId ->
    M.Map IdString FastBelsT ->
    M.Map IdString Int ->
    Int ->
    Int ->
    M.Map IdString CellLoc ->
    [IdString] ->
    M.Map IdString Int ->
    Int ->
    Rng ->
    (Ecp5, Design BelId WireId PipId, M.Map IdString CellLoc, Rng)
strictLegalise ai e cidOf d fbMap chainMap maxX maxY locs0 solveCells udata timeout rng0 =
    let cellOf dX c = M.findWithDefault (error "legal cell") c (designCells dX)
        locOf locs c = M.findWithDefault (CellLoc 0 0 0 0 0 0 False False) c locs
        -- initial unbind: solve cells (and children of solve roots) that
        -- currently have a bel
        unbindOne (eX, dX) ci =
            case cellBel ci of
                Nothing -> (eX, dX)
                Just bel
                    | M.member (cellName ci) udata || (cellCluster ci /= emptyId && M.member (cellCluster ci) udata) ->
                        let (bs, d') = unbindBel (cellName ci) bel (ecp5Bind eX) dX in (setEcp5Bind bs eX, d')
                    | otherwise -> (eX, dX)
        (e1, d1) = foldl' unbindOne (e, d) (cellsIter d)
        heap0 = foldl' (\h c -> remHeapPush h (M.findWithDefault 1 c chainMap) c) M.empty solveCells
        -- cluster2cells: cells with this cluster id, in cellsIter
        -- (reverse insertion) order
        clusterCells cl = [cellName ci | ci <- cellsIter d1, cellCluster ci == cl]
        -- base_arch getClusterPlacement: root at root_bel (with the
        -- absolute-z coercion) + constr children at their offsets
        clusterPlacement cl rootBel =
            case M.lookup cl (designCells d1) of
                Nothing -> Nothing
                Just rootCell ->
                    let Loc rx ry rz = getBelLocation e rootBel
                        (rootBel', Loc rbx rby rbz) =
                            if cellConstrAbsZ rootCell
                                then case getBelByLocation e (Loc rx ry (cellConstrZ rootCell)) of
                                    Just b | isValidBelForCellType e (cellType rootCell) b -> (b, getBelLocation e b)
                                    _ -> (rootBel, getBelLocation e rootBel)
                                else (rootBel, getBelLocation e rootBel)
                        children =
                            [ (childName, bel)
                            | childName <- cellConstrChildren rootCell
                            , Just child <- [M.lookup childName (designCells d1)]
                            , let lx = rbx + cellConstrX child
                                  ly = rby + cellConstrY child
                                  lz = if cellConstrAbsZ child then cellConstrZ child else rbz + cellConstrZ child
                            , Just bel <- [getBelByLocation e (Loc lx ly lz)]
                            , isValidBelForCellType e (cellType child) bel
                            ]
                        _dbgCluster =
                            if nm2 (cellName rootCell) == "storage_3.0.1$DPRAM_COMB0"
                                then unsafePerformIO (appendFile "/tmp/hs_cluster.txt" ("rootBel=" ++ show (getBelLocation e rootBel') ++ " nchild=" ++ show (length (cellConstrChildren rootCell)) ++ " children=" ++ concat [show (unIdString cn) ++ "@" ++ show (getBelLocation e b) ++ " " | (cn, b) <- children] ++ "\n"))
                                else ()
                     in _dbgCluster `seq` if length children == length (cellConstrChildren rootCell)
                            then Just ((cellName rootCell, rootBel') : children)
                            else Nothing
        legaliseCell st ci =
            let _dbgRng = unsafePerformIO (appendFile "/tmp/hs_rng.txt" (nm2 (cellName ci) ++ " " ++ show (rngState (lgRng st)) ++ "\n"))
                cName = _dbgRng `seq` cellName ci
                fbT = M.findWithDefault (buildFastBelsT e (cellType ci)) (cellType ci) fbMap
                totalIters' = lgTotalIters st + 1
                totalNoReset' = lgTotalNoReset st + 1
                (totalItersN, ripupN) =
                    if totalIters' > length solveCells
                        then (0, min (max maxX maxY) (lgRipupRadius st * 2))
                        else (totalIters', lgRipupRadius st)
                _ =
                    if totalNoReset' > max 5000 (8 * length (designCellOrder d1))
                        then error "Unable to find legal placement for all cells, design is probably at utilisation limit."
                        else ()
                st0 = st{lgTotalIters = totalItersN, lgRipupRadius = ripupN, lgTotalNoReset = totalNoReset'}
                inputLen eF dF nx ny =
                    sum
                        [ abs (plcX drvLoc - nx) + abs (plcY drvLoc - ny)
                        | p <- reverse (cellPortOrder ci)
                        , Just pi <- [M.lookup p (cellPorts ci)]
                        , portType pi == PortIn
                        , Just nid <- [portNet pi]
                        , Just ni <- [M.lookup nid (designNets dF)]
                        , Just drvC <- [prCell (netDriver ni)]
                        , Just drvLoc <- [M.lookup drvC (lgLocs st0)]
                        , not (plcGlobal drvLoc)
                        ]
                go eX dX locsX rngX heapX radius iterN iterAt totalForCell placed bestBel bestInp
                    | placed = (eX, dX, locsX, rngX, heapX)
                    | otherwise =
                        let l = locOf locsX cName
                            x0 = max (plcX l - radius) 0
                            y0 = max (plcY l - radius) 0
                            x1 = plcX l + radius
                            y1 = plcY l + radius
                            (nx0, rng1) = rngBounded (x1 - x0 + 1) rngX
                            (ny0, rng2) = rngBounded (y1 - y0 + 1) rng1
                            nx = nx0 + x0
                            ny = ny0 + y0
                            _dbgProbe =
                                if nm2 cName == "basesoc_uart_core_tx2_LUT4_A_Z_PFUMX_ALUT_Z_L6MUX21_D1_Z_L6MUX21_D1_Z_LUT4_Z"
                                    then unsafePerformIO (appendFile "/tmp/hs_cell_probe.txt" ("sol=" ++ show (plcX l) ++ "," ++ show (plcY l) ++ " r=" ++ show radius ++ " nx=" ++ show nx ++ " ny=" ++ show ny ++ " clust=" ++ (if cellCluster ci /= emptyId then "1" else "0") ++ "\n"))
                                    else ()
                            iterN' = _dbgProbe `seq` (iterN + 1)
                            iterAt' = iterAt + 1
                            (radius1, iterN1, iterAt1) =
                                if iterN' >= 10 * (radius + 1)
                                    then
                                        let maxR = max maxX maxY
                                            anyNonempty r =
                                                let xLo = max 0 (plcX l - r)
                                                    xHi = min maxX (plcX l + r)
                                                    yLo = max 0 (plcY l - r)
                                                    yHi = min maxY (plcY l + r)
                                                 in any (\x -> x < fbXSize fbT && any (\y -> y < fbRowSize fbT x && not (null (fbAt fbT x y))) [yLo .. yHi]) [xLo .. xHi]
                                            scan r
                                                | r >= maxR = r
                                                | anyNonempty r = r
                                                | otherwise = scan (min maxR (r + 1))
                                         in (scan (min maxR (radius + 1)), 0, 0)
                                    else (radius, iterN', iterAt')
                            needToExplore = 2 * radius1
                         in if nx < 0 || nx > maxX || ny < 0 || ny > maxY || nx >= fbXSize fbT || ny >= fbRowSize fbT nx || null (fbAt fbT nx ny)
                                then go eX dX locsX rng2 heapX radius1 iterN1 iterAt1 totalForCell False bestBel bestInp
                                else
                                    if iterAt1 >= needToExplore && bestBel /= Nothing
                                        then
                                            let (eB, dB, locsB, heapB) = acceptBel eX dX locsX heapX bestBel
                                             in go eB dB locsB rng2 heapB radius1 iterN1 iterAt1 totalForCell True bestBel bestInp
                                        else
                                            let (eT, dT, locsT, rngT, heapT, placedT, bestBelT, bestInpT) =
                                                    if cellCluster ci == emptyId
                                                        then tryPlaceCell eX dX locsX rng2 heapX radius1 iterAt1 needToExplore bestBel bestInp nx ny
                                                        else tryPlaceCluster eX dX locsX rng2 heapX radius1 bestBel bestInp nx ny
                                             in go eT dT locsT rngT heapT radius1 iterN1 iterAt1 (totalForCell + 1) placedT bestBelT bestInpT
                  where
                    -- accept the best candidate bel seen so far (ripping
                    -- and re-queuing whatever sits there)
                    acceptBel eX dX locsX heapX (Just b) =
                        let (eB0, dB0, heapB0) =
                                case boundBelCell b (ecp5Bind eX) of
                                    Nothing -> (eX, dX, heapX)
                                    Just boundC ->
                                        let (bs, d') = unbindBel boundC b (ecp5Bind eX) dX
                                         in (setEcp5Bind bs eX, d', remHeapPush heapX (M.findWithDefault 1 boundC chainMap) boundC)
                            (eB, dB) = let (bs, d') = bindBelLut (combCtxOf eB0) (ecp5Chipdb eB0) cName b StrengthWeak (ecp5Bind eB0) dB0 in (setEcp5Bind bs eB0, d')
                            Loc lx ly _ = getBelLocation eB b
                            locsB = M.insert cName (locOf locsX cName){plcX = lx, plcY = ly} locsX
                         in (eB, dB, locsB, heapB0)
                    acceptBel _ _ locsX heapX Nothing = (eX, dX, locsX, heapX)
                    tryPlaceCell eX dX locsX rngX heapX radius1 iterAt1 needToExplore bestBel bestInp nx ny =
                        let bels = fbAt fbT nx ny
                            goBel (eA, dA, locsA, rngA, heapA, placedA, bestA, bestInpA) [] = (eA, dA, locsA, rngA, heapA, placedA, bestA, bestInpA)
                            goBel (eA, dA, locsA, rngA, heapA, placedA, bestA, bestInpA) (sz : rest)
                                | placedA = (eA, dA, locsA, rngA, heapA, placedA, bestA, bestInpA)
                                | otherwise =
                                    let avail = checkBelAvail eA sz
                                        (avail', rngA') =
                                            if avail
                                                then (True, rngA)
                                                else
                                                    if radius1 > ripupN
                                                        then (True, rngA)
                                                        else let (v, r') = rngBounded 20000 rngA in (v < 10, r')
                                        _dbgSkip =
                                            if nm2 cName == "basesoc_uart_core_tx2_LUT4_A_Z_PFUMX_ALUT_Z_L6MUX21_D1_Z_L6MUX21_D1_Z_LUT4_Z"
                                                then unsafePerformIO (appendFile "/tmp/hs_skip_probe.txt" ("skip z=" ++ show (biZ (belAt (ecp5Chipdb eA) sz)) ++ " avail=" ++ show avail ++ " avail'=" ++ show avail' ++ " r=" ++ show radius1 ++ "\n"))
                                                else ()
                                     in if not avail'
                                            then _dbgSkip `seq` goBel (eA, dA, locsA, rngA', heapA, placedA, bestA, bestInpA) rest
                                            else
                                                let boundC = boundBelCell sz (ecp5Bind eA)
                                                    canUse = maybe True (\bc -> cellCluster (cellOf dA bc) == emptyId && cellBelStrength (cellOf dA bc) <= StrengthWeak) boundC
                                                 in if not canUse
                                                        then goBel (eA, dA, locsA, rngA', heapA, placedA, bestA, bestInpA) rest
                                                        else
                                                            let (eR, dR) = maybe (eA, dA) (\bc -> let (bs, d') = unbindBel bc sz (ecp5Bind eA) dA in (setEcp5Bind bs eA, d')) boundC
                                                                (eB, dB) = let (bs, d') = bindBelLut (combCtxOf eR) (ecp5Chipdb eR) cName sz StrengthWeak (ecp5Bind eR) dR in (setEcp5Bind bs eR, d')
                                                                valid = isBelLocValidE ai eB cidOf dB sz
                                                                _dbgBel =
                                                                    if nm2 cName == "basesoc_uart_core_tx2_LUT4_A_Z_PFUMX_ALUT_Z_L6MUX21_D1_Z_L6MUX21_D1_Z_LUT4_Z"
                                                                        then unsafePerformIO (appendFile "/tmp/hs_bel_probe.txt" ("z=" ++ show (biZ (belAt (ecp5Chipdb eB) sz)) ++ " avail=" ++ show avail ++ " avail'=" ++ show avail' ++ " boundC=" ++ maybe "-" (show . unIdString) boundC ++ " valid=" ++ (if valid then "1" else "0") ++ " slots=" ++ concatMap (\(zz, cc) -> show zz ++ ":" ++ show cc ++ " ") [(biZ (belAt (ecp5Chipdb eB) b), unIdString c) | b <- getBelsByTile eB nx ny, Just c <- [boundBelCell b (ecp5Bind eB)]] ++ "\n"))
                                                                        else ()
                                                                undo (eX2, dX2) =
                                                                    let (eU, dU) = let (bs, d') = unbindBel cName sz (ecp5Bind eX2) dX2 in (setEcp5Bind bs eX2, d')
                                                                        (eF, dF) = maybe (eU, dU) (\bc -> let (bs, d') = bindBelLut (combCtxOf eU) (ecp5Chipdb eU) bc sz StrengthWeak (ecp5Bind eU) dU in (setEcp5Bind bs eU, d')) boundC
                                                                     in (eF, dF)
                                                        in _dbgBel `seq` (if not valid
                                                                    then let (eF, dF) = undo (eB, dB) in goBel (eF, dF, locsA, rngA', heapA, placedA, bestA, bestInpA) rest
                                                                    else
                                                                        if iterAt1 < needToExplore
                                                                            then
                                                                                let (eF, dF) = undo (eB, dB)
                                                                                    inpLen = inputLen eF dF nx ny
                                                                                 in if inpLen < bestInpA
                                                                                        then (eF, dF, locsA, rngA', heapA, placedA, Just sz, inpLen)
                                                                                        else (eF, dF, locsA, rngA', heapA, placedA, bestA, bestInpA)
                                                                            else
                                                                                let heapF = maybe heapA (\bc -> remHeapPush heapA (M.findWithDefault 1 bc chainMap) bc) boundC
                                                                                    Loc lx ly _ = getBelLocation eB sz
                                                                                    locsF = M.insert cName (locOf locsA cName){plcX = lx, plcY = ly} locsA
                                                                                 in (eB, dB, locsF, rngA', heapF, True, bestA, bestInpA))
                        in goBel (eX, dX, locsX, rngX, heapX, False, bestBel, bestInp) bels
                    tryPlaceCluster eX dX locsX rngX heapX radius1 bestBel bestInp nx ny =
                        let bels = fbAt fbT nx ny
                            goBel2 (eA, dA, locsA, rngA, heapA, placedA, bestA, bestInpA) [] = (eA, dA, locsA, rngA, heapA, placedA, bestA, bestInpA)
                            goBel2 (eA, dA, locsA, rngA, heapA, placedA, bestA, bestInpA) (sz : rest)
                                | placedA = (eA, dA, locsA, rngA, heapA, placedA, bestA, bestInpA)
                                | otherwise =
                                    case clusterPlacement (cellCluster ci) sz of
                                        Nothing -> goBel2 (eA, dA, locsA, rngA, heapA, placedA, bestA, bestInpA) rest
                                        Just targets ->
                                            let bad =
                                                    any
                                                        ( \(_, tb) ->
                                                            case boundBelCell tb (ecp5Bind eA) of
                                                                Nothing -> False
                                                                Just bc ->
                                                                    let bci = cellOf dA bc
                                                                     in cellBelStrength bci > StrengthWeak || cellCluster bci /= emptyId
                                                        )
                                                        targets
                                                _dbgBad =
                                                    if nm2 cName == "basesoc_uart_core_tx2_LUT4_A_Z_PFUMX_ALUT_Z_L6MUX21_D1_Z_L6MUX21_D1_Z_LUT4_Z"
                                                        then unsafePerformIO (appendFile "/tmp/hs_bad.txt" ("bad=" ++ show bad ++ " targets=" ++ concatMap (\(tc, tb) -> show (unIdString tc) ++ "@" ++ show (biZ (belAt (ecp5Chipdb eA) tb)) ++ ":" ++ maybe "empty" (show . unIdString) (boundBelCell tb (ecp5Bind eA)) ++ " ") targets ++ "\n"))
                                                        else ()
                                             in _dbgBad `seq` (if bad
                                                    then goBel2 (eA, dA, locsA, rngA, heapA, placedA, bestA, bestInpA) rest
                                                    else
                                                        let moveOne (eM, dM, moves) (tc, tb) =
                                                                let boundC = boundBelCell tb (ecp5Bind eM)
                                                                    (eM1, dM1, moves1) =
                                                                        case boundC of
                                                                            Nothing -> (eM, dM, moves)
                                                                            Just bc ->
                                                                                if cellCluster (cellOf dM bc) /= emptyId
                                                                                    then
                                                                                        foldl'
                                                                                            ( \(eA2, dA2, m) cc ->
                                                                                                case cellBel (cellOf dA2 cc) of
                                                                                                    Nothing -> (eA2, dA2, m)
                                                                                                    Just cbel -> let (bs, d') = unbindBel cc cbel (ecp5Bind eA2) dA2 in (setEcp5Bind bs eA2, d', M.insert cbel (Just cc) m)
                                                                                            )
                                                                                            (eM, dM, moves)
                                                                                            (clusterCells (cellCluster (cellOf dM bc)))
                                                                                    else let (bs, d') = unbindBel bc tb (ecp5Bind eM) dM in (setEcp5Bind bs eM, d', moves)
                                                                    (eM2, dM2) = let (bs, d') = bindBelLut (combCtxOf eM1) (ecp5Chipdb eM1) tc tb StrengthStrong (ecp5Bind eM1) dM1 in (setEcp5Bind bs eM1, d')
                                                                in (eM2, dM2, M.insert tb boundC moves1)
                                                            (eP, dP, moves) = foldl' moveOne (eA, dA, M.empty :: M.Map BelId (Maybe IdString)) targets
                                                            valid = all (\tb -> isBelLocValidE ai eP cidOf dP tb) (M.keys moves)
                                                            _dbgClu =
                                                                if nm2 cName == "basesoc_uart_core_tx2_LUT4_A_Z_PFUMX_ALUT_Z_L6MUX21_D1_Z_L6MUX21_D1_Z_LUT4_Z"
                                                                    then unsafePerformIO (appendFile "/tmp/hs_bel_probe.txt" ("CLUSTER z=" ++ show (biZ (belAt (ecp5Chipdb eP) sz)) ++ " valid=" ++ (if valid then "1" else "0") ++ " targets=" ++ concatMap (\(tc, tb) -> show (unIdString tc) ++ "@" ++ show (biZ (belAt (ecp5Chipdb eP) tb)) ++ " ") targets ++ "\n"))
                                                                    else ()
                                                            revert (eR, dR) (tb, bc) =
                                                                case boundBelCell tb (ecp5Bind eR) of
                                                                    Nothing ->
                                                                        case bc of
                                                                            Nothing -> (eR, dR)
                                                                            Just bc' -> let (bs, d') = bindBelLut (combCtxOf eR) (ecp5Chipdb eR) bc' tb StrengthWeak (ecp5Bind eR) dR in (setEcp5Bind bs eR, d')
                                                                    Just cur ->
                                                                        let (bs, d') = unbindBel cur tb (ecp5Bind eR) dR
                                                                            (eB2, dB2) = case bc of
                                                                                Nothing -> (setEcp5Bind bs eR, d')
                                                                                Just bc' -> let (bs2, d2) = bindBelLut (combCtxOf eR) (ecp5Chipdb eR) bc' tb StrengthWeak bs d' in (setEcp5Bind bs2 eR, d2)
                                                                         in (eB2, dB2)
                                                     in _dbgClu `seq` (if not valid
                                                            then let (eR2, dR2) = foldl' revert (eP, dP) (M.toList moves) in goBel2 (eR2, dR2, locsA, rngA, heapA, placedA, bestA, bestInpA) rest
                                                            else
                                                            let locsP =
                                                                        foldl'
                                                                            ( \locsM (tc, tb) ->
                                                                                let Loc lx ly _ = getBelLocation eP tb
                                                                                 in M.insert tc (locOf locsM tc){plcX = lx, plcY = ly} locsM
                                                                            )
                                                                            locsA
                                                                            targets
                                                                heapP =
                                                                        foldl'
                                                                            ( \h (_, bc) ->
                                                                                maybe h (\bc' -> if cellCluster (cellOf dP bc') == emptyId || cellCluster (cellOf dP bc') == bc' then remHeapPush h (M.findWithDefault 1 bc' chainMap) bc' else h) bc
                                                                            )
                                                                            heapA
                                                                            (M.toList moves)
                                                         in (eP, dP, locsP, rngA, heapP, True, bestA, bestInpA)))
                        in goBel2 (eX, dX, locsX, rngX, heapX, False, bestBel, bestInp) bels
            in _dbgRng `seq` case go (lgE st) (lgD st) (lgLocs st) (lgRng st) (lgHeap st) 0 0 0 0 False Nothing maxBound of
                    (eF, dF, locsF, rngF, heapF) -> st0{lgE = eF, lgD = dF, lgLocs = locsF, lgRng = rngF, lgHeap = heapF}
        nm2 c = T.unpack (idToText (ecp5IdTable e) c)
        drive st = case remHeapPop (lgHeap st) of
            Nothing -> st
            Just (c, heap') ->
                let st' = st{lgHeap = heap'}
                    ci = cellOf (lgD st') c
                 in case cellBel ci of
                        Just _ -> drive st'
                        Nothing ->
                            let st'' = legaliseCell st' ci
                                ci2 = cellOf (lgD st'') c
                                _dbg =
                                    unsafePerformIO $
                                        let p = M.findWithDefault 1 c chainMap
                                            belStr = case cellBel ci2 of
                                                Nothing -> "-"
                                                Just bel -> let Loc x y z = getBelLocation (lgE st'') bel in show x ++ "," ++ show y ++ "," ++ show z
                                         in appendFile "/tmp/hs_legal.txt" (nm2 c ++ " " ++ show p ++ " " ++ belStr ++ "\n")
                             in _dbg `seq` drive st''
        stFinal = drive (LegState e1 d1 locs0 rng0 heap0 0 2 0)
     in (lgE stFinal, lgD stFinal, lgLocs stFinal, lgRng stFinal)
