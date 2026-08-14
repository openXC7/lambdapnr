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
  ) where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

import Lambdapnr.Arch.Ecp5
import Lambdapnr.Arch.Ecp5.Types (BelId, PipId, WireId)
import Lambdapnr.Arch.Ecp5.ArchCellInfo (assignArchInfo, slicesCompatible)
import Lambdapnr.Arch.Ecp5.Binding (bindBel, boundBelCell, unbindBel)
import Lambdapnr.Arch.Ecp5.Chipdb (belAt, biZ)
import Lambdapnr.Arch.Ecp5.Types (BelId (..), Ecp5Device (..), Location (..), eaDevice)
import Lambdapnr.Kernel.Arch (Loc (..), checkBelAvail, getBelByLocation, getBelGlobalBuf, getBelLocation, getBelByName, getBels, getBelsByTile, getBelType, isValidBelForCellType)
import Lambdapnr.Kernel.DeterministicRng (Rng, shuffle)
import Foreign (Ptr, mallocArray, peekArray, withArray)
import Foreign.C.Types (CDouble (..), CInt (..))
import Foreign.Storable (peek, poke)
import Lambdapnr.Kernel.IdString (IdString (..), IdTable, emptyId, idToText, intern)
import Lambdapnr.Kernel.Netlist
import Lambdapnr.Kernel.Property (propAsString)

-- | The placer's per-cell location record (@CellLocation@).
data CellLoc = CellLoc
    { plcX :: !Int
    , plcY :: !Int
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
                                        let (bs, d') = bindBel (cellName ci) bel StrengthUser (ecp5Bind eAcc) dAcc
                                            e' = setEcp5Bind bs eAcc
                                         in if isBelLocValidE e' cidOf d' bel
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
    let -- the C++ cell_types pool keeps encounter order; available_bels
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
                        , M.insert (cellName ci) (CellLoc lx ly (fromIntegral lx) (fromIntegral ly) True (getBelGlobalBuf eAcc bel)) locsAcc
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
                            loc = CellLoc lx ly (fromIntegral lx) (fromIntegral ly) False (getBelGlobalBuf eAcc bel)
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
                                    let (bs, d') = bindBel (cellName ci) bel StrengthStrong (ecp5Bind eAcc) dAcc
                                        e' = setEcp5Bind bs eAcc
                                     in if isBelLocValidE e' cidOf d' bel
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
                            ( (M.findWithDefault (CellLoc 0 0 0 0 False False) cname acc')
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
    Ecp5 ->
    (T.Text -> Maybe IdString) ->
    Design BelId WireId PipId ->
    BelId ->
    Bool
isBelLocValidE e cidOf d bel =
    let t = getBelType e bel
        cidT x = fromMaybe emptyId (cidOf x)
     in if t == cidT "TRELLIS_COMB" || t == cidT "TRELLIS_FF" || t == cidT "TRELLIS_RAMW"
            then
                let ai = assignArchInfo cidOf d
                    Location x y = belLoc bel
                    slots =
                        [ (biZ (belAt (ecp5Chipdb e) b), c)
                        | b <- getBelsByTile e (fromIntegral x) (fromIntegral y)
                        , Just c <- [boundBelCell b (ecp5Bind e)]
                        ]
                    slotCell z = case lookup (fromIntegral z) slots of
                        Just cName -> Just (M.findWithDefault (error "slot cell") cName (designCells d), ai)
                        Nothing -> Nothing
                 in slicesCompatible slotCell
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
    { esCols :: ![[(Int, Double)]]
    , esRhs :: ![Double]
    }

emptyEqSys :: Int -> EqSys
emptyEqSys n = EqSys (replicate n []) (replicate n 0)

-- | @add_coeff@: accumulate into the sorted column (binary-search insert).
addCoeff :: EqSys -> Int -> Int -> Double -> EqSys
addCoeff es row col val =
    es{esCols = updateNth col (ins row val) (esCols es)}
  where
    ins r v [] = [(r, v)]
    ins r v ((r', x) : rest)
        | r' == r = (r', x + v) : rest
        | r' > r = (r, v) : (r', x) : rest
        | otherwise = (r', x) : ins r v rest

-- | @add_rhs@.
addRhs :: EqSys -> Int -> Double -> EqSys
addRhs es row val = es{esRhs = updateNth row (+ val) (esRhs es)}

updateNth :: Int -> (a -> a) -> [a] -> [a]
updateNth i f xs = case splitAt i xs of
    (pre, x : post) -> pre ++ (f x : post)
    _ -> xs

-- | The Eigen CG solver (same call as the C++: ConjugateGradient with
-- tolerance, solveWithGuess).
solveEqSys :: Double -> EqSys -> [Double] -> [Double]
solveEqSys tol (EqSys cols rhs) guess =
    let colptr = scanl (\acc c -> acc + length c) 0 cols
        (rows, vals) = unzip [(r, v) | c <- cols, (r, v) <- c]
     in unsafePerformIO $
            withArray (map fromIntegral colptr) $ \cp ->
                withArray (map fromIntegral rows) $ \rp ->
                    withArray (map realToFrac vals) $ \vp ->
                        withArray (map realToFrac rhs) $ \bp ->
                            withArray (map realToFrac guess) $ \gp -> do
                                out <- mallocArray (length cols)
                                _ <- c_lpSolveCg (fromIntegral (length cols)) cp rp vp bp gp (realToFrac tol) out
                                map realToFrac <$> peekArray (length cols) out

foreign import ccall unsafe "lp_solve_cg"
    c_lpSolveCg :: CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CDouble -> Ptr CDouble -> IO CInt

-- | @build_equations@ for one axis. @udata@ maps solve cells to rows;
-- any other cell is @dont_solve@.
buildEquations ::
    (IdString -> String) ->
    Design BelId WireId PipId ->
    M.Map IdString CellLoc ->
    [IdString] ->
    M.Map IdString Int ->
    Bool ->
    EqSys
buildEquations nm d locs solveCells udata yaxis =
    let nets = netsIter d
        (es, nPass) = foldl netEq (emptyEqSys (length solveCells), 0 :: Int) nets
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
    locOf c = M.findWithDefault (CellLoc 0 0 0 0 False False) c locs
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
                                -- timing criticality is 0 until the
                                -- timing engine lands (weight multiplier
                                -- becomes 1.0)
                                w =
                                    1.0
                                        / ( fromIntegral (numUsers ni)
                                                * max 1 ((if yaxis then 1 else 1) * abs (oPos - thisPos))
                                          )

                             in stamp (stamp (stamp (stamp es' p p w) p other (-w)) other other w) other p (-w)
                arcEq es' p = processArc (processArc es' p lbp) p ubp
             in (foldl arcEq es ports, n + 1)
    fst3 (c, _, _) = c
    snd3 (_, p, _) = p

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
    let locOf c = M.findWithDefault (CellLoc 0 0 0 0 False False) c locs
        guess = [fromIntegral (if yaxis then plcY (locOf c) else plcX (locOf c)) | c <- solveCells]
        out = solveEqSys 1e-5 es guess
        go acc (c, v) =
            let l = locOf c
             in if yaxis
                    then M.insert c l{plcRawY = v, plcY = max 0 (min maxY (truncate v))} acc
                    else M.insert c l{plcRawX = v, plcX = max 0 (min maxX (truncate v))} acc
     in foldl go locs (zip solveCells out)

-- | The 4 initial solve iterations of @place()@ (build + solve each
-- axis 5 times, then chain-update and report).
placeHeapInitialIters ::
    Ecp5 ->
    (T.Text -> Maybe IdString) ->
    Design BelId WireId PipId ->
    PlacerState ->
    IO PlacerState
placeHeapInitialIters e cidOf d ps0 = do
    writeFile "/tmp/hs_net_names.txt" (unlines [T.unpack (idToText (ecp5IdTable e) (netName ni)) | ni <- netsIter d])
    go ps0 (0 :: Int)
  where
    go ps i
        | i >= 4 = pure ps
        | otherwise = do
            let solveCells = psPlaceCells ps
                udata = M.fromList (zip solveCells [0 ..])
                solveDirection yaxis pAcc =
                    let step (nStep, pAcc') _ =
                            let es = buildEquations (\n -> T.unpack (idToText (ecp5IdTable e) n)) d (psLocs pAcc') solveCells udata yaxis
                                dbg =
                                    if i == 0 && not yaxis && nStep <= 2
                                        then
                                            let ls =
                                                    [ show r ++ " " ++ show c ++ " " ++ show v
                                                    | (c, col) <- zip [0 ..] (esCols es)
                                                    , (r, v) <- col
                                                    ]
                                                        ++ ["RHS " ++ show r ++ " " ++ show v | (r, v) <- zip [0 ..] (esRhs es)]
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
            go ps3 (i + 1)
