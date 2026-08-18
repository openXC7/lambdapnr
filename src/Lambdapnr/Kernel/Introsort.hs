-- |
-- Exact port of GCC libstdc++ (GCC 16.2.1) @std::sort@ (introsort) as
-- implemented in @bits/stl_algo.h@ and @bits/stl_heap.h@, used where the
-- oracle's UNSTABLE sort permutation is observable in output (the ROUTING
-- strings of the global-spine nets in @--write@ output).  The C++ side
-- sorts a @std::vector<std::pair<PortRef *, int>>@ with
--
-- > [this](const auto &a, const auto &b) {
-- >     return global_route_priority(*a.first) < global_route_priority(*b.first);
-- > }
--
-- and the resulting permutation of equal-priority elements depends on the
-- exact introsort algorithm.  This module reproduces that permutation
-- bit-for-bit: every heap operation (@__make_heap@/@__adjust_heap@/
-- @__push_heap@/@__pop_heap@/@__sort_heap@), the median-of-three pivot
-- selection, the Hoare partition loop and the final insertion sort are
-- translated one-to-one, operating on a mutable vector that holds the
-- values themselves (exactly mirroring the C++ move pattern).
module Lambdapnr.Kernel.Introsort (stdSortBy) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Bits (countLeadingZeros, finiteBitSize)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

-- | Sort with the exact permutation of libstdc++ @std::sort@, where
-- @less x y@ is the strict-weak-ordering comparator (@x@ sorts before
-- @y@).  Unstable: equal elements are not necessarily kept in input
-- order.
stdSortBy :: (a -> a -> Bool) -> [a] -> [a]
stdSortBy less xs0
    | null xs0 = []
    | otherwise = V.toList (runST (go less (V.fromList xs0)))
  where
    go :: (a -> a -> Bool) -> V.Vector a -> ST s (V.Vector a)
    go less vals = do
        let n = V.length vals
        arr <- MV.generate n (vals V.!)
        sortRange less arr 0 n
        V.unsafeFreeze arr

    -- std::__sort:  lg(n) * 2 depth limit, then final insertion sort.
    sortRange less arr first last =
        when (first /= last) $ do
            introsortLoop less arr first last (lg (last - first) * 2)
            finalInsertionSort less arr first last

    -- std::__lg (bits/stl_algobase.h): __bit_width(n) - 1 for n > 0.
    lg n = finiteBitSize n - 1 - countLeadingZeros n

    -- std::__introsort_loop (bits/stl_algo.h): while (last - first > 16).
    introsortLoop less arr first last depthLimit =
        if last - first <= 16
            then pure ()
            else
                if depthLimit == 0
                    then partialSort less arr first last last
                    else do
                        cut <- unguardedPartitionPivot less arr first last
                        introsortLoop less arr cut last (depthLimit - 1)
                        introsortLoop less arr first cut (depthLimit - 1)

    -- std::__partial_sort = __heap_select + __sort_heap.
    partialSort less arr first middle last = do
        heapSelect less arr first middle last
        sortHeap less arr first middle

    -- std::__heap_select.
    heapSelect less arr first middle last = do
        makeHeap less arr first middle
        forM_ [middle .. last - 1] $ \i -> do
            vI <- MV.read arr i
            vFirst <- MV.read arr first
            when (less vI vFirst) $ popHeap less arr first middle i

    -- std::__make_heap: adjust from parent (len-2)/2 down to 0.
    makeHeap less arr first last
        | last - first < 2 = pure ()
        | otherwise = do
            let len = last - first
            let go parent = do
                    v <- MV.read arr (first + parent)
                    adjustHeap less arr first parent len v
                    when (parent > 0) $ go (parent - 1)
            go ((len - 2) `quot` 2)

    -- std::__adjust_heap.
    adjustHeap less arr first holeIndex0 len value = do
        let topIndex = holeIndex0
        let loop holeIndex secondChild
                | secondChild < (len - 1) `quot` 2 = do
                    let sc = 2 * (secondChild + 1)
                    vSc <- MV.read arr (first + sc)
                    vScm1 <- MV.read arr (first + sc - 1)
                    let chosen = if less vSc vScm1 then sc - 1 else sc
                    vChosen <- MV.read arr (first + chosen)
                    MV.write arr (first + holeIndex) vChosen
                    loop chosen chosen
                | even len && secondChild == (len - 2) `quot` 2 = do
                    let sc = 2 * (secondChild + 1)
                    vScm1 <- MV.read arr (first + sc - 1)
                    MV.write arr (first + holeIndex) vScm1
                    pushHeap less arr first (sc - 1) topIndex value
                | otherwise =
                    pushHeap less arr first holeIndex topIndex value
        loop holeIndex0 holeIndex0

    -- std::__push_heap.
    pushHeap less arr first holeIndex0 topIndex value = do
        let go holeIndex parent
                | holeIndex > topIndex = do
                    vParent <- MV.read arr (first + parent)
                    if less vParent value
                        then do
                            MV.write arr (first + holeIndex) vParent
                            let holeIndex' = parent
                                parent' = (holeIndex' - 1) `quot` 2
                            go holeIndex' parent'
                        else MV.write arr (first + holeIndex) value
                | otherwise = MV.write arr (first + holeIndex) value
        go holeIndex0 ((holeIndex0 - 1) `quot` 2)

    -- std::__pop_heap(first, last, result).
    popHeap less arr first last result = do
        vResult <- MV.read arr result
        vFirst <- MV.read arr first
        MV.write arr result vFirst
        adjustHeap less arr first 0 (last - first) vResult

    -- std::__sort_heap.
    sortHeap less arr first last =
        if last - first > 1
            then do
                popHeap less arr first (last - 1) (last - 1)
                sortHeap less arr first (last - 1)
            else pure ()

    -- std::__move_median_to_first (bits/stl_algo.h).
    moveMedianToFirst less arr result a b c = do
        va <- MV.read arr a
        vb <- MV.read arr b
        if less va vb
            then do
                vc <- MV.read arr c
                if less vb vc
                    then swapV result b
                    else if less va vc then swapV result c else swapV result a
            else do
                vc <- MV.read arr c
                if less va vc
                    then swapV result a
                    else if less vb vc then swapV result c else swapV result b
      where
        swapV i j = do
            vi <- MV.read arr i
            vj <- MV.read arr j
            MV.write arr i vj
            MV.write arr j vi

    -- std::__unguarded_partition (Hoare, with the median as sentinel).
    unguardedPartition less arr first last pivot = do
        vPivot <- MV.read arr pivot
        let advanceF f = do
                vF <- MV.read arr f
                if less vF vPivot then advanceF (f + 1) else pure f
            advanceL l = do
                vL <- MV.read arr l
                if less vPivot vL then advanceL (l - 1) else pure l
        let go f l = do
                f' <- advanceF f
                l' <- advanceL (l - 1)
                if f' < l'
                    then do
                        vf <- MV.read arr f'
                        vl <- MV.read arr l'
                        MV.write arr f' vl
                        MV.write arr l' vf
                        go (f' + 1) l'
                    else pure f'
        go first last

    -- std::__unguarded_partition_pivot.
    unguardedPartitionPivot less arr first last = do
        let mid = first + (last - first) `quot` 2
            second = first + 1
        moveMedianToFirst less arr first second mid (last - 1)
        unguardedPartition less arr second last first

    -- std::__final_insertion_sort (threshold _S_threshold = 16).
    finalInsertionSort less arr first last =
        if last - first > 16
            then do
                insertionSort less arr first (first + 16)
                unguardedInsertionSort less arr (first + 16) last
            else insertionSort less arr first last

    -- std::__insertion_sort.
    insertionSort less arr first last
        | first == last = pure ()
        | otherwise = go (first + 1)
      where
        go i
            | i == last = pure ()
            | otherwise = do
                vI <- MV.read arr i
                vFirst <- MV.read arr first
                if less vI vFirst
                    then do
                        -- std::__glibcxx_move_backward3(first, i, i+1).
                        forM_ [i - 1, i - 2 .. first] $ \j -> do
                            vj <- MV.read arr j
                            MV.write arr (j + 1) vj
                        MV.write arr first vI
                        go (i + 1)
                    else do
                        unguardedLinearInsert less arr i
                        go (i + 1)

    -- std::__unguarded_insertion_sort.
    unguardedInsertionSort less arr first last = do
        forM_ [first .. last - 1] $ \i -> unguardedLinearInsert less arr i

    -- std::__unguarded_linear_insert.
    unguardedLinearInsert less arr last0 = do
        vVal <- MV.read arr last0
        let go last next = do
                vNext <- MV.read arr next
                if less vVal vNext
                    then do
                        MV.write arr last vNext
                        go next (next - 1)
                    else MV.write arr last vVal
        go last0 (last0 - 1)
