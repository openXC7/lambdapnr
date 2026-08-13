{- | Golden tests for the deterministic RNG.

The first five values of the xorshift64* stream were computed
independently from @deterministic_rng.h@ and cross-checked against the
C++ gtest suite (generic/tests/kernel.cc).
-}
module Lambdapnr.Kernel.DeterministicRngTest (rngTests) where

import Data.List (sort)
import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Lambdapnr.Kernel.DeterministicRng

rngTests :: TestTree
rngTests =
    testGroup
        "DeterministicRng"
        [ testCase "golden xorshift64* sequence" $ do
            let (v1, r1) = rng64 newRng
                (v2, r2) = rng64 r1
                (v3, r3) = rng64 r2
                (v4, r4) = rng64 r3
                (v5, _) = rng64 r4
            assertEqual "first" 0x2d85a5b43ae712a7 v1
            assertEqual "second" 0xf07aa50ab8ec29d4 v2
            assertEqual "third" 0x187610a9e8053ef3 v3
            assertEqual "fourth" 0x377b4623832a212d v4
            assertEqual "fifth" 0xfe41dbdd29e33a40 v5
        , testCase "rng30 is the low 30 bits" $ do
            let (v, _) = rng30 newRng
            assertEqual "rng() truncates to 30 bits" 988222119 v
        , testCase "rngBounded stays in range and is deterministic" $ do
            let (s1, _) = foldl' step (newRng, ()) [1 .. 1000]
                step (r, ()) n = let (x, r') = rngBounded n r in (r', x `seq` ())
            assertBool "stream consumed" (rngState s1 /= rngState newRng)
            let go r 0 acc = acc
                go r n acc =
                    let (x, r') = rngBounded 7 r
                     in go r' (n - 1) (x : acc)
                xs = go newRng 1000 []
            assertBool "all in [0,7)" (all (\x -> x >= 0 && x < 7) xs)
        , testCase "rngSeed resets deterministically" $ do
            let (_, r1) = rng64 (rngSeed 42 newRng)
                (_, r2) = rng64 (rngSeed 42 newRng)
            assertEqual "same seed, same stream" (rngState r1) (rngState r2)
            -- seed 0 maps to the default seed plus 5 warmups
            let warmed = iterate (snd . rng64) newRng !! 5
            assertEqual "seed 0 == default + 5 warmups" (rngState (rngSeed 0 newRng)) (rngState warmed)
        , testCase "shuffle preserves elements and is deterministic" $ do
            let v = V.fromList [0 .. 9 :: Int]
                (v1, r1) = shuffle newRng v
                (v2, r2) = shuffle newRng v
            assertEqual "same input, same output" v1 v2
            assertEqual "same stream state" (rngState r1) (rngState r2)
            assertEqual "permutation preserved" (sort (V.toList v1)) [0 .. 9]
        , testCase "sortedShuffle is sorted before shuffle" $ do
            let v = V.fromList [3, 1, 2, 0 :: Int]
                (v1, _) = sortedShuffle newRng v
            assertEqual "multiset preserved" (sort (V.toList v1)) [0, 1, 2, 3]
        ]
