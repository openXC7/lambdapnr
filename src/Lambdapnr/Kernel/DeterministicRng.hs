{- | Deterministic pseudo-random number generation.

Bit-exact mirror of nextpnr's @DeterministicRNG@
(@common\/kernel\/deterministic_rng.h@): xorshift64* with a fixed default
seed. Every algorithm that consumes randomness (placer moves, router
tie-breaks, shuffles) draws from this stream, so the Haskell port must
reproduce it exactly to match C++ outputs bit-for-bit.

The C++ implementation is stateful; here the state is threaded
explicitly, which keeps the kernels pure and lets parallel phases draw
from independent, deterministically-derived states.
-}
module Lambdapnr.Kernel.DeterministicRng (
    Rng,
    rngState,
    newRng,
    rngSeed,
    rng64,
    rng30,
    rngBounded,
    shuffle,
    sortedShuffle,
) where

import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.List (sort)
import Control.Monad.ST (runST)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.Word (Word32, Word64)

-- | RNG state (the xorshift64* state word).
newtype Rng = Rng {rngState :: Word64}

defaultSeed :: Word64
defaultSeed = 0x3141592653589793

-- | A fresh RNG at the default seed (matches the C++ constructor).
newRng :: Rng
newRng = Rng defaultSeed

{- | One xorshift64* step. Returns the output word and the next state.

Mirrors @rng64@ exactly: the output is computed from the /old/ state,
then the state is advanced with three xorshift rounds.
-}
rng64 :: Rng -> (Word64, Rng)
rng64 (Rng s) = (s * 0x2545F4914F6CDD1D, Rng s3)
  where
    s1 = s `xor` (s `shiftR` 12)
    s2 = s1 `xor` (s1 `shiftL` 25)
    s3 = s2 `xor` (s2 `shiftR` 27)

{- | Reseed, mirroring @rngseed@: seed 0 maps back to the default seed, then
five warmup rounds are consumed (the C++ loop calls @rng64()@ five times).
-}
rngSeed :: Word64 -> Rng -> Rng
rngSeed seed = warmup 5 . const (Rng base)
  where
    base = if seed == 0 then defaultSeed else seed
    warmup 0 r = r
    warmup n r = warmup (n - 1) (snd (rng64 r))

-- | @rng()@ in C++: the low 30 bits of the raw 64-bit output.
rng30 :: Rng -> (Word32, Rng)
rng30 r = (fromIntegral (v .&. 0x3fffffff), r')
  where
    (v, r') = rng64 r

{- | @rng(n)@ in C++: rejection sampling against a power-of-two mask.
/Important/: the C++ implementation masks the raw @rng64()@ output (not
@rng()@), so this must draw @rng64@ directly to stay in lockstep.
-}
rngBounded :: Int -> Rng -> (Int, Rng)
rngBounded n r
    | n <= 0 = error "DeterministicRng.rngBounded: n must be positive"
    | otherwise = go r
  where
    m = ceilPow2 (fromIntegral n) :: Word64
    go r0 =
        let (v, r1) = rng64 r0
            x = fromIntegral (v .&. (m - 1))
         in if x < n then (x, r1) else go r1

-- | Smallest power of two >= n (C++ rounds up to a power of 2).
ceilPow2 :: Word64 -> Word64
ceilPow2 n =
    let m0 = n - 1
        m1 = m0 .|. (m0 `shiftR` 1)
        m2 = m1 .|. (m1 `shiftR` 2)
        m3 = m2 .|. (m2 `shiftR` 4)
        m4 = m3 .|. (m3 `shiftR` 8)
        m5 = m4 .|. (m4 `shiftR` 16)
        m6 = m5 .|. (m5 `shiftR` 32)
     in m6 + 1

{- | Fisher-Yates shuffle, mirroring @shuffle@: for each @i@ in order,
swap element @i@ with element @i + rng(size - i)@ when the latter is
strictly greater.

ST-backed with the same draw sequence (the goldens pin the draws, not
the data structure).
-}
shuffle :: Rng -> Vector a -> (Vector a, Rng)
shuffle r0 v0 = runST $ do
    mv <- V.thaw v0
    let n = V.length v0
        go r i
            | i >= n = do
                v' <- V.freeze mv
                pure (v', r)
            | otherwise = do
                let (j, r') = rngBounded (n - i) r
                    j' = i + j
                if j' > i
                    then do
                        vi <- VM.read mv i
                        vj <- VM.read mv j'
                        VM.write mv i vj
                        VM.write mv j' vi
                    else pure ()
                go r' (i + 1)
    go r0 0

-- | Sort, then shuffle (@sorted_shuffle@ in C++).
sortedShuffle :: (Ord a) => Rng -> Vector a -> (Vector a, Rng)
sortedShuffle r = shuffle r . V.fromList . sort . V.toList
