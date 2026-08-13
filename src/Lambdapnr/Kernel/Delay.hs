{- | Timing delay values.

Mirror of nextpnr's @DelayPair@\/@DelayQuad@ (@common\/kernel\/nextpnr_types.h@).
'DelayT' stays integral for the classic arches (ice40/ecp5/generic use
@int@; himbaechel-xilinx uses @double@ — a separate module will
parameterize that later). Integral arithmetic is mandatory for checksum
and QoR parity with the C++ kernels.
-}
module Lambdapnr.Kernel.Delay (
    DelayT,
    DelayPair (..),
    DelayQuad (..),
    dpFromDelay,
    dqFromDelay,
    dqMinDelay,
    dqMaxDelay,
    dqDelayPair,
    isZeroDelay,
    dpPlus,
    dpMinus,
    dqPlus,
    dqMinus,
    dqNegate,
) where

import Data.Int (Int64)

-- | Delay unit (integer nanoseconds for classic arches).
type DelayT = Int64

-- | Minimum and maximum delay of a timing arc.
data DelayPair = DelayPair
    { dpMin :: !DelayT
    , dpMax :: !DelayT
    }
    deriving (Eq, Show)

-- | Four-quadrant delay: min/max rise and min/max fall.
data DelayQuad = DelayQuad
    { dqRise :: !DelayPair
    , dqFall :: !DelayPair
    }
    deriving (Eq, Show)

dpFromDelay :: DelayT -> DelayPair
dpFromDelay d = DelayPair d d

dqFromDelay :: DelayT -> DelayQuad
dqFromDelay d = DelayQuad (dpFromDelay d) (dpFromDelay d)

dqMinDelay :: DelayQuad -> DelayT
dqMinDelay q = min (dpMin (dqRise q)) (dpMin (dqFall q))

dqMaxDelay :: DelayQuad -> DelayT
dqMaxDelay q = max (dpMax (dqRise q)) (dpMax (dqFall q))

dqDelayPair :: DelayQuad -> DelayPair
dqDelayPair q = DelayPair (dqMinDelay q) (dqMaxDelay q)

isZeroDelay :: DelayT -> Bool
isZeroDelay = (== 0)

dpPlus :: DelayPair -> DelayPair -> DelayPair
dpPlus a b = DelayPair (dpMin a + dpMin b) (dpMax a + dpMax b)

dpMinus :: DelayPair -> DelayPair -> DelayPair
dpMinus a b = DelayPair (dpMin a - dpMin b) (dpMax a - dpMax b)

dqPlus :: DelayQuad -> DelayQuad -> DelayQuad
dqPlus a b = DelayQuad (dpPlus (dqRise a) (dqRise b)) (dpPlus (dqFall a) (dqFall b))

dqMinus :: DelayQuad -> DelayQuad -> DelayQuad
dqMinus a b = DelayQuad (dpMinus (dqRise a) (dqRise b)) (dpMinus (dqFall a) (dqFall b))

dqNegate :: DelayQuad -> DelayQuad
dqNegate a =
    DelayQuad
        (DelayPair (negate (dpMin (dqRise a))) (negate (dpMax (dqRise a))))
        (DelayPair (negate (dpMin (dqFall a))) (negate (dpMax (dqFall a))))
