{-# LANGUAGE BangPatterns #-}

{- | C-exact floating point formatting.

GHC's @printf@\/@show@ do not reproduce the C library byte-for-byte:
@printf %.1f@ prints @Infinity@ where C prints @inf@, and GHC's %g
exponent form differs from glibc. The writers (@--write@/@--sdf@/
@--report@) must match the oracle output byte-for-byte, so this module
implements the exact glibc semantics:

- @printf("%.pg")@: round the exact decimal value to @p@ significant
  digits (round-half-even), strip trailing zeros, exponential form when
  the rounded exponent is @< -4@ or @>= p@, exponent as @e+XX@/@e-XX@
  (at least two digits).
- special values print @inf@/@-inf@/@nan@.

The exact decimal expansion of a binary double is computed from its bit
pattern (a dyadic rational has a terminating decimal expansion), so the
rounding is correct in all cases.
-}
module Lambdapnr.Kernel.CFormat (
    formatG,
) where

import Data.Char (intToDigit)
import GHC.Float (decodeFloat, isInfinite, isNaN)

-- | C @printf("%.*g")@ (also @std::ostream@'s default float format with
-- precision 6, and json11's @%.17g@).
formatG :: Int -> Double -> String
formatG p x
    | isNaN x = "nan"
    | isInfinite x = if x < 0 then "-inf" else "inf"
    | x == 0 = "0"
    | otherwise =
        let sign = if x < 0 then "-" else ""
            digits = exactDigits (abs x)
            -- digits: the exact decimal digits of |x| (no leading zeros);
            -- value = 0.digits * 10^exp10
            (mant, exp10) = digits
            rounded = roundDigits p mant exp10
         in sign ++ renderG p rounded



-- | The exact decimal digits of a positive finite double:
-- @(mantissaDigits, exp10)@ with @x = 0.digits * 10^exp10@.
exactDigits :: Double -> ([Int], Int)
exactDigits x =
    let (m, e2) = decodeFloat x -- x = m * 2^e2, m integer
     in if e2 >= 0
            then
                let s = show (m * (2 ^ e2 :: Integer))
                 in (digitsOf s, length s)
            else
                -- x = m / 2^k with k > 0 = m * 5^k / 10^k: a terminating
                -- decimal with exactly k fraction digits
                let k = negate e2
                    s = show (m * (5 ^ k :: Integer))
                 in (digitsOf s, length s - k)
  where
    digitsOf = map (\c -> fromEnum c - fromEnum '0')

-- | Round a digit list to @p@ significant digits (round-half-even on the
-- exact value); returns @(digits, exp10)@ of the rounded number.
roundDigits :: Int -> [Int] -> Int -> ([Int], Int)
roundDigits p ds e
    | length ds <= p = (trimZeros ds, e)
    | otherwise =
        let keep = take p ds
            rest = drop p ds
            first = head rest
            tailRest = drop 1 rest
            roundUp
                | first > 5 = True
                | first < 5 = False
                | any (/= 0) tailRest = True
                | otherwise = odd (last keep) -- ties to even
            kept
                | roundUp = addOne keep
                | otherwise = keep
         in if length kept > p
                then (trimZeros kept, e + 1) -- carry: 9.99999 -> 10
                else (trimZeros kept, e)
  where
    addOne ds =
        let step (d : rest)
                | d + 1 >= 10 = 0 : step rest
                | otherwise = (d + 1) : rest
            step [] = [1]
         in reverse (step (reverse ds))
    trimZeros = reverse . dropWhile (== 0) . reverse

-- | Render the rounded @(digits, exp10)@ in @%g@ style with precision @p@:
-- the decimal point sits before the first digit; use exponential form when
-- @exp < -4@ or @exp >= p@.
renderG :: Int -> ([Int], Int) -> String
renderG p (digits, exp10)
    | exp10 - 1 < -4 || exp10 - 1 >= p =
        let mantPart = showDigit (head digits) ++ case drop 1 digits of
                [] -> ""
                rest -> "." ++ concatMap showDigit rest
            (esign, eabs) = if exp10 - 1 < 0 then ("-", 1 - exp10) else ("+", exp10 - 1)
         in mantPart ++ "e" ++ esign ++ (if eabs < 10 then "0" else "") ++ show eabs
    | exp10 <= 0 =
        "0." ++ replicate (negate exp10) '0' ++ concatMap showDigit digits
    | length digits > exp10 =
        concatMap showDigit (take exp10 digits) ++ "." ++ concatMap showDigit (drop exp10 digits)
    | otherwise =
        concatMap showDigit digits ++ replicate (exp10 - length digits) '0'
  where
    showDigit = (: []) . intToDigit
