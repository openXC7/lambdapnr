{-# LANGUAGE OverloadedStrings #-}

-- | Memory-bound regression test for the ECP5 chipdb path.
--
-- The chipdb parser used to be able to allocate tens of gigabytes from a
-- single corrupt slice count (a sign-extension bug in the i32 reader made
-- a ~4e9-element 'V.generate', OOM-killing the machine during test runs).
-- Three layers of defence:
--
--   1. the parser bounds-checks every slice before allocating (clean
--      error instead of OOM);
--   2. the test binary embeds an RTS heap cap (-M2G) so a regression can
--      never again take the machine down;
--   3. this test asserts the whole load-and-query surface stays far
--      below a fixed process-RSS ceiling.
--
-- Peak RSS is read from /proc/self/status, so no +RTS -T flag is needed.
module ZzProbeTest (zzProbe) where

import Control.Exception (evaluate)
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector as V
import System.IO (hFlush, stdout)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, testCase)

import Lambdapnr.Arch.Ecp5
import Lambdapnr.Arch.Ecp5.Chipdb (Chipdb (..), ltWires)
import Lambdapnr.Arch.Ecp5.Types (Ecp5Args (..), Ecp5Device (..), SpeedGrade (Speed6))
import Lambdapnr.Kernel.Arch

-- | Ceiling for the whole test process's peak RSS (generous; the loaded
-- chipdb + forced query surface is a few hundred MB).
memCeiling :: Integer
memCeiling = 3 * 1024 * 1024 * 1024

-- | Peak resident set size in bytes from /proc/self/status (0 if
-- unavailable).
peakRss :: IO Integer
peakRss = do
  s <- C.readFile "/proc/self/status"
  pure $
    case filter (C.isPrefixOf "VmHWM:") (C.lines s) of
      (l : _) -> case C.readInt (C.dropWhile (\c -> c == ' ' || c == '\t') (C.drop 6 l)) of
        Just (kB, _) -> fromIntegral kB * 1024
        Nothing -> 0
      [] -> 0

zzProbe :: TestTree
zzProbe = testCase "ecp5 chipdb load stays within memory bounds" $ do
  -- the largest shipped chipdb is the worst case
  e <- loadEcp5 (Ecp5Args Lfe5u85f "" Speed6) "data/ecp5/chipdb-85k.bin" >>= either (error . show) pure
  let cd = ecp5Chipdb e
  -- force the whole query surface: all wires of all location types, then
  -- the full bel/wire/pip enumerations
  _ <- evaluate (V.sum (V.map (fromIntegral . V.length . ltWires) (cdLocations cd)))
  _ <- evaluate (length (getWires e))
  _ <- evaluate (length (getPips e))
  _ <- evaluate (length (getBels e))
  rss <- peakRss
  putStrLn ("zz: peak RSS after 85k chipdb load+query = " ++ show (rss `div` (1024 * 1024)) ++ " MB")
  hFlush stdout
  assertBool
    ("peak RSS after full chipdb load+query exceeds ceiling: "
      ++ show (rss `div` (1024 * 1024))
      ++ " MB")
    (rss < memCeiling)
