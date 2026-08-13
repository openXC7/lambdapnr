{-# LANGUAGE OverloadedStrings #-}

{- | nextpnr-haskell entry point.

Kernel milestone: prints version info and runs a quick RNG self-check
(the first golden value of the xorshift64* stream). The pack/place/
route CLI arrives with the first concrete arch.
-}
module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Word (Word64)
import Numeric (showHex)

import Lambdapnr.Kernel.DeterministicRng (newRng, rng64)

version :: Text
version = "0.1.0.0"

goldenFirst :: Word64
goldenFirst = 0x2d85a5b43ae712a7

main :: IO ()
main = do
    TIO.putStrLn $
        "lambdapnr " <> version <> " — Haskell FPGA place-and-route (kernel milestone)"
    let (v, _) = rng64 newRng
    TIO.putStrLn $
        "  rng self-check: "
            <> hexWord64 v
            <> " == "
            <> hexWord64 goldenFirst
            <> if v == goldenFirst then "  [ok]" else "  [FAILED]"

hexWord64 :: Word64 -> Text
hexWord64 = T.pack . flip showHex ""
