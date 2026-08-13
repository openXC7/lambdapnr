{-# LANGUAGE OverloadedStrings #-}

-- | Property semantics tests (bit strings, extraction, string escaping).
module Lambdapnr.Kernel.PropertyTest (propertyTests) where

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Lambdapnr.Kernel.Property

propertyTests :: TestTree
propertyTests =
    testGroup
        "Property"
        [ testCase "integer construction builds LSB-first bit string" $ do
            assertEqual "0x5, 16 bits" "1010000000000000" (pStr (propFromInt 5 16))
            assertEqual "0xa, 16 bits" "0101000000000000" (pStr (propFromInt 10 16))
            assertEqual "K=4, 32 bits" "00100000000000000000000000000000" (pStr (propFromInt 4 32))
        , testCase "asInt64 roundtrip" $ do
            assertEqual "5" 5 (propAsInt64 (propFromInt 5 16))
            assertEqual "large" (2 ^ (62 :: Int)) (propAsInt64 (propFromInt (2 ^ (62 :: Int)) 64))
        , testCase "to_string reverses bit order (MSB first)" $ do
            assertEqual "0x5" "0000000000000101" (propToStr (propFromInt 5 16))
            assertEqual "0xa" "0000000000001010" (propToStr (propFromInt 10 16))
        , testCase "string escaping (binary-looking literals get a space)" $ do
            assertEqual "all-binary literal" "0101 " (propToStr (propFromString "0101"))
            -- a literal with a trailing space is itself binary-like: the C++
            -- state machine (state 1) appends another space
            assertEqual "trailing-space literal" "0101  " (propToStr (propFromString "0101 "))
            -- but from_string strips that marker space again
            assertEqual "stripped roundtrip" "0101 " (propToStr (propFromStr "0101 "))
            assertEqual "plain literal" "abc" (propToStr (propFromString "abc"))
        , testCase "from_string is the inverse of to_string" $ do
            assertEqual "numeric" (propFromInt 5 16) (propFromStr "0000000000000101")
            assertEqual "escaped string" (propFromString "0101") (propFromStr "0101 ")
            assertEqual "plain string" (propFromString "abc") (propFromStr "abc")
        , testCase "extract slices with padding" $ do
            let p = propFromInt 5 16 -- "1010000000000000"
            assertEqual "low nibble" 5 (propAsInt64 (propExtract 0 4 S0 p))
            assertEqual "bits 0..2" 5 (propAsInt64 (propExtract 0 3 S0 p))
            assertEqual "beyond width pads" 0 (propAsInt64 (propExtract 15 4 S0 p))
        , testCase "state properties" $ do
            assertEqual "S1" 1 (propAsInt64 (propFromState S1))
            assertEqual "S0" 0 (propAsInt64 (propFromState S0))
            assertEqual "S1 str" "1" (pStr (propFromState S1))
            assertBool "S1 is numeric" (not (propIsString (propFromState S1)))
        , testCase "size semantics" $ do
            assertEqual "numeric size in bits" 16 (propSize (propFromInt 5 16))
            assertEqual "string size in bytes" 24 (propSize (propFromString "abc"))
        , testCase "equality compares the bit string" $ do
            assertEqual "same value" (propFromInt 5 16) (propFromInt 5 16)
            assertBool "different widths differ" (propFromInt 5 16 /= propFromInt 5 32)
        ]
