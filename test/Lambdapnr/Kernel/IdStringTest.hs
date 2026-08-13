{-# LANGUAGE OverloadedStrings #-}

-- | IdString interning tests.
module Lambdapnr.Kernel.IdStringTest (idStringTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Lambdapnr.Kernel.IdString

idStringTests :: TestTree
idStringTests =
    testGroup
        "IdString"
        [ testCase "interning is idempotent" $ do
            tbl <- newIdTable
            a <- intern tbl "hello_world"
            b <- intern tbl "hello_world"
            c <- intern tbl "hello_worl"
            assertEqual "same string, same id" a b
            assertBool "distinct strings, distinct ids" (a /= c)
        , testCase "empty string is index 0" $ do
            tbl <- newIdTable
            e <- intern tbl ""
            assertEqual "empty id is 0" emptyId e
            assertEqual "index 0" 0 (unIdString e)
        , testCase "resolution roundtrip" $ do
            tbl <- newIdTable
            a <- intern tbl "alpha"
            b <- intern tbl "beta"
            assertEqual "resolve a" "alpha" (idToStr tbl a)
            assertEqual "resolve b" "beta" (idToStr tbl b)
        , testCase "ids are stable across interning of unrelated strings" $ do
            tbl <- newIdTable
            a <- intern tbl "first"
            _ <- intern tbl "second"
            _ <- intern tbl "third"
            b <- intern tbl "first"
            assertEqual "stable" a b
            assertEqual "index unchanged" 1 (unIdString a)
        , testCase "indices are dense from 1" $ do
            tbl <- newIdTable
            _ <- intern tbl "x"
            _ <- intern tbl "y"
            _ <- intern tbl "z"
            w <- intern tbl "w"
            assertEqual "fourth id is 4" 4 (unIdString w)
        ]
