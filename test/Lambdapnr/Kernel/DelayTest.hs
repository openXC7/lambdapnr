-- | Delay algebra tests.
module Lambdapnr.Kernel.DelayTest (delayTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Lambdapnr.Kernel.Delay

delayTests :: TestTree
delayTests =
    testGroup
        "Delay"
        [ testCase "pair arithmetic" $ do
            let a = DelayPair 1 3
                b = DelayPair 2 4
            assertEqual "plus" (DelayPair 3 7) (dpPlus a b)
            assertEqual "minus" (DelayPair (-1) (-1)) (dpMinus a b)
        , testCase "quad construction and accessors" $ do
            let q = DelayQuad (DelayPair 1 2) (DelayPair 3 4)
            assertEqual "min" 1 (dqMinDelay q)
            assertEqual "max" 4 (dqMaxDelay q)
            assertEqual "pair" (DelayPair 1 4) (dqDelayPair q)
        , testCase "quad arithmetic" $ do
            let a = DelayQuad (DelayPair 1 2) (DelayPair 3 4)
                b = dqFromDelay 5
            assertEqual "plus" (DelayQuad (DelayPair 6 7) (DelayPair 8 9)) (dqPlus a b)
            assertEqual "negate" (DelayQuad (DelayPair (-1) (-2)) (DelayPair (-3) (-4))) (dqNegate a)
        , testCase "scalar construction" $ do
            assertEqual "pair" (DelayPair 7 7) (dpFromDelay 7)
            assertEqual "quad" (DelayQuad (DelayPair 7 7) (DelayPair 7 7)) (dqFromDelay 7)
        , testCase "zero checks" $ do
            assertEqual "zero" True (isZeroDelay 0)
            assertEqual "nonzero" False (isZeroDelay 1)
        ]
