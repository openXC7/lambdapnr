-- | Test entry point.
module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Lambdapnr.Kernel.ChecksumTest (checksumTests)
import Lambdapnr.Kernel.DelayTest (delayTests)
import Lambdapnr.Kernel.DeterministicRngTest (rngTests)
import Lambdapnr.Kernel.IdStringTest (idStringTests)
import Lambdapnr.Kernel.NetlistTest (netlistTests)
import Lambdapnr.Kernel.PropertyTest (propertyTests)

main :: IO ()
main =
    defaultMain $
        testGroup
            "nextpnr-haskell kernel"
            [ rngTests
            , idStringTests
            , propertyTests
            , delayTests
            , netlistTests
            , checksumTests
            ]
