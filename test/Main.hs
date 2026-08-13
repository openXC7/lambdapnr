-- | Test entry point.
module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Lambdapnr.Arch.Ecp5BindingTest (ecp5BindingTests)
import Lambdapnr.Arch.Ecp5Test (ecp5Tests)
import ZzProbeTest (zzProbe)
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
      "lambdapnr"
      [ zzProbe
      , ecp5Tests
      , ecp5BindingTests
      , rngTests
      , idStringTests
      , propertyTests
      , delayTests
      , netlistTests
      , checksumTests
      ]
