-- | Test entry point.
module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Lambdapnr.Arch.Ecp5BindingTest (ecp5BindingTests)
import Lambdapnr.CliSemanticsTest (archcheckTests, cliSemanticsTests)
import Lambdapnr.JsonFrontendTest (jsonFrontendTests)
import Lambdapnr.CliTest (cliTests)
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
      , cliTests
      , cliSemanticsTests
      , archcheckTests
      , jsonFrontendTests
      , rngTests
      , idStringTests
      , propertyTests
      , delayTests
      , netlistTests
      , checksumTests
      ]
