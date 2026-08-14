{-# LANGUAGE OverloadedStrings #-}

-- | JSON frontend tests: the derived mini project and the full
-- rioctrl_controller reference netlist. The reference cell counts are
-- the golden numbers from the LiteX build (see REFERENCE.md): the
-- loaded design must contain exactly the same leaf cells.
module Lambdapnr.JsonFrontendTest (jsonFrontendTests) where

import Data.List (isPrefixOf)
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Lambdapnr.Kernel.IdString (IdString, IdTable, emptyId, idToStr, newIdTable)
import Lambdapnr.Kernel.JsonFrontend (loadJsonDesign)
import Lambdapnr.Kernel.Netlist
import Lambdapnr.Kernel.Property (propAsInt64, propIsString)

miniPath :: FilePath
miniPath = "test/ecp5/rioctrl/derived/rioctrl_mini.json"

refPath :: FilePath
refPath = "test/ecp5/rioctrl/reference/rioctrl_controller.json"

-- | Load a design with a fresh id table; return (design, table).
loadDesign :: FilePath -> IO (Either String (Design IdString IdString IdString, IdTable))
loadDesign path = do
    tbl <- newIdTable
    src <- TIO.readFile path
    r <- loadJsonDesign tbl Nothing src
    pure (fmap (\d -> (d, tbl)) r)

-- | Cell type counts of a design, keyed by the type name.
typeCounts :: IdTable -> Design bel wire pip -> M.Map String Int
typeCounts tbl d =
    M.fromListWith (+)
        [ (idToStr tbl (cellType ci), 1)
        | ci <- M.elems (designCells d)
        ]

jsonFrontendTests :: TestTree
jsonFrontendTests =
    testGroup
        "JsonFrontend"
        [ testCase "mini project loads with the extracted cells" $ do
            r <- loadDesign miniPath
            case r of
                Left err -> assertBool ("load failed: " ++ err) False
                Right (d, tbl) -> do
                    let counts = typeCounts tbl d
                    assertEqual "LUT4 count" 28 (M.findWithDefault 0 "LUT4" counts)
                    assertEqual "TRELLIS_FF count" 18 (M.findWithDefault 0 "TRELLIS_FF" counts)
                    assertEqual "PFUMX count" 1 (M.findWithDefault 0 "PFUMX" counts)
                    assertEqual "JTAGG count" 1 (M.findWithDefault 0 "JTAGG" counts)
                    assertEqual "total cells" 79 (M.size (designCells d))
                    assertBool "nets exist" (not (M.null (designNets d)))
        , testCase "cell parameters decode via from_string" $ do
            r <- loadDesign miniPath
            case r of
                Left err -> assertBool ("load failed: " ++ err) False
                Right (d, tbl) -> do
                    -- the first LUT4: INIT "10" -> numeric 2 (binary, LSB first)
                    let lut = head [ci | ci <- M.elems (designCells d), idToStr tbl (cellType ci) == "LUT4"]
                        initKey = head [k | k <- M.keys (cellParams lut), idToStr tbl k == "INIT"]
                        initP = M.findWithDefault (error "no INIT") initKey (cellParams lut)
                    assertEqual "INIT is numeric" False (propIsString initP)
                    assertEqual "INIT value" 2 (propAsInt64 initP)
        , testCase "top-level port gets an ibuf cell" $ do
            r <- loadDesign refPath
            case r of
                Left err -> assertBool ("load failed: " ++ err) False
                Right (d, tbl) -> do
                    let ibufs = [ci | ci <- M.elems (designCells d), idToStr tbl (cellType ci) == "$nextpnr_ibuf"]
                    assertEqual "one ibuf for clk25" 1 (length ibufs)
                    assertEqual "ibuf named clk25" "clk25" (idToStr tbl (cellName (head ibufs)))
        , testCase "full reference matches the nextpnr cell counts" $ do
            r <- loadDesign refPath
            case r of
                Left err -> assertBool ("load failed: " ++ err) False
                Right (d, tbl) -> do
                    let counts = typeCounts tbl d
                    -- golden numbers from the LiteX build log (REFERENCE.md)
                    assertEqual "LUT4" 3893 (M.findWithDefault 0 "LUT4" counts)
                    assertEqual "TRELLIS_FF" 1910 (M.findWithDefault 0 "TRELLIS_FF" counts)
                    assertEqual "PFUMX" 918 (M.findWithDefault 0 "PFUMX" counts)
                    assertEqual "L6MUX21" 285 (M.findWithDefault 0 "L6MUX21" counts)
                    assertEqual "CCU2C" 234 (M.findWithDefault 0 "CCU2C" counts)
                    assertEqual "DP16KD" 25 (M.findWithDefault 0 "DP16KD" counts)
                    assertEqual "TRELLIS_DPR16X4" 8 (M.findWithDefault 0 "TRELLIS_DPR16X4" counts)
                    assertEqual "MULT18X18D" 4 (M.findWithDefault 0 "MULT18X18D" counts)
                    assertEqual "EHXPLLL" 1 (M.findWithDefault 0 "EHXPLLL" counts)
                    assertEqual "JTAGG" 1 (M.findWithDefault 0 "JTAGG" counts)
                    -- $scopeinfo cells are skipped, the clk25 ibuf is added
                    assertEqual "no scopeinfo" 0 (M.findWithDefault 0 "$scopeinfo" counts)
                    let leaf = sum [n | (t, n) <- M.toList counts, not ("$" `isPrefixOf` t)]
                    assertEqual "leaf cells = imported - helper cells" 15006 leaf
                    assertBool "nets exist" (M.size (designNets d) > 5000)
        , testCase "drivers and users are connected" $ do
            r <- loadDesign miniPath
            case r of
                Left err -> assertBool ("load failed: " ++ err) False
                Right (d, tbl) -> do
                    -- every net driven by exactly one output, inputs as users
                    let driven = [ni | ni <- M.elems (designNets d), prCell (netDriver ni) /= Nothing]
                    assertBool "some driven nets" (not (null driven))
                    assertBool
                        "all drivers reference existing cells"
                        (all (\ni -> M.member (fromJust (prCell (netDriver ni))) (designCells d)) driven)
                    -- every cell port with a net is consistent both ways
                    let okPort ci (p, pi) =
                            case portNet pi of
                                Nothing -> True
                                Just n ->
                                    case M.lookup n (designNets d) of
                                        Nothing -> False
                                        Just ni ->
                                            (portType pi == PortOut && prCell (netDriver ni) == Just (cellName ci) && prPort (netDriver ni) == p)
                                                || (portType pi /= PortOut && any (\u -> maybe False (\ur -> prCell ur == Just (cellName ci) && prPort ur == p) u) (V.toList (netUsers ni)))
                    assertBool
                        "port/net consistency"
                        (all (\(ci, p, pi) -> okPort ci (p, pi)) [(ci, p, pi) | ci <- M.elems (designCells d), (p, pi) <- M.toList (cellPorts ci)])
        ]
  where
    fromJust (Just x) = x
    fromJust Nothing = error "fromJust: Nothing"
