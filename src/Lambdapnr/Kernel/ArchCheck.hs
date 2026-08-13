{-# LANGUAGE FlexibleContexts #-}

{- | Architecture database integrity check — the Haskell port of
@common\/kernel\/archcheck.cc@ (the @--test@ flag).

Walks the whole chipdb through the 'Arch' interface and returns a list
of failure messages (empty = passed). Checks, mirroring the C++
@archcheck_names@\/@archcheck_locs@\/@archcheck_conn@\/@archcheck_buckets@:

* entity names round-trip through @getBelByName@\/@getWireByName@\/@getPipByName@;
* bel locations are in-grid, within tile depth, and round-trip through
  @getBelByLocation@, with no duplicate z per tile;
* connectivity: downhill pips leave their wire, uphill pips arrive at
  it, wire bel-pins agree with @getBelPinWire@, bel pins appear on
  their wire, and sampled pips appear in both their source's downhill
  and destination's uphill lists;
* buckets: every bel is in its own bucket, bucket/cell-type round-trip,
  and every cell type has a non-empty bucket.

The pip-name and pip-membership walks are sampled (1 in 1000): the full
walks would intern tens of millions of names on the largest parts.
-}
module Lambdapnr.Kernel.ArchCheck (archcheck) where

import Data.List (nub)
import qualified Data.Map.Strict as M

import Lambdapnr.Kernel.Arch
import Lambdapnr.Kernel.IdString (IdString)

-- | Sample every k-th element of a list.
sampled :: Int -> [a] -> [a]
sampled k = go 0
  where
    go _ [] = []
    go i (x : xs) = if i `mod` k == 0 then x : go (i + 1) xs else go (i + 1) xs

-- | Run the integrity check; the returned list holds every failure.
archcheck :: (Arch a, Eq (Bel a), Eq (Wire a), Eq (Pip a)) => a -> [String]
archcheck a = concat [names, locs, conn, buckets]
  where
    -- entity names --------------------------------------------------------

    names =
        concat
            [ [ "bel name mismatch: " ++ show (getBelName a b) | b <- getBels a, not (belNameOk b) ]
            , [ "wire name mismatch: " ++ show (getWireName a w) | w <- getWires a, not (wireNameOk w) ]
            , [ "pip name mismatch: " ++ show (getPipName a p) | p <- sampled 1000 (getPips a), not (pipNameOk p) ]
            ]
      where
        belNameOk b = getBelByName a (getBelName a b) == Just b
        wireNameOk w = getWireByName a (getWireName a w) == Just w
        pipNameOk p = getPipByName a (getPipName a p) == Just p

    -- locations -----------------------------------------------------------

    locs =
        concat
            [ [ "bel location out of range: " ++ show (getBelName a b)
              | b <- getBels a
              , let loc = getBelLocation a b
              , locX loc < 0 || locY loc < 0 || locZ loc < 0
                  || locX loc >= getGridDimX a
                  || locY loc >= getGridDimY a
                  || locZ loc >= getTileBelDimZ a (locX loc) (locY loc)
              ]
            , [ "bel location roundtrip: " ++ show (getBelName a b)
              | b <- getBels a
              , let loc = getBelLocation a b
              , getBelByLocation a loc /= Just b
              ]
            , [ "duplicate z at tile (" ++ show x ++ "," ++ show y ++ "): " ++ show (nub zs)
              | x <- [0 .. getGridDimX a - 1]
              , y <- [0 .. getGridDimY a - 1]
              , let zs = [locZ (getBelLocation a b) | b <- getBelsByTile a x y]
              , nub zs /= zs
              ]
            ]

    -- connectivity --------------------------------------------------------

    conn =
        concat
            [ [ "wire " ++ show (getWireName a w) ++ ": downhill pip " ++ show (getPipName a p) ++ " does not leave it"
              | w <- getWires a
              , p <- getPipsDownhill a w
              , getPipSrcWire a p /= w
              ]
            , [ "wire " ++ show (getWireName a w) ++ ": uphill pip " ++ show (getPipName a p) ++ " does not arrive at it"
              | w <- getWires a
              , p <- getPipsUphill a w
              , getPipDstWire a p /= w
              ]
            , [ "wire " ++ show (getWireName a w) ++ ": bel pin (" ++ show (getBelName a b) ++ "," ++ show pin ++ ") does not resolve back"
              | w <- getWires a
              , (b, pin) <- getWireBelPins a w
              , getBelPinWire a b pin /= Just w
              ]
            , [ "bel " ++ show (getBelName a b) ++ ": pin " ++ show pin ++ " wire does not list it"
              | b <- getBels a
              , pin <- getBelPins a b
              , Just w <- [getBelPinWire a b pin]
              , (b, pin) `notElem` getWireBelPins a w
              ]
            , [ "pip " ++ show (getPipName a p) ++ " missing from source downhill or destination uphill"
              | p <- sampled 1000 (getPips a)
              , p `notElem` getPipsDownhill a (getPipSrcWire a p)
                  || p `notElem` getPipsUphill a (getPipDstWire a p)
              ]
            ]

    -- buckets -------------------------------------------------------------

    buckets =
        concat
            [ [ "bel " ++ show (getBelName a b) ++ " not in its own bucket"
              | b <- getBels a
              , let bucket = getBelBucketForBel a b
              , Just bucketBels <- [M.lookup bucket bucketMap]
              , b `notElem` bucketBels
              ]
            , [ "bucket/cell-type mismatch: " ++ show (getBelName a b)
              | b <- getBels a
              , let bucket = getBelBucketForBel a b
              , getBelBucketForCellType a (getBelType a b) /= bucket
              ]
            , [ "empty bucket for cell type " ++ show t
              | t <- getCellTypes a
              , null (getBelsInBucket a (getBelBucketForCellType a t))
              ]
            ]
      where
        bucketMap = M.fromListWith (++) [(getBelBucketForBel a b, [b]) | b <- getBels a]
