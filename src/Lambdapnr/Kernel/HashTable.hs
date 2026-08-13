{- | Deterministic-iteration dictionaries and pools.

The C++ kernel uses open-addressing hash tables (@hashlib.h@) whose
iteration order is deterministic but hash-dependent. The Haskell port
deliberately uses ordered maps instead: iteration follows key order,
which is deterministic across runs, architectures and word sizes. For
the netlist/checksum folds this is provably equivalent (the C++
checksum sums per-entry values, and addition is commutative); for
algorithm phases whose output goldens were captured from the C++
iteration order, the port either replicates @mkhash@ (documented in
SPECIFICATION.md §7.6) or treats goldens as soft.
-}
module Lambdapnr.Kernel.HashTable (
    Dict,
    Pool,
    dictEmpty,
    dictSingleton,
    dictInsert,
    dictDelete,
    dictLookup,
    dictMember,
    dictSize,
    dictFromList,
    dictToList,
    dictKeys,
    dictFoldlWithKey',
    poolEmpty,
    poolInsert,
    poolMember,
    poolFromList,
    poolToList,
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

-- | Ordered dictionary (ascending key order on iteration).
type Dict k v = Map k v

-- | Ordered set.
type Pool a = Set a

dictEmpty :: Dict k v
dictEmpty = M.empty

dictSingleton :: k -> v -> Dict k v
dictSingleton = M.singleton

dictInsert :: (Ord k) => k -> v -> Dict k v -> Dict k v
dictInsert = M.insert

dictDelete :: (Ord k) => k -> Dict k v -> Dict k v
dictDelete = M.delete

dictLookup :: (Ord k) => k -> Dict k v -> Maybe v
dictLookup = M.lookup

dictMember :: (Ord k) => k -> Dict k v -> Bool
dictMember = M.member

dictSize :: Dict k v -> Int
dictSize = M.size

dictFromList :: (Ord k) => [(k, v)] -> Dict k v
dictFromList = M.fromList

dictToList :: Dict k v -> [(k, v)]
dictToList = M.toList

dictKeys :: Dict k v -> [k]
dictKeys = M.keys

dictFoldlWithKey' :: (a -> k -> v -> a) -> a -> Dict k v -> a
dictFoldlWithKey' = M.foldlWithKey'

poolEmpty :: Pool a
poolEmpty = S.empty

poolInsert :: (Ord a) => a -> Pool a -> Pool a
poolInsert = S.insert

poolMember :: (Ord a) => a -> Pool a -> Bool
poolMember = S.member

poolFromList :: (Ord a) => [a] -> Pool a
poolFromList = S.fromList

poolToList :: Pool a -> [a]
poolToList = S.toList
