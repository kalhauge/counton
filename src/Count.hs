module Count where

-- base
import qualified Data.List as List

-- deepseq
import Control.DeepSeq (NFData)

-- criterion
import Criterion (Benchmark, bench, bgroup, nf)

-- containers
import qualified Data.Map.Lazy as Map.Lazy
import qualified Data.Map.Strict as Map.Strict

-- unorederd-containers
import qualified Data.HashMap.Strict as HashMap.Strict

-- hashable
import Data.Hashable (Hashable)

-- counton
import qualified Count.Repeated
import qualified Count.Sort

viaSorted :: Ord a => [a] -> [(a, Int)]
viaSorted = Count.Repeated.best . Count.Sort.best
{-# INLINE viaSorted #-}

viaStrictHashMap :: Hashable a => [a] -> [(a, Int)]
viaStrictHashMap =
  HashMap.Strict.toList
    . HashMap.Strict.fromListWith (+)
    . List.map (\a -> (a, 1))
{-# INLINE viaStrictHashMap #-}

viaStrictMap :: Ord a => [a] -> [(a, Int)]
viaStrictMap =
  Map.Strict.toList
    . Map.Strict.fromListWith (+)
    . List.map (\a -> (a, 1))
{-# INLINE viaStrictMap #-}

viaLazyMap :: Ord a => [a] -> [(a, Int)]
viaLazyMap =
  Map.Lazy.toList
    . Map.Lazy.fromListWith (+)
    . List.map (\a -> (a, 1))
{-# INLINE viaLazyMap #-}

best :: Ord a => [a] -> [(a, Int)]
best = viaSorted
{-# INLINE best #-}

benchmark :: (NFData a, Ord a, Hashable a) => [a] -> [Benchmark]
benchmark items =
  [ b "viaStrictHashMap" viaStrictHashMap
  , b "viaStrictMap" viaStrictMap
  , b "viaLazyMap" viaLazyMap
  -- , b "viaSorted" viaSorted
  -- , bgroup "sort" (Count.Sort.benchmark items)
  -- , bgroup "repeated" (Count.Repeated.benchmark (Count.Sort.best items))
  ]
 where
  b n f = bench n $ nf f items
{-# INLINE benchmark #-}
