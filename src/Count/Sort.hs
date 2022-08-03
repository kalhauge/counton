module Count.Sort where

-- base
import qualified Data.List as List

-- containers
import qualified Data.Set as Set

-- deepseq
import Control.DeepSeq (NFData)

-- criterion
import Criterion (Benchmark, bench, nf)

listsort :: Ord a => [a] -> [a]
listsort = List.sort
{-# INLINE listsort #-}

{- | Set sort, sorts the elements using a set, however it looses any duplicates.
 Mostly kept as an interesting comparation point.
-}
setsort :: Ord a => [a] -> [a]
setsort = Set.toAscList . Set.fromList
{-# INLINE setsort #-}

best :: Ord a => [a] -> [a]
best = listsort
{-# INLINE best #-}

benchmark :: (NFData a, Ord a) => [a] -> [Benchmark]
benchmark items =
  [ b "listsort" listsort
  , b "setsort" setsort
  ]
 where
  b n f = bench n $ nf f items
{-# INLINE benchmark #-}
