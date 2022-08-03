{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Count where

-- base

import Control.Monad.ST (ST, runST)
import Data.Foldable (traverse_)
import qualified Data.List as List

-- deepseq
import Control.DeepSeq (NFData)

-- criterion
import Criterion (Benchmark, bench, nf)

-- containers
import qualified Data.Map.Lazy as Map.Lazy
import qualified Data.Map.Strict as Map.Strict

-- discrimination
import qualified Data.Discrimination as D

-- unorderd-containers
import qualified Data.HashMap.Strict as HashMap.Strict

-- hashable
import Data.Hashable (Hashable)

-- primitive
import qualified Data.Primitive.MutVar as MutVar

-- vector-hashtable
import qualified Data.Vector.Hashtables as H

-- vector
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed.Mutable as UM

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

type HashTable k v =
  H.Dictionary (H.PrimState IO) VM.MVector k UM.MVector v

viaVectorHashMap :: forall a. (Hashable a) => [a] -> [(a, Int)]
viaVectorHashMap items = runST go
 where
  go :: forall s. ST s [(a, Int)]
  go = do
    t :: H.Dictionary (H.PrimState (ST s)) VM.MVector a UM.MVector Int <- H.initialize 1
    let incr = H.alter t (Just . maybe 1 (+ 1))
    traverse_ incr items
    H.toList t
{-# INLINE viaVectorHashMap #-}

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

viaDiscrimination :: D.Grouping a => [a] -> [(a, Int)]
viaDiscrimination = runCounting D.grouping
 where
  runCounting (D.Group m) as = runST do
    l <- MutVar.newMutVar []
    f <- m $ \a -> do
      counter <- MutVar.newMutVar 1
      MutVar.modifyMutVar' l ((a, counter) :)
      return \_ -> MutVar.modifyMutVar' counter (+ 1)
    traverse_ (\a -> f a a) as
    mapM (traverse MutVar.readMutVar) =<< MutVar.readMutVar l
{-# INLINE viaDiscrimination #-}

best :: Hashable a => [a] -> [(a, Int)]
best = viaVectorHashMap
{-# INLINE best #-}

benchmark :: (NFData a, Ord a, Hashable a, D.Grouping a) => [a] -> [Benchmark]
benchmark items =
  [ b "viaVectorHashMap" viaVectorHashMap
  , b "viaStrictHashMap" viaStrictHashMap
  , b "viaStrictMap" viaStrictMap
  , b "viaLazyMap" viaLazyMap
  , b "viaDiscrimination" viaDiscrimination
  -- , b "viaSorted" viaSorted
  -- , bgroup "sort" (Count.Sort.benchmark items)
  -- , bgroup "repeated" (Count.Repeated.benchmark (Count.Sort.best items))
  ]
 where
  b n f = bench n $ nf f items
{-# INLINE benchmark #-}
