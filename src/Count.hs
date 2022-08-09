{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Count where

-- base
import Control.Monad.ST (ST, runST)
import Data.Foldable (traverse_)
import qualified Data.List as List
import Data.Typeable

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
import Data.Hashable (Hashable (..))

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
import qualified Data.IntMap.Strict as IntMap

import qualified Data.ByteString as B
import qualified Finite
import qualified IntCounter

viaSorted :: Ord a => [a] -> [(a, Int)]
viaSorted = Count.Repeated.best . Count.Sort.best
{-# INLINE viaSorted #-}

viaStrictHashMap :: (Eq a, Hashable a) => [a] -> [(a, Int)]
viaStrictHashMap =
  HashMap.Strict.toList
    . HashMap.Strict.fromListWith (+)
    . List.map (\a -> (a, 1))
{-# INLINE viaStrictHashMap #-}

type HashTable m k v =
  H.Dictionary (H.PrimState m) VM.MVector k UM.MVector v

viaVectorHashMap :: forall a. (Eq a, Hashable a) => [a] -> [(a, Int)]
viaVectorHashMap items = runST go
 where
  go :: forall s. ST s [(a, Int)]
  go = do
    t :: HashTable (ST s) k v <- H.initialize 1
    let incr = H.alter t (Just . maybe 1 (+ 1))
    traverse_ incr items
    H.toList t
{-# INLINE viaVectorHashMap #-}

type IntHashTable m v =
  H.Dictionary (H.PrimState m) UM.MVector Int UM.MVector v

viaUnboxedVectorHashMap :: [Int] -> [(Int, Int)]
viaUnboxedVectorHashMap items = runST go
 where
  go :: forall s. ST s [(Int, Int)]
  go = do
    t :: IntHashTable (ST s) v <- H.initialize 1
    let incr = H.alter t (Just . maybe 1 (+ 1))
    traverse_ incr items
    H.toList t
{-# INLINE viaUnboxedVectorHashMap #-}

viaIntCounter :: [Int] -> [(Int, Int)]
viaIntCounter items = runST go
 where
  go :: forall s. ST s [(Int, Int)]
  go = do
    h :: IntCounter.IntCounter (ST s) <- IntCounter.new 60000
    traverse_ (IntCounter.count h) items
    IntCounter.toList h
{-# INLINE viaIntCounter #-}

viaFinite :: forall a. (Finite.Finite a) => [a] -> [(a, Int)]
viaFinite items = runST go
 where
  go :: forall s. ST s [(a, Int)]
  go = do
    h :: Finite.Counter (ST s) a <- Finite.new 120000
    traverse_ (Finite.count h) items
    Finite.toList h
{-# INLINE viaFinite #-}

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

viaIntMap :: [Int] -> [(Int, Int)]
viaIntMap =
  IntMap.toList
    . IntMap.fromListWith (+)
    . List.map (\a -> (a, 1))
{-# INLINE viaIntMap #-}

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

best :: (Eq a, Hashable a) => [a] -> [(a, Int)]
best = viaVectorHashMap
{-# INLINE best #-}

benchmark :: forall a. (NFData a, Typeable a, Ord a, Hashable a, D.Grouping a, Finite.Finite a) => [a] -> [Benchmark]
benchmark items =
  [ b "lengthBaseline" List.length
  , b "viaVectorHashMap" viaVectorHashMap
  , b "viaStrictHashMap" viaStrictHashMap
  , b "viaStrictMap" viaStrictMap
  , b "viaFinite" viaFinite
  , b "viaDiscrimination" viaDiscrimination
  ]
    ++ ( case eqT @[a] @[Int] of
          Just Refl ->
            [ b "viaIntMap" viaIntMap
            , b "viaIntCounter" viaIntCounter
            , b "viaUnboxedVectorHash" viaUnboxedVectorHashMap
            ]
          Nothing -> []
       )
 where
  b :: forall b. NFData b => String -> ([a] -> b) -> Benchmark
  b n f = bench n $ nf f items
  {-# INLINE b #-}
{-# INLINE benchmark #-}
