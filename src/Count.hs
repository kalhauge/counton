{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Count where

-- base
import Control.Monad.ST (ST, runST)
import Data.Foldable (traverse_)
import qualified Data.List as List
import Data.Typeable
import Data.Bits

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
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- counton
import qualified Count.Repeated
import qualified Count.Sort
import qualified Data.ByteString as B
import qualified Data.IntMap.Strict as IntMap

viaSorted :: Ord a => [a] -> [(a, Int)]
viaSorted = Count.Repeated.best . Count.Sort.best
{-# INLINE viaSorted #-}

viaStrictHashMap :: Hashable a => [a] -> [(a, Int)]
viaStrictHashMap =
  HashMap.Strict.toList
    . HashMap.Strict.fromListWith (+)
    . List.map (\a -> (a, 1))
{-# INLINE viaStrictHashMap #-}

type HashTable m k v =
  H.Dictionary (H.PrimState m) VM.MVector k UM.MVector v

viaVectorHashMap :: forall a. (Hashable a) => [a] -> [(a, Int)]
viaVectorHashMap items = runST go
 where
  go :: forall s. ST s [(a, Int)]
  go = do
    t :: HashTable (ST s) k v <- H.initialize 1
    let incr = H.alter t (Just . maybe 1 (+ 1))
    traverse_ incr items
    H.toList t
{-# INLINE viaVectorHashMap #-}

class Finite a where
  group :: a -> U.Vector Int

instance Finite Int where
  group = U.singleton

instance Finite B.ByteString where
  group = U.fromList . toWords
   where
    toWords :: B.ByteString -> [Int]
    toWords !bs
      | B.null bs = []
      | otherwise = List.foldl' (\a r -> a `shiftL` 8 .|. idx r) 0 [0 .. x - 1] : toWords (B.drop x bs)
     where
      x = min 8 t
      idx = fromIntegral . B.index bs
      t = B.length bs

type UnboxedHashTable m k v =
  H.Dictionary (H.PrimState m) UM.MVector k UM.MVector v

instance (Hashable a, UM.Unbox a) => Hashable (U.Vector a) where
  hashWithSalt i = hashWithSalt i . U.toList
  {-# INLINE hashWithSalt #-}

viaVectorHashMap2 :: forall a. (Finite a, Hashable a) => [a] -> [(a, Int)]
viaVectorHashMap2 items = runST go
 where
  go :: forall s. ST s [(a, Int)]
  go = do
    results <- MutVar.newMutVar ([] :: [(a, U.Vector Int)])
    t1 :: UnboxedHashTable (ST s) Int v <- H.initialize 1
    t2 :: UnboxedHashTable (ST s) (Int, Int) v <- H.initialize 1
    t3 :: UnboxedHashTable (ST s) (Int, Int, Int) v <- H.initialize 1
    rest :: HashTable (ST s) a Int <- H.initialize 1
    -- t4 :: V.Vector (UnboxedHashTable (ST s) (Int, Int, Int, Int) v) <- V. H.initialize 1
    -- t5 :: V.Vector (UnboxedHashTable (ST s) (Int, Int, Int, Int, Int) v) <- V. H.initialize 1
    let incr a g = \case
          Nothing -> do
            MutVar.modifyMutVar' results ((a, g) :)
            pure $ Just 1
          Just n -> pure $ Just (n + 1)
        count a = case U.length g of
          1 -> H.alterM t1 (incr a g) (g `U.unsafeIndex` 0)
          2 -> H.alterM t2 (incr a g) (g `U.unsafeIndex` 0, g `U.unsafeIndex` 1)
          3 -> H.alterM t3 (incr a g) (g `U.unsafeIndex` 0, g `U.unsafeIndex` 1, g `U.unsafeIndex` 2)
          _ -> H.alterM rest (incr a g) a
         where
          g = group a
        xlookup a g = case U.length g of
          1 -> H.lookup' t1 (g `U.unsafeIndex` 0)
          2 -> H.lookup' t2 (g `U.unsafeIndex` 0, g `U.unsafeIndex` 1)
          3 -> H.lookup' t3 (g `U.unsafeIndex` 0, g `U.unsafeIndex` 1, g `U.unsafeIndex` 2)
          _ -> H.lookup' rest a
    traverse_ count items
    mapM (\(a, g) -> (a,) <$> xlookup a g) =<< MutVar.readMutVar results
{-# INLINE viaVectorHashMap2 #-}

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

best :: Hashable a => [a] -> [(a, Int)]
best = viaVectorHashMap
{-# INLINE best #-}

benchmark :: forall a. (NFData a, Finite a, Typeable a, Ord a, Hashable a, D.Grouping a) => [a] -> [Benchmark]
benchmark items =
  [ b "lengthBaseline" List.length
  , b "viaVectorHashMap" viaVectorHashMap
  , b "viaVectorHashMap2" viaVectorHashMap2
  , b "viaStrictHashMap" viaStrictHashMap
  , b "viaStrictMap" viaStrictMap
  , b "viaLazyMap" viaLazyMap
  , b "viaDiscrimination" viaDiscrimination
  -- , b "viaSorted" viaSorted
  -- , bgroup "sort" (Count.Sort.benchmark items)
  -- , bgroup "repeated" (Count.Repeated.benchmark (Count.Sort.best items))
  ]
    ++ ( case eqT @[a] @[Int] of
          Just Refl ->
            [ b "viaIntMap" viaIntMap
            , b "viaUnboxedVectorHash" viaUnboxedVectorHashMap
            ]
          Nothing -> []
       )
 where
  b :: forall b. NFData b => String -> ([a] -> b) -> Benchmark
  b n f = bench n $ nf f items
  {-# INLINE b #-}
{-# INLINE benchmark #-}
