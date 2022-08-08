{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A module of finite data, and their compressed.
module Finite where

import Control.Exception (assert)
import Control.Monad (unless, when)
import Control.Monad.Primitive (PrimMonad (PrimState))
import Data.Bits
import qualified Data.ByteString as B
import Data.Foldable
import Data.Hashable
import qualified Data.List as List
import Data.Primitive
import qualified Data.Primitive.MutVar as MutVar
import Debug.Trace
import Prelude hiding (lookup)

type MArray m = MutableByteArray (PrimState m)

class Finite a where
  group :: a -> [Int]

instance Finite Int where
  group a =
    if
        | a == 0 -> [-1, 1]
        | a == -1 -> [-1, -1]
        | otherwise -> [a]
  {-# INLINE group #-}

instance Finite [Int] where
  group a = a
  {-# INLINE group #-}

instance Finite B.ByteString where
  group b = toWords b
   where
    toWords :: B.ByteString -> [Int]
    toWords !bs
      | B.null bs = []
      | otherwise =
          ( List.foldl'
              (\a r' -> a `unsafeShiftL` 7 .|. idx r')
              0
              [0 .. x - 1]
          ) :
          toWords (B.drop x bs)
     where
      x = min 9 t
      idx = fromIntegral . B.index bs
      t = B.length bs
  {-# INLINE group #-}

{- Fourth try is the charm.

  Idea:

  1. Encode all data as zero-less words.
  2. Save entries in array of type

    .|0|C|W|W|W..|.
     |<--- n --->| = |Ws| + 2
-}

data Counter m a = MkCounter
  { array :: !(MutableByteArray (PrimState m))
  , entries :: !(MutVar.MutVar (PrimState m) [(a, Int)])
  }

new :: PrimMonad m => Int -> m (Counter m a)
new n = do
  t <- newByteArray (n * sizeOf (0 :: Int))
  -- array starts as zero --
  setByteArray t 0 n (0 :: Int)
  e <- MutVar.newMutVar []
  pure (MkCounter t e)
{-# INLINE new #-}

count :: forall a m. (Finite a, PrimMonad m) => Counter m a -> a -> m ()
count (MkCounter t e) a = do
  go (h `mod` searchSpace)
 where
  ws = group a
  wl = length ws
  h = hash ws
  n = wl + 2
  searchSpace = size - n
  size = sizeofMutableByteArray t `quot` sizeOf (0 :: Int)

  go :: Int -> m ()
  go !i = do
    j <- (+ 1) <$> findZeroFrom i
    v <- readByteArray @Int t j
    if
        | v == 0 -> do
            findFirstNonZeroFrom t (j + 1) (j + n) >>= \case
              (-1) -> do
                writeAllByteArray t j (1 : ws)
                MutVar.modifyMutVar' e ((a, j) :)
              k -> do
                go k
        | otherwise ->
            findFirstDiffFrom t (j + 1) ws >>= \case
              (-1) -> do
                writeByteArray t j (v + 1)
              k -> go k
  findZeroFrom !i = do
    v <- readByteArray @Int t i
    if v == 0
      then return i
      else findZeroFrom ((i + 1) `mod` searchSpace)
{-# INLINE count #-}

toList :: PrimMonad m => Counter m a -> m [(a, Int)]
toList (MkCounter t e) = do
  mapM (\(a, i) -> (a,) <$> readByteArray t i) =<< MutVar.readMutVar e
{-# INLINE toList #-}

listEntries :: PrimMonad m => Counter m a -> m [(Int, [Int])]
listEntries (MkCounter t _) =
  go [] 0
 where
  size = sizeofMutableByteArray t `quot` sizeOf (0 :: Int)
  go s !i
    | i == size = return s
    | otherwise =
        readByteArray t i >>= \case
          0 -> go s (i + 1)
          v -> do
            (ba, k) <- readByteArrayUntilZero id (i + 1)
            go ((v, ba) : s) (k + 1)

  readByteArrayUntilZero s !i = do
    readByteArray t i >>= \case
      0 -> return (s [], i + 1)
      v -> readByteArrayUntilZero (s . (v :)) (i + 1)
{-# INLINE listEntries #-}

incrByteArray :: PrimMonad m => MutableByteArray (PrimState m) -> Int -> m ()
incrByteArray t !i = do
  v <- readByteArray t i
  writeByteArray t i (v + 1 :: Int)
{-# INLINE incrByteArray #-}

writeAllByteArray ::
  PrimMonad m =>
  MutableByteArray (PrimState m) ->
  Int ->
  [Int] ->
  m ()
writeAllByteArray h = go
 where
  go !i = \case
    [] -> return ()
    x : xs -> do
      writeByteArray h i x
      writeAllByteArray h (i + 1) xs
{-# INLINE writeAllByteArray #-}

findFirstNonZeroFrom ::
  PrimMonad m =>
  MutableByteArray (PrimState m) ->
  Int ->
  Int ->
  m Int
findFirstNonZeroFrom h i k = go i
 where
  go !j
    | j == k = return (-1)
    | otherwise = do
        v <- readByteArray h j
        if
            | v == (0 :: Int) -> go (j + 1)
            | otherwise -> return j
{-# INLINE findFirstNonZeroFrom #-}

findFirstDiffFrom ::
  PrimMonad m =>
  MutableByteArray (PrimState m) ->
  Int ->
  [Int] ->
  m Int
findFirstDiffFrom h i = \case
  [] -> return (-1)
  x : xs -> do
    v <- readByteArray h i
    if
        | v == x -> go (i + 1) xs
        | otherwise -> return i
 where
  go !j = \case
    [] -> return (-1)
    x : xs -> do
      v <- readByteArray h j
      if
          | v == x -> go (j + 1) xs
          | otherwise -> return j
{-# INLINE findFirstDiffFrom #-}

toListOfInt ::
  PrimMonad m =>
  MutableByteArray (PrimState m) ->
  m [Int]
toListOfInt t = go 0
 where
  size = sizeofMutableByteArray t `quot` sizeOf (0 :: Int)
  go !i
    | i == size = return []
    | otherwise = do
        x <- readByteArray t i
        (x :) <$> go (i + 1)

-- test :: [[Int]] -> IO ()
-- test ints = do
--   c@(MkCounter t _) <- new 20
--   print =<< toListOfInt t
--   forM_ ints $ \i -> do
--     count c i
--     print =<< toListOfInt t
--   print =<< Finite.toList c
