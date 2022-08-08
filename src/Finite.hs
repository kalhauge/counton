{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

-- | A module of finite data, and their compressed.
module Finite where

import Control.Monad.Primitive (PrimMonad (PrimState))
import Data.Bits
import qualified Data.ByteString as B
import Data.Hashable
import qualified Data.List as List
import Data.Primitive
import qualified Data.Primitive.MutVar as MutVar
import Prelude hiding (lookup)

type MArray m = MutableByteArray (PrimState m)

class Finite a where
  group :: a -> [Int]

instance Finite Int where
  group a = [a]
  {-# INLINE group #-}

instance Finite B.ByteString where
  group b = toWords b
   where
    toWords :: B.ByteString -> [Int]
    toWords !bs
      | B.null bs = []
      | otherwise =
          List.foldl'
            (\a r' -> a `unsafeShiftL` 8 .|. idx r')
            0
            [0 .. x - 1] :
          toWords (B.drop x bs)
     where
      x = min 8 t
      idx = fromIntegral . B.index bs
      t = B.length bs
  {-# INLINE group #-}

{- Third try is the charm.

  Idea:

  1. Encode all data as zero terminated words.
  2. Save them under their hash.

    ... | words | 0 | count | ...

  3. To Insert: Lookup a position in the array.

    - If it is 0 then, check that it is not followed
      by a 0, then is either

          V           V   V
      ... 0|X|X|X|X|X|0|X|0 ...

      Continue right until you find a sequence equal to
      your sequence, terminated by a zero, followed by a count.
      Or a space great enough to fit your
      sequence. (If you go too long consider resizing the array).

      Problem, how do we differentiate the cases:

        ------+
              V
      ... W|0|C|0|W|0|C ...
      ... 0|0|W|0|C|0|0 ...

      Solution, save all single cases in an IntCounter.

      ... W|W|0|C|0|W|W|0|C ...
      ... 0|0|W|0|C|0|0 ...

      ... 0|0|W|0|C|0|0 ...

-}

data Counter m a = MkCounter
  { intCounter :: !(MutableByteArray (PrimState m))
  , intEntries :: !(MutVar.MutVar (PrimState m) [(a, Int)])
  , stringCounter :: !(MutableByteArray (PrimState m))
  , stringEntries :: !(MutVar.MutVar (PrimState m) [(a, Int)])
  }

new :: PrimMonad m => Int -> m (Counter m a)
new n = do
  ic <- newByteArray (n * 2 * sizeOf (0 :: Int))
  setByteArray ic 0 n (0 :: Int)
  ce <- MutVar.newMutVar []

  t <- newByteArray (n * sizeOf (0 :: Int))
  setByteArray t 0 n (0 :: Int)
  te <- MutVar.newMutVar []

  pure (MkCounter ic ce t te)
{-# INLINE new #-}

count :: (Finite a, PrimMonad m) => Counter m a -> a -> m ()
count (MkCounter ic ce t te) a = do
  case xs' of
    [] -> error "Not Implemented Yet"
    [k] -> go (unsafeShiftL k 1 `mod` size)
     where
      -- From IntCounter, but use count as sentinel to avoid (-1) error
      size = sizeofMutableByteArray t `quot` sizeOf (0 :: Int)
      go !i = do
        key <- readByteArray ic i
        v <- readByteArray ic (i + 1)
        if
            -- slot is empty
            | v == (0 :: Int) -> do
                MutVar.modifyMutVar ce ((a, i + 1) :)
                writeByteArray ic i k
                writeByteArray ic (i + 1) (1 :: Int)
            -- slot is filled
            | key == k -> do
                writeByteArray ic (i + 1) (v + 1 :: Int)
            -- wrong slot
            | otherwise -> do
                go ((i + 2) `rem` size)
    x : xs -> do
      let i = (h `mod` (size - n - 1))
      if i > 2
        then do
          findNextEntryFrom (i - 2) >>= go
        else go i
     where
      go !i = do
        v <- readByteArray t i
        if
            | v == x -> do
                isEntry (i + 1) xs >>= \case
                  (-1) -> incrByteArray t (i + n + 1)
                  j -> findNextEntryFrom j >>= go
            | v == 0 -> do
                checkZeroesWithin (i + 1) (i + n + 2) >>= \case
                  (-1) -> do
                    MutVar.modifyMutVar te ((a, i + n + 1) :)
                    writeAllByteArray t i xs'
                    writeByteArray t (i + n + 1) (1 :: Int)
                  j -> findNextEntryFrom j >>= go
            | otherwise -> do
                isZero ((i - 1) `mod` n) >>= \case
                  True -> go (i + 1)
                  False -> findNextEntryFrom i >>= go

      size = sizeofMutableByteArray t `quot` sizeOf (0 :: Int)
      findNextEntryFrom !j = do
        isZero j >>= \case
          True -> return ((j + 2) `mod` (size - n - 2))
          False -> findNextEntryFrom ((j + 1) `mod` (size - n - 2))

      checkZeroesWithin !i !j = do
        isZero i >>= \case
          True
            | (i + 1) /= j -> checkZeroesWithin (i + 1) j
            | otherwise -> return (-1)
          False -> return (i - 1)

      isZero j =
        (== (0 :: Int)) <$> readByteArray t j

      isEntry !i ys = do
        v <- readByteArray t i
        case ys of
          [] | v == 0 -> return (-1)
          y : ys' | v == y -> isEntry (i + 1) ys'
          _ -> return i
 where
  xs' = group a
  h = hash xs'
  n = length xs'
{-# INLINE count #-}

toList :: PrimMonad m => Counter m a -> m [(a, Int)]
toList (MkCounter ic ce t te) = do
  ics <- mapM (\(a, i) -> (a,) <$> readByteArray ic i) =<< MutVar.readMutVar ce
  ts <- mapM (\(a, i) -> (a,) <$> readByteArray t i) =<< MutVar.readMutVar te
  return (ics ++ ts)
{-# INLINE toList #-}

-- listEntries :: PrimMonad m => Counter m a -> m [([Int], Int)]
-- listEntries (MkCounter ic _ t _) = do
--   error "Broken"
--   -- lst <- IntCounter.toList ic
--   go [] lst) 0
--  where
--   size = sizeofMutableByteArray t `quot` sizeOf (0 :: Int)
--   go s !i
--     | i == size = return s
--     | otherwise =
--         readByteArray t i >>= \case
--           0 -> go s (i + 1)
--           v -> do
--             (ba, k) <- readByteArrayUntilZero (v :) (i + 1)
--             x <- readByteArray t k
--             go ((ba, x) : s) (k + 1)
--
--   readByteArrayUntilZero s !i = do
--     readByteArray t i >>= \case
--       0 -> return (s [], i + 1)
--       v -> readByteArrayUntilZero (s . (v :)) (i + 1)
-- {-# INLINE listEntries #-}

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
