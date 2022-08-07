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
import qualified IntCounter
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

data Counter m = MkCounter
  { intCounter :: !(IntCounter.IntCounter m)
  , stringCounter :: !(MutableByteArray (PrimState m))
  }

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

new :: PrimMonad m => Int -> m (Counter m)
new n = do
  i <- IntCounter.new n
  a <- newByteArray (n * sizeOf (0 :: Int))
  setByteArray a 0 n (0 :: Int)
  pure (MkCounter i a)
{-# INLINE new #-}

count :: PrimMonad m => Counter m -> [Int] -> m ()
count (MkCounter _ _) [] = return ()
count (MkCounter ih _) [i] =
  IntCounter.count ih i
count (MkCounter _ t) xs'@(x : xs) = do
  let i = (h `mod` (size - n - 1))
  if i > 2
    then do
      -- Check if its the end of another entry
      isZero (i - 2) >>= \case
        True -> go i
        False -> do
          -- Or if it's out in nowhere
          isZero (i - 1) >>= \case
            True -> go i
            False -> findNextEntryFrom i >>= go
    else go i
 where
  h = hash xs'
  n = length xs'
  size = sizeofMutableByteArray t `quot` sizeOf (0 :: Int)
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
                writeAllByteArray t i xs'
                writeByteArray t (i + n + 1) (1 :: Int)
              j -> findNextEntryFrom j >>= go
        | otherwise -> do
            isZero ((i - 1) `mod` n) >>= \case
              True -> go (i + 1)
              False -> findNextEntryFrom i >>= go

  checkZeroesWithin !i !j
    | i == j = return (-1)
    | otherwise = do
        isZero i >>= \case
          True -> checkZeroesWithin (i + 1) j
          False -> return (i - 1)

  findNextEntryFrom !j = do
    isZero j >>= \case
      True -> return ((j + 2) `mod` (size - n - 2))
      False -> findNextEntryFrom ((j + 1) `mod` (size - n - 2))

  isZero j =
    (== (0 :: Int)) <$> readByteArray t j

  isEntry !i = \case
    [] -> do
      isZero i >>= \case
        True -> return (-1)
        False -> return i
    y : ys -> do
      v <- readByteArray t i
      if v == y
        then isEntry (i + 1) ys
        else return i
{-# INLINE count #-}

toList :: PrimMonad m => Counter m -> m [([Int], Int)]
toList (MkCounter a t) = do
  lst <- IntCounter.toList a
  go (map (\(a', b) -> ([a'], b)) lst) 0
 where
  size = sizeofMutableByteArray t `quot` sizeOf (0 :: Int)
  go s !i
    | i == size = return s
    | otherwise =
        readByteArray t i >>= \case
          0 -> go s (i + 1)
          v -> do
            (ba, k) <- readByteArrayUntilZero (v :) (i + 1)
            x <- readByteArray t k
            go ((ba, x) : s) (k + 1)

  readByteArrayUntilZero s !i = do
    readByteArray t i >>= \case
      0 -> return (s [], i + 1)
      v -> readByteArrayUntilZero (s . (v :)) (i + 1)
{-# INLINE toList #-}
