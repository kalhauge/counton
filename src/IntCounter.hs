{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO: resize

module IntCounter (IntCounter, new, count, toList) where

import Control.Monad.Primitive
import Data.Bits (Bits (unsafeShiftL))
import Data.Primitive (sizeOf)
import Data.Primitive.ByteArray
import GHC.Exts hiding (toList)
import Prelude hiding (lookup)

newtype IntCounter m = MkCounter (MutableByteArray (PrimState m))

new :: PrimMonad m => Int -> m (IntCounter m)
new n = do
  a <- newByteArray (n * 2 * sizeOf (0 :: Int))
  setByteArray a 0 (n * 2) (-1 :: Int)
  pure (MkCounter a)
{-# INLINE new #-}

count :: forall m. PrimMonad m => IntCounter m -> Int -> m ()
count (MkCounter t) k = do
  let i = unsafeShiftL k 1 `mod` n
  slot <- readByteArray t i
  if
      -- slot is empty
      | slot == (-1 :: Int) -> do
        writeByteArray t i k
        writeByteArray t (i + 1) (1 :: Int)
      -- slot is filled
      | slot == k -> do
        v <- readByteArray t (i + 1)
        writeByteArray t (i + 1) (v + 1 :: Int)
      -- wrong slot
      | otherwise -> do
        go ((i + 2) `rem` n)
 where
  n = sizeofMutableByteArray t `quot` sizeOf (0 :: Int)
  go :: Int -> m ()
  go !i = do
    slot <- readByteArray t i
    if
        -- slot is empty
        | slot == (-1 :: Int) -> do
          writeByteArray t i k
          writeByteArray t (i + 1) (1 :: Int)
        -- slot is filled
        | slot == k -> do
          v <- readByteArray t (i + 1)
          writeByteArray t (i + 1) (v + 1 :: Int)
        -- wrong slot
        | otherwise -> do
          go ((i + 2) `rem` n)
{-# INLINE count #-}

toList :: forall m. PrimMonad m => IntCounter m -> m [(Int, Int)]
toList (MkCounter t) =
  let n = sizeofMutableByteArray t `quot` sizeOf (0 :: Int)
      go :: [(Int, Int)] -> Int -> m [(Int, Int)]
      go s !i
        | i == n = pure s
        | otherwise = do
          slot <- readByteArray t i
          if slot == (-1 :: Int)
            then do
              go s (i + 2)
            else do
              v <- readByteArray t (i + 1)
              go ((slot, v) : s) (i + 2)
   in go [] 0
{-# INLINE toList #-}
