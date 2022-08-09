{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

-- data
import qualified Data.List as List
import Data.Word (Word64)
import qualified System.IO as IO

-- bytestring
import Data.ByteString as B
import Data.ByteString.Char8 as C

-- citerion
import Criterion.Main

-- deepseq
import Control.DeepSeq ((<$!!>))

-- discrimitation
import qualified Data.Discrimination as D

-- counton
import qualified Count

-- contravariant

import Data.Bits
import Data.Char (toLower)
import Data.Functor.Contravariant (Contravariant (contramap))

-- Convert a ByteString into a list of words for use in an
-- internal trie
instance D.Grouping B.ByteString where
  grouping = contramap toWords D.grouping
   where
    toWords :: B.ByteString -> [Word64]
    toWords !bs
      | B.null bs = []
      | otherwise = List.foldl' (\a r -> a `shiftL` 8 .|. idx r) 0 [0 .. x - 1] : toWords (B.drop x bs)
     where
      x = min 8 t
      idx = fromIntegral . B.index bs
      t = B.length bs

main :: IO ()
main = do
  kjvbible :: [B.ByteString] <- {-# SCC readkjvbible #-} C.words . C.map (toLower) <$!!> B.readFile "data/kjvbible.txt"
  numbers :: [Int] <- {-# SCC readkjvbible #-} List.map read . List.words <$!!> IO.readFile "data/numbers.txt"
  -- print $ Count.viaFinite numbers
  defaultMain
    [ bgroup "kjvbible" (Count.benchmark kjvbible)
    , bgroup "numbers" (Count.benchmark numbers)
    , bgroup "10xkjvbible" (Count.benchmark (List.concat $ List.replicate 10 kjvbible))
    ]
