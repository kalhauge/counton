{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Count.Repeated where

-- base
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

-- deepseq
import Control.DeepSeq

-- criterion
import Criterion (Benchmark, bench, nf)

{-# INLINE bygroup #-}
bygroup :: Eq a => [a] -> [(a, Int)]
bygroup = List.map extract . List.group
 where
  extract (a : as) = (a, 1 + List.length as)
  extract [] = error "expect all groups to contain atleast one element"

{-# INLINE bynegroup #-}
bynegroup :: Eq a => [a] -> [(a, Int)]
bynegroup = List.map extract . NonEmpty.group
 where
  extract (a NonEmpty.:| as) = (a, 1 + List.length as)

{-# INLINE tightloop #-}
tightloop :: Eq a => [a] -> [(a, Int)]
tightloop = \case
  a : as -> go a 1 as
  [] -> []
 where
  go !b !n = \case
    x : xs
      | b == x -> go x (n + 1) xs
      | otherwise -> (b, n) : go x 1 xs
    [] -> [(b, n)]

{-# INLINE best #-}
best :: Eq a => [a] -> [(a, Int)]
best = tightloop

{-# INLINE benchmark #-}
benchmark :: (Eq a, NFData a) => [a] -> [Benchmark]
benchmark items =
  deepseq
    items
    [ b "bygroup" bygroup
    , b "bynegroup" bynegroup
    , b "tightloop" tightloop
    ]
 where
  b n f = bench n $ nf f items
