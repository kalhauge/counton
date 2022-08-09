{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

-- data
import Data.Foldable

-- bytestring
import Data.ByteString as B
import Data.ByteString.Char8 as C

-- deepseq
import Control.DeepSeq ((<$!!>))

-- counton
import qualified Count
import Data.Char (toLower)

main :: IO ()
main = do
  kjvbible :: [B.ByteString] <- {-# SCC readkjvbible #-} C.words . C.map (toLower) <$!!> B.readFile "data/kjvbible.txt"
  forM_ (Count.viaFinite kjvbible) $ \(a, k) -> do
    Prelude.putStrLn (C.unpack a <> " " <> show k)
