{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad (zipWithM)
import Count
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (toLower)
import Data.Foldable
import qualified Data.List as List
import Test.Hspec

main :: IO ()
main = hspec do
  describe "counting" do
    kjvbible :: [B.ByteString] <-
      C.words . C.map (toLower)
        <$> runIO (B.readFile "data/kjvbible.txt")
    let counts = List.sortOn fst $ Count.viaStrictMap kjvbible

    describe "viaFinite" do
      it "should produce the right results" do
        let result = List.sortOn fst (Count.viaFinite kjvbible)
        forM_ (zip3 [0 :: Int ..] result counts) $ \(i, r, c) ->
          (i, r) `shouldBe` (i, c)
