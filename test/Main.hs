{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import Count
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (toLower)
import Test.Hspec

main :: IO ()
main = hspec do
  describe "counting" do
    kjvbible :: [B.ByteString] <-
      C.words . C.map (toLower)
        <$> runIO (B.readFile "data/kjvbible.txt")
    let counts = Count.viaStrictMap kjvbible

    describe "viaFinite" do
      it "should produce the right results" do
        Count.viaFinite kjvbible `shouldMatchList` counts
