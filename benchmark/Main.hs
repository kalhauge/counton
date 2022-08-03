module Main where

-- base
import qualified Data.List as List

-- bytestring
import Data.ByteString as B
import Data.ByteString.Char8 as C

-- citerion
import Criterion.Main

-- deepseq
import Control.DeepSeq (NFData, (<$!!>))

-- counton
import qualified Count

main :: IO ()
main = do
  kjvbible <- C.words <$!!> B.readFile "data/kjvbible.txt"

  defaultMain
    [ bgroup "count" (Count.benchmark kjvbible)
    ]
