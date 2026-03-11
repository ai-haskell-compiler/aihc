module Main where

import Criterion.Main
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.Char
import qualified Data.Char8
import qualified Data.Word8

main :: IO ()
main = do
    input <- S.readFile "bench/Bench.hs"
    defaultMain [ 
        bench "Data.Char"  $ whnf (S8.map Data.Char.toLower) input
      , bench "Data.Char8" $ whnf (S8.map Data.Char8.toLower) input
      , bench "Data.Word8" $ whnf (S.map Data.Word8.toLower) input
      ]
