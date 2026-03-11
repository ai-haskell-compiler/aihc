import Data.Peano ( Peano )
import Data.List  ( genericLength )

import Test.Tasty.Bench ( bench, bgroup, defaultMain, nf )

main :: IO ()
main = defaultMain
  [ bgroup "length"
    [ bench "Peano" $ nf (bench_length $ 10 ^ 6) (5 :: Peano)
    , bench "Int"   $ nf (bench_length $ 10 ^ 6) (5 :: Int)
    ]
  ]

bench_length :: (Enum a, Num a, Ord a) => Int -> a -> Bool
bench_length n m =
  (genericLength (replicate n True) >= m)
  ==
  (n >= fromEnum m)
