module Main where

import qualified Cases as A
import Criterion.Main
import qualified Data.Text as B
import Prelude

main :: IO ()
main =
  defaultMain
    $ [ bench "camelize" $ nf A.camelize $! B.replicate 100 "Abc 123 / dsf asdf ;lkj. "
      ]
