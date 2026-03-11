module Main where

import Criterion.Main (defaultMain, bgroup, bench, nf, nfIO, whnf)
import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.Morpheus as M
import qualified Naive as N

main :: IO ()
main = do
  !ar <- LA.rand 1000 1000
  let !ac = LA.tr ar

  defaultMain [
    bgroup "Activation" [
        bench "sigmoid"            $ nf M.sigmoid ar
        , bench "sigmoid gradient" $ nf M.sigmoidGradient ar
        ]
    , bgroup "Matrix Reduce" [
        bench "sum by columns - row major"          $ nf M.columnSum ar
        , bench "sum by columns - column major"       $ nf M.columnSum ac
        , bench "sum by rows - row major"               $ nf M.rowSum ar
        , bench "sum by rows - column major"          $ nf M.rowSum ac
        , bench "max index in columns - row major"    $ nf M.columnMaxIndex ar
        , bench "max index in columns - column major" $ nf M.columnMaxIndex ac
        , bench "max index in rows - row major"       $ nf M.rowMaxIndex ar
        , bench "max index in rows - column major"    $ nf M.rowMaxIndex ac
        ]
    , bgroup "Matrix Reduce (naive)" [
        bench "max index in columns - row major"    $ nf (N.reduceByColumnsV (fromIntegral . LA.maxIndex) ) ar
        , bench "max index in columns - column major" $ nf (N.reduceByColumnsV (fromIntegral . LA.maxIndex) ) ac
        , bench "max index in rows - row major"         $ nf (N.reduceByRowsV (fromIntegral . LA.maxIndex) ) ar
        , bench "max index in rows - column major"    $ nf (N.reduceByRowsV (fromIntegral . LA.maxIndex) ) ac
        ]
    ]
