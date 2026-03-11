module Main where

import Criterion.Main

import qualified Data.Vector.Unboxed as V
import qualified Data.Matrix.Unboxed as M

-- | a 110 by 100 matrix
largeMatrix :: M.Matrix Double
largeMatrix = M.matrix 100 [1 .. 110*100]

main :: IO ()
main = defaultMain
  [ bench "takeDiag largeMatrix" $ nf M.takeDiag largeMatrix
  ]
