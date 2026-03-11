module Naive
(
  reduceByRowsV
  , reduceByColumnsV
)

where

import qualified Numeric.LinearAlgebra as LA

type Matrix = LA.Matrix LA.R
type Vector = LA.Vector LA.R
type R = LA.R

reduceByRowsV :: (Vector -> R) -> Matrix -> Vector
reduceByRowsV f = LA.vector . map f . LA.toRows

reduceByColumnsV :: (Vector -> R) -> Matrix -> Vector
reduceByColumnsV f = LA.vector . map f . LA.toColumns
