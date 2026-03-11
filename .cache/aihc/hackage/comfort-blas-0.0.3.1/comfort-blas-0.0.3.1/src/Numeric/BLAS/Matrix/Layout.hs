module Numeric.BLAS.Matrix.Layout (
   Order(..),
   flipOrder,
   transposeFromOrder,
   ) where

import Numeric.Netlib.Layout (Order(..), flipOrder)


{-# DEPRECATED transposeFromOrder "For internal use in package 'lapack'." #-}
transposeFromOrder :: Order -> Char
transposeFromOrder RowMajor = 'T'
transposeFromOrder ColumnMajor = 'N'
