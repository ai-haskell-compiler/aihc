module Numeric.BLAS.Matrix.Modifier (
   Modi.Transposition(..),
   Modi.transposeOrder,
   Modi.Conjugation(..),
   conjugatedOnRowMajor,
   Modi.Inversion(..),
   ) where

import qualified Numeric.Netlib.Modifier as Modi
import Numeric.Netlib.Modifier (Conjugation(NonConjugated, Conjugated))
import Numeric.Netlib.Layout (Order(RowMajor,ColumnMajor))


{-# DEPRECATED conjugatedOnRowMajor "For internal use in package 'lapack'." #-}
conjugatedOnRowMajor :: Order -> Conjugation
conjugatedOnRowMajor RowMajor = Conjugated
conjugatedOnRowMajor ColumnMajor = NonConjugated
