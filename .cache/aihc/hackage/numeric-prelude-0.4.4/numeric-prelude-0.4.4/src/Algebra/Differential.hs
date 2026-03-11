{-# LANGUAGE RebindableSyntax #-}
module Algebra.Differential where

import qualified Algebra.Ring as Ring


{- |
'differentiate' is a general differentation operation
It must fulfill the Leibnitz condition

>   differentiate (x * y) == differentiate x * y + x * differentiate y

Unfortunately, this scheme cannot be easily extended to more than two variables,
e.g. "MathObj.PowerSeries2".
-}
class Ring.C a => C a where
   differentiate :: a -> a
