-- |Safe overrides of the methods of class 'Fractional'.
module Incipit.Fractional where

import Data.Eq (Eq)
import Data.Maybe (Maybe (Just, Nothing))
import qualified GHC.Real as Real
import GHC.Real (Fractional)

import Incipit.Integral (safeOp)

-- | Fractional division.
(/) ::
  Eq a =>
  Fractional a =>
  a ->
  a ->
  Maybe a
(/) =
  safeOp (Real./)

-- | Reciprocal fraction.
recip ::
  Eq a =>
  Fractional a =>
  a ->
  Maybe a
recip = \case
  0 ->
    Nothing
  a ->
    Just (Real.recip a)
