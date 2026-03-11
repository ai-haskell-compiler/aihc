{- |
This module contains types that may be used
as sample rate type in a dimensional signal.
-}
module Synthesizer.Dimensional.Rate where

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

import qualified Synthesizer.Utility as Util

{-
import NumericPrelude.Numeric
import NumericPrelude.Base as P
-}


{- |
This type does not store a sample rate.
It just provides a phantom type parameter
which asserts a common sample rate among several signals.
-}
data Phantom s = Phantom

{- |
Store the sample rate that a signal is sampled with.
-}
newtype Actual rate = Actual rate

type Dimensional u t = Actual (DN.T (Dim.Recip u) t)


{-# INLINE common #-}
-- common :: Eq rate => String -> Actual rate -> Actual rate -> Actual rate
common :: Eq rate => String -> rate -> rate -> rate
common funcName =
   Util.common ("Sample rates differ in " ++ funcName)
