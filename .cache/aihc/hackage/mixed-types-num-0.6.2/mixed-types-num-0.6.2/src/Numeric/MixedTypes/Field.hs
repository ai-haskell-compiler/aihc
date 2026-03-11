{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  Numeric.MixedType.Field
    Description :  Bottom-up typed division
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

-}

module Numeric.MixedTypes.Field
(
  -- * Field
  CanAddSubMulDivBy, Field, OrderedField, OrderedCertainlyField
  -- * Division
  , module Numeric.MixedTypes.Div
)
where

import Numeric.MixedTypes.PreludeHiding
-- import qualified Prelude as P

import Numeric.CollectErrors ( CN )
-- import qualified Numeric.CollectErrors as CN

import Numeric.MixedTypes.Literals
-- import Numeric.MixedTypes.Bool
-- import Numeric.MixedTypes.Eq
import Numeric.MixedTypes.Ord
-- import Numeric.MixedTypes.MinMaxAbs
-- import Numeric.MixedTypes.AddSub
import Numeric.MixedTypes.Ring
import Numeric.MixedTypes.Div
import Numeric.MixedTypes.Power

{----- Field -----}

type CanAddSubMulDivBy t s =
  (CanAddSubMulBy t s, CanAddSubMulBy t s, CanDivBy t s)

class
  (Ring t,
   HasRationalsWithSample t,
   CanPowBy t Integer, CanPowBy t Int,
   CanDivSameType t, CanRecipSameType t,
   CanAddSubMulDivBy t Rational,
   CanAddSubMulDivBy t Integer,
   CanAddSubMulDivBy t Int
  )
  =>
  Field t

instance Field Rational
instance Field (CN Rational)

class
  (Field t, OrderedRing t, HasOrder t Rational, HasOrder t Rational)
  => OrderedField t

instance OrderedField Rational
instance OrderedField (CN Rational)

class
  (Field t, OrderedCertainlyRing t, HasOrderCertainly t Rational, HasOrderCertainly t Rational)
  => OrderedCertainlyField t

instance OrderedCertainlyField Rational
instance OrderedCertainlyField (CN Rational)
