{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-|
    Module      :  Numeric.MixedType.Ring
    Description :  Ring type classes
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

-}

module Numeric.MixedTypes.Ring
(
  CanAddSubMulBy, Ring, OrderedRing, OrderedCertainlyRing
)
where

import Numeric.MixedTypes.PreludeHiding
-- import qualified Prelude as P
-- import Text.Printf

import Numeric.CollectErrors ( CN )

import Numeric.MixedTypes.Literals
import Numeric.MixedTypes.Bool
import Numeric.MixedTypes.Eq
import Numeric.MixedTypes.Ord
-- import Numeric.MixedTypes.MinMaxAbs
import Numeric.MixedTypes.AddSub
import Numeric.MixedTypes.Mul

{----- Ring -----}

type CanAddSubMulBy t s =
  (CanAddThis t s, CanSubThis t s, CanSub s t, SubType s t ~ t, CanMulBy t s)

class
  (CanNegSameType t, CanAddSameType t, CanSubSameType t, CanMulSameType t,
   HasEq t t,
   HasEq t Integer, CanAddSubMulBy t Integer,
   HasEq t Int, CanAddSubMulBy t Int,
   HasIntegersWithSample t) => Ring t

instance Ring Integer
instance Ring (CN Integer)
instance Ring Rational
instance Ring (CN Rational)

class
  (Ring t
  , HasEq t t
  , HasEq t Int, HasEq t Integer
  , HasOrder t t
  , HasOrder t Int, HasOrder t Integer)
  => OrderedRing t

instance OrderedRing Integer
instance OrderedRing (CN Integer)
instance OrderedRing Rational
instance OrderedRing (CN Rational)

class
  (Ring t
  , HasEqCertainly t t
  , HasEqCertainly t Int, HasEq t Integer
  , HasOrderCertainly t t
  , HasOrderCertainly t Int, HasOrderCertainly t Integer
  , CanTestPosNeg t)
  => OrderedCertainlyRing t

instance OrderedCertainlyRing Integer
instance OrderedCertainlyRing (CN Integer)
instance OrderedCertainlyRing Rational
instance OrderedCertainlyRing (CN Rational)
