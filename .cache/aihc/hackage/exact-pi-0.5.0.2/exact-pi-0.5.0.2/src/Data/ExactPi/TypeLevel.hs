{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ > 805
{-# LANGUAGE NoStarIsType #-}
#endif

{-|
Module      : Data.ExactPi.TypeLevel
Description : Exact non-negative rational multiples of powers of pi at the type level
License     : MIT
Maintainer  : douglas.mcclean@gmail.com
Stability   : experimental

This kind is sufficient to exactly express the closure of Q⁺ ∪ {π} under multiplication and division.
As a result it is useful for representing conversion factors between physical units.
-}
module Data.ExactPi.TypeLevel
(
  -- * Type Level ExactPi Values
  type ExactPi'(..),
  KnownExactPi(..),
  -- * Arithmetic
  type (*), type (/), type Recip,
  type ExactNatural,
  type One, type Pi,
  -- * Conversion to Term Level
  type MinCtxt, type MinCtxt',
  injMin
)
where

import Data.ExactPi
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Ratio
import GHC.TypeLits hiding (type (*), type (^))
import qualified GHC.TypeLits as N
import Numeric.NumType.DK.Integers hiding (type (*), type (/))
import qualified Numeric.NumType.DK.Integers as Z

-- | A type-level representation of a non-negative rational multiple of an integer power of pi.
--
-- Each type in this kind can be exactly represented at the term level by a value of type 'ExactPi',
-- provided that its denominator is non-zero.
--
-- Note that there are many representations of zero, and many representations of dividing by zero.
-- These are not excluded because doing so introduces a lot of extra machinery. Play nice! Future
-- versions may not include a representation for zero.
--
-- Of course there are also many representations of every value, because the numerator need not be
-- comprime to the denominator. For many purposes it is not necessary to maintain the types in reduced
-- form, they will be appropriately reduced when converted to terms.
data ExactPi' = ExactPi' TypeInt -- Exponent of pi
                         Nat -- Numerator
                         Nat -- Denominator

-- | A KnownDimension is one for which we can construct a term-level representation.
--
-- Each validly constructed type of kind 'ExactPi'' has a 'KnownExactPi' instance, provided that
-- its denominator is non-zero.
class KnownExactPi (v :: ExactPi') where
  -- | Converts an 'ExactPi'' type to an 'ExactPi' value.
  exactPiVal :: Proxy v -> ExactPi

-- | Determines the minimum context required for a numeric type to hold the value
-- associated with a specific 'ExactPi'' type.
type family MinCtxt' (v :: ExactPi') where
  MinCtxt' ('ExactPi' 'Zero p 1) = Num
  MinCtxt' ('ExactPi' 'Zero p q) = Fractional
  MinCtxt' ('ExactPi' z p q)     = Floating

type MinCtxt v a = (KnownExactPi v, MinCtxt' v a, KnownMinCtxt (MinCtxt' v))

-- | A KnownMinCtxt is a contraint on values sufficient to allow us to inject certain
-- 'ExactPi' values into types that satisfy the constraint.
class KnownMinCtxt c where
  -- | Injects an 'ExactPi' value into a specified type satisfying this constraint.
  --
  -- The injection is permitted to fail if type constraint does not entail the 'MinCtxt'
  -- required by the 'ExactPi'' representation of the supplied 'ExactPi' value.
  inj :: c a => Proxy c -- ^ A proxy for identifying the required constraint.
             -> ExactPi -- ^ The value to inject.
             -> a       -- ^ A value of the constrained type corresponding to the supplied 'ExactPi' value.

instance KnownMinCtxt Num where
  inj _ = fromInteger . fromJust . toExactInteger

instance KnownMinCtxt Fractional where
  inj _ = fromRational . fromJust . toExactRational

instance KnownMinCtxt Floating where
  inj _ = approximateValue

-- | Converts an 'ExactPi'' type to a numeric value with the minimum required context.
--
-- When the value is known to be an integer, it can be returned as any instance of 'Num'. Similarly,
-- rationals require 'Fractional', and values that involve 'pi' require 'Floating'.
injMin :: forall v a.(MinCtxt v a) => Proxy v -> a
injMin = inj (Proxy :: Proxy (MinCtxt' v)) . exactPiVal

instance (KnownTypeInt z, KnownNat p, KnownNat q, 1 <= q) => KnownExactPi ('ExactPi' z p q) where
  exactPiVal _ = Exact z' (p' % q')
    where
      z' = toNum  (Proxy :: Proxy z)
      p' = natVal (Proxy :: Proxy p)
      q' = natVal (Proxy :: Proxy q)

-- | Forms the product of 'ExactPi'' types (in the arithmetic sense).
type family (a :: ExactPi') * (b :: ExactPi') :: ExactPi' where
  ('ExactPi' z p q) * ('ExactPi' z' p' q') = 'ExactPi' (z Z.+ z') (p N.* p') (q N.* q')

-- | Forms the quotient of 'ExactPi'' types (in the arithmetic sense).
type family (a :: ExactPi') / (b :: ExactPi') :: ExactPi' where
  ('ExactPi' z p q) / ('ExactPi' z' p' q') = 'ExactPi' (z Z.- z') (p N.* q') (q N.* p')

-- | Forms the reciprocal of an 'ExactPi'' type.
type family Recip (a :: ExactPi') :: ExactPi' where
  Recip ('ExactPi' z p q) = 'ExactPi' (Negate z) q p

-- | Converts a type-level natural to an 'ExactPi'' type.
type ExactNatural n = 'ExactPi' 'Zero n 1

-- | The 'ExactPi'' type representing the number 1.
type One = ExactNatural 1

-- | The 'ExactPi'' type representing the number 'pi'.
type Pi = 'ExactPi' 'Pos1 1 1
