{-|
Module      : Math.ExpPairs.LinearForm
Copyright   : (c) Andrew Lelechenko, 2014-2020
License     : GPL-3
Maintainer  : andrew.lelechenko@gmail.com

Linear forms, rational forms and constraints

Provides types for rational forms (to hold objective functions in "Math.ExpPairs") and linear contraints (to hold constraints of optimization). Both of them are built atop of projective linear forms.
-}

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric     #-}

module Math.ExpPairs.LinearForm
  ( LinearForm (..)
  , scaleLF
  , evalLF
  , substituteLF
  , RationalForm (..)
  , evalRF
  , IneqType (..)
  , Constraint (..)
  , checkConstraint
  ) where

import Control.DeepSeq
import Data.Foldable  (Foldable (..), toList)
import Data.Maybe     (mapMaybe)
import Data.Ratio     (numerator, denominator)
import GHC.Generics   (Generic (..))
import Prettyprinter

import Math.ExpPairs.RatioInf

-- |Define an affine linear form of three variables: a*k + b*l + c*m.
-- First argument of 'LinearForm' stands for a, second for b
-- and third for c. Linear forms form a monoid by addition.
data LinearForm t = LinearForm !t !t !t
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance NFData t => NFData (LinearForm t) where
  rnf = rnf . toList

instance (Num t, Eq t, Pretty t) => Pretty (LinearForm t) where
  pretty (LinearForm 0 0 0) = pretty "0"
  pretty (LinearForm a b c) = cat $ punctuate plus $ mapMaybe f [(a, 'k'), (b, 'l'), (c, 'm')] where
    plus = space <> pretty "+" <> space
    f (0, _) = Nothing
    f (1, t) = Just (pretty t)
    f (r, t) = Just (pretty r <+> pretty "*" <+> pretty t)

instance Num t => Num (LinearForm t) where
  (LinearForm a b c) + (LinearForm d e f) = LinearForm (a+d) (b+e) (c+f)
  (*)    = error "Multiplication of LinearForm is undefined"
  negate = fmap negate
  abs    = error "Absolute value of LinearForm is undefined"
  signum = error "Signum of LinearForm is undefined"
  fromInteger n = LinearForm 0 0 (fromInteger n)

instance Num t => Semigroup (LinearForm t) where
  (<>) = (+)

instance Num t => Monoid (LinearForm t) where
  mempty  = 0
  mappend = (<>)

-- | Multiply a linear form by a given coefficient.
scaleLF :: (Num t, Eq t) => t -> LinearForm t -> LinearForm t
scaleLF 0 = const 0
scaleLF s = fmap (* s)

-- |Evaluate a linear form a*k + b*l + c*m for given k, l and m.
evalLF :: Num t => (t, t, t) -> LinearForm t -> t
evalLF (k, l, m) (LinearForm a b c) = a * k + l * b + m * c
{-# INLINE evalLF #-}

-- |Substitute linear forms k, l and m into a given linear form
-- a*k + b*l + c*m to obtain a new linear form.
substituteLF :: (Eq t, Num t) => (LinearForm t, LinearForm t, LinearForm t) -> LinearForm t -> LinearForm t
substituteLF (k, l, m) (LinearForm a b c) = scaleLF a k + scaleLF b l + scaleLF c m

-- | Define a rational form of two variables, equal to the ratio of two 'LinearForm'.
data RationalForm t = (LinearForm t) :/: (LinearForm t)
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
infix 5 :/:

instance (Num t, Eq t, Pretty t) => Pretty (RationalForm t) where
  pretty (l1 :/: l2) = parens (pretty l1) <> softline <> parens (pretty l2)

instance NFData t => NFData (RationalForm t) where
  rnf = rnf . toList

instance Num t => Num (RationalForm t) where
  (+)              = error "Addition of RationalForm is undefined"
  (*)              = error "Multiplication of RationalForm is undefined"
  negate (a :/: b) = negate a :/: b
  abs              = error "Absolute value of RationalForm is undefined"
  signum           = error "Signum of RationalForm is undefined"
  fromInteger n    = fromInteger n :/: 1

instance Num t => Fractional (RationalForm t) where
  fromRational r = fromInteger (numerator r) :/: fromInteger (denominator r)
  recip (a :/: b) = b :/: a

mapTriple :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTriple f (x, y, z) = (f x, f y, f z)
{-# INLINE mapTriple #-}

-- |Evaluate a rational form (a*k + b*l + c*m) \/ (a'*k + b'*l + c'*m)
-- for given k, l and m.
evalRF :: Real t => (Integer, Integer, Integer) -> RationalForm t -> RationalInf
evalRF (k, l, m) (num :/: den) = if denom==0 then InfPlus else Finite (numer / denom) where
  klm = mapTriple fromInteger (k, l, m)
  numer = toRational $ evalLF klm num
  denom = toRational $ evalLF klm den

-- |Constants to specify the strictness of 'Constraint'.
data IneqType
  -- | Strict inequality (>0).
  = Strict
  -- | Non-strict inequality (≥0).
  | NonStrict
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance Pretty IneqType where
  pretty Strict    = pretty ">"
  pretty NonStrict = pretty ">="

-- |A linear constraint of two variables.
data Constraint t = Constraint !(LinearForm t) !IneqType
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance (Num t, Eq t, Pretty t) => Pretty (Constraint t) where
  pretty (Constraint lf ineq) = pretty lf <+> pretty ineq <+> pretty "0"

instance NFData t => NFData (Constraint t) where
  rnf (Constraint l i) = i `seq` rnf l

-- |Evaluate a rational form of constraint and compare
-- its value with 0. Strictness depends on the given 'IneqType'.
checkConstraint :: (Num t, Ord t) => (Integer, Integer, Integer) -> Constraint t -> Bool
checkConstraint (k, l, m) (Constraint lf ineq) = case ineq of
  NonStrict -> numer >= 0
  Strict    -> numer >  0
  where
    klm   = mapTriple fromInteger (k, l, m)
    numer = evalLF klm lf
{-# SPECIALIZE checkConstraint :: (Integer, Integer, Integer) -> Constraint Rational -> Bool #-}
