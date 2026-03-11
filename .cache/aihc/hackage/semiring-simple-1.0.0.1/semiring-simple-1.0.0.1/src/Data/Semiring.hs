-- |
-- Module       : Data.Semiring
-- Description  : Semirings
-- Copyright    : Copyright (c) 2012, Thomas Wilke, Frank Huch, Sebastian Fischer.
--                Copyright (c) 2014-2016, Peter Harpending.
-- License      : BSD3
-- Maintainer   : Peter Harpending <peter@harpending.org>
-- Stability    : experimental
-- Portability  : portable
-- 
-- This library provides a type class for semirings.
--
-- A semiring is an additive commutative monoid with identity 'zero':
-- 
-- Most Haskellers are familiar with the 'Monoid' typeclass:
-- 
-- >      zero <+> a ≡ a
-- > (a <+> b) <+> c ≡ a <+> (b <+> c)
-- 
-- In this case, we've aliased
-- 
-- > zero = mempty
-- > (<+>) = mappend
-- 
-- A commutative monoid adds the requirement of symmetry:
-- 
-- >         a <+> b ≡ b <+> a
-- 
-- A semiring adds the requirement of a multiplication-like operator. However,
-- it does not require the existence of multiplicative inverses,
-- i.e. division. Moreover, multiplication does not need to be commutative.
-- 
-- >        one <.> a ≡ a
-- >        a <.> one ≡ a
-- >  (a <.> b) <.> c ≡ a <.> (b <.> c)
-- 
-- Multiplication distributes over addition:
-- 
-- > a <.> (b <+> c) ≡ (a <.> b) <+> (a <.> b)
-- > (a <+> b) <>. c ≡ (a <.> c) <+> (b <.> c)
-- 
-- 'zero' annihilates a semiring with respect to multiplication:
-- 
-- > zero <.> a ≡ zero
-- > a <.> zero ≡ zero
-- 
-- The classic example of a semiring is the "Tropical numbers". The Tropical
-- numbers, or T, are just real numbers with different operators.
-- 
-- > zero = ∞
-- > a <+> b = minimum {a, b}
-- > one = 0
-- > a <.> b = a + b
-- 
-- We can easily verify that these satisfy the semiring axioms:
-- 
-- First, the requirements for a commutative monoid
-- 
-- >             minimum {∞, a} ≡ minimum {a, ∞} ≡ a
-- >             minimum {a, ∞} ≡ a
-- >             minimum {a, b} ≡ minimum {b, a}
-- > minimum {a, minimum{b, c}} ≡ minimum {minimum {a, b}, c}
-- 
-- >              0 + a ≡ a
-- >              a + 0 ≡ a
-- >        a + (b + c) ≡ (a + b) + c
-- > a + minimum {b, c} ≡ minimum {a + b, a + c}
-- > minimum {a, b} + c ≡ minimum {a + c, b + c}
-- >              a + ∞ ≡ ∞
-- >              ∞ + a ≡ ∞
module Data.Semiring where

import Data.Monoid

-- |Alias for 'mappend'.
infixl 5 <+>
(<+>) :: Monoid m => m -> m -> m
(<+>) = mappend

-- |Alias for 'mempty'
zero :: Monoid m => m
zero = mempty

class Monoid m => Semiring m where
  one :: m
  (<.>) :: m -> m -> m
