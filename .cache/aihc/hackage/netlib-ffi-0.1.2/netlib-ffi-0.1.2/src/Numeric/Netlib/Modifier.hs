module Numeric.Netlib.Modifier where

import Numeric.Netlib.Layout (Order, flipOrder)

import Data.Monoid (Monoid, mempty, mappend)
import Data.Semigroup (Semigroup, (<>))



data Transposition = NonTransposed | Transposed
   deriving (Eq, Show, Enum, Bounded)

instance Semigroup Transposition where
   x<>y = if x==y then NonTransposed else Transposed

instance Monoid Transposition where
   mempty = NonTransposed
   mappend = (<>)

transposeOrder :: Transposition -> Order -> Order
transposeOrder NonTransposed = id
transposeOrder Transposed = flipOrder



data Conjugation = NonConjugated | Conjugated
   deriving (Eq, Show, Enum, Bounded)

instance Semigroup Conjugation where
   x<>y = if x==y then NonConjugated else Conjugated

instance Monoid Conjugation where
   mempty = NonConjugated
   mappend = (<>)


data Inversion = NonInverted | Inverted
   deriving (Eq, Show, Enum, Bounded)

instance Semigroup Inversion where
   x<>y = if x==y then NonInverted else Inverted

instance Monoid Inversion where
   mempty = NonInverted
   mappend = (<>)
