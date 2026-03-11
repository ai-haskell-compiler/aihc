{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Range.Algebra.Internal where

import Prelude hiding (const)

import Data.Range.RangeInternal

import Control.Monad.Free
#if MIN_VERSION_base(4,9,0)
import Data.Functor.Classes
#endif

data RangeExprF r
  = Invert r
  | Union r r
  | Intersection r r
  | Difference r r
  deriving (Show, Eq, Functor)

#if MIN_VERSION_base(4,9,0)
instance Eq1 RangeExprF where
  liftEq eq (Invert a) (Invert b) = eq a b
  liftEq eq (Union a c) (Union b d) = eq a b && eq c d
  liftEq eq (Intersection a c) (Intersection b d) = eq a b && eq c d
  liftEq eq (Difference a c) (Difference b d) = eq a b && eq c d
  liftEq _ _ _ = False

instance Show1 RangeExprF where
  liftShowsPrec showPrec _ p (Invert x) = showString "not " . showParen True (showPrec (p + 1) x)
  liftShowsPrec showPrec _ p (Union a b) =
    showPrec (p + 1) a .
    showString " \\/ " .
    showPrec (p + 1) b
  liftShowsPrec showPrec _ p (Intersection a b) =
    showPrec (p + 1) a .
    showString " /\\ " .
    showPrec (p + 1) b
  liftShowsPrec showPrec _ p (Difference a b) =
    showPrec (p + 1) a .
    showString " - " .
    showPrec (p + 1) b
#endif

newtype RangeExpr a = RangeExpr { getFree :: Free RangeExprF a }
  deriving (Show, Eq, Functor)

-- | This is an F-Algebra. You don't need to know what this is in order to be able
-- to use this module, but, if you are interested you can
-- <https://www.schoolofhaskell.com/user/bartosz/understanding-algebras read more on School of Haskell>.
type Algebra f a = f a -> a

rangeMergeAlgebra :: (Ord a) => Algebra RangeExprF (RangeMerge a)
rangeMergeAlgebra (Invert a) = invertRM a
rangeMergeAlgebra (Union a b) = a `unionRangeMerges` b
rangeMergeAlgebra (Intersection a b) = a `intersectionRangeMerges` b
rangeMergeAlgebra (Difference a b) = a `intersectionRangeMerges` invertRM b
