module Data.ANum (ANum(..)) where

import Control.Applicative (Applicative, pure, (<*>), liftA, liftA2)


newtype ANum f n = ANum { unANum :: f n }
  deriving (Show, Eq, Ord)


instance (Functor f) => Functor (ANum f) where
  fmap f (ANum x) = ANum (fmap f x)

instance (Applicative f) => Applicative (ANum f) where
  pure x = ANum (pure x)
  (ANum f) <*> (ANum x) = ANum (f <*> x)

instance (Foldable f) => Foldable (ANum f) where
    foldMap f = foldMap f . unANum

instance (Traversable f) => Traversable (ANum f) where
    traverse f = fmap ANum . traverse f . unANum

instance (Applicative f, Num n) => Num (ANum f n) where
  fromInteger = pure . fromInteger
  (*) = liftA2 (*)
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  negate = liftA negate
  abs = liftA abs
  signum = liftA signum

instance (Applicative f, Fractional n) => Fractional (ANum f n) where
  (/) = liftA2 (/)
  recip = liftA recip
  fromRational = pure . fromRational

instance (Applicative f, Floating n) => Floating (ANum f n) where
  pi = pure pi
  exp = liftA exp
  sqrt = liftA sqrt
  log = liftA log
  (**) = liftA2 (**)
  logBase = liftA2 logBase
  sin = liftA sin
  tan = liftA tan
  cos = liftA cos
  asin = liftA asin
  atan = liftA atan
  acos = liftA acos
  sinh = liftA sinh
  tanh = liftA tanh
  cosh = liftA cosh
  asinh = liftA asinh
  atanh = liftA atanh
  acosh = liftA acosh
