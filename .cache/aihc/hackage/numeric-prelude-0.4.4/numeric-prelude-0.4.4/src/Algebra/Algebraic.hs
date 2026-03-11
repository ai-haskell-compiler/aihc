{-# LANGUAGE RebindableSyntax #-}
module Algebra.Algebraic where

import qualified Algebra.Field as Field
import qualified Algebra.Laws as Laws
import qualified Algebra.ToRational as ToRational
import qualified Algebra.ToInteger  as ToInteger

import Number.Ratio (Rational, (%), numerator, denominator)
import Algebra.Field ((^-), recip, fromRational')
import Algebra.Ring ((*), (^), fromInteger)
import Algebra.Additive((+))

import NumericPrelude.Base
import qualified Prelude as P


infixr 8  ^/

{- | Minimal implementation: 'root' or '(^\/)'. -}

class (Field.C a) => C a where
    {-# MINIMAL root | (^/) #-}
    sqrt :: a -> a
    sqrt = root 2
    -- sqrt x  =  x ** (1/2)

    root :: P.Integer -> a -> a
    root n x = x ^/ (1 % n)

    (^/) :: a -> Rational -> a
    x ^/ y = root (denominator y) (x ^- numerator y)

genericRoot :: (C a, ToInteger.C b) => b -> a -> a
genericRoot n = root (ToInteger.toInteger n)

power :: (C a, ToRational.C b) => b -> a -> a
power r = (^/ ToRational.toRational r)

instance C P.Float where
    sqrt     = P.sqrt
    root n x = x P.** recip (P.fromInteger n)
    x ^/ y   = x P.** fromRational' y

instance C P.Double where
    sqrt     = P.sqrt
    root n x = x P.** recip (P.fromInteger n)
    x ^/ y   = x P.** fromRational' y


{- * Properties -}

-- propSqrtSqr :: (Eq a, C a, Units.C a) => a -> Bool
-- propSqrtSqr x = sqrt (x^2) == Units.stdAssociate x

propSqrSqrt :: (Eq a, C a) => a -> Bool
propSqrSqrt x = sqrt x ^ 2 == x

propPowerCascade      :: (Eq a, C a) => a -> Rational -> Rational -> Bool
propPowerProduct      :: (Eq a, C a) => a -> Rational -> Rational -> Bool
propPowerDistributive :: (Eq a, C a) => Rational -> a -> a -> Bool

propPowerCascade      x i j  =  Laws.rightCascade (*) (^/) x i j
propPowerProduct      x i j  =  Laws.homomorphism (x^/) (+) (*) i j
propPowerDistributive i x y  =  Laws.leftDistributive (^/) (*) i x y
