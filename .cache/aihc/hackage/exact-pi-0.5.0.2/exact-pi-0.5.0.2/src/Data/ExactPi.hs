{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ParallelListComp    #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.ExactPi
Description : Exact rational multiples of powers of pi
License     : MIT
Maintainer  : douglas.mcclean@gmail.com
Stability   : experimental

This type is sufficient to exactly express the closure of Q ∪ {π} under multiplication and division.
As a result it is useful for representing conversion factors
between physical units. Approximate values are included both to close the remainder
of the arithmetic operations in the `Num` typeclass and to encode conversion
factors defined experimentally.
-}
module Data.ExactPi
(
  ExactPi(..),
  approximateValue,
  isZero,
  isExact,
  isExactZero,
  isExactOne,
  areExactlyEqual,
  isExactInteger,
  toExactInteger,
  isExactRational,
  toExactRational,
  rationalApproximations,
  -- * Utils
  getRationalLimit
)
where

import Data.Monoid
import Data.Ratio ((%), numerator, denominator)
import Data.Semigroup
import Prelude

-- | Represents an exact or approximate real value.
-- The exactly representable values are rational multiples of an integer power of pi.
data ExactPi = Exact Integer Rational -- ^ @'Exact' z q@ = q * pi^z. Note that this means there are many representations of zero.
             | Approximate (forall a.Floating a => a) -- ^ An approximate value. This representation was chosen because it allows conversion to floating types using their native definition of 'pi'.

-- | Approximates an exact or approximate value, converting it to a `Floating` type.
-- This uses the value of `pi` supplied by the destination type, to provide the appropriate
-- precision.
approximateValue :: Floating a => ExactPi -> a
approximateValue (Exact z q) = (pi ^^ z) * (fromRational q)
approximateValue (Approximate x) = x

-- | Identifies whether an 'ExactPi' is an exact or approximate representation of zero.
isZero :: ExactPi -> Bool
isZero (Exact _ 0)     = True
isZero (Approximate x) = x == (0 :: Double)
isZero _               = False

-- | Identifies whether an 'ExactPi' is an exact value.
isExact :: ExactPi -> Bool
isExact (Exact _ _) = True
isExact _           = False

-- | Identifies whether an 'ExactPi' is an exact representation of zero.
isExactZero :: ExactPi -> Bool
isExactZero (Exact _ 0) = True
isExactZero _ = False

-- | Identifies whether an 'ExactPi' is an exact representation of one.
isExactOne :: ExactPi -> Bool
isExactOne (Exact 0 1) = True
isExactOne _ = False

-- | Identifies whether two 'ExactPi' values are exactly equal.
areExactlyEqual :: ExactPi -> ExactPi -> Bool
areExactlyEqual (Exact z1 q1) (Exact z2 q2) = (z1 == z2 && q1 == q2) || (q1 == 0 && q2 == 0)
areExactlyEqual _ _ = False

-- | Identifies whether an 'ExactPi' is an exact representation of an integer.
isExactInteger :: ExactPi -> Bool
isExactInteger (Exact 0 q) | denominator q == 1 = True
isExactInteger _                                = False

-- | Converts an 'ExactPi' to an exact 'Integer' or 'Nothing'.
toExactInteger :: ExactPi -> Maybe Integer
toExactInteger (Exact 0 q) | denominator q == 1 = Just $ numerator q
toExactInteger _                                = Nothing

-- | Identifies whether an 'ExactPi' is an exact representation of a rational.
isExactRational :: ExactPi -> Bool
isExactRational (Exact 0 _) = True
isExactRational _           = False

-- | Converts an 'ExactPi' to an exact 'Rational' or 'Nothing'.
toExactRational :: ExactPi -> Maybe Rational
toExactRational (Exact 0 q) = Just q
toExactRational _           = Nothing

-- | Converts an 'ExactPi' to a list of increasingly accurate rational approximations. Note
-- that 'Approximate' values are converted using the 'Real' instance for 'Double' into a
-- singleton list. Note that exact rationals are also converted into a singleton list.
--
-- Implementation is based on Chudnovsky's algorithm.
rationalApproximations :: ExactPi -> [Rational]
rationalApproximations (Approximate x) = [toRational (x :: Double)]
rationalApproximations (Exact _ 0)     = [0]
rationalApproximations (Exact 0 q)     = [q]
rationalApproximations (Exact z q)
  | even z    = [q * 10005^^k * c^^z     | c <- chudnovsky]
  | otherwise = [q * 10005^^k * c^^z * r | c <- chudnovsky | r <- rootApproximation]
  where k = z `div` 2

chudnovsky :: [Rational]
chudnovsky = [426880 / s | s <- partials]
  where lk = iterate (+545140134) 13591409
        xk = iterate (*(-262537412640768000)) 1
        kk = iterate (+12) 6
        mk = 1: [m * ((k^(3::Int) - 16*k) % (n+1)^(3::Int)) | m <- mk | k <- kk | n <- [0..]]
        values = [m * l / x | m <- mk | l <- lk | x <- xk]
        partials = scanl1 (+) values

-- | Given an infinite converging sequence of rationals, find their limit.
-- Takes a comparison function to determine when convergence is close enough.
--
-- >>> getRationalLimit (==) (rationalApproximations (Exact 1 1)) :: Double
-- 3.141592653589793
getRationalLimit :: Fractional a => (a -> a -> Bool) -> [Rational] -> a
getRationalLimit cmp = go . map fromRational
  where go (x:y:xs)
          | cmp x y   = y
          | otherwise = go (y:xs)
        go [x] = x
        go _ = error "did not converge"

-- | A sequence of convergents approximating @sqrt 10005@, intended to be zipped
-- with 'chudnovsky' in 'rationalApproximations'. Carefully chosen so that
-- the denominator does not increase too rapidly but approximations are still
-- appropriately precise.
--
-- Chudnovsky's series provides no more than 15 digits
-- per iteration, so the root approximation should not
-- have a more rapid rate of convergence.
rootApproximation :: [Rational]
rootApproximation = map head . iterate (drop 4) $ go 1 0 100 1 40
  where
    go pk' qk' pk qk a = (pk % qk): go pk qk (pk' + a*pk) (qk' + a*qk) (240-a)

instance Show ExactPi where
  show (Exact z q) | z == 0 = "Exactly " ++ show q
                   | z == 1 = "Exactly pi * " ++ show q
                   | otherwise = "Exactly pi^" ++ show z ++ " * " ++ show q
  show (Approximate x) = "Approximately " ++ show (x :: Double)

instance Num ExactPi where
  fromInteger n = Exact 0 (fromInteger n)
  (Exact z1 q1) * (Exact z2 q2) = Exact (z1 + z2) (q1 * q2)
  (Exact _ 0) * _ = 0
  _ * (Exact _ 0) = 0
  x * y = Approximate $ approximateValue x * approximateValue y
  (Exact z1 q1) + (Exact z2 q2) | z1 == z2 = Exact z1 (q1 + q2) -- by distributive property
  x + y = Approximate $ approximateValue x + approximateValue y
  abs (Exact z q) = Exact z (abs q)
  abs (Approximate x) = Approximate $ abs x
  signum (Exact _ q) = Exact 0 (signum q)
  signum (Approximate x) = Approximate $ signum x -- we leave this tagged as approximate because we don't know "how" approximate the input was. a case could be made for exact answers here.
  negate x = (Exact 0 (-1)) * x

instance Fractional ExactPi where
  fromRational = Exact 0
  recip (Exact z q) = Exact (negate z) (recip q)
  recip (Approximate x) = Approximate (recip x)

instance Floating ExactPi where
  pi = Exact 1 1
  exp x | isExactZero x = 1
        | otherwise = approx1 exp x
  log (Exact 0 1) = 0
  log x = approx1 log x
  -- It would be possible to give tighter bounds to the trig functions, preserving exactness for arguments that have an exactly representable result.
  sin = approx1 sin
  cos = approx1 cos
  tan = approx1 tan
  asin = approx1 asin
  atan = approx1 atan
  acos = approx1 acos
  sinh = approx1 sinh
  cosh = approx1 cosh
  tanh = approx1 tanh
  asinh = approx1 asinh
  acosh = approx1 acosh
  atanh = approx1 atanh

approx1 :: (forall a.Floating a => a -> a) -> ExactPi -> ExactPi
approx1 f x = Approximate (f (approximateValue x))

-- | The multiplicative semigroup over 'Rational's augmented with multiples of 'pi'.
instance Semigroup ExactPi where
  (<>) = mappend

-- | The multiplicative monoid over 'Rational's augmented with multiples of 'pi'.
instance Monoid ExactPi where
  mempty = 1
  mappend = (*)
