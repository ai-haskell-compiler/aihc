module Data.Complex
  ( Complex (..),
    realPart,
    imagPart,
    mkPolar,
    cis,
    polar,
    magnitude,
    phase,
    conjugate,
  )
where

import Data.Foldable (Foldable (..))
import Prelude

infix 6 :+

-- | A complex number in rectangular form. Both components are strict, as in
-- @base@, so evaluating the constructor to weak head normal form evaluates its
-- real and imaginary parts.
data Complex a = !a :+ !a

instance (Eq a) => Eq (Complex a) where
  (:+) leftReal leftImaginary == (:+) rightReal rightImaginary =
    leftReal == rightReal && leftImaginary == rightImaginary

  left /= right = not (left == right)

instance (Show a) => Show (Complex a) where
  showsPrec precedence ((:+) real imaginary) =
    showParen
      (precedence > 6)
      (showsPrec 7 real . showString " :+ " . showsPrec 7 imaginary)

instance Functor Complex where
  fmap f ((:+) real imaginary) = f real :+ f imaginary

instance Applicative Complex where
  pure value = value :+ value
  (:+) realFunction imaginaryFunction <*> (:+) real imaginary =
    realFunction real :+ imaginaryFunction imaginary

instance Monad Complex where
  (:+) real imaginary >>= next =
    realPart (next real) :+ imagPart (next imaginary)

  (:+) _ _ >> next = next

  return = pure

instance Foldable Complex where
  foldr f initial ((:+) real imaginary) = f real (f imaginary initial)
  foldl f initial ((:+) real imaginary) = f (f initial real) imaginary
  null _ = False

-- | Extract the real component.
realPart :: Complex a -> a
realPart ((:+) real _) = real

-- | Extract the imaginary component.
imagPart :: Complex a -> a
imagPart ((:+) _ imaginary) = imaginary

-- | Reflect a complex number across the real axis.
conjugate :: (Num a) => Complex a -> Complex a
conjugate ((:+) real imaginary) = real :+ negate imaginary

-- | Construct a complex number from its magnitude and phase.
mkPolar :: (Floating a) => a -> a -> Complex a
mkPolar radius theta = radius * cos theta :+ radius * sin theta

-- | Construct the unit complex number at the given phase.
cis :: (Floating a) => a -> Complex a
cis theta = cos theta :+ sin theta

-- | Return a complex number's magnitude and canonical phase.
polar :: (RealFloat a) => Complex a -> (a, a)
polar value = (magnitude value, phase value)

-- | Compute the magnitude without unnecessary overflow or underflow.
magnitude :: (RealFloat a) => Complex a -> a
magnitude ((:+) real imaginary) =
  let power = max (exponent real) (exponent imaginary)
      inversePower = negate power
      scaledReal = scaleFloat inversePower real
      scaledImaginary = scaleFloat inversePower imaginary
   in scaleFloat power (sqrt (scaledReal * scaledReal + scaledImaginary * scaledImaginary))

-- | Return the phase in the range @(-pi, pi]@, using zero for the origin.
phase :: (RealFloat a) => Complex a -> a
phase ((:+) real imaginary) =
  case real == 0 && imaginary == 0 of
    True -> 0
    False -> atan2 imaginary real

instance (RealFloat a) => Num (Complex a) where
  (:+) leftReal leftImaginary + (:+) rightReal rightImaginary =
    (leftReal + rightReal) :+ (leftImaginary + rightImaginary)

  (:+) leftReal leftImaginary - (:+) rightReal rightImaginary =
    (leftReal - rightReal) :+ (leftImaginary - rightImaginary)

  (:+) leftReal leftImaginary * (:+) rightReal rightImaginary =
    (leftReal * rightReal - leftImaginary * rightImaginary)
      :+ (leftReal * rightImaginary + leftImaginary * rightReal)

  negate ((:+) real imaginary) = negate real :+ negate imaginary
  abs value = magnitude value :+ 0
  signum value@((:+) real imaginary) =
    case real == 0 && imaginary == 0 of
      True -> 0
      False ->
        let radius = magnitude value
         in real / radius :+ imaginary / radius
  fromInteger value = fromInteger value :+ 0

instance (RealFloat a) => Fractional (Complex a) where
  (:+) leftReal leftImaginary / (:+) rightReal rightImaginary =
    let power = negate (max (exponent rightReal) (exponent rightImaginary))
        scaledReal = scaleFloat power rightReal
        scaledImaginary = scaleFloat power rightImaginary
        divisor = rightReal * scaledReal + rightImaginary * scaledImaginary
     in ((leftReal * scaledReal + leftImaginary * scaledImaginary) / divisor)
          :+ ((leftImaginary * scaledReal - leftReal * scaledImaginary) / divisor)

  recip value = 1 / value
  fromRational value = fromRational value :+ 0

instance (RealFloat a) => Floating (Complex a) where
  pi = pi :+ 0
  exp ((:+) real imaginary) =
    let exponential = exp real
     in exponential * cos imaginary :+ exponential * sin imaginary
  log value = log (magnitude value) :+ phase value
  left ** right = complexPower left right
  logBase base value = log value / log base
  sqrt = complexSquareRoot
  sin ((:+) real imaginary) =
    sin real * cosh imaginary :+ cos real * sinh imaginary
  cos ((:+) real imaginary) =
    cos real * cosh imaginary :+ negate (sin real * sinh imaginary)
  tan value = sin value / cos value
  sinh ((:+) real imaginary) =
    cos imaginary * sinh real :+ sin imaginary * cosh real
  cosh ((:+) real imaginary) =
    cos imaginary * cosh real :+ sin imaginary * sinh real
  tanh value = sinh value / cosh value
  asin value@((:+) real imaginary) =
    case log ((negate imaginary :+ real) + sqrt (1 - value * value)) of
      (:+) resultReal resultImaginary -> resultImaginary :+ negate resultReal
  acos value =
    case sqrt (1 - value * value) of
      (:+) squareReal squareImaginary ->
        case log (value + (negate squareImaginary :+ squareReal)) of
          (:+) resultReal resultImaginary -> resultImaginary :+ negate resultReal
  atan value@((:+) real imaginary) =
    case log (((1 - imaginary) :+ real) / sqrt (1 + value * value)) of
      (:+) resultReal resultImaginary -> resultImaginary :+ negate resultReal
  asinh value = log (value + sqrt (1 + value * value))
  acosh value = log (value + sqrt (value + 1) * sqrt (value - 1))
  atanh value = (1 / 2) * log ((1 + value) / (1 - value))
  log1p value = log (1 + value)
  expm1 value = exp value - 1
  log1pexp value = log1p (exp value)
  log1mexp value = log1p (negate (exp value))

complexPower :: (RealFloat a) => Complex a -> Complex a -> Complex a
complexPower _ ((:+) 0 0) = 1 :+ 0
complexPower ((:+) 0 0) ((:+) exponentReal _) =
  case compare exponentReal 0 of
    GT -> 0 :+ 0
    LT -> (1 / 0) :+ 0
    EQ -> (0 / 0) :+ (0 / 0)
complexPower base@((:+) real imaginary) exponentValue@((:+) exponentReal _) =
  case isInfinite real || isInfinite imaginary of
    True ->
      case compare exponentReal 0 of
        GT -> (1 / 0) :+ 0
        LT -> 0 :+ 0
        EQ -> (0 / 0) :+ (0 / 0)
    False -> exp (log base * exponentValue)

complexSquareRoot :: (RealFloat a) => Complex a -> Complex a
complexSquareRoot ((:+) 0 0) = 0
complexSquareRoot value@((:+) real imaginary) =
  let first = sqrt ((magnitude value + abs real) / 2)
      second = abs imaginary / (first * 2)
   in case real < 0 of
        True -> complexSquareRootWithSign imaginary second first
        False -> complexSquareRootWithSign imaginary first second

complexSquareRootWithSign :: (RealFloat a) => a -> a -> a -> Complex a
complexSquareRootWithSign imaginary resultReal resultImaginary =
  resultReal
    :+ case imaginary < 0 of
      True -> negate resultImaginary
      False -> resultImaginary
