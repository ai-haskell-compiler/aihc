
{-# LANGUAGE EmptyDataDecls #-}

{-|
The purpose of this module is to provide newtype wrapper that allows one to effectively override the equality operator of a value so that it is /approximate/ rather than /exact/.  For example, the type

> type ApproximateDouble = AbsolutelyApproximateValue (Digits Five) Double

defines an alias for a wrapper containing 'Double's such that two doubles are equal if they are equal to within five decimals of accuracy;  for example, we have that

> 1 == (1+10^^(-6) :: ApproximateDouble)

evaluates to 'True'.  Note that we did not need to wrap the value @1+10^^(-6)@ since 'AbsolutelyApproximateValue' is an instance of 'Num'.  For convenience, 'Num' as well as many other of the numerical classes such as 'Real' and 'Floating' have all been derived for the wrappers defined in this package so that one can conveniently use the wrapped values in the same way as one would use the values themselves.

Two kinds of wrappers are provided by this package.

* 'AbsolutelyApproximateValue' wraps values that are considered to be equal if their absolute difference falls within the specified tolerance.

* 'RelativelyApproximateValue' wraps values that are considered to be equal if the absolute difference between the values divided by the average of the absolute values is within the given relative tolerance, /or/ if the absolute value of both values falls within the zero tolerance;  the latter case is checked because otherwise no value, no matter how small, would be approximately equal to zero.

The tolerance is specified through a type annotation.  One can use any annotation that one wishes as long as the type is an instance of 'AbsoluteTolerance' (for absolute tolerances) and/or 'RelativeTolerance' and 'ZeroTolerance' (for relative tolerances).  For convenience, this package provides the type 'Digits' that allows one to specify the tolerance in terms of the number of digits, making use of type-level natural numbers.  The annotation @Digits n@ sets the tolerance to @10^-n@, so that in the case of the absolute tolerance and the zero tolerance @n@ is the number of decimal places that numbers have to match to be equal to respectively either each other or to zero, and in the case of relative tolerance @n@ is (roughly) the number of leading digits that two numbers have to match in order to be equal to each other.
-}

module Data.Eq.Approximate
    (-- * Type wrappers
     AbsolutelyApproximateValue(..)
    ,RelativelyApproximateValue(..)
     -- * Classes for tolerance type annotations
     -- $classes

     -- ** Absolute tolerance
    ,AbsoluteTolerance(..)
    ,getAbsoluteTolerance
     -- ** Relative tolerance
    ,RelativeTolerance(..)
    ,getRelativeTolerance
     -- ** Zero tolerance
    ,ZeroTolerance(..)
    ,getZeroTolerance
     -- * Tolerance annotations using Digits
    ,Digits(..)
    ,unwrapDigits
    ,toleranceFromDigits
    ) where

import Control.Arrow
import Data.Function
import Text.Printf

import TypeLevel.NaturalNumber

{-|
The newtype 'AbsolutelyApproximateValue' is a wrapper that can contain an arbitrary value tagged with a tolerance; two values are equal to each other if the absolute difference is less than or equal to this tolerance.  The type annotation @absolute_tolerance@, which must be an instance of 'AbsoluteTolerance', specifies the tolerance.  For convenience, one may specify the tolerance using the type @Digits n@ where @n@ is a type-level natural specifying the number of decimals in the tolerance (i.e., @Digits Four@ specifies a tolerance of 0.0001).

It is recommended that one use this wrapper by creating aliases, such as

> type ApproximateDouble = AbsolutelyApproximateValue (Digits Five)
> wrapAD :: Double -> ApproximateDouble
> wrapAD = AbsolutelyApproximateValue
> unwrapAD :: ApproximateDouble -> Double
> unwrapAD = unwrapAbsolutelyApproximateValue

You can then replace the type 'Double' in your code with the type alias @ApproximateDouble@ to get the feature of approximate equality.  Most of the time you will find that you do not need to use wrapping functions to construct wrapped values since 'AbsolutelyApproximateValue' is an instance of whatever numerical types the wrapped value is, so that for example @1 + sqrt 2/3@ is already a value of type @ApproximateDouble@ without needing to be wrapped first.
-}
newtype AbsolutelyApproximateValue absolute_tolerance value =
        AbsolutelyApproximateValue { unwrapAbsolutelyApproximateValue :: value }
{-|
The newtype 'RelativelyApproximateValue' is a wrapper that can contain an arbitrary value tagged with a zero tolerance and a relative tolerance;  two values are equal to each other if their absolute values are both less than or equal to the zero tolerance, or if the absolute difference between them divided by the average of the absolute values is less than or equal to the relative tolerance.

The type annotation @zero_tolerance@, which must be an instance of 'ZeroTolerance', specifies the tolerance within which a value is considered to be equal to zero.  For convenience, one may specify the tolerance using the type @Digits n@ where @n@ is a type-level natural specifying the number of decimals in the tolerance (i.e., @Digits Four@ specifies a tolerance of 0.0001).

The type annotation @relative_tolerance@, which must be an instance of 'RelativeTolerance', specifies the relative tolerance within which two values that are not approximately equal to zero are considered to be equal to each other.  For convenience, as with the zero tolerance, one may specify the relative tolerance using the type @Digits n@ where @n@ is a type-level natural specifying the number of decimals in the tolerance (i.e., @Digits Four@ specifies a relative tolerance of 0.0001, so that two values are equal if they agree to the first four leading digits).

It is recommended that one use this wrapper by creating aliases, such as

> type ApproximateDouble = RelativelyApproximateValue (Digits Five) (Digits Five)
> wrapAD :: Double -> ApproximateDouble
> wrapAD = RelativelyApproximateValue
> unwrapAD :: ApproximateDouble -> Double
> unwrapAD = unwrapRelativelyApproximateValue

You can then replace the type 'Double' in your code with the type alias @ApproximateDouble@ to get the feature of approximate equality.  Most of the time you will find that you do not need to use wrapping functions to construct wrapped values since 'RelativelyApproximateValue' is an instance of whatever numerical types the wrapped value is, so that for example @1 + sqrt 2/3@ is already a value of type @ApproximateDouble@ without needing to be wrapped first.
-}
newtype RelativelyApproximateValue zero_tolerance relative_tolerance value =
        RelativelyApproximateValue { unwrapRelativelyApproximateValue :: value }
{-|
Digits is a type constructor that can be used to specify tolerances using type-level natural numbers.  Annotating a wrapper with the type @Digits n@ specifies that the corresponding tolerance has a numerical value of @10^(-n)@.
-}
data Digits n
-- $classes
-- The classes in this section are used to associate numerical tolerance information with the types that are used to annotate the type wrappers 'AbsolutelyApproximateValue' and 'RelativelyApproximateValue'.  One typically should not need to use them since the annotation @Digits n@ should cover most common cases.
-- 
-- One might consider hand-rolling instances in order to obtain faster code (since this will bypass the type-level natural numbers), however I have found in my own experience that using hand-rolled instances rather than @Digits Five@ in my own program resulted in code that was one or two orders of magnitude /slower/ than simply using 'Digits'.  This is not to claim that hand-rolled instances can /never/ be faster than using 'Digits', but one should be careful about trying this without checking whether there really is an improvement.  In general, 'Digits' seems to be fast enough in the common case that hand-rolling instances for speed is unlikely to be worth the trouble.
{-|
The class 'AbsoluteTolerance' is used to define the absolute tolerances associated with types that will be used as absolute tolerance type annotations in 'AbsolutelyApproximateValue'.
-}

class AbsoluteTolerance absolute_tolerance where
    {-|
        This method retrieves the numerical absolute tolerance associated with the type;  it should be a constant function.
    -}
    absoluteToleranceOf ::
        Fractional value =>
        AbsolutelyApproximateValue absolute_tolerance value ->
        value
{-|
The class 'RelativeTolerance' is used to define the relative tolerances associated with types that will be used as relative tolerance type annotations in 'RelativelyApproximateValue'.
-}

class RelativeTolerance relative_tolerance where
    {-|
        This method retrieves the numerical relative tolerance associated with the type;  it should be a constant function.
    -}
    relativeToleranceOf ::
        Fractional value =>
        RelativelyApproximateValue zero_tolerance relative_tolerance value ->
        value
{-|
The class 'ZeroTolerance' is used to define the numerical zero tolerances associated with types that will be used as zero tolerance type annotations in 'RelativelyApproximateValue'.
-}

class ZeroTolerance zero_tolerance where
    {-|
        This method retrieves the numerical zero tolerance associated with the type;  it should be a constant function.
    -}
    zeroToleranceOf ::
        Fractional value =>
        RelativelyApproximateValue zero_tolerance relative_tolerance value ->
        value
instance (AbsoluteTolerance tolerance
         ,Ord value
         ,Fractional value
         ,Show value
         ) =>
         Show (AbsolutelyApproximateValue tolerance value)
  where
    show x =
        show (unwrapAAV x)
        ++ " +/- " ++
        show (absoluteToleranceOf x)
instance (AbsoluteTolerance tolerance
         ,Ord value
         ,Fractional value
         ) =>
         Eq (AbsolutelyApproximateValue tolerance value)
  where
    a == b = abs (unwrapAAV a - unwrapAAV b) <= absoluteToleranceOf a
instance (AbsoluteTolerance tolerance
         ,Ord value
         ,Fractional value
         ) =>
         Ord (AbsolutelyApproximateValue tolerance value)
  where
    compare a b
      | a == b    = EQ
      | otherwise = (compare `on` unwrapAAV) a b
instance Enum value => Enum (AbsolutelyApproximateValue tolerance value) where
    succ = liftAAV1 succ
    pred = liftAAV1 pred
    toEnum = wrapAAV . toEnum
    fromEnum = unwrapAAVAndApply fromEnum
    enumFrom = map wrapAAV . enumFrom . unwrapAAV
    enumFromThen a b = map wrapAAV (enumFromThen (unwrapAAV a) (unwrapAAV b))
    enumFromTo a b = map wrapAAV (enumFromTo (unwrapAAV a) (unwrapAAV b))
    enumFromThenTo a b c = map wrapAAV (enumFromThenTo (unwrapAAV a) (unwrapAAV b) (unwrapAAV c))
instance
    (AbsoluteTolerance tolerance
    ,Ord value
    ,Fractional value
    ) => Num (AbsolutelyApproximateValue tolerance value)
  where
    (+) = liftAAV2 (+)
    (*) = liftAAV2 (*)
    (-) = liftAAV2 (-)
    negate = liftAAV1 negate
    abs = liftAAV1 abs
    signum = liftAAV1 signum
    fromInteger = wrapAAV . fromInteger
instance
    (AbsoluteTolerance tolerance
    ,Ord value
    ,Fractional value
    ,Real value
    ) => Real (AbsolutelyApproximateValue tolerance value)
  where
    toRational = unwrapAAVAndApply toRational
instance
    (AbsoluteTolerance tolerance
    ,Ord value
    ,Fractional value
    ,Integral value
    ) => Integral (AbsolutelyApproximateValue tolerance value)
  where
    quot = liftAAV2 quot
    rem = liftAAV2 rem
    div = liftAAV2 div
    mod = liftAAV2 mod
    quotRem a b = (wrapAAV *** wrapAAV) $ (quotRem `on` unwrapAAV) a b
    divMod a b = (wrapAAV *** wrapAAV) $ (divMod `on` unwrapAAV) a b
    toInteger = toInteger . unwrapAAV
instance
    (AbsoluteTolerance tolerance
    ,Ord value
    ,Fractional value
    ) => Fractional (AbsolutelyApproximateValue tolerance value)
  where
    (/) = liftAAV2 (/)
    recip = liftAAV1 recip
    fromRational = wrapAAV . fromRational
instance
    (AbsoluteTolerance tolerance
    ,Ord value
    ,Floating value
    ) => Floating (AbsolutelyApproximateValue tolerance value)
  where
    pi = wrapAAV pi
    exp = liftAAV1 exp
    sqrt = liftAAV1 sqrt
    log = liftAAV1 log
    (**) = liftAAV2 (**)
    logBase = liftAAV2 logBase
    sin = liftAAV1 sin
    tan = liftAAV1 tan
    cos = liftAAV1 cos
    asin = liftAAV1 asin
    atan = liftAAV1 atan
    acos = liftAAV1 acos
    sinh = liftAAV1 sinh
    tanh = liftAAV1 tanh
    cosh = liftAAV1 cosh
    asinh = liftAAV1 asinh
    atanh = liftAAV1 atanh
    acosh = liftAAV1 acosh
instance
    (AbsoluteTolerance tolerance
    ,Ord value
    ,RealFrac value
    ) => RealFrac (AbsolutelyApproximateValue tolerance value)
  where
    properFraction = second wrapAAV . unwrapAAVAndApply properFraction
    truncate = unwrapAAVAndApply truncate
    round = unwrapAAVAndApply round
    ceiling = unwrapAAVAndApply ceiling
    floor = unwrapAAVAndApply floor
instance
    (AbsoluteTolerance tolerance
    ,Ord value
    ,RealFloat value
    ) => RealFloat (AbsolutelyApproximateValue tolerance value)
  where
    floatRadix = unwrapAAVAndApply floatRadix
    floatDigits = unwrapAAVAndApply floatDigits
    floatRange = unwrapAAVAndApply floatRange
    decodeFloat = unwrapAAVAndApply decodeFloat
    encodeFloat x y = wrapAAV (encodeFloat x y)
    exponent = unwrapAAVAndApply exponent
    significand = liftAAV1 significand
    scaleFloat i = liftAAV1 (scaleFloat i)
    isNaN = unwrapAAVAndApply isNaN
    isInfinite = unwrapAAVAndApply isInfinite
    isDenormalized = unwrapAAVAndApply isDenormalized
    isNegativeZero = unwrapAAVAndApply isNegativeZero
    isIEEE = unwrapAAVAndApply isIEEE
    atan2 = liftAAV2 atan2
instance (ZeroTolerance zerotol
         ,RelativeTolerance reltol
         ,Ord value
         ,Fractional value
         ,Show value
         ) =>
         Show (RelativelyApproximateValue zerotol reltol value)
  where
    show a@(RelativelyApproximateValue x) =
        if a == 0
            then "0 +/- " ++ show (zeroToleranceOf a)
            else show x ++ " +/- " ++ show (x * relativeToleranceOf a * 100)
instance (ZeroTolerance zerotol
         ,RelativeTolerance reltol
         ,Ord value
         ,Fractional value
         ) =>
         Eq (RelativelyApproximateValue zerotol reltol value)
  where
    a == b =
        case (unwrapRAV a,unwrapRAV b) of
            (0,0) -> True
            (0,y) -> abs y <= zerotol
            (x,0) -> abs x <= zerotol
            (x,y)
              | abs x <= zerotol && abs y <= zerotol -> True
              | otherwise -> abs (x - y) / (abs x + abs y) * 2 <= reltol
      where
        zerotol = zeroToleranceOf a
        reltol = relativeToleranceOf a
instance (ZeroTolerance zerotol
         ,RelativeTolerance reltol
         ,Ord value
         ,Fractional value
         ) =>
         Ord (RelativelyApproximateValue zerotol reltol value)
  where
    compare a b
      | a == b    = EQ
      | otherwise = (compare `on` unwrapRAV) a b
instance Enum value => Enum (RelativelyApproximateValue zerotol reltol value) where
    succ = liftRAV1 succ
    pred = liftRAV1 pred
    toEnum = wrapRAV . toEnum
    fromEnum = unwrapRAVAndApply fromEnum
    enumFrom = map wrapRAV . enumFrom . unwrapRAV
    enumFromThen a b = map wrapRAV (enumFromThen (unwrapRAV a) (unwrapRAV b))
    enumFromTo a b = map wrapRAV (enumFromTo (unwrapRAV a) (unwrapRAV b))
    enumFromThenTo a b c = map wrapRAV (enumFromThenTo (unwrapRAV a) (unwrapRAV b) (unwrapRAV c))
instance
    (ZeroTolerance zerotol
    ,RelativeTolerance reltol
    ,Ord value
    ,Fractional value
    ) => Num (RelativelyApproximateValue zerotol reltol value)
  where
    (+) = liftRAV2 (+)
    (*) = liftRAV2 (*)
    (-) = liftRAV2 (-)
    negate = liftRAV1 negate
    abs = liftRAV1 abs
    signum = liftRAV1 signum
    fromInteger = wrapRAV . fromInteger
instance
    (ZeroTolerance zerotol
    ,RelativeTolerance reltol
    ,Ord value
    ,Fractional value
    ,Real value
    ) => Real (RelativelyApproximateValue zerotol reltol value)
  where
    toRational = unwrapRAVAndApply toRational
instance
    (ZeroTolerance zerotol
    ,RelativeTolerance reltol
    ,Ord value
    ,Fractional value
    ,Integral value
    ) => Integral (RelativelyApproximateValue zerotol reltol value)
  where
    quot = liftRAV2 quot
    rem = liftRAV2 rem
    div = liftRAV2 div
    mod = liftRAV2 mod
    quotRem a b = (wrapRAV *** wrapRAV) $ (quotRem `on` unwrapRAV) a b
    divMod a b = (wrapRAV *** wrapRAV) $ (divMod `on` unwrapRAV) a b
    toInteger = toInteger . unwrapRAV
instance
    (ZeroTolerance zerotol
    ,RelativeTolerance reltol
    ,Ord value
    ,Fractional value
    ) => Fractional (RelativelyApproximateValue zerotol reltol value)
  where
    (/) = liftRAV2 (/)
    recip = liftRAV1 recip
    fromRational = wrapRAV . fromRational
instance
    (ZeroTolerance zerotol
    ,RelativeTolerance reltol
    ,Ord value
    ,Floating value
    ) => Floating (RelativelyApproximateValue zerotol reltol value)
  where
    pi = wrapRAV pi
    exp = liftRAV1 exp
    sqrt = liftRAV1 sqrt
    log = liftRAV1 log
    (**) = liftRAV2 (**)
    logBase = liftRAV2 logBase
    sin = liftRAV1 sin
    tan = liftRAV1 tan
    cos = liftRAV1 cos
    asin = liftRAV1 asin
    atan = liftRAV1 atan
    acos = liftRAV1 acos
    sinh = liftRAV1 sinh
    tanh = liftRAV1 tanh
    cosh = liftRAV1 cosh
    asinh = liftRAV1 asinh
    atanh = liftRAV1 atanh
    acosh = liftRAV1 acosh
instance
    (ZeroTolerance zerotol
    ,RelativeTolerance reltol
    ,Ord value
    ,RealFrac value
    ) => RealFrac (RelativelyApproximateValue zerotol reltol value)
  where
    properFraction = second wrapRAV . unwrapRAVAndApply properFraction
    truncate = unwrapRAVAndApply truncate
    round = unwrapRAVAndApply round
    ceiling = unwrapRAVAndApply ceiling
    floor = unwrapRAVAndApply floor
instance
    (ZeroTolerance zerotol
    ,RelativeTolerance reltol
    ,Ord value
    ,RealFloat value
    ) => RealFloat (RelativelyApproximateValue zerotol reltol value)
  where
    floatRadix = unwrapRAVAndApply floatRadix
    floatDigits = unwrapRAVAndApply floatDigits
    floatRange = unwrapRAVAndApply floatRange
    decodeFloat = unwrapRAVAndApply decodeFloat
    encodeFloat x y = wrapRAV (encodeFloat x y)
    exponent = unwrapRAVAndApply exponent
    significand = liftRAV1 significand
    scaleFloat i = liftRAV1 (scaleFloat i)
    isNaN = unwrapRAVAndApply isNaN
    isInfinite = unwrapRAVAndApply isInfinite
    isDenormalized = unwrapRAVAndApply isDenormalized
    isNegativeZero = unwrapRAVAndApply isNegativeZero
    isIEEE = unwrapRAVAndApply isIEEE
    atan2 = liftRAV2 atan2
instance NaturalNumber n => AbsoluteTolerance (Digits n) where
    absoluteToleranceOf =
        toleranceFromDigits
        .
        getAbsoluteTolerance
instance NaturalNumber n => RelativeTolerance (Digits n) where
    relativeToleranceOf =
        toleranceFromDigits
        .
        getRelativeTolerance
instance NaturalNumber n => ZeroTolerance (Digits n) where
    zeroToleranceOf =
        toleranceFromDigits
        .
        getZeroTolerance
{-|
This is a convenience (constant) function for extracting the type-level natural number contained within the 'Digits' type constructor; it returns the value 'undefined', so don't try to evaluate the result.
-}
unwrapDigits :: Digits n -> n
unwrapDigits _ = undefined
{-|
This is a convenience (constant) function that computes the numerical tolerance specified by @Digits n@, which is @10^(-n)@.
-}
toleranceFromDigits :: NaturalNumber n => Fractional value => Digits n -> value
toleranceFromDigits =
    recip
    .
    fromInteger
    .
    ((10::Integer) ^)
    .
    naturalNumberAsInt
    .
    unwrapDigits
{-|
This is a convenience (constant) function for extracting the relative tolerance type annotation from 'AbsolutelyApproximateValue';  it returns the value 'undefined', so don't try to evaluate the result.
-}
getAbsoluteTolerance ::
    AbsolutelyApproximateValue absolute_tolerance value ->
    absolute_tolerance
getAbsoluteTolerance = undefined
{-|
This is a convenience (constant) function for extracting the relative tolerance type annotation from 'RelativelyApproximateValue';  it returns the value 'undefined', so don't try to evaluate the result.
-}
getRelativeTolerance ::
    RelativelyApproximateValue zero_tolerance relative_tolerance value ->
    relative_tolerance
getRelativeTolerance = undefined
{-|
This is a convenience (constant) function for extracting the zero tolerance type annotation from 'RelativelyApproximateValue';  it returns the value 'undefined', so don't try to evaluate the result.
-}
getZeroTolerance ::
    RelativelyApproximateValue zero_tolerance relative_tolerance value ->
    zero_tolerance
getZeroTolerance = undefined
{-# INLINE wrapAAV #-}
{-# INLINE unwrapAAV #-}
{-# INLINE liftAAV1 #-}
{-# INLINE liftAAV2 #-}
{-# INLINE unwrapAAVAndApply #-}
wrapAAV = AbsolutelyApproximateValue
unwrapAAV = unwrapAbsolutelyApproximateValue
liftAAV1 f = wrapAAV . f . unwrapAAV
liftAAV2 f a b = wrapAAV (f (unwrapAAV a) (unwrapAAV b))
unwrapAAVAndApply f = f . unwrapAAV

{-# INLINE wrapRAV #-}
{-# INLINE unwrapRAV #-}
{-# INLINE liftRAV1 #-}
{-# INLINE liftRAV2 #-}
{-# INLINE unwrapRAVAndApply #-}
wrapRAV = RelativelyApproximateValue
unwrapRAV = unwrapRelativelyApproximateValue
liftRAV1 f = wrapRAV . f . unwrapRAV
liftRAV2 f a b = wrapRAV (f (unwrapRAV a) (unwrapRAV b))
unwrapRAVAndApply f = f . unwrapRAV
