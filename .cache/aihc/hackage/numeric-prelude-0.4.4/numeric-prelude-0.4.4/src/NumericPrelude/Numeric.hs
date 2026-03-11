{-# LANGUAGE RebindableSyntax #-}
module NumericPrelude.Numeric (
    {- Additive -} (+), (-), negate, zero, subtract, sum, sum1,
    {- ZeroTestable -} isZero,
    {- Ring -} (*), one, fromInteger, (^), ringPower, sqr, product, product1,
    {- IntegralDomain -} div, mod, divMod, divides, even, odd,
    {- Field -} (/), recip, fromRational', (^-), fieldPower, fromRational,
    {- Algebraic -} (^/), sqrt,
    {- Transcendental -}
        pi, exp, log, logBase, (**), (^?), sin, cos, tan,
        asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh,
    {- Absolute -} abs, signum,
    {- RealIntegral -} quot, rem, quotRem,
    {- RealFrac -} splitFraction, fraction, truncate, round, ceiling, floor, approxRational,
    {- RealTrans -} atan2,
    {- ToRational -} toRational,
    {- ToInteger -} toInteger, fromIntegral,
    {- Units -} isUnit, stdAssociate, stdUnit, stdUnitInv,
    {- PID -} extendedGCD, gcd, lcm, euclid, extendedEuclid,
    {- Ratio -} Rational, (%), numerator, denominator,
    Integer, Int, Float, Double,
    {- Module -} (*>)
) where

import Number.Ratio (Rational, (%), numerator, denominator)

import Algebra.Module((*>))
import Algebra.RealTranscendental(atan2)
import Algebra.Transcendental
import Algebra.Algebraic((^/), sqrt)
import Algebra.RealRing(splitFraction, fraction, truncate, round, ceiling, floor, approxRational, )
import Algebra.Field((/), (^-), recip, fromRational', fromRational, )
import Algebra.PrincipalIdealDomain (extendedGCD, gcd, lcm, euclid, extendedEuclid)
import Algebra.Units (isUnit, stdAssociate, stdUnit, stdUnitInv)
import Algebra.RealIntegral (quot, rem, quotRem, )
import Algebra.IntegralDomain (div, mod, divMod, divides, even, odd)
import Algebra.Absolute (abs, signum, )
import Algebra.Ring (one, fromInteger, (*), (^), sqr, product, product1)
import Algebra.Additive (zero, (+), (-), negate, subtract, sum, sum1)
import Algebra.ZeroTestable (isZero)
import Algebra.ToInteger (ringPower, fieldPower, toInteger, fromIntegral, )
import Algebra.ToRational (toRational, )

import Prelude (Int, Integer, Float, Double)
