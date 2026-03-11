{-# LANGUAGE RebindableSyntax #-}
module Algebra.Transcendental where

import qualified Algebra.Algebraic as Algebraic

import qualified Algebra.Laws as Laws

import Algebra.Algebraic (sqrt)
import Algebra.Field     ((/), recip)
import Algebra.Ring      ((*), (^), fromInteger)
import Algebra.Additive  ((+), (-), negate)

import qualified Prelude as P
import NumericPrelude.Base


infixr 8  **, ^?

{-|
Transcendental is the type of numbers supporting the elementary
transcendental functions.  Examples include real numbers, complex
numbers, and computable reals represented as a lazy list of rational
approximations.

Note the default declaration for a superclass.  See the comments
below, under "Instance declaractions for superclasses".

The semantics of these operations are rather ill-defined because of
branch cuts, etc.

Minimal complete definition:
     pi, exp, (log or logBase), sin, cos, atan
-}
class (Algebraic.C a) => C a where
    {-# MINIMAL pi, exp, (log | logBase), sin, cos, atan #-}
    pi                  :: a
    exp, log            :: a -> a
    logBase, (**)       :: a -> a -> a
    sin, cos, tan       :: a -> a
    asin, acos, atan    :: a -> a
    sinh, cosh, tanh    :: a -> a
    asinh, acosh, atanh :: a -> a

    {-# INLINE logBase #-}
    {-# INLINE (**) #-}
    {-# INLINE tan #-}
    {-# INLINE asin #-}
    {-# INLINE acos #-}
    {-# INLINE sinh #-}
    {-# INLINE tanh #-}
    {-# INLINE cosh #-}
    {-# INLINE asinh #-}
    {-# INLINE atanh #-}
    {-# INLINE acosh #-}

    x ** y           =  exp (log x * y)
    logBase x y      =  log y / log x
    log              =  logBase (exp 1)

    tan  x           =  sin x / cos x

    asin x           =  atan (x / sqrt (1-x^2))
    acos x           =  pi/2 - asin x

    -- if these definitions have errors, then those in FMP.Types have them, too
    sinh x           =  (exp x - exp (-x)) / 2
    cosh x           =  (exp x + exp (-x)) / 2
    -- tanh x           =  (exp x - exp (-x)) / (exp x + exp (-x))
    tanh x           =  sinh x / cosh x

    asinh x          =  log (sqrt (x^2+1) + x)
    acosh x          =  log (sqrt (x^2-1) + x)
    atanh x          =  (log (1+x) - log (1-x)) / 2


instance C P.Float where
    {-# INLINE pi #-}
    {-# INLINE exp #-}
    {-# INLINE log #-}
    {-# INLINE logBase #-}
    {-# INLINE (**) #-}
    {-# INLINE sin #-}
    {-# INLINE tan #-}
    {-# INLINE cos #-}
    {-# INLINE asin #-}
    {-# INLINE atan #-}
    {-# INLINE acos #-}
    {-# INLINE sinh #-}
    {-# INLINE tanh #-}
    {-# INLINE cosh #-}
    {-# INLINE asinh #-}
    {-# INLINE atanh #-}
    {-# INLINE acosh #-}

    (**)  = (P.**)
    exp   = P.exp;   log   = P.log;   logBase = P.logBase
    pi    = P.pi;
    sin   = P.sin;   cos   = P.cos;   tan     = P.tan
    asin  = P.asin;  acos  = P.acos;  atan    = P.atan
    sinh  = P.sinh;  cosh  = P.cosh;  tanh    = P.tanh
    asinh = P.asinh; acosh = P.acosh; atanh   = P.atanh

instance C P.Double where
    {-# INLINE pi #-}
    {-# INLINE exp #-}
    {-# INLINE log #-}
    {-# INLINE logBase #-}
    {-# INLINE (**) #-}
    {-# INLINE sin #-}
    {-# INLINE tan #-}
    {-# INLINE cos #-}
    {-# INLINE asin #-}
    {-# INLINE atan #-}
    {-# INLINE acos #-}
    {-# INLINE sinh #-}
    {-# INLINE tanh #-}
    {-# INLINE cosh #-}
    {-# INLINE asinh #-}
    {-# INLINE atanh #-}
    {-# INLINE acosh #-}

    (**)  = (P.**)
    exp   = P.exp;   log   = P.log;   logBase = P.logBase
    pi    = P.pi;
    sin   = P.sin;   cos   = P.cos;   tan     = P.tan
    asin  = P.asin;  acos  = P.acos;  atan    = P.atan
    sinh  = P.sinh;  cosh  = P.cosh;  tanh    = P.tanh
    asinh = P.asinh; acosh = P.acosh; atanh   = P.atanh



{-# INLINE (^?) #-}
(^?) :: C a => a -> a -> a
(^?) = (**)


{-* Transcendental laws, will only hold approximately on floating point numbers -}

propExpLog      :: (Eq a, C a) => a -> Bool
propLogExp      :: (Eq a, C a) => a -> Bool
propExpNeg      :: (Eq a, C a) => a -> Bool
propLogRecip    :: (Eq a, C a) => a -> Bool
propExpProduct  :: (Eq a, C a) => a -> a -> Bool
propExpLogPower :: (Eq a, C a) => a -> a -> Bool
propLogSum      :: (Eq a, C a) => a -> a -> Bool

propExpLog      x   = exp (log x)     == x
propLogExp      x   = log (exp x)     == x
propExpNeg      x   = exp (negate x)  == recip (exp x)
propLogRecip    x   = log (recip x)   == negate (log x)
propExpProduct  x y = Laws.homomorphism exp (+) (*) x y
propExpLogPower x y = exp (log x * y) == x ** y
propLogSum      x y = Laws.homomorphism log (*) (+) x y


propPowerCascade      :: (Eq a, C a) => a -> a -> a -> Bool
propPowerProduct      :: (Eq a, C a) => a -> a -> a -> Bool
propPowerDistributive :: (Eq a, C a) => a -> a -> a -> Bool

propPowerCascade      x i j  =  Laws.rightCascade (*) (**) x i j
propPowerProduct      x i j  =  Laws.homomorphism (x**) (+) (*) i j
propPowerDistributive i x y  =  Laws.rightDistributive (**) (*) i x y

{- * Trigonometric laws, addition theorems -}

propTrigonometricPythagoras :: (Eq a, C a) => a -> Bool
propTrigonometricPythagoras x  =  cos x ^ 2 + sin x ^ 2 == 1

propSinPeriod   :: (Eq a, C a) => a -> Bool
propCosPeriod   :: (Eq a, C a) => a -> Bool
propTanPeriod   :: (Eq a, C a) => a -> Bool

propSinPeriod x = sin (x+2*pi) == sin x
propCosPeriod x = cos (x+2*pi) == cos x
propTanPeriod x = tan (x+2*pi) == tan x

propSinAngleSum  :: (Eq a, C a) => a -> a -> Bool
propCosAngleSum  :: (Eq a, C a) => a -> a -> Bool

propSinAngleSum x y  =  sin (x+y) == sin x * cos y + cos x * sin y
propCosAngleSum x y  =  cos (x+y) == cos x * cos y - sin x * sin y

propSinDoubleAngle :: (Eq a, C a) => a -> Bool
propCosDoubleAngle :: (Eq a, C a) => a -> Bool

propSinDoubleAngle x  =  sin (2*x) == 2 * sin x * cos x
propCosDoubleAngle x  =  cos (2*x) == 2 * cos x ^ 2 - 1

propSinSquare :: (Eq a, C a) => a -> Bool
propCosSquare :: (Eq a, C a) => a -> Bool

propSinSquare x  =  sin x ^ 2 == (1 - cos (2*x)) / 2
propCosSquare x  =  cos x ^ 2 == (1 + cos (2*x)) / 2

