{-# LANGUAGE RebindableSyntax #-}
module MathObj.PowerSeries.Core where

import qualified MathObj.Polynomial.Core as Poly

import qualified Algebra.Module         as Module
import qualified Algebra.Transcendental as Transcendental
import qualified Algebra.Field          as Field
import qualified Algebra.Ring           as Ring
import qualified Algebra.Additive       as Additive
import qualified Algebra.ZeroTestable   as ZeroTestable

import qualified Data.List.Match as Match
import qualified NumericPrelude.Numeric as NP
import qualified NumericPrelude.Base as P

import NumericPrelude.Base    hiding (const)
import NumericPrelude.Numeric hiding (negate, stdUnit, divMod,
                              sqrt, exp, log,
                              sin, cos, tan, asin, acos, atan)


{- $setup
>>> import qualified MathObj.PowerSeries.Core as PS
>>> import qualified MathObj.PowerSeries.Example as PSE
>>> import Test.NumericPrelude.Utility (equalTrunc, (/\))
>>> import qualified Test.QuickCheck as QC
>>> import NumericPrelude.Numeric as NP
>>> import NumericPrelude.Base as P
>>> import Prelude ()
>>> import Control.Applicative (liftA3)
>>>
>>> checkHoles ::
>>>    Int -> ([Rational] -> [Rational]) ->
>>>    Rational -> [Rational] -> QC.Property
>>> checkHoles trunc f x xs =
>>>    QC.choose (1,10) /\ \expon ->
>>>    equalTrunc trunc
>>>       (f (PS.insertHoles expon (x:xs)) ++ repeat zero)
>>>       (PS.insertHoles expon (f (x:xs)) ++ repeat zero)
>>>
>>> genInvertible :: QC.Gen [Rational]
>>> genInvertible =
>>>    liftA3 (\x0 x1 xs -> x0:x1:xs)
>>>       QC.arbitrary (fmap QC.getNonZero QC.arbitrary) QC.arbitrary
-}


{-# INLINE evaluate #-}
evaluate :: Ring.C a => [a] -> a -> a
evaluate = flip Poly.horner

{-# INLINE evaluateCoeffVector #-}
evaluateCoeffVector :: Module.C a v => [v] -> a -> v
evaluateCoeffVector = flip Poly.hornerCoeffVector

{-# INLINE evaluateArgVector #-}
evaluateArgVector :: (Module.C a v, Ring.C v) => [a] -> v -> v
evaluateArgVector = flip Poly.hornerArgVector


{-# INLINE approximate #-}
approximate :: Ring.C a => [a] -> a -> [a]
approximate y x =
   scanl (+) zero (zipWith (*) (iterate (x*) 1) y)

{-# INLINE approximateCoeffVector #-}
approximateCoeffVector :: Module.C a v => [v] -> a -> [v]
approximateCoeffVector y x =
   scanl (+) zero (zipWith (*>) (iterate (x*) 1) y)

{-# INLINE approximateArgVector #-}
approximateArgVector :: (Module.C a v, Ring.C v) => [a] -> v -> [v]
approximateArgVector y x =
   scanl (+) zero (zipWith (*>) y (iterate (x*) 1))


{- * Simple series manipulation -}

{- |
For the series of a real function @f@
compute the series for @\x -> f (-x)@
-}

alternate :: Additive.C a => [a] -> [a]
alternate = zipWith id (cycle [id, NP.negate])

{- |
For the series of a real function @f@
compute the series for @\x -> (f x + f (-x)) \/ 2@
-}

holes2 :: Additive.C a => [a] -> [a]
holes2 = zipWith id (cycle [id, P.const zero])

{- |
For the series of a real function @f@
compute the real series for @\x -> (f (i*x) + f (-i*x)) \/ 2@
-}
holes2alternate :: Additive.C a => [a] -> [a]
holes2alternate =
   zipWith id (cycle [id, P.const zero, NP.negate, P.const zero])


{- |
For power series of @f x@, compute the power series of @f(x^n)@.

prop> QC.choose (1,10) /\ \m -> QC.choose (1,10) /\ \n xs -> equalTrunc 100 (PS.insertHoles m $ PS.insertHoles n xs) (PS.insertHoles (m*n) xs)
-}
insertHoles :: Additive.C a => Int -> [a] -> [a]
insertHoles n =
   if n<=0
     then error $ "insertHoles requires positive exponent, but got " ++ show n
     else concatMap (\x -> x : replicate (n-1) zero)


{- * Series arithmetic -}

add, sub :: (Additive.C a) => [a] -> [a] -> [a]
add = Poly.add
sub = Poly.sub

negate :: (Additive.C a) => [a] -> [a]
negate = Poly.negate

scale :: Ring.C a => a -> [a] -> [a]
scale = Poly.scale

mul :: Ring.C a => [a] -> [a] -> [a]
mul = Poly.mul


stripLeadZero :: (ZeroTestable.C a) => [a] -> [a] -> ([a],[a])
stripLeadZero (x:xs) (y:ys) =
  if isZero x && isZero y
    then stripLeadZero xs ys
    else (x:xs,y:ys)
stripLeadZero xs ys = (xs,ys)


divMod :: (ZeroTestable.C a, Field.C a) => [a] -> [a] -> ([a],[a])
divMod xs ys =
   let (yZero,yRem) = span isZero ys
       (xMod, xRem) = Match.splitAt yZero xs
   in  (divide xRem yRem, xMod)

{- |
Divide two series where the absolute term of the divisor is non-zero.
That is, power series with leading non-zero terms are the units
in the ring of power series.

Knuth: Seminumerical algorithms
-}
divide :: (Field.C a) => [a] -> [a] -> [a]
divide (x:xs) (y:ys) =
   let zs = map (/y) (x : sub xs (mul zs ys))
   in  zs
divide [] _ = []
divide _ [] = error "PowerSeries.divide: division by empty series"

{- |
Divide two series also if the divisor has leading zeros.
-}
divideStripZero :: (ZeroTestable.C a, Field.C a) => [a] -> [a] -> [a]
divideStripZero x' y' =
   let (x0,y0) = stripLeadZero x' y'
   in  if null y0 || isZero (head y0)
         then error "PowerSeries.divideStripZero: Division by zero."
         else divide x0 y0


progression :: Ring.C a => [a]
progression = Poly.progression

recipProgression :: (Field.C a) => [a]
recipProgression = map recip progression

differentiate :: (Ring.C a) => [a] -> [a]
differentiate = Poly.differentiate

integrate :: (Field.C a) => a -> [a] -> [a]
integrate = Poly.integrate


{- |
We need to compute the square root only of the first term.
That is, if the first term is rational,
then all terms of the series are rational.

prop> equalTrunc 50 PSE.sqrtExpl (PS.sqrt (\1 -> 1) [1,1])
prop> equalTrunc 500 (1:1:repeat 0) (PS.sqrt (\1 -> 1) (PS.mul [1,1] [1,1]))
prop> checkHoles 50 (PS.sqrt (\1 -> 1)) 1
-}
sqrt :: Field.C a => (a -> a) -> [a] -> [a]
sqrt _ [] = []
sqrt f0 (x:xs) =
   let y  = f0 x
       ys = map (/(y+y)) (xs - (0 : mul ys ys))
   in  y:ys

{-
pow alpha t = t^alpha
(pow alpha . x)' = alpha * (pow (alpha-1) . x) * x'
(pow alpha . x)' * x = alpha * (pow alpha . x) * x'

y = pow alpha . x
y' * x = alpha * y * x'

This yields an implementation that is a fused
exp (alpha * log x)
-}

{- |
Input series must start with a non-zero term,
even better with a positive one.

prop> equalTrunc 100 (PSE.powExpl (-1/3)) (PS.pow (\1 -> 1) (-1/3) [1,1])
prop> equalTrunc 50 (PSE.powExpl (-1/3)) (PS.exp (\0 -> 1) (PS.scale (-1/3) PSE.log))
prop> checkHoles 30 (PS.pow (\1 -> 1) (1/3)) 1
prop> checkHoles 30 (PS.pow (\1 -> 1) (2/5)) 1
-}
pow :: (Field.C a) => (a -> a) -> a -> [a] -> [a]
pow f0 expon x =
   let y  = integrate (f0 (head x)) y'
       y' = scale expon (mul y (derivedLog x))
   in  y


{- |
The first term needs a transcendent computation but the others do not.
That's why we accept a function which computes the first term.

> (exp . x)' =   (exp . x) * x'
> (sin . x)' =   (cos . x) * x'
> (cos . x)' = - (sin . x) * x'

prop> equalTrunc 500 PSE.expExpl (PS.exp (\0 -> 1) [0,1])
prop> equalTrunc 100 (1:1:repeat 0) (PS.exp (\0 -> 1) PSE.log)
prop> checkHoles 30 (PS.exp (\0 -> 1)) 0
-}
exp :: Field.C a => (a -> a) -> [a] -> [a]
exp f0 x =
   let x' = differentiate x
       y  = integrate (f0 (head x)) (mul y x')
   in  y

sinCos :: Field.C a => (a -> (a,a)) -> [a] -> ([a],[a])
sinCos f0 x =
   let (y0Sin, y0Cos) = f0 (head x)
       x'   = differentiate x
       ySin = integrate y0Sin         (mul yCos x')
       yCos = integrate y0Cos (negate (mul ySin x'))
   in  (ySin, yCos)

sinCosScalar :: Transcendental.C a => a -> (a,a)
sinCosScalar x = (Transcendental.sin x, Transcendental.cos x)

{- |
prop> equalTrunc 500 PSE.sinExpl (PS.sin (\0 -> (0,1)) [0,1])
prop> equalTrunc 50 (0:1:repeat 0) (PS.sin (\0 -> (0,1)) PSE.asin)
prop> checkHoles 20 (PS.sin (\0 -> (0,1))) 0
-}
sin :: Field.C a => (a -> (a,a)) -> [a] -> [a]
sin f0 = fst . sinCos f0
{- |
prop> equalTrunc 500 PSE.cosExpl (PS.cos (\0 -> (0,1)) [0,1])
prop> checkHoles 20 (PS.cos (\0 -> (0,1))) 0
-}
cos :: Field.C a => (a -> (a,a)) -> [a] -> [a]
cos f0 = snd . sinCos f0

{- |
prop> equalTrunc 50 PSE.tanExpl (PS.tan (\0 -> (0,1)) [0,1])
prop> equalTrunc 50 (0:1:repeat 0) (PS.tan (\0 -> (0,1)) PSE.atan)
prop> checkHoles 20 (PS.tan (\0 -> (0,1))) 0
-}
tan :: (Field.C a) => (a -> (a,a)) -> [a] -> [a]
tan f0 = uncurry divide . sinCos f0

{-
(log x)' == x'/x
(asin x)' == (acos x) == x'/sqrt(1-x^2)
(atan x)' == x'/(1+x^2)
-}

{- |
Input series must start with non-zero term.

prop> equalTrunc 500 PSE.logExpl (PS.log (\1 -> 0) [1,1])
prop> equalTrunc 100 (0:1:repeat 0) (PS.log (\1 -> 0) PSE.exp)
prop> checkHoles 30 (PS.log (\1 -> 0)) 1
-}
log :: (Field.C a) => (a -> a) -> [a] -> [a]
log f0 x = integrate (f0 (head x)) (derivedLog x)

{- |
Computes @(log x)'@, that is @x'\/x@
-}
derivedLog :: (Field.C a) => [a] -> [a]
derivedLog x = divide (differentiate x) x

{- |
prop> equalTrunc 500 PSE.atan (PS.atan (\0 -> 0) [0,1])
prop> equalTrunc 50 (0:1:repeat 0) (PS.atan (\0 -> 0) PSE.tan)
prop> checkHoles 20 (PS.atan (\0 -> 0)) 0
-}
atan :: (Field.C a) => (a -> a) -> [a] -> [a]
atan f0 x =
   let x' = differentiate x
   in  integrate (f0 (head x)) (divide x' ([1] + mul x x))

{- |
prop> equalTrunc 100 (0:1:repeat 0) (PS.asin (\1 -> 1) (\0 -> 0) PSE.sin)
prop> equalTrunc 50 PSE.asin (PS.asin (\1 -> 1) (\0 -> 0) [0,1])
prop> checkHoles 30 (PS.asin (\1 -> 1) (\0 -> 0)) 0
-}
asin :: (Field.C a) => (a -> a) -> (a -> a) -> [a] -> [a]
asin sqrt0 f0 x =
   let x' = differentiate x
   in  integrate (f0 (head x))
                 (divide x' (sqrt sqrt0 ([1] - mul x x)))

{- |
Would be a nice test, but we cannot compute exactly with 'pi':

> equalTrunc 50 PSE.acos (PS.acos (\1 -> 1) (\0 -> pi/2) [0,1])
-}
acos :: (Field.C a) => (a -> a) -> (a -> a) -> [a] -> [a]
acos = asin

{- |
Since the inner series must start with a zero,
the first term is omitted in y.
-}
compose :: (Ring.C a) => [a] -> [a] -> [a]
compose xs y = foldr (\x acc -> x : mul y acc) [] xs


{- |
Compose two power series where the outer series
can be developed for any expansion point.
To be more precise:
The outer series must be expanded with respect to the leading term
of the inner series.
-}
composeTaylor :: Ring.C a => (a -> [a]) -> [a] -> [a]
composeTaylor x (y:ys) = compose (x y) ys
composeTaylor x []     = x 0


{-
X(t) = t*x(t)
R(t) = t*r(t)

r(t) = 1 / (x(r(t)*t))
R(t)/t
   = 1 / (x(R(t)))
   = 1 / (X(R(t)) / R(t))
   = 1 / (t / R(t))
-}

{- |
This function returns the series of the inverse function in the form:
(point of the expansion, power series).

That is, say we have the equation:

> y = a + f(x)

where function f is given by a power series with f(0) = 0.
We want to solve for x:

> x = f^-1(y-a)

If you pass the power series of @a+f(x)@ to 'inv',
you get @(a, f^-1)@ as answer, where @f^-1@ is a power series.

The linear term of @f@ (the coefficient of @x@) must be non-zero.

This needs cubic run-time and thus is exceptionally slow.
Computing inverse series for special power series might be faster.

prop> genInvertible /\ \xs -> let (y,ys) = PS.inv xs; (z,zs) = PS.invDiff xs in y==z && equalTrunc 15 ys zs
-}
-- how about NonEmpty.T here?
inv :: (Eq a, Field.C a) => [a] -> (a, [a])
inv [] = error "inv: power series must be non-zero"
inv (x:xs) =
   (x, let r = divide [1] (compose xs r) in 0 : r)


{-
(x . y) = id
(x' . y) * y' = 1
y' = 1 / (x' . y)
-}

{-
Like 'inv' but with a slightly cumbersome implementation.
-}
invDiff :: (Field.C a) => [a] -> (a, [a])
invDiff x =
   let y' = divide [1] (compose (differentiate x) (tail y))
       y  = integrate 0 y'
            -- the first term is zero, which is required for composition
   in  (head x, y)
