{-# LANGUAGE RebindableSyntax #-}
{- |
This module implements polynomial functions on plain lists.
We use such functions in order to implement methods of other datatypes.

The module organization differs from that of @ResidueClass@:
Here the @Polynomial@ module exports the type
that fits to the NumericPrelude type classes,
whereas in @ResidueClass@ the sub-modules export various flavors of them.
-}
module MathObj.Polynomial.Core (
   horner, hornerCoeffVector, hornerArgVector,
   normalize,
   shift, unShift,
   equal,
   add, sub, negate,
   scale, collinear,
   tensorProduct, tensorProductAlt,
   mul, mulShear, mulShearTranspose,
   divMod, divModRev,
   stdUnit,
   progression, differentiate, integrate, integrateInt,
   mulLinearFactor,
   alternate, dilate, shrink,
   ) where

import qualified Algebra.Module               as Module
import qualified Algebra.Field                as Field
import qualified Algebra.IntegralDomain       as Integral
import qualified Algebra.Ring                 as Ring
import qualified Algebra.Additive             as Additive
import qualified Algebra.ZeroTestable         as ZeroTestable

import qualified Data.List.Reverse.StrictSpine as Rev
import qualified Data.List as List
import NumericPrelude.List (zipWithOverlap, )
import Data.Tuple.HT (mapPair, mapFst, forcePair, )
import Data.List.HT (switchL, shear, shearTranspose, outerProduct)

import qualified NumericPrelude.Base as P
import qualified NumericPrelude.Numeric as NP

import NumericPrelude.Base    hiding (const, reverse, )
import NumericPrelude.Numeric hiding (divMod, negate, stdUnit, )


{- $setup
>>> import qualified MathObj.Polynomial.Core as PolyCore
>>> import qualified MathObj.Polynomial as Poly
>>> import qualified Data.List as List
>>> import qualified Test.QuickCheck as QC
>>> import Test.QuickCheck ((==>))
>>> import Data.Tuple.HT (mapPair, mapSnd)
>>> import NumericPrelude.Numeric
>>> import NumericPrelude.Base
>>> import Prelude ()
>>>
>>> intPoly :: [Integer] -> [Integer]
>>> intPoly = id
>>>
>>> ratioPoly :: [Rational] -> [Rational]
>>> ratioPoly = id
-}


{- |
Horner's scheme for evaluating a polynomial in a ring.
-}
{-# INLINE horner #-}
horner :: Ring.C a => a -> [a] -> a
horner x = foldr (\c val -> c+x*val) zero

{- |
Horner's scheme for evaluating a polynomial in a module.
-}
{-# INLINE hornerCoeffVector #-}
hornerCoeffVector :: Module.C a v => a -> [v] -> v
hornerCoeffVector x = foldr (\c val -> c+x*>val) zero

{-# INLINE hornerArgVector #-}
hornerArgVector :: (Module.C a v, Ring.C v) => v -> [a] -> v
hornerArgVector x = foldr (\c val -> c*>one+val*x) zero


{- |
It's also helpful to put a polynomial in canonical form.
'normalize' strips leading coefficients that are zero.
-}
{-# INLINE normalize #-}
normalize :: (ZeroTestable.C a) => [a] -> [a]
normalize = Rev.dropWhile isZero

{- |
Multiply by the variable, used internally.
-}
{-# INLINE shift #-}
shift :: (Additive.C a) => [a] -> [a]
shift [] = []
shift l  = zero : l

{-# INLINE unShift #-}
unShift :: [a] -> [a]
unShift []     = []
unShift (_:xs) = xs

{-# INLINE equal #-}
equal :: (Eq a, ZeroTestable.C a) => [a] -> [a] -> Bool
equal x y = and (zipWithOverlap isZero isZero (==) x y)


add, sub :: (Additive.C a) => [a] -> [a] -> [a]
add = (+)
sub = (-)

{-# INLINE negate #-}
negate :: (Additive.C a) => [a] -> [a]
negate = map NP.negate


{-# INLINE scale #-}
scale :: Ring.C a => a -> [a] -> [a]
scale s = map (s*)


collinear :: (Eq a, Ring.C a) => [a] -> [a] -> Bool
collinear (x:xs) (y:ys) =
   if x==zero && y==zero
     then collinear xs ys
     else scale x ys == scale y xs
-- here at least one of xs and ys is empty
collinear xs ys =
   all (==zero) xs && all (==zero) ys


{- |
prop> \(QC.NonEmpty xs) (QC.NonEmpty ys) -> PolyCore.tensorProduct xs ys == List.transpose (PolyCore.tensorProduct ys (intPoly xs))
-}
{-# INLINE tensorProduct #-}
tensorProduct :: Ring.C a => [a] -> [a] -> [[a]]
tensorProduct = outerProduct (*)

tensorProductAlt :: Ring.C a => [a] -> [a] -> [[a]]
tensorProductAlt xs ys = map (flip scale ys) xs


{- |
'mul' is fast if the second argument is a short polynomial,
'MathObj.PowerSeries.**' relies on that fact.
-}

{-# INLINE mul #-}
mul :: Ring.C a => [a] -> [a] -> [a]
{- prevent from generation of many zeros
   if the first operand is the empty list -}
mul [] = P.const []
mul xs = foldr (\y zs -> let (v:vs) = scale y xs in v : add vs zs) []
-- this one fails on infinite lists
--    mul xs = foldr (\y zs -> add (scale y xs) (shift zs)) []

{- |
prop> \xs ys  ->  PolyCore.equal (intPoly $ PolyCore.mul xs ys) (PolyCore.mulShear xs ys)
-}
{-# INLINE mulShear #-}
mulShear :: Ring.C a => [a] -> [a] -> [a]
mulShear xs ys = map sum (shear (tensorProduct xs ys))

{-# INLINE mulShearTranspose #-}
mulShearTranspose :: Ring.C a => [a] -> [a] -> [a]
mulShearTranspose xs ys = map sum (shearTranspose (tensorProduct xs ys))


{- |
prop> \x y -> case (PolyCore.normalize x, PolyCore.normalize y) of (nx, ny) -> not (null (ratioPoly ny)) ==> mapSnd PolyCore.normalize (PolyCore.divMod nx ny) == mapPair (PolyCore.normalize, PolyCore.normalize) (PolyCore.divMod x y)
prop> \x y -> not (isZero (ratioPoly y)) ==> let z = fst $ PolyCore.divMod (Poly.coeffs x) y in  PolyCore.normalize z == z
prop> \x y -> case PolyCore.normalize $ ratioPoly y of ny -> not (null ny) ==> List.length (snd $ PolyCore.divMod x y) < List.length ny
-}
divMod :: (ZeroTestable.C a, Field.C a) => [a] -> [a] -> ([a], [a])
divMod x y =
   mapPair (List.reverse, List.reverse) $
   divModRev (List.reverse x) (List.reverse y)

{-
snd $ Poly.divMod (repeat (1::Double)) [1,1]
-}
{- |
The modulus will always have one element less than the divisor.
This means that the modulus will be denormalized in some cases,
e.g. @mod [2,1,1] [1,1,1] == [1,0]@ instead of @[1]@.
-}
divModRev :: (ZeroTestable.C a, Field.C a) => [a] -> [a] -> ([a], [a])
divModRev x y =
   case dropWhile isZero y of
      [] -> error "MathObj.Polynomial: division by zero"
      y0:ys ->
         let -- the second parameter represents lazily (length x - length (normalize y))
             aux xs' =
               forcePair .
               switchL
                 ([], xs')
                 (P.const $
                    let (x0:xs) = xs'
                        q0      = x0/y0
                    in  mapFst (q0:) . aux (sub xs (scale q0 ys)))
         in  aux x (drop (length ys) x)

{-# INLINE stdUnit #-}
stdUnit :: (ZeroTestable.C a, Ring.C a) => [a] -> a
stdUnit x = case normalize x of
    [] -> one
    l  -> last l


{-# INLINE progression #-}
progression :: Ring.C a => [a]
progression = iterate (one+) one

{-# INLINE differentiate #-}
differentiate :: (Ring.C a) => [a] -> [a]
differentiate = zipWith (*) progression . drop 1

{-# INLINE integrate #-}
integrate :: (Field.C a) => a -> [a] -> [a]
integrate c x = c : zipWith (/) x progression

{- |
Integrates if it is possible to represent the integrated polynomial
in the given ring.
Otherwise undefined coefficients occur.
-}
{-# INLINE integrateInt #-}
integrateInt :: (ZeroTestable.C a, Integral.C a) => a -> [a] -> [a]
integrateInt c x =
   c : zipWith Integral.divChecked x progression


{-# INLINE mulLinearFactor #-}
mulLinearFactor :: Ring.C a => a -> [a] -> [a]
mulLinearFactor x yt@(y:ys) = Additive.negate (x*y) : yt - scale x ys
mulLinearFactor _ [] = []

{-# INLINE alternate #-}
alternate :: Additive.C a => [a] -> [a]
alternate = zipWith ($) (cycle [id, Additive.negate])

{-# INLINE shrink #-}
shrink :: Ring.C a => a -> [a] -> [a]
shrink k = zipWith (*) (iterate (k*) one)

{-# INLINE dilate #-}
dilate :: Field.C a => a -> [a] -> [a]
dilate = shrink . Field.recip


{-
see htam: Wavelet/DyadicResultant

resultant :: Ring.C a => [a] -> [a] -> [a]
resultant xs ys =

discriminant :: Ring.C a => [a] -> a
discriminant xs =
   let degree = genericLength xs
   in  parityFlip (divChecked (degree*(degree-1)) 2)
                  (resultant xs (differentiate xs))
          `divChecked` last xs
-}

