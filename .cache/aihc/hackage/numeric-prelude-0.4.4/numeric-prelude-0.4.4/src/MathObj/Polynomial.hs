{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
Polynomials and rational functions in a single indeterminate.
Polynomials are represented by a list of coefficients.
All non-zero coefficients are listed, but there may be extra '0's at the end.

Usage:
Say you have the ring of 'Integer' numbers
and you want to add a transcendental element @x@,
that is an element, which does not allow for simplifications.
More precisely, for all positive integer exponents @n@
the power @x^n@ cannot be rewritten as a sum of powers with smaller exponents.
The element @x@ must be represented by the polynomial @[0,1]@.

In principle, you can have more than one transcendental element
by using polynomials whose coefficients are polynomials as well.
However, most algorithms on multi-variate polynomials
prefer a different (sparse) representation,
where the ordering of elements is not so fixed.

If you want division, you need "Number.Ratio"s
of polynomials with coefficients from a "Algebra.Field".

You can also compute with an algebraic element,
that is an element which satisfies an algebraic equation like
@x^3-x-1==0@.
Actually, powers of @x@ with exponents above @3@ can be simplified,
since it holds @x^3==x+1@.
You can perform these computations with "Number.ResidueClass" of polynomials,
where the divisor is the polynomial equation that determines @x@.
If the polynomial is irreducible
(in our case @x^3-x-1@ cannot be written as a non-trivial product)
then the residue classes also allow unrestricted division
(except by zero, of course).
That is, using residue classes of polynomials
you can work with roots of polynomial equations
without representing them by radicals
(powers with fractional exponents).
It is well-known, that roots of polynomials of degree above 4
may not be representable by radicals.
-}

module MathObj.Polynomial
   (T, fromCoeffs, coeffs, degree,
    showsExpressionPrec, const,
    evaluate, evaluateCoeffVector, evaluateArgVector,
    collinear,
    integrate,
    compose, fromRoots, reverse,
    translate, dilate, shrink, )
where

import qualified MathObj.Polynomial.Core as Core

import qualified Algebra.Differential         as Differential
import qualified Algebra.VectorSpace          as VectorSpace
import qualified Algebra.Module               as Module
import qualified Algebra.Vector               as Vector
import qualified Algebra.Field                as Field
import qualified Algebra.PrincipalIdealDomain as PID
import qualified Algebra.Units                as Units
import qualified Algebra.IntegralDomain       as Integral
import qualified Algebra.Ring                 as Ring
import qualified Algebra.Additive             as Additive
import qualified Algebra.ZeroTestable         as ZeroTestable
import qualified Algebra.Indexable            as Indexable

import Control.Monad (liftM, )
import qualified Data.List as List

import Test.QuickCheck (Arbitrary(arbitrary))

import qualified MathObj.Wrapper.Haskell98 as W98

import NumericPrelude.Base    hiding (const, reverse, )
import NumericPrelude.Numeric

import qualified Prelude as P98


{- $setup
>>> import qualified MathObj.Polynomial as Poly
>>> import qualified Algebra.IntegralDomain as Integral
>>> import qualified Algebra.Laws as Laws
>>> import NumericPrelude.Numeric
>>> import NumericPrelude.Base
>>> import Prelude ()
>>>
>>> intPoly :: Poly.T Integer -> Poly.T Integer
>>> intPoly = id
>>>
>>> ratioPoly :: Poly.T Rational -> Poly.T Rational
>>> ratioPoly = id
-}

{- |
prop> Laws.identity (+) zero . intPoly
prop> Laws.commutative (+) . intPoly
prop> Laws.associative (+) . intPoly
prop> Laws.identity (*) one . intPoly
prop> Laws.commutative (*) . intPoly
prop> Laws.associative (*) . intPoly
prop> Laws.leftDistributive (*) (+) . intPoly
prop> Integral.propInverse . ratioPoly
-}
newtype T a = Cons {coeffs :: [a]}

{-
>>> import Test.QuickCheck ((==>))
-}


{-# INLINE fromCoeffs #-}
fromCoeffs :: [a] -> T a
fromCoeffs = lift0

{-# INLINE lift0 #-}
lift0 :: [a] -> T a
lift0 = Cons

{-# INLINE lift1 #-}
lift1 :: ([a] -> [a]) -> (T a -> T a)
lift1 f (Cons x0) = Cons (f x0)

{-# INLINE lift2 #-}
lift2 :: ([a] -> [a] -> [a]) -> (T a -> T a -> T a)
lift2 f (Cons x0) (Cons x1) = Cons (f x0 x1)

degree :: (ZeroTestable.C a) => T a -> Maybe Int
degree x =
   case Core.normalize (coeffs x) of
      [] -> Nothing
      (_:xs) -> Just $ length xs

{-
Functor instance is e.g. useful for showing polynomials in residue rings.
@fmap (ResidueClass.concrete 7) (polynomial [1,4,4::ResidueClass.T Integer] * polynomial [1,5,6])@
-}

instance Functor T where
   fmap f (Cons xs) = Cons (map f xs)

{-# INLINE plusPrec #-}
{-# INLINE appPrec #-}
plusPrec, appPrec :: Int
plusPrec = 6
appPrec  = 10

instance (Show a) => Show (T a) where
   showsPrec p (Cons xs) =
      showParen (p >= appPrec) (showString "Polynomial.fromCoeffs " . shows xs)

{-# INLINE showsExpressionPrec #-}
showsExpressionPrec :: (Show a, ZeroTestable.C a, Additive.C a) =>
   Int -> String -> T a -> String -> String
showsExpressionPrec p var poly =
    if isZero poly
      then showString "0"
      else
        let terms = filter (not . isZero . fst)
                       (zip (coeffs poly) monomials)
            monomials = id :
                        showString "*" . showString var :
                        map (\k -> showString "*" . showString var
                                 . showString "^" . shows k)
                            [(2::Int)..]
            showsTerm x showsMon = showsPrec (plusPrec+1) x . showsMon
        in showParen (p > plusPrec)
           (foldl (.) id $ List.intersperse (showString " + ") $
            map (uncurry showsTerm) terms)


{-# INLINE evaluate #-}
evaluate :: Ring.C a => T a -> a -> a
evaluate (Cons y) x = Core.horner x y

{- |
Here the coefficients are vectors,
for example the coefficients are real and the coefficents are real vectors.
-}
{-# INLINE evaluateCoeffVector #-}
evaluateCoeffVector :: Module.C a v => T v -> a -> v
evaluateCoeffVector (Cons y) x = Core.hornerCoeffVector x y

{- |
Here the argument is a vector,
for example the coefficients are complex numbers or square matrices
and the coefficents are reals.
-}
{-# INLINE evaluateArgVector #-}
evaluateArgVector :: (Module.C a v, Ring.C v) => T a -> v -> v
evaluateArgVector (Cons y) x = Core.hornerArgVector x y

{- |
'compose' is the functional composition of polynomials.

It fulfills
  @ eval x . eval y == eval (compose x y) @
-}

-- compose :: Module.C a b => T b -> T a -> T a
-- compose (Cons x) y = Core.horner y (map const x)
{-# INLINE compose #-}
compose :: (Ring.C a) => T a -> T a -> T a
compose (Cons x) y = Core.horner y (map const x)

{-# INLINE const #-}
const :: a -> T a
const x = lift0 [x]


collinear :: (Eq a, Ring.C a) => T a -> T a -> Bool
collinear (Cons x) (Cons y) = Core.collinear x y


instance (Eq a, ZeroTestable.C a) => Eq (T a) where
   (Cons x) == (Cons y) = Core.equal x y

instance (Indexable.C a, ZeroTestable.C a) => Indexable.C (T a) where
   compare = Indexable.liftCompare coeffs

instance (ZeroTestable.C a) => ZeroTestable.C (T a) where
   isZero (Cons x) = isZero x


instance (Additive.C a) => Additive.C (T a) where
   (+)    = lift2 Core.add
   (-)    = lift2 Core.sub
   zero   = lift0 []
   negate = lift1 Core.negate


instance Vector.C T where
   zero  = zero
   (<+>) = (+)
   (*>)  = Vector.functorScale

instance (Module.C a b) => Module.C a (T b) where
   (*>) x = lift1 (x *>)

instance (Field.C a, Module.C a b) => VectorSpace.C a (T b)


instance (Ring.C a) => Ring.C (T a) where
   one         = const one
   fromInteger = const . fromInteger
   (*)         = lift2 Core.mul


{- |
The 'Integral.C' instance is intensionally built
from the 'Field.C' structure of the polynomial coefficients.
If we would use @Integral.C a@ superclass,
then the Euclidean algorithm could not determine
the greatest common divisor of e.g. @[1,1]@ and @[2]@.
-}
instance (ZeroTestable.C a, Field.C a) => Integral.C (T a) where
   divMod (Cons x) (Cons y) =
      let (d,m) = Core.divMod x y
      in  (Cons d, Cons m)

instance (ZeroTestable.C a, Field.C a) => Units.C (T a) where
   isUnit (Cons []) = False
   isUnit (Cons (x0:xs)) = not (isZero x0) && all isZero xs
   stdUnit    (Cons x) = const        (Core.stdUnit x)
   stdUnitInv (Cons x) = const (recip (Core.stdUnit x))

{-
Polynomials are a Euclidean domain, so no instance is necessary
(although it might be faster).
-}

instance (ZeroTestable.C a, Field.C a) => PID.C (T a)


instance (Ring.C a) => Differential.C (T a) where
   differentiate = lift1 Core.differentiate


{-# INLINE integrate #-}
integrate :: (Field.C a) => a -> T a -> T a
integrate = lift1 . Core.integrate



{-# INLINE fromRoots #-}
fromRoots :: (Ring.C a) => [a] -> T a
fromRoots = Cons . foldl (flip Core.mulLinearFactor) [one]

{-# INLINE reverse #-}
reverse :: Additive.C a => T a -> T a
reverse = lift1 Core.alternate

translate :: Ring.C a => a -> T a -> T a
translate d =
   lift1 $ foldr (\c p -> [c] + Core.mulLinearFactor d p) []

shrink :: Ring.C a => a -> T a -> T a
shrink = lift1 . Core.shrink

dilate :: Field.C a => a -> T a -> T a
dilate = lift1 . Core.dilate


instance (Arbitrary a, ZeroTestable.C a) => Arbitrary (T a) where
   arbitrary = liftM (fromCoeffs . Core.normalize) arbitrary


-- * Haskell 98 legacy instances

{- |
It is disputable whether polynomials shall be represented by number literals or not.
An advantage is, that one can write
let x = polynomial [0,1]
in  (x^2+x+1)*(x-1)
However the output looks much different.
-}
{-# INLINE notImplemented #-}
notImplemented :: String -> a
notImplemented name =
   error $ "MathObj.Polynomial: method " ++ name ++ " cannot be implemented"

-- legacy instances for use of numeric literals in GHCi
instance (P98.Num a) => P98.Num (T a) where
   fromInteger = const . P98.fromInteger
   negate = W98.unliftF1 Additive.negate
   (+)    = W98.unliftF2 (Additive.+)
   (*)    = W98.unliftF2 (Ring.*)
   abs    = notImplemented "abs"
   signum = notImplemented "signum"

instance (P98.Fractional a) => P98.Fractional (T a) where
   fromRational = const . P98.fromRational
   (/) = notImplemented "(/)"
