{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Copyright   :  (c) Henning Thielemann 2004-2005

Maintainer  :  numericprelude@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes


For a multi-set of numbers,
we describe a sequence of the sums of powers of the numbers in the set.
These can be easily converted to polynomials and back.
Thus they provide an easy way for computations on the roots of a polynomial.
-}
module MathObj.PowerSum where

import qualified MathObj.Polynomial as Poly
import qualified MathObj.Polynomial.Core as PolyCore
import qualified MathObj.PowerSeries.Core as PS

import qualified Algebra.VectorSpace  as VectorSpace
import qualified Algebra.Module       as Module
import qualified Algebra.Algebraic    as Algebraic
import qualified Algebra.Field        as Field
import qualified Algebra.IntegralDomain as Integral
import qualified Algebra.Ring         as Ring
import qualified Algebra.Additive     as Additive
import qualified Algebra.ZeroTestable as ZeroTestable

import Control.Monad(liftM2)
import qualified Data.List as List
import Data.List.HT (shearTranspose, sieve)

import NumericPrelude.Base as P hiding (const)
import NumericPrelude.Numeric as NP


newtype T a = Cons {sums :: [a]}


{- * Conversions -}

lift0 :: [a] -> T a
lift0 = Cons

lift1 :: ([a] -> [a]) -> (T a -> T a)
lift1 f (Cons x0) = Cons (f x0)

lift2 :: ([a] -> [a] -> [a]) -> (T a -> T a -> T a)
lift2 f (Cons x0) (Cons x1) = Cons (f x0 x1)


const :: (Ring.C a) => a -> T a
const x = Cons [1,x]

{- Newton-Girard formulas,  cf. Modula-3: arithmetic/RootBasic.mg
   s'/s = p -}

{-
  s[k] - the elementary symmetric polynomial of degree k
  p[k] - sum of the k-th power

  s[0](x0,x1,x2) = 1
  s[1](x0,x1,x2) = x0+x1+x2
  s[2](x0,x1,x2) = x0*x1+x1*x2+x2*x0
  s[3](x0,x1,x2) = x0*x1*x2
  s[4](x0,x1,x2) = 0

  p[0](x0,x1,x2) =  1   +  1   +  1
  p[1](x0,x1,x2) = x0   + x1   + x2
  p[2](x0,x1,x2) = x0^2 + x1^2 + x2^2
  p[3](x0,x1,x2) = x0^3 + x1^3 + x2^3
  p[4](x0,x1,x2) = x0^4 + x1^4 + x2^4

  s(t) := s[0] + s[1]*t + s[2]*t^2 + ...
  p(t) :=        p[1]*t + p[2]*t^2 + ...

  Then it holds
    t*s'(t) + p(-t)*s(t) = 0
  This can be proven by considering p as sum of geometric series
  and differentiating s in the root-wise factored form.

  Note that we index the coefficients the other way round
  and that the coefficients of the polynomial
  are not pure elementary symmetric polynomials of the roots
  but have alternating signs, too.
-}
fromElemSym :: (Eq a, Ring.C a) => [a] -> [a]
fromElemSym s =
   fromIntegral (length s - 1) :
      PolyCore.alternate (divOneFlip s (PolyCore.differentiate s))

divOneFlip :: (Eq a, Ring.C a) => [a] -> [a] -> [a]
divOneFlip (1:xs) =
   let aux (y:ys) = y : aux (ys - PolyCore.scale y xs)
       aux [] = []
   in  aux
divOneFlip _ =
   error "divOneFlip: first element must be one"

fromElemSymDenormalized :: (Field.C a, ZeroTestable.C a) => [a] -> [a]
fromElemSymDenormalized s =
   fromIntegral (length s - 1) :
      PolyCore.alternate (PS.derivedLog s)


toElemSym :: (Field.C a, ZeroTestable.C a) => [a] -> [a]
toElemSym p =
   let s' = PolyCore.mul (PolyCore.alternate (tail p)) s
       s  = PolyCore.integrate 1 s'
   in  s

toElemSymInt :: (Integral.C a, ZeroTestable.C a) => [a] -> [a]
toElemSymInt p =
   let s' = PolyCore.mul (PolyCore.alternate (tail p)) s
       s  = PolyCore.integrateInt 1 s'
   in  s



fromPolynomial :: (Field.C a, ZeroTestable.C a) => Poly.T a -> [a]
fromPolynomial =
   let aux s =
          fromIntegral (length s - 1) :
             PolyCore.negate (PS.derivedLog s)
   in  aux . reverse . Poly.coeffs

elemSymFromPolynomial :: Additive.C a => Poly.T a -> [a]
elemSymFromPolynomial = PolyCore.alternate . reverse . Poly.coeffs

{- toPolynomial is not possible because this had to consume the whole sum sequence. -}



binomials :: Ring.C a => [[a]]
binomials = [1] : binomials + map (0:) binomials

{- * Show -}

appPrec :: Int
appPrec  = 10

instance (Show a) => Show (T a) where
  showsPrec p (Cons xs) =
    showParen (p >= appPrec)
       (showString "PowerSum.Cons " . shows xs)


{- * Additive -}

{- Use binomial expansion of (x+y)^n -}
add :: (Ring.C a) => [a] -> [a] -> [a]
add xs ys =
   let powers = shearTranspose (PolyCore.tensorProduct xs ys)
   in  zipWith Ring.scalarProduct binomials powers

instance (Ring.C a) => Additive.C (T a) where
   zero   = const zero
   (+)    = lift2 add
   negate = lift1 PolyCore.alternate


{- * Ring -}

mul :: (Ring.C a) => [a] -> [a] -> [a]
mul xs ys = zipWith (*) xs ys

pow :: Integer -> [a] -> [a]
pow n =
   if n<0
     then error "PowerSum.pow: negative exponent"
     else sieve (fromInteger n)
       -- map head . iterate (List.genericDrop (toInteger n))

instance (Ring.C a) => Ring.C (T a) where
   one           = const one
   fromInteger n = const (fromInteger n)
   (*)           = lift2 mul
   x^n           = lift1 (pow n) x


{- * Module -}

instance (Module.C a v, Ring.C v) => Module.C a (T v) where
   x *> y = lift1 (zipWith (*>) (iterate (x*) one)) y

instance (VectorSpace.C a v, Ring.C v) => VectorSpace.C a (T v)


{- * Field.C -}

instance (Field.C a, ZeroTestable.C a) => Field.C (T a) where
   recip = lift1 (fromElemSymDenormalized . reverse . toElemSym)


{- * Algebra -}

root :: (Ring.C a) => Integer -> [a] -> [a]
root n xs =
   let upsample m ys =
          concat (List.intersperse
             (List.genericReplicate (m - 1) zero)
             (map (:[]) ys))
   in  case compare n 0 of
          LT -> upsample (-n) (reverse xs)
          GT -> upsample n xs
          EQ -> [1]

instance (Field.C a, ZeroTestable.C a) => Algebraic.C (T a) where
   root n = lift1 (fromElemSymDenormalized . root n . toElemSym)


{- given the list of power sums @x1^j + ... + xn^j@
   and a power series for the function @f@,
   compute the series approximations of @f(x1) + ... + f(xn)@. -}
approxSeries :: Module.C a b => [b] -> [a] -> [b]
approxSeries y x =
   scanl (+) zero (zipWith (*>) x y)


{- input lists contain roots -}
propOp :: (Eq a, Field.C a, ZeroTestable.C a) =>
   ([a] -> [a] -> [a]) -> (a -> a -> a) -> [a] -> [a] -> [Bool]
propOp powerOp op xs ys =
   let zs = liftM2 op xs ys
       xp = fromPolynomial (Poly.fromRoots xs)
       yp = fromPolynomial (Poly.fromRoots ys)
       ze = elemSymFromPolynomial (Poly.fromRoots zs)
   in  zipWith (==) (toElemSym (powerOp xp yp)) ze
       -- PolyCore.equal (toElemSym (powerOp xp yp)) ze
