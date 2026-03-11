{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Copyright   :  (c) Henning Thielemann 2004-2006

Maintainer  :  numericprelude@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

Polynomials with negative and positive exponents.
-}
module MathObj.LaurentPolynomial where

import qualified MathObj.Polynomial  as Poly
import qualified MathObj.PowerSeries as PS
import qualified MathObj.PowerSeries.Core as PSCore

import qualified Algebra.VectorSpace  as VectorSpace
import qualified Algebra.Module       as Module
import qualified Algebra.Vector       as Vector
import qualified Algebra.Field        as Field
import qualified Algebra.Ring         as Ring
import qualified Algebra.Additive     as Additive
import qualified Algebra.ZeroTestable as ZeroTestable

import qualified Number.Complex as Complex

import qualified NumericPrelude.Numeric as NP

import NumericPrelude.Base    hiding (const, reverse, )
import NumericPrelude.Numeric hiding (div, negate, )

import qualified Data.List as List
import Data.List.HT (mapAdjacent)


{- | Polynomial including negative exponents -}

data T a = Cons {expon :: Int, coeffs :: [a]}


{- * Basic Operations -}

const :: a -> T a
const x = fromCoeffs [x]

(!) :: Additive.C a => T a -> Int -> a
(!) (Cons xt x) n =
   if n<xt
     then zero
     else head (drop (n-xt) x ++ [zero])

fromCoeffs :: [a] -> T a
fromCoeffs = fromShiftCoeffs 0

fromShiftCoeffs :: Int -> [a] -> T a
fromShiftCoeffs = Cons

fromPolynomial :: Poly.T a -> T a
fromPolynomial = fromCoeffs . Poly.coeffs

fromPowerSeries :: PS.T a -> T a
fromPowerSeries = fromCoeffs . PS.coeffs

bounds :: T a -> (Int, Int)
bounds (Cons xt x) = (xt, xt + length x - 1)

shift :: Int -> T a -> T a
shift t (Cons xt x) = Cons (xt+t) x

{-# DEPRECATED translate "In order to avoid confusion with Polynomial.translate, use 'shift' instead" #-}
translate :: Int -> T a -> T a
translate = shift


instance Functor T where
  fmap f (Cons xt xs) = Cons xt (map f xs)


{- * Show -}

appPrec :: Int
appPrec  = 10

instance (Show a) => Show (T a) where
  showsPrec p (Cons xt xs) =
    showParen (p >= appPrec)
       (showString "LaurentPolynomial.Cons " . shows xt .
        showString " " . shows xs)

{- * Additive -}

add :: Additive.C a => T a -> T a -> T a
add (Cons _ [])  y          = y
add  x          (Cons _ []) = x
add (Cons xt x) (Cons yt y) =
   if xt < yt
     then Cons xt (addShifted (yt-xt) x y)
     else Cons yt (addShifted (xt-yt) y x)

{-
Compute the value of a series of Laurent polynomials.

Requires that the start exponents constitute a (weakly) rising sequence,
where each exponent occurs only finitely often.

Cf. Synthesizer.Cut.arrange
-}
series :: (Additive.C a) => [T a] -> T a
series [] = fromCoeffs []
series ps =
   let es = map expon  ps
       xs = map coeffs ps
       ds = mapAdjacent subtract es
   in  Cons (head es) (addShiftedMany ds xs)

{- |
Add lists of numbers respecting a relative shift between the starts of the lists.
The shifts must be non-negative.
The list of relative shifts is one element shorter
than the list of summands.
Infinitely many summands are permitted,
provided that runs of zero shifts are all finite.


We could add the lists either with 'foldl' or with 'foldr',
'foldl' would be straightforward, but more time consuming (quadratic time)
whereas foldr is not so obvious but needs only linear time.

(stars denote the coefficients,
 frames denote what is contained in the interim results)
'foldl' sums this way:

> | | | *******************************
> | | +--------------------------------
> | |          ************************
> | +----------------------------------
> |                        ************
> +------------------------------------

I.e. 'foldl' would use much time find the time differences
by successive subtraction 1.

'foldr' mixes this way:

>     +--------------------------------
>     | *******************************
>     |      +-------------------------
>     |      | ************************
>     |      |           +-------------
>     |      |           | ************

-}
addShiftedMany :: (Additive.C a) => [Int] -> [[a]] -> [a]
addShiftedMany ds xss =
   foldr (uncurry addShifted) [] (zip (ds++[0]) xss)



addShifted :: Additive.C a => Int -> [a] -> [a] -> [a]
addShifted del px py =
   let recurse 0 x      = PSCore.add x py
       recurse d []     = replicate d zero ++ py
       recurse d (x:xs) = x : recurse (d-1) xs
   in  if del >= 0
         then recurse del px
         else error "LaurentPolynomial.addShifted: negative shift"


negate :: Additive.C a => T a -> T a
negate (Cons xt x) = Cons xt (map NP.negate x)

sub :: Additive.C a => T a -> T a -> T a
sub x y = add x (negate y)

instance Additive.C a => Additive.C (T a) where
   zero   = fromCoeffs []
   (+)    = add
   (-)    = sub
   negate = negate


{- * Module -}

instance Vector.C T where
   zero  = zero
   (<+>) = (+)
   (*>)  = Vector.functorScale

instance (Module.C a b) => Module.C a (T b) where
    x *> Cons yt ys = Cons yt (x *> ys)

instance (Field.C a, Module.C a b) => VectorSpace.C a (T b)


{- * Ring -}

mul :: Ring.C a => T a -> T a -> T a
mul (Cons xt x) (Cons yt y) = Cons (xt+yt) (PSCore.mul x y)

instance (Ring.C a) => Ring.C (T a) where
    one           = const one
    fromInteger n = const (fromInteger n)
    (*)           = mul


{- * Field.C -}

div :: (Field.C a, ZeroTestable.C a) => T a -> T a -> T a
div (Cons xt xs) (Cons yt ys) =
   let (xzero,x) = span isZero xs
       (yzero,y) = span isZero ys
   in  Cons (xt - yt + length xzero - length yzero)
            (PSCore.divide x y)

instance (Field.C a, ZeroTestable.C a) => Field.C (T a) where
   (/) = div

divExample :: T NP.Rational
divExample = div (Cons 1 [0,0,1,2,1]) (Cons 1 [0,0,0,1,1])




{- * Comparisons -}

{- |
Two polynomials may be stored differently.
This function checks whether two values of type @LaurentPolynomial@
actually represent the same polynomial.
-}
equivalent :: (Eq a, ZeroTestable.C a) => T a -> T a -> Bool
equivalent xs ys =
   let (Cons xt x, Cons yt y) =
          if expon xs <= expon ys
            then (xs,ys)
            else (ys,xs)
       (xPref, xSuf) = splitAt (yt-xt) x
       aux (a:as) (b:bs) = a == b && aux as bs
       aux []     bs     = all isZero bs
       aux as     []     = all isZero as
   in  all isZero xPref  &&  aux xSuf y

instance (Eq a, ZeroTestable.C a) => Eq (T a) where
   (==) = equivalent


identical :: (Eq a) => T a -> T a -> Bool
identical (Cons xt xs) (Cons yt ys) =
   xt==yt && xs == ys


{- |
Check whether a Laurent polynomial has only the absolute term,
that is, it represents the constant polynomial.
-}
isAbsolute :: (ZeroTestable.C a) => T a -> Bool
isAbsolute (Cons xt x) =
   and (zipWith (\i xi -> isZero xi || i==0) [xt..] x)



{- * Transformations of arguments -}

{- | p(z) -> p(-z) -}
alternate :: Additive.C a => T a -> T a
alternate (Cons xt x) =
   Cons xt (zipWith id (drop (mod xt 2) (cycle [id,NP.negate])) x)

{- | p(z) -> p(1\/z) -}
reverse :: T a -> T a
reverse (Cons xt x) =
   Cons (1 - xt - length x) (List.reverse x)

{- |
p(exp(i·x)) -> conjugate(p(exp(i·x)))

If you interpret @(p*)@ as a linear operator on the space of Laurent polynomials,
then @(adjoint p *)@ is the adjoint operator.
-}
adjoint :: Additive.C a => T (Complex.T a) -> T (Complex.T a)
adjoint x =
   let (Cons yt y) = reverse x
   in  (Cons yt (map Complex.conjugate y))
