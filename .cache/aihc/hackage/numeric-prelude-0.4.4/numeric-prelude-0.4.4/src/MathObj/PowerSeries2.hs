{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
Two-variate power series.
-}

module MathObj.PowerSeries2 where

import qualified MathObj.PowerSeries2.Core as Core
import qualified MathObj.PowerSeries as PS
import qualified MathObj.Polynomial.Core as Poly

import qualified Algebra.Vector         as Vector
import qualified Algebra.Algebraic      as Algebraic
import qualified Algebra.Field          as Field
import qualified Algebra.Ring           as Ring
import qualified Algebra.Additive       as Additive
import qualified Algebra.ZeroTestable   as ZeroTestable

import Data.List (isPrefixOf, )
import qualified Data.List.Match as Match

import NumericPrelude.Base    hiding (const)
import NumericPrelude.Numeric

{- |
In order to handle both variables equivalently
we maintain a list of coefficients for terms of the same total degree.
That is

> eval [[a], [b,c], [d,e,f]] (x,y) ==
>    a + b*x+c*y + d*x^2+e*x*y+f*y^2

Although the sub-lists are always finite and thus are more like polynomials than power series,
division and square root computation are easier to implement for power series.
-}
newtype T a = Cons {coeffs :: Core.T a} deriving (Ord)


isValid :: [[a]] -> Bool
isValid = flip isPrefixOf [1..] . map length

check :: [[a]] -> [[a]]
check xs =
   zipWith (\n x ->
      if Match.compareLength n x == EQ
        then x
        else error "PowerSeries2.check: invalid length of sub-list")
     (iterate (():) [()]) xs


fromCoeffs :: [[a]] -> T a
fromCoeffs  =  Cons . check

fromPowerSeries0 :: Ring.C a => PS.T a -> T a
fromPowerSeries0 x =
   fromCoeffs $
   zipWith (:) (PS.coeffs x) $
   iterate (0:) []

fromPowerSeries1 :: Ring.C a => PS.T a -> T a
fromPowerSeries1 x =
   fromCoeffs $
   zipWith (++) (iterate (0:) []) $
   map (:[]) (PS.coeffs x)


lift0 :: Core.T a -> T a
lift0 = Cons

lift1 :: (Core.T a -> Core.T a) -> (T a -> T a)
lift1 f (Cons x0) = Cons (f x0)

lift2 :: (Core.T a -> Core.T a -> Core.T a) -> (T a -> T a -> T a)
lift2 f (Cons x0) (Cons x1) = Cons (f x0 x1)


const :: a -> T a
const x = lift0 [[x]]


{-# INLINE truncate #-}
truncate :: Int -> T a -> T a
truncate n = lift1 (take n)


instance Functor T where
   fmap f (Cons xs) = Cons (map (map f) xs)

appPrec :: Int
appPrec  = 10

instance (Show a) => Show (T a) where
   showsPrec p (Cons xs) =
      showParen (p >= appPrec) (showString "PowerSeries2.fromCoeffs " . shows xs)


instance (Eq a, ZeroTestable.C a) => Eq (T a) where
   (Cons x) == (Cons y) = Poly.equal x y

instance (Additive.C a) => Additive.C (T a) where
   negate = lift1 Core.negate
   (+)    = lift2 Core.add
   (-)    = lift2 Core.sub
   zero   = lift0 []


instance (Ring.C a) => Ring.C (T a) where
   one           = const one
   fromInteger n = const (fromInteger n)
   (*)           = lift2 Core.mul

instance Vector.C T where
   zero  = zero
   (<+>) = (+)
   (*>)  = Vector.functorScale


instance (Field.C a) => Field.C (T a) where
   (/) = lift2 Core.divide


instance (Algebraic.C a) => Algebraic.C (T a) where
   sqrt   = lift1 (Core.sqrt Algebraic.sqrt)
   x ^/ y = lift1 (Core.pow (Algebraic.^/ y) (fromRational' y)) x
