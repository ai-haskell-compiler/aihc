{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
Power series, either finite or unbounded.
(zipWith does exactly the right thing to make it work almost transparently.)
-}
module MathObj.PowerSeries where

import qualified MathObj.PowerSeries.Core as Core
import qualified MathObj.Polynomial.Core as Poly

import qualified Algebra.Differential   as Differential
import qualified Algebra.IntegralDomain as Integral
import qualified Algebra.VectorSpace    as VectorSpace
import qualified Algebra.Module         as Module
import qualified Algebra.Vector         as Vector
import qualified Algebra.Transcendental as Transcendental
import qualified Algebra.Algebraic      as Algebraic
import qualified Algebra.Field          as Field
import qualified Algebra.Ring           as Ring
import qualified Algebra.Additive       as Additive
import qualified Algebra.ZeroTestable   as ZeroTestable

import NumericPrelude.Base    hiding (const)
import NumericPrelude.Numeric


{- $setup
>>> import qualified MathObj.PowerSeries.Core as PS
>>> import qualified MathObj.PowerSeries as PST
>>> import qualified Test.QuickCheck as QC
>>> import Test.NumericPrelude.Utility (equalTrunc, (/\))
>>> import NumericPrelude.Numeric as NP
>>> import NumericPrelude.Base as P
>>> import Prelude ()
-}


newtype T a = Cons {coeffs :: [a]} deriving (Ord)

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

{-# INLINE const #-}
const :: a -> T a
const x = lift0 [x]

{-
Functor instance is e.g. useful for showing power series in residue rings.
@fmap (ResidueClass.concrete 7) (powerSeries [1,4,4::ResidueClass.T Integer] * powerSeries [1,5,6])@
-}

instance Functor T where
   fmap f (Cons xs) = Cons (map f xs)

{-# INLINE appPrec #-}
appPrec :: Int
appPrec  = 10

instance (Show a) => Show (T a) where
   showsPrec p (Cons xs) =
     showParen (p >= appPrec) (showString "PowerSeries.fromCoeffs " . shows xs)


{-# INLINE truncate #-}
truncate :: Int -> T a -> T a
truncate n = lift1 (take n)

{- |
Evaluate (truncated) power series.
-}
{-# INLINE evaluate #-}
evaluate :: Ring.C a => T a -> a -> a
evaluate (Cons y) = Core.evaluate y

{- |
Evaluate (truncated) power series.
-}
{-# INLINE evaluateCoeffVector #-}
evaluateCoeffVector :: Module.C a v => T v -> a -> v
evaluateCoeffVector (Cons y) = Core.evaluateCoeffVector y


{-# INLINE evaluateArgVector #-}
evaluateArgVector :: (Module.C a v, Ring.C v) => T a -> v -> v
evaluateArgVector (Cons y) = Core.evaluateArgVector y

{- |
Evaluate approximations that is evaluate all truncations of the series.
-}
{-# INLINE approximate #-}
approximate :: Ring.C a => T a -> a -> [a]
approximate (Cons y) = Core.approximate y


{- |
Evaluate approximations that is evaluate all truncations of the series.
-}
{-# INLINE approximateCoeffVector #-}
approximateCoeffVector :: Module.C a v => T v -> a -> [v]
approximateCoeffVector (Cons y) = Core.approximateCoeffVector y


{- |
Evaluate approximations that is evaluate all truncations of the series.
-}
{-# INLINE approximateArgVector #-}
approximateArgVector :: (Module.C a v, Ring.C v) => T a -> v -> [v]
approximateArgVector (Cons y) = Core.approximateArgVector y


{-
Note that the derived instances only make sense for finite series.
-}

instance (Eq a, ZeroTestable.C a) => Eq (T a) where
   (Cons x) == (Cons y) = Poly.equal x y

instance (Additive.C a) => Additive.C (T a) where
   negate = lift1 Poly.negate
   (+)    = lift2 Poly.add
   (-)    = lift2 Poly.sub
   zero   = lift0 []

{- |
prop> QC.choose (1,10) /\ \expon (QC.Positive x) xs -> let xt = x:xs in  equalTrunc 15 (PS.pow (const x) (1 % expon) (PST.coeffs (PST.fromCoeffs xt ^ expon)) ++ repeat zero) (xt ++ repeat zero)
-}
instance (Ring.C a) => Ring.C (T a) where
   one           = const one
   fromInteger n = const (fromInteger n)
   (*)           = lift2 Core.mul

instance Vector.C T where
   zero  = zero
   (<+>) = (+)
   (*>)  = Vector.functorScale

instance (Module.C a b) => Module.C a (T b) where
   (*>) x = lift1 (x *>)

instance (Field.C a, Module.C a b) => VectorSpace.C a (T b)


instance (Field.C a) => Field.C (T a) where
   (/) = lift2 Core.divide


instance (ZeroTestable.C a, Field.C a) => Integral.C (T a) where
   divMod (Cons x) (Cons y) =
      let (d,m) = Core.divMod x y
      in  (Cons d, Cons m)


instance (Ring.C a) => Differential.C (T a) where
   differentiate = lift1 Core.differentiate


instance (Algebraic.C a) => Algebraic.C (T a) where
   sqrt   = lift1 (Core.sqrt Algebraic.sqrt)
   x ^/ y = lift1 (Core.pow (Algebraic.^/ y)
                       (fromRational' y)) x


instance (Transcendental.C a) =>
             Transcendental.C (T a) where
   pi = const Transcendental.pi
   exp = lift1 (Core.exp Transcendental.exp)
   sin = lift1 (Core.sin Core.sinCosScalar)
   cos = lift1 (Core.cos Core.sinCosScalar)
   tan = lift1 (Core.tan Core.sinCosScalar)
   x ** y = Transcendental.exp (Transcendental.log x * y)
                {- This order of multiplication is especially fast
                   when y is a singleton. -}
   log  = lift1 (Core.log  Transcendental.log)
   asin = lift1 (Core.asin Algebraic.sqrt Transcendental.asin)
   acos = lift1 (Core.acos Algebraic.sqrt Transcendental.acos)
   atan = lift1 (Core.atan Transcendental.atan)

{- |
It fulfills
  @ evaluate x . evaluate y == evaluate (compose x y) @
-}

compose :: (Ring.C a, ZeroTestable.C a) => T a -> T a -> T a
compose (Cons [])    (Cons []) = Cons []
compose (Cons (x:_)) (Cons []) = Cons [x]
compose (Cons x) (Cons (y:ys)) =
   if isZero y
     then Cons (Core.compose x ys)
     else error "PowerSeries.compose: inner series must not have an absolute term."

shrink :: Ring.C a => a -> T a -> T a
shrink = lift1 . Poly.shrink

dilate :: Field.C a => a -> T a -> T a
dilate = lift1 . Poly.dilate
