{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
module Numeric.BLAS.Scalar (
   RealOf,
   ComplexOf,
   zero,
   one,
   minusOne,
   isZero,

   selectReal,
   selectFloating,
   ComplexSingleton(Real,Complex),
   complexSingleton,
   complexSingletonOf,
   complexSingletonOfFunctor,

   PrecisionSingleton(Float,Double),
   precisionSingleton,
   precisionOf,
   precisionOfFunctor,

   equal,
   fromReal,
   toComplex,
   absolute,
   absoluteSquared,
   norm1,
   realPart,
   conjugate,
   ) where

import qualified Numeric.Netlib.Class as Class

import Data.Functor.Identity (Identity(Identity, runIdentity))

import qualified Data.Complex as Complex
import Data.Complex (Complex((:+)))


type family RealOf x

type instance RealOf Float = Float
type instance RealOf Double = Double
type instance RealOf (Complex.Complex a) = a


type ComplexOf x = Complex.Complex (RealOf x)


data ComplexSingleton a where
   Real :: (Class.Real a, RealOf a ~ a) => ComplexSingleton a
   Complex :: (Class.Real a) => ComplexSingleton (Complex.Complex a)

complexSingleton :: (Class.Floating a) => ComplexSingleton a
complexSingleton = Class.switchFloating Real Real Complex Complex

complexSingletonOf :: (Class.Floating a) => a -> ComplexSingleton a
complexSingletonOf = const complexSingleton

complexSingletonOfFunctor :: (Class.Floating a) => f a -> ComplexSingleton a
complexSingletonOfFunctor = const complexSingleton

withComplexSingleton :: (Class.Floating a) => (ComplexSingleton a -> a) -> a
withComplexSingleton f = f complexSingleton


data PrecisionSingleton a where
   Float :: PrecisionSingleton Float
   Double :: PrecisionSingleton Double

precisionSingleton :: (Class.Real a) => PrecisionSingleton a
precisionSingleton = Class.switchReal Float Double

precisionOf :: (Class.Real a) => a -> PrecisionSingleton a
precisionOf _ = precisionSingleton

precisionOfFunctor :: (Class.Real a) => f a -> PrecisionSingleton a
precisionOfFunctor _ = precisionSingleton


-- ToDo: move to netlib-carray:Utility or netlib-ffi:Class
zero, one, minusOne :: Class.Floating a => a
zero = selectFloating 0 0 0 0
one = selectFloating 1 1 1 1
minusOne = selectFloating (-1) (-1) (-1) (-1)

selectReal :: (Class.Real a) => Float -> Double -> a
selectReal rf rd = runIdentity $ Class.switchReal (Identity rf) (Identity rd)

selectFloating ::
   (Class.Floating a) =>
   Float -> Double -> Complex.Complex Float -> Complex.Complex Double -> a
selectFloating rf rd cf cd =
   withComplexSingleton $ \sw ->
      case sw of
         Real -> selectReal rf rd
         Complex -> Class.switchReal cf cd



equal :: (Class.Floating a) => a -> a -> Bool
equal a b =
   case complexSingletonOf a of
      Real -> a==b
      Complex -> a==b


isZero :: (Class.Floating a) => a -> Bool
isZero = equal zero


fromReal :: (Class.Floating a) => RealOf a -> a
fromReal a =
   withComplexSingleton $ \sw ->
      case sw of
         Real -> a
         Complex -> a:+0

toComplex :: (Class.Floating a) => a -> ComplexOf a
toComplex a =
   case complexSingletonOf a of
      Real -> a:+0
      Complex -> a

realPart :: (Class.Floating a) => a -> RealOf a
realPart a =
   case complexSingletonOf a of
      Real -> a
      Complex -> Complex.realPart a

absolute :: (Class.Floating a) => a -> RealOf a
absolute a =
   case complexSingletonOf a of
      Real -> abs a
      Complex -> Complex.magnitude a


norm1 :: (Class.Floating a) => a -> RealOf a
norm1 a =
   case complexSingletonOf a of
      Real -> abs a
      Complex -> case a of r:+i -> abs r + abs i


absoluteSquared :: (Class.Floating a) => a -> RealOf a
absoluteSquared a =
   case complexSingletonOf a of
      Real -> a*a
      Complex -> case a of r:+i -> r*r+i*i


conjugate :: (Class.Floating a) => a -> a
conjugate a =
   case complexSingletonOf a of
      Real -> a
      Complex -> Complex.conjugate a
