{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
See "Algebra.DimensionTerm".
-}

module Number.DimensionTerm where

import qualified Algebra.DimensionTerm as Dim

import qualified Algebra.OccasionallyScalar as OccScalar
import qualified Algebra.Module        as Module
import qualified Algebra.Algebraic     as Algebraic
import qualified Algebra.Field         as Field
import qualified Algebra.Absolute      as Absolute
import qualified Algebra.Ring          as Ring
import qualified Algebra.Additive      as Additive

import Algebra.Field    ((/), fromRational', )
import Algebra.Ring     ((*), one, fromInteger, )
import Algebra.Additive ((+), (-), zero, negate, )
import Algebra.Module   ((*>), )

import System.Random (Random, randomR, random)

import Control.DeepSeq (NFData(rnf), )

import Data.Tuple.HT (mapFst, )
import NumericPrelude.Base
import Prelude ()


{- * Number type -}

newtype T u a = Cons a
   deriving (Eq, Ord)


instance (Dim.C u, Show a) => Show (T u a) where
   showsPrec p x =
      let disect :: T u a -> (u,a)
          disect (Cons y) = (undefined, y)
          (u,z) = disect x
      in  showParen (p >= Dim.appPrec)
            (showString "DimensionNumber.fromNumberWithDimension " . showsPrec Dim.appPrec u .
             showString " " . showsPrec Dim.appPrec z)

instance NFData a => NFData (T u a) where
   rnf (Cons x) = rnf x


fromNumber :: a -> Scalar a
fromNumber = Cons

toNumber :: Scalar a -> a
toNumber (Cons x) = x

fromNumberWithDimension :: Dim.C u => u -> a -> T u a
fromNumberWithDimension _ = Cons

toNumberWithDimension :: Dim.C u => u -> T u a -> a
toNumberWithDimension _ (Cons x) = x


instance (Dim.C u, Additive.C a) => Additive.C (T u a) where
   zero                = Cons zero
   (Cons a) + (Cons b) = Cons (a+b)
   (Cons a) - (Cons b) = Cons (a-b)
   negate (Cons a)     = Cons (negate a)

instance (Dim.C u, Module.C a b) => Module.C a (T u b) where
   a *> (Cons b) = Cons (a *> b)

instance (Dim.IsScalar u, Ring.C a) => Ring.C (T u a) where
   one                 = Cons one
   (Cons a) * (Cons b) = Cons (a*b)
   fromInteger a       = Cons (fromInteger a)

instance (Dim.IsScalar u, Field.C a) => Field.C (T u a) where
   (Cons a) / (Cons b) = Cons (a/b)
   recip (Cons a)      = Cons (Field.recip a)
   fromRational' a     = Cons (fromRational' a)

instance (Dim.IsScalar u, OccScalar.C a b) => OccScalar.C a (T u b) where
   toScalar =
      OccScalar.toScalar . toNumber . rewriteDimension Dim.toScalar
   toMaybeScalar =
      OccScalar.toMaybeScalar . toNumber . rewriteDimension Dim.toScalar
   fromScalar =
      rewriteDimension Dim.fromScalar . fromNumber . OccScalar.fromScalar

instance (Dim.C u, Random a) => Random (T u a) where
  randomR (Cons l, Cons u) = mapFst Cons . randomR (l,u)
  random = mapFst Cons . random


infixl 7 &*&, *&
infixl 7 &/&

(&*&) :: (Dim.C u, Dim.C v, Ring.C a) =>
   T u a -> T v a -> T (Dim.Mul u v) a
(&*&) (Cons x) (Cons y) = Cons (x Ring.* y)

(&/&) :: (Dim.C u, Dim.C v, Field.C a) =>
   T u a -> T v a -> T (Dim.Mul u (Dim.Recip v)) a
(&/&) (Cons x) (Cons y) = Cons (x Field./ y)

mulToScalar :: (Dim.C u, Ring.C a) =>
   T u a -> T (Dim.Recip u) a -> a
mulToScalar x y = cancelToScalar (x &*& y)

divToScalar :: (Dim.C u, Field.C a) =>
   T u a -> T u a -> a
divToScalar x y = cancelToScalar (x &/& y)

cancelToScalar :: (Dim.C u) =>
   T (Dim.Mul u (Dim.Recip u)) a -> a
cancelToScalar =
   toNumber . rewriteDimension Dim.cancelRight


recip :: (Dim.C u, Field.C a) =>
   T u a -> T (Dim.Recip u) a
recip (Cons x) = Cons (Field.recip x)

unrecip :: (Dim.C u, Field.C a) =>
   T (Dim.Recip u) a -> T u a
unrecip (Cons x) = Cons (Field.recip x)

sqr :: (Dim.C u, Ring.C a) =>
   T u a -> T (Dim.Sqr u) a
sqr x = x &*& x

sqrt :: (Dim.C u, Algebraic.C a) =>
   T (Dim.Sqr u) a -> T u a
sqrt (Cons x) = Cons (Algebraic.sqrt x)


abs :: (Dim.C u, Absolute.C a) => T u a -> T u a
abs (Cons x) = Cons (Absolute.abs x)

absSignum :: (Dim.C u, Absolute.C a) => T u a -> (T u a, a)
absSignum x0@(Cons x) = (abs x0, Absolute.signum x)

scale, (*&) :: (Dim.C u, Ring.C a) =>
   a -> T u a -> T u a
scale x (Cons y) = Cons (x Ring.* y)

(*&) = scale


rewriteDimension :: (Dim.C u, Dim.C v) => (u -> v) -> T u a -> T v a
rewriteDimension _ (Cons x) = Cons x


{-
type class for converting Dim types to Dim value is straight-forward
   class SIDimensionType u where
      dynamic :: DimensionNumber u a -> SIValue a

   instance SIDimensionType Scalar where
      dynamic (DimensionNumber.Cons x) = SIValue.scalar x

   instance SIDimensionType Length where
      dynamic (DimensionNumber.Cons x) = SIValue.meter * dynamic x
-}


{- * Example constructors -}

type Scalar      a = T Dim.Scalar a
type Length      a = T Dim.Length a
type Time        a = T Dim.Time a
type Mass        a = T Dim.Mass a
type Charge      a = T Dim.Charge a
type Angle       a = T Dim.Angle a
type Temperature a = T Dim.Temperature a
type Information a = T Dim.Information a

type Frequency   a = T Dim.Frequency a
type Voltage     a = T Dim.Voltage a


scalar :: a -> Scalar a
scalar = fromNumber

length :: a -> Length a
length = Cons

time :: a -> Time a
time = Cons

mass :: a -> Mass a
mass = Cons

charge :: a -> Charge a
charge = Cons

frequency :: a -> Frequency a
frequency = Cons

angle :: a -> Angle a
angle = Cons

temperature :: a -> Temperature a
temperature = Cons

information :: a -> Information a
information = Cons


voltage :: a -> Voltage a
voltage = Cons
