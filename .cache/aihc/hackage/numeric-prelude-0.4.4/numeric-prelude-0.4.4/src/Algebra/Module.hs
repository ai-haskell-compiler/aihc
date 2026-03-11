{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Copyright   :  (c) Dylan Thurston, Henning Thielemann 2004-2005

Maintainer  :  numericprelude@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

Abstraction of modules
-}

module Algebra.Module where

import qualified Number.Ratio as Ratio

import qualified Algebra.PrincipalIdealDomain as PID
import qualified Algebra.Ring      as Ring
import qualified Algebra.Additive  as Additive
import qualified Algebra.ToInteger as ToInteger

import qualified Algebra.Laws as Laws

import Algebra.Ring     ((*), fromInteger, )
import Algebra.Additive ((+), zero, sum, )

import qualified NumericPrelude.Elementwise as Elem
import Control.Applicative (Applicative(pure, (<*>)), )

import qualified Data.Complex as Complex98
import Data.Int (Int, Int8, Int16, Int32, Int64, )

import Data.Function.HT (powerAssociative, )
import Data.List (map, zipWith, )
import Data.Tuple.HT (fst3, snd3, thd3, )
import Data.Tuple (fst, snd, )

import qualified Prelude as P
import Prelude((.), Eq, Bool, Integer, Float, Double, ($), )


-- Is this right?
infixr 7 *>

{-
Functional dependency can't be used
since @Complex.T a@ is a module
with respect to both @a@ and @Complex.T a@.

class Algebra.Module.C a v | v -> a where
-}

{-|
A Module over a ring satisfies:

>   a *> (b + c) === a *> b + a *> c
>   (a * b) *> c === a *> (b *> c)
>   (a + b) *> c === a *> c + b *> c
-}
class (Ring.C a, Additive.C v) => C a v where
    -- | scale a vector by a scalar
    (*>) :: a -> v -> v


{-# INLINE (<*>.*>) #-}
(<*>.*>) ::
   (C a x) =>
   Elem.T (a,v) (x -> c) -> (v -> x) -> Elem.T (a,v) c
(<*>.*>) f acc =
   f <*> Elem.element (\(a,v) -> a *> acc v)



{-* Instances for atomic types -}

instance C Float Float where
   {-# INLINE (*>) #-}
   (*>) = (*)

instance C Double Double where
   {-# INLINE (*>) #-}
   (*>) = (*)

instance C Int Int where
   {-# INLINE (*>) #-}
   (*>) = (*)

instance C Int8 Int8 where
   {-# INLINE (*>) #-}
   (*>) = (*)

instance C Int16 Int16 where
   {-# INLINE (*>) #-}
   (*>) = (*)

instance C Int32 Int32 where
   {-# INLINE (*>) #-}
   (*>) = (*)

instance C Int64 Int64 where
   {-# INLINE (*>) #-}
   (*>) = (*)

instance C Integer Integer where
   {-# INLINE (*>) #-}
   (*>) = (*)

instance (PID.C a) => C (Ratio.T a) (Ratio.T a) where
   {-# INLINE (*>) #-}
   (*>) = (*)

instance (PID.C a) => C Integer (Ratio.T a) where
   {-# INLINE (*>) #-}
   x *> y = fromInteger x * y



{-* Instances for composed types -}

instance (C a b0, C a b1) => C a (b0, b1) where
   {-# INLINE (*>) #-}
   (*>) = Elem.run2 $ pure (,) <*>.*> fst <*>.*> snd
   -- s *> (x0,x1)   = (s *> x0, s *> x1)

instance (C a b0, C a b1, C a b2) => C a (b0, b1, b2) where
   {-# INLINE (*>) #-}
   (*>) = Elem.run2 $ pure (,,) <*>.*> fst3 <*>.*> snd3 <*>.*> thd3
   -- s *> (x0,x1,x2) = (s *> x0, s *> x1, s *> x2)

instance (C a v) => C a [v] where
   {-# INLINE (*>) #-}
   (*>) = map . (*>)

instance (C a v) => C a (c -> v) where
   {-# INLINE (*>) #-}
   (*>) s f = (*>) s . f


instance (C a b, P.RealFloat b) => C a (Complex98.Complex b) where
   {-# INLINE (*>) #-}
   s *> (x Complex98.:+ y)  =  (s *> x) Complex98.:+ (s *> y)


{-* Related functions -}

{-|
Compute the linear combination of a list of vectors.

ToDo:
Should it use 'NumericPrelude.List.Checked.zipWith' ?
-}
linearComb :: C a v => [a] -> [v] -> v
linearComb c = sum . zipWith (*>) c

{-|
This function can be used to define any
'Additive.C' as a module over 'Integer'.

Better move to "Algebra.Additive"?
-}
{-# INLINE integerMultiply #-}
integerMultiply :: (ToInteger.C a, Additive.C v) => a -> v -> v
integerMultiply a v =
   powerAssociative (+) zero v (ToInteger.toInteger a)


{- * Properties -}

propCascade :: (Eq v, C a v) => v -> a -> a -> Bool
propCascade  =  Laws.leftCascade (*) (*>)

propRightDistributive :: (Eq v, C a v) => a -> v -> v -> Bool
propRightDistributive  =  Laws.rightDistributive (*>) (+)

propLeftDistributive :: (Eq v, C a v) => v -> a -> a -> Bool
propLeftDistributive x  =  Laws.homomorphism (*>x) (+) (+)
