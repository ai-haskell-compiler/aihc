{-# LANGUAGE Rank2Types #-}
{- |
Copyright   :  (c) Henning Thielemann 2008-2009
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
               and local universal quantification


Light-weight sample parameter inference which will fit most needs.
We only do \"poor man's inference\", only for sample rates.
The sample rate will be provided as an argument of a special type 'T'.
This argument will almost never be passed explicitly
but should be handled by operators analogous to '($)' and '(.)'.

In contrast to the run-time inference approach,
we have the static guarantee that the sample rate is fixed
before passing a signal to the outside world.
However we still need to make it safe that signals
that are rendered for one sample rate
are not processed with another sample rate.
-}
module Synthesizer.Dimensional.Process (
      T(..),
      run, {-share,-} withParam, getSampleRate,
      toTimeScalar,    toFrequencyScalar,
      toTimeDimension, toFrequencyDimension,
      intFromTime, intFromTime98,
      DimensionGradient, toGradientScalar,
      loop, pure,
      ($:), ($::), ($^), ($#),
      (.:), (.^),
      liftP, liftP2, liftP3, liftP4,
   ) where

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

import Number.DimensionTerm ((*&), (&/&), ) -- ((&*&), )

import qualified Algebra.RealRing      as RealRing
import qualified Algebra.Field          as Field
import qualified Algebra.Ring           as Ring

import Control.Monad.Fix (MonadFix(mfix), )
-- import Control.Monad.Trans.Reader ()
import Synthesizer.ApplicativeUtility
import qualified Control.Applicative as App
import Control.Applicative (Applicative)

import NumericPrelude.Numeric
import NumericPrelude.Base

import qualified Prelude as P
import Prelude (RealFrac)


{- |
This wraps a function which computes a sample rate dependent result.
Sample rate tells how many values per unit are stored
for representation of a signal.

The process is labeled with a type variable @s@ which is part the signals.
This way we can ensure that signals are only used
with the sample rate they are created for.
-}
newtype T s u t a = Cons {process :: DN.T (Dim.Recip u) t -> a}

instance Functor (T s u t) where
   fmap f (Cons g) = Cons (f . g)

instance Applicative (T s u t) where
   pure  = pure
   (<*>) = apply

instance Monad (T s u t) where
   return = pure
   (>>=)  = bind

instance MonadFix (T s u t) where
   mfix = loop . withParam


{-# INLINE pure #-}
pure :: a -> T s u t a
pure = Cons . const

{-# INLINE apply #-}
apply :: T s u t (a -> b) -> T s u t a -> T s u t b
apply (Cons f) arg = Cons $ \sr -> f sr (process arg sr)


{- |
Get results from the Process monad.
You can obtain only signals (or other values)
that do not implicitly depend on the sample rate,
that is value without the @s@ type parameter.
-}
{-# INLINE run #-}
run :: (Dim.C u) => DN.T (Dim.Recip u) t -> (forall s. T s u t a) -> a
run sampleRate f = process f sampleRate

{-
{- |
You can write
@x >>= (\x0 -> Cut.zip $# x0 $# x0)@
or
@share x (\x0 -> Cut.zip $: x0 $: x0)@.
'share' allows for more consistent usage of @($:)@.
-}
share :: T s u t a -> (T s u t a -> T s u t b) -> T s u t b
share x y  =  y . return =<< x
-}

{-# INLINE bind #-}
bind :: T s u t a -> (a -> T s u t b) -> T s u t b
bind (Cons f) mg =
   Cons $ \ sr -> process (mg (f sr)) sr

-- same as Inference.Reader.Process.injectParam
{-# INLINE withParam #-}
withParam :: (a -> T s u t b) -> T s u t (a -> b)
withParam f = Cons (\sr a -> process (f a) sr)


{-# INLINE getSampleRate #-}
getSampleRate :: Dim.C u => T s u t (DN.T (Dim.Recip u) t)
getSampleRate = Cons id


{-# INLINE toTimeScalar #-}
toTimeScalar {- , (~*&) -} :: (Ring.C t, Dim.C u) =>
   DN.T u t -> T s u t t
toTimeScalar time =
   fmap (DN.mulToScalar time) getSampleRate

{-# INLINE toFrequencyScalar #-}
toFrequencyScalar {- , (~/&) -} :: (Field.C t, Dim.C u) =>
   DN.T (Dim.Recip u) t -> T s u t t
toFrequencyScalar freq =
   fmap (DN.divToScalar freq) getSampleRate


{-# INLINE toTimeDimension #-}
toTimeDimension :: (Field.C t, Dim.C u) =>
   t -> T s u t (DN.T u t)
toTimeDimension t =
   fmap (\sampleRate -> t *& DN.unrecip sampleRate) getSampleRate

{-# INLINE toFrequencyDimension #-}
toFrequencyDimension :: (Ring.C t, Dim.C u) =>
   t -> T s u t (DN.T (Dim.Recip u) t)
toFrequencyDimension f =
   fmap (\sampleRate -> f *& sampleRate) getSampleRate


type DimensionGradient u v = Dim.Mul (Dim.Recip u) v

{-# INLINE toGradientScalar #-}
toGradientScalar :: (Field.C q, Dim.C u, Dim.C v) =>
   DN.T v q -> DN.T (DimensionGradient u v) q -> T s u q q
toGradientScalar amp steepness =
   toFrequencyScalar
   (DN.rewriteDimension (Dim.identityRight . Dim.applyRightMul Dim.cancelRight . Dim.associateRight) $
    steepness &/& amp)

{-
infixl 7 ~*&, ~/&

(~*&) = toTimeScalar
(~/&) = toFrequencyScalar
-}


checkedChunkSize ::
   String -> Int -> Int
checkedChunkSize funcName cs =
   if cs>0
     then cs
     else error $ funcName ++ ": negative chunkSize"

intFromTime ::
   (RealRing.C t, Dim.C u) =>
   String ->
   DN.T u t ->
   T s u t Int
intFromTime funcName t =
   fmap (checkedChunkSize funcName . RealRing.ceiling) $ toTimeScalar t

intFromTime98 ::
   (Ring.C t, RealFrac t, Dim.C u) =>
   String ->
   DN.T u t ->
   T s u t Int
intFromTime98 funcName t =
   fmap (checkedChunkSize funcName . P.ceiling) $ toTimeScalar t
