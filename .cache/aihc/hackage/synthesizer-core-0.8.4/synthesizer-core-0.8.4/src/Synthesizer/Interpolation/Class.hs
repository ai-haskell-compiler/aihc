{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
See NumericPrelude.AffineSpace for design discussion.
-}
module Synthesizer.Interpolation.Class where

import qualified Synthesizer.State.Signal as Sig

import qualified Algebra.Module as Module
import qualified Algebra.PrincipalIdealDomain as PID
import qualified Algebra.Ring as Ring

import qualified Sound.Frame.NumericPrelude.Stereo as Stereo
import qualified Number.Ratio as Ratio
import qualified Number.Complex as Complex

import Control.Applicative (Applicative(pure, (<*>)), liftA2, )
import Data.Tuple.HT (mapPair, mapSnd, fst3, snd3, thd3, )

import NumericPrelude.Numeric hiding (zero, )
import NumericPrelude.Base
import Prelude ()

{- |
Given that @scale zero v == Additive.zero@
this type class is equivalent to Module in the following way:

> scaleAndAccumulate (a,x) =
>    let ax = a *> x
>    in  (ax, (ax+))

(see implementation of 'scaleAndAccumulateModule')
and

> x+y = scaleAccumulate one y $ scale one x
> zero = scale zero x
> s*>x = scale s x

But this redundancy is only because of a lack of the type system
or lack of my imagination how to solve it better.
Use this type class for all kinds of interpolation,
that is where addition and scaling alone make no sense.

I intended to name this class AffineSpace,
because all interpolations should be affine combinations.
This property is equivalent to interpolations that preserve constant functions.
However, I cannot easily assert this property
and I'm not entirely sure
that all reasonable interpolations are actually affine.

Early versions had a @zero@ method,
but this is against the idea of interpolation.
For implementing @zero@ we needed a @Maybe@ wrapper
for interpolation of @StorableVector@s.
Btw. having @zero@ instead of @scale@ is also inefficient,
since every sum must include a zero summand,
which works well only when the optimizer
simplifies addition with a constant.

We use only one class method
that contains actually two methods:
@scale@ and @scaleAccumulate@.
We expect that instances are always defined on record types
lifting interpolations from scalars to records.
This should be done using 'makeMac' and friends
or the 'MAC' type and the 'Applicative' interface
for records with many elements.
-}
class Ring.C a => C a v where
   scaleAndAccumulate :: (a,v) -> (v, v -> v)


instance C Float Float where
   {-# INLINE scaleAndAccumulate #-}
   scaleAndAccumulate = scaleAndAccumulateRing

instance C Double Double where
   {-# INLINE scaleAndAccumulate #-}
   scaleAndAccumulate = scaleAndAccumulateRing

instance (C a v) => C a (Complex.T v) where
   {-# INLINE scaleAndAccumulate #-}
   scaleAndAccumulate =
      makeMac2 (Complex.+:) Complex.real Complex.imag

instance (PID.C a) => C (Ratio.T a) (Ratio.T a) where
   {-# INLINE scaleAndAccumulate #-}
   scaleAndAccumulate = scaleAndAccumulateRing

instance (C a v, C a w) => C a (v, w) where
   {-# INLINE scaleAndAccumulate #-}
   scaleAndAccumulate = makeMac2 (,) fst snd

instance (C a v, C a w, C a u) => C a (v, w, u) where
   {-# INLINE scaleAndAccumulate #-}
   scaleAndAccumulate = makeMac3 (,,) fst3 snd3 thd3

instance C a v => C a (Stereo.T v) where
   {-# INLINE scaleAndAccumulate #-}
   scaleAndAccumulate =
      makeMac2 Stereo.cons Stereo.left Stereo.right



infixl 6 +.*

{-# INLINE scale #-}
scale :: C a v => (a,v) -> v
scale = fst . scaleAndAccumulate

{-# INLINE scaleAccumulate #-}
scaleAccumulate :: C a v => (a,v) -> v -> v
scaleAccumulate = snd . scaleAndAccumulate

{- |
Infix variant of 'scaleAccumulate'.
-}
{-# INLINE (+.*) #-}
(+.*) :: C a v => v -> (a,v) -> v
(+.*) = flip scaleAccumulate


combine2 :: C a v => a -> (v, v) -> v
combine2 a (x,y) =
   scaleAccumulate (one-a, x) $
   scale (a, y)

combineMany :: C a v => (a, Sig.T a) -> (v, Sig.T v) -> v
combineMany (a,as) (v,vs) =
   Sig.foldL (flip scaleAccumulate) (scale (a,v)) $
   Sig.zip as vs


-- * convenience functions for defining scaleAndAccumulate

{-# INLINE scaleAndAccumulateRing #-}
scaleAndAccumulateRing ::
   Ring.C a =>
   (a,a) -> (a, a -> a)
scaleAndAccumulateRing (a,x) =
   let ax = a * x
   in  (ax, (ax+))

{-# INLINE scaleAndAccumulateModule #-}
scaleAndAccumulateModule ::
   Module.C a v =>
   (a,v) -> (v, v -> v)
scaleAndAccumulateModule (a,x) =
   let ax = a *> x
   in  (ax, (ax+))


{-# INLINE scaleAndAccumulateApplicative #-}
scaleAndAccumulateApplicative ::
   (C a v, Applicative f) =>
   (a, f v) -> (f v, f v -> f v)
scaleAndAccumulateApplicative (a,x) =
   let ax = fmap (curry scaleAndAccumulate a) x
   in  (fmap fst ax, (fmap snd ax <*>))

{-# INLINE scaleAndAccumulateRingApplicative #-}
scaleAndAccumulateRingApplicative ::
   (Ring.C a, Applicative f) =>
   (a, f a) -> (f a, f a -> f a)
scaleAndAccumulateRingApplicative (a,x) =
   let ax = fmap (a*) x
   in  (ax, liftA2 (+) ax)

{-# INLINE scaleAndAccumulateModuleApplicative #-}
scaleAndAccumulateModuleApplicative ::
   (Module.C a v, Applicative f) =>
   (a, f v) -> (f v, f v -> f v)
scaleAndAccumulateModuleApplicative (a,x) =
   let ax = fmap (a*>) x
   in  (ax, liftA2 (+) ax)


{- |
A special reader monad.
-}
newtype MAC a v x = MAC {runMac :: (a,v) -> (x, v -> x)}

{-# INLINE element #-}
element ::
   (C a x) =>
   (v -> x) -> MAC a v x
element f =
   MAC $ \(a,x) ->
      mapSnd (.f) $ scaleAndAccumulate (a, f x)

instance Functor (MAC a v) where
   {-# INLINE fmap #-}
   fmap f (MAC x) =
      MAC $ mapPair (f, (f .)) . x

instance Applicative (MAC a v) where
   {-# INLINE pure #-}
   {-# INLINE (<*>) #-}
   pure x = MAC $ const (x, const x)
   MAC f <*> MAC x =
      MAC $ \av ->
         let (xav,add) = x av
             (g,fadd)  = f av
         in  (g xav, \y -> fadd y (add y))

{-# INLINE makeMac #-}
makeMac ::
   (C a x) =>
   (x -> v) ->
   (v -> x) ->
   (a,v) -> (v, v -> v)
makeMac cons x =
   runMac $ pure cons <*> element x

{-# INLINE makeMac2 #-}
makeMac2 ::
   (C a x, C a y) =>
   (x -> y -> v) ->
   (v -> x) -> (v -> y) ->
   (a,v) -> (v, v -> v)
makeMac2 cons x y =
   runMac $ pure cons <*> element x <*> element y

{-# INLINE makeMac3 #-}
makeMac3 ::
   (C a x, C a y, C a z) =>
   (x -> y -> z -> v) ->
   (v -> x) -> (v -> y) -> (v -> z) ->
   (a,v) -> (v, v -> v)
makeMac3 cons x y z =
   runMac $ pure cons <*> element x <*> element y <*> element z
