{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Copyright   :  (c) Henning Thielemann 2008
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

Moog cascade lowpass with resonance.
-}
module Synthesizer.Plain.Filter.Recursive.Moog (
   Parameter(Parameter, feedback, lowpassParam),
   parameter,
   State,
   lowpass,
   lowpassModifier,
   lowpassCausal,
   ) where

import Synthesizer.Plain.Filter.Recursive (Pole(..))
import Synthesizer.Plain.Filter.NonRecursive (envelopeVector)
import qualified Synthesizer.Plain.Filter.Recursive.FirstOrder as Filt1
import qualified Synthesizer.Plain.Signal   as Sig
import qualified Synthesizer.Plain.Modifier as Modifier
import qualified Synthesizer.Causal.Process as Causal

import qualified Synthesizer.Interpolation.Class as Interpol

import qualified Control.Monad.Trans.State as MS
import qualified Control.Applicative as App
import Control.Arrow ((&&&), (>>^), (^>>), )
import Control.Applicative (pure, liftA2, (<*>), )

import qualified Data.Foldable as Fold
import qualified Data.Traversable as Trav
import Data.Function.HT (nest, )

import qualified Algebra.Module                as Module
import qualified Algebra.Transcendental        as Trans
import qualified Algebra.Ring                  as Ring

import NumericPrelude.Numeric
import NumericPrelude.Base


data Parameter a =
    Parameter
       {feedback :: !a
           {- ^ Feedback of the lowpass cascade -}
       ,lowpassParam :: !(Filt1.Parameter a)
           {- ^ Feedback of each of the lowpasses of 1st order -} }
  deriving Show


instance Functor Parameter where
   {-# INLINE fmap #-}
   fmap f p = Parameter
      (f $ feedback p) (fmap f $ lowpassParam p)

instance App.Applicative Parameter where
   {-# INLINE pure #-}
   pure x = Parameter x (Filt1.Parameter x)
   {-# INLINE (<*>) #-}
   f <*> p = Parameter
      (feedback f $ feedback p) (lowpassParam f <*> lowpassParam p)

instance Fold.Foldable Parameter where
   {-# INLINE foldMap #-}
   foldMap = Trav.foldMapDefault

instance Trav.Traversable Parameter where
   {-# INLINE sequenceA #-}
   sequenceA p =
      liftA2 Parameter
         (feedback p) (Trav.sequenceA (lowpassParam p))

instance Interpol.C a v => Interpol.C a (Parameter v) where
   {-# INLINE scaleAndAccumulate #-}
   scaleAndAccumulate = Interpol.makeMac2 Parameter feedback lowpassParam


{-
For small frequencies we get cancellations and division zero by zero.
-}
_parameterInstable :: Trans.C a => Int -> Pole a -> Parameter a
_parameterInstable order (Pole resonance frequency) =
    let beta  = frequency * 2 * pi
        alpha = (pi-beta) / fromIntegral order
        k     = sin alpha / sin (alpha+beta)

        q = ((sin (alpha+beta) - sin alpha) / sin beta) ^ fromIntegral order
        f = (resonance-1) / (resonance*q+1)
    in  Parameter f (Filt1.Parameter k)

{-
sin (a+b)
 = sin a * cos b + cos a * sin b
 = (2*ta*(1-tb^2) + 2*tb*(1-ta^2)) / ((1+ta^2)*(1+tb^2)) where ta = tan(a/2); tb = tan(b/2)

sin (a+b) - sin a
 = 2*(ta*(1-tb^2) + tb*(1-ta^2) - ta*(1+tb^2)) / ((1+ta^2)*(1+tb^2))
 = 2*(tb*(1-ta^2) - 2*ta*tb^2) / ((1+ta^2)*(1+tb^2))
 = sin b * (1-ta^2 - 2*ta*tb) / (1+ta^2)
-}
parameter :: Trans.C a => Int -> Pole a -> Parameter a
parameter order (Pole resonance frequency) =
    let beta2  = frequency * pi
        alpha2 = (pi/2-beta2) / fromIntegral order
        tanAlpha2 = tan alpha2
        tanBeta2  = tan beta2
        k =
           tanAlpha2*(1+tanBeta2^2) /
           (tanAlpha2*(1-tanBeta2^2) + tanBeta2*(1-tanAlpha2^2))

        d = (1-tanAlpha2^2 - 2*tanAlpha2*tanBeta2) / (1+tanAlpha2^2)
        q = d ^ fromIntegral order
        f = (resonance-1) / (resonance*q+1)
    in  Parameter f (Filt1.Parameter k)


type State = []

{-
Used for _lowpassState,
list of internal values may be processed by Applicative.traverse.
-}
lowpassStepStack :: (Ring.C a, Module.C a v) =>
   Parameter a -> v -> MS.State (State v) v
lowpassStepStack (Parameter f k) x =
   do y0 <- MS.gets head
      y1 <- Modifier.stackStatesR (Filt1.lowpassStep k) (x - f *> y0)
      return ((1+f) *> y1)

_lowpassStepRev :: (Ring.C a, Module.C a v) =>
   Parameter a -> v -> MS.State (State v) v
_lowpassStepRev (Parameter f k) x = MS.state $ \s ->
    let news =
           tail (scanl
              (MS.evalState . Filt1.lowpassStep k)
              -- (\u0 y1 -> let Filt1.Parameter k0 = k in (1-k0) *> u0 + k0 *> y1)
              (x - f *> last s) s)
    in  ((1+f) *> last news, news)


lowpassModifier :: (Ring.C a, Module.C a v) =>
   Int -> Modifier.Simple (State v) (Parameter a) v v
lowpassModifier order =
   Modifier.Simple (replicate order zero) lowpassStepStack


{-# INLINE lowpassCausal #-}
{-# INLINE lowpassCausalStacked #-}
{-# INLINE _lowpassCausalModifier #-}
lowpassCausal, lowpassCausalStacked, _lowpassCausalModifier ::
   (Ring.C a, Module.C a v) =>
   Int -> Causal.T (Parameter a, v) v
lowpassCausal = lowpassCausalStacked

lowpassCausalStacked order =
   Causal.map fst &&&
   Causal.feedbackControlled
      ((\(((Parameter f k),x),y0) -> (k, x - f *> y0)) ^>>
       Causal.replicateControlled order Filt1.lowpassCausal)
      (snd ^>> Causal.consInit zero)
    >>^ (\((Parameter f _k),y1) -> (1+f) *> y1)

_lowpassCausalModifier order =
   Causal.fromSimpleModifier (lowpassModifier order)


lowpass, _lowpassState, lowpassRecursive ::
   (Ring.C a, Module.C a v) =>
   Int -> Sig.T (Parameter a) -> Sig.T v -> Sig.T v

{-| Choose one of the implementations below -}
lowpass = lowpassRecursive

{-| Simulate the Moog cascade by a list of states of the partial lowpasses -}
_lowpassState order =
   Sig.modifyModulated (lowpassModifier order)

{-| The elegant way of implementing the Moog cascade by recursion -}
lowpassRecursive order c x =
   let k = map lowpassParam c
       f = map feedback c
       z = zipWith subtract (envelopeVector f (zero:y)) x
       y = nest order (Filt1.lowpass k) z
   in  zipWith (*>) (map (1+) f) y
