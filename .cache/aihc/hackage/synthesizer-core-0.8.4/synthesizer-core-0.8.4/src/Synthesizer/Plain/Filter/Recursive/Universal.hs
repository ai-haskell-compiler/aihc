{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Copyright   :  (c) Henning Thielemann 2008-2012
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

State variable filter.
One filter that generates lowpass, bandpass, highpass, bandlimit at once.
-}
module Synthesizer.Plain.Filter.Recursive.Universal (
   Parameter(..),
   Result(..),
   State,
   causal,
   modifier,
   modifierInit,
   parameter,
   parameterToSecondOrderLowpass,
   run,
   runInit,
   step,

   -- for testing
   parameterAlt,
   parameterOld,
   ) where

import Synthesizer.Plain.Filter.Recursive (Pole(..))
import qualified Synthesizer.Plain.Signal   as Sig
import qualified Synthesizer.Plain.Modifier as Modifier
import qualified Synthesizer.Causal.Process as Causal

import qualified Synthesizer.Plain.Filter.Recursive.SecondOrder as SecondOrder

import qualified Synthesizer.Interpolation.Class as Interpol

import qualified Control.Monad.Trans.State as MS
import qualified Control.Applicative.HT as App
import Control.Applicative (Applicative, pure, (<*>))

import qualified Data.Foldable as Fold
import qualified Data.Traversable as Trav

import Foreign.Storable (Storable(..))
import qualified Foreign.Storable.Record as Store

import qualified Algebra.Module                as Module
import qualified Algebra.Transcendental        as Trans
import qualified Algebra.Ring                  as Ring
import qualified Algebra.Additive              as Additive

import NumericPrelude.Numeric
import NumericPrelude.Base


data Parameter a =
        Parameter {k1, k2, ampIn, ampI1, ampI2, ampLimit :: !a}

instance Functor Parameter where
   {-# INLINE fmap #-}
   fmap f p = Parameter
      (f $ k1 p) (f $ k2 p) (f $ ampIn p) (f $ ampI1 p) (f $ ampI2 p) (f $ ampLimit p)

instance Applicative Parameter where
   {-# INLINE pure #-}
   pure x = Parameter x x x x x x
   {-# INLINE (<*>) #-}
   f <*> p = Parameter
      (k1 f $ k1 p) (k2 f $ k2 p) (ampIn f $ ampIn p) (ampI1 f $ ampI1 p) (ampI2 f $ ampI2 p) (ampLimit f $ ampLimit p)

instance Fold.Foldable Parameter where
   {-# INLINE foldMap #-}
   foldMap = Trav.foldMapDefault

instance Trav.Traversable Parameter where
   {-# INLINE sequenceA #-}
   sequenceA p =
      App.lift6 Parameter
         (k1 p) (k2 p) (ampIn p) (ampI1 p) (ampI2 p) (ampLimit p)

instance Interpol.C a v => Interpol.C a (Parameter v) where
   {-# INLINE scaleAndAccumulate #-}
   scaleAndAccumulate =
      Interpol.scaleAndAccumulateApplicative
{-
      Interpol.runMac $ App.lift6 Parameter
         (Interpol.element k1)
         (Interpol.element k2)
         (Interpol.element ampIn)
         (Interpol.element ampI1)
         (Interpol.element ampI2)
         (Interpol.element ampLimit)
-}

instance Storable a => Storable (Parameter a) where
   sizeOf    = Store.sizeOf storeParameter
   alignment = Store.alignment storeParameter
   peek      = Store.peek storeParameter
   poke      = Store.poke storeParameter

storeParameter ::
   Storable a => Store.Dictionary (Parameter a)
storeParameter =
   Store.run $
   App.lift6 Parameter
      (Store.element k1)
      (Store.element k2)
      (Store.element ampIn)
      (Store.element ampI1)
      (Store.element ampI2)
      (Store.element ampLimit)


data Result a =
        Result {highpass, bandpass, lowpass, bandlimit :: !a}

instance Functor Result where
   {-# INLINE fmap #-}
   fmap f p = Result
      (f $ highpass p) (f $ bandpass p) (f $ lowpass p) (f $ bandlimit p)

instance Applicative Result where
   {-# INLINE pure #-}
   pure x = Result x x x x
   {-# INLINE (<*>) #-}
   f <*> p = Result
      (highpass f $ highpass p) (bandpass f $ bandpass p) (lowpass f $ lowpass p) (bandlimit f $ bandlimit p)

instance Fold.Foldable Result where
   {-# INLINE foldMap #-}
   foldMap = Trav.foldMapDefault

instance Trav.Traversable Result where
   {-# INLINE sequenceA #-}
   sequenceA p =
      App.lift4 Result
         (highpass p) (bandpass p) (lowpass p) (bandlimit p)

instance Additive.C v => Additive.C (Result v) where
   {-# INLINE zero #-}
   {-# INLINE (+) #-}
   {-# INLINE (-) #-}
   {-# INLINE negate #-}
   zero   = pure zero
   (+)    = App.lift2 (+)
   (-)    = App.lift2 (-)
   negate = fmap negate
{-
   zero = Result zero zero zero zero
   (+) (Result xhp xbp xlp xbl) (Result yhp ybp ylp ybl) =
      Result (xhp + yhp) (xbp + ybp) (xlp + ylp) (xbl + ybl)
   (-) (Result xhp xbp xlp xbl) (Result yhp ybp ylp ybl) =
      Result (xhp - yhp) (xbp - ybp) (xlp - ylp) (xbl - ybl)
   negate (Result xhp xbp xlp xbl) =
      Result (negate xhp) (negate xbp) (negate xlp) (negate xbl)
-}

instance Module.C a v => Module.C a (Result v) where
   {-# INLINE (*>) #-}
   s*>v = fmap (s*>) v
{-
   s *> (Result hp bp lp bl) =
      Result (s *> hp) (s *> bp) (s *> lp) (s *> bl)
-}

instance Storable a => Storable (Result a) where
   sizeOf    = Store.sizeOf storeResult
   alignment = Store.alignment storeResult
   peek      = Store.peek storeResult
   poke      = Store.poke storeResult

storeResult ::
   Storable a => Store.Dictionary (Result a)
storeResult =
   Store.run $
   App.lift4 Result
      (Store.element highpass)
      (Store.element bandpass)
      (Store.element lowpass)
      (Store.element bandlimit)



{-|
The computation of the internal parameters is a bit complicated,
but it fulfills the following properties:

* At the resonance frequency the band pass has 180 degree phase shift.
  This is also approximately the frequency
  where the filter has maximum output.
  Even more important, this is the frequency where the band limit filter works.

* At the resonance frequency highpass, lowpass, and bandpass
  amplify by the factor @resonance@.

* The lowpass amplifies the frequency zero by factor 1.

* The highpass amplifies the highest representable (Nyquist) frequency by the factor 1.

* The bandlimit amplifies both frequency zero and Nyquist frequency
  by factor one and cancels the resonance frequency.
-}
{-# INLINE parameter #-}
parameter, parameterAlt, parameterOld :: Trans.C a => Pole a -> Parameter a
parameter (Pole resonance frequency) =
   let w      = sin (pi*frequency)
       w2     = w^2
       q2     = resonance^2
       q21w2  = 4*q2*(1-w2)
       sqrtQZ = w * sqrt (q21w2 + w2)
       pk1    = (w2+sqrtQZ) / (q2+w2+sqrtQZ)
       d      = (q21w2*w2 + w2^2 - q2)
                  / (q21w2 - 2*q2 - w2 + (1-4*w2)*sqrtQZ)
       volHP  = (2-pk1)/4 - d
       volRel = sqrt ((2-pk1 + 4 * d) / volHP)
   in  Parameter
          (pk1/volRel)  volHP
          volHP  volRel  volRel  (recip resonance)

parameterAlt (Pole resonance frequency) =
   let w      = sin (pi*frequency)
       w2     = w^2
       q2     = resonance^2
       sqrtQZ = w * sqrt (4*q2 + w2 - 4*q2*w2)
       pk1    = (w2+sqrtQZ) / (q2+w2+sqrtQZ)
       zr     = 1 - 2 * w2
       pk2    = 2-pk1 +
                   4 * (w2^2-q2*zr^2) / (2*q2*zr-w2+(1-4*w2)*sqrtQZ)
       volHP  = (4-2*pk1-pk2) / 4
       volLP  = pk2
       volBP  = sqrt (volHP*volLP)
   in  Parameter
          (pk1*volHP/volBP)  (pk2*volHP/volLP)
          volHP  (volBP/volHP)  (volLP/volBP)  (recip resonance)

{-
This computation is more affected by cancelations
for small frequencies, i.e. zr1 = cos eps - 1.
-}
parameterOld (Pole resonance frequency) =
   let zr     = cos (2*pi*frequency)
       zr1    = zr-1
       q2     = resonance^2
       sqrtQZ = sqrt (zr1*(-8*q2+zr1-4*q2*zr1))
       pk1    = (-zr1+sqrtQZ) / (2*q2-zr1+sqrtQZ)
       q21zr  = 4*q2*zr
       a      = 2 * (zr1*zr1-q21zr*zr) / (zr1+q21zr+(1+2*zr1)*sqrtQZ)
       pk2    = a+2-pk1
       volHP  = (4-2*pk1-pk2) / 4
       volLP  = pk2
       volBP  = sqrt (volHP*volLP)
   in  Parameter
          (pk1*volHP/volBP)  (pk2*volHP/volLP)
          volHP  (volBP/volHP)  (volLP/volBP)  (recip resonance)


{-
simplified iteration:

s'  = u + k1*i1 - k2*i2
i1' = i1 - s'
i2' = i2 - i1'
y0 = i2'

s' = u + k1*i1 - k2*i2
y0 = i2 - (i1 - s')

y0 = i2 - (i1 - (u + k1*i1 - k2*i2))

y0 = i2 - i1 + u + k1*i1 - k2*i2

y0 = u + (k1-1)*i1 + (1-k2)*i2

y0 = u + (k1-1)*(y1-y2) + (1-k2)*y1

y0 = u + (k1-k2)*y1 + (1-k1)*y2
-}
{- |
Convert parameters of universal filter to general second order filter parameters.
Filtering with these parameters does not yield exactly the same result
since the initial conditions are different.
-}
parameterToSecondOrderLowpass ::
   (Ring.C a) => Parameter a -> SecondOrder.Parameter a
parameterToSecondOrderLowpass p =
   SecondOrder.Parameter {
      SecondOrder.c0 = 1,
      SecondOrder.c1 = 0,
      SecondOrder.c2 = 0,
      SecondOrder.d1 = k1 p - k2 p,
      SecondOrder.d2 = 1 - k1 p
   }


type State v = (v,v)

{-| Universal filter: Computes high pass, band pass, low pass in one go -}
{-# INLINE step #-}
step :: (Ring.C a, Module.C a v) =>
   Parameter a -> v -> MS.State (State v) (Result v)
step p u =
   MS.state $ \(i1,i2) ->
      let newsum = ampIn p *> u + k1 p *> i1 - k2 p *> i2
          newi1  = i1 - ampI1 p *> newsum
          newi2  = i2 - ampI2 p *> newi1
          out    = Result newsum newi1 newi2 (u + ampLimit p *> newi1)
      in  (out, (newi1, newi2))

{-# INLINE modifierInit #-}
modifierInit :: (Ring.C a, Module.C a v) =>
   Modifier.Initialized (State v) (v,v) (Parameter a) v (Result v)
modifierInit =
   Modifier.Initialized id step

{-# INLINE modifier #-}
modifier :: (Ring.C a, Module.C a v) =>
   Modifier.Simple (State v) (Parameter a) v (Result v)
modifier = Sig.modifierInitialize modifierInit (zero, zero)

{-# INLINE causal #-}
causal ::
   (Ring.C a, Module.C a v) =>
   Causal.T (Parameter a, v) (Result v)
causal =
   Causal.fromSimpleModifier modifier


{-# INLINE runInit #-}
runInit :: (Ring.C a, Module.C a v) =>
   (v,v) -> Sig.T (Parameter a) -> Sig.T v -> Sig.T (Result v)
runInit = Sig.modifyModulatedInit modifierInit

{-# INLINE run #-}
run :: (Ring.C a, Module.C a v) =>
   Sig.T (Parameter a) -> Sig.T v -> Sig.T (Result v)
run = runInit (zero, zero)
