{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Copyright   :  (c) Henning Thielemann 2008
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

All recursive filters with real coefficients
can be decomposed into first order and second order filters with real coefficients.
This follows from the Fundamental theorem of algebra.
-}
module Synthesizer.Plain.Filter.Recursive.SecondOrder (
   Parameter (Parameter, c0, c1, c2, d1, d2),
   State (State, u1, u2, y1, y2),
   adjustPassband,
   amplify,
   causal,
   modifier,
   modifierInit,
   run,
   runInit,
   step,
   zeroState,
   ) where

import qualified Synthesizer.Plain.Signal   as Sig
import qualified Synthesizer.Plain.Modifier as Modifier
import Synthesizer.Plain.Filter.Recursive (Passband(Lowpass,Highpass))

import qualified Synthesizer.Interpolation.Class as Interpol

import qualified Control.Applicative.HT as App
import Control.Applicative (Applicative, pure, (<*>), )

import qualified Data.Foldable as Fold
import qualified Data.Traversable as Trav

import qualified Synthesizer.Causal.Process as Causal

import qualified Algebra.Module                as Module
import qualified Algebra.Field                 as Field
import qualified Algebra.Ring                  as Ring
import qualified Algebra.Additive              as Additive

import Data.List (zipWith6)

import qualified Control.Monad.Trans.State as MS

import qualified Foreign.Storable.Record as Store
import Foreign.Storable (Storable(..))

import NumericPrelude.Numeric
import NumericPrelude.Base


{- | Parameters for a general recursive filter of 2nd order. -}
data Parameter a =
   Parameter {c0, c1, c2, d1, d2 :: !a}
       deriving Show


instance Functor Parameter where
   {-# INLINE fmap #-}
   fmap f p = Parameter
      (f $ c0 p) (f $ c1 p) (f $ c2 p) (f $ d1 p) (f $ d2 p)

instance Applicative Parameter where
   {-# INLINE pure #-}
   pure x = Parameter x x x x x
   {-# INLINE (<*>) #-}
   f <*> p = Parameter
      (c0 f $ c0 p) (c1 f $ c1 p) (c2 f $ c2 p) (d1 f $ d1 p) (d2 f $ d2 p)

instance Fold.Foldable Parameter where
   {-# INLINE foldMap #-}
   foldMap = Trav.foldMapDefault

instance Trav.Traversable Parameter where
   {-# INLINE sequenceA #-}
   sequenceA p =
      App.lift5 Parameter
         (c0 p) (c1 p) (c2 p) (d1 p) (d2 p)

instance Interpol.C a v => Interpol.C a (Parameter v) where
   {-# INLINE scaleAndAccumulate #-}
   scaleAndAccumulate =
      Interpol.runMac $
         App.lift5 Parameter
            (Interpol.element c0)
            (Interpol.element c1)
            (Interpol.element c2)
            (Interpol.element d1)
            (Interpol.element d2)



data State a =
   State {u1, u2, y1, y2 :: !a}
       deriving Show

zeroState :: Additive.C a => State a
zeroState =
   State
      {u1 = zero, u2 = zero,
       y1 = zero, y2 = zero}


instance Functor State where
   {-# INLINE fmap #-}
   fmap f p = State
      (f $ u1 p) (f $ u2 p) (f $ y1 p) (f $ y2 p)

instance Applicative State where
   {-# INLINE pure #-}
   pure x = State x x x x
   {-# INLINE (<*>) #-}
   f <*> p = State
      (u1 f $ u1 p) (u2 f $ u2 p) (y1 f $ y1 p) (y2 f $ y2 p)

instance Fold.Foldable State where
   {-# INLINE foldMap #-}
   foldMap = Trav.foldMapDefault

instance Trav.Traversable State where
   {-# INLINE sequenceA #-}
   sequenceA p =
      App.lift4 State
         (u1 p) (u2 p) (y1 p) (y2 p)



instance Storable a => Storable (Parameter a) where
   sizeOf    = Store.sizeOf storeParameter
   alignment = Store.alignment storeParameter
   peek      = Store.peek storeParameter
   poke      = Store.poke storeParameter

storeParameter ::
   Storable a => Store.Dictionary (Parameter a)
storeParameter =
   Store.run $
   App.lift5 Parameter
      (Store.element c0)
      (Store.element c1)
      (Store.element c2)
      (Store.element d1)
      (Store.element d2)


instance Storable a => Storable (State a) where
   sizeOf    = Store.sizeOf storeState
   alignment = Store.alignment storeState
   peek      = Store.peek storeState
   poke      = Store.poke storeState

storeState ::
   Storable a => Store.Dictionary (State a)
storeState =
   Store.run $
   App.lift4 State
      (Store.element u1)
      (Store.element u2)
      (Store.element y1)
      (Store.element y2)


{- |
Given a function which computes the filter parameters of a lowpass filter
for a given frequency,
turn that into a function which generates highpass parameters,
if requested filter type is Highpass.
-}
{-# INLINE adjustPassband #-}
adjustPassband :: (Field.C a) =>
   Passband -> (a -> Parameter a) -> (a -> Parameter a)
adjustPassband kind comp f =
   case kind of
      Lowpass  -> comp f
      Highpass ->
         let p = comp (0.5-f)
         in  Parameter (c0 p) (- c1 p) (c2 p) (- d1 p) (d2 p)

{- |
Change filter parameter such that result is amplified by a given factor.
-}
{-# INLINE amplify #-}
amplify :: (Ring.C a) =>
   a -> Parameter a -> Parameter a
amplify a p =
   p{c0 = a * c0 p,
     c1 = a * c1 p,
     c2 = a * c2 p}

{-# INLINE step #-}
step :: (Ring.C a, Module.C a v) =>
   Parameter a -> v -> MS.State (State v) v
step c u0 = MS.state $ \s ->
   let y0 =
          c0 c *> u0   +
          c1 c *> u1 s + d1 c *> y1 s +
          c2 c *> u2 s + d2 c *> y2 s
   in  (y0, State
               {u1 = u0, u2 = u1 s,
                y1 = y0, y2 = y1 s})


{-# INLINE modifierInit #-}
modifierInit :: (Ring.C a, Module.C a v) =>
   Modifier.Initialized (State v) (State v) (Parameter a) v v
modifierInit =
   Modifier.Initialized id step

{-# INLINE modifier #-}
modifier :: (Ring.C a, Module.C a v) =>
   Modifier.Simple (State v) (Parameter a) v v
modifier =
   Sig.modifierInitialize modifierInit zeroState

{-# INLINE causal #-}
causal :: (Ring.C a, Module.C a v) =>
   Causal.T (Parameter a, v) v
causal =
   Causal.fromSimpleModifier modifier


{-# INLINE runInit #-}
runInit :: (Ring.C a, Module.C a v) =>
   State v -> Sig.T (Parameter a) -> Sig.T v -> Sig.T v
runInit sInit control input =
   let u0s = input
       u1s = u1 sInit : u0s
       u2s = u2 sInit : u1s
       y1s = y1 sInit : y0s
       y2s = y2 sInit : y1s
       y0s = zipWith6
          (\c u0_ u1_ u2_ y1_ y2_ ->
              c0 c *> u0_ +
              c1 c *> u1_ + d1 c *> y1_ +
              c2 c *> u2_ + d2 c *> y2_)
          control u0s u1s u2s y1s y2s
   in  y0s

{-# INLINE run #-}
run :: (Ring.C a, Module.C a v) =>
   Sig.T (Parameter a) -> Sig.T v -> Sig.T v
run =
   runInit zeroState
