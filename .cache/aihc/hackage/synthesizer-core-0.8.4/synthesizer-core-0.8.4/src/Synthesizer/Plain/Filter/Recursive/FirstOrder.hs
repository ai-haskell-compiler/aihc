{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Copyright   :  (c) Henning Thielemann 2008-2011
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

First order low pass and high pass filter.
-}
module Synthesizer.Plain.Filter.Recursive.FirstOrder where

import qualified Synthesizer.Plain.Signal   as Sig
import qualified Synthesizer.Plain.Modifier as Modifier
import qualified Synthesizer.Causal.Process as Causal

import qualified Synthesizer.Interpolation.Class as Interpol

import qualified Control.Applicative as App
import Control.Monad.Trans.State (State, state, )
import Control.Applicative (pure, liftA2, )

import qualified Data.Foldable as Fold
import qualified Data.Traversable as Trav

import qualified Foreign.Storable.Newtype as Store
import qualified Foreign.Storable.Traversable as StoreTrav
import Foreign.Storable (Storable(sizeOf, alignment, peek, poke))

import qualified Test.QuickCheck as QC

import qualified Algebra.Module                as Module
import qualified Algebra.Transcendental        as Trans
import qualified Algebra.Ring                  as Ring
import qualified Algebra.Additive              as Additive

import NumericPrelude.Numeric
import NumericPrelude.Base



newtype Parameter a = Parameter {getParameter :: a}
   deriving (Eq, Show)


instance Functor Parameter where
   {-# INLINE fmap #-}
   fmap f (Parameter k) = Parameter (f k)

instance App.Applicative Parameter where
   {-# INLINE pure #-}
   pure x = Parameter x
   {-# INLINE (<*>) #-}
   Parameter f <*> Parameter k =
      Parameter (f k)

instance Fold.Foldable Parameter where
   {-# INLINE foldMap #-}
   foldMap = Trav.foldMapDefault

instance Trav.Traversable Parameter where
   {-# INLINE sequenceA #-}
   sequenceA (Parameter k) =
      fmap Parameter k

instance Interpol.C a v => Interpol.C a (Parameter v) where
   {-# INLINE scaleAndAccumulate #-}
   scaleAndAccumulate = Interpol.makeMac Parameter getParameter

instance Storable a => Storable (Parameter a) where
   sizeOf = Store.sizeOf getParameter
   alignment = Store.alignment getParameter
   peek = Store.peek Parameter
   poke = Store.poke getParameter

instance QC.Arbitrary a => QC.Arbitrary (Parameter a) where
   arbitrary = fmap Parameter QC.arbitrary


{-| Convert cut-off frequency to feedback factor. -}
{-# INLINE parameter #-}
parameter :: Trans.C a => a -> Parameter a
parameter freq = Parameter (exp (-2*pi*freq))


{-# INLINE lowpassStep #-}
lowpassStep :: (Ring.C a, Module.C a v) =>
   Parameter a -> v -> State v v
lowpassStep (Parameter c) x =
   state (\s -> let y = x + c *> (s-x) in (y,y))

{-# INLINE lowpassModifierInit #-}
lowpassModifierInit :: (Ring.C a, Module.C a v) =>
   Modifier.Initialized v v (Parameter a) v v
lowpassModifierInit =
   Modifier.Initialized id lowpassStep

{-# INLINE lowpassModifier #-}
lowpassModifier :: (Ring.C a, Module.C a v) =>
   Modifier.Simple v (Parameter a) v v
lowpassModifier =
   Sig.modifierInitialize lowpassModifierInit zero

{-# INLINE lowpassCausal #-}
lowpassCausal ::
   (Ring.C a, Module.C a v) =>
   Causal.T (Parameter a, v) v
lowpassCausal =
   Causal.fromSimpleModifier lowpassModifier


{-# INLINE lowpassInit #-}
lowpassInit :: (Ring.C a, Module.C a v) =>
   v -> Sig.T (Parameter a) -> Sig.T v -> Sig.T v
lowpassInit =
   Sig.modifyModulatedInit lowpassModifierInit

{-# INLINE lowpass #-}
lowpass :: (Ring.C a, Module.C a v) =>
   Sig.T (Parameter a) -> Sig.T v -> Sig.T v
lowpass = lowpassInit zero


{-# INLINE highpassStep #-}
highpassStep :: (Ring.C a, Module.C a v) =>
   Parameter a -> v -> State v v
highpassStep c x =
   fmap (x-) (lowpassStep c x)

{-# INLINE highpassModifierInit #-}
highpassModifierInit :: (Ring.C a, Module.C a v) =>
   Modifier.Initialized v v (Parameter a) v v
highpassModifierInit =
   Modifier.Initialized id highpassStep

{-# INLINE highpassModifier #-}
highpassModifier :: (Ring.C a, Module.C a v) =>
   Modifier.Simple v (Parameter a) v v
highpassModifier =
   Sig.modifierInitialize highpassModifierInit zero

{-# INLINE highpassInit #-}
highpassInit :: (Ring.C a, Module.C a v) =>
   v -> Sig.T (Parameter a) -> Sig.T v -> Sig.T v
highpassInit =
   Sig.modifyModulatedInit highpassModifierInit

highpassInitAlt :: (Ring.C a, Module.C a v) =>
   v -> Sig.T (Parameter a) -> Sig.T v -> Sig.T v
highpassInitAlt y0 control x =
   zipWith (-) x $ lowpassInit y0 control x

{-# INLINE highpass #-}
highpass :: (Ring.C a, Module.C a v) =>
   Sig.T (Parameter a) -> Sig.T v -> Sig.T v
highpass = highpassInit zero



data Result a = Result {highpass_, lowpass_ :: !a}
   deriving (Eq)

instance Functor Result where
   {-# INLINE fmap #-}
   fmap f p = Result (f $ highpass_ p) (f $ lowpass_ p)

instance App.Applicative Result where
   {-# INLINE pure #-}
   pure x = Result x x
   {-# INLINE (<*>) #-}
   f <*> p = Result (highpass_ f $ highpass_ p) (lowpass_ f $ lowpass_ p)

instance Fold.Foldable Result where
   {-# INLINE foldMap #-}
   foldMap = Trav.foldMapDefault

instance Trav.Traversable Result where
   {-# INLINE sequenceA #-}
   sequenceA p = liftA2 Result (highpass_ p) (lowpass_ p)

instance Additive.C v => Additive.C (Result v) where
   {-# INLINE zero #-}
   {-# INLINE (+) #-}
   {-# INLINE (-) #-}
   {-# INLINE negate #-}
   zero   = pure zero
   (+)    = liftA2 (+)
   (-)    = liftA2 (-)
   negate = fmap negate
{-
   zero = Result zero zero
   (+) (Result xhp xlp) (Result yhp ylp) = Result (xhp + yhp) (xlp + ylp)
   (-) (Result xhp xlp) (Result yhp ylp) = Result (xhp - yhp) (xlp - ylp)
   negate               (Result xhp xlp) = Result (negate xhp) (negate xlp)
-}

instance Module.C a v => Module.C a (Result v) where
   {-# INLINE (*>) #-}
   s*>v = fmap (s*>) v
{-
   s *> (Result hp lp) = Result (s *> hp) (s *> lp)
-}

instance Storable a => Storable (Result a) where
   sizeOf    = StoreTrav.sizeOf
   alignment = StoreTrav.alignment
   peek      = StoreTrav.peekApplicative
   poke      = StoreTrav.poke

instance QC.Arbitrary a => QC.Arbitrary (Result a) where
   arbitrary = liftA2 Result QC.arbitrary QC.arbitrary


{-# INLINE step #-}
step :: (Module.C a v) =>
   Parameter a -> v -> State v (Result v)
step c x =
   fmap (\lp -> Result (x-lp) lp) (lowpassStep c x)

{-# INLINE modifierInit #-}
modifierInit :: (Module.C a v) =>
   Modifier.Initialized v v (Parameter a) v (Result v)
modifierInit =
   Modifier.Initialized id step

{-# INLINE modifier #-}
modifier :: (Module.C a v) =>
   Modifier.Simple v (Parameter a) v (Result v)
modifier =
   Sig.modifierInitialize modifierInit zero

{-# INLINE causal #-}
causal ::
   (Module.C a v) =>
   Causal.T (Parameter a, v) (Result v)
causal =
   Causal.fromSimpleModifier modifier

{-# INLINE causalInit #-}
causalInit ::
   (Module.C a v) =>
   v -> Causal.T (Parameter a, v) (Result v)
causalInit =
   Causal.fromInitializedModifier modifierInit
