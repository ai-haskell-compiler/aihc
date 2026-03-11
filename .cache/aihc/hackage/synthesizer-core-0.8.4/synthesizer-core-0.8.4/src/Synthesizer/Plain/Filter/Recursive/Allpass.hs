{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Copyright   :  (c) Henning Thielemann 2008-2010
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.Plain.Filter.Recursive.Allpass (
   Parameter(Parameter, getParameter),
   State,
   cascade,
   cascadeCausal,
   cascadeModifier,
   cascadeParameter,
   cascadeStep,
   cascadeDiverseStep,
   firstOrder,
   firstOrderCausal,
   firstOrderModifier,
   firstOrderStep,
   flangerParameter,
   flangerPhase,
   makePhase,
   parameter,
   parameterApprox,

   -- for testing
   parameterAlt,
   cascadeState,
   cascadeIterative,
   cascadeStepRec,
   cascadeStepScanl,
   cascadeStepStack,
   cascadeCausalModifier,
   cascadeCausalStacked,
   ) where

import qualified Synthesizer.Plain.Signal   as Sig
import qualified Synthesizer.Plain.Modifier as Modifier
import qualified Synthesizer.Causal.Process as Causal

import qualified Synthesizer.Interpolation.Class as Interpol

import qualified Control.Monad.Trans.State as MS
import qualified Control.Applicative as App

import qualified Data.Foldable as Fold
import qualified Data.Traversable as Trav
import Data.Tuple.HT (mapSnd, )
import Data.Function.HT (nest, )
import Data.List.HT (mapAdjacent, switchR, )

import qualified Foreign.Storable.Newtype as Store
import Foreign.Storable (Storable(sizeOf, alignment, peek, poke))

import qualified Algebra.Module                as Module
import qualified Algebra.RealTranscendental    as RealTrans
import qualified Algebra.Transcendental        as Trans
import qualified Algebra.Field                 as Field
import qualified Algebra.Ring                  as Ring
import qualified Algebra.ZeroTestable          as ZeroTestable

import qualified Number.Complex as Complex

import NumericPrelude.Numeric
import NumericPrelude.Base



newtype Parameter a =
   Parameter {getParameter :: a}  {- ^ Feedback factor. -}
   deriving Show

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


{-
cos phi = (1-r^2)/(1+r^2)
cos omega = (1-s^2)/(1+s^2)
cos (phi-omega)
   = cos phi * cos omega + sin phi * sin omega
   = ((1-r^2)*(1-s^2) + 4*r*s) / ((1+r^2) * (1+s^2))
k = ((1-r^2)*(1+s^2) - (1+r^2)*(1-s^2)) /
    ((1+r^2) * (1+s^2) - ((1-r^2)*(1-s^2) + 4*r*s))
k = 2*(s^2-r^2) / (2*r^2+2*s^2 - 4*r*s)
k = (s^2-r^2) / (r-s)^2
k = (s+r) / (s-r)
-}
{- |
Compute the filter parameter
such that a given phase shift is achieved at a certain frequency.

Both frequency and phase are with respect to unit 1.
This is conform to Phase definition
and allows to avoid Transcendental constraint in some cases
since we need no factor @2*pi@.
See for instance 'parameterApprox'.
However, it is intended that the phase parameter is not of type Phase,
because for the 'cascadeParameter' we divide by the cascade order
and then there is a difference between phase pi and 3*pi.
-}
{-# INLINE parameter #-}
parameter :: Trans.C a =>
     a    {- ^ The phase shift to be achieved for the given frequency. -}
  -> a    {- ^ The frequency we specified the phase shift for. -}
  -> Parameter a
parameter phase frequency =
   let s = tan (pi*frequency)
       r = tan (pi*phase)
   in  Parameter $ (s+r) / (s-r)


{- |
This is the same as 'parameter',
but for @phase = frequency@
it has a division of a zero by a zero of multiplicity 2,
whereas 'parameter' has a division  of a non-zero number by zero.
Thus 'parameter' suffers less from cancellation
if @phase@ is close to @frequency@.
-}
{-# INLINE parameterAlt #-}
parameterAlt :: Trans.C a =>
     a    {- ^ The phase shift to be achieved for the given frequency. -}
  -> a    {- ^ The frequency we specified the phase shift for. -}
  -> Parameter a
parameterAlt phase frequency =
   let omega = 2*pi * frequency
       phi   = 2*pi * phase
       k = (cos phi - cos omega) / (1 - cos (phi - omega))
   in  Parameter k


{- |
An approximation to 'parameter' for small phase and frequency values.
It needs only field operations
due to our choice of the unit 1 for the phase parameter.
-}
{-# INLINE parameterApprox #-}
parameterApprox :: Trans.C a =>
     a    {- ^ The phase shift to be achieved for the given frequency. -}
  -> a    {- ^ The frequency we specified the phase shift for. -}
  -> Parameter a
parameterApprox phase frequency =
   Parameter $ (frequency + phase) / (frequency - phase)


-- * atomic first order allpass

type State v = (v,v)

{-# INLINE firstOrderStep #-}
firstOrderStep :: (Ring.C a, Module.C a v) =>
   Parameter a -> v -> MS.State (State v) v
firstOrderStep (Parameter k) u0 =
   MS.state (\(u1,y1) -> let y0 = u1 + k *> (u0-y1) in (y0,(u0,y0)))

{-# INLINE firstOrderModifier #-}
firstOrderModifier :: (Ring.C a, Module.C a v) =>
   Modifier.Simple (State v) (Parameter a) v v
firstOrderModifier =
   Modifier.Simple (zero,zero) firstOrderStep

{-# INLINE firstOrderCausal #-}
firstOrderCausal :: (Ring.C a, Module.C a v) =>
   Causal.T (Parameter a, v) v
firstOrderCausal =
   Causal.fromSimpleModifier firstOrderModifier

{-# INLINE firstOrder #-}
firstOrder :: (Ring.C a, Module.C a v) =>
   Sig.T (Parameter a) -> Sig.T v -> Sig.T v
firstOrder = Sig.modifyModulated firstOrderModifier


{- |
Compute phase shift of an allpass at a given frequency.
-}
{-# INLINE makePhase #-}
makePhase :: (RealTrans.C a, ZeroTestable.C a) => Parameter a -> a -> a
makePhase (Parameter k) frequency =
   let cis = Complex.cis (- 2*pi * frequency)
   in  Complex.phase (Complex.fromReal k + cis) / pi + frequency


-- * allpass cascade with uniform control

{-# INLINE cascadeParameter #-}
cascadeParameter :: Trans.C a =>
     Int  {- ^ The number of equally designed 1st order allpasses. -}
  -> a    {- ^ The phase shift to be achieved for the given frequency. -}
  -> a    {- ^ The frequency we specified the phase shift for. -}
  -> Parameter a
cascadeParameter order phase =
   parameter (phase / fromIntegral order)

{-# INLINE flangerPhase #-}
flangerPhase :: Field.C a => a
flangerPhase = -1

{-# INLINE flangerParameter #-}
flangerParameter :: Trans.C a => Int -> a -> Parameter a
flangerParameter order frequency =
   cascadeParameter order flangerPhase frequency


{-# INLINE cascadeStep #-}
cascadeStep :: (Ring.C a, Module.C a v) =>
   Parameter a -> v -> MS.State [v] v
cascadeStep = cascadeStepRec

{-
internal storage is not very efficient
because the second value of one pair is equal
to the first value of the subsequent value
-}
{-# INLINE cascadeStepStackPairs #-}
cascadeStepStackPairs :: (Ring.C a, Module.C a v) =>
   Parameter a -> v -> MS.State [State v] v
cascadeStepStackPairs k =
   -- stackStatesR would work as well, but with reversed list of states
   Modifier.stackStatesL (firstOrderStep k)

{-# INLINE cascadeStepStack #-}
{-# INLINE cascadeStepRec #-}
{-# INLINE cascadeStepScanl #-}
cascadeStepStack, cascadeStepRec, cascadeStepScanl ::
   (Ring.C a, Module.C a v) =>
   Parameter a -> v -> MS.State [v] v
cascadeStepStack k x =
   MS.state $
      mapSnd fromPairs .
      MS.runState (cascadeStepStackPairs k x) .
      toPairs

{-# INLINE fromPairs #-}
fromPairs :: [(a,a)] -> [a]
fromPairs xs@(x:_) = fst x : map snd xs
fromPairs [] = error "Allpass.fromPairs: empty list"

{-# INLINE toPairs #-}
toPairs :: [a] -> [(a,a)]
toPairs xs = mapAdjacent (,) xs


cascadeStepRec (Parameter k) x = MS.state $ \s ->
   let crawl _ [] = error "Allpass.crawl needs at least one element in the list"
       crawl u0 (_:[]) = u0:[]
       crawl u0 (u1:y1:us) =
           let y0 = u1 + k *> (u0-y1)
           in  u0 : crawl y0 (y1:us)
       news = crawl x s
   in  (last news, news)

cascadeStepScanl k x = MS.state $ \s ->
   let news =
          scanl
             (MS.evalState . firstOrderStep k)
             x (mapAdjacent (,) s)
   in  (switchR
           (error "Allpass.cascade needs at least one element in the state list")
           (flip const) news,
        news)


{-# INLINE cascadeModifier #-}
cascadeModifier :: (Ring.C a, Module.C a v) =>
   Int -> Modifier.Simple [v] (Parameter a) v v
cascadeModifier order =
   Modifier.Simple (replicate (succ order) zero) cascadeStep

{-# INLINE cascadeCausal #-}
{-# INLINE cascadeCausalStacked #-}
{-# INLINE cascadeCausalModifier #-}
cascadeCausal, cascadeCausalStacked, cascadeCausalModifier ::
   (Ring.C a, Module.C a v) =>
   Int -> Causal.T (Parameter a, v) v
cascadeCausal = cascadeCausalModifier

cascadeCausalStacked order =
   Causal.replicateControlled order firstOrderCausal

cascadeCausalModifier order =
   Causal.fromSimpleModifier (cascadeModifier order)


{-# INLINE cascade #-}
{-# INLINE cascadeState #-}
{-# INLINE cascadeIterative #-}
cascade, cascadeState, cascadeIterative ::
   (Ring.C a, Module.C a v) =>
   Int -> Sig.T (Parameter a) -> Sig.T v -> Sig.T v

{-| Choose one of the implementations below -}
cascade = cascadeState

{-| Simulate the Allpass cascade by a list of states of the partial allpasses -}
cascadeState order =
   Sig.modifyModulated (cascadeModifier order)

{-| Directly implement the allpass cascade as multiple application of allpasses of 1st order -}
cascadeIterative order c =
   nest order (firstOrder c)



-- * allpass cascade with independently controlled atomic allpasses

{-# INLINE cascadeDiverseStep #-}
{-# INLINE cascadeDiverseStepScanl #-}
cascadeDiverseStep, cascadeDiverseStepScanl :: (Ring.C a, Module.C a v) =>
   [Parameter a] -> v -> MS.State [v] v
cascadeDiverseStep = cascadeDiverseStepScanl

cascadeDiverseStepScanl ks x = MS.state $ \s ->
   let news =
          scanl
             (\u0 (k,uy1) -> MS.evalState (firstOrderStep k u0) uy1)
             x (zip ks $ mapAdjacent (,) s)
   in  (switchR
           (error "Allpass.cascadeDiverse needs at least one element in the state list")
           (flip const) news,
        news)
