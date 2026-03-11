{- |
Signals equipped with volume and sample rate information that may carry a unit.
Kind of volume and sample rate is configurable by types.
-}
module Synthesizer.Dimensional.Signal (
   T, R,
   asTypeOfAmplitude,
   render, apply,
   cache, bindCached, share,
   store, restore,
   ($-),  ($&),
   (&*^), (&*>^),
   ) where

import Synthesizer.Dimensional.Signal.Private as SigA

-- import qualified Synthesizer.Dimensional.Rate as Rate
import qualified Synthesizer.Dimensional.Amplitude as Amp
import qualified Synthesizer.Dimensional.Amplitude.Displacement as Disp
import qualified Synthesizer.Dimensional.Amplitude.Flat as Flat
import qualified Synthesizer.Dimensional.Amplitude.Control as CtrlV

import qualified Synthesizer.Dimensional.Process as Proc
import Synthesizer.Dimensional.Process (($:), {-($^), ($#), -} )

import qualified Synthesizer.Generic.Signal  as SigG

-- import qualified Synthesizer.State.Signal as Sig

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim
-- import Number.DimensionTerm ((&/&))

-- import qualified Algebra.Module         as Module
-- import qualified Algebra.RealRing      as RealRing
import qualified Algebra.Field          as Field
import qualified Algebra.Absolute           as Absolute
-- import qualified Algebra.Ring           as Ring

import Control.Applicative (Applicative, )


-- * infix operators for convenience

infixl 0 $-

{- |
Take a scalar argument where a process expects a signal.
Only possible for non-negative values so far.
-}
{-# INLINE ($-) #-}
($-) :: (Field.C y, Absolute.C y, Dim.C u, Dim.C v) =>
    Proc.T s u t (R s v y y -> a) -> DN.T v y -> Proc.T s u t a
($-) f x = f $: Proc.pure (CtrlV.constant x)


($&) :: Applicative f => f (a -> b) -> f a -> f b
($&) = ($:)


infix 7 &*^, &*>^

{-# INLINE (&*^) #-}
(&*^) :: (Flat.C y flat, SigG.Transform sig y) =>
   amp ->
   Proc.T s u t (SigA.T rate flat (sig y)) ->
   Proc.T s u t (SigA.T rate (Amp.Numeric amp) (sig y))
(&*^) v =
   fmap $ Disp.inflateGeneric v

{-# INLINE (&*>^) #-}
(&*>^) ::
   amp ->
   Proc.T s u t (SigA.T rate (Amp.Flat y) sig) ->
   Proc.T s u t (SigA.T rate (Amp.Numeric amp) sig)
(&*>^) v =
   fmap $ Disp.inflate v
