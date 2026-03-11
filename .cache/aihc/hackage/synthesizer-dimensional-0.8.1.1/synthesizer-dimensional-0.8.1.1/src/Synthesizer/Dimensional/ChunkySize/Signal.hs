{-# LANGUAGE NoImplicitPrelude #-}
{- |
Copyright   :  (c) Henning Thielemann 2009
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.Dimensional.ChunkySize.Signal (
   store, length,
   ) where

import qualified Synthesizer.Dimensional.Rate as Rate
import qualified Synthesizer.Dimensional.Amplitude as Amp
import qualified Synthesizer.Dimensional.Signal.Private as SigA

import qualified Synthesizer.ChunkySize as ChunkySize
import qualified Synthesizer.ChunkySize.Cut as CutC
import qualified Synthesizer.ChunkySize.Signal as SigC

import qualified Synthesizer.State.Signal as Sig


type Signal s amp sig =
   SigA.T (Rate.Phantom s) amp sig

type Size s =
   SigA.T (Rate.Phantom s) Amp.Abstract ChunkySize.T



{-# INLINE store #-}
store ::
   (SigC.Write sig yv) =>
   Size s ->
   Signal s amp (Sig.T yv) ->
   Signal s amp (sig yv)
store =
   \cs -> SigA.processBody (SigC.fromState (SigA.body cs))


{-
Move to a new module Analysis in order to be consistent with other Analysis modules?
-}
{-# INLINE length #-}
length :: (CutC.Read sig) =>
   Signal s amp sig ->
   Size s
length =
   \xs -> SigA.abstractFromBody (CutC.length (SigA.body xs))
