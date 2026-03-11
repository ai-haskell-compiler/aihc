module Synthesizer.PiecewiseConstant.Generic (
   toSignal,
   toSignalInit,
   toSignalInitWith,
   ) where

import qualified Synthesizer.PiecewiseConstant.Private as PC
import Synthesizer.PiecewiseConstant.Private (StrictTime)

import qualified Synthesizer.Generic.Signal as SigG
import qualified Synthesizer.Generic.Cut    as CutG

import qualified Data.EventList.Relative.BodyTime as EventListBT
import qualified Data.EventList.Relative.TimeBody as EventList

import qualified Numeric.NonNegative.Wrapper as NonNegW



replicateLong ::
   (SigG.Write sig y) =>
   StrictTime -> y -> sig y
replicateLong tl y =
   CutG.concat $
   map (\t ->
      SigG.replicate
--         (SigG.LazySize $ fromIntegral $ maxBound::Int)
         SigG.defaultLazySize
         (NonNegW.toNumber t) y) $
   PC.chopLongTime tl

{-# INLINE toSignal #-}
toSignal :: (SigG.Write sig y) => EventListBT.T StrictTime y -> sig y
toSignal = PC.toSignal replicateLong

{-# INLINE toSignalInit #-}
toSignalInit :: (SigG.Write sig y) => y -> EventList.T StrictTime y -> sig y
toSignalInit = PC.toSignalInit replicateLong

{-# INLINE toSignalInitWith #-}
toSignalInitWith ::
   (SigG.Write sig c) =>
   (y -> c) -> c -> EventList.T StrictTime [y] -> sig c
toSignalInitWith = PC.toSignalInitWith replicateLong
