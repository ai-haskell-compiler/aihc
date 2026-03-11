module Synthesizer.PiecewiseConstant.Storable (
   toSignal,
   toSignalInit,
   toSignalInitWith,
   ) where

import qualified Synthesizer.PiecewiseConstant.Private as PC
import Synthesizer.PiecewiseConstant.Private (StrictTime)

import qualified Synthesizer.Storable.Signal     as SigSt
import qualified Data.StorableVector.Lazy.Pattern as SigStV
import qualified Data.StorableVector.Lazy        as SVL

import qualified Data.EventList.Relative.BodyTime  as EventListBT
import qualified Data.EventList.Relative.TimeBody  as EventList

import Foreign.Storable (Storable, )

import qualified Numeric.NonNegative.Wrapper as NonNegW
import qualified Numeric.NonNegative.Chunky as NonNegChunky



chunkSizesFromStrictTime :: StrictTime -> NonNegChunky.T SigSt.ChunkSize
chunkSizesFromStrictTime =
   NonNegChunky.fromChunks .
   map (SVL.ChunkSize . NonNegW.toNumber) .
   PC.chopLongTime


replicateLong :: (Storable y) => StrictTime -> y -> SigSt.T y
replicateLong t y =
   SigStV.replicate (chunkSizesFromStrictTime t) y

{-# INLINE toSignal #-}
toSignal :: (Storable y) => EventListBT.T StrictTime y -> SigSt.T y
toSignal = PC.toSignal replicateLong

{-# INLINE toSignalInit #-}
toSignalInit :: (Storable y) => y -> EventList.T StrictTime y -> SigSt.T y
toSignalInit = PC.toSignalInit replicateLong

{-# INLINE toSignalInitWith #-}
toSignalInitWith ::
   (Storable c) =>
   (y -> c) -> c -> EventList.T StrictTime [y] -> SigSt.T c
toSignalInitWith = PC.toSignalInitWith replicateLong
