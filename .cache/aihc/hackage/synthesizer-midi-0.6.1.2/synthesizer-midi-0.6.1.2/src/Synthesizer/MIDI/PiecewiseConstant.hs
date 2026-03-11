{- |
Convert MIDI events of a MIDI controller to a control signal.
-}
{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.MIDI.PiecewiseConstant (
   T,
   duration,
   PC.zipWith,

   initWith,
   controllerLinear,
   controllerExponential,
   pitchBend,
   channelPressure,
   bendWheelPressure,
   bendWheelPressureZip,
   ) where

import qualified Synthesizer.MIDI.EventList as Ev
import Synthesizer.MIDI.EventList (LazyTime, StrictTime, Filter, Channel, )

import qualified Sound.MIDI.Message.Class.Check as Check
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import qualified Synthesizer.MIDI.Value.BendModulation as BM
import qualified Synthesizer.MIDI.Value.BendWheelPressure as BWP
import qualified Synthesizer.MIDI.Value as MV

import qualified Synthesizer.PiecewiseConstant.Signal as PC

import qualified Data.EventList.Relative.TimeMixed as EventListTM
import qualified Data.EventList.Relative.MixedTime as EventListMT
import qualified Data.EventList.Relative.BodyTime  as EventListBT
import qualified Data.EventList.Relative.TimeBody  as EventList

import qualified Numeric.NonNegative.Class   as NonNeg
import qualified Numeric.NonNegative.Chunky as NonNegChunky

import qualified Algebra.Transcendental as Trans
import qualified Algebra.RealRing       as RealRing
import qualified Algebra.Field          as Field

import Control.Monad.Trans.State (State, evalState, state, get, put, )
import Control.Monad (liftM, liftM2, )
import Data.Traversable (traverse, )
import Data.Foldable (traverse_, )

import qualified Data.List.HT as ListHT
import Data.Either (Either(Left, Right), )
import Data.Maybe (maybe, )
import Data.Function ((.), ($), flip, )

import NumericPrelude.Numeric
import NumericPrelude.Base (fmap, (>>), )


type T = EventListBT.T StrictTime


duration :: T y -> LazyTime
duration =
   NonNegChunky.fromChunks . EventListBT.getTimes


{-# INLINE initWith #-}
initWith ::
   (y -> c) ->
   c -> EventList.T StrictTime [y] -> T c
initWith f initial =
{-
   EventListTM.switchBodyR EventListBT.empty
      (\xs _ -> EventListMT.consBody initial xs) .
-}
   EventListMT.consBody initial .
   flip EventListTM.snocTime NonNeg.zero .
   flip evalState initial .
   traverse
      (\ys -> traverse_ (put . f) ys >> get)


{-# INLINE controllerLinear #-}
controllerLinear ::
   (Check.C event, Field.C y) =>
   Channel -> Ev.Controller ->
   (y,y) -> y ->
   Filter event (T y)
controllerLinear chan ctrl bnd initial =
   liftM (initWith (MV.controllerLinear bnd) initial) $
   Ev.getControllerEvents chan ctrl


{-# INLINE controllerExponential #-}
controllerExponential ::
   (Check.C event, Trans.C y) =>
   Channel -> Ev.Controller ->
   (y,y) -> y ->
   Filter event (T y)
controllerExponential chan ctrl bnd initial =
   liftM (initWith (MV.controllerExponential bnd) initial) $
   Ev.getControllerEvents chan ctrl


{- |
@pitchBend channel range center@:
emits frequencies on an exponential scale from
@center/range@ to @center*range@.
-}
{-# INLINE pitchBend #-}
pitchBend ::
   (Check.C event, Trans.C y) =>
   Channel ->
   y -> y ->
   Filter event (T y)
pitchBend chan range center =
   liftM (initWith (MV.pitchBend range center) center) $
   Ev.getSlice (Check.pitchBend chan)
--   getPitchBendEvents chan

{-# INLINE channelPressure #-}
channelPressure ::
   (Check.C event, Trans.C y) =>
   Channel ->
   y -> y ->
   Filter event (T y)
channelPressure chan maxVal initVal =
   liftM (initWith (MV.controllerLinear (0,maxVal)) initVal) $
   Ev.getSlice (Check.channelPressure chan)


{-# INLINE bendWheelPressure #-}
bendWheelPressure ::
   (Check.C event, RealRing.C y, Trans.C y) =>
   Channel ->
   Int -> y -> y ->
   Filter event (T (BM.T y))
bendWheelPressure chan
      pitchRange wheelDepth pressDepth =
   let toBM = BM.fromBendWheelPressure pitchRange wheelDepth pressDepth
   in  liftM (initWith toBM (toBM BWP.deflt)) $
       state $
       EventList.unzip .
       fmap ListHT.unzipEithers .
       flip evalState BWP.deflt .
       traverse (traverse (separateBWP chan))

separateBWP ::
   Check.C event =>
   Channel -> event -> State BWP.T (Either BWP.T event)
separateBWP chan ev =
   fmap (maybe (Right ev) Left) $
   BWP.check chan ev


{- |
This one is certainly not as efficient as 'bendWheelPressure'
since it first slices the event list
and then zips the slices together.
-}
{-# INLINE bendWheelPressureZip #-}
bendWheelPressureZip ::
   (Check.C event, RealRing.C y, Trans.C y) =>
   Channel ->
   Int -> y -> y ->
   Filter event (T (BM.T y))
bendWheelPressureZip chan
     pitchRange wheelDepth pressDepth =
   liftM2 (PC.zipWith BM.Cons)
      (pitchBend chan (2^?(fromIntegral pitchRange/12)) 1)
      (liftM2 (PC.zipWith (+))
         (controllerLinear chan VoiceMsg.modulation (0,wheelDepth) 0)
         (channelPressure chan pressDepth 0))
