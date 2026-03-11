module Synthesizer.MIDI.CausalIO.ControllerSet (
   T,
   fromChannel,
   slice, PCS.Controller(..),

   controllerLinear,
   controllerExponential,
   pitchBend,
   channelPressure,
   bendWheelPressure,
   ) where

import qualified Synthesizer.CausalIO.Process as PIO
import qualified Synthesizer.MIDI.CausalIO.Process as MIO

import qualified Synthesizer.MIDI.PiecewiseConstant.ControllerSet as PCS
import qualified Synthesizer.MIDI.EventList as MIDIEv
import qualified Synthesizer.MIDI.Value.BendModulation as BM
import qualified Synthesizer.MIDI.Value.BendWheelPressure as BWP
import qualified Synthesizer.MIDI.Value as MV
import qualified Synthesizer.PiecewiseConstant.Signal as PC

import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import qualified Sound.MIDI.Message.Class.Check as Check

import qualified Data.EventList.Relative.TimeTime  as EventListTT
import qualified Data.EventList.Relative.BodyTime  as EventListBT
import qualified Data.EventList.Relative.MixedTime as EventListMT

import qualified Algebra.Transcendental as Trans
import qualified Algebra.Field          as Field
import qualified Algebra.RealRing       as RealRing

import qualified Control.Monad.Trans.State as MS

import qualified Data.Accessor.Basic as Acc

import qualified Data.Map as Map

import Data.Traversable (traverse, )
import Data.Foldable (traverse_, )

import Control.Arrow (Arrow, arr, )
import Control.Category ((.), )

import qualified Data.Maybe as Maybe
import Data.Maybe.HT (toMaybe, )

import NumericPrelude.Numeric
import NumericPrelude.Base hiding ((.), )
import Prelude ()



-- see PCS.mapInsertMany
mapInsertMany ::
   (Ord key) =>
   [(key,a)] -> Map.Map key a -> Map.Map key a
mapInsertMany assignments inits =
   foldl (flip (uncurry Map.insert)) inits assignments


fromChannel ::
   (Check.C event) =>
   MIDIEv.Channel ->
   PIO.T
      (EventListTT.T MIDIEv.StrictTime [event])
      (PCS.T PCS.Controller Int)
fromChannel chan =
   (PIO.traverse Map.empty $ \evs0 -> do
      initial <- MS.get
      fmap (PCS.Cons initial) $
         traverse (\ys -> MS.modify (mapInsertMany ys) >> return ys) evs0)
   .
   MIO.mapMaybe (PCS.maybeController chan)


type T arrow y =
   arrow
      (PCS.T PCS.Controller Int)
      (EventListBT.T PC.ShortStrictTime y)


slice ::
   (Arrow arrow) =>
   PCS.Controller ->
   (Int -> y) {- ^ This might be a function from "Synthesizer.MIDI.Value"
                   or "Synthesizer.Dimensional.MIDIValue" -} ->
   y ->
   T arrow y
slice c f deflt =
   arr $ \(PCS.Cons initial stream) ->
      let yin = maybe deflt f $ Map.lookup c initial
      in  PC.subdivideLongStrict $
          EventListMT.consBody yin $
          flip MS.evalState yin $
          traverse
             (\ys -> traverse_ (MS.put . f) ys >> MS.get) $
          fmap
             (Maybe.mapMaybe
                (\(ci,a) -> toMaybe (c==ci) a))
             stream


controllerLinear ::
   (Field.C y, Arrow arrow) =>
   MIDIEv.Controller ->
   (y,y) -> y ->
   T arrow y
controllerLinear ctrl bnd initial =
   slice (PCS.Controller ctrl) (MV.controllerLinear bnd) initial

controllerExponential ::
   (Trans.C y, Arrow arrow) =>
   MIDIEv.Controller ->
   (y,y) -> y ->
   T arrow y
controllerExponential ctrl bnd initial =
   slice (PCS.Controller ctrl) (MV.controllerExponential bnd) initial

pitchBend ::
   (Trans.C y, Arrow arrow) =>
   y -> y ->
   T arrow y
pitchBend range center =
   slice PCS.PitchBend (MV.pitchBend range center) center

channelPressure ::
   (Trans.C y, Arrow arrow) =>
   y -> y ->
   T arrow y
channelPressure maxVal initial =
   slice PCS.Pressure (MV.controllerLinear (zero,maxVal)) initial

bendWheelPressure ::
   (RealRing.C y, Trans.C y, Arrow arrow) =>
   Int -> y -> y ->
   T arrow (BM.T y)
bendWheelPressure pitchRange wheelDepth pressDepth =
   arr $ \(PCS.Cons initial stream) ->
      let set key field =
             maybe id (Acc.set field) $
             Map.lookup key initial
          yin =
             set PCS.PitchBend BWP.bend $
             set (PCS.Controller VoiceMsg.modulation) BWP.wheel $
             set PCS.Pressure BWP.pressure $
             BWP.deflt
      in  PC.subdivideLongStrict $
          fmap (BM.fromBendWheelPressure pitchRange wheelDepth pressDepth) $
          EventListMT.consBody yin $
          flip MS.evalState yin $
          traverse (\ys0 -> traverse_ MS.put ys0 >> MS.get) $
          fmap Maybe.catMaybes $
          flip MS.evalState BWP.deflt $
          traverse (traverse PCS.checkBendWheelPressure) stream
