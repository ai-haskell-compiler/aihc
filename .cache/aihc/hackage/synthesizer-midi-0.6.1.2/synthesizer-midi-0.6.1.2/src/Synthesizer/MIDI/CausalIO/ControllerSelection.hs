module Synthesizer.MIDI.CausalIO.ControllerSelection (
   fromChannel,
   filter,
   T(Cons),

   controllerLinear,
   controllerExponential,
   pitchBend,
   channelPressure,
   ) where

import qualified Synthesizer.CausalIO.Process as PIO
import qualified Synthesizer.MIDI.CausalIO.Process as MIO

import qualified Synthesizer.MIDI.PiecewiseConstant.ControllerSet as PCS
import qualified Synthesizer.MIDI.EventList as MIDIEv
import qualified Synthesizer.MIDI.Value as MV

import qualified Sound.MIDI.Message.Class.Check as Check

import qualified Data.EventList.Relative.TimeTime  as EventListTT

import qualified Algebra.Transcendental as Trans
import qualified Algebra.Field          as Field

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Tuple.HT (mapSnd, )

import Control.Arrow (Arrow, )

import NumericPrelude.Numeric
import NumericPrelude.Base hiding ((.), filter, )
import Prelude ()



fromChannel ::
   (Check.C event, Arrow arrow) =>
   MIDIEv.Channel ->
   arrow
      (EventListTT.T MIDIEv.StrictTime [event])
      (EventListTT.T MIDIEv.StrictTime [(PCS.Controller, Int)])
fromChannel chan =
   MIO.mapMaybe $ PCS.maybeController chan


-- see PCS.mapInsertMany
mapInsertMany ::
   (Ord key) =>
   [(key,a)] -> Map.Map key a -> Map.Map key a
mapInsertMany assignments inits =
   foldl (flip (uncurry Map.insert)) inits assignments



data T a =
   Cons PCS.Controller (Int -> a) a

filter ::
   [T a] ->
   PIO.T
      (EventListTT.T MIDIEv.StrictTime [(PCS.Controller, Int)])
      (PCS.T Int a)
filter mapping =
   let dict =
          Map.fromList $
          zipWith (\n (Cons cc f _init) -> (cc, (n, f)))
             [0 ..] mapping
   in  PIO.mapAccum
          (\evs curMap ->
             let ctrlEvs =
                    fmap (Maybe.mapMaybe (\(cc, val) ->
                       fmap (mapSnd ($ val)) $ Map.lookup cc dict)) evs
             in  (PCS.Cons curMap ctrlEvs,
                  mapInsertMany
                     (concat $ EventListTT.getBodies ctrlEvs)
                     curMap))
          (Map.fromList $ zip [0..] $
           map (\(Cons _cc _f initVal) -> initVal) mapping)


controllerLinear ::
   (Field.C y) =>
   MIDIEv.Controller ->
   (y,y) -> y ->
   T y
controllerLinear ctrl bnd initial =
   Cons (PCS.Controller ctrl) (MV.controllerLinear bnd) initial

controllerExponential ::
   (Trans.C y) =>
   MIDIEv.Controller ->
   (y,y) -> y ->
   T y
controllerExponential ctrl bnd initial =
   Cons (PCS.Controller ctrl) (MV.controllerExponential bnd) initial

pitchBend ::
   (Trans.C y) =>
   y -> y ->
   T y
pitchBend range center =
   Cons PCS.PitchBend (MV.pitchBend range center) center

channelPressure ::
   (Trans.C y) =>
   y -> y ->
   T y
channelPressure maxVal initial =
   Cons PCS.Pressure (MV.controllerLinear (zero,maxVal)) initial
