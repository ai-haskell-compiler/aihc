module Reactive.Banana.ALSA.Time where

import qualified Reactive.Banana.MIDI.Time as Time
import Reactive.Banana.ALSA.Private (Reactor, )

import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer.RealTime as RealTime
import qualified Sound.ALSA.Sequencer.Time as ATime

import Prelude hiding (div, )


type AbsoluteTicks = Time.T Reactor Time.Absolute Time.Ticks
type RelativeTicks = Time.T Reactor Time.Relative Time.Ticks

type AbsoluteSeconds = Time.T Reactor Time.Absolute Time.Seconds
type RelativeSeconds = Time.T Reactor Time.Relative Time.Seconds



fromStamp :: ATime.Stamp -> AbsoluteTicks
fromStamp t =
   case t of
      ATime.Real rt ->
         Time.cons $ Time.Ticks $ RealTime.toInteger rt
--      _ -> 0,
      _ -> error "unsupported time stamp type"

toStamp :: AbsoluteTicks -> ATime.Stamp
toStamp t =
   ATime.Real $ RealTime.fromInteger $ Time.unTicks $ Time.decons t


fromEvent :: Event.T -> AbsoluteTicks
fromEvent ev =
   case Event.time ev of
      ATime.Cons ATime.Absolute stamp -> fromStamp stamp
      _ -> error "Time.fromEvent: we can only handle absolute time stamps"
