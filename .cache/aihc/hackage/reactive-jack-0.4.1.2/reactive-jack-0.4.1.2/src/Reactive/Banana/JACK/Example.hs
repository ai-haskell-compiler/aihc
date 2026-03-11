module Reactive.Banana.JACK.Example where

import qualified Reactive.Banana.JACK.Process as Process

import qualified Reactive.Banana.MIDI.Controller as Ctrl
import qualified Reactive.Banana.MIDI.Common as Common
import qualified Reactive.Banana.MIDI.Process as ProcessMidi
import qualified Reactive.Banana.MIDI.Utility as RBU
import qualified Reactive.Banana.MIDI.Note as Note
import qualified Reactive.Banana.MIDI.Time as Time

import qualified Reactive.Banana.Bunch.Combinators as RB

import qualified Sound.MIDI.Message.Class.Check as Check
import qualified Sound.MIDI.Message as MidiMsg
import Sound.MIDI.Message.Channel.Voice (Velocity, normalVelocity, )

import qualified Control.Monad.Trans.State as MS
import Control.Monad (liftM2, liftM3, join, )


melody ::
   RB.Event (pitch, Velocity) ->
   Process.Reactor (RB.Event [Note.Boundary pitch Velocity])
melody =
   fmap fst .
   RBU.traverse []
      (\(pc, v) -> do
          off <- MS.get
          MS.put [Note.Boundary pc normalVelocity False]
          return $ off ++ [Note.Boundary pc v True])

time :: Rational -> Process.RelativeSeconds
time = Time.relative "example" . Time.Seconds

ticks :: Rational -> Process.Reactor Process.RelativeTicks
ticks = Time.ticksFromSeconds . time

getTempo ::
   (Check.C ev) =>
   RB.Event ev ->
   Process.Reactor (RB.Behavior Process.RelativeTicks, RB.Event ev)
getTempo ctrl =
   join $
   liftM3 (uncurry ProcessMidi.tempoCtrl Ctrl.tempoDefault)
      (ticks 0.15) (liftM2 (,) (ticks 0.5) (ticks 0.05)) (return ctrl)

loop ::
   RB.Behavior Process.AbsoluteTicks ->
   RB.Event MidiMsg.T -> Process.Reactor (RB.Event [MidiMsg.T])
loop times input = do
   -- beat <- ProcessMidi.beat . fst =<< getTempo input
   beat <- ProcessMidi.beatVar times . fst =<< getTempo input
   fmap (fmap (map Note.fromBnd)) $ melody $
      fmap (const (Common.PitchChannel (Common.pitch 60) (Common.channel 0),
                   normalVelocity)) beat
