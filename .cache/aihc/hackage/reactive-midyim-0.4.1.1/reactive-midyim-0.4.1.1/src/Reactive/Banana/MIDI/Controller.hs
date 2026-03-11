module Reactive.Banana.MIDI.Controller where

import qualified Reactive.Banana.MIDI.Time as Time

import qualified Sound.MIDI.Message.Class.Query as Query
import qualified Sound.MIDI.Message.Class.Construct as Construct
import qualified Sound.MIDI.Message.Channel as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg

import Sound.MIDI.Message.Channel (Channel, )
import Sound.MIDI.Message.Channel.Voice (Controller, )

import Data.Maybe.HT (toMaybe, )
import Data.Monoid (mappend, )


tempoDefault :: (Channel, Controller)
tempoDefault =
   (ChannelMsg.toChannel 0, VoiceMsg.toController 16)


type RelativeTickTime m = Time.T m Time.Relative Time.Ticks

duration, durationLinear, durationExponential ::
   (RelativeTickTime m, RelativeTickTime m) ->
   Int -> RelativeTickTime m
duration = durationExponential

durationLinear (minDur, maxDur) val =
   let k = fromIntegral val / 127
   in  Time.scale (1-k) minDur
       `mappend`
       Time.scale k maxDur
--   minDur + Time.scale (fromIntegral val / 127) (maxDur-minDur)

durationExponential (minDur, maxDur) val =
   Time.scale (Time.div maxDur minDur ** (fromIntegral val / 127)) minDur


{-
range ::
   (RealFrac b) =>
   (b,b) -> (a -> b) -> (a -> Int)
range (l,u) f x =
   round $
   limit (0,127) $
   127*(f x - l)/(u-l)
-}


{- |
Map NoteOn events to a controller value.
This way you may play notes via the resonance frequency of a filter.
-}
fromNote ::
   (Query.C msg, Construct.C msg) =>
   (Int -> Int) -> Controller -> msg -> Maybe msg
fromNote f ctrl e =
   maybe
      (Just e)
      (\(c, (_v, p, on)) ->
         toMaybe on $
         curry (Construct.anyController c) ctrl $
         f $ VoiceMsg.fromPitch p)
      (Query.noteExplicitOff e)
