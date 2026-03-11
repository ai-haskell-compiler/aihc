-- cf. Haskore/Guitar
module Reactive.Banana.MIDI.Guitar where

import qualified Reactive.Banana.MIDI.Pitch as Pitch
import Sound.MIDI.Message.Channel.Voice (Pitch, toPitch, )

import qualified Data.List.Key as Key
import Data.Maybe (mapMaybe, )


mapChordToString ::
   (Pitch.C pitch) =>
   [Pitch] -> [pitch] -> [pitch]
mapChordToString strings chord =
   mapMaybe (choosePitchForString chord) strings

choosePitchForString ::
   (Pitch.C pitch) =>
   [pitch] -> Pitch -> Maybe pitch
choosePitchForString chord string =
   let roundDown x d = x - mod x d
       minAbove x =
          Pitch.increase
             (- roundDown (Pitch.subtract string (Pitch.extract x)) 12) x
   in  Key.maximum (fmap Pitch.extract) $ map minAbove chord

stringPitches :: [Pitch]
stringPitches =
   reverse $ map toPitch [40, 45, 50, 55, 59, 64]
--   reverse [(-2,E), (-2,A), (-1,D), (-1,G), (-1,B), (0,E)]
