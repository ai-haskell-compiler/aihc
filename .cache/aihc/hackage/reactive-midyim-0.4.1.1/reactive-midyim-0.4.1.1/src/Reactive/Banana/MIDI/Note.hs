module Reactive.Banana.MIDI.Note where

import qualified Reactive.Banana.MIDI.Pitch as Pitch
import qualified Reactive.Banana.MIDI.Time as Time
import qualified Reactive.Banana.MIDI.Common as Common
import Reactive.Banana.MIDI.Common (PitchChannel(PitchChannel), )

import qualified Sound.MIDI.Message.Class.Query as Query
import qualified Sound.MIDI.Message.Class.Construct as Construct
import qualified Sound.MIDI.Message.Channel.Mode as Mode
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import Sound.MIDI.Message.Channel.Voice (Velocity, Pitch, )

import Control.Monad (mplus, )
import Data.Monoid (mappend, )



data Boundary key value =
     Boundary key value Bool
   deriving (Eq, Show)

data BoundaryExt key value =
     BoundaryExt (Boundary key value)
   | AllOff (key -> Bool)
   {- ^
   The predicate shall return True,
   if a certain key shall be released by the AllOff statement.
   E.g. the predicate might check for the appropriate channel.
   -}


maybeBnd ::
   Query.C msg =>
   msg -> Maybe (Boundary PitchChannel Velocity)
maybeBnd =
   fmap (\(c, (v, p, on)) -> Boundary (PitchChannel p c) v on) .
   Query.noteExplicitOff

maybeBndExt ::
   Query.C msg =>
   msg -> Maybe (BoundaryExt PitchChannel Velocity)
maybeBndExt ev =
   mplus
      (fmap BoundaryExt $ maybeBnd ev)
      (let allOff chan = Just $ AllOff $ \(PitchChannel _p c) -> chan == c
       in  case Query.mode ev of
               Just (chan, Mode.AllNotesOff) -> allOff chan
               Just (chan, Mode.AllSoundOff) -> allOff chan
               _ -> Nothing)


class Pitch.C x => Make x where
   make :: Construct.C msg => x -> Velocity -> Bool -> msg

instance Make Pitch where
   make p =
      make (PitchChannel p minBound)

instance Make PitchChannel where
   make (PitchChannel p c) vel on =
      Construct.note c (vel, p, on)



fromBnd ::
   (Make key, Common.VelocityField value, Construct.C msg) =>
   Boundary key value -> msg
fromBnd (Boundary pc vel on) =
   make pc (Common.getVelocity vel) on


bundle ::
   (Construct.C msg) =>
   Time.T m Time.Relative Time.Ticks ->
   Time.T m Time.Relative Time.Ticks ->
   (PitchChannel, Velocity) ->
   Common.Bundle m msg
bundle start dur (pc, vel) =
   Common.Future start (make pc vel True) :
   Common.Future (mappend start dur) (make pc vel False) :
   []





lift ::
   (Query.C msg, Construct.C msg) =>
   (Boundary PitchChannel Velocity -> Boundary PitchChannel Velocity) ->
   (msg -> Maybe msg)
lift f msg =
   fmap (fromBnd . f) $ maybeBnd msg

liftMaybe ::
   (Query.C msg, Construct.C msg) =>
   (Boundary PitchChannel Velocity -> Maybe (Boundary PitchChannel Velocity)) ->
   (msg -> Maybe msg)
liftMaybe f msg =
   fmap fromBnd . f =<< maybeBnd msg

{- |
Pitch.C a note event by the given number of semitones.
Non-note events are returned without modification.
If by transposition a note leaves the range of representable MIDI notes,
then we return Nothing.
-}
transpose ::
   Int ->
   Boundary PitchChannel v ->
   Maybe (Boundary PitchChannel v)
transpose d (Boundary (PitchChannel p0 c) v on) =
   fmap
      (\p1 -> Boundary (PitchChannel p1 c) v on)
      (Pitch.increase d p0)

{- |
Swap order of keys.
Non-note events are returned without modification.
If by reversing a note leaves the range of representable MIDI notes,
then we return Nothing.
-}
reverse ::
   Boundary PitchChannel v ->
   Maybe (Boundary PitchChannel v)
reverse (Boundary (PitchChannel p0 c) v on) =
   fmap
      (\p1 -> Boundary (PitchChannel p1 c) v on)
      (Pitch.maybeFromInt $ (60+64 -) $ VoiceMsg.fromPitch p0)

reduceVelocity ::
   Velocity ->
   Boundary pc Velocity ->
   Boundary pc Velocity
reduceVelocity decay (Boundary pc v on) =
   Boundary pc
      (case VoiceMsg.fromVelocity v of
         0 -> v
         vel ->
            VoiceMsg.toVelocity $
            vel - min (VoiceMsg.fromVelocity decay) (vel-1))
      on
