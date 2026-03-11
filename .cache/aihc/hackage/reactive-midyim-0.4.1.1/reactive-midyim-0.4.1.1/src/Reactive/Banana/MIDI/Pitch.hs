module Reactive.Banana.MIDI.Pitch where

import Reactive.Banana.MIDI.Common
          (PitchChannel(PitchChannel),
           PitchChannelVelocity(PitchChannelVelocity), )

import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import Sound.MIDI.Message.Channel.Voice (Pitch, fromPitch, )

import Data.Bool.HT (if', )
import Data.Maybe.HT (toMaybe, )
import Data.Maybe (fromMaybe, )

import Prelude hiding (subtract, )


class C pitch where
   extract :: pitch -> Pitch
   increase :: Int -> pitch -> Maybe pitch

instance C Pitch where
   extract = id
   increase d p =
      maybeFromInt $ d + VoiceMsg.fromPitch p


instance C PitchChannel where
   extract (PitchChannel p _) = p
   increase d (PitchChannel p c) = do
      p' <- increase d p
      return $ PitchChannel p' c

instance C PitchChannelVelocity where
   extract (PitchChannelVelocity pc _) = extract pc
   increase d (PitchChannelVelocity pc v) = do
      pc' <- increase d pc
      return $ PitchChannelVelocity pc' v


maybeFromInt :: Int -> Maybe Pitch
maybeFromInt p =
   toMaybe
      (VoiceMsg.fromPitch minBound <= p  &&
       p <= VoiceMsg.fromPitch maxBound)
      (VoiceMsg.toPitch p)

subtract :: Pitch -> Pitch -> Int
subtract p0 p1 =
   VoiceMsg.fromPitch p1 - VoiceMsg.fromPitch p0



toClosestOctave :: C pitch => Int -> pitch -> pitch
toClosestOctave target sourceClass =
   let t = target
       s = fromPitch $ extract sourceClass
       x = mod (s - t + 6) 12 + t - 6
       y =
          if' (x<0) (x+12) $
          if' (x>127) (x-12) x
   in  fromMaybe (error "toClosestOctave: pitch should always be in MIDI note range") $
       increase (y-s) sourceClass
