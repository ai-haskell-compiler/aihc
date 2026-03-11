module Synthesizer.MIDI.Value.BendWheelPressure where

import qualified Sound.MIDI.Message.Class.Check as Check
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import Sound.MIDI.Message.Channel (Channel, )

import qualified Data.Accessor.Monad.Trans.State as AccState
import qualified Data.Accessor.Basic as Accessor

import Control.Monad.Trans.State (State, get, )
import Control.Monad (msum, )

import Data.Traversable (sequence, )

import Control.DeepSeq (NFData, rnf, )

import Prelude hiding (sequence, )


data T = Cons {bend_, wheel_, pressure_ :: Int}
   deriving (Show, Eq)

deflt :: T
deflt = Cons 0 0 0


bend, wheel, pressure :: Accessor.T T Int
bend =
   Accessor.fromSetGet
      (\b (Cons _ w p) -> Cons b w p)
      bend_

wheel =
   Accessor.fromSetGet
      (\w (Cons b _ p) -> Cons b w p)
      wheel_

pressure =
   Accessor.fromSetGet
      (\p (Cons b w _) -> Cons b w p)
      pressure_


instance NFData T where
   rnf (Cons b w p) =
      case (rnf b, rnf w, rnf p) of
         ((), (), ()) -> ()


check ::
   Check.C event =>
   Channel -> event -> State T (Maybe T)
check chan ev =
   sequence $
   (fmap (>> get)) $
   msum $ map ($ ev) $
      (fmap (AccState.set bend) . Check.pitchBend chan) :
      (fmap (AccState.set wheel) . Check.controller chan VoiceMsg.modulation) :
      (fmap (AccState.set pressure) . Check.channelPressure chan) :
      []
