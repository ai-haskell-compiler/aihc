{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sound.ALSA.Sequencer.Event.Remove (
  T,
  malloc,
  run,

  Condition,
  getCondition,
  setCondition,

  condInput,
  condOutput,
  condDest,
  condDestChannel,
  condTimeBefore,
  condTimeAfter,
  condTimeTick,
  condEventType,
  condIgnoreOff,
  condTagMatch,

  getQueue,
  getChannel,
  getEventType,
  getTag,
  getDest,
  getRealTime,
  getTickTime,

  setQueue,
  setChannel,
  setEventType,
  setTag,
  setDest,
  setRealTime,
  setTickTime,
  ) where

#include <alsa/asoundlib.h>
#include <Sound/ALSA/Sequencer/Area.h>

import qualified Sound.ALSA.Sequencer.Marshal.Sequencer as Seq
import qualified Sound.ALSA.Sequencer.Marshal.Address as Addr
import qualified Sound.ALSA.Sequencer.Marshal.Queue as Queue
import qualified Sound.ALSA.Sequencer.Marshal.Event as Event
import qualified Sound.ALSA.Sequencer.Marshal.RealTime as RealTime
import qualified Sound.ALSA.Sequencer.Marshal.Time as Time
import qualified Sound.ALSA.Sequencer.Area as Area
import qualified Sound.ALSA.Exception as Exc

import qualified Data.EnumBitSet as EnumSet
import qualified Foreign.C.Types as C
import Foreign.Ptr (Ptr, )


#area "remove_events"

-- | Remove events according to the given conditions
run :: Seq.T mode -> T -> IO ()
run (Seq.Cons h) info =
  Exc.checkResult_ "EventRemove.run" =<< with info (run_ h)

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_remove_events"
  run_ :: Ptr Seq.Core -> Ptr T_ -> IO C.CInt


#{get_set_int_gen "condition", "Condition", "Condition", "EnumSet.Cons", "EnumSet.decons", "C.CUInt"}

#{get_set_int "queue", "Queue", "Queue.T", "Queue.imp", "Queue.exp"}
#{get_set_int "channel", "Channel", "Event.Channel", "(Event.Channel . fromIntegral)", "(fromIntegral . Event.unChannel)"}
#{get_set_int "event_type", "EventType", "Event.EType", "(Event.EType . fromIntegral)", "(fromIntegral . Event.unEType)"}
#{get_set_int "tag", "Tag", "Event.Tag", "(Event.Tag . fromIntegral)", "(fromIntegral . Event.unTag)"}
#{get_set_ptr "dest", "Dest", "Addr.T"}
#{get_set_ptr "time", "RealTime", "RealTime.T"}
#{get_set_ptr "time", "TickTime", "Time.Tick"}


newtype ConditionFlag = ConditionFlag Int
   deriving (Eq, Ord, Enum)

type Condition = EnumSet.T C.CUInt ConditionFlag


#{enum Condition, EnumSet.Cons
 , condInput       = SND_SEQ_REMOVE_INPUT        /* Flush input queues */
 , condOutput      = SND_SEQ_REMOVE_OUTPUT       /* Flush output queues */
 , condDest        = SND_SEQ_REMOVE_DEST         /* Restrict by destination q:client:port */
 , condDestChannel = SND_SEQ_REMOVE_DEST_CHANNEL /* Restrict by channel */
 , condTimeBefore  = SND_SEQ_REMOVE_TIME_BEFORE  /* Restrict to before time */
 , condTimeAfter   = SND_SEQ_REMOVE_TIME_AFTER   /* Restrict to time or after */
 , condTimeTick    = SND_SEQ_REMOVE_TIME_TICK    /* Time is in ticks */
 , condEventType   = SND_SEQ_REMOVE_EVENT_TYPE   /* Restrict to event type */
 , condIgnoreOff   = SND_SEQ_REMOVE_IGNORE_OFF   /* Do not flush off events */
 , condTagMatch    = SND_SEQ_REMOVE_TAG_MATCH    /* Restrict to events with given tag */
 }
