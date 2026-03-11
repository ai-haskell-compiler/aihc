{- |
This module allows to cancel events according to some criteria.
In all cases other than 'setInput' and 'setOutput'
the criteria are combined by logical AND.
For every criterion we provide three kinds of accessors:

* @set@: enable a criterion

* @put@: enable or disable a criterion

* @get@: query, whether the criterion is enabled or disabled.

Currently only the @set@ functions are really usable.
The @put@ and @get@ functions would become useful
for manipulating the remove criterion record, that ALSA maintains.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sound.ALSA.Sequencer.Event.RemoveMonad (
  T,
  run,
  -- no need to export this, since Event.Remove is not exported as well
  -- apply,

  setInput,     putInput,     getInput,
  setOutput,    putOutput,    getOutput,
  setChannel,   putChannel,   getChannel,
  setEventType, putEventType,
  setTag,       putTag,       getTag,
  setDest,      putDest,      getDest,
  setTime,      putTime,      getTime,
  setIgnoreOff, putIgnoreOff, getIgnoreOff,
  ) where

import qualified Sound.ALSA.Sequencer.Event.Remove as Remove

import qualified Sound.ALSA.Sequencer.Marshal.Sequencer as Seq
import qualified Sound.ALSA.Sequencer.Marshal.Address as Addr
import qualified Sound.ALSA.Sequencer.Marshal.Queue as Queue
import qualified Sound.ALSA.Sequencer.Marshal.Event as Event
import qualified Sound.ALSA.Sequencer.Marshal.Time as Time

import qualified Control.Monad.Trans.Reader as MR
import qualified Control.Monad.Trans.State as MS
import qualified Control.Monad.Trans.Class as MT
import Control.Applicative (Applicative, )

import qualified Data.EnumBitSet as EnumSet
import Data.EnumBitSet ((.-.), (.|.), )

import Control.Monad (liftM2, )
import Data.Monoid (mempty, mappend, )


newtype T a = Cons (MR.ReaderT Remove.T (MS.StateT Remove.Condition IO) a)
   deriving (Functor, Applicative, Monad)


unpack :: T a -> Remove.T -> Remove.Condition -> IO (a, Remove.Condition)
unpack (Cons m) r = MS.runStateT (MR.runReaderT m r)

-- | apply the changes in the Remove monad to the Remove record
_apply :: T a -> Remove.T -> IO a
_apply m r = do
  c0 <- Remove.getCondition r
  (a,c1) <- unpack m r c0
  Remove.setCondition r c1
  return a

-- | Remove events according to the given conditions
run :: Seq.T mode -> T a -> IO a
run h m = do
  r <- Remove.malloc
  (a,c) <- unpack m r EnumSet.empty
  Remove.setCondition r c
  Remove.run h r
  return a



liftGet :: (Remove.T -> IO a) -> T a
liftGet f = Cons $ MR.ReaderT $ MT.lift . f

liftGetCond :: (Remove.T -> IO a) -> Remove.Condition -> T (Maybe a)
liftGetCond f cond = do
   b <- getCond cond
   if b
     then fmap Just $ liftGet f
     else return Nothing

liftSet :: (Remove.T -> b -> IO a) -> b -> T a
liftSet f x = Cons $ MR.ReaderT $ MT.lift . flip f x

liftSetCond :: (Remove.T -> a -> IO b) -> Remove.Condition -> a -> T b
liftSetCond f cond x = do
   modifyCond $ mappend cond
   liftSet f x

liftPutCond :: (Remove.T -> a -> IO ()) -> Remove.Condition -> Maybe a -> T ()
liftPutCond f cond mx =
   case mx of
      Nothing -> modifyCond $ (.-. cond)
      Just x -> liftSetCond f cond x

getCond :: Remove.Condition -> T Bool
getCond cond =
   Cons $ MT.lift $ MS.gets $ EnumSet.subset cond

setCond :: Remove.Condition -> T ()
setCond cond =
   modifyCond $ mappend cond

putCond :: Remove.Condition -> Bool -> T ()
putCond cond b =
   modifyCond $ (if b then (.|.) else flip (.-.)) cond

modifyCond :: (Remove.Condition -> Remove.Condition) -> T ()
modifyCond f =
   Cons $ MT.lift $ MS.modify f

{- |
All events in the local input buffer are removed.
The conditions are not checked for these events.
This is equivalent to 'Event.dropInputBuffer'.
-}
setInput :: T ()
putInput :: Bool -> T ()
getInput :: T Bool

{- |
Matching events in the local output buffer are removed, too.
Matching events in the kernel buffer are removed in any case.
If there are no further conditions,
then this is equivalent to 'Event.dropOutputBuffer'.
-}
setOutput :: T ()
putOutput :: Bool -> T ()
getOutput :: T Bool

setChannel :: Event.Channel -> T ()
putChannel :: Maybe Event.Channel -> T ()
getChannel :: T (Maybe Event.Channel)

_setEventType :: Event.EType -> T ()
_getEventType :: T Event.EType
setEventType :: Event.Type e => e -> T ()
putEventType :: Event.Type e => Maybe e -> T ()

setTag :: Event.Tag -> T ()
putTag :: Maybe Event.Tag -> T ()
getTag :: T (Maybe Event.Tag)

{- |
ALSA maintainers say, that destination address and queue are checked together,
at least in the kernel buffer.
However up to ALSA-1.0.22 the check for the queue is missing in libasound
for the local buffer.
-}
setDest :: (Addr.T, Queue.T) -> T ()
putDest :: Maybe (Addr.T, Queue.T) -> T ()
getDest :: T (Maybe (Addr.T, Queue.T))

{- |
NoteOff events are kept in any case.
-}
setIgnoreOff :: T ()
putIgnoreOff :: Bool -> T ()
getIgnoreOff :: T Bool


getInput = getCond Remove.condInput
setInput = setCond Remove.condInput
putInput = putCond Remove.condInput

getOutput = getCond Remove.condOutput
setOutput = setCond Remove.condOutput
putOutput = putCond Remove.condOutput

getChannel = liftGetCond Remove.getChannel Remove.condDestChannel
setChannel = liftSetCond Remove.setChannel Remove.condDestChannel
putChannel = liftPutCond Remove.setChannel Remove.condDestChannel

_getEventType = liftGet Remove.getEventType
_setEventType = liftSetCond Remove.setEventType Remove.condEventType
setEventType =
   liftSetCond Remove.setEventType Remove.condEventType . Event.expEv
putEventType =
   liftPutCond Remove.setEventType Remove.condEventType . fmap Event.expEv

getTag = liftGetCond Remove.getTag Remove.condTagMatch
setTag = liftSetCond Remove.setTag Remove.condTagMatch
putTag = liftPutCond Remove.setTag Remove.condTagMatch

getDestQueue :: Remove.T -> IO (Addr.T, Queue.T)
getDestQueue r = liftM2 (,) (Remove.getDest r) (Remove.getQueue r)
setDestQueue :: Remove.T -> (Addr.T, Queue.T) -> IO ()
setDestQueue r (a,q) = Remove.setDest r a >> Remove.setQueue r q

getDest = liftGetCond getDestQueue Remove.condDest
setDest = liftSetCond setDestQueue Remove.condDest
putDest = liftPutCond setDestQueue Remove.condDest

getIgnoreOff = getCond Remove.condIgnoreOff
setIgnoreOff = setCond Remove.condIgnoreOff
putIgnoreOff = putCond Remove.condIgnoreOff


getTime :: T (Maybe Ordering, Time.Stamp)
getTime = do
   ticks <- getCond Remove.condTimeTick
   stamp <-
      if ticks
        then fmap Time.Tick $ liftGet Remove.getTickTime
        else fmap Time.Real $ liftGet Remove.getRealTime
   after  <- getCond Remove.condTimeAfter
   before <- getCond Remove.condTimeBefore
   let mo =
          case (after, before) of
             (False, False) -> Nothing
             (True,  False) -> Just GT
             (False, True ) -> Just LT
             (True,  True ) -> Just EQ
   return (mo, stamp)

setTime :: Ordering -> Time.Stamp -> T ()
setTime o = putTime $ Just o

putTime :: Maybe Ordering -> Time.Stamp -> T ()
putTime mo t = do
   modifyCond ( .-. (Remove.condTimeAfter .|. Remove.condTimeBefore))
   modifyCond $ mappend $
      case mo of
         Nothing -> mempty
         Just LT -> Remove.condTimeBefore
         Just GT -> Remove.condTimeAfter
         Just EQ -> mappend Remove.condTimeBefore Remove.condTimeAfter
   case t of
      Time.Tick x -> do
         modifyCond $ (.-. Remove.condTimeTick)
         liftSet Remove.setTickTime x
      Time.Real x -> do
         modifyCond $ (.|. Remove.condTimeTick)
         liftSet Remove.setRealTime x
