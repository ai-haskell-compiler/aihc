module TimerWheel.Internal.AlarmBuckets
  ( AlarmBuckets,
    AlarmId,
    insert,
    delete,
    deleteExpiredAt,
    timestampToIndex,
  )
where

import Data.Atomics qualified as Atomics
import Data.Primitive.Array (MutableArray)
import Data.Primitive.Array qualified as Array
import GHC.Base (RealWorld)
import TimerWheel.Internal.Alarm (Alarm (..))
import TimerWheel.Internal.Bucket (Bucket)
import TimerWheel.Internal.Bucket qualified as Bucket
import TimerWheel.Internal.Nanoseconds (Nanoseconds)
import TimerWheel.Internal.Prelude
import TimerWheel.Internal.Timestamp (Timestamp)
import TimerWheel.Internal.Timestamp qualified as Timestamp

type AlarmBuckets =
  MutableArray RealWorld (Bucket Alarm)

type AlarmId =
  Int

insert :: AlarmBuckets -> Nanoseconds -> AlarmId -> Timestamp -> Alarm -> IO ()
insert buckets resolution alarmId timestamp alarm = do
  ticket <- Atomics.readArrayElem buckets index
  loop ticket
  where
    loop :: Atomics.Ticket (Bucket Alarm) -> IO ()
    loop ticket = do
      (success, ticket1) <-
        Atomics.casArrayElem
          buckets
          index
          ticket
          (Bucket.insert alarmId timestamp alarm (Atomics.peekTicket ticket))
      if success then pure () else loop ticket1

    index :: Int
    index =
      timestampToIndex buckets resolution timestamp

delete :: AlarmBuckets -> Nanoseconds -> AlarmId -> Timestamp -> IO Bool
delete buckets resolution alarmId timestamp = do
  ticket <- Atomics.readArrayElem buckets index
  loop ticket
  where
    loop :: Atomics.Ticket (Bucket Alarm) -> IO Bool
    loop ticket =
      case Bucket.deleteExpectingHit alarmId (Atomics.peekTicket ticket) of
        Nothing -> pure False
        Just bucket -> do
          (success, ticket1) <- Atomics.casArrayElem buckets index ticket bucket
          if success then pure True else loop ticket1

    index :: Int
    index =
      timestampToIndex buckets resolution timestamp

deleteExpiredAt :: AlarmBuckets -> Int -> Timestamp -> IO (Bucket Alarm)
deleteExpiredAt buckets index now = do
  ticket <- Atomics.readArrayElem buckets index
  loop ticket
  where
    loop :: Atomics.Ticket (Bucket Alarm) -> IO (Bucket Alarm)
    loop ticket = do
      let Bucket.Pair expired bucket1 = Bucket.partition now (Atomics.peekTicket ticket)
      if Bucket.isEmpty expired
        then pure Bucket.empty
        else do
          (success, ticket1) <- Atomics.casArrayElem buckets index ticket bucket1
          if success then pure expired else loop ticket1

-- `timestampToIndex buckets resolution timestamp` figures out which index `timestamp` corresponds to in `buckets`,
-- where each bucket corresponds to `resolution` nanoseconds.
--
-- For example, consider a three-element `buckets` with resolution `1000000000`.
--
--   +--------------------------------------+
--   | 1000000000 | 1000000000 | 1000000000 |
--   +--------------------------------------+
--
-- Some timestamp like `1053298012387` gets binned to one of the three indices 0, 1, or 2, with quick and easy maffs:
--
--   1. Figure out which index the timestamp corresponds to, if there were infinitely many:
--
--        1053298012387 `div` 1000000000 = 1053
--
--   2. Wrap around per the actual length of the array:
--
--        1053 `rem` 3 = 0
timestampToIndex :: AlarmBuckets -> Nanoseconds -> Timestamp -> Int
timestampToIndex buckets resolution timestamp =
  -- This downcast is safe because there are at most `maxBound :: Int` buckets (not that anyone would ever have that
  -- many...)
  fromIntegral @Word64 @Int
    (Timestamp.epoch resolution timestamp `rem` fromIntegral @Int @Word64 (Array.sizeofMutableArray buckets))
