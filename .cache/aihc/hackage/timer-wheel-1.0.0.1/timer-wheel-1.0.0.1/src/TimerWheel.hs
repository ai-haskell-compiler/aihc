-- | This module is intended to be imported qualified:
--
-- > import TimerWheel (TimerWheel)
-- > import TimerWheel qualified
module TimerWheel
  ( -- * Timer wheel
    TimerWheel,

    -- * Timer wheel configuration
    Config (..),
    Seconds,

    -- * Timer
    Timer,

    -- * Constructing a timer wheel
    create,
    with,

    -- * Querying a timer wheel
    count,

    -- * Registering timers in a timer wheel
    register,
    register_,
    recurring,
    recurring_,

    -- * Canceling timers
    cancel,
  )
where

import Control.Exception (mask_)
import Data.Primitive.Array qualified as Array
import Ki qualified
import TimerWheel.Internal.Alarm (Alarm (..))
import TimerWheel.Internal.AlarmBuckets (AlarmBuckets, AlarmId)
import TimerWheel.Internal.AlarmBuckets qualified as AlarmBuckets
import TimerWheel.Internal.Bucket (Bucket)
import TimerWheel.Internal.Bucket qualified as Bucket
import TimerWheel.Internal.Counter (Counter, decrCounter_, incrCounter, incrCounter_, newCounter, readCounter)
import TimerWheel.Internal.Nanoseconds (Nanoseconds (..))
import TimerWheel.Internal.Nanoseconds qualified as Nanoseconds
import TimerWheel.Internal.Prelude
import TimerWheel.Internal.Timer (Timer (..), cancel)
import TimerWheel.Internal.Timestamp (Timestamp)
import TimerWheel.Internal.Timestamp qualified as Timestamp

-- | A timer wheel is a vector-of-collections-of timers to fire. Timers may be one-shot or recurring, and may be
-- scheduled arbitrarily far in the future.
--
-- A timer wheel is configured with a /spoke count/ and /resolution/:
--
-- * The /spoke count/ determines the size of the timer vector.
--
--     A __larger spoke count__ will require __more memory__, but will result in __less insert contention__.
--
-- * The /resolution/ determines the duration of time that each spoke corresponds to, and thus how often timers are
--   checked for expiry.
--
--     For example, in a timer wheel with a /resolution/ of __@1 second@__, a timer that is scheduled to fire at
--     __@8.4 o'clock@__ will end up firing around __@9.0 o'clock@__ instead (that is, on the
--     __@1 second@__-boundary).
--
--     A __larger resolution__ will result in __more insert contention__ and __less accurate timers__, but will require
--     __fewer wakeups__ by the timeout thread.
--
-- The timeout thread has some important properties:
--
--     * There is only one, and it fires expired timers synchronously. If your timer actions execute quicky, you can
--       'register' them directly. Otherwise, consider registering an action that enqueues the real action to be
--       performed on a job queue.
--
--     * A synchronous exception thrown by a registered timer will bring the timeout thread down, and the exception will
--       be propagated to the thread that created the timer wheel. If you want to log and ignore exceptions, for example,
--       you will have to bake this into the registered actions yourself.
--
-- __API summary__
--
-- +----------+---------+----------------+
-- | Create   | Query   | Modify         |
-- +==========+=========+================+
-- | 'create' | 'count' | 'register'     |
-- +----------+---------+----------------+
-- | 'with'   |         | 'register_'    |
-- +----------+         +----------------+
-- |          |         | 'recurring'    |
-- |          |         +----------------+
-- |          |         | 'recurring_'   |
-- +----------+---------+----------------+
data TimerWheel = TimerWheel
  { buckets :: {-# UNPACK #-} !AlarmBuckets,
    resolution :: {-# UNPACK #-} !Nanoseconds,
    -- The number of registered alarms.
    count :: {-# UNPACK #-} !Counter,
    -- A counter to generate unique ints that identify registered actions, so they can be canceled.
    supply :: {-# UNPACK #-} !Counter
  }

-- | A timer wheel config.
--
-- * @spokes@ must be ∈ @[1, maxBound]@, and is set to @1024@ if invalid.
-- * @resolution@ must be ∈ @(0, ∞]@, and is set to @1@ if invalid.
--
-- __API summary__
--
-- +----------+
-- | Create   |
-- +==========+
-- | 'Config' |
-- +----------+
data Config = Config
  { -- | Spoke count
    spokes :: {-# UNPACK #-} !Int,
    -- | Resolution
    resolution :: !Seconds
  }
  deriving stock (Generic, Show)

-- | Create a timer wheel in a scope.
create ::
  -- | ​
  Ki.Scope ->
  -- | ​
  Config ->
  -- | ​
  IO TimerWheel
create scope config = do
  buckets <- Array.newArray spokes Bucket.empty
  count_ <- newCounter
  supply <- newCounter
  Ki.fork_ scope (runTimerReaperThread buckets resolution)
  pure TimerWheel {buckets, count = count_, resolution, supply}
  where
    spokes = if config.spokes <= 0 then 1024 else config.spokes
    resolution = Nanoseconds.fromNonNegativeSeconds (if config.resolution <= 0 then 1 else config.resolution)

-- | Perform an action with a timer wheel.
with ::
  -- | ​
  Config ->
  -- | ​
  (TimerWheel -> IO a) ->
  -- | ​
  IO a
with config action =
  Ki.scoped \scope -> do
    wheel <- create scope config
    action wheel

-- | Get the number of timers in a timer wheel.
--
-- /O(1)/.
count :: TimerWheel -> IO Int
count wheel =
  readCounter wheel.count

-- | @register wheel delay action@ registers __@action@__ in __@wheel@__ to fire after __@delay@__ seconds.
--
-- When canceled, the timer returns whether or not the cancelation was successful; @False@ means the timer had either
-- already fired, or had already been canceled.
register ::
  -- | The timer wheel
  TimerWheel ->
  -- | The delay before the action is fired
  Seconds ->
  -- | The action to fire
  IO () ->
  -- | The timer
  IO (Timer Bool)
register wheel delay action = do
  now <- Timestamp.now
  let ringsAt = now `Timestamp.plus` Nanoseconds.fromSeconds delay
  alarmId <- incrCounter wheel.supply
  insertAlarm wheel alarmId ringsAt (OneShot (action >> decrCounter_ wheel.count))
  coerce @(IO (IO Bool)) @(IO (Timer Bool)) do
    pure do
      mask_ do
        deleted <- AlarmBuckets.delete wheel.buckets wheel.resolution alarmId ringsAt
        when deleted (decrCounter_ wheel.count)
        pure deleted

-- | Like 'register', but for when you don't intend to cancel the timer.
register_ ::
  -- | The timer wheel
  TimerWheel ->
  -- | The delay before the action is fired
  Seconds ->
  -- | The action to fire
  IO () ->
  IO ()
register_ wheel delay action =
  void (register wheel delay action)

-- | @recurring wheel action delay@ registers __@action@__ in __@wheel@__ to fire in __@delay@__ seconds, and every
-- __@delay@__ seconds thereafter.
recurring ::
  -- | The timer wheel
  TimerWheel ->
  -- | The delay before each action is fired
  Seconds ->
  -- | The action to fire repeatedly
  IO () ->
  -- | The timer
  IO (Timer ())
recurring wheel (Nanoseconds.fromSeconds -> delay) action = do
  now <- Timestamp.now
  alarmId <- incrCounter wheel.supply
  canceledRef <- newIORef False
  insertAlarm wheel alarmId (now `Timestamp.plus` delay) (Recurring action delay canceledRef)
  coerce @(IO (IO ())) @(IO (Timer ())) do
    pure do
      mask_ do
        writeIORef canceledRef True
        decrCounter_ wheel.count

-- | Like 'recurring', but for when you don't intend to cancel the timer.
recurring_ ::
  TimerWheel ->
  -- | The delay before each action is fired
  Seconds ->
  -- | The action to fire repeatedly
  IO () ->
  IO ()
recurring_ wheel (Nanoseconds.fromSeconds -> delay) action = do
  now <- Timestamp.now
  alarmId <- incrCounter wheel.supply
  insertAlarm wheel alarmId (now `Timestamp.plus` delay) (Recurring_ action delay)

insertAlarm :: TimerWheel -> AlarmId -> Timestamp -> Alarm -> IO ()
insertAlarm wheel alarmId ringsAt alarm =
  mask_ do
    incrCounter_ wheel.count
    AlarmBuckets.insert wheel.buckets wheel.resolution alarmId ringsAt alarm

------------------------------------------------------------------------------------------------------------------------
-- Timer reaper thread
--
-- The main loop is rather simple, but the code is somewhat fiddly. In brief, the reaper thread wakes up to fire all of
-- the expired timers in bucket N, then sleeps, then wakes up to fire all of the expired timers in bucket N+1, then
-- sleeps, and so on, forever.
--
-- It wakes up on the "bucket boundaries", that is,
--
--   +------+------+------+------+------+------+------+------+------+------+
--   |      |      |      |      |      |      |      |      |      |      |
--   |      |      |      |      |      |      |      |      |      |      |
--   +------+------+------+------+------+------+------+------+------+------+
--                           ^   ^
--                           |   we wake up around here
--                           |
--                           to fire all of the expired timers stored here
--
-- It's entirely possible the reaper thread gets hopelessly behind, that is, it's taken so long to expire all of the
-- timers in previous buckets that we're behind schedule an entire bucket or more. That might look like this:
--
--   +------+------+------+------+------+------+------+------+------+------+
--   |      |      |      |      |      |      |      |      |      |      |
--   |      |      |      |      |      |      |      |      |      |      |
--   +------+------+------+------+------+------+------+------+------+------+
--                           ^                    ^
--                           |                    we are very behind, and enter the loop around here
--                           |
--                           yet we nonetheless fire all of the expired timers stored here, as if we were on time
--
-- That's accomplished simplly by maintaining in the loop state the "ideal" time that we wake up, ignoring reality. We
-- only ultimately check the *actual* current time when determining how long to *sleep* after expiring all of the timers
-- in the current bucket. If we're behind schedule, we won't sleep at all.
--
--   +------+------+------+------+------+------+------+------+------+------+
--   |      |      |      |      |      |      |      |      |      |      |
--   |      |      |      |      |      |      |      |      |      |      |
--   +------+------+------+------+------+------+------+------+------+------+
--                           ^   ^                  ^
--                           |   |                  |
--                           |   we enter the loop with this "ideal" time
--                           |                      |
--                           to fire timers in here |
--                                                  |
--                                                  not caring how far ahead the actual current time is
--
-- On to expiring timers: a "bucket" of timers is stored at each array index, which can be partitioned into "expired"
-- (meant to fire at or before the ideal time) and "not expired" (to expire on a subsequent wrap around the bucket
-- array).
--
--   +-----------------------+
--   |           /           |
--   | expired  /            |
--   |         / not expired |
--   |        /              |
--   +-----------------------+
--
-- The reaper thread simply atomically partitions the bucket, keeping the expired collection for itself, and putting the
-- not-expired collection back in the array.
--
-- Next, the timers are carefully fired one-by-one, in timestamp order. It's possible that two or more timers are
-- scheduled to expire concurrently (i.e. on the same nanosecond); that's fine: we fire them in the order they were
-- scheduled.
--
-- Let's say this is our set of timers to fire.
--
--    Ideal time         Timers to fire
--   +--------------+   +-----------------------------+
--   | 700          |   | Expiry | Type               |
--   +--------------+   +--------+--------------------+
--                      | 630    | One-shot           |
--    Next ideal time   | 643    | Recurring every 10 |
--   +--------------+   | 643    | One-shot           |
--   | 800          |   | 689    | Recurring every 80 |
--   +--------------+   +--------+--------------------+
--
-- Expiring a one-shot timer is simple: call the IO action and move on.
--
-- Expiring a recurring timer is less simple (but still simple): call the IO action, then schedule the next occurrence.
-- There are two possibilities.
--
--   1. The next occurrence is *at or before* the ideal time, which means it ought to fire along with the other timers
--      in the queue, right now. So, insert it into the collection of timers to fire.
--
--   2. The next occurrence is *after* the ideal time, so enqueue it in the array of buckets wherever it belongs.
--
-- After all expired timers are fired, the reaper thread has one last decision to make: how long should we sleep? We
-- get the current timestamp, and if it's still before the next ideal time (i.e. the current ideal time plus the wheel
-- resolution), then we sleep for the difference.
--
-- If the actual time is at or after the next ideal time, that's kind of bad - it means the reaper thread is behind
-- schedule. The user's enqueued actions have taken too long, or their wheel resolution is too short. Anyway, it's not
-- our problem, our behavior doesn't change per whether we are behind schedule or not.
runTimerReaperThread :: AlarmBuckets -> Nanoseconds -> IO v
runTimerReaperThread buckets resolution = do
  -- Sleep until the very first bucket of timers expires
  --
  --     resolution                         = 100
  --     now                                = 184070
  --     progress   = now % resolution      = 70
  --     remaining  = resolution - progress = 30
  --     idealTime  = now + remaining       = 184100
  --
  --   +-------------------------+----------------+---------
  --   | progress = 70           | remaining = 30 |
  --   +-------------------------+----------------+
  --   | resolution = 100                         |
  --   +------------------------------------------+---------
  --                             ^                ^
  --                             now              idealTime
  now <- Timestamp.now
  let progress = now `Timestamp.intoEpoch` resolution
  let remaining = resolution `Nanoseconds.unsafeMinus` progress
  Nanoseconds.sleep remaining
  -- Enter the Loop™
  let idealTime = now `Timestamp.plus` remaining
  theLoop idealTime (AlarmBuckets.timestampToIndex buckets resolution now)
  where
    -- `index` could be derived from `thisTime`, but it's cheaper to just store it separately and bump by 1 as we go
    theLoop :: Timestamp -> Int -> IO v
    theLoop !idealTime !index = do
      expired <- AlarmBuckets.deleteExpiredAt buckets index idealTime
      fireBucket expired

      now <- Timestamp.now
      let !nextIdealTime = idealTime `Timestamp.plus` resolution
      when (nextIdealTime > now) (Nanoseconds.sleep (nextIdealTime `Timestamp.unsafeMinus` now))

      theLoop nextIdealTime ((index + 1) `rem` Array.sizeofMutableArray buckets)
      where
        fireBucket :: Bucket Alarm -> IO ()
        fireBucket bucket0 =
          case Bucket.pop bucket0 of
            Bucket.PopNada -> pure ()
            Bucket.PopAlgo alarmId ringsAt timer bucket1 -> do
              expired <- fireAlarm bucket1 alarmId ringsAt timer
              fireBucket expired

        fireAlarm :: Bucket Alarm -> AlarmId -> Timestamp -> Alarm -> IO (Bucket Alarm)
        fireAlarm bucket alarmId ringsAt alarm =
          case alarm of
            OneShot action -> do
              action
              pure bucket
            Recurring action delay canceledRef ->
              readIORef canceledRef >>= \case
                True -> pure bucket
                False -> fireRecurring action delay
            Recurring_ action delay -> fireRecurring action delay
          where
            fireRecurring :: IO () -> Nanoseconds -> IO (Bucket Alarm)
            fireRecurring action delay = do
              action
              let ringsAtNext = ringsAt `Timestamp.plus` delay
              if ringsAtNext < idealTime
                then pure $! Bucket.insert alarmId ringsAtNext alarm bucket
                else do
                  AlarmBuckets.insert buckets resolution alarmId ringsAtNext alarm
                  pure bucket
