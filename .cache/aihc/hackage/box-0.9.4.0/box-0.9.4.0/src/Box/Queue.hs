-- | STM Queues, based originally on [pipes-concurrency](https://hackage.haskell.org/package/pipes-concurrency)
module Box.Queue
  ( Queue (..),
    queueL,
    queueR,
    queue,
    fromAction,
    emitQ,
    commitQ,
    fromActionWith,
    toBoxM,
    toBoxSTM,
    concurrentlyLeft,
    concurrentlyRight,
  )
where

import Box.Box
import Box.Codensity
import Box.Committer
import Box.Emitter
import Box.Functor
import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Catch as C
import Prelude

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Box
-- >>> import Prelude

-- | 'Queue' specifies how messages are queued
data Queue a
  = Unbounded
  | Bounded Int
  | Single
  | Latest a
  | Newest Int
  | New

-- | create a queue, supplying the ends and a sealing function.
ends :: Queue a -> STM (a -> STM (), STM a)
ends qu =
  case qu of
    Bounded n -> do
      q <- newTBQueue (fromIntegral n)
      pure (writeTBQueue q, readTBQueue q)
    Unbounded -> do
      q <- newTQueue
      pure (writeTQueue q, readTQueue q)
    Single -> do
      m <- newEmptyTMVar
      pure (putTMVar m, takeTMVar m)
    Latest a -> do
      t <- newTVar a
      pure (writeTVar t, readTVar t)
    New -> do
      m <- newEmptyTMVar
      pure (\x -> tryTakeTMVar m *> putTMVar m x, takeTMVar m)
    Newest n -> do
      q <- newTBQueue (fromIntegral n)
      let write x = writeTBQueue q x <|> (tryReadTBQueue q *> write x)
      pure (write, readTBQueue q)

-- | write to a queue, checking the seal
writeCheck :: TVar Bool -> (a -> STM ()) -> a -> STM Bool
writeCheck sealed i a = do
  b <- readTVar sealed
  if b
    then pure False
    else do
      i a
      pure True

-- | read from a queue, and retry if not sealed
readCheck :: TVar Bool -> STM a -> STM (Maybe a)
readCheck sealed o =
  (Just <$> o)
    <|> ( do
            b <- readTVar sealed
            check b
            pure Nothing
        )

-- | turn a queue into a box (and a seal)
toBoxSTM ::
  Queue a ->
  STM (Box STM a a, STM ())
toBoxSTM q = do
  (i, o) <- ends q
  sealed <- newTVar False
  let seal = writeTVar sealed True
  pure
    ( Box
        (Committer (writeCheck sealed i))
        (Emitter (readCheck sealed o)),
      seal
    )

-- | turn a queue into a box (and a seal), and lift from stm to the underlying monad.
toBoxM ::
  Queue a ->
  IO (Box IO a a, IO ())
toBoxM q = do
  (b, s) <- atomically $ toBoxSTM q
  pure (liftB b, atomically s)

-- | run two actions concurrently, but wait and return on the left result.
concurrentlyLeft :: IO a -> IO b -> IO a
concurrentlyLeft left right =
  withAsync left $ \a ->
    withAsync right $ \_ ->
      wait a

-- | run two actions concurrently, but wait and return on the right result.
concurrentlyRight :: IO a -> IO b -> IO b
concurrentlyRight left right =
  withAsync left $ \_ ->
    withAsync right $ \b ->
      wait b

-- | connect a committer and emitter action via spawning a queue, and wait for the Committer action to complete.
withQL ::
  Queue a ->
  (Queue a -> IO (Box IO a a, IO ())) ->
  (Committer IO a -> IO l) ->
  (Emitter IO a -> IO r) ->
  IO l
withQL q spawner cio eio =
  bracket
    (spawner q)
    snd
    ( \(box, seal) ->
        concurrentlyLeft
          (cio (committer box) `finally` seal)
          (eio (emitter box) `finally` seal)
    )

-- | connect a committer and emitter action via spawning a queue, and wait for the Emitter action to complete.
withQR ::
  Queue a ->
  (Queue a -> IO (Box IO a a, IO ())) ->
  (Committer IO a -> IO l) ->
  (Emitter IO a -> IO r) ->
  IO r
withQR q spawner cio eio =
  bracket
    (spawner q)
    snd
    ( \(box, seal) ->
        concurrentlyRight
          (cio (committer box) `finally` seal)
          (eio (emitter box) `finally` seal)
    )

-- | connect a committer and emitter action via spawning a queue, and wait for both to complete.
withQ ::
  Queue a ->
  (Queue a -> IO (Box IO a a, IO ())) ->
  (Committer IO a -> IO l) ->
  (Emitter IO a -> IO r) ->
  IO (l, r)
withQ q spawner cio eio =
  bracket
    (spawner q)
    snd
    ( \(box, seal) ->
        concurrently
          (cio (committer box) `finally` seal)
          (eio (emitter box) `finally` seal)
    )

-- | Create an unbounded queue, returning the result from the Committer action.
queueL ::
  Queue a ->
  (Committer IO a -> IO l) ->
  (Emitter IO a -> IO r) ->
  IO l
queueL q cm em = withQL q toBoxM cm em

-- | Create an unbounded queue, returning the result from the Emitter action.
queueR ::
  Queue a ->
  (Committer IO a -> IO l) ->
  (Emitter IO a -> IO r) ->
  IO r
queueR q cm em = withQR q toBoxM cm em

-- | Create an unbounded queue, returning both results.
--
-- >>> queue Unbounded (\c -> glue c <$|> qList [1..3]) toListM
-- ((),[1,2,3])
queue ::
  Queue a ->
  (Committer IO a -> IO l) ->
  (Emitter IO a -> IO r) ->
  IO (l, r)
queue q cm em = withQ q toBoxM cm em

-- | lift a box from STM
liftB :: Box STM a b -> Box IO a b
liftB (Box c e) = Box (foist atomically c) (foist atomically e)

-- | Turn a box action into a box continuation
fromAction :: (Box IO a b -> IO r) -> CoBox IO b a
fromAction baction = Codensity $ fuseActions baction

-- | Turn a box action into a box continuation
fromActionWith :: Queue a -> Queue b -> (Box IO a b -> IO r) -> CoBox IO b a
fromActionWith qa qb baction = Codensity $ fuseActionsWith qa qb baction

-- | Connect up two box actions via two Unbounded queues
fuseActions :: (Box IO a b -> IO r) -> (Box IO b a -> IO r') -> IO r'
fuseActions abm bam = do
  (Box ca ea, _) <- toBoxM Unbounded
  (Box cb eb, _) <- toBoxM Unbounded
  concurrentlyRight (abm (Box ca eb)) (bam (Box cb ea))

-- | Connect up two box actions via two queues
fuseActionsWith :: Queue a -> Queue b -> (Box IO a b -> IO r) -> (Box IO b a -> IO r') -> IO r'
fuseActionsWith qa qb abm bam = do
  (Box ca ea, _) <- toBoxM qa
  (Box cb eb, _) <- toBoxM qb
  concurrentlyRight (abm (Box ca eb)) (bam (Box cb ea))

-- | Hook a committer action to a queue, creating an emitter continuation.
emitQ :: Queue a -> (Committer IO a -> IO r) -> CoEmitter IO a
emitQ q cio = Codensity $ \eio -> queueR q cio eio

-- | Hook a committer action to a queue, creating an emitter continuation.
commitQ :: Queue a -> (Emitter IO a -> IO r) -> CoCommitter IO a
commitQ q eio = Codensity $ \cio -> queueL q cio eio
