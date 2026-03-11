{-# LANGUAGE FlexibleContexts, RankNTypes #-}
-- | This is a wrapped version of "Control.Concurrent.Thread.Group"
-- with types generalised from 'IO' to all monads in either 'MonadBase'
-- or 'MonadBaseControl'.
module Control.Concurrent.Thread.Group.Lifted (
    TG.ThreadGroup
  , new
  , TG.nrOfRunning
  , wait
  , waitN

  -- * Forking threads
  , fork
  , forkOS
  , forkOn
  , forkWithUnmask
  , forkOnWithUnmask
  ) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.Thread (Result)
import Control.Monad.Base
import Control.Monad.Trans.Control
import qualified Control.Concurrent.Thread.Group as TG

-- | Generalized version of 'TG.new'.
new :: MonadBase IO m => m TG.ThreadGroup
new = liftBase TG.new

-- | Generalized version of 'TG.wait'.
wait :: MonadBase IO m => TG.ThreadGroup -> m ()
wait = liftBase . TG.wait

-- | Generalized version of 'TG.waitN'.
waitN :: MonadBase IO m => Int -> TG.ThreadGroup -> m ()
waitN i = liftBase . TG.waitN i

-- | Generalized version of 'TG.forkIO'.
fork :: MonadBaseControl IO m
     => TG.ThreadGroup
     -> m a
     -> m (ThreadId, m (Result a))
fork tg action = liftBaseWith $ \runInBase -> fixTypes
  =<< TG.forkIO tg (runInBase action)

-- | Generalized version of 'TG.forkOS'.
forkOS :: MonadBaseControl IO m
       => TG.ThreadGroup
       -> m a
       -> m (ThreadId, m (Result a))
forkOS tg action = liftBaseWith $ \runInBase -> fixTypes
  =<< TG.forkOS tg (runInBase action)

-- | Generalized version of 'TG.forkOn'.
forkOn :: MonadBaseControl IO m
       => Int
       -> TG.ThreadGroup
       -> m a
       -> m (ThreadId, m (Result a))
forkOn i tg action = liftBaseWith $ \runInBase -> fixTypes
  =<< TG.forkOn i tg (runInBase action)

-- | Generalized version of 'TG.forkIOWithUnmask'.
forkWithUnmask :: MonadBaseControl IO m
               => TG.ThreadGroup
               -> ((forall b. m b -> m b) -> m a)
               -> m (ThreadId, m (Result a))
forkWithUnmask tg action = liftBaseWith $ \runInBase -> fixTypes
  =<< TG.forkIOWithUnmask tg (\unmask -> runInBase (action (liftBaseOp_ unmask)))

-- | Generalized version of 'TG.forkOnWithUnmask'.
forkOnWithUnmask :: MonadBaseControl IO m
                 => Int
                 -> TG.ThreadGroup
                 -> ((forall b. m b -> m b) -> m a)
                 -> m (ThreadId, m (Result a))
forkOnWithUnmask i tg action = liftBaseWith $ \runInBase -> fixTypes
  =<< TG.forkOnWithUnmask i tg (\unmask -> runInBase (action (liftBaseOp_ unmask)))

----------------------------------------

fixTypes :: MonadBaseControl IO m
         => (ThreadId, IO (Result (StM m a)))
         -> IO (ThreadId, m (Result a))
fixTypes = return . fmap (\c -> liftBase c >>= mapMEither restoreM)
  where
    -- missing instance of Traversable for Either
    mapMEither :: Monad m => (a -> m b) -> Either c a -> m (Either c b)
    mapMEither f (Right r) = f r >>= \v -> return (Right v)
    mapMEither _ (Left v) = return (Left v)
