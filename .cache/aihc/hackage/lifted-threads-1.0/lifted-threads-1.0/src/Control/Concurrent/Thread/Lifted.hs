{-# LANGUAGE FlexibleContexts, RankNTypes #-}
-- | This is a wrapped version of "Control.Concurrent.Thread" with
-- types generalised from 'IO' to all monads in either 'MonadBase'
-- or 'MonadBaseControl'.
module Control.Concurrent.Thread.Lifted (
  -- * Forking threads
    fork
  , forkOS
  , forkOn
  , forkWithUnmask
  , forkOnWithUnmask
  -- * Results
  , Result
  , result
  ) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.Thread (Result)
import Control.Monad.Base
import Control.Monad.Trans.Control
import qualified Control.Concurrent.Thread as T

-- | Generalized version of 'T.forkIO'.
fork :: MonadBaseControl IO m
     => m a
     -> m (ThreadId, m (Result a))
fork action = liftBaseWith $ \runInBase -> fixTypes
  =<< T.forkIO (runInBase action)

-- | Generalized version of 'T.forkOS'.
forkOS :: MonadBaseControl IO m
       => m a
       -> m (ThreadId, m (Result a))
forkOS action = liftBaseWith $ \runInBase -> fixTypes
  =<< T.forkOS (runInBase action)

-- | Generalized version of 'T.forkOn'.
forkOn :: MonadBaseControl IO m
       => Int
       -> m a
       -> m (ThreadId, m (Result a))
forkOn i action = liftBaseWith $ \runInBase -> fixTypes
  =<< T.forkOn i (runInBase action)

-- | Generalized version of 'T.forkIOWithUnmask'.
forkWithUnmask :: MonadBaseControl IO m
               => ((forall b. m b -> m b) -> m a)
               -> m (ThreadId, m (Result a))
forkWithUnmask action = liftBaseWith $ \runInBase -> fixTypes
  =<< T.forkIOWithUnmask (\unmask -> runInBase (action (liftBaseOp_ unmask)))

-- | Generalized version of 'T.forkOnWithUnmask'.
forkOnWithUnmask :: MonadBaseControl IO m
                 => Int
                 -> ((forall b. m b -> m b) -> m a)
                 -> m (ThreadId, m (Result a))
forkOnWithUnmask i action = liftBaseWith $ \runInBase -> fixTypes
  =<< T.forkOnWithUnmask i (\unmask -> runInBase (action (liftBaseOp_ unmask)))

-- Generalized version of 'T.result'.
result :: MonadBase IO m => Result a -> m a
result = liftBase . T.result

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
