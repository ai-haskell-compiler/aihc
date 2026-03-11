-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Database.CQL.IO.Jobs
    ( Jobs
    , JobReplaced (..)
    , newJobs
    , runJob
    , runJob_
    , tryRunJob
    , tryRunJob_
    , cancelJobs
    , listJobs
    , listJobKeys
    ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (asyncExceptionFromException, asyncExceptionToException)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.IORef
import Data.Map.Strict (Map)
import Data.Typeable
import Data.Unique

import qualified Data.Map.Strict as Map

-- | A registry for asynchronous computations ("jobs") associated with keys
-- of type @k@, with only a single job running at a time for a particular key.
newtype Jobs k = Jobs (IORef (Map k Job))

-- | Internal representation of a job. The 'Unique' value ensures that
-- a job that finishes or is aborted never accidentily removes a newly
-- registered job for the same key from the 'Jobs' registry.
data Job = Job
    { _jobUniq :: !Unique
    , jobAsync :: !(Async ())
    }

-- | The asynchronous exception used to cancel a job if it is replaced
-- by another job.
data JobReplaced = JobReplaced deriving (Eq, Show, Typeable)
instance Exception JobReplaced where
    toException   = asyncExceptionToException
    fromException = asyncExceptionFromException

newJobs :: MonadIO m => m (Jobs k)
newJobs = liftIO $ Jobs <$> newIORef Map.empty

-- | 'runJob' and ignore the result.
runJob_ :: (MonadIO m, Ord k) => Jobs k -> k -> IO () -> m ()
runJob_ j k = void . runJob j k

-- | Run an asynchronous job for a key. If there is a running job for the same
-- key, it is replaced and cancelled with a 'JobReplaced' exception.
runJob :: (MonadIO m, Ord k) => Jobs k -> k -> IO () -> m (Async ())
runJob j@(Jobs ref) k = runJobWith addJob j k
  where
    addJob new = atomicModifyIORef' ref $ \jobs ->
        let jobs' = Map.insert k new jobs
            old   = Map.lookup k jobs
            val   = jobAsync new
        in (jobs', (True, val, old))

-- | 'tryRunJob' and ignore the result.
tryRunJob_ :: (MonadIO m, Ord k) => Jobs k -> k -> IO () -> m ()
tryRunJob_ j k = void . tryRunJob j k

-- | Try to run an asynchronous job for a key. If there is a running job for
-- the same key, 'Nothing' is returned and the job will not run.
tryRunJob :: (MonadIO m, Ord k) => Jobs k -> k -> IO () -> m (Maybe (Async ()))
tryRunJob j@(Jobs ref) k = runJobWith addJob j k
  where
    addJob new = atomicModifyIORef' ref $ \jobs ->
        if Map.member k jobs
            then (jobs, (False, Nothing, Nothing))
            else
                let jobs' = Map.insert k new jobs
                    val   = Just (jobAsync new)
                in (jobs', (True, val, Nothing))

-- | Cancel all running jobs.
cancelJobs :: MonadIO m => Jobs k -> m ()
cancelJobs (Jobs d) = liftIO $ do
    jobs <- Map.elems <$> atomicModifyIORef' d (\m -> (Map.empty, m))
    mapM_ (cancel . jobAsync) jobs

-- | List all running jobs.
listJobs :: MonadIO m => Jobs k -> m [(k, Async ())]
listJobs (Jobs j) = liftIO $ Map.foldrWithKey f [] <$> readIORef j
  where
    f k a b = (k, jobAsync a) : b

-- | List the keys of all running jobs.
listJobKeys :: MonadIO m => Jobs k -> m [k]
listJobKeys (Jobs j) = liftIO $ Map.keys <$> readIORef j

------------------------------------------------------------------------------
-- Internal

runJobWith :: (MonadIO m, Ord k)
    => (Job -> IO (Bool, a, Maybe Job))
    -> Jobs k
    -> k
    -> IO ()
    -> m a
runJobWith addJob (Jobs ref) k io = liftIO $ do
    u <- newUnique
    l <- newEmptyMVar
    -- Once the async is created and waiting on the latch @l@, it must
    -- either be unblocked by putMVar or cancelled, hence masking
    -- between 'async' and 'run' (nb. 'takeMVar' is interruptible).
    mask $ \restore -> do
        new <- async $ do
            takeMVar l
            restore io
            remove u
          `catches`
            [ Handler $ \x@JobReplaced     -> throwM x
            , Handler $ \x@SomeException{} -> remove u >> throwM x
            ]
        restore (run u l new) `onException` cancel new
  where
    run u l new = do
        (ok, a, old) <- addJob (Job u new)
        mapM_ ((`cancelWith` JobReplaced) . jobAsync) old
        if ok then putMVar l () else cancel new
        return a

    remove u = atomicModifyIORef' ref $ \jobs ->
        let update = Map.update $ \a ->
                        case a of
                            Job u' _ | u == u' -> Nothing
                            _                  -> Just a
        in (update k jobs, ())

