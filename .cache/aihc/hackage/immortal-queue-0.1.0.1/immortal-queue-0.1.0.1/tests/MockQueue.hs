{-# LANGUAGE LambdaCase #-}
module MockQueue where

import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.STM
import           Control.Immortal.Queue


data Task
    = Log Integer Int
    -- ^ Always succeeds with Integer after Int milliseconds
    | Fail String
    -- ^ Always fails

queueConfig :: TVar ([Integer], [String]) -> TQueue Task -> ImmortalQueue Task
queueConfig output q = ImmortalQueue { qThreadCount = 2
                                     , qPollWorkerTime = 200
                                     , qPop = atomically $ readTQueue q
                                     , qPush = atomically . writeTQueue q
                                     , qHandler = performTask
                                     , qFailure = handleError
                                     }
  where
    performTask = \case
        Log i t -> threadDelay (1000 * t) >> atomically (addSuccess i)
        Fail _  -> error "failed"

    handleError t _ = atomically $ case t of
        Log _ _ -> return ()
        Fail s  -> addFailure s

    addSuccess i = modifyTVar output $ \(s, f) -> (s ++ [i], f)
    addFailure m = modifyTVar output $ \(s, f) -> (s, f ++ [m])


-- | Run pool that processes all the given tasks, splitting successes and
-- failures.
runPool :: [Task] -> IO ([Integer], [String])
runPool = runPool_ True Nothing


-- | Run a pool that processes the given tasks. `cleanClose` indicates if the
-- pool should be closed cleanly and `waitTime` will wait the specified
-- time before closing/killing or wait until the queue is empty if Nothing.
runPool_ :: Bool -> Maybe Int -> [Task] -> IO ([Integer], [String])
runPool_ cleanClose waitTime tasks = do
    output  <- newTVarIO ([], [])
    tqueue  <- newTQueueIO
    workers <- processImmortalQueue $ queueConfig output tqueue
    atomically $ mapM_ (writeTQueue tqueue) tasks
    maybe (waitEmpty tqueue) (threadDelay . (* 1000)) waitTime
    if cleanClose then closeImmortalQueue workers else killImmortalQueue workers
    readTVarIO output


-- | Wait until the given queue is empty
waitEmpty :: TQueue a -> IO ()
waitEmpty q = do
    isEmpty <- atomically $ isEmptyTQueue q
    if isEmpty then return () else threadDelay 1000000 >> waitEmpty q
