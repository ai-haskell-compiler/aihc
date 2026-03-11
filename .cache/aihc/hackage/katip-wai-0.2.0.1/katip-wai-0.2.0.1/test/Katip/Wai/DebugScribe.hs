module Katip.Wai.DebugScribe
  ( withDebugScribe
  )
where

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import Control.Monad (void)
import qualified Data.Aeson as Aeson
import qualified Katip


-- | Collect all logs in memory and return a list of the json encoded logs when closed.
withDebugScribe :: Katip.PermitFunc -> Katip.Verbosity -> (Katip.Scribe -> IO a) -> IO [Aeson.Value]
withDebugScribe permitFunc verbosity useScribe = do
  queue <- STM.newTQueueIO

  let push logItem =
        let event = Just $ Katip.itemJson verbosity logItem
         in STM.atomically (STM.writeTQueue queue event)

  allLogsVar <- MVar.newEmptyMVar

  let finalizer = do
        STM.atomically $ STM.writeTQueue queue Nothing
        void $ MVar.readMVar allLogsVar

  let handleEvents logs = do
        event <- STM.atomically $ STM.readTQueue queue
        case event of
          Just logValue -> handleEvents (logValue : logs)
          Nothing -> MVar.putMVar allLogsVar $ reverse logs

  let scribe =
        Katip.Scribe
          { Katip.liPush = push
          , Katip.scribeFinalizer = finalizer
          , Katip.scribePermitItem = permitFunc
          }

  Async.withAsync (handleEvents []) $ \eventHandler -> do
    _ <- useScribe scribe
    Async.wait eventHandler
    MVar.readMVar allLogsVar
