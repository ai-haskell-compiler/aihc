{-# LANGUAGE
    RankNTypes
  , ScopedTypeVariables
  , NamedFieldPuns
  , FlexibleContexts
  #-}

module Test.WebSockets.Simple where

import Network.WebSockets.Simple (WebSocketsApp (..), WebSocketsAppParams (..), ConnectionException (..), CloseOrigin (..))
import Data.Singleton.Class (Extractable (..))
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Control.Aligned (MonadBaseControl (..))
import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChan, writeTChan, readTChan)



-- | Runs two 'WebSocketsApp's together in a forged channel.
runConnected :: forall send receive m stM
              . ( MonadIO m
                , MonadBaseControl IO m stM
                , Extractable stM
                )
             => WebSocketsApp m receive send
             -> WebSocketsApp m send receive
             -> m (Async (), Async (), TChan send, TChan receive)
runConnected sendsSreceivesR sendsRreceivesS = do
  (sendChan, receiveChan) <- liftIO $ atomically $ (,) <$> newTChan <*> newTChan

  let sendToSend :: send -> m ()
      sendToSend s = liftIO $ atomically $ writeTChan sendChan s

      sendToReceive :: receive -> m ()
      sendToReceive r = liftIO $ atomically $ writeTChan receiveChan r

      close :: m ()
      close = do
        onClose sendsRreceivesS ClosedOnClose ConnectionClosed
        onClose sendsSreceivesR ClosedOnClose ConnectionClosed

  sToR <- liftBaseWith $ \runInBase -> async $ forever $ do
    s <- atomically $ readTChan sendChan
    fmap runSingleton $ runInBase $ onReceive sendsRreceivesS WebSocketsAppParams
      { send = sendToReceive
      , close
      } s

  rToS <- liftBaseWith $ \runInBase -> async $ forever $ do
    r <- atomically $ readTChan receiveChan
    fmap runSingleton $ runInBase $ onReceive sendsSreceivesR WebSocketsAppParams
      { send = sendToSend
      , close
      } r

  onOpen sendsRreceivesS WebSocketsAppParams
    { send = sendToReceive
    , close
    }

  onOpen sendsSreceivesR WebSocketsAppParams
    { send = sendToSend
    , close
    }

  pure (sToR,rToS,sendChan,receiveChan)
