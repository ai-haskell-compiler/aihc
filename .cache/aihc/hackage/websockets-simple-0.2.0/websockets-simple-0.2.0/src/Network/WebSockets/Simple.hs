{-# LANGUAGE
    DeriveGeneric
  , DeriveDataTypeable
  , RankNTypes
  , ScopedTypeVariables
  , NamedFieldPuns
  , FlexibleContexts
  , InstanceSigs
  , OverloadedStrings
  #-}

module Network.WebSockets.Simple
  ( -- * Types
    WebSocketsApp (..), WebSocketsAppParams (..)
  , Network.WebSockets.ConnectionException (..), CloseOrigin (..), WebSocketsSimpleError (..)
  , -- * Running
    dimap', dimapJson, dimapStringify
  , toClientAppT, toClientAppTString, toClientAppTBinary, toClientAppTBoth
  , accept
  , -- * Utilities
    expBackoffStrategy
  , hoistWebSocketsApp
  ) where

import Network.WebSockets
  ( DataMessage (..), sendTextData, sendBinaryData
  , sendClose, receiveDataMessage, acceptRequest, ConnectionException (..))
import Network.WebSockets.Trans (ServerAppT, ClientAppT)
import Data.Profunctor (Profunctor (..))
import Data.Aeson (ToJSON (..), FromJSON (..), Value)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as LT
import Data.ByteString.Lazy (ByteString)
import Data.Singleton.Class (Extractable (..))

import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Catch (Exception, MonadThrow, catch, throwM, MonadCatch)
import Control.Monad.Trans.Control.Aligned (MonadBaseControl (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, newTVarIO, readTVarIO, writeTVar)

import GHC.Generics (Generic)
import Data.Typeable (Typeable)



data WebSocketsAppParams m send = WebSocketsAppParams
  { send  :: send -> m ()
  , close :: m ()
  } deriving (Generic, Typeable)


data WebSocketsApp m receive send = WebSocketsApp
  { onOpen    :: WebSocketsAppParams m send -> m ()
  , onReceive :: WebSocketsAppParams m send -> receive -> m ()
  , onClose   :: CloseOrigin -> ConnectionException -> m () -- ^ Should be re-entrant; this exception is caught in all uses of 'send', even if used in a dead 'Network.WebSockets.Connection' in a lingering thread.
  } deriving (Generic, Typeable)

data CloseOrigin
  = ClosedOnSend
  | ClosedOnClose
  | ClosedOnReceive
  deriving (Eq, Show)


instance Profunctor (WebSocketsApp m) where
  dimap :: forall a b c d. (a -> b) -> (c -> d) -> WebSocketsApp m b c -> WebSocketsApp m a d
  dimap receiveF sendF WebSocketsApp{onOpen,onReceive,onClose} = WebSocketsApp
    { onOpen = onOpen . getParams
    , onReceive = \params -> onReceive (getParams params) . receiveF
    , onClose = onClose
    }
    where
      getParams :: WebSocketsAppParams m d -> WebSocketsAppParams m c
      getParams WebSocketsAppParams{send,close} = WebSocketsAppParams{send = send . sendF,close}


hoistWebSocketsApp :: (forall a. m a -> n a)
                   -> (forall a. n a -> m a)
                   -> WebSocketsApp m receive send
                   -> WebSocketsApp n receive send
hoistWebSocketsApp f coF WebSocketsApp{onOpen,onReceive,onClose} = WebSocketsApp
  { onOpen = \WebSocketsAppParams{send,close} -> f (onOpen WebSocketsAppParams{send = coF . send, close = coF close})
  , onReceive = \WebSocketsAppParams{send,close} r -> f (onReceive WebSocketsAppParams{send = coF . send, close = coF close} r)
  , onClose = \o e -> f (onClose o e)
  }


instance Applicative m => Semigroup (WebSocketsApp m receive send) where
  x <> y = WebSocketsApp
    { onOpen = \params -> onOpen x params *> onOpen y params
    , onReceive = \params r -> onReceive x params r *> onReceive y params r
    , onClose = \o mE -> onClose x o mE *> onClose y o mE
    }



instance Applicative m => Monoid (WebSocketsApp m receive send) where
  mempty = WebSocketsApp
    { onOpen = \_ -> pure ()
    , onReceive = \_ _ -> pure ()
    , onClose = \_ _ -> pure ()
    }


dimap' :: Monad m
       => (receive' -> m receive)
       -> (send -> send')
       -> WebSocketsApp m receive send
       -> WebSocketsApp m receive' send'
dimap' from to WebSocketsApp{onReceive,onOpen,onClose} = WebSocketsApp
  { onOpen = onOpen . newParams
  , onClose
  , onReceive = \params r -> from r >>= onReceive (newParams params)
  }
  where
    newParams WebSocketsAppParams{send,close} = WebSocketsAppParams
      { send = send . to
      , close
      }


dimapJson :: ToJSON send
          => FromJSON receive
          => MonadThrow m
          => WebSocketsApp m receive send
          -> WebSocketsApp m Value Value
dimapJson = dimap' fromJson toJSON
  where
    fromJson x = case Aeson.parseMaybe parseJSON x of
      Nothing -> throwM (JSONValueError x)
      Just y -> pure y


dimapStringify :: MonadThrow m
               => WebSocketsApp m Value Value
               -> WebSocketsApp m Text Text
dimapStringify = dimap' fromJson (LT.decodeUtf8 . Aeson.encode)
  where
    fromJson x = case Aeson.decode (LT.encodeUtf8 x) of
      Nothing -> throwM (JSONParseError x)
      Just y -> pure y



-- | This can throw a 'WebSocketSimpleError' to the main thread via 'Control.Concurrent.Async.link' when json parsing fails.
toClientAppT :: forall send receive m stM
              . ToJSON send
             => FromJSON receive
             => MonadIO m
             => MonadBaseControl IO m stM
             => MonadThrow m
             => MonadCatch m
             => Extractable stM
             => WebSocketsApp m receive send
             -> ClientAppT m () -- WebSocketsAppThreads
toClientAppT = toClientAppTString . dimapStringify . dimapJson


toClientAppTString :: MonadBaseControl IO m stM
                   => Extractable stM
                   => MonadCatch m
                   => MonadIO m
                   => WebSocketsApp m Text Text
                   -> ClientAppT m ()
toClientAppTString x = toClientAppTBoth x mempty



toClientAppTBinary :: MonadBaseControl IO m stM
                   => Extractable stM
                   => MonadCatch m
                   => MonadIO m
                   => WebSocketsApp m ByteString ByteString
                   -> ClientAppT m ()
toClientAppTBinary = toClientAppTBoth mempty



toClientAppTBoth :: forall m stM
                  . MonadBaseControl IO m stM
                 => Extractable stM
                 => MonadCatch m
                 => MonadIO m
                 => WebSocketsApp m Text Text
                 -> WebSocketsApp m ByteString ByteString
                 -> ClientAppT m ()
toClientAppTBoth textApp binApp conn = do
  let sendText :: Text -> m ()
      sendText x = liftIO (sendTextData conn x) `catch` onClose textApp ClosedOnSend
      sendBin :: ByteString -> m ()
      sendBin x = liftIO (sendBinaryData conn x) `catch` onClose binApp ClosedOnSend
      catchBoth :: ConnectionException -> m ()
      catchBoth e = do
        onClose textApp ClosedOnReceive e
        onClose binApp ClosedOnReceive e

      close :: m ()
      close = liftIO (sendClose conn ("requesting close" :: ByteString)) `catch` catchBoth

      paramsText :: WebSocketsAppParams m Text
      paramsText = WebSocketsAppParams{send=sendText,close}
      paramsBin :: WebSocketsAppParams m ByteString
      paramsBin = WebSocketsAppParams{send=sendBin,close}

  onOpen textApp paramsText
  onOpen binApp paramsBin

  liftBaseWith $ \runInBase ->
    let runM :: forall a. m a -> IO a
        runM = fmap runSingleton . runInBase

        go' :: IO ()
        go' = forever $ do
          data' <- receiveDataMessage conn
          case data' of
            Text xs mt ->
              let t = fromMaybe (LT.decodeUtf8 xs) mt
              in  runM (onReceive textApp paramsText t)
            Binary xs ->
              runM (onReceive binApp paramsBin xs)
    in  go' `catch` (runM . catchBoth)



accept :: MonadIO m
       => ClientAppT m () -> ServerAppT m
accept wsApp pending =
  liftIO (acceptRequest pending) >>= wsApp



-- | A simple backoff strategy, which (per second), will increasingly delay at @2^soFar@, until @soFar >= 5minutes@, where it will then routinely poll every
--   5 minutes.
expBackoffStrategy :: forall m a
                    . ( MonadIO m
                      )
                   => m a -- ^ Action to call, like pinging a scoped channel to trigger the reconnect
                   -> m (ConnectionException -> m a)
expBackoffStrategy action = do
  soFarVar <- liftIO $ newTVarIO (0 :: Int)

  let second = 1000000

  pure $ \_ -> do
    liftIO $ do
      soFar <- readTVarIO soFarVar
      let delay
            | soFar >= 5 * 60 = 5 * 60
            | otherwise       = 2 ^ soFar
      atomically $ writeTVar soFarVar (soFar + delay)
      threadDelay (delay * second)
    action



data WebSocketsSimpleError
  = JSONParseError Text
  | JSONValueError Value
  deriving (Generic, Eq, Show)

instance Exception WebSocketsSimpleError

