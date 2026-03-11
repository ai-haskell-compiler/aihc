{-# LANGUAGE OverloadedStrings #-}

-- | Websocket components built with 'Box'es.
module Box.Websocket
  ( SocketConfig (..),
    defaultSocketConfig,
    connect,
    serve,
    pending,
    serverApp,
    receiver,
    receiver_,
    sender,
    sender_,
    duplex,
    duplex_,
    clientBox,
    clientCoBox,
    serverBox,
    serverCoBox,
    responseServer,
  )
where

import Box
import Box.Socket.Types
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.ByteString qualified as BS
import Data.Functor.Contravariant
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import Network.WebSockets

-- | Socket configuration
--
-- >>> defaultSocketConfig
-- SocketConfig {host = "127.0.0.1", port = 9160, path = "/"}
data SocketConfig = SocketConfig
  { host :: Text,
    port :: Int,
    path :: Text
  }
  deriving (Show, Eq, Generic)

-- | official default
defaultSocketConfig :: SocketConfig
defaultSocketConfig = SocketConfig "127.0.0.1" 9160 "/"

-- | connect an action (ie a client)
connect :: SocketConfig -> Codensity IO Connection
connect c = Codensity $ \action ->
  runClient (unpack $ host c) (port c) (unpack $ path c) action

-- | serve an action (ie a server)
serve :: SocketConfig -> Codensity IO Connection
serve c =
  Codensity $
    runServerWithOptions (defaultServerOptions {serverHost = unpack (host c), serverPort = port c}) . upgrade
  where
    upgrade action p = void $ action <$|> pending p

-- | Attach a box to a 'PendingConnection' in wai-style.
serverApp ::
  Box IO Text Text ->
  PendingConnection ->
  IO ()
serverApp b p = upgrade (duplex (CloseAfter 0.2) b) p
  where
    upgrade action p' = void $ action <$|> pending p'

-- | Given a 'PendingConnection', provide a 'Connection' continuation.
pending :: PendingConnection -> Codensity IO Connection
pending p = Codensity $ \action ->
  bracket
    (acceptRequest p)
    (\_ -> pure ())
    ( \conn ->
        withAsync
          (forever $ sendPing conn ("connect ping" :: BS.ByteString) >> sleep 30)
          (\_ -> action conn)
    )

-- | Commit received messages, finalising on receiving a 'CloseRequest'
receiver ::
  (WebSocketsData a) =>
  Committer IO a ->
  Connection ->
  IO ()
receiver c conn = go
  where
    go = do
      msg <- try (receiveData conn)
      case msg of
        Left (CloseRequest _ _) -> pure ()
        Left err -> throwIO err
        Right msg' -> commit c msg' >> go

-- | Commit received messages, finalising on receiving a 'CloseRequest', with event logging.
receiver_ ::
  (WebSocketsData a, Show a) =>
  Committer IO a ->
  Committer IO Text ->
  Connection ->
  IO ()
receiver_ c cLog conn = go
  where
    go = do
      msg <- try (receiveData conn)
      _ <- commit cLog ("receiveData:" <> pack (show msg))
      case msg of
        Left (CloseRequest _ _) -> pure ()
        Left err -> throwIO err
        Right msg' -> commit c msg' >> go

-- | Send emitted messages, returning whether the socket remained open (the 'Emitter' ran out of emits) or closed (a 'CloseRequest' was received).
sender ::
  (WebSocketsData a) =>
  Emitter IO a ->
  Connection ->
  IO SocketStatus
sender e conn = go
  where
    go = do
      msg <- emit e
      case msg of
        Nothing -> pure SocketOpen
        Just msg' -> do
          ok <- try (sendTextData conn msg')
          case ok of
            Left (CloseRequest _ _) -> pure SocketClosed
            Left err -> throwIO err
            Right () -> go

-- | Send emitted messages, returning whether the socket remained open (the 'Emitter' ran out of emits) or closed (a 'CloseRequest' was received). With event logging.
sender_ ::
  (WebSocketsData a, Show a) =>
  Emitter IO a ->
  Committer IO Text ->
  Connection ->
  IO SocketStatus
sender_ e cLog conn = go
  where
    go = do
      msg <- emit e
      _ <- commit cLog ("emit:" <> pack (show msg))
      case msg of
        Nothing -> pure SocketOpen
        Just msg' -> do
          ok <- try (sendTextData conn msg')
          _ <- commit cLog ("sendTextData:" <> pack (show ok))
          case ok of
            Left (CloseRequest _ _) -> pure SocketClosed
            Left err -> throwIO err
            Right () -> go

-- | A two-way connection. Closes if it receives a 'CloseRequest' exception, or if 'PostSend' is 'CloseAfter'.
duplex ::
  (WebSocketsData a) =>
  PostSend ->
  Box IO a a ->
  Connection ->
  IO ()
duplex ps (Box c e) conn = do
  concurrentlyRight
    ( do
        status <- sender e conn
        case (ps, status) of
          (CloseAfter s, SocketOpen) -> do
            sleep s
            sendClose conn ("close after sending" :: Text)
          _ -> pure ()
    )
    (receiver c conn)

-- | A two-way connection. Closes if it receives a 'CloseRequest' exception, or if 'PostSend' is 'CloseAfter'. With event logging.
duplex_ ::
  (WebSocketsData a, Show a) =>
  PostSend ->
  Committer IO Text ->
  Box IO a a ->
  Connection ->
  IO ()
duplex_ ps cLog (Box c e) conn = do
  concurrentlyRight
    ( do
        status <- sender_ e (contramap ("sender_:" <>) cLog) conn
        _ <- commit cLog ("sender_ closed with " <> pack (show status))
        case (ps, status) of
          (CloseAfter s, SocketOpen) -> do
            sleep s
            sendClose conn ("close after sending" :: Text)
          _ -> pure ()
    )
    ( do
        receiver_ c (contramap ("receiver_:" <>) cLog) conn
        void $ commit cLog "receiver_ closed"
    )
  void $ commit cLog "duplex_ closed"

-- | A 'Box' action for a client.
clientBox ::
  (WebSocketsData a) =>
  SocketConfig ->
  PostSend ->
  Box IO a a ->
  IO ()
clientBox cfg ps b = duplex ps b <$|> connect cfg

-- | A client 'CoBox'.
clientCoBox ::
  (WebSocketsData a) =>
  SocketConfig ->
  PostSend ->
  CoBox IO a a
clientCoBox cfg ps = fromAction (clientBox cfg ps)

-- | A 'Box' action for a server.
serverBox ::
  (WebSocketsData a) =>
  SocketConfig ->
  PostSend ->
  Box IO a a ->
  IO ()
serverBox cfg ps b = duplex ps b <$|> serve cfg

-- | A server 'CoBox'.
serverCoBox ::
  (WebSocketsData a) =>
  SocketConfig ->
  PostSend ->
  CoBox IO a a
serverCoBox cfg ps = fromAction (serverBox cfg ps)

-- | A receiver that applies a response function to received messages.
responseServer :: (WebSocketsData a) => SocketConfig -> (a -> Maybe a) -> IO ()
responseServer cfg f = fuse (pure . f) <$|> serverCoBox cfg (CloseAfter 0.5)
