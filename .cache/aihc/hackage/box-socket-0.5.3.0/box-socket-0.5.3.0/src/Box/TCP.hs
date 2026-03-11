{-# LANGUAGE OverloadedStrings #-}

-- | TCP Boxes.
module Box.TCP
  ( TCPConfig (..),
    defaultTCPConfig,
    TCPEnv (..),
    Socket,
    connect,
    serve,
    receiver,
    sender,
    duplex,
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
import Control.Monad
import Data.ByteString (ByteString)
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Network.Simple.TCP (Socket)
import Network.Simple.TCP qualified as NS

-- | TCP configuration
--
-- >>> defaultTCPConfig
-- TCPConfig {hostPreference = HostAny, host = "127.0.0.1", port = "3566", chunk = 2048, endLine = "\n"}
data TCPConfig = TCPConfig
  { hostPreference :: NS.HostPreference,
    host :: Text,
    port :: Text,
    chunk :: Int,
    endLine :: Text
  }
  deriving (Show, Eq, Generic)

-- | default
defaultTCPConfig :: TCPConfig
defaultTCPConfig = TCPConfig NS.HostAny "127.0.0.1" "3566" 2048 "\n"

-- | An active TCP environment
data TCPEnv = TCPEnv
  { socket :: NS.Socket,
    sockaddr :: NS.SockAddr
  }

-- | connect an action (ie a client)
connect :: TCPConfig -> Codensity IO TCPEnv
connect cfg =
  Codensity $
    NS.connect (unpack $ host cfg) (unpack $ port cfg)
      . (\action (s, a) -> action (TCPEnv s a))

-- | serve an action (ie a server)
serve :: TCPConfig -> Codensity IO TCPEnv
serve cfg =
  Codensity $
    NS.serve (hostPreference cfg) (unpack $ port cfg)
      . (\action (s, a) -> void $ action (TCPEnv s a))

-- | Commit received ByteStrings.
receiver ::
  TCPConfig ->
  Committer IO ByteString ->
  Socket ->
  IO ()
receiver cfg c conn = go
  where
    go = do
      msg <- NS.recv conn (chunk cfg)
      case msg of
        Nothing -> pure ()
        Just bs -> commit c bs >> go

-- | Send emitted ByteStrings.
sender ::
  Emitter IO ByteString ->
  Socket ->
  IO SocketStatus
sender e conn = go
  where
    go = do
      bs <- emit e
      case bs of
        Nothing -> pure SocketOpen
        Just bs' -> NS.send conn bs' >> go

-- | A two-way connection.
duplex ::
  TCPConfig ->
  PostSend ->
  Box IO ByteString ByteString ->
  Socket ->
  IO ()
duplex cfg ps (Box c e) conn = do
  _ <-
    race
      ( do
          status <- sender e conn
          case (ps, status) of
            (CloseAfter s, SocketOpen) -> sleep s
            _ -> pure ()
      )
      (receiver cfg c conn)
  pure ()

-- | A 'Box' action for a client.
clientBox ::
  TCPConfig ->
  PostSend ->
  Box IO ByteString ByteString ->
  IO ()
clientBox cfg ps b = duplex cfg ps b . socket <$|> connect cfg

-- | A client 'CoBox'.
clientCoBox ::
  TCPConfig ->
  PostSend ->
  CoBox IO ByteString ByteString
clientCoBox cfg ps = fromAction (clientBox cfg ps)

-- | A 'Box' action for a server.
serverBox ::
  TCPConfig ->
  PostSend ->
  Box IO ByteString ByteString ->
  IO ()
serverBox cfg ps b = duplex cfg ps b . socket <$|> serve cfg

-- | A server 'CoBox'.
serverCoBox ::
  TCPConfig ->
  PostSend ->
  CoBox IO ByteString ByteString
serverCoBox cfg ps = fromAction (serverBox cfg ps)

-- | A receiver that applies a response function to received ByteStrings.
responseServer :: TCPConfig -> (ByteString -> Maybe ByteString) -> IO ()
responseServer cfg f = fuse (pure . f) <$|> serverCoBox cfg (CloseAfter 0.5)
