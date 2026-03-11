{-# LANGUAGE OverloadedStrings #-}

-- | It's a box. It's a websocket. It's an example.
module Box.Websocket.Example where

import Box
import Box.Socket.Types
import Box.Websocket
import Control.Concurrent.Async
import Data.Functor.Contravariant
import Data.Text (Text)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Box
-- >>> import Box.Websocket.Example
-- >>> import Control.Concurrent.Async

-- | A server that only sends and a client that only receives.
--
-- >>> senderExample ["a","b"]
-- ["a","b"]
senderExample :: [Text] -> IO [Text]
senderExample ts = do
  (c, r) <- refCommitter
  a <- async (serverBox defaultSocketConfig (CloseAfter 0.2) . Box mempty <$|> qList ts)
  sleep 0.1
  clientBox defaultSocketConfig StayOpen (Box c mempty)
  sleep 0.1
  cancel a
  r

-- | echo server example
--
-- >>> echoExample ["a","b","c"]
-- ["echo: a","echo: b","echo: c"]
echoExample :: [Text] -> IO [Text]
echoExample ts = do
  (c, r) <- refCommitter
  a <-
    async
      (responseServer defaultSocketConfig (pure . (("echo: " :: Text) <>)))
  sleep 0.1
  clientBox defaultSocketConfig (CloseAfter 0.2) . Box c <$|> qList ts
  sleep 0.1
  cancel a
  r

-- | echo server example, with event logging.
--
-- The order of events is non-deterministic, so this is a rough guide:
--
-- > echoLogExample ["a","b","c"]
-- (["echo: a","echo: b","echo: c"],["client:sender_:emit:Just \"a\"","client:sender_:sendTextData:Right ()","client:sender_:emit:Just \"b\"","client:sender_:sendTextData:Right ()","client:sender_:emit:Just \"c\"","client:sender_:sendTextData:Right ()","client:sender_:emit:Nothing","client:sender_ closed with SocketOpen","server:receiver_:receiveData:Right \"a\"","server:receiver_:receiveData:Right \"b\"","server:receiver_:receiveData:Right \"c\"","server:sender_:emit:Just \"echo: a\"","server:sender_:sendTextData:Right ()","server:sender_:emit:Just \"echo: b\"","server:sender_:sendTextData:Right ()","server:sender_:emit:Just \"echo: c\"","server:sender_:sendTextData:Right ()","client:receiver_:receiveData:Right \"echo: a\"","client:receiver_:receiveData:Right \"echo: b\"","client:receiver_:receiveData:Right \"echo: c\"","server:receiver_:receiveData:Left (CloseRequest 1000 \"close after sending\")","server:receiver_ closed","client:receiver_:receiveData:Left (CloseRequest 1000 \"close after sending\")","client:receiver_ closed","client:duplex_ closed","server:duplex_ closed"])
echoLogExample :: [Text] -> IO ([Text], [Text])
echoLogExample ts = do
  (c, r) <- refCommitter
  (cLog, resLog) <- refCommitter
  a <-
    async
      (fuse (pure . pure . (("echo: " :: Text) <>)) <$|> fromAction (\b -> duplex_ (CloseAfter 0.5) (contramap ("server:" <>) cLog) b <$|> serve defaultSocketConfig))
  sleep 0.1
  duplex_ (CloseAfter 0.2) (contramap ("client:" <>) cLog) . Box c <$> qList ts <*|> connect defaultSocketConfig
  sleep 0.1
  cancel a
  (,) <$> r <*> resLog

-- | "q" to close the client, reads and writes from std
--
-- >> clientIO
-- *** Exception: Network.Socket.connect: <socket: ...>: does not exist (Connection refused)
-- ...
clientIO :: IO ()
clientIO =
  clientBox defaultSocketConfig (CloseAfter 0) (stdBox "q")

-- | "q" to close a client socket down. Ctrl-c to close the server. Reads and writes from std.
--
-- >> a <- async serverIO
-- >> serverIO
-- *** Exception: Network.Socket.bind: resource busy (Address already in use)
-- ...
--
-- >> cancel a
serverIO :: IO ()
serverIO = serverBox defaultSocketConfig (CloseAfter 0) (stdBox "q")
