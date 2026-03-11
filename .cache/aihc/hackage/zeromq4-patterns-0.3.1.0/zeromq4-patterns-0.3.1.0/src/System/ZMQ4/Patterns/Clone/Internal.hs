{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module System.ZMQ4.Patterns.Clone.Internal (
  -- * Shared
  Message(..)
, iSTATE_REQUEST

  -- * Server
, publisher

  -- * Client
, subscriber
, queryLastState
) where

import Control.Concurrent.Async (Async, waitBoth, uninterruptibleCancel)
import Control.Concurrent.MVar
import Control.Concurrent.STM

import Control.Monad (forever, void, when)
import Control.Monad.Catch
import Control.Monad.IO.Class (liftIO)

import Data.Binary
import Data.ByteString (ByteString)
import Data.Word (Word64)
import qualified Data.ByteString.Lazy as BL

import System.ZMQ4.Monadic

-- | Actual message sent between server and client.
data Message a = Message {
    messageVersion :: {-# UNPACK #-} !Word64
  , messageValue :: !a
  } deriving (Eq, Show)

instance Binary a => Binary (Message a) where
    put Message{..} = do
        put messageVersion
        put messageValue

    get = do
        messageVersion <- get
        messageValue <- get
        return $! Message{..}

-- | Message sent by a client to request a state snapshot
iSTATE_REQUEST :: ByteString
iSTATE_REQUEST = "STATE_REQUEST?"

-- | A server publishing messages to a client.
--
-- This function will start serving messages on the
-- current thread.
--
-- It will send all objects that are pushed on the
-- channel to a PUB socket.
--
-- It will listen for snapshot requests on a ROUTER
-- socket.
--
-- The server will automatically keep the last sent
-- object cached, and use at as a snapshot. Sequencing
-- is automatically handled using a 'Word64' sequence
-- counter.
publisher :: Binary a
          => String    -- ^ Bind address of the PUB socket
          -> String    -- ^ Bind address of the ROUTER socket
          -> MVar a    -- ^ Channel of incoming messages
          -> IO ()
publisher !pubAddr !routerAddr !chan = runZMQ $ do
    ready <- liftIO newEmptyMVar
    routerC <- liftIO . atomically $ newTVar Nothing

    withAsync (publisher' ready routerC) $ \t1 ->
        withAsync (router ready routerC) $ \t2 ->
            void . liftIO $ waitBoth t1 t2
  where
    publisher' :: forall z void. MVar () -> TVar (Maybe ByteString) -> ZMQ z void
    publisher' ready routerC = do
        -- bind socket
        pub <- socket Pub
        bind pub pubAddr

        -- wait for router
        () <- liftIO $ takeMVar ready

        -- handle all messages
        let next :: Word64 -> ZMQ z void
            next !seq' = do a <- liftIO $ takeMVar chan
                            let !env = Message seq' a
                                !msg = BL.toStrict (encode env)
                                !jmsg = Just msg
                            liftIO . atomically $ writeTVar routerC jmsg
                            send pub [] msg
                            next (seq'+1)

        next 0

    router :: forall z void. MVar () -> TVar (Maybe ByteString) -> ZMQ z void
    router ready routerC = do
        -- bind socket
        rout <- socket Router
        bind rout routerAddr

        -- send ready
        liftIO $ putMVar ready ()

        -- handle all messages
        let getSnapshot :: IO ByteString
            getSnapshot = liftIO . atomically $ readTVar routerC >>= \case
                            Nothing -> retry
                            Just x -> return x

        forever $ do
            receiveMulti rout >>= \case
                [identity', req] -> when (req == iSTATE_REQUEST) $ do
                        snapshot <- liftIO getSnapshot
                        sendMulti rout [identity', snapshot]
                _ -> return ()

-- | A client receiving messages from a server.
--
-- This function will start reading messages on the
-- current thread.
--
-- It will connect to the server's PUB and ROUTER
-- ports. It will backlog messages from the PUB
-- socket, while requesting the initial state snapshot
-- from the ROUTER socket. Afterwards, it will forever
-- read from the PUB socket, and process all
-- in-sequence updates.
--
-- Note that this pattern is not 100% reliable. Messages might be dropped
-- between the initial state request and the first update.
subscriber :: Binary a
           => String    -- ^ Address of the server's PUB socket
           -> String    -- ^ Address of the server's ROUTER socket
           -> MVar a    -- ^ CHannel where incoming messsages will be written to
           -> IO ()
subscriber !pubAddr !routerAddr !chan = runZMQ $ do
    -- connect to both sockets
    sub <- socket Sub
    connect sub pubAddr
    subscribe sub ""

    dealer <- socket Dealer
    connect dealer routerAddr

    -- get inital snapshot
    send dealer [] iSTATE_REQUEST
    msg <- receive dealer
    let !env = decode (BL.fromStrict msg)
        !a = messageValue env
    liftIO $ putMVar chan a

    -- get remaining updates
    forever $ do
        msg' <- receive sub
        let !env' = decode (BL.fromStrict msg')
            !a' = messageValue env'

        when (messageVersion env' > messageVersion env) $
            liftIO $ putMVar chan a'

-- | Only request the most recent state from the server.
--
-- This function will query the ROUTER port for the latest state, and return
-- it.
queryLastState :: Binary a
               => String    -- ^ Address of the server's ROUTER socket
               -> IO a
queryLastState !routerAddr = runZMQ $ do
    dealer <- socket Dealer
    connect dealer routerAddr

    send dealer [] iSTATE_REQUEST
    msg <- receive dealer
    let !env = decode (BL.fromStrict msg)
        !a = messageValue env
    return a

-- | Helper function
withAsync :: ZMQ z a -> (Async a -> ZMQ z b) -> ZMQ z b
withAsync action inner = mask $ \restore -> do
    a <- async (restore action)
    restore (inner a) `finally` liftIO (uninterruptibleCancel a)

