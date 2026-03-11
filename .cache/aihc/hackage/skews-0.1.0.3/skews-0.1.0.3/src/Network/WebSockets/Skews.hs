{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.WebSockets.Skews
  ( -- * The server object type and related types.
    Server
  , Args(..)
  , RequestHandler

    -- * Controling the server's lifecycle.
  , start
  , reinit
  , threadId

    -- * Configuring how the server responds/sends WebSocket messages to the client.
  , enqueRequestHandler
  , enqueResponse
  , replaceRequestHandlers
  , setDefaultRequestHandler
  , setDefaultResponse
  , respondWith
  , doNothing
  , sendToClients
  , forgetDefaultRequestHandler
  , forgetDefaultResponse
  , forgetReceivedRequests

    -- * Checking the server's status.
  , listeningPort
  , listeningHost
  , recentlyReceived
  , countConnectedClients
  ) where


import           Control.Applicative  ((<|>))
import           Control.Concurrent   (ThreadId, forkIO)
import           Control.Exception    (handleJust)
import qualified Data.ByteString.Lazy as B
import qualified Data.IORef           as IOR

#if MIN_VERSION_deque(0, 3, 0)
import qualified Deque.Lazy           as Q
import           GHC.Exts             (fromList)
#else
import           Deque                (fromList)
import qualified Deque                as Q
#endif

import qualified Network.WebSockets   as WS


-- | The Server object.
--   You can easily configure the behavior (how the server responds with some requests)
--   after creating by 'start' function.
data Server =
  Server
    { requestHandlerQueue   :: !(IOR.IORef (Q.Deque RequestHandler))
    , defaultRequestHandler :: !(IOR.IORef (Maybe RequestHandler))
    , recentlyReceivedRef   :: !(IOR.IORef (Q.Deque B.ByteString))
    , clientConnections     :: !(IOR.IORef [WS.Connection])
    , threadId              :: !ThreadId -- ^ Call 'Control.Concurrent.killThread' to stop the server.
    , listeningPort         :: !Int
    , listeningHost         :: !String
    }


-- | Used to configure the server's behavior when receiving 'WS.DataMessage' as a 'B.ByteString'.
--   If returns @Nothing@, the server does nothing.
type RequestHandler = B.ByteString -> IO (Maybe B.ByteString)

-- | Maybe often used 'RequestHandler'. Always respond with the given 'WS.Message'.
respondWith :: B.ByteString -> RequestHandler
respondWith = const . return . Just


-- | Maybe often used 'RequestHandler'. Do nothing.
doNothing :: RequestHandler
doNothing = const $ return Nothing


-- TODO: Get an unused port number automatically
data Args = Args
  { host       :: String
  , portNumber :: Int
  }


-- | Start the server by the given hostname and port number as 'Args' object.
start :: Args -> IO Server
start (Args listeningHost listeningPort) = do
  requestHandlerQueue   <- IOR.newIORef mempty
  defaultRequestHandler <- IOR.newIORef Nothing
  recentlyReceivedRef   <- IOR.newIORef mempty
  clientConnections     <- IOR.newIORef mempty
  threadId <- forkIO $ WS.runServer listeningHost listeningPort $ \pc -> do
    c <- WS.acceptRequest pc
    addClientConnection clientConnections c

    let
      loop = do
        m <- WS.receive c
        case m of
          WS.DataMessage _ _ _ dat -> do
            mbrh  <- deque requestHandlerQueue
            dmbrh <- IOR.readIORef defaultRequestHandler
            let bs = WS.fromDataMessage dat
            case mbrh <|> dmbrh of
              Just rh -> maybe (return ()) (WS.sendBinaryData c) =<< rh bs
              Nothing -> return ()
            IOR.modifyIORef recentlyReceivedRef (Q.snoc bs)
            loop
          WS.ControlMessage (WS.Close _ _) ->
            WS.sendClose c ("Bye" :: B.ByteString)
          _other -> loop

    ignoreConnectionClosed loop

  return Server {..}


addClientConnection :: IOR.IORef [WS.Connection] -> WS.Connection -> IO ()
addClientConnection ccsr c =
  IOR.atomicModifyIORef' ccsr (\ccs -> (c : ccs, ()))


-- | Configure the request handler called when the server receives next.
--   'RequestHandler's configured with this function and other non-@Default@ functions are "dequeued".
--   So the server responds with the request handler only once.
--
--   If you need the server to respond always with the same response,
--   use 'setDefaultResponse' and 'setDefaultRequestHandler'.
enqueRequestHandler :: Server -> RequestHandler -> IO ()
enqueRequestHandler Server {..} rh =
  IOR.atomicModifyIORef' requestHandlerQueue (\q -> (Q.snoc rh q, ()))


-- | Configure the response called when the server receives next.
enqueResponse :: Server -> B.ByteString -> IO ()
enqueResponse s = enqueRequestHandler s . respondWith


-- | Configure the request handler called when no request handlers are queued.
setDefaultRequestHandler :: Server -> RequestHandler -> IO ()
setDefaultRequestHandler Server {..} =
  IOR.atomicWriteIORef defaultRequestHandler . Just


-- | Reset the request handler queue.
replaceRequestHandlers :: Server -> [RequestHandler] -> IO ()
replaceRequestHandlers Server {..} =
  IOR.atomicWriteIORef requestHandlerQueue . fromList


-- | Configure the response called when no request handlers are queued.
setDefaultResponse :: Server -> B.ByteString -> IO ()
setDefaultResponse s = setDefaultRequestHandler s . respondWith


-- | Delete the default request handler. After calling this function,
--   the 'Server' object doesn't respond to any message if the request handler queue is empty.
forgetDefaultRequestHandler :: Server -> IO ()
forgetDefaultRequestHandler Server {..} =
  IOR.atomicWriteIORef defaultRequestHandler Nothing


-- | Alias for 'forgetDefaultRequestHandler'
forgetDefaultResponse :: Server -> IO ()
forgetDefaultResponse = forgetDefaultRequestHandler


-- | Forget recently received requests.
forgetReceivedRequests :: Server -> IO ()
forgetReceivedRequests Server {..} =
  IOR.atomicWriteIORef recentlyReceivedRef mempty


-- | Send the given 'Message' immediately to the all connected clients.
sendToClients :: Server -> B.ByteString -> IO ()
sendToClients Server {..} msg =
  mapM_ (`WS.sendBinaryData` msg) =<< IOR.readIORef clientConnections


-- | Close all connections, forget recently received requests,
--   and delete any configured 'RequestHandler's.
reinit :: Server -> IO ()
reinit Server {..} = do
  IOR.atomicWriteIORef requestHandlerQueue   mempty
  IOR.atomicWriteIORef defaultRequestHandler Nothing
  IOR.atomicWriteIORef recentlyReceivedRef   mempty

  mapM_ (ignoreConnectionClosed . (`WS.sendClose` ("Bye" :: B.ByteString)))
    =<< IOR.readIORef clientConnections
  IOR.atomicWriteIORef clientConnections []


-- | Retrieve any messages sent by the clients.
recentlyReceived :: Server -> IO (Q.Deque B.ByteString)
recentlyReceived Server {..} = IOR.readIORef recentlyReceivedRef


deque :: IOR.IORef (Q.Deque a) -> IO (Maybe a)
deque qr = IOR.atomicModifyIORef' qr $ \q -> case Q.uncons q of
  Just (x, qLeft) -> (qLeft, Just x)
  _               -> (mempty, Nothing)


ignoreConnectionClosed :: IO () -> IO ()
ignoreConnectionClosed = handleJust selectClosed (const $ return ())
 where
  selectClosed :: WS.ConnectionException -> Maybe ()
  selectClosed WS.ConnectionClosed = Just ()
  selectClosed _                   = Nothing


-- | For debugging
countConnectedClients :: Server -> IO Int
countConnectedClients Server {..} = length <$> IOR.readIORef clientConnections
