{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Backend-free MessagePack RPC Client.
module Network.MessagePack.RPC.Client
    (
    -- * Config
      Config(..)
    , NotificationHandler
    , RequestHandler
    , Logger
    , Formatter
    , defaultConfig
    -- * Backend
    , Backend(..)
    -- * Client
    , Client
    , withClient
    , shutdown
    -- * Call and reply
    , Result
    , call
    , reply
    )
where

import           Control.Concurrent                      (forkFinally, forkIO,
                                                          killThread)
import qualified Control.Concurrent.MVar                 as MVar
import qualified Control.Exception.Safe                  as E
import           Control.Monad                           (forever, void, when)
import qualified Data.ByteString.Lazy                    as BL
import qualified Data.HashMap.Strict                     as HM
import qualified Data.IORef                              as IORef
import qualified Data.MessagePack                        as MsgPack
#if !MIN_VERSION_base(4,13,0)
import           Data.Monoid                             ((<>))
#endif
import           System.IO                               (hPrint, stderr)
import           System.Timeout                          (timeout)

import           Data.MessagePack.RPC
import           Network.MessagePack.RPC.Client.Internal

-- | Notification handler. The 3rd argument is response objects.
type NotificationHandler = Client -> MethodName -> [MsgPack.Object] -> IO ()

-- | Notification handler. The 2nd argument is message id to be used
--   for replying. The 3rd argument is response objects.
type RequestHandler
    = Client -> MessageId -> MethodName -> [MsgPack.Object] -> IO ()

-- | Configuration for MessagePack RPC.
data Config = Config {
    notificationHandler :: NotificationHandler
  , requestHandler      :: RequestHandler
  , logger              :: Logger
  , exceptionHandlers   :: [E.Handler IO ()]
    -- ^ Handles an exception thrown from the receiver thread,
    --   which is the only thread to receive 'Message's via 'Backend'.
    --   Until exiting from the block of 'withClient', the receiver thread
    --   indefinitely waits for frames via 'backendRecv'.
  , formatter           :: Formatter
  , waitRequestHandler  :: Bool
  }

-- | The default configuration.
--    'formatter' is 'show'.
--     Others do nothing.
defaultConfig :: Config
defaultConfig = Config
    { notificationHandler = \_ _ _ -> return ()
    , requestHandler      = \_ _ _ _ -> return ()
    , logger              = \_ -> return ()
    , exceptionHandlers = [E.Handler $ \(E.SomeException e) -> hPrint stderr e]
    , formatter           = show
    , waitRequestHandler  = False
    }

-- TODO: Returns any exception
-- | Calling RPC.
call :: Client -> MethodName -> [MsgPack.Object] -> IO Result
call client funName args = do
    rrsp <- E.bracket register unregister sendAndRecv
    case rrsp of
        Nothing  -> return $ Left MsgPack.ObjectNil
        Just rsp -> return rsp
  where
    sendAndRecv (requestId, rspVar) = do
        let request = RequestMessage requestId funName args
        backendSend (clientBackend client) $ BL.toStrict $ MsgPack.pack request
        clientLog client $ "sent: " <> clientFormat client request
        timeout 3000000 $ MVar.takeMVar rspVar
    register = do
        requestId <- getNewMessageId ss
        rspVar    <- MVar.newEmptyMVar
        IORef.atomicModifyIORef' (dispatchTable ss)
            $ \tbl -> (HM.insert requestId rspVar tbl, ())
        return (requestId, rspVar)
    unregister (requestId, _) = IORef.atomicModifyIORef' (dispatchTable ss)
        $ \tbl -> (HM.delete requestId tbl, ())
    ss = clientSessionState client

-- | Replying RPC. This should be used in 'RequestHandler'.
reply :: Client -> MessageId -> Result -> IO ()
reply client mid result = do
    let response = ResponseMessage mid result
    let p        = BL.toStrict $ MsgPack.pack response
    backendSend (clientBackend client) p
    clientLog client $ "sent: " <> clientFormat client response

getNewMessageId :: SessionState -> IO MessageId
getNewMessageId ss =
    IORef.atomicModifyIORef (lastMessageId ss) $ \cur -> (cur + 1, cur)

receiverThread :: Client -> Config -> IO ()
receiverThread client config =
    (`E.catches` exceptionHandlers config) $ forever $ do
        response <- MsgPack.unpack . BL.fromStrict =<< backendRecv
            (clientBackend client)
        clientLog client $ "received: " <> clientFormat client response
        case response of
            ResponseMessage mid result -> do
                tbl <- IORef.readIORef $ dispatchTable ss
                case HM.lookup mid tbl of
                    Just rspVar -> MVar.putMVar rspVar result
                    Nothing ->
                        clientLog client
                            $  "ERROR: No MVar assinged with request ID "
                            ++ show mid
                            ++ "."
            NotificationMessage methodName params ->
                void . forkIO $ notificationHandler config
                                                    client
                                                    methodName
                                                    params
            RequestMessage mid methodName params ->
                void . forkIO $ requestHandler config
                                               client
                                               mid
                                               methodName
                                               params
    where ss = clientSessionState client

-- | Executing the action in the 3rd argument with a 'Client'.
withClient :: Config -> Backend -> (Client -> IO a) -> IO a
withClient config backend action = do
    ss     <- initSessionState
    wait   <- MVar.newEmptyMVar
    tidref <- IORef.newIORef Nothing
    let client = Client ss backend (logger config) (formatter config) tidref
    tid <- forkFinally (receiverThread client config)
        $ \_ -> MVar.putMVar wait ()
    IORef.writeIORef tidref $ Just tid
    takeAction client wait `E.finally` (backendClose backend >> killThread tid)
  where
    takeAction client wait = do
        returned <- action client
        when (waitRequestHandler config) $ MVar.takeMVar wait
        return returned

-- | This function cleans up the internal states including
--   the termination of internal threads.
shutdown :: Client -> IO ()
shutdown client = do
    mtid <- IORef.readIORef $ clientHandlerTid client
    case mtid of
        Nothing  -> return ()
        Just tid -> killThread tid
