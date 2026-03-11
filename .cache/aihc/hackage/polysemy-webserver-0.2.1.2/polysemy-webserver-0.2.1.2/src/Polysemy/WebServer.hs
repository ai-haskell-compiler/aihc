{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Polysemy.WebServer (WebServer(..), PendingWebRequest, startWebServer,
  startWebServerSettings,
  respondWebRequest, getBody, upgradeToWebSocketsResponse,
  acceptPendingWebSocketConnection, rejectPendingWebSocketConnection,
  whilePingingWebSocket, sendWebSocketDataMessages, receiveWebSocketDataMessage,
  sendWebSocketCloseCode, runWebServerFinal) where
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.WebSockets.Connection as WS
import qualified Network.WebSockets as WS
import Polysemy
import Polysemy.Final
import Data.Functor
import Control.Monad
import Control.Exception (catch)
import Data.Word (Word16)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

newtype PendingWebRequest =
  PendingWebRequest (Wai.Response -> IO Wai.ResponseReceived)

data WebServer m a where
  -- |Starts a new web-server listening on a port, sending all requests to the provided
  --  function.
  StartWebServer :: Warp.Port -> (
    Wai.Request -> PendingWebRequest -> m Wai.ResponseReceived) ->
    WebServer m ()
  StartWebServerSettings :: Warp.Settings -> (
    Wai.Request -> PendingWebRequest -> m Wai.ResponseReceived) ->
    WebServer m ()
  -- | Responds to a web request (usually called from the callback to
  --   StartWebServer.
  RespondWebRequest :: PendingWebRequest -> Wai.Response ->
                       WebServer m Wai.ResponseReceived
  -- | Reads the entire body of a request into memory. Takes a maximum length
  --   to read - if the body length exceeds this length, returns Nothing.
  GetBody :: Int -> Wai.Request -> WebServer m (Maybe BS.ByteString)
  -- | Builds a response to upgrade a connection to a web socket.
  --   Returns Nothing if the request is not appropriate to upgrade.
  UpgradeToWebSocketsResponse :: WS.ConnectionOptions ->
    (WS.PendingConnection -> m ()) -> Wai.Request -> WebServer m (Maybe Wai.Response)
  -- | Accepts a pending WebSockets connection.
  AcceptPendingWebSocketConnection :: WS.PendingConnection -> WS.AcceptRequest ->
    WebServer m (Either (Either WS.HandshakeException WS.ConnectionException) WS.Connection)
  -- | Rejects a pending WebSockets connection.
  RejectPendingWebSocketConnection :: WS.PendingConnection -> WS.RejectRequest ->
    WebServer m ()
  -- | Runs an app, and sends a ping message over the WebSockets connection
  --   every n seconds while the app is executing. When the app completes,
  --   the pings will also stop.
  WhilePingingWebSocket :: WS.Connection -> Int -> m a -> WebServer m (Maybe a)
  -- | Sends some data messages over the WebSockets connection.
  SendWebSocketDataMessages :: WS.Connection -> [WS.DataMessage] -> WebServer m ()
  -- | Receives a data message from the WebSockets connection. Returns a
  --   Left @WS.CloseRequest if the connection is closed cleanly. Returns a
  --   Left @WS.ConnectionClosed if the
  --   connection is closed uncleanly.
  ReceiveWebSocketDataMessage :: WS.Connection -> WebServer m (Either WS.ConnectionException WS.DataMessage)
  -- | Sends a friendly close message and close code on a WebSocket.
  --   See http://tools.ietf.org/html/rfc6455#section-7.4 for a list of close
  --   codes.
  SendWebSocketCloseCode :: WS.WebSocketsData a => WS.Connection -> Word16 -> a -> WebServer m ()

makeSem ''WebServer

runStartWebServer :: forall rInitial r f.
  ((Final IO) `Member` r, Functor f) =>
  Warp.Port -> (
    Wai.Request -> PendingWebRequest ->
    Sem rInitial Wai.ResponseReceived) ->
  Sem (WithTactics WebServer f (Sem rInitial) r) (f ())
runStartWebServer port app = do
  s0 <- getInitialStateT
  appFnS <- bindT $ uncurry app
  ins <- getInspectorT
  let
    appFn :: (Wai.Request, PendingWebRequest) ->
             Sem r (Maybe Wai.ResponseReceived)
    appFn = runWebServerFinal . (fmap (inspect ins)) . appFnS . (s0 $>)
  withStrategicToFinal $ do
    appFnS' <- bindS (raise . appFn)
    ins' <- getInspectorS
    s1 <- getInitialStateS
    let
      appFn' :: (Wai.Request, PendingWebRequest) ->
                IO (Maybe Wai.ResponseReceived)
      appFn' = (fmap (join . inspect ins')) . appFnS' . (s1 $>)
    return $ do
      let
        doRequestIO :: Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) ->
                       IO Wai.ResponseReceived
        doRequestIO req respond = do
          maybeRR <- appFn' (req, PendingWebRequest respond)
          case maybeRR of
            Just rr -> return rr
            Nothing -> respond $
              Wai.responseLBS (HTTP.status500) [] "Internal server error"
          
      Warp.run port $ \req reply -> doRequestIO req reply
      return $ s1 $> s0

runStartWebServerSettings :: forall rInitial r f.
  ((Final IO) `Member` r, Functor f) =>
  Warp.Settings -> (
    Wai.Request -> PendingWebRequest ->
    Sem rInitial Wai.ResponseReceived) ->
  Sem (WithTactics WebServer f (Sem rInitial) r) (f ())
runStartWebServerSettings settings app = do
  s0 <- getInitialStateT
  appFnS <- bindT $ uncurry app
  ins <- getInspectorT
  let
    appFn :: (Wai.Request, PendingWebRequest) ->
             Sem r (Maybe Wai.ResponseReceived)
    appFn = runWebServerFinal . (fmap (inspect ins)) . appFnS . (s0 $>)
  withStrategicToFinal $ do
    appFnS' <- bindS (raise . appFn)
    ins' <- getInspectorS
    s1 <- getInitialStateS
    let
      appFn' :: (Wai.Request, PendingWebRequest) ->
                IO (Maybe Wai.ResponseReceived)
      appFn' = (fmap (join . inspect ins')) . appFnS' . (s1 $>)
    return $ do
      let
        doRequestIO :: Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) ->
                       IO Wai.ResponseReceived
        doRequestIO req respond = do
          maybeRR <- appFn' (req, PendingWebRequest respond)
          case maybeRR of
            Just rr -> return rr
            Nothing -> respond $
              Wai.responseLBS (HTTP.status500) [] "Internal server error"
          
      Warp.runSettings settings $ \req reply -> doRequestIO req reply
      return $ s1 $> s0

ioToWebServerTactics ::
  forall a rInitial r f. (Functor f, Final IO `Member` r) =>
  IO a -> Sem (WithTactics WebServer f (Sem rInitial) r) (f a)
ioToWebServerTactics action = pureT =<< embedFinal action

runRespondWebRequest :: forall rInitial r f.
  ((Final IO) `Member` r, Functor f) =>
  PendingWebRequest -> Wai.Response ->
  Sem (WithTactics WebServer f (Sem rInitial) r) (f Wai.ResponseReceived)
runRespondWebRequest (PendingWebRequest respond) resp =
  ioToWebServerTactics (respond resp)

runGetBody :: forall rInitial r f.
  ((Final IO) `Member` r, Functor f) =>
  Int -> Wai.Request ->
  Sem (WithTactics WebServer f (Sem rInitial) r) (f (Maybe BS.ByteString))
runGetBody maxLen req = do
  body <- embedFinal $ Wai.lazyRequestBody req
  let strictBody = LBS.toStrict $ LBS.take (fromIntegral $ maxLen + 1) body
  if BS.length strictBody > maxLen
    then pureT Nothing
    else pureT (Just strictBody)

runUpgradeToWebSocketsResponse :: forall rInitial r f. (Final IO `Member` r, Functor f) =>
  WS.ConnectionOptions ->
  (WS.PendingConnection -> Sem rInitial ()) ->
  Wai.Request ->
  Sem (WithTactics WebServer f (Sem rInitial) r) (f (Maybe Wai.Response))
runUpgradeToWebSocketsResponse opts app req = do
  stT <- getInitialStateT
  boundTApp' <- bindT app
  let boundTApp :: WS.PendingConnection -> Sem r ()
      boundTApp = runWebServerFinal . ($> ()) . boundTApp' . (stT $>)
  withStrategicToFinal $ do
    stS <- getInitialStateS
    boundTSApp' <- bindS (raise . boundTApp)
    let boundTSApp :: WS.PendingConnection -> IO ()
        boundTSApp = ($> ()) . boundTSApp' . (stS $>)
        finalResp :: Maybe Wai.Response
        finalResp = WaiWs.websocketsApp opts boundTSApp req
    return . return $ stS $> (stT $> finalResp)

runAcceptPendingWebSocketConnection ::
  (Final IO `Member` r, Functor f) =>
  WS.PendingConnection ->
  WS.AcceptRequest ->
  Sem (WithTactics WebServer f (Sem rInitial) r) (
    f (Either (Either WS.HandshakeException WS.ConnectionException)
       WS.Connection))
runAcceptPendingWebSocketConnection conn opts =
  ioToWebServerTactics inIO
  where
    inIO :: IO (Either
                (Either WS.HandshakeException WS.ConnectionException)
                WS.Connection)
    inIO = catch (catch (Right <$> WS.acceptRequestWith conn opts)
                   (return . Left . Right))
                 (return . Left . Left)

runWhilePingingWebSocket :: forall rInitial a r f.
  (Final IO `Member` r, Functor f) =>
  WS.Connection -> Int -> Sem rInitial a ->
  Sem (WithTactics WebServer f (Sem rInitial) r) (f (Maybe a))
runWhilePingingWebSocket conn n app = do
  stT <- getInitialStateT
  appT' <- runT app
  insT <- getInspectorT
  let
    appT :: Sem r (Maybe a)
    appT = (inspect insT) <$> runWebServerFinal appT'
  withStrategicToFinal $ do
    appTS' <- runS (raise appT)
    stS <- getInitialStateS
    insS <- getInspectorS
    let appTS :: IO (Maybe a)
        appTS = (join . inspect insS) <$> appTS'
    return $ ((stS $>) . (stT $>)) <$> WS.withPingThread conn n (return ()) appTS

runReceiveWebSocketDataMessage :: forall rInitial r f.
  (Final IO `Member` r, Functor f) =>
  WS.Connection ->
  Sem (WithTactics WebServer f (Sem rInitial) r) (
  f (Either WS.ConnectionException WS.DataMessage))
runReceiveWebSocketDataMessage conn =
  ioToWebServerTactics inIO
  where
    inIO :: IO (Either WS.ConnectionException WS.DataMessage)
    inIO = catch (Right <$> WS.receiveDataMessage conn)
                 (return . Left)

runWebServerFinal :: ((Final IO) `Member` r) =>
                     Sem (WebServer ': r) a -> Sem r a
runWebServerFinal =
  interpretH (\v -> case v of
                 StartWebServer port app -> runStartWebServer port app
                 StartWebServerSettings settings app -> runStartWebServerSettings settings app
                 RespondWebRequest reqId response -> runRespondWebRequest reqId response
                 GetBody maxLen req -> runGetBody maxLen req
                 UpgradeToWebSocketsResponse opts app req ->
                   runUpgradeToWebSocketsResponse opts app req
                 AcceptPendingWebSocketConnection conn opts ->
                   runAcceptPendingWebSocketConnection conn opts
                 RejectPendingWebSocketConnection conn opts ->
                   ioToWebServerTactics (WS.rejectRequestWith conn opts)
                 WhilePingingWebSocket conn n app ->
                   runWhilePingingWebSocket conn n app
                 SendWebSocketDataMessages conn msgs ->
                   ioToWebServerTactics $ WS.sendDataMessages conn msgs
                 ReceiveWebSocketDataMessage conn ->
                   runReceiveWebSocketDataMessage conn
                 SendWebSocketCloseCode conn code msg ->
                   ioToWebServerTactics $ WS.sendCloseCode conn code msg
             )
