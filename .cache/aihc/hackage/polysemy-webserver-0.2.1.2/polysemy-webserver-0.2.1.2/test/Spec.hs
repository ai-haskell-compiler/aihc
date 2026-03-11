{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Polysemy
import Polysemy.Final
import Polysemy.Error
import Polysemy.WebServer
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Types.Status as HTTP
import Network.HTTP.Simple
import Polysemy.Async
import Test.Hspec
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Connection as WS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import Network.WebSockets.Client as WSC
import Control.Monad
import Text.Read (readMaybe)
import Data.ByteString.Lazy.Char8 as LBS8
import Control.Concurrent

eitherRedistrib :: Either (Either a b) c -> Either a (Either b c)
eitherRedistrib (Left (Left a)) = Left a
eitherRedistrib (Left (Right b)) = Right (Left b)
eitherRedistrib (Right c) = Right (Right c)

testServerApp :: WebServer `Member` r =>
  Wai.Request -> PendingWebRequest -> Sem r Wai.ResponseReceived
testServerApp req pendingReq
  | Wai.pathInfo req == [] = respondWebRequest pendingReq (
  Wai.responseLBS HTTP.status200 [] "Success")
  | Wai.pathInfo req == ["ws"] = do
      maybeResp <- upgradeToWebSocketsResponse WS.defaultConnectionOptions
        testWSApp req
      let resp = maybe (Wai.responseLBS HTTP.status400 [] "Bad request")
            id maybeResp
      respondWebRequest pendingReq resp
  | otherwise = respondWebRequest pendingReq (
      Wai.responseLBS HTTP.status404 [] "Not Found")
testWSApp :: forall r. WebServer `Member` r =>
  WS.PendingConnection -> Sem r ()
testWSApp pendConn =
  void $ runError @WS.ConnectionException $
    runError @WS.HandshakeException $ do
      conn <- fromEither =<< fromEither . eitherRedistrib =<<
                acceptPendingWebSocketConnection pendConn WS.defaultAcceptRequest
      whilePingingWebSocket conn 30 (go conn)
  where
    go :: (WebServer `Member` r2,
           Error WS.ConnectionException `Member` r2) =>
          WS.Connection -> Sem r2 ()
    go conn = do
      msg <- fromEither =<< receiveWebSocketDataMessage conn
      let nextResp = case msg of
                       WS.Text bs _
                         | Just n <- readMaybe . LBS8.unpack $ bs ->
                             T.pack $ show (n + 1)
                       _ -> "Invalid message: " <> (T.pack . show $ msg)
      sendWebSocketDataMessages conn [WS.Text (LBS.fromStrict $ TE.encodeUtf8 nextResp) Nothing]
      go conn
main :: IO ()
main = runFinal . runWebServerFinal . asyncToIOFinal $ do
  -- With the WebServer effect, start a new server...
  async $ startWebServer 8123 testServerApp

  -- To do: Ideally provide a better way to know when it's ready...
  embedFinal $ threadDelay 5000000
  embedFinal . hspec $ do
    describe "Web Server" $ do
      it "should respond to normal requests" $ do
        -- Use a client library to test...
        req <- parseRequest "http://127.0.0.1:8123/"
        resp <- httpLBS req
        getResponseStatusCode resp `shouldBe` 200
        getResponseBody resp `shouldBe` "Success"
      it "should support upgrading to WebSockets" $ do
        result <- WSC.runClient "127.0.0.1" 8123 "/ws" $ \conn -> do
          WS.sendTextData conn ("42" :: T.Text)
          r1 <- WS.receiveData conn
          WS.sendTextData conn (r1 :: LBS.ByteString)
          WS.receiveData conn
        result `shouldBe` ("44" :: T.Text)
