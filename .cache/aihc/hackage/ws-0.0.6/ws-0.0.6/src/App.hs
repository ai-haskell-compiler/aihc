{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  #-}

module App where

import App.Types (AppM, Env (envSecure, envHost, envPort, envPath))

import Network.WebSockets
  (ClientApp, DataMessage (..), ConnectionException (..), runClient, receiveDataMessage, sendTextData, sendClose)
import Wuss (runSecureClient)

import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Encoding    as LT
import Data.Monoid ((<>))
import Control.Monad (forever, unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Trans (lift)
import Control.Monad.Catch (handle)
import Control.Concurrent.Async (async, link, withAsync, wait)
import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import System.Exit (exitSuccess, exitFailure)
import System.Console.Haskeline (getExternalPrint, getInputLine)


app :: AppM ()
app = do
  print' <- getExternalPrint
  env <- lift ask

  outgoingChan <- liftIO newChan

  mainThread <- liftIO $ async $
    handle (handleConnException print') $
      if envSecure env
      then runSecureClient
            (envHost env)
            (envPort env)
            (envPath env)
            (ws print' outgoingChan)
      else runClient
            (envHost env)
            (fromIntegral $ envPort env)
            (envPath env)
            (ws print' outgoingChan)

  liftIO (link mainThread)

  forever $ do
    mx <- getInputLine $ T.unpack $ (if envSecure env then "wss" else "ws")
                           <> "://" <> T.pack (envHost env)
                           <> ":" <> T.pack (show (envPort env)) <> T.pack (envPath env) <> "> "
    case mx of
      Nothing -> pure ()
      Just x -> liftIO $ writeChan outgoingChan x
  where
    -- totally ripped off from
    -- https://hackage.haskell.org/package/wuss-1.0.4/docs/Wuss.html
    ws :: (String -> IO ()) -> Chan String -> ClientApp ()
    ws print' outgoingChan conn = do
      -- always listen for incoming messages in a separate thread
      let listen = forever $ do
            message <- receiveDataMessage conn
            let bs = case message of
                      Text x _ -> x
                      Binary x -> x
            print' $ case LT.decodeUtf8' bs of
              Left e -> "[Warn] UTF8 Decode Error: " ++ show e
              Right t -> LT.unpack t

      -- always listen for outgoing messages in the main thread
      let sender = forever $ do
            userInput <- readChan outgoingChan
            unless (userInput == "") $
              sendTextData conn (T.pack userInput)

      withAsync listen $ \l ->
        withAsync sender $ \s -> do
          void $ wait l
          void $ wait s

      sendClose conn ("Bye from ws!" :: T.Text)


handleConnException :: (String -> IO ()) -> ConnectionException -> IO a
handleConnException print' e =
  case e of
    CloseRequest c m -> do
      print' $ "[Info] Closing with code " ++ show c
            ++ " and message " ++ show m
      exitSuccess
    ConnectionClosed -> do
      print' "[Error] Connection closed unexpectedly"
      exitFailure
    ParseException s -> do
      print' $ "[Error] Websocket stream parse failure: " ++ s
      exitFailure
    UnicodeException s -> do
      print' $ "[Error] Websocket couldn't parse unicode: " ++ s
      exitFailure
