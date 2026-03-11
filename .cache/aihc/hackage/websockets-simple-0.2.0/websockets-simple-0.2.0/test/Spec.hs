{-# LANGUAGE
    NamedFieldPuns
  #-}

module Main where

import Network.WebSockets.Simple (WebSocketsApp (..), WebSocketsAppParams (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, writeTChan, newTChanIO, readTChan)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)
import Test.Hspec (runIO, it)
import Test.WebSockets.Simple (runConnected)


testReceivingApp :: TChan Int -> WebSocketsApp IO Int Int
testReceivingApp receivedChan = WebSocketsApp
  { onOpen = \WebSocketsAppParams{send} -> do
      putStrLn "sending in 10 seconds..."
      threadDelay $ 10^6 * 10
      send 0
      putStrLn "sent."
  , onReceive = \_ x ->
      atomically $ writeTChan receivedChan x
  , onClose = \_ _ -> pure ()
  }

testSendingApp :: WebSocketsApp IO Int Int
testSendingApp = WebSocketsApp
  { onOpen = \_ -> pure ()
  , onReceive = \WebSocketsAppParams{send} x -> do
      putStrLn "received, sending in 10 seconds..."
      threadDelay $ 10^6 * 10
      send x
      putStrLn "sent."
  , onClose = \_ _ -> pure ()
  }


main :: IO ()
main = do
  rwChan <- newTChanIO

  _ <- runConnected (testReceivingApp rwChan) testSendingApp


  rwSpec <- testSpec "Atomic writes and receipts" $ do
    out <- runIO $ atomically $ readTChan rwChan
    it "has produced a receieved relay" $ out == 0

  defaultMain $ testGroup "WebSockets Simple"
    [ rwSpec
    ]
