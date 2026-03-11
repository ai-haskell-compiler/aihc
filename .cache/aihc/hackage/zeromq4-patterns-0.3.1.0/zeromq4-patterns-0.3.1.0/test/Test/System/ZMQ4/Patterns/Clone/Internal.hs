{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.System.ZMQ4.Patterns.Clone.Internal (tests) where

import Control.Concurrent
import Control.Concurrent.Async

import Data.Binary (encode, decode)
import Data.Word (Word64)

import System.ZMQ4.Patterns.Clone.Internal

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

tests :: Test
tests = testGroup "System.ZMQ4.Patterns.Clone.Internal" [
    testProperty "binary message" prop_binary_message
  , testProperty "server client" prop_server_client
  , testProperty "server queryLastState" prop_server_queryLastState
  ]

newtype TestMessage = TestMessage (Message Int) deriving (Eq, Show)

instance Arbitrary TestMessage where
    arbitrary = TestMessage <$> (Message <$> arbitrary <*> arbitrary)

prop_binary_message :: TestMessage -> Bool
prop_binary_message (TestMessage m) = m == decode (encode m)


newtype ServerClientTest = ServerClientTest {
    serverClientTestMessages :: [Word64]
  } deriving (Show)

instance Arbitrary ServerClientTest where
    arbitrary = do
        xs <- sized $ \n' ->
            let n = fromIntegral (max 1 n') in
            return [1..n]
        return $! ServerClientTest xs

prop_server_client :: ServerClientTest -> Property
prop_server_client test = within (10*1000*1000) $ ioProperty $ do
    let pubAddr    = "ipc:///tmp/zeromq4-clone-pattern-test-pub.socket"
        routerAddr = "ipc:///tmp/zeromq4-clone-pattern-test-router.socket"

    pushC <- newEmptyMVar
    recvC <- newEmptyMVar

    withAsync (publisher pubAddr routerAddr pushC) $ \_ ->
      withAsync (subscriber pubAddr routerAddr recvC) $ \_ ->
        withAsync (pushAll pushC) $ \_ ->
          receiveAll recvC
  where
    messages = serverClientTestMessages test

    pushAll :: MVar Word64 -> IO ()
    pushAll c = mapM_ (\x -> putMVar c x >> threadDelay (100))
                      messages

    receiveAll :: MVar Word64 -> IO Property
    receiveAll c = do
        initSeq <- takeMVar c
        let next :: Word64 -> IO Property
            next !s | s == (fromIntegral $ length messages) = return $ property True
                    | otherwise = do ms' <- race (threadDelay (100*1000))
                                                 (takeMVar c)
                                     case ms' of
                                        Left () -> return $ property True
                                        Right s' ->
                                            if s' <= s then
                                               return $ counterexample "out of order" False
                                            else
                                               next s'
        next initSeq

prop_server_queryLastState :: Word64 -> Property
prop_server_queryLastState val = within (10*1000*1000) $ ioProperty $ do
    let pubAddr    = "ipc:///tmp/zeromq4-clone-pattern-test-pub.socket"
        routerAddr = "ipc:///tmp/zeromq4-clone-pattern-test-router.socket"

    pushC <- newEmptyMVar

    withAsync (publisher pubAddr routerAddr pushC) $ \_ ->
      withAsync (putMVar pushC val) $ \_ -> do
        m <- queryLastState routerAddr
        return $ property (m == val)
