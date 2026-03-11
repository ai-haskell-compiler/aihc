{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Data.Store.StreamingSpec where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (race, concurrently)
import           Control.Concurrent.MVar
import           Control.Exception (try)
import           Control.Monad (void, (<=<), forM_)
import           Control.Monad.Trans.Free (runFreeT, FreeF(..))
import           Control.Monad.Trans.Free.Church (fromFT)
import           Control.Monad.Trans.Resource
import qualified Data.ByteString as BS
import           Data.Conduit ((=$=), ($$))
import qualified Data.Conduit.List as C
import           Data.List (unfoldr)
import           Data.Monoid
import           Data.Store.Internal
import           Data.Store.Streaming
import           Data.Store.Streaming.Internal
import           Data.Streaming.Network (runTCPServer, runTCPClient, clientSettingsTCP, serverSettingsTCP, setAfterBind)
import           Data.Streaming.Network.Internal (AppData(..))
import           Data.Void (absurd, Void)
import           Network.Socket.ByteString (send)
import qualified System.IO.ByteBuffer as BB
import           System.Posix.Types (Fd(..))
import           Test.Hspec
import           Test.Hspec.SmallCheck
import           Test.SmallCheck

#if MIN_VERSION_network(3,0,0)
import           Network.Socket (Socket, socketPort, withFdSocket)
#else
import           Network.Socket (Socket(..), socketPort)
import           Foreign.C.Types (CInt)
#endif

spec :: Spec
spec = do
  describe "conduitEncode and conduitDecode" $ do
    it "Roundtrips ([Int])." $ property roundtrip
    it "Roundtrips ([Int]), with chunked transfer." $ property roundtripChunked
    it "Throws an Exception on incomplete messages." conduitIncomplete
    it "Throws an Exception on excess input." $ property conduitExcess
  describe "peekMessage" $ do
    describe "ByteString" $ do
      it "demands more input when needed." $ property (askMoreBS (headerLength + 1))
      it "demands more input on incomplete message magic." $ property (askMoreBS 1)
      it "demands more input on incomplete SizeTag." $ property (askMoreBS (headerLength - 1))
      it "successfully decodes valid input." $ property canPeekBS
  describe "decodeMessage" $ do
    describe "ByteString" $ do
      it "Throws an Exception on incomplete messages." decodeIncomplete
      it "Throws an Exception on messages that are shorter than indicated." decodeTooShort
#ifndef mingw32_HOST_OS
    describe "Socket" $ do
      it "Decodes data trickling through a socket." $ property decodeTricklingMessageFd
#endif

roundtrip :: [Int] -> Property IO
roundtrip xs = monadic $ do
  xs' <- runResourceT $ C.sourceList xs
    =$= C.map Message
    =$= conduitEncode
    =$= conduitDecode Nothing
    =$= C.map fromMessage
    $$ C.consume
  return $ xs' == xs

roundtripChunked :: [Int] -> Property IO
roundtripChunked input = monadic $ do
  let (xs, chunkLengths) = splitAt (length input `div` 2) input
  bs <- C.sourceList xs
    =$= C.map Message
    =$= conduitEncode
    $$ C.fold (<>) mempty
  let chunks = unfoldr takeChunk (bs, chunkLengths)
  xs' <- runResourceT $ C.sourceList chunks
    =$= conduitDecode (Just 10)
    =$= C.map fromMessage
    $$ C.consume
  return $ xs' == xs
  where
    takeChunk (x, _) | BS.null x = Nothing
    takeChunk (x, []) = Just (x, (BS.empty, []))
    takeChunk (x, l:ls) =
        let (chunk, rest) = BS.splitAt l x
        in Just (chunk, (rest, ls))

conduitIncomplete :: Expectation
conduitIncomplete =
    (runResourceT (C.sourceList [incompleteInput]
                  =$= conduitDecode (Just 10)
                  $$ C.consume)
    :: IO [Message Integer]) `shouldThrow` \PeekException{} -> True

conduitExcess :: [Int] -> Property IO
conduitExcess xs = monadic $ do
  bs <- C.sourceList xs
    =$= C.map Message
    =$= conduitEncode
    $$ C.fold (<>) mempty
  res <- try (runResourceT (C.sourceList [bs `BS.append` "excess bytes"]
                            =$= conduitDecode (Just 10)
                            $$ C.consume) :: IO [Message Int])
  case res of
      Right _ -> return False
      Left (PeekException _ _) -> return True

-- splits an encoded message after n bytes.  Feeds the first part to
-- peekResult, expecting it to require more input.  Then, feeds the
-- second part and checks if the decoded result is the original
-- message.
askMoreBS :: Int -> Integer -> Property IO
askMoreBS n x = monadic $ BB.with (Just 10) $ \ bb -> do
  let bs = encodeMessage (Message x)
      (start, end) = BS.splitAt n $ bs
  BB.copyByteString bb start
  peekResult <- runFreeT (fromFT (peekMessageBS bb))
  case peekResult of
    Free cont ->
      runFreeT (cont end) >>= \case
        Pure (Message x') -> return $ x' == x
        Free _ -> return False
    Pure _ -> return False

canPeekBS :: Integer -> Property IO
canPeekBS x = monadic $ BB.with (Just 10) $ \ bb -> do
  let bs = encodeMessage (Message x)
  BB.copyByteString bb bs
  peekResult <- runFreeT (fromFT (peekMessageBS bb))
  case peekResult of
    Free _ -> return False
    Pure (Message x') -> return $ x' == x

#ifndef mingw32_HOST_OS
#if !MIN_VERSION_network(3,0,0)
withFdSocket :: Socket -> (CInt -> IO r) -> IO r
withFdSocket (MkSocket fd _ _ _ _) f = f fd
#endif

withServer :: (Socket -> Socket -> IO a) -> IO a
withServer cont = do
  sock1Var :: MVar Socket <- newEmptyMVar
  sock2Var :: MVar Socket <- newEmptyMVar
  portVar :: MVar Int <- newEmptyMVar
  doneVar :: MVar Void <- newEmptyMVar
  let adSocket ad = case appRawSocket' ad of
        Nothing -> error "withServer.adSocket: no raw socket in AppData"
        Just sock -> sock
  let ss = setAfterBind
        (putMVar portVar . fromIntegral <=< socketPort)
        (serverSettingsTCP 0 "127.0.0.1")
  x <- fmap (either (either absurd absurd) id) $ race
    (race
      (runTCPServer ss $ \ad -> do
        putMVar sock1Var (adSocket ad)
        void (readMVar doneVar))
      (do port <- takeMVar portVar
          runTCPClient (clientSettingsTCP port "127.0.0.1") $ \ad -> do
            putMVar sock2Var (adSocket ad)
            readMVar doneVar))
    (do sock1 <- takeMVar sock1Var
        sock2 <- takeMVar sock2Var
        cont sock1 sock2)
  putMVar doneVar (error "withServer: impossible: read from doneVar")
  return x

decodeTricklingMessageFd :: Integer -> Property IO
decodeTricklingMessageFd v = monadic $ do
  let bs = encodeMessage (Message v)
  BB.with Nothing $ \bb ->
    withServer $ \sock1 sock2 -> do
      let generateChunks :: [Int] -> BS.ByteString -> [BS.ByteString]
          generateChunks xs0 bs_ = case xs0 of
            [] -> generateChunks [1,3,10] bs_
            x : xs -> if BS.null bs_
              then []
              else BS.take x bs_ : generateChunks xs (BS.drop x bs_)
      let chunks = generateChunks [] bs
      ((), Message v') <- concurrently
        (forM_ chunks $ \chunk -> do
          void (send sock1 chunk)
          threadDelay (10 * 1000))
        (withFdSocket sock2 (decodeMessageFd bb . Fd))
      return (v == v')

#endif

decodeIncomplete :: IO ()
decodeIncomplete = BB.with (Just 0) $ \ bb -> do
  BB.copyByteString bb (BS.take 1 incompleteInput)
  (decodeMessageBS bb (return Nothing) :: IO (Maybe (Message Integer)))
    `shouldThrow` \PeekException{} -> True

incompleteInput :: BS.ByteString
incompleteInput =
  let bs = encodeMessage (Message (42 :: Integer))
  in BS.take (BS.length bs - 1) bs

decodeTooShort :: IO ()
decodeTooShort = BB.with Nothing $ \bb -> do
    BB.copyByteString bb (encodeMessageTooShort . Message $ (1 :: Int))
    (decodeMessageBS bb (return Nothing) :: IO (Maybe (Message Int)))
        `shouldThrow` \PeekException{} -> True

encodeMessageTooShort :: Message Int -> BS.ByteString
encodeMessageTooShort msg =
    BS.take (BS.length encoded - (getSize (0 :: Int))) encoded
  where
    encoded = encodeMessage msg
