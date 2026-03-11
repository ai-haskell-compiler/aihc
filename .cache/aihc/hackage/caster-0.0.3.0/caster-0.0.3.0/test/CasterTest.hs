{-# LANGUAGE BangPatterns #-}
module CasterTest where

import           System.Log.Caster
import qualified System.Log.Caster.Monad     as CM

import           Test.QuickCheck.Instances
import           Test.QuickCheck.Monadic     as QCM
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Reader        (MonadReader (..), ReaderT,
                                              runReaderT)
import qualified Data.ByteString             as SBS
import qualified Data.ByteString.Builder     as BB
import qualified Data.ByteString.FastBuilder as FB
import qualified Data.ByteString.Lazy        as LBS
import           Data.Semigroup
import qualified Data.Text                   as ST
import qualified Data.Text.Encoding          as STE
import qualified Data.Text.Lazy              as LT
import qualified Data.Text.Lazy.Encoding     as LTE
import           System.Directory
import           System.IO

logLevels :: [LogLevel]
logLevels = [LogDebug, LogInfo, LogNotice, LogWarn, LogError, LogCritical, LogAlert, LogEmergency]

prop_toBuilder_String :: String -> Bool
prop_toBuilder_String str = (LT.unpack . LTE.decodeUtf8 . FB.toLazyByteString $ toBuilder str) == str

prop_toBuilder_strict_Text :: ST.Text -> Bool
prop_toBuilder_strict_Text str = (LT.toStrict . LTE.decodeUtf8 . FB.toLazyByteString $ toBuilder str) == str

prop_toBuilder_lazy_Text :: LT.Text -> Bool
prop_toBuilder_lazy_Text str = (LTE.decodeUtf8 . FB.toLazyByteString $ toBuilder str) == str

prop_toBuilder_strict_ByteString :: SBS.ByteString -> Bool
prop_toBuilder_strict_ByteString str = FB.toStrictByteString (toBuilder str) == str

prop_toBuilder_lazy_ByteString :: LBS.ByteString -> Bool
prop_toBuilder_lazy_ByteString str = FB.toLazyByteString (toBuilder str) == str

data TestShow = Foo | Bar | Baz deriving (Show)
instance Arbitrary TestShow where
  arbitrary = elements [Foo, Bar, Baz]

prop_toBuilder_Show :: TestShow -> Bool
prop_toBuilder_Show x = FB.toLazyByteString (toBuilder x) == (LTE.encodeUtf8 . LT.pack $ show x)

prop_concat :: String -> String -> Bool
prop_concat str0 str1 = (LT.unpack . LTE.decodeUtf8 . FB.toLazyByteString $ str0 <:> str1) == str0 <> str1

instance Arbitrary LogLevel where
  arbitrary = elements logLevels

prop_broadcastLog :: [(LogLevel, String)] -> Property
prop_broadcastLog msgs = monadicIO $ do
  result <- run $ do
    chan <- newLogChan
    LogQueue lq' <- newLogQueue
    let lq = LogQueue lq'

    (file0, handle0) <- openTempFile "/tmp" "caster_test_0.log"
    let listener0 = handleListenerFlush defaultFormatter handle0
    thread0 <- forkIO $ relayLog chan LogDebug listener0

    (file1, handle1) <- openTempFile "/tmp" "caster_test_1.log"
    let listener1 = handleListenerFlush defaultFormatter handle1
    thread1 <- forkIO $ relayLog chan LogDebug listener1

    (file2, handle2) <- openTempFile "/tmp" "caster_test_2.log"
    let listener2 = handleListenerFlush defaultFormatter handle2
    thread2 <- forkIO $ relayLog chan LogDebug listener2

    threadb <- forkIO $ broadcastLog lq chan

    mapM_ (uncurry $ logAs lq) msgs
    atomically $ wait lq'

    hClose handle0
    hClose handle1
    hClose handle2

    log0 <- SBS.readFile file0
    log1 <- SBS.readFile file1
    log2 <- SBS.readFile file2

    let !res = log0 == log1 && log1 == log2

    killThread thread0
    killThread thread1
    killThread thread2
    killThread threadb


    removeFile file0
    removeFile file1
    removeFile file2

    pure res
  QCM.assert result
  where
    wait q = tryPeekTQueue q >>= \case
      Just _ -> wait q
      Nothing -> pure ()

unit_stdout :: IO ()
unit_stdout = testListenrWith stdoutListener

unit_terminal :: IO ()
unit_terminal = testListenrWith terminalListener

testListenrWith :: Listener -> IO ()
testListenrWith l = do
  chan <- newLogChan
  lq <- newLogQueue
  thread0 <- forkIO $ relayLog chan LogDebug l
  threadb <- forkIO $ broadcastLog lq chan
  forM_ logLevels (\l -> logAs lq l "nyaan")
  threadDelay 100000
  killThread thread0
  killThread threadb


instance CM.MonadCaster (ReaderT LogQueue IO) where
  getLogQueue = ask

unit_monad :: IO ()
unit_monad = do
  chan <- newLogChan
  lq <- newLogQueue
  thread0 <- forkIO $ relayLog chan LogDebug stdoutListener
  flip runReaderT lq $ do
    CM.debug "mona"
    CM.info "mona"
    CM.notice "mona"
    CM.warn "mona"
    CM.err "mona"
    CM.critical "mona"
    CM.alert "mona"
    CM.emergency "mona"
  threadb <- forkIO $ broadcastLog lq chan
  threadDelay 100001
  killThread threadb
  killThread thread0

