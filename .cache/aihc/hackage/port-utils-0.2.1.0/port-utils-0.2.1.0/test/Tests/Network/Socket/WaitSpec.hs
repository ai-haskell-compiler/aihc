{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
module Tests.Network.Socket.WaitSpec where
import qualified Network.Socket.Wait.Internal as W
import qualified Network.Socket.Free as F
import qualified Network.Socket as N
import qualified Control.Concurrent as C
import qualified Control.Exception as E
import qualified Test.Hspec as H
import qualified System.Timeout as T
import qualified Control.Concurrent.Async as A
import qualified Control.Monad.Trans.State as St
import qualified Control.Monad as M

instance Monad m => Semigroup (St.StateT s m ()) where
  (<>) = (>>)

instance Monad m => Monoid (St.StateT s m ()) where
  mempty = pure ()
  mappend = (<>)

blockPort :: N.Socket -> IO ()
blockPort sock = E.bracket F.openFreePort (N.close . snd) $ \(port1, sock1) -> do
  N.listen sock1 1
  N.connect sock $ N.SockAddrInet (fromIntegral port1) $ N.tupleToHostAddress (127,0,0,1)
  M.forever $ C.threadDelay 100000000

testAtFirstUnavailable :: String -> (IO () -> Int -> IO ()) -> H.SpecWith (Int, N.Socket)
testAtFirstUnavailable message test = H.describe (message ++ " when the port is") $ do
   H.it "free" $ \(port, sock) -> N.close sock >> test (pure ()) port
   H.it "is bound and CLOSED" $ \(port, sock) -> test (N.close sock) port
   H.it "is in a state other than listening" $ \(port, sock) ->
     A.withAsync (blockPort sock) $ \thread -> test (A.cancel thread >> N.close sock) port

testUnavailable :: String -> (Int -> IO ()) -> H.SpecWith (Int, N.Socket)
testUnavailable message = testAtFirstUnavailable message . const

withListeningSocket :: Int -> IO () -> IO ()
withListeningSocket port action = E.bracket (N.socket N.AF_INET N.Stream N.defaultProtocol) N.close $ \sock -> do
  N.setSocketOption sock N.ReuseAddr 1
  (M.void $ N.bind sock $ N.SockAddrInet (fromIntegral port) $ N.tupleToHostAddress (127,0,0,1))
    `E.onException` (putStrLn "bad stuff")
  N.listen sock 1
  action

spec :: H.Spec
spec = do
  -- Black box tests of wait with all the caveats. Just a little bit of sanity testing.
  H.describe "wait" $ H.before F.openFreePort $ H.after (N.close . snd) $ do
    testUnavailable "never returns" $ \port -> do
      T.timeout 100000 (W.wait "127.0.0.1" port) >>= \case
        Nothing -> pure ()
        Just () -> fail "wait returned! I should have blocked forever!"

    H.it "does connect if port is available" $ \(port, sock) -> do
      N.listen sock 128
      W.wait "127.0.0.1" port

    testAtFirstUnavailable "at first does not return" $ \unblock port -> do
      A.withAsync (W.wait "127.0.0.1" port) $ \thread -> do
        C.threadDelay 100000
        -- Ensure wait is blocking the thread from completing
        A.poll thread >>= \case
          Just _ -> fail "wait returned when the port was unavailable!"
          Nothing -> pure ()
        -- Free the port
        unblock
        -- Ensure wait is able to connect when the socket is in a listening state.
        withListeningSocket port $ A.wait thread

    H.it "throw if the port is invalid" $ \(_, _) ->
      W.wait "localhost" 0 `H.shouldThrow` (\(_ :: IOError) -> True)

    H.it "throws if the host does not exist" $ \(_, _) ->
      W.wait "invalid." 3000 `H.shouldThrow` (\(_ :: IOError) -> True)

  H.describe "connectAction" $ H.before F.openFreePort $ H.after (N.close . snd) $ do
    testUnavailable "return False" $ \port ->
      W.connectAction "127.0.0.1" port `H.shouldReturn` False

    H.it "returns True if it connects" $ \(port, sock) -> do
      N.listen sock 128
      W.connectAction "127.0.0.1" port `H.shouldReturn` True

    testAtFirstUnavailable "returns True if connects after failing" $ \unblock port-> do
      W.connectAction "127.0.0.1" port `H.shouldReturn` False

      unblock

      withListeningSocket port $
         W.connectAction "127.0.0.1" port `H.shouldReturn` True

#ifdef linux_HOST_OS
    H.it "returns false if there is a connection timeout" $ \_ -> do
      W.connectAction "93.184.216.34" 80 `H.shouldReturn` False
#endif

  H.describe "waitM" $ do
    H.it "returns immediantly if the action returns True" $
      flip St.execState False (W.waitM mempty (St.put True) (pure True)) `H.shouldBe` False

    H.it "loops until the action returns True" $ do
      let theAction = do
            St.get >>= \case
              0 -> St.put 1 >> pure False
              _ -> St.put 2 >> pure True

      flip St.execState 0 (W.waitM mempty (pure ()) theAction) `H.shouldBe` (2 :: Int)

main :: IO ()
main = H.hspec spec