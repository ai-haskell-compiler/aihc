module Test.WithExtras
  ( testWithTLSApplication
  , testWithTLSApplicationSettings
  , withTLSApplication
  , withTLSApplicationSettings
  )
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad (when)
import Network.Socket (Socket, close)
import Network.Wai (Application)
import Network.Wai.Handler.Warp
  ( Port
  , Settings
  , defaultSettings
  , defaultShouldDisplayException
  , openFreePort
  )
import Network.Wai.Handler.Warp.Internal (settingsBeforeMainLoop)
import Network.Wai.Handler.WarpTLS
  ( runTLSSocket
  , tlsSettings
  )
import Test.Certs.Temp (certificatePath, keyPath, withCertPathsInTmp')


runTLSSocket' :: Settings -> Socket -> Application -> IO ()
runTLSSocket' settings sock app = withCertPathsInTmp' $ \cp -> do
  let tls = tlsSettings (certificatePath cp) (keyPath cp)
  runTLSSocket tls settings sock app


{- | Runs the given 'Application' on a free port. Passes the port to the given
 operation and executes it, while the 'Application' is running. Shuts down the
 server before returning.
-}
withTLSApplication :: IO Application -> (Port -> IO a) -> IO a
withTLSApplication = withTLSApplicationSettings defaultSettings


{- | 'withTLSApplication' with given 'Settings'. This will ignore the port value
 set by 'setPort' in 'Settings'.
-}
withTLSApplicationSettings :: Settings -> IO Application -> (Port -> IO a) -> IO a
withTLSApplicationSettings settings' mkApp action = do
  app <- mkApp
  withFreePort $ \(port, sock) -> do
    started <- mkWaiter
    let settings =
          settings'
            { settingsBeforeMainLoop =
                notify started () >> settingsBeforeMainLoop settings'
            }
    result <-
      race
        (runTLSSocket' settings sock app)
        (waitFor started >> action port)
    case result of
      Left () -> throwIO $ ErrorCall "Unexpected: runSettingsSocket exited"
      Right x -> return x


testWithTLSApplication :: IO Application -> (Port -> IO a) -> IO a
testWithTLSApplication = testWithTLSApplicationSettings defaultSettings


testWithTLSApplicationSettings :: Settings -> IO Application -> (Port -> IO a) -> IO a
testWithTLSApplicationSettings settings mkApp action = do
  callingThread <- myThreadId
  app <- mkApp
  let wrappedApp request respond =
        app request respond `catch` \e -> do
          when
            (defaultShouldDisplayException e)
            (throwTo callingThread e)
          throwIO e
  withTLSApplicationSettings settings (return wrappedApp) action


data Waiter a = Waiter
  { notify :: a -> IO ()
  , waitFor :: IO a
  }


mkWaiter :: IO (Waiter a)
mkWaiter = do
  mvar <- newEmptyMVar
  return
    Waiter
      { notify = putMVar mvar
      , waitFor = readMVar mvar
      }


-- | Like 'openFreePort' but closes the socket before exiting.
withFreePort :: ((Port, Socket) -> IO a) -> IO a
withFreePort = bracket openFreePort (close . snd)
