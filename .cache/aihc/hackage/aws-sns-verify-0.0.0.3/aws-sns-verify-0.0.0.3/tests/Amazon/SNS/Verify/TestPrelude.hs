module Amazon.SNS.Verify.TestPrelude
  ( module X
  , module Amazon.SNS.Verify.TestPrelude
  ) where

import Prelude as X

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Data.Text as X (Text)
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import Test.Hspec as X

useCertServer :: IO () -> IO ()
useCertServer action = do
  (setReady, whenReady) <- initReadyState
  race_ (whenReady action) $
    runSettings (setBeforeMainLoop setReady . setPort 3000 $ defaultSettings) $
      \req send ->
        if rawPathInfo req == "/404"
          then send $ responseLBS notFound404 [] ""
          else send $ responseFile ok200 [] "./tests/cert.pem" Nothing
 where
  initReadyState = do
    ready <- newEmptyMVar
    return (putMVar ready (), withMVar ready . const)
