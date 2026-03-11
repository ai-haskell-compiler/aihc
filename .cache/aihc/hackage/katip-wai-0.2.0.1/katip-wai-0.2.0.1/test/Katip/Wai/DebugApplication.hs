module Katip.Wai.DebugApplication
  ( DebugApplication
  , sendNoContentRequest
  , sendLogRequest
  , sendNotFoundRequest
  , withDebugApplication
  )
where

import Control.Exception (bracket)
import Control.Monad (join)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Coerce (coerce)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Katip (KatipContextT, runKatipContextT)
import qualified Katip
import Katip.Wai (ApplicationT, runApplication)
import qualified Katip.Wai
import Katip.Wai.DebugScribe (withDebugScribe)
import qualified Network.HTTP.Client as Http
import Network.HTTP.Types.Method (Method)
import qualified Network.HTTP.Types.Status as Status
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Test.Hspec (shouldBe)


newtype DebugApplication = DebugApplication Warp.Port


toUrl :: DebugApplication -> String
toUrl (DebugApplication port) = "http://localhost:" <> show port


sendLogRequest :: Http.Manager -> DebugApplication -> Method -> Text -> IO ()
sendLogRequest manager app method message = do
  baseRequest <- Http.parseRequest $ toUrl app <> "/log"
  let request =
        Http.setQueryString [("message", Just $ encodeUtf8 message)] $
          baseRequest{Http.method = method}

  response <- Http.httpLbs request manager

  let status = Http.responseStatus response
   in status `shouldBe` Status.status202

  let headers = Http.responseHeaders response
      contentType = lookup "Content-Type" headers
   in contentType `shouldBe` Just "text/plain; charset=UTF-8"

  let bytes = LazyByteString.toStrict $ Http.responseBody response
      body = decodeUtf8With lenientDecode bytes
   in body `shouldBe` message


sendNoContentRequest :: Http.Manager -> DebugApplication -> Method -> IO ()
sendNoContentRequest manager app method = do
  baseRequest <- Http.parseRequest $ toUrl app
  let request = baseRequest{Http.method = method}

  response <- Http.httpLbs request manager

  let body = Http.responseBody response
  body `shouldBe` mempty

  let status = Http.responseStatus response
  status `shouldBe` Status.status204


sendNotFoundRequest :: Http.Manager -> DebugApplication -> Method -> IO ()
sendNotFoundRequest manager app method = do
  baseRequest <- Http.parseRequest $ toUrl app <> "/not-found"
  let request = baseRequest{Http.method = method}

  response <- Http.httpLbs request manager

  let body = Http.responseBody response
  body `shouldBe` mempty

  let status = Http.responseStatus response
  status `shouldBe` Status.status404


mkApplication :: Katip.Severity -> ApplicationT (KatipContextT IO)
mkApplication severity =
  let base request send =
        case Wai.pathInfo request of
          ["log"] ->
            let queryString = Wai.queryString request
                message = Maybe.fromMaybe "Example log message" $ do
                  bytes <- join $ lookup "message" queryString
                  pure $ decodeUtf8With lenientDecode bytes
             in do
                  Katip.logFM severity (Katip.ls message)
                  send $
                    Wai.responseLBS
                      (toEnum 202)
                      [("Content-Type", "text/plain; charset=UTF-8")]
                      (LazyByteString.fromStrict $ encodeUtf8 message)
          [] ->
            send $ Wai.responseLBS (toEnum 204) [] mempty
          _ ->
            send $ Wai.responseLBS (toEnum 404) [] mempty
   in Katip.Wai.middleware severity base


withLogEnv :: (Katip.LogEnv -> IO a) -> IO [Aeson.Value]
withLogEnv useLogEnv = do
  withDebugScribe (Katip.permitItem minBound) Katip.V3 $ \scribe ->
    let makeLogEnv =
          Katip.initLogEnv "debug-app" "local-test"
            >>= Katip.registerScribe "debug" scribe Katip.defaultScribeSettings
     in bracket makeLogEnv Katip.closeScribes useLogEnv


withDebugApplication :: Katip.Severity -> (DebugApplication -> IO a) -> IO [Aeson.Value]
withDebugApplication severity useApp =
  withLogEnv $ \logEnv ->
    let toIO = runKatipContextT logEnv () "spec"
        app = runApplication toIO (mkApplication severity)
     in Warp.testWithApplication (pure app) (useApp . coerce)
