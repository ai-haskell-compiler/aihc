module Katip.Wai.Request
  ( Request (..)
  , traceRequest
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Time as Time
import Data.UUID (UUID)
import qualified Data.UUID.V4 as V4
import qualified Network.HTTP.Types as HttpTypes
import qualified Network.Socket as Socket
import qualified Network.Wai as Wai
import qualified System.Clock as Clock


-- | An incoming http request.
data Request = Request
  { traceId :: UUID
  -- ^ Unique identifier for the request.
  , method :: HttpTypes.Method
  -- ^ HTTP request method, ie 'GET', 'HEAD', 'POST', 'PUT', 'DELETE', 'CONNECT', 'OPTIONS', 'TRACE', or 'PATCH'.
  , httpVersion :: HttpTypes.HttpVersion
  -- ^ HTTP version that was used for this request.
  , rawPathInfo :: ByteString
  -- ^ Raw path info for this request.
  , requestHeaders :: HttpTypes.RequestHeaders
  -- ^ All of the headers that were sent in this request. Be careful not log any sensitive headers, like API Keys.
  , isSecure :: Bool
  -- ^ Set to 'True' if the connection used https.
  , remoteHost :: Socket.SockAddr
  -- ^ The remote host the request was sent from.
  , pathInfo :: [Text]
  -- ^ Same as 'rawPathInfo' except this is broken up into a list.
  , queryString :: HttpTypes.Query
  -- ^ The query string from the request.
  , receivedAt :: Time.UTCTime
  -- ^ The time the request was received on the server.
  , startedAt :: Clock.TimeSpec
  -- ^ The time the server started processing the request. You can probably ignore this, it's only here so we can time how long it takes to respond.
  }
  deriving (Show, Eq)


-- | Trace a 'Wai.Request' by assigning it a unique 'UUID' and capture information about the request.
traceRequest :: MonadIO m => Wai.Request -> m Request
traceRequest waiRequest = do
  traceId <- liftIO V4.nextRandom
  receivedAt <- liftIO Time.getCurrentTime
  startedAt <- liftIO $ Clock.getTime Clock.Monotonic

  let request =
        Request
          { traceId = traceId
          , receivedAt = receivedAt
          , startedAt = startedAt
          , method = Wai.requestMethod waiRequest
          , httpVersion = Wai.httpVersion waiRequest
          , rawPathInfo = Wai.rawPathInfo waiRequest
          , requestHeaders = Wai.requestHeaders waiRequest
          , isSecure = Wai.isSecure waiRequest
          , remoteHost = Wai.remoteHost waiRequest
          , pathInfo = Wai.pathInfo waiRequest
          , queryString = Wai.queryString waiRequest
          }

  pure request
