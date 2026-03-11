module Katip.Wai.Response
  ( Response (..)
  , traceResponse
  ) where

import Katip.Wai.Request (Request)
import qualified Katip.Wai.Request as Request

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Time as Time
import qualified Network.HTTP.Types as HttpTypes
import qualified Network.Wai as Wai
import qualified System.Clock as Clock


-- | Response that was sent back to client.
data Response = Response
  { status :: HttpTypes.Status
  -- ^ The HTTP status code of the response.
  , responseHeaders :: HttpTypes.ResponseHeaders
  -- ^ The headers in the response. Be careful not to log any sensntive headers.
  , respondedAt :: Time.UTCTime
  -- ^ The time server responded.
  , responseTime :: Clock.TimeSpec
  -- ^ How long it took the server to respond.
  }
  deriving (Show, Eq)


-- | Trace a response and time how long it took to process a request.
traceResponse :: MonadIO m => Request -> Wai.Response -> m Response
traceResponse request response = do
  endedAt <- liftIO $ Clock.getTime Clock.Monotonic
  respondedAt <- liftIO Time.getCurrentTime

  let responseTime = endedAt `Clock.diffTimeSpec` Request.startedAt request

  pure
    Response
      { status = Wai.responseStatus response
      , responseHeaders = Wai.responseHeaders response
      , respondedAt
      , responseTime
      }
