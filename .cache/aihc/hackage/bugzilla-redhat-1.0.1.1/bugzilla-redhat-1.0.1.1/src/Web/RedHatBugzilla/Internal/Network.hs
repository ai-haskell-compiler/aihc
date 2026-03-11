{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.RedHatBugzilla.Internal.Network
( BugzillaServer
, BugzillaApiKey (..)
, BugzillaSession (..)
, BugzillaException (..)
, QueryPart
, Request
, requestUrl
, newBzRequest
, sendBzRequest
) where

import Blaze.ByteString.Builder (toByteString)
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Exception (Exception, throw)
import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Typeable
import Network.HTTP.Simple (defaultRequest, httpLBS, parseRequest)
import Network.HTTP.Conduit (Request(..), Response(..), host, path, port,
                             queryString, requestHeaders, secure)
import Network.HTTP.Types.URI (QueryText, encodePathSegments, renderQueryText)

type BugzillaServer  = T.Text

newtype BugzillaApiKey = BugzillaApiKey T.Text

-- | A session for Bugzilla queries. Use 'anonymousSession' and
-- 'loginSession', as appropriate, to create one.
data BugzillaSession = AnonymousSession BugzillaServer
                     | ApiKeySession BugzillaServer BugzillaApiKey

bzServer :: BugzillaSession -> BugzillaServer
bzServer (AnonymousSession svr) = svr
bzServer (ApiKeySession svr _)   = svr

data BugzillaException
  = BugzillaJSONParseError String
  | BugzillaAPIError Int String
  | BugzillaUnexpectedValue String
  deriving (Show, Typeable)

instance Exception BugzillaException

type QueryPart = (T.Text, Maybe T.Text)

requestUrl :: Request -> B.ByteString
requestUrl req = "https://" <> host req <> path req <> queryString req

sslRequest :: Request
sslRequest =
  defaultRequest {
    secure = True,
    port   = 443
  }

newBzRequest :: BugzillaSession -> [T.Text] -> QueryText -> Request
newBzRequest session methodParts query =
    let req =
          baseRequest {
          path = toByteString $ encodePathSegments $ "rest" : methodParts,
          queryString = toByteString $ renderQueryText True query
          }
    in case session of
         ApiKeySession _ (BugzillaApiKey key) ->
           req { requestHeaders = [("Authorization",
                                    "Bearer " <> TE.encodeUtf8 key)] }
         _ -> req
  where
    -- Try to parse the bzServer first, if it has a scheme then use it as the base request,
    -- otherwise force a secure ssl request.
    baseRequest :: Request
    baseRequest = fromMaybe (sslRequest { host = serverBytes }) (parseRequest serverStr)
    serverBytes = TE.encodeUtf8 serverTxt
    serverStr = T.unpack serverTxt
    serverTxt = bzServer session

data BzError = BzError Int String
               deriving (Eq, Show)

instance FromJSON BzError where
  parseJSON (Object v) = BzError <$> v .: "code"
                                 <*> v .: "message"
  parseJSON _          = mzero

handleError :: String -> BL.ByteString -> IO b
handleError parseError body = do
  let mError = eitherDecode body
  case mError of
    Left _                   -> throw $ BugzillaJSONParseError parseError
    Right (BzError code msg) -> throw $ BugzillaAPIError code msg

sendBzRequest :: FromJSON a => Request -> IO a
sendBzRequest req = runResourceT $ do
  response <- liftIO $ httpLBS req
  let mResult = eitherDecode $ responseBody response
  case mResult of
    Left msg      -> liftIO $ handleError msg (responseBody response)
    Right decoded -> return decoded
