-- |
-- Use this module instead of @Network.Wreq@ to use string-like datatypes.
--
-- see https://hackage.haskell.org/package/wreq/docs/Network-Wreq.html
--
module Network.Wreq.StringLess
    (
    -- * HTTP verbs

    -- ** Sessions
    -- $session

    -- ** GET
      get
    , getWith
    -- ** POST
    -- $postable
    , post
    , postWith
    -- ** HEAD
    , head_
    , headWith
    -- ** OPTIONS
    , options
    , optionsWith
    -- ** PUT
    , put
    , putWith
    -- ** DELETE
    , delete
    , deleteWith
    -- ** Custom Method
    , customMethod
    , customMethodWith
    , customHistoriedMethod
    , customHistoriedMethodWith
    -- ** Custom Payload Method
    , customPayloadMethod
    , customPayloadMethodWith
    , customHistoriedPayloadMethod
    , customHistoriedPayloadMethodWith
    -- * Incremental consumption of responses
    -- ** GET
    , foldGet
    , foldGetWith

    -- * Configuration
    , Wreq.Options
    , Wreq.defaults
    , Wreq.manager
    , Wreq.header
    , Wreq.param
    , Wreq.redirects
    , Wreq.headers
    , Wreq.params
    , Wreq.cookie
    , Wreq.cookies
    , Wreq.checkResponse

    -- ** Authentication
    -- $auth
    , Wreq.Auth
    , Wreq.AWSAuthVersion(..)
    , Wreq.auth
    , Wreq.basicAuth
    , Wreq.oauth1Auth
    , Wreq.oauth2Bearer
    , Wreq.oauth2Token
    , Wreq.awsAuth
    -- ** Proxy settings
    , Wreq.Proxy(Proxy)
    , Wreq.proxy
    , Wreq.httpProxy
    -- ** Using a manager with defaults
    , Wreq.withManager

  -- * Payloads for POST and PUT
    , Payload(..)
    -- ** URL-encoded form data
    , FormParam(..)
    , FormValue
    -- ** Multipart form data
    , Wreq.Part
    , Wreq.partName
    , Wreq.partFileName
    , Wreq.partContentType
    , Wreq.partGetBody
    -- *** Smart constructors
    , Wreq.partBS
    , Wreq.partLBS
    , Wreq.partText
    , Wreq.partString
    , Wreq.partFile
    , Wreq.partFileSource

    -- * Responses
    , Wreq.Response
    , Wreq.responseBody
    , Wreq.responseHeader
    , Wreq.responseLink
    , Wreq.responseCookie
    , Wreq.responseHeaders
    , Wreq.responseCookieJar
    , Wreq.responseStatus
    , Wreq.Status
    , Wreq.statusCode
    , Wreq.statusMessage
    -- ** Link headers
    , Wreq.Link
    , Wreq.linkURL
    , Wreq.linkParams
    -- ** Decoding responses
    , JSONError(..)
    , Wreq.asJSON
    , Wreq.asValue

    -- * Cookies
    -- $cookielenses
    , Wreq.Cookie
    , Wreq.cookieName
    , Wreq.cookieValue
    , Wreq.cookieExpiryTime
    , Wreq.cookieDomain
    , Wreq.cookiePath

    -- * Parsing responses
    , Wreq.atto
    , Wreq.atto_
    ) where

import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Lazy               as LBS
import qualified Network.Wreq                       as Wreq
import           Network.Wreq.StringLess.StringLike
import           Network.Wreq.Types

get :: StringLike s => s -> IO (Wreq.Response LBS.ByteString)
get = Wreq.get . toString


getWith :: StringLike s => Wreq.Options -> s -> IO (Wreq.Response LBS.ByteString)
getWith opts = Wreq.getWith opts . toString


post :: StringLike s => Postable a => s -> a -> IO (Wreq.Response LBS.ByteString)
post = Wreq.post . toString


postWith :: StringLike s => Postable a => Wreq.Options -> s -> a -> IO (Wreq.Response LBS.ByteString)
postWith opts = Wreq.postWith opts . toString


head_ :: StringLike s => s -> IO (Wreq.Response ())
head_ = Wreq.head_ . toString


headWith :: StringLike s => Wreq.Options -> s -> IO (Wreq.Response ())
headWith opts = Wreq.headWith opts . toString


options :: StringLike s => s -> IO (Wreq.Response ())
options = Wreq.options . toString


optionsWith :: StringLike s => Wreq.Options -> s -> IO (Wreq.Response ())
optionsWith opts = Wreq.optionsWith opts . toString


put :: StringLike s => Putable a => s -> a -> IO (Wreq.Response LBS.ByteString)
put = Wreq.put . toString


putWith :: StringLike s => Putable a => Wreq.Options -> s -> a -> IO (Wreq.Response LBS.ByteString)
putWith opts = Wreq.putWith opts . toString


delete :: StringLike s => s -> IO (Wreq.Response LBS.ByteString)
delete = Wreq.delete . toString


deleteWith :: StringLike s => Wreq.Options -> s -> IO (Wreq.Response LBS.ByteString)
deleteWith opts = Wreq.deleteWith opts . toString


customMethod :: StringLike s => s -> s -> IO(Wreq.Response LBS.ByteString)
customMethod method url = Wreq.customMethod (toString method) (toString url)


customMethodWith :: StringLike s => s -> Wreq.Options -> s -> IO (Wreq.Response LBS.ByteString)
customMethodWith method opts url = Wreq.customMethodWith (toString method) opts (toString url)


customHistoriedMethod :: StringLike s => s -> s -> IO (Wreq.HistoriedResponse LBS.ByteString)
customHistoriedMethod method url = Wreq.customHistoriedMethod (toString method) (toString url)


customHistoriedMethodWith :: StringLike s => s -> Wreq.Options -> s -> IO (Wreq.HistoriedResponse LBS.ByteString)
customHistoriedMethodWith method opts url = Wreq.customHistoriedMethodWith (toString method) opts (toString url)


customPayloadMethod :: StringLike s => Postable a => s -> s -> a -> IO (Wreq.Response LBS.ByteString)
customPayloadMethod method url = Wreq.customPayloadMethod (toString method) (toString url)


customPayloadMethodWith :: StringLike s => Postable a => s -> Options -> s -> a -> IO (Wreq.Response LBS.ByteString)
customPayloadMethodWith method opts url = Wreq.customPayloadMethodWith (toString method) opts (toString url)


customHistoriedPayloadMethod :: (Postable a, StringLike s) => s -> s -> a -> IO (Wreq.HistoriedResponse LBS.ByteString)
customHistoriedPayloadMethod method url = Wreq.customHistoriedPayloadMethod (toString method) (toString url)


customHistoriedPayloadMethodWith :: (Postable a, StringLike s) => s -> Wreq.Options -> s -> a -> IO (Wreq.HistoriedResponse LBS.ByteString)
customHistoriedPayloadMethodWith method opts url = Wreq.customHistoriedPayloadMethodWith (toString method) opts (toString url)


foldGet :: StringLike s => (a -> BS.ByteString -> IO a) -> a -> s -> IO a
foldGet f z url = Wreq.foldGet f z (toString url)


foldGetWith :: StringLike s => Options -> (a -> BS.ByteString -> IO a) -> a -> s -> IO a
foldGetWith opts f z url = Wreq.foldGetWith opts f z (toString url)
