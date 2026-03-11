--
-- HTTP types for use with io-streams and pipes
--
-- Copyright © 2012-2014 Operational Dynamics Consulting, Pty Ltd
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the BSD licence.
--

{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_HADDOCK hide, prune #-}

--
-- | If you're not http-streams or pipes-http and you're importing this,
-- you're Doing It Wrong.
--

module Network.Http.Internal (
    Hostname,
    Port,
    Request(..),
    EntityBody(..),
    ExpectMode(..),
    Response(..),
    StatusCode,
    TransferEncoding(..),
    ContentEncoding(..),
    getStatusCode,
    getStatusMessage,
    getHeader,
    getHeaderMap,
    Method(..),
    Headers,
    emptyHeaders,
    updateHeader,
    removeHeader,
    buildHeaders,
    lookupHeader,
    retrieveHeaders,
    HttpType (getHeaders),
    HttpParseException(..),

    hasBrotli,

    -- for testing
    composeRequestBytes,
    composeResponseBytes
) where

import Prelude hiding (lookup)

import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Builder (copyByteString,
                                                      copyByteString,
                                                      fromByteString,
                                                      fromByteString,
                                                      toByteString)
import qualified Blaze.ByteString.Builder.Char8 as Builder (fromChar,
                                                            fromShow,
                                                            fromString)
import Control.Exception (Exception)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.CaseInsensitive (CI, mk, original)
-- import Data.HashMap.Strict (HashMap, delete, empty, foldrWithKey, insert,
--                             insertWith, lookup, toList)
import qualified Data.Map.Strict as Map
import Data.Map (Map)

import Data.Int (Int64)
import Data.List (foldl')
import Data.Monoid as Mon (mconcat, mempty)
import Data.Word (Word16)

{-
    This is a String because that's what the uri package works in. There
    was a fairly detailed disucssion on haskell-cafe about this, with
    the conclusion that URLs are composed of characters, not octets.
-}

type Hostname = ByteString

type Port = Word16

{-# INLINE hasBrotli #-}
hasBrotli :: Bool
#if defined(MIN_VERSION_brotli_streams)
hasBrotli = True
#else
hasBrotli = False
#endif

--
-- | HTTP Methods, as per RFC 2616
--
data Method
    = GET
    | HEAD
    | POST
    | PUT
    | DELETE
    | TRACE
    | OPTIONS
    | CONNECT
    | PATCH
    | Method ByteString
        deriving (Show, Read, Ord)


instance Eq Method where
    GET          == GET              = True
    HEAD         == HEAD             = True
    POST         == POST             = True
    PUT          == PUT              = True
    DELETE       == DELETE           = True
    TRACE        == TRACE            = True
    OPTIONS      == OPTIONS          = True
    CONNECT      == CONNECT          = True
    PATCH        == PATCH            = True
    GET          == Method "GET"     = True
    HEAD         == Method "HEAD"    = True
    POST         == Method "POST"    = True
    PUT          == Method "PUT"     = True
    DELETE       == Method "DELETE"  = True
    TRACE        == Method "TRACE"   = True
    OPTIONS      == Method "OPTIONS" = True
    CONNECT      == Method "CONNECT" = True
    PATCH        == Method "PATCH"   = True
    Method a     == Method b         = a == b
    m@(Method _) == other            = other == m
    _            == _                = False

--
-- | A description of the request that will be sent to the server. Note
-- unlike other HTTP libraries, the request body is /not/ a part of this
-- object; that will be streamed out by you when actually sending the
-- request with 'sendRequest'.
--
-- 'Request' has a useful @Show@ instance that will output the request
-- line and headers (as it will be sent over the wire but with the @\\r@
-- characters stripped) which can be handy for debugging.
--
-- Note that the actual @Host:@ header is not set until the request is sent,
-- so you will not see it in the Show instance (unless you call 'setHostname'
-- to override the value inherited from the @Connection@).
--
data Request
    = Request {
        qMethod  :: !Method,
        qHost    :: !(Maybe ByteString),
        qPath    :: !ByteString,
        qBody    :: !EntityBody,
        qExpect  :: !ExpectMode,
        qHeaders :: !Headers
    } deriving (Eq)

instance Show Request where
    show q = {-# SCC "Request.show" #-}
        S.unpack $ S.filter (/= '\r') $ Builder.toByteString $ composeRequestBytes q "<default>"


data EntityBody = Empty | Chunking | Static Int64 deriving (Show, Eq, Ord)

data ExpectMode = Normal | Continue deriving (Show, Eq, Ord)

{-
    The bit that builds up the actual string to be transmitted. This
    is on the critical path for every request, so we'll want to revisit
    this to improve performance.

    - Rewrite rule for Method?
    - How can serializing the Headers be made efficient?

    This code includes the RFC compliant CR-LF sequences as line
    terminators, which is why the Show instance above has to bother
    with removing them.
-}

composeRequestBytes :: Request -> ByteString -> Builder
composeRequestBytes q h' =
    mconcat
       [requestline,
        hostLine,
        headerFields,
        crlf]
  where
    requestline = Mon.mconcat
       [method,
        sp,
        uri,
        sp,
        version,
        crlf]

    method = case qMethod q of
        GET     -> Builder.fromString "GET"
        HEAD    -> Builder.fromString "HEAD"
        POST    -> Builder.fromString "POST"
        PUT     -> Builder.fromString "PUT"
        DELETE  -> Builder.fromString "DELETE"
        TRACE   -> Builder.fromString "TRACE"
        OPTIONS -> Builder.fromString "OPTIONS"
        CONNECT -> Builder.fromString "CONNECT"
        PATCH   -> Builder.fromString "PATCH"
        (Method x) -> Builder.fromByteString x

    uri = case qPath q of
        ""   -> Builder.fromChar '/'
        path -> Builder.copyByteString path

    version = Builder.fromString "HTTP/1.1"

    hostLine = mconcat
       [Builder.fromString "Host: ",
        hostname,
        crlf]

    hostname = case qHost q of
        Just x' -> Builder.copyByteString x'
        Nothing -> Builder.copyByteString h'

    headerFields = joinHeaders $ unWrap $ qHeaders q


crlf = Builder.fromString "\r\n"

sp = Builder.fromChar ' '


type StatusCode = Int

--
-- | A description of the response received from the server. Note
-- unlike other HTTP libraries, the response body is /not/ a part
-- of this object; that will be streamed in by you when calling
-- 'receiveResponse'.
--
-- Like 'Request', 'Response' has a @Show@ instance that will output
-- the status line and response headers as they were received from the
-- server.
--
data Response
    = Response {
        pStatusCode       :: !StatusCode,
        pStatusMsg        :: !ByteString,
        pTransferEncoding :: !TransferEncoding,
        pContentEncoding  :: !ContentEncoding,
        pContentLength    :: !(Maybe Int64),
        pHeaders          :: !Headers
    }

instance Show Response where
    show p =     {-# SCC "Response.show" #-}
        S.unpack $ S.filter (/= '\r') $ Builder.toByteString $ composeResponseBytes p


data TransferEncoding = None | Chunked

data ContentEncoding = Identity | Gzip | Deflate | Br {- Brotli -} | UnknownCE !ByteString
    deriving (Show)


--
-- | Get the HTTP response status code.
--
getStatusCode :: Response -> StatusCode
getStatusCode = pStatusCode
{-# INLINE getStatusCode #-}

--
-- | Get the HTTP response status message. Keep in mind that this is
-- /not/ normative; whereas 'getStatusCode' values are authoritative.
--
getStatusMessage :: Response -> ByteString
getStatusMessage = pStatusMsg
{-# INLINE getStatusMessage #-}

--
-- | Lookup a header in the response. HTTP header field names are
-- case-insensitive, so you can specify the name to lookup however you
-- like. If the header is not present @Nothing@ will be returned.
--
-- >     let n = case getHeader p "Content-Length" of
-- >                Just x' -> read x' :: Int
-- >                Nothing -> 0
--
-- which of course is essentially what goes on inside the client library when
-- it receives a response from the server and has to figure out how many bytes
-- to read.
--
-- There is a fair bit of complexity in some of the other HTTP response
-- fields, so there are a number of specialized functions for reading
-- those values where we've found them useful.
--
getHeader :: Response -> ByteString -> Maybe ByteString
getHeader p k =
    lookupHeader h k
  where
    h = pHeaders p

-- | Expose all headers in the response as (case-insenstiive) key-value 'Map'ping.
--
-- @since 0.1.1.0
getHeaderMap :: Response -> Map (CI ByteString) ByteString
getHeaderMap = unWrap . pHeaders

--
-- | Accessors common to both the outbound and return sides of an HTTP
-- connection.
--
class HttpType τ where

    --
    -- | Get the Headers from a Request or Response. Most people do not need
    -- this; for most cases you just need to get a header or two from the
    -- response, for which you can use 'getHeader'. On the other hand, if you
    -- do need to poke around in the raw headers,
    --
    -- @ import Network.Http.Types @
    --
    -- will give you functions like 'lookupHeader' and 'updateHeader' to to
    -- work with.
    --
    getHeaders :: τ -> Headers

instance HttpType Request where
    getHeaders q = qHeaders q

instance HttpType Response where
    getHeaders p = pHeaders p


composeResponseBytes :: Response -> Builder
composeResponseBytes p =
    mconcat
       [statusline,
        headerFields,
        crlf]
  where
    statusline = mconcat
       [version,
        sp,
        code,
        sp,
        message,
        crlf]

    code = Builder.fromShow $ pStatusCode p

    message = Builder.copyByteString $ pStatusMsg p

    version = Builder.fromString "HTTP/1.1"

    headerFields = joinHeaders $ unWrap $ pHeaders p


--
-- | The map of headers in a 'Request' or 'Response'. Note that HTTP
-- header field names are case insensitive, so if you call 'setHeader'
-- on a field that's already defined but with a different capitalization
-- you will replace the existing value.
--
{-
    This is a fair bit of trouble just to avoid using a typedef here.
    Probably worth it, though; every other HTTP client library out there
    exposes the gory details of the underlying map implementation, and
    to use it you need to figure out all kinds of crazy imports. Indeed,
    this code used here in the Show instance for debugging has been
    copied & pasted around various projects of mine since I started
    writing Haskell. It's quite tedious, and very arcane! So, wrap it
    up.
-}
newtype Headers = Wrap {
    unWrap :: Map (CI ByteString) ByteString
} deriving (Eq)

instance Show Headers where
    show x = S.unpack $ S.filter (/= '\r') $ Builder.toByteString $ joinHeaders $ unWrap x

joinHeaders :: Map (CI ByteString) ByteString -> Builder
joinHeaders m = Map.foldrWithKey combine Mon.mempty m

combine :: CI ByteString -> ByteString -> Builder -> Builder
combine k v acc =
    mconcat [acc, key, Builder.fromString ": ", value, crlf]
  where
    key = Builder.copyByteString $ original k
    value = Builder.fromByteString v
{-# INLINE combine #-}

emptyHeaders :: Headers
emptyHeaders =
    Wrap Map.empty

--
-- | Set a header field to the specified value. This will overwrite
-- any existing value for the field. Remember that HTTP fields names
-- are case insensitive!
--
updateHeader :: Headers -> ByteString -> ByteString -> Headers
updateHeader x k v =
    Wrap result
  where
    !result = Map.insert (mk k) v m
    !m = unWrap x

--
-- | Remove a header from the map. If a field with that name is not present,
-- then this will have no effect.
--
removeHeader :: Headers -> ByteString -> Headers
removeHeader x k =
    Wrap result
  where
    !result = Map.delete (mk k) m
    !m = unWrap x


--
-- | Given a list of field-name,field-value pairs, construct a Headers map.
--
{-
    This is only going to be used by RequestBuilder and ResponseParser,
    obviously. And yes, as usual, we go to a lot of trouble to splice out the
    function doing the work, in the name of type sanity.
-}
buildHeaders :: [(ByteString, ByteString)] -> Headers
buildHeaders hs =
    Wrap result
  where
    result = foldl' addHeader Map.empty hs

{-
    insertWith is used here for the case where a header is repeated
    (for example, Set-Cookie) and the values need to be intercalated
    with ',' as per RFC 2616 §4.2.
-}
addHeader
    :: Map (CI ByteString) ByteString
    -> (ByteString,ByteString)
    -> Map (CI ByteString) ByteString
addHeader m (k,v) =
    Map.insertWith f (mk k) v m
  where
    f new old = S.concat [old, ",", new]


lookupHeader :: Headers -> ByteString -> Maybe ByteString
lookupHeader x k =
    Map.lookup (mk k) m
  where
    !m = unWrap x


--
-- | Get the headers as a field-name,field-value association list.
--
retrieveHeaders :: Headers -> [(ByteString, ByteString)]
retrieveHeaders x =
    map down $ Map.toList m
  where
    !m = unWrap x

down :: (CI ByteString, ByteString) -> (ByteString, ByteString)
down (k, v) =
    (original k, v)

data HttpParseException = HttpParseException String
        deriving (Show)

instance Exception HttpParseException
