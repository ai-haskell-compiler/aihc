{-# LANGUAGE CPP #-}

{-|
A small library for querying a Web API.

@
{-# LANGUAGE OverloadedStrings #-}

import Data.Text.IO as T
import Network.HTTP.Query

main = do
  let api = "http://www.example.com/api/1"
      endpoint = api +/+ "search"
  res <- webAPIQuery endpoint $ makeKey "q" "needle"
  T.putStrLn $
    case lookupKey "results" res of
      Nothing ->
        fromMaybe "search failed" $ lookupKey "error" res
      Just results ->
        lookupKey' "location" results
@
-}

module Network.HTTP.Query (
  -- * Queries
  withURLQuery,
  webAPIQuery,
  apiQueryURI,
  (+/+),
  -- * Query parameters
  Query,
  QueryItem,
  maybeKey,
  makeKey,
  makeItem,
  -- * Lookup fields
  lookupKey,
  lookupKeyEither,
  lookupKey'
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad.IO.Class (MonadIO)
#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.Key (fromText)
#endif
import Data.Aeson.Types
#if !MIN_VERSION_http_conduit(2,3,3)
import Data.ByteString (ByteString)
#endif
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import Data.Text (Text)
import Network.HTTP.Client.Conduit
import Network.HTTP.Simple
import Network.URI

#if !MIN_VERSION_http_conduit(2,3,1)
type Query = [(ByteString, Maybe ByteString)]
#endif
#if !MIN_VERSION_http_conduit(2,3,3)
type QueryItem = (ByteString, Maybe ByteString)
#endif

-- | Maybe create a query key
maybeKey :: String -> Maybe String -> Query
maybeKey _ Nothing = []
maybeKey k mval = [(B.pack k, fmap B.pack mval)]

-- | Make a singleton key-value Query
makeKey :: String -> String -> Query
makeKey k val = [(B.pack k, Just (B.pack val))]

-- | Make a key-value QueryItem
makeItem :: String -> String -> QueryItem
makeItem k val = (B.pack k, Just (B.pack val))

-- | Combine two path segments with a slash
--
-- > "abc" +/+ "def" == "abc/def"
-- > "abc/" +/+ "def" == "abc/def"
-- > "abc" +/+ "/def" == "abc/def"
infixr 5 +/+
(+/+) :: String -> String -> String
s +/+ t =
  case (s,t) of
    ("",_) -> t
    (_,"") -> s
    (_,_) -> L.dropWhileEnd (== '/') s ++ '/' : L.dropWhile (== '/') t

-- | Sets up an API request for some action
withURLQuery :: String -> Query -> (Request -> a) -> a
withURLQuery url params act =
  case parseURI url of
    Nothing -> error $ "Cannot parse uri: " ++ url
    Just uri ->
      let req = setRequestQueryString params $
                requestFromURI_ uri
      in act req

-- | Low-level web api query
webAPIQuery :: (MonadIO m, FromJSON a)
            => String -- ^ URL of endpoint
            -> Query -- ^ query options
            -> m a -- ^ returned JSON
webAPIQuery url params =
  withURLQuery url params $ fmap getResponseBody . httpJSON

-- | Get the URI for a web query
apiQueryURI :: String -- ^ URL of endpoint
            -> Query -- ^ query options
            -> URI
apiQueryURI url params =
  withURLQuery url params getUri

-- FIXME support "key1.key2" etc
-- | Look up key in object
lookupKey :: FromJSON a => Text -> Object -> Maybe a
lookupKey k =
  parseMaybe (.: fromText k)

-- | Like lookupKey but returns error message if not found
lookupKeyEither :: FromJSON a => Text -> Object -> Either String a
lookupKeyEither k = parseEither (.: fromText k)

-- | Like lookupKey but raises an error if no key found
lookupKey' :: FromJSON a => Text -> Object -> a
lookupKey' k =
  either error id . parseEither (.: fromText k)

#if !MIN_VERSION_aeson(2,0,0)
fromText :: Text -> Text
fromText = id
#endif
