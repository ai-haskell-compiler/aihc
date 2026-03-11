{-# LANGUAGE CPP, OverloadedStrings #-}

{-|
A library for listing "files" in an http "directory".

@
import Network.HTTP.Directory
import qualified Data.Text as T
import qualified Data.Text.IO as T

main = do
  let url = \"https:\/\/example.com\/some\/dir\/\"
  files <- httpDirectory\' url
  mapM_ T.putStrLn files
  let file = url '+/+' T.unpack (head files)
  httpFileSize\' file >>= print
  httpLastModified\' file >>= print
@

The main methods use http-client and most of the primed ones http-conduit.
-}

module Network.HTTP.Directory
       ( httpDirectory,
         httpDirectory',
         httpRawDirectory,
         httpRawDirectory',
         httpExists,
         httpExists',
         httpFileSize,
         httpFileSize',
         httpLastModified,
         httpLastModified',
         httpFileSizeTime,
         httpFileSizeTime',
         httpFileSizeAndTime',
         httpFileHeaders,
         httpFileHeaders',
         httpManager,
         httpRedirect,
         httpRedirect',
         httpRedirects,
         isHttpUrl,
         trailingSlash,
         noTrailingSlash,
         Manager,
         (+/+)
       ) where

import Control.Monad (when)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
#if MIN_VERSION_base(4,11,0)
import Data.Functor ((<&>))
#endif
import qualified Data.List as L
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)

import Network.HTTP.Client (hrRedirects, httpLbs, httpNoBody, Manager, method,
                            newManager, parseRequest, Request,
                            Response, responseBody, responseHeaders,
                            responseOpenHistory, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Date (httpDateToUTC, parseHTTPDate)
import qualified Network.HTTP.Simple as S
import Network.HTTP.Types (hAccept, hContentLength, hLocation, hLastModified,
                           methodHead, statusCode, ResponseHeaders)
import Network.URI (parseURI, URI(..))

import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor

-- | List the files (hrefs) in an http directory
--
-- It filters out absolute urls & paths, queries, '..', and '#' links.
--
-- Raises an error if the http request fails.
--
-- Note if the directory (webpage) url is redirected to a different path
-- you may need to use 'httpRedirect' to determine
-- the actual final url prefix for relative links (files).
--
-- (Before 0.1.4 this was the same as httpRawDirectory)
httpDirectory :: Manager -> String -> IO [Text]
httpDirectory mgr url = do
  hrefs <- httpRawDirectory mgr url
  return $ defaultFilesFilter (parseURI url) hrefs

defaultFilesFilter :: Maybe URI -> [Text] -> [Text]
defaultFilesFilter mUri =
  L.nub . filter (not . or . flist (map T.isInfixOf [":", "?", "#"] ++ [nonTrailingSlash] ++ [(`elem` ["../", ".."])])) . map removePath
  where
    -- picked from swish
    flist :: [a->b] -> a -> [b]
    flist fs a = map ($ a) fs

    -- may return "" which nonTrailingSlash then removes
    removePath :: Text -> Text
    removePath t =
      case murlPath of
        Nothing -> t
        Just path ->
          fromMaybe t $ T.stripPrefix path t

    murlPath :: Maybe Text
    murlPath = fmap (T.pack . trailingSlash . uriPath) mUri

    -- True means remove
    nonTrailingSlash :: Text -> Bool
    nonTrailingSlash "" = True     -- from removed uriPath
    nonTrailingSlash "/" = True
    nonTrailingSlash t =
      (T.length t > 1) && ("/" `T.isInfixOf` T.init t)

-- | Like httpDirectory but uses the global Manager
--
-- @since 0.1.4
httpDirectory' :: String -> IO [Text]
httpDirectory' url = do
  hrefs <- httpRawDirectory' url
  return $ defaultFilesFilter (parseURI url) hrefs

httpRawDirectoryInternal :: (Request -> IO (Response BL.ByteString)) -> String
                         -> IO [Text]
httpRawDirectoryInternal httpreq url = do
  request <- parseRequest url
  response <- httpreq $ S.addRequestHeader hAccept "text/html" request
  checkResponse url response
  let body = responseBody response
      doc = parseLBS body
      cursor = fromDocument doc
  return $ concatMap (attribute "href") $ cursor $// element "a"

-- | List all the hrefs in an http directory html file.
--
-- Raises an error if the http request fails.
--
-- Note if the directory (webpage) url is redirected to a different path
-- you may need to use 'httpRedirect' to determine
-- the actual final url prefix for relative links
-- (files).
--
-- @since 0.1.4
{- HLINT ignore "Use section" -}
httpRawDirectory :: Manager -> String -> IO [Text]
httpRawDirectory mgr = httpRawDirectoryInternal (flip httpLbs mgr)

-- | List all the hrefs in an http directory html file.
--
-- Raises an error if the http request fails.
--
-- Like httpRawDirectory but uses Network.HTTP.Simple (http-conduit)
--
-- @since 0.1.9
httpRawDirectory' :: String -> IO [Text]
httpRawDirectory' = httpRawDirectoryInternal S.httpLBS

-- | Test if an file (url) exists
--
-- @since 0.1.3
httpExists :: Manager -> String -> IO Bool
httpExists mgr url = do
  response <- httpHead mgr url
  return $ statusCode (responseStatus response) == 200

-- | Test if an file (url) exists
--
-- @since 0.1.9
httpExists' :: String -> IO Bool
httpExists' url = do
  response <- httpHead' url
  return $ statusCode (responseStatus response) == 200

-- | Try to get the filesize (Content-Length field) of an http file
--
-- Raises an error if the http request fails.
httpFileSize :: Manager -> String -> IO (Maybe Integer)
httpFileSize mgr url =
  httpFileHeaders mgr url <&>
  fmap (read . B.unpack) . lookup hContentLength

-- | Try to get the filesize (Content-Length field) of an http file
--
-- Raises an error if the http request fails.
--
-- @since 0.1.9
httpFileSize' :: String -> IO (Maybe Integer)
httpFileSize' url =
  httpFileHeaders' url <&>
  fmap (read . B.unpack) . lookup hContentLength

-- | Try to get the modification time (Last-Modified field) of an http file
--
-- Raises an error if the http request fails.
--
-- @since 0.1.1
httpLastModified :: Manager -> String -> IO (Maybe UTCTime)
httpLastModified mgr url = do
  headers <- httpFileHeaders mgr url
  let mdate = lookup hLastModified headers
  return $ httpDateToUTC <$> (parseHTTPDate =<< mdate)

-- | Try to get the modification time (Last-Modified field) of an http file
--
-- Raises an error if the http request fails. Uses global Manager
--
-- @since 0.1.9
httpLastModified' :: String -> IO (Maybe UTCTime)
httpLastModified' url = do
  headers <- httpFileHeaders' url
  let mdate = lookup hLastModified headers
  return $ httpDateToUTC <$> (parseHTTPDate =<< mdate)

-- | Try to get the filesize and modification time of an http file
--
-- Raises an error if the http request fails.
--
-- @since 0.1.10
httpFileSizeTime :: Manager -> String -> IO (Maybe Integer, Maybe UTCTime)
httpFileSizeTime mgr url = do
  headers <- httpFileHeaders mgr url
  let msize = read . B.unpack <$> lookup hContentLength headers
      mdate = lookup hLastModified headers
      mtime = httpDateToUTC <$> (parseHTTPDate =<< mdate)
  return (msize, mtime)

-- | Try to get the filesize and modification time of an http file
--
-- Global Manager version.
--
-- Raises an error if the http request fails.
--
-- @since 0.1.10
httpFileSizeTime' :: String -> IO (Maybe Integer, Maybe UTCTime)
httpFileSizeTime' url = do
  headers <- httpFileHeaders' url
  let msize = read . B.unpack <$> lookup hContentLength headers
      mdate = lookup hLastModified headers
      mtime = httpDateToUTC <$> (parseHTTPDate =<< mdate)
  return (msize, mtime)

-- | Try to get the filesize and modification time together of an http file
-- Unlike httpFileSizeTime', it combines the results into one Maybe.
--
-- Uses global Manager.
--
-- Raises an error if the http request fails.
--
-- @since 0.1.11
httpFileSizeAndTime' :: String -> IO (Maybe (Integer, UTCTime))
httpFileSizeAndTime' url = do
  headers <- httpFileHeaders' url
  return $ msizetime headers
  where
    msizetime headers = do
      size <- read . B.unpack <$> lookup hContentLength headers
      date <- lookup hLastModified headers
      time <- httpDateToUTC <$> parseHTTPDate date
      return (size,time)

-- | Return the HTTP headers for a file
--
-- Raises an error if the http request fails.
--
-- @since 0.1.10
httpFileHeaders :: Manager -> String -> IO ResponseHeaders
httpFileHeaders mgr url = do
  response <- httpHead mgr url
  checkResponse url response
  return $ responseHeaders response

-- | Return the HTTP headers of an http file.
--
-- Global Manager version.
--
-- Raises an error if the http request fails.
--
-- @since 0.1.10
httpFileHeaders' :: String -> IO ResponseHeaders
httpFileHeaders' url = do
  response <- httpHead' url
  checkResponse url response
  return $ responseHeaders response

-- conflicts with Request
checkResponse :: String -> Response r -> IO ()
checkResponse url response =
  when (statusCode (responseStatus response) /= 200) $ do
    putStrLn url
    error' $ show $ responseStatus response

-- | Alias for 'newManager tlsManagerSettings'
-- so one does not need to import http-client etc
--
-- @since 0.1.2
httpManager :: IO Manager
httpManager =
  newManager tlsManagerSettings

-- | Returns the list of http redirects for an url in reverse order
-- (ie last redirect is listed first)
httpRedirects :: Manager -> String -> IO [B.ByteString]
httpRedirects mgr url = do
  request <- parseRequestHead url
  respHist <- responseOpenHistory request mgr
  return $ reverse $ mapMaybe (lookup hLocation . responseHeaders . snd) $ hrRedirects respHist

-- | Return final redirect for an url
httpRedirect :: Manager -> String -> IO (Maybe B.ByteString)
httpRedirect mgr url =
  listToMaybe <$> httpRedirects mgr url

-- | Like httpRedirect but uses global Manager.
--
-- @since 0.1.4
httpRedirect' :: String -> IO (Maybe B.ByteString)
httpRedirect' url = do
  mgr <- httpManager
  listToMaybe <$> httpRedirects mgr url

parseRequestHead :: String -> IO Request
parseRequestHead url = do
  request <- parseRequest url
  return $
    S.addRequestHeader hAccept "*/*" $
    request {method = methodHead}

httpHead :: Manager -> String -> IO (Response ())
httpHead mgr url = do
  request <- parseRequestHead url
  httpNoBody request mgr

httpHead' :: String -> IO (Response ())
httpHead' url = do
  request <- parseRequestHead url
  S.httpNoBody request

-- | Test if string starts with http[s]:
--
-- @since 0.1.5
isHttpUrl :: String -> Bool
isHttpUrl loc = "http:" `L.isPrefixOf` loc || "https:" `L.isPrefixOf` loc

-- | Make sure an url ends with "\/"
--
-- @
-- trailingSlash "url" == "url\/"
-- trailingSlash "url\/" == "url\/"
-- @
--
-- @since 0.1.6
trailingSlash :: String -> String
trailingSlash "" = ""
trailingSlash str =
  if last str == '/' then str else str ++ "/"

-- | Remove all trailing slashes from filename or url
--
-- @
-- noTrailingSlash "dir\/" == "dir"
-- noTrailingSlash "dir\/\/" == "dir"
-- @
--
-- @since 0.1.6
noTrailingSlash :: Text -> Text
noTrailingSlash = T.dropWhileEnd (== '/')

-- from simple-cmd
error' :: String -> a
error' = errorWithoutStackTrace

-- | This +\/+ eats extra slashes.
--
-- @
-- "dir\/\/" +\/+ "\/subdir\/" = "dir\/subdir\/"
-- @
--
-- @since 0.1.9
infixr 5 +/+
(+/+) :: String -> String -> String
s +/+ t =
  case (s,t) of
    ("",_) -> t
    (_,"") -> s
    (_,_) -> L.dropWhileEnd (== '/') s ++ '/' : L.dropWhile (== '/') t

#if !MIN_VERSION_base(4,11,0)
infixl 1 <&>

(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as
#endif
