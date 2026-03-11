
--------------------------------------------------------------------
-- |
-- Module    : Network.Curl.Download
-- Copyright : (c) Don Stewart
-- License   : BSD3
--
-- Maintainer:  Don Stewart <dons@galois.com>
-- Stability :  provisional
-- Portability: posix
--
-- A binding to curl, an efficient, high level library for
-- retrieving files using Uniform Resource Locators (URLs).
--
-- Content may be retrieved as a strings, "ByteString" or parsed
-- as HTML tags, XML or RSS and Atom feeds.
--
-- Error handling is encapsulated in the "Either" type.
--
--------------------------------------------------------------------

module Network.Curl.Download (

        -- * The basic interface to network content
          openURI
        , openURIString

        -- * Parsers for common formats
        , openAsTags
        , openAsXML
        , openAsFeed

        -- * A lower level interface
        , openURIWithOpts
    ) where

import Network.Curl
import Foreign

import Data.IORef

import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Char8    as Char8

-- Parsers
import qualified Text.HTML.TagSoup    as TagSoup
import qualified Text.XML.Light       as XML
import qualified Text.Feed.Import     as Feed
import qualified Text.Feed.Types      as Feed

------------------------------------------------------------------------

-- | Download content specified by a url using curl, returning the
-- content as a strict "ByteString".
--
-- If an error occurs, "Left" is returned, with a
-- protocol-specific error string.
--
-- Examples:
--
-- > openURI "http://haskell.org"
--
openURI :: String -> IO (Either String S.ByteString)
openURI s = openURIWithOpts [] s

-- | Like openURI, but returns the result as a 'String'
--
-- Examples:
--
-- > openURIString "http://haskell.org"
--
openURIString :: String -> IO (Either String String)
openURIString s = (fmap Char8.unpack) `fmap` openURI s


-- | Like openURI, but takes curl options.
--
-- Examples:
--
-- > openURIWithOpts [CurlPost True] "http://haskell.org"
--
openURIWithOpts :: [CurlOption] -> String -> IO (Either String S.ByteString)
openURIWithOpts opts s = case parseURL s of
     Nothing  -> return $ Left $ "Malformed url: "++ s
     Just url -> do
        e <- getFile url opts
        return $ case e of
             Left err   -> Left $ "Failed to connect: " ++ err
             Right src  -> Right src

------------------------------------------------------------------------
-- Parser interface:

-- | Download the content as for "openURI", but return it as a list of
-- parsed tags using the tagsoup html parser.
--
openAsTags:: String -> IO (Either String [TagSoup.Tag String])
openAsTags s = (fmap TagSoup.parseTags) `fmap` openURIString s

-- | Download the content as for "openURI", but return it as parsed XML,
-- using the xml-light parser.
--
openAsXML:: String -> IO (Either String [XML.Content])
openAsXML s = (fmap XML.parseXML) `fmap` openURIString s

-- | Download the content as for "openURI", but return it as parsed RSS
-- or Atom content, using the feed library parser.
--
openAsFeed :: String -> IO (Either String Feed.Feed)
openAsFeed s = do
       e <- openURIString s
       return $ case e of
        Left  err -> Left err   -- gluing Either -> Maybe
        Right src -> case Feed.parseFeedString src of
                        Nothing   -> Left "Unable to parse feed"
                        Just src' -> Right src'

------------------------------------------------------------------------
-- Internal:
--

newtype URL = URL String

parseURL :: String -> Maybe URL
parseURL s = Just (URL s) -- no parsing

getFile :: URL -> [CurlOption] -> IO (Either String S.ByteString)
getFile (URL url) flags = do
    h <- initialize

    let start = 1024
    buf  <- mallocBytes start
    ref  <- newIORef (P buf 0)

    setopt h (CurlFailOnError True)
    setDefaultSSLOpts h url
    setopt h (CurlURL url)
    setopt h (CurlWriteFunction (gather ref))
    mapM_ (setopt h) flags
    rc        <- perform h
    P buf' sz <- readIORef ref

    if rc /= CurlOK
        then do
            free buf'
            return $ Left (show rc)
        else do
            fp <- newForeignPtr finalizerFree buf'
            return (Right $! S.fromForeignPtr fp 0 (fromIntegral sz))

data P = P !(Ptr Word8) !Int

gather :: IORef P -> WriteFunction
gather r = writer $ \(src, m) -> do
    P dest n <- readIORef r
    dest' <- reallocBytes dest (n + m)
    S.memcpy (dest' `plusPtr` n) src (fromIntegral m)
    writeIORef r (P dest' (n + m))

-- memcpy chunks of data into our bytestring.
writer :: ((Ptr Word8, Int) -> IO ()) -> WriteFunction
writer f src sz nelems _ = do
    let n' = sz * nelems
    f (castPtr src, fromIntegral n')
    return n'
