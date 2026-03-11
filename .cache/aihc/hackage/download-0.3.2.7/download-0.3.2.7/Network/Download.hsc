{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------------------------
-- |
-- Module    : Network.Download
-- Copyright : (c) Don Stewart
-- License   : BSD3
--
-- Maintainer:  Don Stewart <dons@galois.com>
-- Stability :  provisional
-- Portability: posix
--
-- A binding to libdownload, an efficient, high level library for
-- retrieving files using Uniform Resource Locators (URLs). This
-- provides simple, uniform access to file, FTP and HTTP resources.
-- Content may be retrieved as a strings, "ByteString" or parsed
-- as HTML tags, XML or RSS and Atom feeds.
--
-- Error handling is encapsulated in the "Either" type.
--
--------------------------------------------------------------------

module Network.Download (

        -- * The basic interface to network content
          openURI
        , openURIString

        -- * Parsers for common formats
        , openAsTags
        , openAsXML
        , openAsFeed

    ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.C.Types
import Foreign.C.String

import qualified Foreign.Concurrent as C
import Control.Exception

import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Char8    as Char8
import Data.Word (Word8)

-- Parsers
import qualified Text.HTML.TagSoup    as TagSoup
import qualified Text.XML.Light       as XML
import qualified Text.Feed.Import     as Feed
import qualified Text.Feed.Types      as Feed
import System.IO.Unsafe (unsafePerformIO)

#include "download.h"

------------------------------------------------------------------------

-- | Download content specified by url (in RFC1738 form), using either
-- FTP, HTTP or file protocols, returning the content as a strict
-- "ByteString".
--
-- If the url is malformed, a "Left" value is returned.
-- Similarly, if an error occurs, "Left" is returned, with a
-- protocol-specific error string.
--
-- If the file protocol is used, documents will be retrieved from the
-- local filesystem. If the ftp scheme is used, the FTP protocol
-- (RFC959) is used. If no user name or password are provided,
-- anonymous login, with user name 'anonymous' and password 'anonymous'
-- will be attempted.
--
-- If the http method is used, HTTP\/1.1 will be used.
--
-- Examples:
--
-- > openURI "http://haskell.org"
--
openURI :: String -> IO (Either String S.ByteString)
openURI s = case parseURL s of
     Nothing  -> return $ Left $ "Malformed url: "++ s
     Just url -> do
        e <- getFile url []
        return $ case e of
             Left err   -> Left $ "Failed to connect: " ++ err
             Right src  -> Right src

-- | Like openURI, but returns the result as a 'String'
--
-- Examples:
--
-- > openURIString "http://haskell.org"
--
openURIString :: String -> IO (Either String String)
openURIString s = (fmap Char8.unpack) `fmap` openURI s

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

-- A data type, tracked by the garbage collector, that handles
-- libdownload's url structures.
--
data URL = URL {-# UNPACK #-} !(ForeignPtr URL_)
        deriving (Eq, Ord, Show)

data URL_

-- | Takes a URL in the form of a String and splits it into its
-- components function according to the Common Internet Scheme Syntax
-- detailed in RFC1738.
--

parseURL :: String -> Maybe URL
parseURL s = unsafePerformIO $ withCString s $ \cstr -> do
    p <- c_parseURL cstr
    if p == nullPtr
       then return Nothing
       else do
          fp <- C.newForeignPtr p (c_freeURL p) -- The pointer returned by downloadMakeURL()
                                                -- or downloadParseURL() should be freed
                                                -- using downloadFreeURL()
          return . Just $ URL fp

-- struct url *downloadParseURL(const char *);
foreign import ccall unsafe "downloadParseURL"
    c_parseURL :: CString -> IO (Ptr URL_)

-- void downloadFreeURL(struct url *);
foreign import ccall unsafe "downloadFreeURL"
    c_freeURL :: Ptr URL_ -> IO ()

------------------------------------------------------------------------

-- The flags argument is a string of characters which specify
-- transfer options.  The meaning of the individual flags is
-- scheme‐dependent
--
data Flag
    = Passive       -- ^ a passive (rather than active) connection will be attempted.
    | Low           -- ^ data sockets will be allocated in the low (or
                    -- default) port range instead of the high port range
    | Direct        -- ^ use a direct connection even if a proxy server is defined
  deriving (Eq,Show)

encodeFlag :: Flag -> Char
encodeFlag Passive = 'p'
encodeFlag Low     = 'l'
encodeFlag Direct  = 'd'

------------------------------------------------------------------------


--      The flags argument is a string of characters which specify
--      transferoptions.  The meaning of the individual flags is
--      scheme‐dependent, and is detailed in the appropriate section below.
--
getFile :: URL -> [Flag] -> IO (Either String S.ByteString)
getFile (URL url_fp) flags = do
  withForeignPtr url_fp $ \c_url ->
   withCString (map encodeFlag flags) $ \c_flags ->
     bracket
        (do sp <- c_get c_url c_flags
            dl <- lastErrorCode
            if dl /= dlerr_none || sp == nullPtr
                     then return Nothing
                     else return $ Just sp)

-- all other functions return a stream pointer which may be used to
-- access the requested document, or NULL if an error occurred.
    --
        (maybe (return ()) c_fclose)

        $ \sp -> case sp of
              Nothing     -> Left `fmap` lastError
              Just stream -> load stream

        -- but if we can stat it, we can be more efficient.
    where
        load :: Ptr CFile -> IO (Either String S.ByteString)
        load stream = do
            let start = 1024
            p  <- mallocBytes start
            i  <- c_fread p 1 (fromIntegral start) stream

            -- I hate C error handling
            dl_err <- lastErrorCode
            st_err <- c_ferror stream
            if dl_err /= dlerr_none || st_err /= 0
               then do free p
                       Left `fmap` lastError
               else
                if i < fromIntegral start
                    then do p' <- reallocBytes p (fromIntegral i)
                            fp <- newForeignPtr finalizerFree p'
                            return (Right $! S.fromForeignPtr fp 0 (fromIntegral i))

                else go stream p start

        -- duplicated too much code
        go stream p n = do
            let n' = 2 * n
            p' <- reallocBytes p n'
            i  <- c_fread (p' `plusPtr` n) 1 (fromIntegral n) stream

            dl_err <- lastErrorCode
            st_err <- c_ferror stream
            if dl_err /= dlerr_none || st_err /= 0
               then do free p
                       Left `fmap` lastError
               else
                if i < fromIntegral n
                    then do let i' = n + fromIntegral i
                            p'' <- reallocBytes p' i'
                            fp  <- newForeignPtr finalizerFree p''
                            return (Right $! S.fromForeignPtr fp 0 (fromIntegral i'))
                    else go stream p' n'

------------------------------------------------------------------------

-- size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream);
--
foreign import ccall unsafe "fread"
    c_fread :: Ptr Word8 -> CSize -> CSize -> Ptr CFile -> IO CSize

-- int fclose(FILE *fp);
--
foreign import ccall unsafe "fclose"
    c_fclose :: Ptr CFile -> IO () -- ignoring CInt

-- int ferror(FILE *stream);
--
foreign import ccall unsafe "ferror"
    c_ferror :: Ptr CFile -> IO CInt

------------------------------------------------------------------------

foreign import ccall unsafe "downloadGet"
    c_get :: Ptr URL_ -> CString -> IO (Ptr CFile)

foreign import ccall unsafe "hs_download_utils.h hs_download_last_error"
    c_lastErrorString :: IO CString

lastError :: IO String
lastError = peekCString =<< c_lastErrorString

-- Wrap this safely...
foreign import ccall unsafe "hs_download_utils.h hs_download_last_error_code"
    lastErrorCode :: IO DLError

------------------------------------------------------------------------

{-
version :: String
version = #const_str USER_AGENT_STRING
-}

------------------------------------------------------------------------

-- type URLLen = Int
-- 
-- #{enum URLLen,
--     , url_schemelen = URL_SCHEMELEN
--     , url_userlen   = URL_USERLEN
--    , url_pwdlen    = URL_PWDLEN
--     }

------------------------------------------------------------------------

{-
newtype Scheme = Scheme { unScheme :: String }
    deriving (Eq, Show)

scheme_ftp, scheme_http, scheme_https, scheme_file :: Scheme
scheme_ftp   = Scheme #const_str SCHEME_FTP
scheme_http  = Scheme #const_str SCHEME_HTTP
scheme_https = Scheme #const_str SCHEME_HTTPS
scheme_file  = Scheme #const_str SCHEME_FILE
-}

------------------------------------------------------------------------

newtype DLError = DLError CInt
    deriving (Eq, Show)

#{enum DLError, DLError
    , dlerr_none    = DLERR_NONE
    }

{-
    , dlerr_abort   = DLERR_ABORT
    , dlerr_auth    = DLERR_AUTH
    , dlerr_down    = DLERR_DOWN
    , dlerr_exists  = DLERR_EXISTS
    , dlerr_full    = DLERR_FULL
    , dlerr_info    = DLERR_INFO
    , dlerr_memory  = DLERR_MEMORY
    , dlerr_moved   = DLERR_MOVED
    , dlerr_network = DLERR_NETWORK
    , dlerr_ok      = DLERR_OK
    , dlerr_proto   = DLERR_PROTO
    , dlerr_resolv  = DLERR_RESOLV
    , dlerr_server  = DLERR_SERVER
    , dlerr_temp    = DLERR_TEMP
    , dlerr_timeout = DLERR_TIMEOUT
    , dlerr_unavail = DLERR_UNAVAIL
    , dlerr_unknown = DLERR_UNKNOWN
    , dlerr_url     = DLERR_URL
    , dlerr_verbose = DLERR_VERBOSE
-}
