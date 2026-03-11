-- | Module      : Codec.Archive.Zim.Parser
-- Description : API for parsing ZIM files
-- Copyright   : (c) Robbin C.
-- License     : GPLv3
-- Maintainer  : Robbin C.
-- Stability   : unstable
-- Portability : portable
--
-- This is a library for parsing ZIM (<http://openzim.org>) files. ZIM files
-- contain offline web content (eg, Wikipedia) which can be browsed locally
-- without an Internet connection.
--
-- The API is meant to be intuitive for normal use-cases.
--
-- To get content for "A/index.htm" from ZIM file "file.zim":
--
-- > > mimeContent <- "file.zim" `getContent` Url "A/index.htm"
-- > > :t mimeContent
-- > mimeContent :: Maybe (B8.ByteString, BL.ByteString)
-- > > print mimeContent
-- > Just ("text/html", "<html><head>...</html>")
--
-- The above will open the file, parse the ZIM header, lookup the
-- MIME type and content of the URL, close the file and return the MIME type and
-- content as a pair. Note that content is a lazy bytestring.
--
-- The above operation should suffice for a simple webserver serving a ZIM file.
-- For finer control, it is possible to cache and reuse the file handle and the
-- ZIM header.
--
-- > > hdl <- openBinaryFile "file.zim" ReadMode
-- > > hdr <- getHeader hdl
-- > > :t hdr
-- > hdr :: ZimHeader
-- > > (hdl, hdr) `getContent` Url "A/index.htm"
-- > Just ("text/html", "<html><head>...</html>")
--
-- ZIM files of Wikimedia Foundation (Wikipedia, Wikibooks, etc) can be
-- found at http://ftpmirror.your.org/pub/kiwix/zim.
--
-- Below is a full example of a Scotty web server that serves a ZIM file
-- (specified on command line) on localhost port 3000:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Control.Monad.IO.Class (liftIO)
-- > import Data.Text.Lazy (toStrict, fromStrict)
-- > import Data.Text.Encoding (decodeUtf8, encodeUtf8)
-- > import System.Environment (getArgs)
-- > import Network.HTTP.Types.Status (status404)
-- > import Web.Scotty
-- > import Codec.Archive.Zim.Parser (getMainPageUrl, getContent, Url(..))
-- >
-- > main :: IO ()
-- > main = do
-- >     [fp] <- getArgs
-- >     scotty 3000 $ do
-- >       get "/" (redirectToZimMainPage fp)
-- >       get (regex "^/(./.*)$") (serveZimUrl fp)
-- >       notFound $ text "Invalid URL!"
-- >
-- > redirectToZimMainPage :: FilePath -> ActionM ()
-- > redirectToZimMainPage fp = do
-- >     res <- liftIO $ getMainPageUrl fp
-- >     case res of
-- >       Nothing -> do
-- >         status status404
-- >         text "This ZIM file has no main page specified!"
-- >       Just (Url url) -> redirect . fromStrict $ decodeUtf8 url
-- >
-- > serveZimUrl :: FilePath -> ActionM ()
-- > serveZimUrl fp = do
-- >     url <- (encodeUtf8 . toStrict) <$> param "1"
-- >     res <- liftIO $ fp `getContent` Url url
-- >     case res of
-- >       Nothing -> do
-- >         liftIO . putStrLn $ "Invalid URL: " ++ show url
-- >         status status404
-- >         text $ "Invalid URL!"
-- >       Just (mimeType, content) -> do
-- >         liftIO . putStrLn $ "Serving: " ++ show url
-- >         setHeader "Content-Type" (fromStrict $ decodeUtf8 mimeType)
-- >         raw content
--
-- Feedback and contributions are welcome on
-- <http://github.com/robbinch/zim-parser>.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
module Codec.Archive.Zim.Parser (
-- * Functions
  getHeader
, getMimeList
, getDE
, getMainPageUrl
, getCluster
, getBlob
, getContent
, searchDE
, MimeList
, mkNsTitle
, mkNsTitlePrefix
, mkNsUrl
, RunZim
, ZimGetDE
, ZimSearchDE
, ZimGetContent
-- * Exceptions
, ZimException(..)
-- * ZIM Header
, ZimHeader(..)
-- * ZIM Directory Entry
, ZimDirEntType(..)
, ZimDirEnt(..)
, UrlIndex(..)
, TitleIndex(..)
, ClusterNumber(..)
, BlobNumber(..)
, Cluster(..)
, Blob(..)
, Url(..)
, Title
, TitlePrefix
-- * ZIM file format
-- | Following is a short summary of the ZIM file format.
-- The authoritative reference is at
-- http://www.openzim.org/wiki/ZIM_file_format.
--
-- === 1. ZIM header
-- This is an 80-byte header (see 'ZimHeader'). Among other things, it contains
-- file offsets to the below.
--
-- === 2. List of MIME types
-- This is a sequence of null-terminated strings (eg. @text\/html@,
-- @text\/javascript@). The last string is zero length, so the end always
-- consists of 2 consecutive null bytes.
--
-- === 3. List of URLs
-- This is a sequence of 8-byte file offsets, each pointing to a directory
-- entry. This list is sorted by the directory entries' URL.
--
-- === 4. List of Titles
-- This is a sequence of 4-byte indices, each pointing to a URL above (which in
-- turn point to a directory entry). This list is sorted by the directory
-- entries' Title.
--
-- === 5. Directory Entries
-- This is a sequence of Directory Entries (see 'ZimDirEnt').
-- The first 2 bytes determine the type of this entry, which also determine the
-- length.
-- Contents include:
--
-- ==== a. MIME type
-- This 2-byte field means:
--
-- [@0xffff@] This directory entry is a 'ZimRedirectEntry'.
-- [@0xfffe@] This directory entry is a 'ZimLinkTarget'.
-- [@0xfffd@] This directory entry is a 'ZimDeletedEntry'.
-- [@any other value@] This directory entry is a 'ZimArticleEntry' and this
-- index into the MIME list from above determines its MIME type.
--
-- ==== b. Namespace
-- This single character determines the directory entry's namespace.
-- (eg. __A__ for articles, __I__ for images, etc.) The comprehensive list is
-- at http://www.openzim.org/wiki/ZIM_file_format#Namespaces.
--
-- ==== c. Cluster and Blob number
-- Only for 'ZimArticleEntry', this is the directory entry's Cluster and
-- Blob number. The Cluster number is a 4-byte index into the list of Clusters
-- below. The Blob number refers to a block inside the (decompressed) cluster.
-- Together, they provide the content of this directory entry.
--
-- ==== d. URL and Title
-- These 2 null-terminated strings represent the URL and Title of this directory
-- entry respectively. If the Title is empty, it is taken to be the same as the
-- URL.
--
-- === 6. List of Clusters
-- This is a list of 8-byte file offsets, each pointing to a cluster in the
-- file. -- The end of a cluster is also the start of the next cluster.
-- Therefore, the length of a cluster is the difference between the adjacent
-- offsets. For the last cluster, the end is the Checksum file offset, as the
-- Checksum is always the last 16 bytes of a ZIM file.
--
-- ==== a. Compression Type
-- The first byte of the cluster determines if it is uncompressed
-- (eg. PNG image) or compressed with LZMA (eg. HTML).
--
-- [@0 or 1@] No compression
-- [@4@] Compressed with LZMA
--
-- ==== b. List of Blobs
-- This is a list of 4-byte offsets, each pointing inside this cluster.
-- The end of a blob is also the start of the next blob.
-- Therefore, the length of a blob is the difference between the adjacent
-- offsets. The last offset points to the end of the data area so there is
-- always one more offset than blobs.
) where

import           Prelude ()
import           Prelude.Compat
import           Codec.Compression.Lzma (decompress)
import           Control.Exception (Exception, throw)
import           Control.Monad (when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import           Data.Char (chr)
import           Data.Maybe (fromJust)
import           Data.Typeable (Typeable)
import           System.IO (Handle, IOMode (ReadMode), withBinaryFile)

import           Data.Conduit (Sink, await, ($$), (=$))
import           Data.Conduit.Binary (sourceHandleRange, sourceLbs)
import           Data.Conduit.Serialization.Binary (conduitGet, sinkGet)

import           Data.Array.IArray (Array, listArray, (!))
import           Data.Binary.Get
import           Numeric (showHex)

-- | Other than the below, ErrorCall can be thrown by LZMA library if there is a problem with decompression.
data ZimException
  = ZimInvalidMagic       -- ^ ZIM file has invalid magic number (anything other than 72173914).
  | ZimParseError String  -- ^ There is an error in parsing.
  | ZimIncompleteInput    -- ^ There is insufficient bytes required to parse.
  | ZimInvalidIndex Int   -- ^ The given index (URL, title or cluster) is out of bounds for this ZIM file.
  deriving (Show, Typeable)
instance Exception ZimException

-- | See http://www.openzim.org/wiki/ZIM_file_format#Header for more details.
data ZimHeader = ZimHeader {
  -- | Magic Number of file (somewhat superfluous as 'getZimHeader' will throw an exception if magic number is anything other than 72173914)
    zimMagicNumber   :: Int
  -- | Version of ZIM header
  , zimVersion       :: Int
  -- | UUID of file
  , zimUuid          :: B.ByteString
  -- | Number of articles
  , zimArticleCount  :: Int
  -- | Number of clusters
  , zimClusterCount  :: Int
  -- | Position of sorted URL pointers
  , zimUrlPtrPos     :: Integer
  -- | Position of sorted Title pointers
  , zimTitlePtrPos   :: Integer
  -- | Position of Cluster pointers
  , zimClusterPtrPos :: Integer
  -- | Position of MIME list
  , zimMimeListPos   :: Integer
  -- | Index of main page
  , zimMainPage      :: Maybe Int
  -- | Index of layout page
  , zimLayoutPage    :: Maybe Int
  -- | Position of MD5 checksum
  , zimChecksumPos   :: Integer
  } deriving (Show, Eq)

-- | There are 4 types of directory entries. Most content in a ZIM file are
-- usually 'ZimArticleEntry' or 'ZimRedirectEntry'.
data ZimDirEntType
  = ZimArticleEntry
  | ZimRedirectEntry
  | ZimLinkTarget
  | ZimDeletedEntry
  deriving (Eq, Show)

-- | See http://www.openzim.org/wiki/ZIM_file_format#Directory_Entries for more
-- details.
data ZimDirEnt = ZimDirEnt {
  -- | Type of this Directory Entry
    zimDeType          :: ZimDirEntType
  -- | Index into MIME list given by 'getZimMimeList'
  , zimDeMimeType      :: Int
  -- | Parameter Length
  , zimDeParameterLen  :: Int
  -- | Namespace
  , zimDeNamespace     :: Char
  -- | Revision
  , zimDeRevision      :: Int
  -- | Redirect Index (only applicable for 'ZimRedirectEntry')
  , zimDeRedirectIndex :: Maybe Int
  -- | Content is stored in this Cluster Number (only applicable for 'ZimArticleEntry')
  , zimDeClusterNumber :: Maybe Int
  -- | Content is stored in this Blob Number (only applicable for 'ZimArticleEntry')
  , zimDeBlobNumber    :: Maybe Int
  -- | URL
  , zimDeUrl           :: B8.ByteString
  -- | Title
  , zimDeTitle         :: B8.ByteString
  -- , zimDeParameter     :: BL.ByteString -- unused
  } deriving (Eq, Show)

-- | List of Mime Types
type MimeList = Array Int B8.ByteString

-- | Wrapper for URL index
newtype UrlIndex      = UrlIndex      Int deriving (Eq, Ord, Show)
-- | Wrapper for Title index
newtype TitleIndex    = TitleIndex    Int deriving (Eq, Ord, Show)
-- | Wrapper for Cluster number
newtype ClusterNumber = ClusterNumber Int deriving (Eq, Ord, Show)
-- | Wrapper for Blob number
newtype BlobNumber    = BlobNumber    Int deriving (Eq, Ord, Show)

-- | Wrapper for Url
newtype Url = Url B.ByteString deriving (Eq, Ord, Show)
-- | Construct a Url with a Namespace.
mkNsUrl :: Char -> B.ByteString -> Url
mkNsUrl c s = Url $ c `B8.cons` '/' `B8.cons` s

-- | Wrapper for Title
newtype Title = Title B.ByteString deriving (Eq, Ord, Show)
-- | Construct a Title with a Namespace.
mkNsTitle :: Char -> B.ByteString -> Title
mkNsTitle c s = Title $ c `B8.cons` '/' `B8.cons` s

-- | Wrapper for Title Prefix
newtype TitlePrefix = TitlePrefix B.ByteString deriving (Eq, Ord, Show)
-- | Construct a TitlePrefix with a Namespace.
mkNsTitlePrefix :: Char -> B.ByteString -> TitlePrefix
mkNsTitlePrefix c s = TitlePrefix $ c `B8.cons` '/' `B8.cons` s

-- | Wrapper for Cluster
newtype Cluster = Cluster {unCluster :: BL.ByteString}
-- | Wrapper for Blob
newtype Blob = Blob {unBlob :: BL.ByteString}

parseZimHeader :: Get ZimHeader
parseZimHeader = do
  magicNumber   <- fromIntegral <$> getWord32le
  when (magicNumber /= 72173914) $ throw ZimInvalidMagic
  version       <- fromIntegral <$> getWord32le
  uuid          <- getByteString 16
  articleCount  <- fromIntegral <$> getWord32le
  clusterCount  <- fromIntegral <$> getWord32le
  urlPtrPos     <- fromIntegral <$> getWord64le
  titlePtrPos   <- fromIntegral <$> getWord64le
  clusterPtrPos <- fromIntegral <$> getWord64le
  mimeListPos   <- fromIntegral <$> getWord64le
  mainPage      <- fromIntegral <$> getWord32le
  layoutPage    <- fromIntegral <$> getWord32le
  checksumPos   <- fromIntegral <$> getWord64le
  return $ ZimHeader magicNumber version uuid articleCount clusterCount
    urlPtrPos titlePtrPos clusterPtrPos mimeListPos
    (maybePage mainPage) (maybePage layoutPage) checksumPos
  where
    maybePage page
      | page == 0xffffffff = Nothing
      | otherwise = Just page

-- | Instances of this class represent a Zim File and are able to perform ZIM
-- operations (getMimeList, getContent, etc). Valid instances include a Handle
-- to a ZIM file, a FilePath to a ZIM file, or a (Handle, ZimHeader) where
-- ZimHeader is parsed previously (so it does not need to be reparsed).
class RunZim h where
  runZim :: h -> (Handle -> ZimHeader -> IO a) -> IO a

instance RunZim Handle where
  runZim hdl f = do
    hdr <- src $$ sinkGet parseZimHeader
    f hdl hdr
    where
      (pos, len) = (Just 0, Just 80)
      src        = sourceHandleRange hdl pos len

instance RunZim (Handle, ZimHeader) where
  runZim x f = uncurry f x

instance RunZim FilePath where
  runZim fp f = withBinaryFile fp ReadMode $ \hdl -> runZim hdl f

getHeader :: RunZim h => h -> IO ZimHeader
getHeader h = runZim h $ \_ hdr -> return hdr

-- Parses a list of null-terminated byte sequence.
-- Last entry is zero length (end of block is always 2 null bytes).
parseByteStringsNul :: Sink B8.ByteString IO [B8.ByteString]
parseByteStringsNul = conduitGet getLazyByteStringNul =$ loop id
  where
    loop :: ([B8.ByteString] -> [B8.ByteString]) -> Sink BL.ByteString IO [B8.ByteString]
    loop front = await >>= maybe
      (return $ front [])
      (\x -> let bs = BL.toStrict x
             in if B8.null $ BL.toStrict x
               then return (front [])
               else loop (front . (bs:))
      )

getMimeList :: RunZim h => h -> IO MimeList
getMimeList h = runZim h $ \hdl hdr -> do
  let (pos, len) = (Just $ zimMimeListPos hdr, Nothing)
      src        = sourceHandleRange hdl pos len
  mimeList <- src $$ parseByteStringsNul
  return $ listArray (0, length mimeList) mimeList

parseZimDirEnt :: Get ZimDirEnt
parseZimDirEnt = do
  mimeType  <- fromIntegral       <$> getWord16le
  parmLen   <- fromIntegral       <$> getWord8
  namespace <- chr . fromIntegral <$> getWord8
  revision  <- fromIntegral       <$> getWord32le
  let deType = getDEType mimeType
  (redirectIndex, clusterNumber, blobNumber) <- parseRedirectClusterBlob deType
  url       <- BL.toStrict        <$> getLazyByteStringNul
  title     <- BL.toStrict        <$> getLazyByteStringNul
  -- specs: title is same as url if title is empty
  let title'
        | B.null title = url
        | otherwise = title
  return $ ZimDirEnt deType mimeType parmLen namespace revision
    redirectIndex clusterNumber blobNumber url title'
  where
    getDEType mimeType = case mimeType of
      0xffff -> ZimRedirectEntry
      0xfffe -> ZimLinkTarget
      0xfffd -> ZimDeletedEntry
      _      -> ZimArticleEntry
    offset = Just . fromIntegral
    parseRedirectClusterBlob deType = case deType of
      ZimArticleEntry  -> (\x y -> (Nothing, offset x, offset y)) <$> getWord32le <*> getWord32le
      ZimRedirectEntry -> (\x ->   (offset x, Nothing, Nothing )) <$> getWord32le
      ZimLinkTarget    -> skip 8 >> return (Nothing, Nothing, Nothing)
      ZimDeletedEntry  -> skip 8 >> return (Nothing, Nothing, Nothing)

class ZimGetDE k where
  getDE :: RunZim h => h -> k -> IO ZimDirEnt

instance ZimGetDE UrlIndex where
  getDE h (UrlIndex i) = runZim h $ \hdl hdr -> do
    let urlPtrPos    = zimUrlPtrPos hdr + 8 * toInteger i
        srcStart pos = sourceHandleRange hdl (Just $ fromIntegral pos) Nothing
    when (i < 0 || i >= zimArticleCount hdr) . throw $ ZimInvalidIndex i
    dePos <- srcStart urlPtrPos $$ sinkGet getWord64le
    srcStart dePos $$ sinkGet parseZimDirEnt

instance ZimGetDE TitleIndex where
  getDE h (TitleIndex i) = runZim h $ \hdl hdr -> do
    let titlePtrPos = Just $ zimTitlePtrPos hdr + 4 * fromIntegral i
        srcTitle    = sourceHandleRange hdl titlePtrPos Nothing
    when (i < 0 || i >= zimArticleCount hdr) . throw $ ZimInvalidIndex i
    urlIndex <- srcTitle $$ sinkGet getWord32le
    (hdl, hdr) `getDE` (UrlIndex $ fromIntegral urlIndex)

getCluster :: RunZim h => h -> ClusterNumber -> IO Cluster
getCluster h (ClusterNumber i) = runZim h $ \hdl hdr -> do
  let limit = zimClusterCount hdr - 1
  when (i < 0 || i > limit) . throw $ ZimInvalidIndex i
  let clusterPos = Just $ zimClusterPtrPos hdr + 8 * fromIntegral i
      src        = sourceHandleRange hdl clusterPos Nothing
  (pos0, pos1) <- src $$ sinkGet $ (,) <$> getWord64le <*> getWord64le
  -- length of last cluster is determined by checksum pos instead of next cluster pos
  let
    len
      | i == limit = fromIntegral (zimChecksumPos hdr) - pos0
      | otherwise  = pos1 - pos0
    toI = Just . fromIntegral
    srcCluster = sourceHandleRange hdl (toI pos0) (toI len)
  bs <- srcCluster $$ sinkGet getRemainingLazyByteString
  case BL.uncons bs of
    Just (0, cluster) -> return $ Cluster cluster
    Just (1, cluster) -> return $ Cluster cluster
    Just (4, cluster) -> return . Cluster $ decompress cluster
    Just (x, _) -> throw . ZimParseError $
      "Cluster " ++ show i ++ " (offset: " ++ showHex pos0 "" ++ ", length: " ++ show len ++ ") compressed with unsupported type: " ++ show x
    Nothing -> throw . ZimParseError $
      "Insufficient bytes for cluster " ++ show i

getBlob :: RunZim h => h -> (ClusterNumber, BlobNumber) -> IO Blob
getBlob h (c, BlobNumber b) = do
  Cluster cluster <- h `getCluster` c
  let src = sourceLbs (BL.drop (4 * fromIntegral b) cluster)
  (pos0, pos1) <- src $$ sinkGet $ (,) <$> getWord32le <*> getWord32le
  let len = pos1 - pos0
  return . Blob $ getRange pos0 len cluster
  where
    getRange p l = BL.take (fromIntegral l) . BL.drop (fromIntegral p)

-- | Returns URL of main page in ZIM.
-- This URL can be used for redirecting to the actual page.
getMainPageUrl :: RunZim h => h -> IO (Maybe Url)
getMainPageUrl h = runZim h $ \hdl hdr ->
  case zimMainPage hdr of
    Nothing -> return Nothing
    Just i -> do
      de <- (hdl, hdr) `getDE` UrlIndex i
      return . Just $ mkNsUrl (zimDeNamespace de) (zimDeUrl de)

class ZimGetContent k where
  -- | Get (MIME type, Content). Note that Content is lazy.
  getContent :: RunZim h => h -> k -> IO (Maybe (B.ByteString, BL.ByteString))

instance ZimGetContent (MimeList, ZimDirEnt) where
  getContent h (ml, de) = runZim h $ \hdl hdr -> do
    case zimDeType de of
      ZimRedirectEntry -> do
        let u = UrlIndex . fromJust $ zimDeRedirectIndex de
        (hdl, hdr) `getDE` u >>= ((hdl, hdr) `getContent`)
      ZimArticleEntry  -> do
        let (Just c, Just b) = (zimDeClusterNumber de, zimDeBlobNumber de)
        content <- unBlob <$> (hdl, hdr) `getBlob` (ClusterNumber c, BlobNumber b)
        return $ Just (ml ! zimDeMimeType de, content)
      _ -> return $ Just (ml ! zimDeMimeType de, BL.empty)

instance ZimGetContent ZimDirEnt where
  getContent h de = runZim h $ \hdl hdr -> do
    ml <- getMimeList (hdl, hdr)
    (hdl, hdr) `getContent` (ml, de)

instance ZimGetContent (MimeList, Url) where
  getContent h (ml, url) = runZim h $ \hdl hdr -> do
    des <- (hdl, hdr) `searchDE` url
    case des of
      [] -> return Nothing
      ((_, de) : _) -> (hdl, hdr) `getContent` (ml, de)

instance ZimGetContent Url where
  getContent h url = runZim h $ \hdl hdr -> do
    ml <- getMimeList (hdl, hdr)
    (hdl, hdr) `getContent` (ml, url)

instance ZimGetContent (MimeList, Title) where
  getContent h (ml, title) = runZim h $ \hdl hdr -> do
    des <- (hdl, hdr) `searchDE` title
    case des of
      [] -> return Nothing
      ((_, de) : _) -> (hdl, hdr) `getContent` (ml, de)

instance ZimGetContent Title where
  getContent h title = runZim h $ \hdl hdr -> do
    ml <- getMimeList (hdl, hdr)
    (hdl, hdr) `getContent` (ml, title)

instance ZimGetContent (MimeList, UrlIndex) where
  getContent h (ml, u) = runZim h $ \hdl hdr -> do
    de <- (hdl, hdr) `getDE` u
    (hdl, hdr) `getContent` (ml, de)

instance ZimGetContent UrlIndex where
  getContent h u = runZim h $ \hdl hdr -> do
    ml <- getMimeList (hdl, hdr)
    (hdl, hdr) `getContent` (ml, u)

instance ZimGetContent (MimeList, TitleIndex) where
  getContent h (ml, t) = runZim h $ \hdl hdr -> do
    de <- (hdl, hdr) `getDE` t
    (hdl, hdr) `getContent` (ml, de)

instance ZimGetContent TitleIndex where
  getContent h t = runZim h $ \hdl hdr -> do
    ml <- getMimeList (hdl, hdr)
    (hdl, hdr) `getContent` (ml, t)

-- Binary Search implementation used for searching sorted URL and Title lists.
binarySearch :: (Int -> IO (Ordering, a)) -> Int -> Int -> IO (Maybe a)
binarySearch f low high
  | high < low = return Nothing
  | otherwise = do
      let mid = (low + high) `div` 2
      (o, x) <- f mid
      case o of
        LT -> binarySearch f low       (mid -1)
        GT -> binarySearch f (mid + 1) high
        EQ -> return $ Just x

class ZimSearchDE k where
  -- | Search for a Directory Entry on a RunZim.
  -- When searching for a:
  --
  --  [@Url@]    Returns either 0 (not found) or 1 element.
  --  [@Title@]  Returns either 0 (not found) or 1 element.
  --  [@TitlePrefix@] Returns either 0 (not found) or 2 elements corresponding to lower and upper bound of titles containing the prefix.
  --
  searchDE :: RunZim h => h -> k -> IO [(Int, ZimDirEnt)]

instance ZimSearchDE Url where
  searchDE h url = runZim h $ \hdl hdr -> do
    let
      f i = do
        de <- (hdl, hdr) `getDE` UrlIndex i
        let v = mkNsUrl (zimDeNamespace de) (zimDeUrl de)
        return (compare url v, (UrlIndex i, de))
    res <- binarySearch f 0 (zimArticleCount hdr - 1)
    return $ maybe [] (\(UrlIndex i, r) -> [(i, r)]) res

instance ZimSearchDE Title where
  searchDE h title = runZim h $ \hdl hdr -> do
    let
      f i = do
        de <- (hdl, hdr) `getDE` TitleIndex i
        let v = mkNsTitle (zimDeNamespace de) (zimDeTitle de)
        return (compare title v, (TitleIndex i, de))
    res <- binarySearch f 0 (zimArticleCount hdr - 1)
    return $ maybe [] (\(TitleIndex i, r) -> [(i, r)]) res

instance ZimSearchDE TitlePrefix where
  searchDE h (TitlePrefix pre) = runZim h $ \hdl hdr -> do
    let
      preLen = B8.length pre - 2  -- minus namespace prefix
      limit = zimArticleCount hdr - 1
      -- extracts title to compare from Directory Entry
      mkT x = mkNsTitle (zimDeNamespace x) (B8.take preLen (zimDeTitle x))
      g idx = (\x -> (x, mkT x)) <$> (hdl, hdr) `getDE` idx
      -- i has to be the entry just before prefix matches
      lowerBound i = do
        de <- (hdl, hdr) `getDE` TitleIndex i
        case compare (Title pre) (mkT de) of
          -- if prefix matches, we still return LT as we want to find the entry BEFORE.
          -- special case: if i = 0, then this is the lower bound.
          EQ | i == 0 -> return (EQ, (TitleIndex i, de))
             | otherwise -> return (LT, (TitleIndex i, de))
          lgt -> do
            -- if succeeding entry has prefix, that is the lower bound.
            (de', Title v') <- g $ TitleIndex (i + 1)
            if pre `B8.isPrefixOf` v'
              then return (EQ, (TitleIndex $ i + 1, de'))
              else return (lgt, (TitleIndex i, de))
      upperBound i = do
        de <- (hdl, hdr) `getDE` TitleIndex i
        case compare (Title pre) (mkT de) of
          EQ | i == limit -> return (EQ, (TitleIndex i, de))
             | otherwise -> return (GT, (TitleIndex i, de))
          lgt -> do
            (de', Title v') <- g $ TitleIndex (i - 1)
            if pre `B8.isPrefixOf` v'
              then return (EQ, (TitleIndex $ i - 1, de'))
              else return (lgt, (TitleIndex i, de))

    lb <- binarySearch lowerBound 0 limit
    case lb of
      Nothing -> return []
      _ -> do
        ub <- binarySearch upperBound 0 limit
        let Just (TitleIndex lbi, lb') = lb
            Just (TitleIndex ubi, ub') = ub
        return [(lbi, lb'), (ubi, ub')]
