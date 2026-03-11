{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-orphans #-}
module Debian.Apt.Index
    ( update
    , Fetcher
    , CheckSums(..)
    , Compression(..)
    , FileTuple
    , Size
    , controlFromIndex
    , controlFromIndex'
    , findContentsFiles
    , findIndexes
    , indexesInRelease
    , tupleFromFilePath
    ) where

import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.BZip as BZip
import Control.Lens (over, to, view)
import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Digest.Pure.MD5 as MD5
import qualified Data.Digest.Pure.SHA as SHA
import Data.Either (partitionEithers)
import Data.Function
import Data.List as List (null, intercalate, sortBy, isSuffixOf, isPrefixOf)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Text as Text (Text, unpack, concat, lines, words)
import Data.Time
import Debian.Apt.Methods
import Debian.Codename (Codename, codename)
import Debian.Control (formatControl)
import Debian.Control.ByteString
--import Debian.Control.Common
import Debian.Control.Text (decodeControl)
import Debian.Release
import Debian.Sources
import Debian.URI (uriPathLens, uriToString')
import Debian.VendorURI (VendorURI, vendorURI)
import Network.URI
import System.Directory
import System.FilePath ((</>))
import System.Posix.Files
import System.FilePath (takeBaseName)
--import qualified System.Unix.Misc as Misc
import Text.ParserCombinators.Parsec.Error
import Text.PrettyPrint (render)
import Distribution.Pretty (pretty)
import Text.Read (readMaybe)
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure, (<$>), (<*>))
#endif

-- |Package indexes on the server are uncompressed or compressed with
-- gzip or bzip2. We do not know what will exist on the server until we
-- actually look. This type is used to mark the compression status of
-- what was actually found.
data Compression
    = BZ2 | GZ | Uncompressed
      deriving (Read, Show, Eq, Ord, Enum, Bounded)

data CheckSums
    = CheckSums { md5sum :: Maybe String
                , sha1   :: Maybe String
                , sha256 :: Maybe String
                }
      deriving (Read, Show, Eq)

-- |function-type for a function that downloads a file
-- The timestamp is optional. If the local file is as new or newer
-- than the remote copy, the download may be skipped.
--
-- A good choice might be a partially parameterized call to
-- 'Debian.Apt.Methods.fetch'
type Fetcher =
    URI ->              -- remote URI
    FilePath ->         -- local file name
    Maybe UTCTime ->    -- optional time stamp for local file
    IO Bool             -- True on success, False on failure

-- |update - similar to apt-get update

-- downloads the index files associated with a sources.list. The
-- downloaded index files will have the same basenames that apt-get uses
-- in \/var\/lib\/apt\/lists. You can almost use this function instead of
-- calling apt-get update. However there are a few key differences:
--  1. apt-get update also updates the binary cache files
--  2. apt-get update uses the partial directory and lock file in\ /var\/lib\/apt\/lists
--  3. apt-get update downloads the Release and Release.gpg files
update :: Fetcher -- ^ function that will do actually downloading
       -> FilePath -- ^ download indexes to the directory (must already exist)
       -> String -- ^ binary architecture
       -> [DebSource] -- ^ sources.list
       -> IO [Maybe (FilePath, Compression)] -- ^ (basename of index file, compression status)
update fetcher basePath arch sourcesList =
    mapM (uncurry $ fetchIndex fetcher) (map (\(uri, fp, _) -> (uri, (basePath </> fp))) (concatMap (indexURIs arch) sourcesList))

-- | download possibly compressed files
-- NOTE: index uri must not include the .bz2 or .gz extension
fetchIndex :: Fetcher -- ^ function that will do the actual fetch
           -> URI -- ^ remote URI of package index, without .bz2 or .gz extension
           -> FilePath -- ^ name to save downloaded file as, without .bz2 or .gz extension
           -> IO (Maybe (FilePath, Compression)) -- ^ (downloaded file name + extension, compression status)
fetchIndex fetcher uri localPath =
    do let localPath' = localPath ++ ".bz2"
       --lm <- getLastModified localPath'
       res <- fetcher (uri { uriPath = (uriPath uri) ++ ".bz2" }) localPath' Nothing
       if res
          then return $ Just (localPath', BZ2)
          else do let localPath' = localPath ++ ".gz"
                  lm <- getLastModified localPath'
                  res <- fetcher (uri { uriPath = (uriPath uri) ++ ".gz" }) localPath' lm
                  if res
                     then return $ Just (localPath', GZ)
                     else do lm <- getLastModified localPath
                             res <- fetcher (uri { uriPath = (uriPath uri) }) localPath lm
                             if res
                                then return (Just (localPath, Uncompressed))
                                else return Nothing

-- |examine a DebSource line, and calculate for each section:
--  - the URI to the uncompressed index file
--  - the basename that apt-get would name the downloaded index
-- FIXME: ExactPath dist will fail with error at runtime :(
indexURIs :: String -- ^ which binary architecture
          -> DebSource -- ^ line from sources.list
          -> [(URI, FilePath, DebSource)] -- ^ (remote uri, local name, deb source for just this section)
indexURIs arch debSource =
    map (\ section -> let (uri, fp) = calcPath (view sourceType debSource) arch baseURI release section
                      in (uri,fp, debSource { _sourceDist = (Right (release, [section])) }) ) sections
    where
      baseURI = view sourceUri debSource
      (release, sections) =
          either (error $ "indexURIs: support not implemented for exact path: " ++ render (pretty debSource)) id (view sourceDist debSource)

-- |return a tuple for the section
--  - the URI to the uncompressed index file
--  - the basename that apt-get uses for the downloaded index
-- FIXME: support for Release and Release.gpg
calcPath :: SourceType -- ^ do we want Packages or Sources
         -> String  -- ^ The binary architecture to use for Packages
         -> VendorURI -- ^ base URI as it appears in sources.list
         -> Codename -- ^ the release (e.g., unstable, testing, stable, sid, etc)
         -> Section -- ^ the section (main, contrib, non-free, etc)
         -> (URI, [Char]) -- ^ (uri to index file, basename for the downloaded file)
calcPath srcType arch baseURI release section =
          let indexPath = case srcType of
                      DebSrc -> "source/Sources"
                      Deb -> "binary-" ++ arch </> "Packages"
              uri' = over uriPathLens (\path -> path </> "dists" </> codename release </> sectionName' section </> indexPath) (view vendorURI baseURI)
              path = view uriPathLens uri'
          in (uri', addPrefix (escapePath path))
          where
            addPrefix s = prefix scheme user' pass' reg port ++ {- "_" ++ -} s
            prefix "http:" (Just user) Nothing (Just host) port = user ++ host ++ port
            prefix "http:" _ _ (Just host) port = host ++ port
            prefix "ftp:" _ _ (Just host) _ = host
            prefix "file:" Nothing Nothing Nothing "" = ""
            prefix "ssh:" (Just user) Nothing (Just host) port = user ++ host ++ port
            prefix "ssh:" _ _ (Just host) port = host ++ port
            prefix _ _ _ _ _ = error ("calcPath: unsupported uri: " ++ view (vendorURI . to uriToString') baseURI)
            user' = maybeOfString user
            pass' = maybeOfString pass
            (user, pass) = break (== ':') userpass
            userpass = maybe "" uriUserInfo auth
            reg = maybeOfString $ maybe "" uriRegName auth
            port = maybe "" uriPort auth
            scheme = view (vendorURI . to uriScheme) baseURI
            auth = view (vendorURI . to uriAuthority) baseURI
            --path = uriPath baseURI

            escapePath :: String -> String
            escapePath s = intercalate "_" $ wordsBy (== '/') s

            maybeOfString :: String -> Maybe String
            maybeOfString "" = Nothing
            maybeOfString s = Just s

            wordsBy :: Eq a => (a -> Bool) -> [a] -> [[a]]
            wordsBy p s =
                case (break p s) of
                  (s, []) -> [s]
                  (h, t) -> h : wordsBy p (drop 1 t)

-- |Parse a possibly compressed index file.
controlFromIndex :: Compression -> FilePath -> L.ByteString -> Either ParseError (Control' Text)
controlFromIndex GZ path s = either Left (Right . decodeControl) . parseControl path . B.concat . L.toChunks . GZip.decompress $ s
controlFromIndex BZ2 path s = either Left (Right . decodeControl) . parseControl path . B.concat . L.toChunks . BZip.decompress $ s
controlFromIndex Uncompressed path s = either Left (Right . decodeControl) . parseControl path . B.concat . L.toChunks $ s

-- |parse an index possibly compressed file
controlFromIndex' :: Compression -> FilePath -> IO (Either ParseError (Control' Text))
controlFromIndex' compression path = L.readFile path >>= return . controlFromIndex compression path

type Size = Integer
type FileTuple = (CheckSums, Size, FilePath)

-- |A release file contains a list of indexes (Packages\/Sources). Each
-- Package or Source index may appear multiple times because it may be
-- compressed several different ways. This function will return an
-- assoc list where the key is the name of the uncompressed package
-- index name and the value is the list of (file, compression) which
-- decompress to the key.
groupIndexes :: [FileTuple] -> [(FilePath, [(FileTuple, Compression)])]
groupIndexes indexFiles =
    M.toList $ M.fromListWith combine $ map makeKV indexFiles
    where
      makeKV fileTuple@(_,_,fp) =
          let (name, compressionMethod) = uncompressedName fp
          in
            (name, [(fileTuple, compressionMethod)])
      combine = (\x y -> sortBy (compare `on` snd) (x ++ y))
{-
      with t@(_,_,fp) m =
          let (un, compression) =
          in
            M.insertWith
-}

{-
groupIndexes' :: String ->[FileTuple] -> [(FilePath, [(FileTuple, Compression)])]
groupIndexes' iType indexFiles =
    M.toList (foldr (insertType iType) M.empty indexFiles)
    where
      insertType iType t@(_,_,fp) m =
          case uncompressedName' iType fp of
            Nothing -> m
            (Just (un, compression)) ->
                M.insertWith (\x y -> sortBy (compare `on` snd) (x ++ y)) un [(t, compression)] m
-}

-- |The release file contains the checksums for the uncompressed
-- package indexes, even if the uncompressed package indexes are not
-- stored on the server. This function returns the list of files that
-- actually exist.
filterExists :: FilePath -> (FilePath, [(FileTuple, Compression)]) -> IO (FilePath, [(FileTuple, Compression)])
filterExists distDir (fp, alternatives) =
          do e <- filterM ( \((_,_,fp),_) -> fileExist (distDir </> fp)) alternatives
             -- when (null e) (error $ "None of these files exist: " ++ show alternatives)
             return (fp, e)

findIndexes :: FilePath -> String -> [FileTuple] -> IO [(FileTuple, Compression)]
findIndexes distDir iType controlFiles =
    let indexes = groupIndexes controlFiles
    in
      do indexes' <- mapM (filterExists distDir) (filter (isType iType) indexes)
         return $ map (head . snd) (filter (not . List.null . snd) indexes')
    where
      isType iType (fp, _) = iType `isSuffixOf` fp

{-
findIndexes' :: FilePath -> String -> [FileTuple] -> IO [(FileTuple, Compression)]
findIndexes' distDir iType controlFiles =
    let m = groupIndexes' iType controlFiles
    in
      do m' <- mapM (filterExists distDir) m
         return $ map (head . snd) (filter (not . null . snd) m')
-}

      -- insertType :: String -> (CheckSums, Integer, FilePath) -> M.Map FilePath ((CheckSums, Integer, FilePath), Compression) -> M.Map FilePath ((CheckSums, Integer, FilePath), Compression)

{-
uncompressedName' :: String -> FilePath -> Maybe (FilePath, Compression)
uncompressedName' iType fp
          | isSuffixOf iType fp = Just (fp, Uncompressed)
          | isSuffixOf (iType ++".gz") fp = Just (reverse . (drop 3) . reverse $ fp, GZ)
          | isSuffixOf (iType ++".bz2") fp = Just (reverse . (drop 4) . reverse $ fp, BZ2)
          | otherwise = Nothing
-}

uncompressedName :: FilePath -> (FilePath, Compression)
uncompressedName fp
          | isSuffixOf ".gz"  fp = (reverse . (drop 3) . reverse $ fp, GZ)
          | isSuffixOf ".bz2" fp = (reverse . (drop 4) . reverse $ fp, BZ2)
          | otherwise            = (fp, Uncompressed)

indexesInRelease :: (FilePath -> Bool)
                 -> Control' Text -- ^ A release file
                 -> [(CheckSums, Integer, FilePath)] -- ^
indexesInRelease filterp (Control [p]) =
    -- In a release file we should find one or more of the fields
    -- "SHA256", "SHA1", or "MD5Sum", each containing a list of triples
    case attempts of
      (_, fps:_) -> filter (\(_,_,fp) -> filterp fp) fps
      (errs,  _) -> error $ "No indexes in release: " <> intercalate ", " errs
    where
      attempts = partitionEithers
        [ attempt makeSHA256 "SHA256"
        , attempt makeSHA1 "SHA1"
        , attempt makeMD5 "MD5Sum" ]

      attempt mksum fn = makeTuples mksum =<< makeTriples =<<
        maybe (Left $ "No " <> fn <> " Field") Right (fieldValue fn p)

      makeSHA256 s = CheckSums {md5sum = Nothing, sha1 = Nothing, sha256 = Just s}
      makeSHA1 s = CheckSums {md5sum = Nothing, sha1 = Just s, sha256 = Nothing}
      makeMD5 s = CheckSums {md5sum = Just s, sha1 = Nothing, sha256 = Nothing}

      makeTuples :: (String -> CheckSums) -> [(Text, Text, Text)] -> Either String [(CheckSums, Integer, FilePath)]
      makeTuples mk triples =
          case partitionEithers (fmap (makeTuple mk) triples) of
            ([], tuples) -> Right tuples
            (s : _, _) -> Left s

      makeTuple :: (String -> CheckSums) -> (Text, Text, Text) -> Either String (CheckSums, Integer, FilePath)
      makeTuple mk (sum, size, fp) =
          (,,) <$> pure (mk (Text.unpack sum))
               <*> maybe (Left ("Invalid size field: " ++ show size)) Right (readMaybe (Text.unpack size))
               <*> pure (Text.unpack fp)

      makeTriples :: Text -> Either String [(Text, Text, Text)]
      makeTriples t = case partitionEithers (map makeTriple (Text.lines t)) of
                        ([], xs) -> Right xs
                        (s : _, _) -> Left s

      makeTriple :: Text -> Either String (Text, Text, Text)
      makeTriple t = case Text.words t of
                       [a, b, c] -> Right (a, b, c)
                       _ -> Left ("Invalid checksum line: " ++ show t)
indexesInRelease _ x = error $ "Invalid release file: " <> Text.unpack (Text.concat (formatControl x))

-- |make a FileTuple for a file found on the local disk
-- returns 'Nothing' if the file does not exist.
tupleFromFilePath :: FilePath -> FilePath -> IO (Maybe FileTuple)
tupleFromFilePath basePath fp =
          do e <- fileExist (basePath </> fp)
             if not e
              then return Nothing
              else do size <- getFileStatus (basePath </> fp) >>= return . fromIntegral . fileSize
                      md5 <- L.readFile (basePath </> fp) >>= return . show . MD5.md5
                      sha1 <- L.readFile (basePath </> fp) >>= return . show . SHA.sha1
                      sha256 <- L.readFile (basePath </> fp) >>= return . show . SHA.sha256
                      return $ Just (CheckSums { md5sum = Just md5, sha1 = Just sha1, sha256 = Just sha256 }, size, fp)

-- |find the Contents-* files. These are not listed in the Release file
findContentsFiles :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
findContentsFiles filterP distDir =
          do files <- getDirectoryContents distDir
             return $ filter filterP $ filter (isPrefixOf "Contents-" . takeBaseName) files
