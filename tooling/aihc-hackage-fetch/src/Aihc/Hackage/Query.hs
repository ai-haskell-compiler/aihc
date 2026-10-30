{-# LANGUAGE ScopedTypeVariables #-}

-- | The Hackage package index, read one package at a time.
--
-- Hackage serves the whole index as a tarball, and reading that tarball is
-- how aihc used to answer every question about a package. The tarball is a
-- 140 megabyte download and a gigabyte on disk, which is a large price for
-- the few hundred cabal files that one plan reads.
--
-- This module asks Hackage for one package at a time instead. Two
-- endpoints answer every question the planner has:
--
--   * @\/package\/NAME.json@ gives every version of a package and whether
--     the maintainer deprecates it. The body is a few hundred bytes.
--   * @\/package\/NAME-VERSION\/NAME.cabal@ gives the newest revision of
--     one cabal file, and @\/package\/NAME-VERSION\/revision\/N.cabal@
--     gives revision @N@ of it.
--
-- Each answer is kept under @~\/.cache\/aihc\/hackage-query@. A cabal file
-- at a numbered revision never changes, so it is cached for good. A version
-- list and the newest revision of a cabal file do change, so they are
-- refetched once they reach 'queryMaxAge'.
--
-- The cost of this is round trips: a cold plan of forty packages makes a
-- few hundred small requests, where the tarball made one large one. A plan
-- that a lock file pins reads only cached files and makes none.
module Aihc.Hackage.Query
  ( HackageQueries,
    QueryOptions (..),
    defaultQueryOptions,
    newHackageQueries,
    QueryVersion (..),
    queryPackageVersions,
    queryCabalFile,
    getQueryCacheDir,

    -- * The endpoints
    hackageVersionsUrl,
    hackageCabalUrl,

    -- * The pieces of an answer
    parseVersionsJson,
    cabalFileRevision,
  )
where

import Aihc.Hackage.Cache (getHackageCacheDir)
import Aihc.Hackage.Package (Version, parseVersionString, showVersion)
import Aihc.Http (httpGet)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (unless, when)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isDigit, isSpace, toLower)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import System.Directory (createDirectoryIfMissing, doesFileExist, getModificationTime, renameFile)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (hClose, hPutStrLn, openBinaryTempFile, stderr)

-- | Where one package's versions are listed.
hackageVersionsUrl :: String -> String
hackageVersionsUrl name = "https://hackage.haskell.org/package/" <> name <> ".json"

-- | Where one cabal file is served, at a numbered revision or at the
-- newest one.
hackageCabalUrl :: String -> Version -> Maybe Int -> String
hackageCabalUrl name version revision =
  "https://hackage.haskell.org/package/"
    <> name
    <> "-"
    <> showVersion version
    <> case revision of
      Nothing -> "/" <> name <> ".cabal"
      Just wanted -> "/revision/" <> show wanted <> ".cabal"

-- | How the queries may be made.
data QueryOptions = QueryOptions
  { -- | Reask for an answer that can change once the cached copy is older
    -- than this.
    queryMaxAge :: !NominalDiffTime,
    -- | Allow asking Hackage. A cached answer is still read when this is
    -- off.
    queryAllowNetwork :: !Bool,
    queryVerbose :: !Bool
  }

-- | Reask for a day-old answer, over the network, with progress on stderr.
defaultQueryOptions :: QueryOptions
defaultQueryOptions =
  QueryOptions
    { queryMaxAge = 24 * 60 * 60,
      queryAllowNetwork = True,
      queryVerbose = True
    }

-- | A handle on the queries, holding the answers this process has already.
--
-- A plan asks for the same version list once per package and the same cabal
-- file once per candidate it tries, so the answers are kept for the rest of
-- the process and the disk is read once.
data HackageQueries = HackageQueries
  { queryOptions :: !QueryOptions,
    queryVersionMemo :: !(IORef (Map String (Maybe [QueryVersion]))),
    queryCabalMemo :: !(IORef (Map (String, Version, Maybe Int) (Either String (Int, BS.ByteString))))
  }

-- | One version of a package as Hackage lists it.
data QueryVersion = QueryVersion
  { queryVersionVersion :: !Version,
    -- | The maintainer's @preferred-versions@ exclude this version.
    queryVersionDeprecated :: !Bool
  }
  deriving (Eq, Show)

-- | Create a handle that asks Hackage as questions arrive.
newHackageQueries :: QueryOptions -> IO HackageQueries
newHackageQueries opts = HackageQueries opts <$> newIORef Map.empty <*> newIORef Map.empty

-- | XDG cache directory holding the answers.
--
-- @~\/.cache\/aihc\/hackage-query@
getQueryCacheDir :: IO FilePath
getQueryCacheDir = do
  cacheBase <- getHackageCacheDir
  pure (cacheBase <> "-query")

-- | The versions of a package, newest first, or 'Nothing' for a package
-- Hackage does not have.
queryPackageVersions :: HackageQueries -> String -> IO (Maybe [QueryVersion])
queryPackageVersions queries name = do
  memo <- readIORef (queryVersionMemo queries)
  case Map.lookup name memo of
    Just answer -> pure answer
    Nothing -> do
      cacheDir <- getQueryCacheDir
      let path = cacheDir </> "versions" </> (name <> ".json")
      body <- fetchCached queries path (hackageVersionsUrl name) ("the versions of " <> name) Mutable
      answer <- case body of
        NotFound -> pure Nothing
        Fetched contents ->
          case parseVersionsJson contents of
            Left problem -> ioError (userError ("Hackage listed the versions of " <> name <> " in a shape aihc does not read: " <> problem))
            Right versions -> pure (Just versions)
      remember (queryVersionMemo queries) name answer
      pure answer

-- | The cabal file of one version at one revision, or at the newest
-- revision when none is given, with the revision that was read.
queryCabalFile :: HackageQueries -> String -> Version -> Maybe Int -> IO (Either String (Int, BS.ByteString))
queryCabalFile queries name version wantedRevision = do
  memo <- readIORef (queryCabalMemo queries)
  case Map.lookup key memo of
    Just answer -> pure answer
    Nothing -> do
      cacheDir <- getQueryCacheDir
      let path = cacheDir </> "cabal" </> cabalFileName name version wantedRevision
          -- A numbered revision of a cabal file never changes, so it is
          -- cached for good. The newest revision changes when the
          -- maintainer revises the release.
          freshness = maybe Mutable (const Immutable) wantedRevision
      body <- fetchCached queries path (hackageCabalUrl name version wantedRevision) description freshness
      answer <- case body of
        NotFound -> pure (Left missing)
        Fetched contents -> do
          -- A file asked for by number is at that number, whatever it
          -- holds: the endpoint names the revision it serves. Only the
          -- newest revision has to be read out of the file.
          let revision = fromMaybe (cabalFileRevision contents) wantedRevision
          -- The newest revision turned out to be this numbered one, so the
          -- answer is kept under that number too. A later ask for it, to
          -- replay a lock file say, is then free.
          case wantedRevision of
            Just _ -> pure ()
            Nothing -> do
              let numbered = cacheDir </> "cabal" </> cabalFileName name version (Just revision)
              exists <- doesFileExist numbered
              unless exists (writeFileAtomically numbered contents)
              remember (queryCabalMemo queries) (name, version, Just revision) (Right (revision, contents))
          pure (Right (revision, contents))
      remember (queryCabalMemo queries) key answer
      pure answer
  where
    key = (name, version, wantedRevision)
    release = name <> "-" <> showVersion version
    description =
      "the cabal file of " <> release <> case wantedRevision of
        Nothing -> ""
        Just wanted -> " at revision " <> show wanted
    missing = case wantedRevision of
      Nothing -> "Hackage has no " <> release
      Just wanted -> "Hackage has no revision " <> show wanted <> " of " <> release

-- | Where one cabal file is cached. A numbered revision is named by its
-- number; the newest revision has no number until it is read, so it is
-- cached under its own name.
cabalFileName :: String -> Version -> Maybe Int -> FilePath
cabalFileName name version revision =
  name
    <> "-"
    <> showVersion version
    <> case revision of
      Nothing -> "-latest.cabal"
      Just wanted -> "-r" <> show wanted <> ".cabal"

remember :: (Ord k) => IORef (Map k v) -> k -> v -> IO ()
remember ref key value = atomicModifyIORef' ref (\memo -> (Map.insert key value memo, ()))

-- | Whether a cached answer may be used without asking again.
data Freshness
  = -- | The answer can change, so a cached copy expires.
    Mutable
  | -- | The answer never changes, so a cached copy is kept for good.
    Immutable

-- | What an answer turned out to be.
data Answer
  = Fetched !BS.ByteString
  | -- | Hackage does not have it. This is an answer, not a failure.
    NotFound

-- | Answer from the cache, or ask Hackage and cache what it says.
--
-- A failed request falls back to a stale cached copy, so a broken network
-- degrades a plan to yesterday's answers rather than failing it.
fetchCached :: HackageQueries -> FilePath -> String -> String -> Freshness -> IO Answer
fetchCached queries path url description freshness = do
  usable <- cachedIsUsable (queryOptions queries) path freshness
  if usable
    then Fetched <$> BS.readFile path
    else
      if not (queryAllowNetwork (queryOptions queries))
        then do
          cached <- doesFileExist path
          if cached
            then Fetched <$> BS.readFile path
            else ioError (userError ("Hackage is not cached for " <> description <> " and the network is disabled"))
        else do
          when (queryVerbose (queryOptions queries)) $
            hPutStrLn stderr ("Asking Hackage for " <> description <> "...")
          fetched <- try (httpGet url)
          case fetched of
            Right (Right body) -> do
              let contents = LBS.toStrict body
              createDirectoryIfMissing True (takeDirectory path)
              writeFileAtomically path contents
              pure (Fetched contents)
            Right (Left failure)
              | isNotFound failure -> pure NotFound
              | otherwise -> fallback failure
            Left (err :: SomeException) -> fallback (displayException err)
  where
    fallback failure = do
      cached <- doesFileExist path
      if cached
        then do
          hPutStrLn stderr ("Warning: could not ask Hackage for " <> description <> ", using the cached answer: " <> failure)
          Fetched <$> BS.readFile path
        else ioError (userError ("Failed to ask Hackage for " <> description <> ": " <> failure))

-- | A cached file is present and, when the answer can change, young
-- enough.
cachedIsUsable :: QueryOptions -> FilePath -> Freshness -> IO Bool
cachedIsUsable opts path freshness = do
  exists <- doesFileExist path
  if not exists
    then pure False
    else case freshness of
      Immutable -> pure True
      Mutable -> do
        modified <- getModificationTime path
        now <- getCurrentTime
        pure (diffUTCTime now modified <= queryMaxAge opts)

-- | Both HTTP backends of 'Aihc.Http' report a status this way.
isNotFound :: String -> Bool
isNotFound failure = "HTTP 404 " `isPrefixOf` failure

-- | Write through a temporary file in the same directory, so an
-- interrupted write leaves no half-written answer behind. The name is
-- unique, because two solves can cache the same answer at the same time and
-- must not rename each other's unfinished file into place.
writeFileAtomically :: FilePath -> BS.ByteString -> IO ()
writeFileAtomically path contents = do
  (temporary, handle) <- openBinaryTempFile (takeDirectory path) (takeFileName path <> ".tmp")
  BS.hPut handle contents
  hClose handle
  renameFile temporary path

-- | Read what @\/package\/NAME.json@ says: an object of one field per
-- version, whose value is @normal@ or @deprecated@. The versions come back
-- newest first, as the planner tries them in that order.
parseVersionsJson :: BS.ByteString -> Either String [QueryVersion]
parseVersionsJson contents = do
  listed <- Aeson.eitherDecodeStrict contents :: Either String (Map String String)
  versions <- traverse one (Map.toList listed)
  pure (sortOn (Down . queryVersionVersion) versions)
  where
    one (versionText, status) =
      case parseVersionString versionText of
        Nothing -> Left ("could not read the version " <> show versionText)
        Just version -> Right (QueryVersion version (status == "deprecated"))

-- | The revision a cabal file is at. A revised file carries the field
-- @x-revision@; the first upload of a release carries no such field and is
-- revision zero.
cabalFileRevision :: BS.ByteString -> Int
cabalFileRevision contents =
  case [value | line <- BSC.lines contents, Just value <- [revisionField line]] of
    value : _ -> value
    [] -> 0
  where
    revisionField line = do
      let (field, rest) = BSC.break (== ':') line
      if BSC.map toLower (BSC.dropWhile isSpace field) /= BSC.pack "x-revision"
        then Nothing
        else do
          let value = BSC.dropWhile isSpace (BSC.drop 1 rest)
          (number, remaining) <- BSC.readInt (BSC.takeWhile isDigit value)
          if BS.null remaining then Just number else Nothing
