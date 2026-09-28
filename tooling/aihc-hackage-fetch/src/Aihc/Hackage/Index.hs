-- | Parse a Hackage @01-index.tar.gz@ into package versions.
--
-- The index carries two kinds of entry this module reads. A
-- @\<name\>\/\<version\>\/\<name\>.cabal@ entry records that a version was
-- uploaded, or that its cabal file was revised: the index is append-only,
-- so the @n@th such entry for a version is revision @n - 1@ of its cabal
-- file. A @\<name\>\/preferred-versions@ entry records the range the
-- maintainer still prefers. Deprecating a version is expressed by punching
-- a hole in that range, so @foo <1.1 || >1.1@ deprecates @1.1@. A package
-- without such an entry has no restriction.
--
-- 'scanIndex' reads the uncompressed index once and records, for every
-- cabal entry, where in the tarball it sits, so that the cabal file of any
-- version and revision can be read back later without holding the index in
-- memory. The older two-pass functions derive the newest preferred version
-- of each package and are kept for the tools that only need that.
module Aihc.Hackage.Index
  ( parseHackageIndex,
    parseHackageIndexUpdatedSince,
    PreferredRanges,
    parsePreferredRanges,
    parsePreferredVersionsEntry,
    latestPreferredVersions,

    -- * Every version and revision
    IndexScan (..),
    IndexEntry (..),
    scanIndex,
    readIndexEntry,
  )
where

import Aihc.Hackage.Types (PackageSpec (..))
import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Archive.Tar.Index (TarEntryOffset, hReadEntry, nextEntryOffset)
import Codec.Compression.GZip qualified as GZip
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.Int (Int64)
import Data.List (isPrefixOf)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Types.Dependency (Dependency, depVerRange)
import Distribution.Types.Version (Version)
import Distribution.Types.VersionRange (VersionRange, intersectVersionRanges, withinRange)
import System.FilePath.Posix (splitDirectories)
import System.IO (Handle)

-- | The version range each package's maintainer still prefers, by package
-- name. A package absent from the map has no restriction.
type PreferredRanges = Map.Map String VersionRange

-- | Parse a compressed Hackage @01-index.tar.gz@ into latest package versions.
parseHackageIndex :: LBSC.ByteString -> Either String [PackageSpec]
parseHackageIndex =
  parseHackageIndexWith (const True)

-- | Parse a compressed Hackage @01-index.tar.gz@ into latest package versions
-- whose latest package entry was uploaded at or after the given Unix timestamp.
parseHackageIndexUpdatedSince :: Int64 -> LBSC.ByteString -> Either String [PackageSpec]
parseHackageIndexUpdatedSince cutoff =
  parseHackageIndexWith (\(_, uploadedAt) -> uploadedAt >= cutoff)

parseHackageIndexWith :: ((Version, Int64) -> Bool) -> LBSC.ByteString -> Either String [PackageSpec]
parseHackageIndexWith keep bytes =
  case collectEntries Map.empty (Tar.read (GZip.decompress bytes)) of
    Left err -> Left err
    Right packages
      | Map.null packages -> Left "No package versions found in Hackage index"
      | otherwise ->
          Right
            [ PackageSpec name (prettyShow version)
            | (name, (version, uploadedAt)) <- Map.toAscList packages,
              keep (version, uploadedAt)
            ]
  where
    collectEntry packages entry =
      case packageVersionFromEntryPath (Tar.entryPath entry) of
        Nothing -> packages
        Just (name, version) ->
          Map.insertWith newerVersion name (version, Tar.entryTime entry) packages

    newerVersion new old =
      if fst new > fst old then new else old

    -- Force the accumulator at each step. A lazy chain of 'collectEntry'
    -- thunks pins every entry, and with it the whole decompressed index.
    collectEntries packages (Tar.Next entry rest) =
      let next = collectEntry packages entry
       in next `seq` collectEntries next rest
    collectEntries packages Tar.Done = Right packages
    collectEntries _ (Tar.Fail err) = Left (show err)

-- | Collect the preferred version range of every package in the index.
--
-- A package whose entry is empty or unparseable is left out, which leaves it
-- unrestricted rather than excluding every version of it.
parsePreferredRanges :: LBSC.ByteString -> Either String PreferredRanges
parsePreferredRanges bytes =
  collectEntries Map.empty (Tar.read (GZip.decompress bytes))
  where
    -- Force the accumulator at each step. A lazy chain of 'collectEntry'
    -- thunks pins every entry, and with it the whole decompressed index.
    collectEntries ranges (Tar.Next entry rest) =
      let next = collectPreferredEntry ranges entry
       in next `seq` collectEntries next rest
    collectEntries ranges Tar.Done = Right ranges
    collectEntries _ (Tar.Fail err) = Left (show err)

collectPreferredEntry :: PreferredRanges -> Tar.Entry -> PreferredRanges
collectPreferredEntry ranges entry =
  case (preferredVersionsPackageName (Tar.entryPath entry), Tar.entryContent entry) of
    (Just name, Tar.NormalFile contents _) ->
      -- The index is append-only, so a later entry for a package is the
      -- one in force and replaces what an earlier entry said.
      case parsePreferredVersionsEntry contents of
        Nothing -> Map.delete name ranges
        Just range -> Map.insert name range ranges
    _ -> ranges

-- | Parse the contents of a @preferred-versions@ entry into a version range.
--
-- The file holds @build-depends@ style constraint lines, as in
-- @binary >=0.8 && <0.9@. Several lines constrain the package together.
-- Comment and blank lines are ignored, and so is the package name each line
-- carries, because the entry path already names the package.
parsePreferredVersionsEntry :: LBSC.ByteString -> Maybe VersionRange
parsePreferredVersionsEntry contents =
  case mapMaybe parseConstraintLine (lines (LBSC.unpack contents)) of
    [] -> Nothing
    range : rest -> Just (foldl' intersectVersionRanges range rest)
  where
    parseConstraintLine line
      | null trimmed = Nothing
      | "--" `isPrefixOf` trimmed = Nothing
      | otherwise = depVerRange <$> (simpleParsec trimmed :: Maybe Dependency)
      where
        trimmed = dropWhile (== ' ') line

-- | Collect the newest version of each package that its preferred range admits.
--
-- A package with no range in 'PreferredRanges' keeps its newest version. One
-- whose every version is excluded is left out entirely, because there is no
-- version of it to prefer.
latestPreferredVersions :: PreferredRanges -> LBSC.ByteString -> Either String (Map.Map String Version)
latestPreferredVersions ranges bytes =
  case collectEntries Map.empty (Tar.read (GZip.decompress bytes)) of
    Left err -> Left err
    Right packages
      | Map.null packages -> Left "No package versions found in Hackage index"
      | otherwise -> Right packages
  where
    -- Force the accumulator at each step. A lazy chain of 'collectEntry'
    -- thunks pins every entry, and with it the whole decompressed index.
    collectEntries packages (Tar.Next entry rest) =
      let next = collectEntry packages entry
       in next `seq` collectEntries next rest
    collectEntries packages Tar.Done = Right packages
    collectEntries _ (Tar.Fail err) = Left (show err)

    collectEntry packages entry =
      case packageVersionFromEntryPath (Tar.entryPath entry) of
        Nothing -> packages
        Just (name, version)
          | preferred name version -> Map.insertWith max name version packages
          | otherwise -> packages

    preferred name version =
      case Map.lookup name ranges of
        Nothing -> True
        Just range -> withinRange version range

-- | One cabal file of the index: a version of a package at one revision,
-- and where its entry sits in the uncompressed tarball.
data IndexEntry = IndexEntry
  { indexEntryVersion :: !Version,
    -- | Revision @0@ is the cabal file the release was uploaded with.
    indexEntryRevision :: !Int,
    -- | The block offset of the entry, for 'readIndexEntry'.
    indexEntryOffset :: !TarEntryOffset
  }
  deriving (Eq, Show)

-- | What one pass over the index records.
data IndexScan = IndexScan
  { -- | Every cabal entry of every package, in the order the index lists
    -- them: for one version, increasing revisions.
    scanEntries :: !(Map.Map String [IndexEntry]),
    scanPreferredRanges :: !PreferredRanges,
    -- | The newest modification time among the entries, in Unix seconds:
    -- the moment the index describes.
    scanIndexState :: !Int64
  }
  deriving (Eq, Show)

-- | Scan an uncompressed index once for every cabal entry and every
-- preferred range.
scanIndex :: LBS.ByteString -> Either String IndexScan
scanIndex bytes =
  finish <$> collectEntries (0, Map.empty, Map.empty, 0) (Tar.read bytes)
  where
    finish (_, entries, ranges, latest) =
      IndexScan
        { scanEntries = Map.map reverse entries,
          scanPreferredRanges = ranges,
          scanIndexState = latest
        }

    -- Force the accumulator at each step. A lazy chain of thunks pins every
    -- entry, and with it the whole decompressed index.
    collectEntries acc (Tar.Next entry rest) =
      let next = collectEntry acc entry
       in next `seq` collectEntries next rest
    collectEntries acc Tar.Done = Right acc
    collectEntries _ (Tar.Fail err) = Left (show err)

    collectEntry (!offset, !entries, !ranges, !latest) entry =
      let offset' = nextEntryOffset entry offset
          latest' = max latest (Tar.entryTime entry)
          ranges' = collectPreferredEntry ranges entry
          entries' =
            case packageVersionFromEntryPath (Tar.entryPath entry) of
              Nothing -> entries
              Just (name, version) -> Map.alter (Just . addRevision version offset) name entries
       in (offset', entries', ranges', latest')

    -- The revisions of a version arrive in order, so the count of earlier
    -- entries for the same version is the revision number of this one.
    addRevision version offset previous =
      let earlier = fromMaybe [] previous
          revision = length [() | IndexEntry v _ _ <- earlier, v == version]
       in IndexEntry version revision offset : earlier

-- | Read the cabal file at an entry's offset from the uncompressed index.
readIndexEntry :: Handle -> IndexEntry -> IO BS.ByteString
readIndexEntry handle entry = do
  tarEntry <- hReadEntry handle (indexEntryOffset entry)
  case Tar.entryContent tarEntry of
    Tar.NormalFile contents _ -> pure (LBS.toStrict contents)
    _ -> ioError (userError ("The Hackage index entry at block " <> show (indexEntryOffset entry) <> " is not a file"))

-- | The package a @\<name\>\/preferred-versions@ entry belongs to.
preferredVersionsPackageName :: FilePath -> Maybe String
preferredVersionsPackageName path =
  case splitDirectories path of
    [name, "preferred-versions"] -> Just name
    _ -> Nothing

packageVersionFromEntryPath :: FilePath -> Maybe (String, Version)
packageVersionFromEntryPath path =
  case splitDirectories path of
    [name, rawVersion, cabalFile]
      | cabalFile == name ++ ".cabal",
        Just version <- simpleParsec rawVersion ->
          Just (name, version)
    _ -> Nothing
