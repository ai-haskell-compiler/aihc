-- | Parse a Hackage @01-index.tar.gz@ into package versions.
--
-- The index carries two kinds of entry this module reads. A
-- @\<name\>\/\<version\>\/\<name\>.cabal@ entry records that a version was
-- uploaded, and a @\<name\>\/preferred-versions@ entry records the range its
-- maintainer still prefers. Deprecating a version is expressed by punching a
-- hole in that range, so @foo <1.1 || >1.1@ deprecates @1.1@. A package
-- without such an entry has no restriction.
--
-- Resolving the newest non-deprecated version therefore needs both kinds of
-- entry, and the index is append-only rather than grouped by package, so a
-- package's range can appear after the versions it constrains. The two passes
-- below read the index twice instead of retaining every version of every
-- package in memory: 'parsePreferredRanges' collects the ranges, which are few,
-- and 'latestPreferredVersions' then keeps only the newest version of each
-- package that its range admits.
module Aihc.Hackage.Index
  ( parseHackageIndex,
    parseHackageIndexUpdatedSince,
    PreferredRanges,
    parsePreferredRanges,
    parsePreferredVersionsEntry,
    latestPreferredVersions,
  )
where

import Aihc.Hackage.Types (PackageSpec (..))
import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.Int (Int64)
import Data.List (isPrefixOf)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Types.Dependency (Dependency, depVerRange)
import Distribution.Types.Version (Version)
import Distribution.Types.VersionRange (VersionRange, intersectVersionRanges, withinRange)
import System.FilePath.Posix (splitDirectories)

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
      let next = collectEntry ranges entry
       in next `seq` collectEntries next rest
    collectEntries ranges Tar.Done = Right ranges
    collectEntries _ (Tar.Fail err) = Left (show err)

    collectEntry ranges entry =
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
