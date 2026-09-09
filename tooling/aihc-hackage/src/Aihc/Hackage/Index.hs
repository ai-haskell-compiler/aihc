-- | Parse a Hackage @01-index.tar.gz@ into package versions.
module Aihc.Hackage.Index
  ( parseHackageIndex,
    parseHackageIndexUpdatedSince,
  )
where

import Aihc.Hackage.Types (PackageSpec (..))
import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Types.Version (Version)
import System.FilePath.Posix (splitDirectories)

-- | Parse a compressed Hackage @01-index.tar.gz@ into latest package versions.
parseHackageIndex :: LBS.ByteString -> Either String [PackageSpec]
parseHackageIndex =
  parseHackageIndexWith (const True)

-- | Parse a compressed Hackage @01-index.tar.gz@ into latest package versions
-- whose latest package entry was uploaded at or after the given Unix timestamp.
parseHackageIndexUpdatedSince :: Int64 -> LBS.ByteString -> Either String [PackageSpec]
parseHackageIndexUpdatedSince cutoff =
  parseHackageIndexWith (\(_, uploadedAt) -> uploadedAt >= cutoff)

parseHackageIndexWith :: ((Version, Int64) -> Bool) -> LBS.ByteString -> Either String [PackageSpec]
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

    collectEntries packages (Tar.Next entry rest) =
      collectEntries (collectEntry packages entry) rest
    collectEntries packages Tar.Done = Right packages
    collectEntries _ (Tar.Fail err) = Left (show err)

packageVersionFromEntryPath :: FilePath -> Maybe (String, Version)
packageVersionFromEntryPath path =
  case splitDirectories path of
    [name, rawVersion, cabalFile]
      | cabalFile == name ++ ".cabal",
        Just version <- simpleParsec rawVersion ->
          Just (name, version)
    _ -> Nothing
