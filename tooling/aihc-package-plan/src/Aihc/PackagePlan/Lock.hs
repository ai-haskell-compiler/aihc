-- |
-- Module      : Aihc.PackagePlan.Lock
-- Description : The aihc.lock file
--
-- A lock file records a solved plan so that later builds reuse it instead
-- of solving, and so that the plan survives changes on Hackage. It is JSON
-- with the packages sorted by name, one package per line, so that a diff
-- touches only the packages that changed.
--
-- The file holds one assignment per target platform, because conditions on
-- @os@ and @arch@ can change a package's dependencies, and names the
-- emulated compiler release it was solved for: the core-library versions
-- differ between releases, so a lock written for another release is
-- ignored and rewritten.
module Aihc.PackagePlan.Lock
  ( LockFile (..),
    LockEntry (..),
    LockSource (..),
    lockFileName,
    lockFormatVersion,
    platformKey,
    readLockFile,
    writeLockFile,
    renderLockFile,
    parseLockFile,
    lockEntriesFromSolution,
    lockPreferences,
    lockRecorded,
  )
where

import Aihc.Hackage.Package (Arch, FlagAssignment, OS, PackageName, Version, archName, mkFlagAssignment, mkFlagName, mkPackageName, osName, parseVersionString, showVersion, unFlagAssignment, unFlagName, unPackageName)
import Aihc.PackagePlan.Json qualified as Json
import Aihc.PackagePlan.Solver (Assignment (..), CandidateSource (..), Preference (..), Solution)
import Control.Exception (bracket)
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import System.Directory (doesFileExist, removeFile, renameFile)
import System.FilePath (takeDirectory, takeFileName)
import System.IO (hClose, openBinaryTempFileWithDefaultPermissions)

-- | The file is called @aihc.lock@ wherever it lives.
lockFileName :: FilePath
lockFileName = "aihc.lock"

-- | Bumped on any change to the shape of the file.
lockFormatVersion :: Int
lockFormatVersion = 1

data LockSource = LockCore | LockHackage | LockLocal
  deriving (Eq, Ord, Show)

data LockEntry = LockEntry
  { lockName :: !PackageName,
    lockVersion :: !Version,
    lockSource :: !LockSource,
    -- | The cabal file revision a Hackage release was solved with.
    lockRevision :: !(Maybe Int),
    -- | Every flag the solver searched or a constraint fixed.
    lockFlags :: !FlagAssignment
  }
  deriving (Eq, Show)

data LockFile = LockFile
  { -- | The emulated GHC release, as @ghc-9.12.4@.
    lockCompiler :: !String,
    -- | When the Hackage index the plan was solved against was taken, as
    -- an ISO 8601 timestamp. Informational.
    lockIndexState :: !(Maybe String),
    -- | One assignment per platform key, see 'platformKey'.
    lockPlatforms :: !(Map String [LockEntry])
  }
  deriving (Eq, Show)

-- | The key of a platform in the file, as @linux-x86_64@.
platformKey :: OS -> Arch -> String
platformKey os arch = osName os <> "-" <> archName arch

-- | The lock beside a package, if there is one. A file that does not parse
-- is reported rather than ignored, since a hand edit that broke it should
-- not silently drop every pin.
readLockFile :: FilePath -> IO (Maybe (Either String LockFile))
readLockFile path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    -- Read strictly: the file is rewritten right after it is read when the
    -- plan changes, and a lazy read would still hold it open.
    else Just . parseLockFile . BL.fromStrict <$> BS.readFile path

-- | Write the lock to a temporary file in the same directory, then rename
-- it. A rename replaces the file in one step, so a process that reads the
-- lock gets the old file or the new file, and never a part of a file.
writeLockFile :: FilePath -> LockFile -> IO ()
writeLockFile path lock =
  bracket (openBinaryTempFileWithDefaultPermissions (takeDirectory path) (takeFileName path <> ".tmp")) (\(temporary, handle) -> hClose handle >> removeIfPresent temporary) $ \(temporary, handle) -> do
    BL.hPut handle (renderLockFile lock)
    hClose handle
    renameFile temporary path
  where
    removeIfPresent temporary = do
      exists <- doesFileExist temporary
      when exists (removeFile temporary)

-- | Render the file with one package per line, in name order.
renderLockFile :: LockFile -> BL.ByteString
renderLockFile lock =
  BLC.unlines
    ( [ BLC.pack "{",
        field "format" (Json.encode (Json.Number (toInteger lockFormatVersion))) <> BLC.pack ",",
        field "compiler" (jsonString (lockCompiler lock)) <> BLC.pack ",",
        field "index-state" (Json.encode (maybe Json.Null (Json.String . T.pack) (lockIndexState lock))) <> BLC.pack ",",
        BLC.pack "  \"platforms\": {"
      ]
        <> concat (separatedGroups (map platform (Map.toAscList (lockPlatforms lock))))
        <> [BLC.pack "  }", BLC.pack "}"]
    )
  where
    field :: String -> BL.ByteString -> BL.ByteString
    field name value = BLC.pack ("  " <> show name <> ": ") <> value
    platform (key, entries) =
      [BLC.pack ("    " <> show key <> ": [")]
        <> separated (map ((BLC.pack "      " <>) . renderEntry) (sortOn lockName entries))
        <> [BLC.pack "    ]"]
    separated [] = []
    separated [x] = [x]
    separated (x : rest) = (x <> BLC.pack ",") : separated rest
    -- A comma after the last line of every group but the last.
    separatedGroups [] = []
    separatedGroups [group] = [group]
    separatedGroups (group : rest) = (init group <> [last group <> BLC.pack ","]) : separatedGroups rest

renderEntry :: LockEntry -> BL.ByteString
renderEntry entry =
  BLC.pack "{"
    <> BLC.intercalate
      (BLC.pack ", ")
      ( [ pair "name" (jsonString (unPackageName (lockName entry))),
          pair "version" (jsonString (showVersion (lockVersion entry))),
          pair "source" (jsonString (sourceText (lockSource entry)))
        ]
          <> [pair "revision" (Json.encode (Json.Number (toInteger revision))) | Just revision <- [lockRevision entry]]
          <> [ pair "flags" (Json.encode (Json.Object (Map.fromList [(T.pack (unFlagName flag), Json.Bool value) | (flag, value) <- unFlagAssignment (lockFlags entry)])))
             | not (null (unFlagAssignment (lockFlags entry)))
             ]
      )
    <> BLC.pack "}"
  where
    pair :: String -> BL.ByteString -> BL.ByteString
    pair name value = BLC.pack (show name <> ": ") <> value

sourceText :: LockSource -> String
sourceText source =
  case source of
    LockCore -> "core"
    LockHackage -> "hackage"
    LockLocal -> "local"

parseLockFile :: BL.ByteString -> Either String LockFile
parseLockFile bytes = do
  value <- Json.decode bytes
  object <- asObject "lock file" value
  format <- required "format" asInt object
  if format /= lockFormatVersion
    then Left ("unsupported lock format " <> show format <> "; this compiler writes format " <> show lockFormatVersion)
    else
      LockFile
        <$> required "compiler" asString object
        <*> optional "index-state" asString object
        <*> (required "platforms" (asObject "platforms") object >>= fmap Map.fromList . mapM parsePlatform . Map.toList)
  where
    parsePlatform (key, value) = do
      entries <- asArray "platform" value >>= mapM parseEntry
      pure (T.unpack key, entries)
    parseEntry value = do
      object <- asObject "package" value
      name <- required "name" asString object
      versionText <- required "version" asString object
      version <- maybe (Left ("invalid version " <> versionText)) Right (parseVersionString versionText)
      sourceName <- required "source" asString object
      source <- case sourceName of
        "core" -> Right LockCore
        "hackage" -> Right LockHackage
        "local" -> Right LockLocal
        other -> Left ("unknown package source " <> other)
      revision <- optional "revision" asInt object
      flags <- optional "flags" (asObject "flags") object
      flagValues <- traverse (asBool "flag") (fromMaybe Map.empty flags)
      pure
        LockEntry
          { lockName = mkPackageName name,
            lockVersion = version,
            lockSource = source,
            lockRevision = revision,
            lockFlags = mkFlagAssignment [(mkFlagName (T.unpack flag), flagValue) | (flag, flagValue) <- Map.toList flagValues]
          }

-- | A JSON string, with quotes and escapes.
jsonString :: String -> BL.ByteString
jsonString = Builder.toLazyByteString . Json.encodeString

-- | A field that must be present.
required :: String -> (Json.Value -> Either String a) -> Map T.Text Json.Value -> Either String a
required name convert object =
  case Map.lookup (T.pack name) object of
    Just value -> either (Left . inField name) Right (convert value)
    Nothing -> Left ("key " <> show name <> " not found")

-- | A field that can be absent or null.
optional :: String -> (Json.Value -> Either String a) -> Map T.Text Json.Value -> Either String (Maybe a)
optional name convert object =
  case Map.lookup (T.pack name) object of
    Nothing -> Right Nothing
    Just Json.Null -> Right Nothing
    Just value -> either (Left . inField name) (Right . Just) (convert value)

inField :: String -> String -> String
inField name message = "in " <> show name <> ": " <> message

asObject :: String -> Json.Value -> Either String (Map T.Text Json.Value)
asObject _ (Json.Object object) = Right object
asObject what _ = Left ("expected an object for the " <> what)

asArray :: String -> Json.Value -> Either String [Json.Value]
asArray _ (Json.Array values) = Right values
asArray what _ = Left ("expected an array for the " <> what)

asString :: Json.Value -> Either String String
asString (Json.String text) = Right (T.unpack text)
asString _ = Left "expected a string"

asBool :: String -> Json.Value -> Either String Bool
asBool _ (Json.Bool value) = Right value
asBool what _ = Left ("expected a boolean for the " <> what)

asInt :: Json.Value -> Either String Int
asInt (Json.Number number)
  | number >= toInteger (minBound :: Int) && number <= toInteger (maxBound :: Int) = Right (fromInteger number)
asInt _ = Left "expected an integer"

-- | The entries of one platform from a solution.
lockEntriesFromSolution :: Solution -> [LockEntry]
lockEntriesFromSolution solution =
  [ LockEntry
      { lockName = name,
        lockVersion = assignmentVersion assignment,
        lockSource = case assignmentSource assignment of
          CandidateHackage -> LockHackage
          CandidateLocal _ -> LockLocal
          CandidateCore _ -> LockCore,
        lockRevision = case assignmentSource assignment of
          CandidateHackage -> Just (assignmentRevision assignment)
          _ -> Nothing,
        lockFlags = assignmentFlags assignment
      }
  | (name, assignment) <- Map.toAscList solution
  ]

-- | What the entries of one platform prefer, for a solve that keeps as
-- much of the lock as it can.
lockPreferences :: [LockEntry] -> Map PackageName Preference
lockPreferences entries =
  Map.fromList
    [ (lockName entry, Preference (lockVersion entry) (lockRevision entry) (lockFlags entry))
    | entry <- entries
    ]

-- | The entries of one platform in the form 'Aihc.PackagePlan.Solver.verifySolution' checks.
lockRecorded :: [LockEntry] -> Map PackageName (Version, Maybe Int, FlagAssignment)
lockRecorded entries =
  Map.fromList [(lockName entry, (lockVersion entry, lockRevision entry, lockFlags entry)) | entry <- entries]
