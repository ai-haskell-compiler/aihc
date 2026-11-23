-- |
-- Module      : Aihc.PackagePlan.Lock
-- Description : The aihc.lock file
--
-- A lock file records a solved plan so that later builds reuse it instead
-- of solving, and so that the plan survives changes on Hackage. It is TOML
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
    LockProblem (..),
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
import Aihc.PackagePlan.Solver (Assignment (..), CandidateSource (..), Preference (..), Solution)
import Aihc.PackagePlan.Toml (Value (..))
import Aihc.PackagePlan.Toml qualified as Toml
import Control.Exception (bracket)
import Control.Monad (unless, when, (>=>))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Char (isSpace)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.Directory (doesFileExist, removeFile, renameFile)
import System.FilePath (takeDirectory, takeFileName)
import System.IO (hClose, openBinaryTempFileWithDefaultPermissions)

-- | The file is called @aihc.lock@ wherever it lives.
lockFileName :: FilePath
lockFileName = "aihc.lock"

-- | Bumped on any change to the shape of the file.
lockFormatVersion :: Int
lockFormatVersion = 2

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

-- | Why a lock file gives no plan.
data LockProblem
  = -- | The file does not parse. This is reported rather than ignored,
    -- since a hand edit that broke it should not silently drop every pin.
    LockInvalid String
  | -- | The file is JSON, the format 1 of an earlier aihc. It is ignored and
    -- written again, as a lock for another compiler release is.
    LockOldFormat
  deriving (Eq, Show)

-- | The lock beside a package, if there is one.
readLockFile :: FilePath -> IO (Maybe (Either LockProblem LockFile))
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

-- | Render the file with one package per line, in name order. Each line of
-- an array ends with a comma, so a new last package changes one line only.
renderLockFile :: LockFile -> BL.ByteString
renderLockFile lock =
  BL.fromStrict . TE.encodeUtf8 . T.unlines $
    [ "format = " <> Toml.renderValue (Integer (toInteger lockFormatVersion)),
      "compiler = " <> Toml.renderValue (string (lockCompiler lock))
    ]
      <> ["index-state = " <> Toml.renderValue (string indexState) | Just indexState <- [lockIndexState lock]]
      <> ["", "[platforms]"]
      <> concatMap platform (Map.toAscList (lockPlatforms lock))
  where
    platform (key, entries) =
      [Toml.renderKey (T.pack key) <> " = ["]
        <> map (\entry -> "  " <> renderEntry entry <> ",") (sortOn lockName entries)
        <> ["]"]

-- | One package as an inline table, with its fields in a fixed order.
renderEntry :: LockEntry -> Text
renderEntry entry =
  Toml.renderInlineTable $
    [ ("name", string (unPackageName (lockName entry))),
      ("version", string (showVersion (lockVersion entry))),
      ("source", string (sourceText (lockSource entry)))
    ]
      <> [("revision", Integer (toInteger revision)) | Just revision <- [lockRevision entry]]
      <> [("flags", Table (Map.fromList [(T.pack (unFlagName flag), Bool on) | (flag, on) <- flags])) | not (null flags)]
  where
    flags = unFlagAssignment (lockFlags entry)

string :: String -> Value
string = String . T.pack

sourceText :: LockSource -> String
sourceText source =
  case source of
    LockCore -> "core"
    LockHackage -> "hackage"
    LockLocal -> "local"

parseLockFile :: BL.ByteString -> Either LockProblem LockFile
parseLockFile bytes
  | BL.take 1 (BL.dropWhile (isSpace . toEnum . fromIntegral) bytes) == "{" = Left LockOldFormat
  | otherwise = either (Left . LockInvalid) Right $ do
      text <- either (Left . show) Right (TE.decodeUtf8' (BL.toStrict bytes))
      root <- Toml.parseToml lockFileName text
      format <- required "format" asInt root
      unless (format == lockFormatVersion) $
        Left ("unsupported lock format " <> show format <> "; this compiler writes format " <> show lockFormatVersion)
      LockFile
        <$> required "compiler" asString root
        <*> optional "index-state" asString root
        <*> (required "platforms" asTable root >>= traverse (asArray >=> traverse lockEntry) . Map.mapKeys T.unpack)
  where
    lockEntry content = do
      table <- asTable content
      name <- required "name" asString table
      versionText <- required "version" asString table
      version <- maybe (Left ("invalid version " <> versionText)) Right (parseVersionString versionText)
      source <- required "source" asString table >>= lockSourceFromText
      revision <- optional "revision" asInt table
      flags <- maybe (Right Map.empty) (traverse asBool) =<< optional "flags" asTable table
      pure
        LockEntry
          { lockName = mkPackageName name,
            lockVersion = version,
            lockSource = source,
            lockRevision = revision,
            lockFlags = mkFlagAssignment [(mkFlagName (T.unpack flag), on) | (flag, on) <- Map.toList flags]
          }
    lockSourceFromText sourceName =
      case sourceName of
        "core" -> Right LockCore
        "hackage" -> Right LockHackage
        "local" -> Right LockLocal
        other -> Left ("unknown package source " <> other)
    required name convert table =
      maybe (Left ("the key " <> T.unpack name <> " is missing")) (inKey name convert) (Map.lookup name table)
    optional name convert table =
      traverse (inKey name convert) (Map.lookup name table)
    inKey name convert content =
      either (\problem -> Left (problem <> " at the key " <> T.unpack name)) Right (convert content)
    asString content = case content of String text -> Right (T.unpack text); _ -> Left "expected a string"
    asInt content = case content of Integer number -> Right (fromInteger number); _ -> Left "expected an integer"
    asBool content = case content of Bool on -> Right on; _ -> Left "expected a boolean"
    asTable content = case content of Table table -> Right table; _ -> Left "expected a table"
    asArray content = case content of Array items -> Right items; _ -> Left "expected an array"

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
