-- |
-- Module      : Aihc.PackagePlan.Lock
-- Description : The aihc.lock file
--
-- A lock file records a solved plan so that later builds reuse it instead
-- of solving, and so that the plan survives changes on Hackage. It is a
-- line-based text file with the packages sorted by name, one package per
-- line, so that a diff touches only the packages that changed:
--
-- > format 2
-- > compiler ghc-9.12.4
-- > index-state 2026-09-17T00:00:00Z
-- >
-- > platform linux-x86_64
-- >   base 4.21.0.0 core
-- >   unix 2.8.8.0 hackage revision=1 +os-string
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
import Aihc.PackagePlan.Solver (Assignment (..), CandidateSource (..), Preference (..), Solution)
import Control.Exception (bracket)
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Char (isSpace)
import Data.List (sortOn, stripPrefix)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import System.Directory (doesFileExist, removeFile, renameFile)
import System.FilePath (takeDirectory, takeFileName)
import System.IO (hClose, openBinaryTempFileWithDefaultPermissions)
import Text.Read (readMaybe)

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
  BLC.pack . unlines $
    [ "format " <> show lockFormatVersion,
      "compiler " <> lockCompiler lock
    ]
      <> ["index-state " <> state | Just state <- [lockIndexState lock]]
      <> concatMap platform (Map.toAscList (lockPlatforms lock))
  where
    platform (key, entries) =
      ["", "platform " <> key] <> map (("  " <>) . renderEntry) (sortOn lockName entries)

-- | One package as words: the name, the version, the source, the
-- revision of a Hackage release, and each flag as @+flag@ or @-flag@.
renderEntry :: LockEntry -> String
renderEntry entry =
  unwords $
    [ unPackageName (lockName entry),
      showVersion (lockVersion entry),
      sourceText (lockSource entry)
    ]
      <> ["revision=" <> show revision | Just revision <- [lockRevision entry]]
      <> [(if value then "+" else "-") <> unFlagName flag | (flag, value) <- sortOn fst (unFlagAssignment (lockFlags entry))]

sourceText :: LockSource -> String
sourceText source =
  case source of
    LockCore -> "core"
    LockHackage -> "hackage"
    LockLocal -> "local"

-- | Read the file. Empty lines and lines that start with @--@ are ignored.
parseLockFile :: BL.ByteString -> Either String LockFile
parseLockFile bytes =
  case [(number, words line) | (number, line) <- zip [1 :: Int ..] (lines (BLC.unpack bytes)), significant line] of
    (_, ["format", formatText]) : rest
      | readMaybe formatText == Just lockFormatVersion -> parseHeader rest
      | otherwise -> unsupported formatText
    _ -> unsupported "1"
  where
    significant line =
      let content = dropWhile isSpace line
       in not (null content) && take 2 content /= "--"
    unsupported found =
      Left ("unsupported lock format " <> found <> "; this compiler writes format " <> show lockFormatVersion <> ". Remove the file to write it again")
    parseHeader ((_, ["compiler", compiler]) : rest) = do
      let (state, rest') =
            case rest of
              (_, ["index-state", value]) : after -> (Just value, after)
              _ -> (Nothing, rest)
      platforms <- parsePlatforms Map.empty rest'
      pure LockFile {lockCompiler = compiler, lockIndexState = state, lockPlatforms = platforms}
    parseHeader rest = Left (atLine rest "expected \"compiler NAME\"")
    parsePlatforms platforms [] = Right platforms
    parsePlatforms platforms ((_, ["platform", key]) : rest) = do
      let (entryLines, rest') = break (isPlatform . snd) rest
      entries <- mapM parseEntry entryLines
      parsePlatforms (Map.insert key entries platforms) rest'
    parsePlatforms _ rest = Left (atLine rest "expected \"platform KEY\"")
    isPlatform (keyword : _) = keyword == "platform"
    isPlatform [] = False
    parseEntry (number, nameText : versionText : sourceName : options) = do
      let failure message = Left ("line " <> show number <> ": " <> message)
      version <- maybe (failure ("invalid version " <> versionText)) Right (parseVersionString versionText)
      source <- case sourceName of
        "core" -> Right LockCore
        "hackage" -> Right LockHackage
        "local" -> Right LockLocal
        other -> failure ("unknown package source " <> other)
      (revision, flags) <- foldr (parseOption failure) (Right (Nothing, [])) options
      pure
        LockEntry
          { lockName = mkPackageName nameText,
            lockVersion = version,
            lockSource = source,
            lockRevision = revision,
            lockFlags = mkFlagAssignment flags
          }
    parseEntry (number, _) = Left ("line " <> show number <> ": expected \"NAME VERSION SOURCE\"")
    parseOption failure option result = do
      (revision, flags) <- result
      case option of
        '+' : flag | not (null flag) -> Right (revision, (mkFlagName flag, True) : flags)
        '-' : flag | not (null flag) -> Right (revision, (mkFlagName flag, False) : flags)
        _
          | Just number <- readMaybe =<< stripPrefix "revision=" option -> Right (Just number, flags)
        _ -> failure ("invalid package option " <> option)
    atLine rest message =
      case rest of
        (number, _) : _ -> "line " <> show number <> ": " <> message
        [] -> "unexpected end of file: " <> message

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
