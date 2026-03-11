-- | This module provides type-safe access to directory manipulations.
--
--   It is designed to be imported instead of "System.Directory".
--   (It is intended to provide versions of functions from that
--   module which have equivalent functionality but are more
--   typesafe). "System.Path" is a companion module providing
--   a type-safe alternative to "System.FilePath".
--
--   You will typically want to import as follows:
--
--   > import qualified System.Path.IO as PathIO
--   > import qualified System.Path as Path
--   > import System.Path.Directory (createDirectory)
module System.Path.Directory
(
  -- * Actions on directories
  createDirectory,
  createDirectoryIfMissing,
  removeDirectory,
  removeDirectoryRecursive,
  renameDirectory,

  getDirectoryContents,
  absDirectoryContents,
  relDirectoryContents,
  filesInDir,
  dirsInDir,

  getCurrentDirectory,
  setCurrentDirectory,

  -- * Pre-defined directories
  getHomeDirectory,
  getAppUserDataDirectory,
  getUserDocumentsDirectory,
  getTemporaryDirectory,

  -- * Actions on files
  removeFile,
  renameFile,
  copyFile,
  canonicalizePath,
  makeRelativeToCurrentDirectory,
  findExecutable,

  -- * Existence tests
  doesFileExist,
  doesDirectoryExist,

  -- * Permissions
  Permissions,
  getPermissions,
  setPermissions,

  -- * Timestamps
  getModificationTime,
)

where

import qualified System.Path.Internal.PartClass as Class
import qualified System.Path as Path
import System.Path (
    Path, path,
    AbsPath, AbsDir, AbsFile, RelPath, RelDir, RelFile,
    DirPath, FilePath, absDir, (</>),
    )

import System.Path.ModificationTime (convertTime)
import Data.Time (UTCTime)

import qualified System.Directory as SD
import System.Directory (Permissions)

import Control.Applicative ((<$>))

import Data.List (partition)
import Data.Tuple.HT (mapPair)

import Prelude hiding (FilePath)


------------------------------------------------------------------------
-- Actions on directories

createDirectory :: Class.AbsRel ar => DirPath ar -> IO ()
createDirectory = SD.createDirectory . Path.toString

createDirectoryIfMissing :: Class.AbsRel ar => Bool -> DirPath ar -> IO ()
createDirectoryIfMissing flag = SD.createDirectoryIfMissing flag . Path.toString

removeDirectory :: Class.AbsRel ar => DirPath ar -> IO ()
removeDirectory = SD.removeDirectory . Path.toString

removeDirectoryRecursive :: Class.AbsRel ar => DirPath ar -> IO ()
removeDirectoryRecursive = SD.removeDirectoryRecursive . Path.toString

renameDirectory ::
  (Class.AbsRel ar1, Class.AbsRel ar2) => DirPath ar1 -> DirPath ar2 -> IO ()
renameDirectory p1 p2 =
  SD.renameDirectory (Path.toString p1) (Path.toString p2)

-- | Retrieve the contents of a directory without any directory prefixes.
-- In contrast to 'System.Directory.getDirectoryContents',
-- exclude special directories \".\" and \"..\".
getDirectoryContents :: Class.AbsRel ar => DirPath ar -> IO [Path.RelFileDir]
getDirectoryContents dir =
    map Path.path <$> plainDirectoryContents dir

-- | Retrieve the contents of a directory path (which may be relative) as absolute paths
absDirectoryContents ::
  Class.AbsRel ar => DirPath ar -> IO ([AbsDir], [AbsFile])
absDirectoryContents p = do
  cd <- absDir <$> SD.getCurrentDirectory
  let dir = Path.withAbsRel id (cd </>) p
  mapPair (map (dir </>), map (dir </>)) <$> relDirectoryContents dir

-- | Returns paths relative /to/ the supplied (abs or relative) directory path.
--   eg (for current working directory of @\/somewhere\/cwd\/@):
--
-- > show (relDirectoryContents "d/e/f/") == (["subDir1A","subDir1B"],
-- >                                                      ["file1A","file1B"])
--
relDirectoryContents ::
  Class.AbsRel ar => DirPath ar -> IO ([RelDir], [RelFile])
relDirectoryContents dir = do
  filenames <- plainDirectoryContents dir
  mapPair (map (Path.relDir . fst), map (Path.relFile . fst)) .
    partition snd . zip filenames
      <$> mapM (doesDirectoryExist . (dir </>) . Path.relPath) filenames

plainDirectoryContents :: Class.AbsRel ar => DirPath ar -> IO [String]
plainDirectoryContents dir =
    filter (not . flip elem [".",".."]) <$>
    SD.getDirectoryContents (Path.toString dir)

-- | A convenient alternative to 'relDirectoryContents' if you only want files.
filesInDir :: Class.AbsRel ar => DirPath ar -> IO [RelFile]
filesInDir dir = snd <$> relDirectoryContents dir

-- | A convenient alternative to 'relDirectoryContents' if you only want directories.
dirsInDir :: Class.AbsRel ar => DirPath ar -> IO [RelDir]
dirsInDir dir = fst <$> relDirectoryContents dir


getCurrentDirectory :: IO AbsDir
getCurrentDirectory = absDir <$> SD.getCurrentDirectory

setCurrentDirectory :: Class.AbsRel ar => DirPath ar -> IO ()
setCurrentDirectory = SD.setCurrentDirectory . Path.toString


------------------------------------------------------------------------
-- Pre-defined directories

getHomeDirectory :: IO AbsDir
getHomeDirectory = absDir <$> SD.getHomeDirectory

getAppUserDataDirectory :: String -> IO AbsDir
getAppUserDataDirectory user = absDir <$> SD.getAppUserDataDirectory user

getUserDocumentsDirectory :: IO AbsDir
getUserDocumentsDirectory = absDir <$> SD.getUserDocumentsDirectory

getTemporaryDirectory :: IO AbsDir
getTemporaryDirectory = absDir <$> SD.getTemporaryDirectory


------------------------------------------------------------------------
-- Actions on files

removeFile :: Class.AbsRel ar => FilePath ar -> IO ()
removeFile = SD.removeFile . Path.toString

renameFile ::
    (Class.AbsRel ar1, Class.AbsRel ar2) =>
    FilePath ar1 -> FilePath ar2 -> IO ()
renameFile p1 p2 = SD.renameFile (Path.toString p1) (Path.toString p2)

copyFile ::
    (Class.AbsRel ar1, Class.AbsRel ar2) =>
    FilePath ar1 -> FilePath ar2 -> IO ()
copyFile p1 p2 = SD.copyFile (Path.toString p1) (Path.toString p2)

canonicalizePath ::
    (Class.AbsRel ar, Class.FileDir fd) => Path ar fd -> IO (AbsPath fd)
canonicalizePath p = path <$> SD.canonicalizePath (Path.toString p)

makeRelativeToCurrentDirectory ::
    (Class.AbsRel ar, Class.FileDir fd) => Path ar fd -> IO (RelPath fd)
makeRelativeToCurrentDirectory p =
    path <$> SD.makeRelativeToCurrentDirectory (Path.toString p)

findExecutable :: String -> IO (Maybe AbsFile)
findExecutable s = fmap path <$> SD.findExecutable s


------------------------------------------------------------------------
-- Existence tests

doesFileExist :: Class.AbsRel ar => FilePath ar -> IO Bool
doesFileExist = SD.doesFileExist . Path.toString

doesDirectoryExist :: Class.AbsRel ar => DirPath ar -> IO Bool
doesDirectoryExist = SD.doesDirectoryExist . Path.toString


------------------------------------------------------------------------
-- Permissions

getPermissions ::
    (Class.AbsRel ar, Class.FileDir fd) => Path ar fd -> IO Permissions
getPermissions p = SD.getPermissions (Path.toString p)

setPermissions ::
    (Class.AbsRel ar, Class.FileDir fd) => Path ar fd -> Permissions -> IO ()
setPermissions p perms = SD.setPermissions (Path.toString p) perms


------------------------------------------------------------------------
-- Timestamps

getModificationTime ::
    (Class.AbsRel ar, Class.FileDir fd) => Path ar fd -> IO UTCTime
getModificationTime p = convertTime <$> SD.getModificationTime (Path.toString p)
