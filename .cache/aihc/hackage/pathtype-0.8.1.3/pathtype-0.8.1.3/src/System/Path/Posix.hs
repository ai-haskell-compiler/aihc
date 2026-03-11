{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-- | This module provides type-safe access to filepath manipulations.
--
--   Normally you would import 'System.Path' (which will use the
--   default implementation for the host platform) instead of this.
--   However, importing this explicitly allows for manipulation of
--   non-native paths.
--
module System.Path.Posix (
    Path,
    AbsFile, RelFile, AbsDir, RelDir,
    Abs, Rel, File, Dir,
    AbsRelFile, AbsRelDir, AbsFileDir, RelFileDir,
    AbsRel, FileDir, AbsRelFileDir,
    AbsPath, RelPath, FilePath, DirPath,
    AbsRelPath, FileDirPath,
    asPath,
    asRelFile, asRelDir, asAbsFile, asAbsDir,
    asRelPath, asAbsPath, asFilePath, asDirPath,
    path, maybe, maybePath, parse, parsePath,
    relFile, relDir, absFile, absDir,
    abs, rel, absRel, file, dir, fileDir,
    relPath, absPath, filePath, dirPath,
    rootDir, currentDir, emptyFile,
    toString,
    isAbsoluteString, isRelativeString, equalFilePath,
    pathSeparator, pathSeparators, isPathSeparator,
    Core.extSeparator, Core.isExtSeparator,
    Core.searchPathSeparator, Core.isSearchPathSeparator,
    addTrailingPathSeparator, dropTrailingPathSeparator,
    hasTrailingPathSeparator,
    testAll,
    ) where

import qualified System.Path.RegularExpression as RegEx
import qualified System.Path.Internal.PartClass as Class
import qualified System.Path.Internal as Core

import Data.Tagged (Tagged(Tagged), untag)

import qualified Test.DocTest.Driver as DocTest

import Prelude hiding (FilePath, maybe, abs)


data Posix = Posix

_osDummy :: Posix
_osDummy = Posix

type System = Posix

type Path = Core.Path System

type AbsFile = Core.AbsFile System
type RelFile = Core.RelFile System
type AbsDir  = Core.AbsDir  System
type RelDir  = Core.RelDir  System
type AbsRelFile = Core.AbsRelFile System
type AbsRelDir  = Core.AbsRelDir  System
type AbsFileDir = Core.AbsFileDir System
type RelFileDir = Core.RelFileDir System
type AbsRelFileDir = Core.AbsRelFileDir System

type Abs  fd = Core.Abs  System fd
type Rel  fd = Core.Rel  System fd
type File ar = Core.File System ar
type Dir  ar = Core.Dir  System ar
type AbsRel  fd = Core.AbsRel  System fd
type FileDir ar = Core.FileDir System ar

type AbsPath  fd = Core.AbsPath  System fd
type RelPath  fd = Core.RelPath  System fd
type FilePath ar = Core.FilePath System ar
type DirPath  ar = Core.DirPath  System ar
type AbsRelPath  fd = Core.AbsRelPath  System fd
type FileDirPath ar = Core.FileDirPath System ar

{-# DEPRECATED asPath "Use 'maybePath', 'parsePath' or 'path' instead." #-}
asPath :: (Class.AbsOrRel ar, Class.FileOrDir fd) => String -> Path ar fd
asPath = Core.asPath

{-# DEPRECATED asRelFile "Use 'relFile' instead." #-}
asRelFile :: String -> RelFile
asRelFile = Core.asRelFile

{-# DEPRECATED asRelDir "Use 'relDir' instead." #-}
asRelDir :: String -> RelDir
asRelDir = Core.asRelDir

{-# DEPRECATED asAbsFile "Use 'absFile' instead." #-}
asAbsFile :: String -> AbsFile
asAbsFile = Core.asAbsFile

{-# DEPRECATED asAbsDir "Use 'absDir' instead." #-}
asAbsDir :: String -> AbsDir
asAbsDir = Core.asAbsDir

{-# DEPRECATED asRelPath "Use 'relPath' instead." #-}
asRelPath :: (Class.FileOrDir fd) => String -> RelPath fd
asRelPath = Core.asRelPath

{-# DEPRECATED asAbsPath "Use 'absPath' instead." #-}
asAbsPath :: (Class.FileOrDir fd) => String -> AbsPath fd
asAbsPath = Core.asAbsPath

{-# DEPRECATED asFilePath "Use 'filePath' instead." #-}
asFilePath :: (Class.AbsOrRel ar) => String -> FilePath ar
asFilePath = Core.asFilePath

{-# DEPRECATED asDirPath "Use 'dirPath' instead." #-}
asDirPath :: (Class.AbsOrRel ar) => String -> DirPath ar
asDirPath = Core.asDirPath


{-# DEPRECATED maybePath "Use Path.maybe instead." #-}
{-# DEPRECATED parsePath "Use Path.parse instead." #-}

maybe, maybePath ::
    (Class.AbsRel ar, Class.FileDir fd) => String -> Maybe (Path ar fd)
maybe = Core.maybe
maybePath = Core.maybe

parse, parsePath ::
    (Class.AbsRel ar, Class.FileDir fd) => String -> Either String (Path ar fd)
parse = Core.parse
parsePath = Core.parse


path :: (Class.AbsRel ar, Class.FileDir fd) => String -> Path ar fd
path = Core.path

relFile :: String -> RelFile
relFile = Core.relFile

relDir :: String -> RelDir
relDir = Core.relDir

absFile :: String -> AbsFile
absFile = Core.absFile

absDir :: String -> AbsDir
absDir = Core.absDir


rel :: (Class.FileDir fd) => String -> Rel fd
rel = Core.rel

abs :: (Class.FileDir fd) => String -> Abs fd
abs = Core.abs

absRel :: (Class.FileDir fd) => String -> AbsRel fd
absRel = Core.absRel

file :: (Class.AbsRel ar) => String -> File ar
file = Core.file

dir :: (Class.AbsRel ar) => String -> Dir ar
dir = Core.dir

fileDir :: (Class.AbsRel ar) => String -> FileDir ar
fileDir = Core.fileDir


relPath :: (Class.FileDir fd) => String -> RelPath fd
relPath = Core.relPath

absPath :: (Class.FileDir fd) => String -> AbsPath fd
absPath = Core.absPath

filePath :: (Class.AbsRel ar) => String -> FilePath ar
filePath = Core.filePath

dirPath :: (Class.AbsRel ar) => String -> DirPath ar
dirPath = Core.dirPath


rootDir :: AbsDir
rootDir = Core.rootDir

currentDir :: RelDir
currentDir = Core.currentDir

emptyFile :: RelFile
emptyFile = Core.emptyFile


toString :: (Class.AbsRel ar, Class.FileDir fd) => Path ar fd -> String
toString = Core.toString


instance Core.System Posix where
   pathSeparator = Tagged pathSeparator
   splitAbsolute = Tagged $ RegEx.run $ RegEx.single isPathSeparator
   canonicalize = Tagged id
   splitDrive = Tagged $ return ""
   genDrive = Tagged $ return ""

withOS :: Tagged System a -> a
withOS = untag


{-# DEPRECATED equalFilePath "Use System.FilePath.equalFilePath instead." #-}
{-# DEPRECATED isAbsoluteString "Use System.FilePath.isAbsolute instead." #-}
{-# DEPRECATED isRelativeString "Use System.FilePath.isRelative instead." #-}

equalFilePath :: String -> String -> Bool
equalFilePath = withOS Core.equalFilePath

isAbsoluteString :: String -> Bool
isAbsoluteString = withOS Core.isAbsoluteString

isRelativeString :: String -> Bool
isRelativeString = withOS Core.isRelativeString


pathSeparator :: Char
pathSeparator = '/'

pathSeparators :: [Char]
pathSeparators = withOS Core.pathSeparators

isPathSeparator :: Char -> Bool
isPathSeparator = withOS Core.isPathSeparator


{-# DEPRECATED addTrailingPathSeparator "Use System.FilePath.addTrailingPathSeparator instead." #-}
{-# DEPRECATED dropTrailingPathSeparator "Use System.FilePath.dropTrailingPathSeparator instead." #-}
{-# DEPRECATED hasTrailingPathSeparator "Use System.FilePath.hasTrailingPathSeparator instead." #-}

-- | This is largely for 'System.FilePath' compatibility
addTrailingPathSeparator :: String -> String
addTrailingPathSeparator = (++[pathSeparator])

-- | This is largely for 'System.FilePath' compatibility
dropTrailingPathSeparator :: String -> String
dropTrailingPathSeparator = init

-- | This is largely for 'System.FilePath' compatibility
hasTrailingPathSeparator :: String -> Bool
hasTrailingPathSeparator = isPathSeparator . last


testAll :: [(String, DocTest.T ())]
testAll = Core.testAll Posix
