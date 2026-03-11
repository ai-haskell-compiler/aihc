module System.Path.Internal
(
  -- * The main filepath (& dirpath) abstract type
  Path, -- kept abstract

  -- * Type Synonyms
  AbsFile,
  RelFile,
  AbsDir,
  RelDir,
  AbsRelFile,
  AbsRelDir,
  AbsFileDir,
  RelFileDir,
  AbsRelFileDir,

  AbsPath,     Abs,
  RelPath,     Rel,
  FilePath,    File,
  DirPath,     Dir,
  AbsRelPath,  AbsRel,
  FileDirPath, FileDir,

  -- * Decisions on path types
  withAbsRel, withFileDir,

  -- * Path to String conversion
  toString,
  getPathString,

  -- * Constants
  rootDir,
  currentDir,
  emptyFile,

  -- * Parsing Functions
  maybePath, maybe,
  parsePath, parse,

  -- * Checked Construction Functions
  path,
  relFile,
  relDir,
  absFile,
  absDir,
  relPath,    rel,
  absPath,    abs,
  filePath,   file,
  dirPath,    dir,
  absRel,     fileDir,

  idAbsRel, idAbs, idRel,
  idFileDir, idFile, idDir,

  -- * Unchecked Construction Functions
  asPath,
  asRelFile,
  asRelDir,
  asAbsFile,
  asAbsDir,
  asRelPath,
  asAbsPath,
  asFilePath,
  asDirPath,

  -- * Checked Construction Functions
  mkPathAbsOrRel,
  mkPathFileOrDir,
  mkAbsPath,
  mkAbsPathFromCwd,

  -- * Basic Manipulation Functions
  (</>),
  (<.>),
  (<++>),
  addExtension,
  combine,
  dropExtension,
  dropExtensions,
  dropFileName,
  replaceExtension,
  replaceBaseName,
  replaceDirectory,
  replaceFileName,
  splitExtension,
  splitExtensions,
  splitFileName,
  splitDirName,
  takeBaseName,
  takeDirectory,
  takeSuperDirectory,
  takeExtension,
  takeExtensions,
  takeFileName,
  takeDirName,
  mapFileName,
  mapFileNameF,

  -- * Auxillary Manipulation Functions
  equalFilePath,
  joinPath,
  normalise,
  splitPath,
  makeRelative,
  makeRelativeMaybe,
  makeAbsolute,
  makeAbsoluteFromCwd,
  dynamicMakeAbsolute,
  dynamicMakeAbsoluteFromCwd,
  genericMakeAbsolute,
  genericMakeAbsoluteFromCwd,
  pathMap,
  dirFromFile,
  fileFromDir,
  toFileDir,
  fromFileDir,
  fileFromFileDir,
  dirFromFileDir,
  toAbsRel,
  fromAbsRel,

  -- * Path Predicates
  isAbsolute,
  isRelative,
  isAbsoluteString,
  isRelativeString,
  hasAnExtension,
  hasExtension,

  -- * Separators
  System(..),
  extSeparator,
  searchPathSeparator,
  isExtSeparator,
  isSearchPathSeparator,

  -- * Generic Manipulation Functions
  genericAddExtension,
  genericDropExtension,
  genericDropExtensions,
  genericSplitExtension,
  genericSplitExtensions,
  genericTakeExtension,
  genericTakeExtensions,

  -- * Tests
  testAll,
  isValid,
)

where

import qualified System.Path.Internal.PartClass as Class
import qualified System.Path.Internal.Part as Part
import qualified System.Path.Internal.Component as PC
import qualified System.Path.Internal.Separator as Sep
import System.Path.Internal.PartClass as Class
        (WrapFileDir(WrapFileDir), WrapAbsRel(WrapAbsRel), FuncArg(..), fdMap)
import System.Path.Internal.Part (absPC)
import System.Path.Internal.System (System(..))
import System.Path.Internal.Component
        (Component(Component), GenComponent)

import qualified System.Directory as SD

import qualified Control.Monad.Trans.Class as MT
import qualified Control.Monad.Trans.State as MS
import Control.Monad (MonadPlus, guard, liftM2, mplus, mzero)
import Control.Applicative (Const(Const), liftA2, (<$>), (<$))
import Control.DeepSeq (NFData(rnf))

import qualified Data.Monoid.HT as MonHT
import qualified Data.List.HT as ListHT
import Data.Tagged (Tagged(Tagged), untag)
import Data.Functor.Compose (Compose(Compose), getCompose)
import Data.List (isSuffixOf, stripPrefix, intersperse)
import Data.String (IsString(fromString))
import Data.Maybe.HT (toMaybe)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Tuple.HT (mapFst, mapSnd)
import Data.Monoid (Monoid(mempty, mappend, mconcat), Endo(Endo), appEndo)
import Data.Semigroup (Semigroup(sconcat, (<>)), )
import Data.Char (isSpace)
import Data.Ord.HT (comparing)
import Data.Eq.HT (equating)

import Text.Show.HT (concatS)
import Text.Printf (printf)

import qualified Test.DocTest.Driver as DocTest
import qualified Test.QuickCheck as QC
import Test.QuickCheck
          (Gen, Property, property, Arbitrary(arbitrary), frequency)

import qualified Prelude as P
import Prelude hiding (FilePath, maybe, abs)


{- $setup
>>> :set -fno-warn-warnings-deprecations
>>> import qualified System.Path.PartClass as Class
>>> import qualified System.Path.Generic as Path
>>> import qualified System.Path.Posix as Posix
>>> import qualified System.Path.Windows as Windows
>>> import System.Path.Generic ((</>), (<.>), relFile, relDir, absFile, absDir)
>>> import Data.List (isSuffixOf, isPrefixOf)
>>> import Data.Char (toLower)
>>> import qualified Test.QuickCheck as QC
>>> forAllAbsRel :: (Class.FileDir fd, QC.Testable prop) => (Default.AbsRel fd -> prop) -> QC.Property
>>> forAllAbsRel = QC.forAll QC.arbitrary
-}


------------------------------------------------------------------------
-- Types

-- | This is the main filepath abstract datatype
data Path os ar fd = Path ar [Component os] fd

instance
    (System os, Class.AbsRel ar, Class.FileDir fd) =>
        Eq (Path os ar fd) where
    (==)  =  equating inspectPath

instance
    (System os, Class.AbsRel ar, Class.FileDir fd) =>
        Ord (Path os ar fd) where
    compare  =  comparing inspectPath

inspectPath ::
    Path os ar fd -> (WrapAbsRel os ar, [Component os], WrapFileDir os fd)
inspectPath (Path ar pcs fd) = (WrapAbsRel ar, pcs, WrapFileDir fd)


selTag :: Path os ar fd -> Tagged os a -> a
selTag _ = untag


type AbsFile os = Path os Part.Abs Part.File
type RelFile os = Path os Part.Rel Part.File
type AbsDir  os = Path os Part.Abs Part.Dir
type RelDir  os = Path os Part.Rel Part.Dir
type AbsRelFile os = Path os Part.AbsRel Part.File
type AbsRelDir  os = Path os Part.AbsRel Part.Dir
type AbsFileDir os = Path os Part.Abs Part.FileDir
type RelFileDir os = Path os Part.Rel Part.FileDir
type AbsRelFileDir os = Path os Part.AbsRel Part.FileDir

type Abs  os fd = Path os Part.Abs fd
type Rel  os fd = Path os Part.Rel fd
type File os ar = Path os ar Part.File
type Dir  os ar = Path os ar Part.Dir
type AbsRel  os fd = Path os Part.AbsRel fd
type FileDir os ar = Path os ar Part.FileDir

{-# DEPRECATED RelPath     "Use Path.Rel instead." #-}
{-# DEPRECATED AbsPath     "Use Path.Abs instead." #-}
{-# DEPRECATED AbsRelPath  "Use Path.AbsRel instead." #-}
{-# DEPRECATED FilePath    "Use Path.File instead." #-}
{-# DEPRECATED DirPath     "Use Path.Dir instead." #-}
{-# DEPRECATED FileDirPath "Use Path.FileDir instead." #-}

type AbsPath  os fd = Path os Part.Abs fd
type RelPath  os fd = Path os Part.Rel fd
type FilePath os ar = Path os ar Part.File
type DirPath  os ar = Path os ar Part.Dir
type AbsRelPath  os fd = Path os Part.AbsRel fd
type FileDirPath os ar = Path os ar Part.FileDir

instance (Class.AbsRel ar, Class.FileDir fd) => NFData (Path os ar fd) where
    rnf (Path ar pcs fd) =
        rnf (Class.withAbsRel rnf () ar, pcs, Class.withFileDir rnf () () fd)

-- I don't think this basic type of fold is appropriate for a nested datatype
-- pathFold :: a -> (a -> String -> a) -> Path ar fd -> a
-- pathFold pr f PathRoot = pr
-- pathFold pr f (FileDir d pc) = f (pathFold pr f d) (unPathComponent pc)

-- | Map over the components of the path.
--
-- prop> Path.pathMap (map toLower) (absDir "/tmp/Reports/SpreadSheets") == Posix.absDir "/tmp/reports/spreadsheets"
pathMap ::
    (Class.FileDir fd) => (String -> String) -> Path os ar fd -> Path os ar fd
pathMap f (Path ar pcs fd) = Path ar (map (PC.map f) pcs) (fdMap f fd)


mapFilePart ::
    (GenComponent -> GenComponent) -> FilePath os ar -> FilePath os ar
mapFilePart f (Path ar pcs (Part.File fd)) = Path ar pcs $ Part.File $ f fd

mapFilePartF ::
    (Functor f) =>
    (GenComponent -> f GenComponent) -> FilePath os ar -> f (FilePath os ar)
mapFilePartF f (Path ar pcs (Part.File fd)) =
    Path ar pcs <$> Part.File <$> f fd

splitFilePart ::
    (GenComponent -> (GenComponent, a)) -> FilePath os ar -> (FilePath os ar, a)
splitFilePart f (Path ar pcs (Part.File fd)) = mapFst (Path ar pcs . Part.File) $ f fd

mapPathDirs ::
    ([Component os] -> [Component os]) -> Path os ar fd -> Path os ar fd
mapPathDirs f ~(Path ar pcs fd) = Path ar (f pcs) fd


withAbsRel ::
    (Class.AbsRel ar) =>
    (AbsPath os fd -> a) -> (RelPath os fd -> a) -> Path os ar fd -> a
withAbsRel fAbs fRel (Path ar pcs fd) =
    Class.withAbsRel
        (\drive -> fAbs $ Path (Part.Abs (Component drive)) pcs fd)
        (fRel $ Path Part.Rel pcs fd)
        ar

switchFileDir ::
    (Class.FileDir fd) =>
    f (FilePath os ar) -> f (DirPath os ar) -> f (FileDirPath os ar) ->
    f (Path os ar fd)
switchFileDir f d fd =
    getCompose $ Class.switchFileDir (Compose f) (Compose d) (Compose fd)

switchFileOrDir ::
    (Class.FileOrDir fd) =>
    f (FilePath os ar) -> f (DirPath os ar) -> f (Path os ar fd)
switchFileOrDir f d =
    getCompose $ Class.switchFileOrDir (Compose f) (Compose d)

withFileDir ::
    (Class.FileOrDir fd) =>
    (FilePath os ar -> a) -> (DirPath os ar -> a) -> Path os ar fd -> a
withFileDir f g = runFuncArg $ switchFileOrDir (FuncArg f) (FuncArg g)


-- | Currently not exported
eitherFromAbsRel ::
    Class.AbsRel ar => Path os ar fd -> Either (AbsPath os fd) (RelPath os fd)
eitherFromAbsRel = withAbsRel Left Right

-- | Currently not exported
_eitherFromFileDir ::
    Class.FileOrDir fd => Path os ar fd -> Either (FilePath os ar) (DirPath os ar)
_eitherFromFileDir = withFileDir Left Right

------------------------------------------------------------------------
-- Read & Show instances

{- |
We show and parse file path components
using the rather generic 'relPath' smart constructor
instead of 'relFile', 'relDir' and @relPath str :: FileDirPath ar@.
Otherwise handling of all cases of 'Part.File', 'Part.Dir' and 'Part.FileDir' types
becomes pretty complicated.

>>> Posix.rootDir </> relDir "bla" </> relFile "blub"
rootDir </> relPath "bla" </> relPath "blub"
>>> Just (Posix.rootDir </> relDir "bla" </> relFile "blub")
Just (rootDir </> relPath "bla" </> relPath "blub")
>>> Posix.currentDir </> relDir "bla" </> relFile "blub"
currentDir </> relPath "bla" </> relPath "blub"
>>> Just (Posix.currentDir </> relDir "bla" </> relFile "blub")
Just (currentDir </> relPath "bla" </> relPath "blub")
>>> Windows.absDir "c:" </> relDir "bla" </> relFile "blub"
absDir "c:" </> relPath "bla" </> relPath "blub"
>>> Just (Windows.absDir "c:\\" </> relDir "bla" </> relFile "blub")
Just (absDir "c:\\" </> relPath "bla" </> relPath "blub")
-}
instance
    (System os, Class.AbsRel ar, Class.FileDir fd) =>
        Show (Path os ar fd) where
    showsPrec = untag showsPrecTagged

showsPrecTagged ::
    (System os, Class.AbsRel ar, Class.FileDir fd) =>
    Tagged os (Int -> Path os ar fd -> ShowS)
showsPrecTagged =
    flip fmap rootStringTagged $ \root d x ->
        case pathComponents x of
            (ar, pcs) ->
                showParen (d>5) $ concatS $
                intersperse
                    (showChar ' ' . showString combineOperator . showChar ' ') $
                Class.withAbsRel
                    (\drive ->
                        if drive == root
                          then showString rootName
                          else showsCons absDirName drive)
                    (showString currentName)
                    ar :
                map (\(Component pc) -> showsCons relPathName pc) pcs

showsCons :: Show a => String -> a -> ShowS
showsCons name arg  =  showString name . showChar ' ' . showsPrec 11 arg

{- |
Currently it also parses Part.AbsRel and Part.FileDir paths,
although these cannot be composed with the accepted combinators.
-}
-- prop> read "rootDir" == Posix.rootDir
-- prop> read "rootDir" == Windows.rootDir
-- prop> read "currentDir" == Posix.currentDir
-- prop> read "currentDir" == Windows.currentDir
-- prop> let path = Posix.rootDir </> relDir "bla" </> relFile "blub" in read (show path) == path
-- prop> let path = Just (Posix.rootDir </> relDir "bla" </> relFile "blub") in read (show path) == path
-- prop> let path = Posix.currentDir </> relDir "bla" </> relFile "blub" in read (show path) == path
-- prop> let path = Just (Posix.currentDir </> relDir "bla" </> relFile "blub") in read (show path) == path
-- prop> let path = Windows.rootDir </> relDir "bla" </> relFile "blub" in read (show path) == path
-- prop> let path = Just (Windows.rootDir </> relDir "bla" </> relFile "blub") in read (show path) == path
-- prop> let path = Windows.absDir "c:" </> relDir "bla" </> relFile "blub" in read (show path) == path
instance
    (System os, Class.AbsRel ar, Class.FileDir fd) =>
        Read (Path os ar fd) where
    readsPrec d = readParen (d>5) $ untag readsPrecTagged

readsPrecTagged ::
    (System os, Class.AbsRel ar, Class.FileDir fd) =>
    Tagged os (ReadS (Path os ar fd))
readsPrecTagged =
    flip fmap readsSplitDrive $ \readsSplDrv ->
        let go =
                handleMismatch
                    (skipSpaces >> matchString combineOperator)
                    (return [])
                    (liftM2 (:) (fmap Component $ readsCons relPathName) go)
        in  MS.runStateT $ do
                skipSpaces
                MT.lift . maybeToList =<<
                    liftM2 maybePathFromComponents readsSplDrv go

skipSpaces :: (Monad m) => MS.StateT String m ()
skipSpaces = MS.modify $ dropWhile isSpace

readsCons :: (Read a) => String -> MS.StateT String [] a
readsCons name = do
    skipSpaces
    matchString name
    MS.StateT $ readsPrec 11

handleMismatch ::
    MS.StateT s Maybe () ->
    MS.StateT s m a -> MS.StateT s m a -> MS.StateT s m a
handleMismatch act err success =
    MS.StateT $ \s0 ->
        case MS.execStateT act s0 of
           Nothing -> MS.runStateT err s0
           Just s1 -> MS.runStateT success s1

matchString :: (MonadPlus m) => String -> MS.StateT String m ()
matchString prefix =
    MS.StateT $ P.maybe mzero (return . (,) ()) . stripPrefix prefix

readsSplitDrive ::
    (System os, Class.AbsRel ar) => Tagged os (MS.StateT String [] ar)
readsSplitDrive =
    flip fmap readsSplitDriveAbs $ \readsSplDrvAbs ->
        Class.switchAbsRel
            readsSplDrvAbs
            readsSplitDriveRel
            (mplus
                (fmap (\(Part.Abs drive) -> Part.AbsO drive) readsSplDrvAbs)
                (fmap (\Part.Rel -> Part.RelO) readsSplitDriveRel))

readsSplitDriveAbs :: (System os) => Tagged os (MS.StateT String [] Part.Abs)
readsSplitDriveAbs =
    flip fmap rootStringTagged $ \root ->
        fmap absPC $
            (root <$ matchString rootName)
            `mplus`
            readsCons absDirName

readsSplitDriveRel :: (MonadPlus m) => MS.StateT String m Part.Rel
readsSplitDriveRel = matchString currentName >> return Part.Rel


-- | Convert the 'Path' into a plain 'String' as required for OS calls.
--
-- prop> \p -> Path.asPath (Path.toString p) == (p::Default.AbsFile)
toString ::
    (System os, Class.AbsRel ar, Class.FileDir fd) => Path os ar fd -> String
toString = flip toStringS ""

{-# DEPRECATED getPathString "Use Path.toString instead." #-}

-- | Synonym of 'toString' intended for unqualified use.
getPathString ::
    (System os, Class.AbsRel ar, Class.FileDir fd) => Path os ar fd -> String
getPathString = toString

toStringS ::
    (System os, Class.AbsRel ar, Class.FileDir fd) => Path os ar fd -> ShowS
toStringS x =
    case pathComponents x of
        (ar, []) ->
            Class.withAbsRel showString (showString currentDirComponent) ar
        (ar, pcs) ->
            concatS $
            Class.withAbsRel (\drive -> (showString drive :)) id ar $
            intersperse (showChar (selTag x pathSeparator)) $
            map (\(Component pc) -> showString pc) pcs


------------------------------------------------------------------------
-- Constants

-- prop> Posix.toString Path.rootDir == "/"
-- prop> Windows.toString Path.rootDir == "\\"
rootDir :: (System os) => AbsDir os
rootDir = untag rootDirTagged

rootDirTagged :: (System os) => Tagged os (AbsDir os)
rootDirTagged = fmap (\root -> Path (absPC root) [] Part.Dir) rootStringTagged

rootStringTagged :: (System os) => Tagged os String
rootStringTagged = fmap (\sep -> [sep]) pathSeparator

-- prop> Posix.toString Path.currentDir == "."
-- prop> Windows.toString Path.currentDir == "."
currentDir :: (System os) => RelDir os
currentDir = mempty

{- |
This is a file with path @\"\"@.
You will not be able to create a file with this name.
We also forbid parsing @\"\"@ by 'relFile'.
You might only need this file path as intermediate step
when manipulating extensions of files like @\".bashrc\"@.
-}
emptyFile :: (System os) => RelFile os
emptyFile = atomicFile $ Part.File PC.empty

atomicFile :: Part.File -> RelFile os
atomicFile = Path Part.Rel []

rootName :: String
rootName = "rootDir"

currentName :: String
currentName = "currentDir"

currentDirComponent :: String
currentDirComponent = "."

absDirName :: String
absDirName = "absDir"

relPathName :: String
relPathName = "relPath"


------------------------------------------------------------------------
-- Parsing Functions

{-# DEPRECATED maybePath "Use Path.maybe instead." #-}
{-# DEPRECATED parsePath "Use Path.parse instead." #-}

-- | This function is intended for checking and parsing paths
--   provided as user input.
--
-- prop> fmap Posix.toString (Posix.maybePath "/" :: Maybe Posix.AbsDir) == Just "/"
-- prop> fmap Posix.toString (Posix.maybePath "/" :: Maybe Posix.AbsFile) == Nothing
-- prop> fmap Posix.toString (Posix.maybePath "/" :: Maybe Posix.RelDir) == Nothing
-- prop> fmap Posix.toString (Posix.maybePath "/" :: Maybe Posix.RelFile) == Nothing
-- prop> fmap Posix.toString (Posix.maybePath "/tmp" :: Maybe Posix.AbsDir) == Just "/tmp"
-- prop> fmap Posix.toString (Posix.maybePath "/tmp" :: Maybe Posix.AbsFile) == Just "/tmp"
-- prop> fmap Posix.toString (Posix.maybePath "/tmp" :: Maybe Posix.RelDir) == Nothing
-- prop> fmap Posix.toString (Posix.maybePath "/tmp" :: Maybe Posix.RelFile) == Nothing
-- prop> fmap Posix.toString (Posix.maybePath "/tmp/" :: Maybe Posix.AbsDir) == Just "/tmp"
-- prop> fmap Posix.toString (Posix.maybePath "/tmp/" :: Maybe Posix.AbsFile) == Nothing
-- prop> fmap Posix.toString (Posix.maybePath "/tmp/" :: Maybe Posix.RelDir) == Nothing
-- prop> fmap Posix.toString (Posix.maybePath "/tmp/" :: Maybe Posix.RelFile) == Nothing
-- prop> fmap Posix.toString (Posix.maybePath "/tmp" :: Maybe Posix.AbsRelFileDir) == Just "/tmp"
-- prop> fmap Posix.toString (Posix.maybePath "/tmp/" :: Maybe Posix.AbsRelFileDir) == Just "/tmp"
-- prop> fmap Posix.toString (Posix.maybePath "file.txt" :: Maybe Posix.RelFile) == Just "file.txt"
-- prop> fmap Posix.toString (Posix.maybePath "file.txt" :: Maybe Posix.AbsFile) == Nothing
-- prop> fmap Windows.toString (Windows.maybePath "\\tmp" :: Maybe Windows.AbsDir) == Just "\\tmp"
-- prop> fmap Windows.toString (Windows.maybePath "a:\\tmp" :: Maybe Windows.AbsDir) == Just "a:\\tmp"
-- prop> fmap Windows.toString (Windows.maybePath "a:tmp" :: Maybe Windows.AbsDir) == Just "a:tmp"
-- prop> fmap Windows.toString (Windows.maybePath "a:\\" :: Maybe Windows.AbsDir) == Just "a:\\"
-- prop> fmap Windows.toString (Windows.maybePath "a:" :: Maybe Windows.AbsDir) == Just "a:"
-- prop> fmap Windows.toString (Windows.maybePath "tmp" :: Maybe Windows.RelDir) == Just "tmp"
-- prop> fmap Windows.toString (Windows.maybePath "\\tmp" :: Maybe Windows.RelDir) == Nothing
-- prop> fmap Windows.toString (Windows.maybePath "a:\\tmp" :: Maybe Windows.RelDir) == Nothing
-- prop> fmap Windows.toString (Windows.maybePath "a:tmp" :: Maybe Windows.RelDir) == Nothing
-- prop> fmap Windows.toString (Windows.maybePath "tmp" :: Maybe Windows.AbsDir) == Nothing
maybe, maybePath ::
    (System os, Class.AbsRel ar, Class.FileDir fd) =>
    String -> Maybe (Path os ar fd)
maybe str = do
    let (ar0, pcs0, fd0) = untag makePathComponents str
    ar <- Class.fromAbsRel ar0
    (pcs, fd) <-
        case fd0 of
            Left Part.FileDir -> arrangeComponents pcs0
            Right Part.Dir ->
                fmap ((,) pcs0) $
                Class.switchFileDir Nothing (Just Part.Dir) (Just Part.FileDir)
    return $ Path ar pcs fd

maybePath = maybe

parse, parsePath ::
    (System os, Class.AbsRel ar, Class.FileDir fd) =>
    String -> Either String (Path os ar fd)
parse = pathWithNames arName fdName
parsePath = parse

pathWithNames ::
    (System os, Class.AbsRel ar, Class.FileDir fd) =>
    Const String ar -> Const String fd ->
    String -> Either String (Path os ar fd)
pathWithNames (Const ar) (Const fd) str =
    P.maybe (Left (printf "\"%s\" is not a valid %s%spath" str ar fd)) Right $
    maybePath str

arName :: (Class.AbsRel ar) => Const String ar
arName = Class.switchAbsRel (Const "absolute ") (Const "relative ") (Const "")

fdName :: (Class.FileDir fd) => Const String fd
fdName = Class.switchFileDir (Const "file ") (Const "directory ") (Const "")

------------------------------------------------------------------------
-- Checked Construction Functions

-- | This function is intended for converting path strings
--   with known content, e.g. string literals, to the 'Path' type.
path ::
    (System os, Class.AbsRel ar, Class.FileDir fd) =>
    String -> Path os ar fd
path = either error id . parsePath

-- | Construct a 'RelFile' from a 'String'.
--
-- prop> Posix.toString (Posix.relFile "file.txt") == "file.txt"
-- prop> Posix.toString (Posix.relFile "tmp") == "tmp"
relFile :: (System os) => String -> RelFile os
relFile = path

-- | Construct a 'RelDir' from a 'String'.
--
-- prop> Posix.toString (Posix.relDir ".") == "."
-- prop> Posix.toString (Posix.relDir "file.txt") == "file.txt"
-- prop> Posix.toString (Posix.relDir "tmp") == "tmp"
relDir :: (System os) => String -> RelDir os
relDir = path

-- | Construct an 'AbsFile' from a 'String'.
--
-- prop> Posix.toString (Posix.absFile "/file.txt") == "/file.txt"
-- prop> Posix.toString (Posix.absFile "/tmp") == "/tmp"
absFile :: (System os) => String -> AbsFile os
absFile = path

-- | Construct an 'AbsDir' from a 'String'.
--
-- prop> Posix.toString (Posix.absDir "/file.txt") == "/file.txt"
-- prop> Posix.toString (Posix.absDir "/tmp") == "/tmp"
absDir :: (System os) => String -> AbsDir os
absDir = path

-- | Construct a 'Rel fd' from a 'String'.
rel :: (System os, Class.FileDir fd) => String -> Rel os fd
rel = path

-- | Construct an 'Abs fd' from a 'String'.
abs :: (System os, Class.FileDir fd) => String -> Abs os fd
abs = path

-- | Construct an 'AbsRel fd' from a 'String'.
absRel :: (System os, Class.FileDir fd) => String -> AbsRel os fd
absRel = path

-- | Construct a 'File ar' from a 'String'.
file :: (System os, Class.AbsRel ar) => String -> File os ar
file = path

-- | Construct a 'Dir ar' from a 'String'.
dir :: (System os, Class.AbsRel ar) => String -> Dir os ar
dir = path

-- | Construct a 'FileDir ar' from a 'String'.
fileDir :: (System os, Class.AbsRel ar) => String -> FileDir os ar
fileDir = path


{-# DEPRECATED relPath    "Use Path.rel instead." #-}
{-# DEPRECATED absPath    "Use Path.abs instead." #-}
{-# DEPRECATED filePath   "Use Path.file instead." #-}
{-# DEPRECATED dirPath    "Use Path.dir instead." #-}

-- | Construct a 'RelPath fd' from a 'String'.
relPath :: (System os, Class.FileDir fd) => String -> RelPath os fd
relPath = path

-- | Construct an 'AbsPath fd' from a 'String'.
absPath :: (System os, Class.FileDir fd) => String -> AbsPath os fd
absPath = path

-- | Construct a 'FilePath ar' from a 'String'.
filePath :: (System os, Class.AbsRel ar) => String -> FilePath os ar
filePath = path

-- | Construct a 'DirPath ar' from a 'String'.
dirPath :: (System os, Class.AbsRel ar) => String -> DirPath os ar
dirPath = path



idAbsRel :: AbsRelPath os fd -> AbsRelPath os fd
idAbsRel = id

idAbs :: AbsPath os fd -> AbsPath os fd
idAbs = id

idRel :: RelPath os fd -> RelPath os fd
idRel = id


idFileDir :: FileDirPath os fd -> FileDirPath os fd
idFileDir = id

idFile :: FilePath os fd -> FilePath os fd
idFile = id

idDir :: DirPath os fd -> DirPath os fd
idDir = id


{-# DEPRECATED asPath "Use 'maybePath', 'parsePath' or 'path' instead." #-}
{-# DEPRECATED asRelFile "Use 'relFile' instead." #-}
{-# DEPRECATED asRelDir "Use 'relDir' instead." #-}
{-# DEPRECATED asAbsFile "Use 'absFile' instead." #-}
{-# DEPRECATED asAbsDir "Use 'absDir' instead." #-}
{-# DEPRECATED asRelPath "Use 'relPath' instead." #-}
{-# DEPRECATED asAbsPath "Use 'absPath' instead." #-}
{-# DEPRECATED asFilePath "Use 'filePath' instead." #-}
{-# DEPRECATED asDirPath "Use 'dirPath' instead." #-}

------------------------------------------------------------------------
-- Unchecked Construction Functions
-- NB - these construction functions are non-IO and do no checking!!

-- | Use a 'String' as a 'Path' whose type is determined by its context.
--   You should not use this and other @as*@ functions,
--   since they may silently turn a relative path to an absolute one,
--   or vice versa, or they may accept a path as file path
--   although it ends on a slash.
--   If you are certain about the string content
--   then you should use 'path'.
--   If you got the string as user input then use 'maybePath' or 'parsePath'.
--
-- prop> Posix.asPath "/tmp" == Posix.absDir "/tmp"
-- prop> Posix.asPath "file.txt" == Posix.relFile "file.txt"
-- prop> Path.isAbsolute (Posix.asAbsDir "/tmp")
-- prop> Path.isRelative (Posix.asRelDir "/tmp")
-- prop> Posix.toString (Posix.asPath "/tmp" :: Posix.AbsDir) == "/tmp"
-- prop> Posix.toString (Posix.asPath "/tmp" :: Posix.RelDir) == "tmp"
-- prop> Windows.toString (Windows.asPath "\\tmp" :: Windows.AbsDir) == "\\tmp"
-- prop> Windows.toString (Windows.asPath "a:\\tmp" :: Windows.AbsDir) == "a:\\tmp"
-- prop> Windows.toString (Windows.asPath "a:tmp" :: Windows.AbsDir) == "a:tmp"
-- prop> Windows.toString (Windows.asPath "tmp" :: Windows.RelDir) == "tmp"
asPath ::
    (System os, Class.AbsRel ar, Class.FileDir fd) => String -> Path os ar fd
asPath = uncurry mkPathFromComponents . untag mkPathComponents


-- | Use a 'String' as a 'RelFile'. No checking is done.
--
-- prop> Posix.toString (Posix.asRelFile "file.txt") == "file.txt"
-- prop> Posix.toString (Posix.asRelFile "/file.txt") == "file.txt"
-- prop> Posix.toString (Posix.asRelFile "tmp") == "tmp"
-- prop> Posix.toString (Posix.asRelFile "/tmp") == "tmp"
asRelFile :: (System os) => String -> RelFile os
asRelFile = asPath

-- | Use a 'String' as a 'RelDir'. No checking is done.
--
-- prop> Posix.toString (Posix.asRelDir ".") == "."
-- prop> Posix.toString (Posix.asRelDir "file.txt") == "file.txt"
-- prop> Posix.toString (Posix.asRelDir "/file.txt") == "file.txt"
-- prop> Posix.toString (Posix.asRelDir "tmp") == "tmp"
-- prop> Posix.toString (Posix.asRelDir "/tmp") == "tmp"
asRelDir :: (System os) => String -> RelDir os
asRelDir = asPath

-- | Use a 'String' as an 'AbsFile'. No checking is done.
--
-- prop> Posix.toString (Posix.asAbsFile "/file.txt") == "/file.txt"
-- prop> Posix.toString (Posix.asAbsFile "/tmp") == "/tmp"
asAbsFile :: (System os) => String -> AbsFile os
asAbsFile = asPath

-- | Use a 'String' as an 'AbsDir'. No checking is done.
--
-- prop> Posix.toString (Posix.asAbsDir "/file.txt") == "/file.txt"
-- prop> Posix.toString (Posix.asAbsDir "/tmp") == "/tmp"
asAbsDir :: (System os) => String -> AbsDir os
asAbsDir = asPath

-- | Use a 'String' as a 'RelPath fd'. No checking is done.
asRelPath :: (System os, Class.FileDir fd) => String -> RelPath os fd
asRelPath = asPath

-- | Use a 'String' as an 'AbsPath fd'. No checking is done.
asAbsPath :: (System os, Class.FileDir fd) => String -> AbsPath os fd
asAbsPath = asPath

-- | Use a 'String' as a 'FilePath ar'. No checking is done.
asFilePath :: (System os, Class.AbsRel ar) => String -> FilePath os ar
asFilePath = asPath

-- | Use a 'String' as a 'DirPath ar'. No checking is done.
asDirPath :: (System os, Class.AbsRel ar) => String -> DirPath os ar
asDirPath = asPath

-- | Forbid use of OverloadedStrings and prevent custom orphan instances
instance
    (ForbiddenSystem os, ForbiddenAbsRel ar, ForbiddenFileDir fd) =>
        IsString (Path os ar fd) where fromString = forbiddenFromString

class System os => ForbiddenSystem os where
    forbiddenFromString :: String -> Path os ar fd

class Class.AbsRel ar => ForbiddenAbsRel ar where
class Class.FileDir fd => ForbiddenFileDir fd where

------------------------------------------------------------------------
-- Checked Construction Functions

{-# DEPRECATED mkPathAbsOrRel "Use Path.absRel instead." #-}

-- | Examines the supplied string and constructs an absolute or
-- relative path as appropriate.
--
-- prop> Path.mkPathAbsOrRel "/tmp" == Left (Posix.absDir "/tmp")
-- prop> Path.mkPathAbsOrRel  "tmp" == Right (Posix.relDir "tmp")
-- prop> Path.mkPathAbsOrRel "\\tmp" == Left (Windows.absDir "\\tmp")
-- prop> Path.mkPathAbsOrRel "d:\\tmp" == Left (Windows.absDir "d:\\tmp")
-- prop> Path.mkPathAbsOrRel "d:tmp" == Left (Windows.absDir "d:tmp")
-- prop> Path.mkPathAbsOrRel "tmp" == Right (Windows.relDir "tmp")
mkPathAbsOrRel, mkPathAbsOrRelPriv ::
    (System os, Class.FileDir fd) =>
    String -> Either (AbsPath os fd) (RelPath os fd)
mkPathAbsOrRel = mkPathAbsOrRelPriv
mkPathAbsOrRelPriv = eitherFromAbsRel . absRel

{-# DEPRECATED mkPathFileOrDir "Don't let the path type depend on current file system content. Instead choose the path type according to the needed disk object type." #-}

-- | Searches for a file or directory with the supplied path string
--   and returns a 'Part.File' or 'Part.Dir' path as appropriate. If neither exists
--   at the supplied path, 'Nothing' is returned.
mkPathFileOrDir ::
    (System os, Class.AbsRel ar) =>
    String -> IO (Maybe (Either (FilePath os ar) (DirPath os ar)))
mkPathFileOrDir s = do
  isfile <- SD.doesFileExist s
  isdir <- SD.doesDirectoryExist s
  case (isfile, isdir) of
    (False, False) -> return Nothing
    (True,  False) -> return $ Just $ Left $ path s
    (False, True ) -> return $ Just $ Right $ path s
    (True,  True ) -> ioError $ userError "mkPathFileOrDir - object type changed while checking"

{-# DEPRECATED mkAbsPath "Use Path.dynamicMakeAbsolute instead." #-}

-- | Convert a 'String' into an 'AbsPath' by interpreting it as
--   relative to the supplied directory if necessary.
--
-- prop> Path.mkAbsPath (absDir "/tmp") "foo.txt" == Posix.absFile "/tmp/foo.txt"
-- prop> Path.mkAbsPath (absDir "/tmp") "/etc/foo.txt" == Posix.absFile "/etc/foo.txt"
mkAbsPath ::
    (System os, Class.FileDir fd) => AbsDir os -> String -> AbsPath os fd
mkAbsPath d = either id (makeAbsolute d) . mkPathAbsOrRelPriv

{-# DEPRECATED mkAbsPathFromCwd "Use Path.dynamicMakeAbsoluteFromCwd instead." #-}

-- | Convert a 'String' into an 'AbsPath' by interpreting it as
--   relative to the cwd if necessary.
mkAbsPathFromCwd ::
    (System os, Class.FileDir fd) => String -> IO (AbsPath os fd)
mkAbsPathFromCwd = either return makeAbsoluteFromCwd . mkPathAbsOrRelPriv


------------------------------------------------------------------------
-- Internal Functions for GenComponent manipulation

mkPathFromComponents ::
    (Class.FileDir fd) => ar -> [Component os] -> Path os ar fd
mkPathFromComponents ar pcs =
    uncurry (Path ar) $
    Class.switchFileDir
        (mapSnd Part.File $
         ListHT.switchR ([], PC.empty) (curry $ mapSnd PC.untag) pcs)
        (pcs, Part.Dir)
        (pcs, Part.FileDir)

maybePathFromComponents ::
    (Class.FileDir fd) => ar -> [Component os] -> Maybe (Path os ar fd)
maybePathFromComponents ar pcs =
    fmap (uncurry $ Path ar) $ arrangeComponents pcs

arrangeComponents ::
    (Class.FileDir fd) => [Component os] -> Maybe ([Component os], fd)
arrangeComponents pcs =
    getCompose $
    Class.switchFileDir
        (Compose $ fmap (mapSnd (Part.File . PC.untag)) $ ListHT.viewR pcs)
        (Compose $ Just (pcs, Part.Dir))
        (Compose $ Just (pcs, Part.FileDir))

mkPathComponents ::
    (System os, Class.AbsRel ar) =>
    Tagged os (String -> (ar, [Component os]))
mkPathComponents =
    liftA2
        (\isSep splDriveOS ->
            mapSnd (nonEmptyComponents . ListHT.chop isSep)
             . MS.runState splDriveOS)
        isPathSeparator splitDriveOS

{- |
Parse path string independent from expectations
expressed by the type parameters.
-}
makePathComponents ::
    (System os) =>
    Tagged os
        (String ->
            (Part.AbsRel, [Component os], Either Part.FileDir Part.Dir))
makePathComponents =
    liftA2
        (\isSep splAbsolute str ->
            let (ar, pct) =
                    mapSnd (ListHT.chop isSep) $
                    MS.runState splAbsolute str
                (pcs1, fd) =
                    case ListHT.viewR pct of
                        Nothing -> ([], Right Part.Dir)
                        Just (pcs, pc) ->
                            if null pc -- caused by trailing slash
                              then (pcs, Right Part.Dir)
                              else (pct, Left Part.FileDir)
            in  (ar, nonEmptyComponents pcs1, fd))
        isPathSeparator splitAbsoluteO

nonEmptyComponents :: [String] -> [Component os]
nonEmptyComponents = map Component . filter (not . null)

splitDriveOS ::
    (System os, Class.AbsRel ar) => Tagged os (MS.State String ar)
splitDriveOS =
    liftA2
        (\splDrive splAbsolute ->
            Class.switchAbsRel (fmap absPC splDrive) (return Part.Rel) splAbsolute)
        splitDriveAbs splitAbsoluteO

splitDriveAbs :: (System os) => Tagged os (MS.State String String)
splitDriveAbs =
    liftA2
        (\isSep splDrive -> do
            drive <- splDrive
            xt <- MS.get
            case xt of
                [] -> return drive
                x:xs ->
                    if isSep x
                      then MS.put xs >> return (drive++[x])
                      else return drive)
        isPathSeparator splitDrive

splitAbsoluteO :: (System os) => Tagged os (MS.State String Part.AbsRel)
splitAbsoluteO =
    fmap (\drive -> if null drive then Part.RelO else Part.AbsO $ Component drive)
    <$>
    splitAbsolute

-- | > \p -> uncurry Path.mkPathFromComponents (Path.pathComponents p) == (p::Default.AbsDir)
pathComponents ::
    (Class.FileDir fd) => Path os ar fd -> (ar, [Component os])
pathComponents (Path ar pcs fd) =
    (ar, pcs ++ Class.withFileDir ((:[]) . PC.retag) [] [] fd)

prop_mkPathFromComponents_pathComponents :: (System os) => AbsDir os -> Property
prop_mkPathFromComponents_pathComponents p =
    property $ uncurry mkPathFromComponents (pathComponents p) == p



------------------------------------------------------------------------
-- Basic Manipulation Functions

combineOperator :: String
combineOperator = "</>"


instance (Class.Rel ar, Class.Dir fd) => Semigroup (Path os ar fd) where
    Path r pcs0 _dir <> Path _rel pcs1 d = Path r (pcs0 ++ pcs1) d
    sconcat paths =
        Path Class.relVar
            (sconcat $ fmap (\(Path _rel pcs _dir) -> pcs) paths) Class.dirVar

instance (Class.Rel ar, Class.Dir fd) => Monoid (Path os ar fd) where
    mempty = Path Class.relVar [] Class.dirVar
    mappend (Path r pcs0 _dir) (Path _rel pcs1 d) = Path r (pcs0 ++ pcs1) d
    mconcat paths =
        Path Class.relVar
            (concatMap (\(Path _rel pcs _dir) -> pcs) paths) Class.dirVar


-- | Infix variant of 'combine'.
--
-- prop> Posix.toString (Posix.absDir "/tmp" </> Posix.relFile "file.txt") == "/tmp/file.txt"
-- prop> Posix.toString (Posix.absDir "/tmp" </> Posix.relDir "dir" </> Posix.relFile "file.txt") == "/tmp/dir/file.txt"
-- prop> Posix.toString (Posix.relDir "dir" </> Posix.relFile "file.txt") == "dir/file.txt"
-- prop> Windows.toString (Windows.absDir "\\tmp" </> Windows.relFile "file.txt") == "\\tmp\\file.txt"
-- prop> Windows.toString (Windows.absDir "c:\\tmp" </> Windows.relFile "file.txt") == "c:\\tmp\\file.txt"
-- prop> Windows.toString (Windows.absDir "c:tmp" </> Windows.relFile "file.txt") == "c:tmp\\file.txt"
-- prop> Windows.toString (Windows.absDir "c:\\" </> Windows.relDir "tmp" </> Windows.relFile "file.txt") == "c:\\tmp\\file.txt"
-- prop> Windows.toString (Windows.absDir "c:" </> Windows.relDir "tmp" </> Windows.relFile "file.txt") == "c:tmp\\file.txt"
-- prop> Windows.toString (Windows.relDir "dir" </> Windows.relFile "file.txt") == "dir\\file.txt"
(</>) :: DirPath os ar -> RelPath os fd -> Path os ar fd
Path ar pcs0 Part.Dir  </>  Path Part.Rel pcs1 fd  =  Path ar (pcs0 ++ pcs1) fd

infixr 5  </>

-- | Infix variant of 'addExtension'.
--   We only allow files (and not directories) to have extensions added
--   by this function. This is because it's the vastly common case and
--   an attempt to add one to a directory will - more often than not -
--   represent an error.
--   We don't however want to prevent the corresponding operation on
--   directories, and so we provide a function that is more flexible:
--   'genericAddExtension'.
(<.>) :: FilePath os ar -> String -> FilePath os ar
p <.> ext = mapFilePart (flip PC.addExtension ext) p

infixl 7  <.>

(<++>) :: FilePath os ar -> String -> FilePath os ar
p <++> str = mapFileName (++str) p

infixl 7  <++>

-- | Add an extension, even if there is already one there.
--   E.g. @addExtension \"foo.txt\" \"bat\" -> \"foo.txt.bat\"@.
--
-- prop> Path.addExtension (relFile "file.txt") "bib" == Posix.relFile "file.txt.bib"
-- prop> Path.addExtension (relFile "file.") ".bib" == Posix.relFile "file..bib"
-- prop> Path.addExtension (relFile "file") ".bib" == Posix.relFile "file.bib"
-- prop> Path.addExtension Path.emptyFile "bib" == Posix.relFile ".bib"
-- prop> Path.addExtension Path.emptyFile ".bib" == Posix.relFile ".bib"
-- prop> Path.takeFileName (Path.addExtension Path.emptyFile "ext") == Posix.relFile ".ext"
addExtension :: FilePath os ar -> String -> FilePath os ar
addExtension = (<.>)

-- | Join an (absolute or relative) directory path with a relative
--   (file or directory) path to form a new path.
--
-- prop> \p -> Path.combine Path.currentDir p == (p::Default.RelDir)
combine :: DirPath os ar -> RelPath os fd -> Path os ar fd
combine = (</>)


-- | Remove last extension, and the \".\" preceding it.
--
-- prop> forAllAbsRel $ \x -> Path.dropExtension x == fst (Path.splitExtension x)
dropExtension :: FilePath os ar -> FilePath os ar
dropExtension = fst . splitExtension

-- | Drop all extensions
--
-- prop> forAllAbsRel $ \x -> not $ Path.hasAnExtension (Path.dropExtensions x)
dropExtensions :: FilePath os ar -> FilePath os ar
dropExtensions = fst . splitExtensions

-- | Synonym for 'takeDirectory'
dropFileName :: FilePath os ar -> DirPath os ar
dropFileName = fst . splitFileName


-- | Set the extension of a file, overwriting one if already present.
--
-- prop> Path.replaceExtension (relFile "file.txt") ".bob" == Posix.relFile "file.bob"
-- prop> Path.replaceExtension (relFile "file.txt") "bob" == Posix.relFile "file.bob"
-- prop> Path.replaceExtension (relFile "file") ".bob" == Posix.relFile "file.bob"
-- prop> Path.replaceExtension (relFile "file.txt") "" == Posix.relFile "file"
-- prop> Path.replaceExtension (relFile "file.fred.bob") "txt" == Posix.relFile "file.fred.txt"
replaceExtension :: FilePath os ar -> String -> FilePath os ar
replaceExtension p ext = dropExtension p <.> ext

replaceBaseName :: FilePath os ar -> String -> FilePath os ar
replaceBaseName p bn =
    mapFilePart (PC.addExtension (Component bn) . snd . PC.splitExtension) p

replaceDirectory :: FilePath os ar1 -> DirPath os ar2 -> FilePath os ar2
replaceDirectory (Path _ _ fd) (Path ar pcs _) = Path ar pcs fd

replaceFileName :: FilePath os ar -> String -> FilePath os ar
replaceFileName p fn = mapFilePart (const (Component fn)) p


-- | Split on the extension. 'addExtension' is the inverse.
--
-- prop> forAllAbsRel $ \x -> uncurry (<.>) (Path.splitExtension x) == x
-- prop> forAllAbsRel $ \x -> uncurry Path.addExtension (Path.splitExtension x) == x
-- prop> Path.splitExtension (relFile "file.txt") == (Posix.relFile "file",".txt")
-- prop> Path.splitExtension (relFile ".bashrc") == (Posix.emptyFile, ".bashrc")
-- prop> Path.splitExtension (relFile "file") == (Posix.relFile "file","")
-- prop> Path.splitExtension (relFile "file/file.txt") == (Posix.relFile "file/file",".txt")
-- prop> Path.splitExtension (relFile "file.txt/boris") == (Posix.relFile "file.txt/boris","")
-- prop> Path.splitExtension (relFile "file.txt/boris.ext") == (Posix.relFile "file.txt/boris",".ext")
-- prop> Path.splitExtension (relFile "file/path.txt.bob.fred") == (Posix.relFile "file/path.txt.bob",".fred")
splitExtension :: FilePath os ar -> (FilePath os ar, String)
splitExtension = splitFilePart PC.splitExtension

-- | Split on all extensions
--
-- prop> Path.splitExtensions (relFile "file.tar.gz") == (Posix.relFile "file",".tar.gz")
-- prop> \p -> uncurry (<.>) (Path.splitExtension p) == (p::Default.AbsFile)
splitExtensions :: FilePath os ar -> (FilePath os ar, String)
splitExtensions = splitFilePart PC.splitExtensions

-- | prop> \p -> uncurry Path.combine (Path.splitFileName p) == (p::Default.AbsFile)
splitFileName :: FilePath os ar -> (DirPath os ar, RelFile os)
splitFileName (Path ar pcs fd) = (Path ar pcs Part.Dir, atomicFile fd)

-- | > \p -> (uncurry Path.combine <$> Path.splitDirName p) == toMaybe (not $ Default.isDrive p) (p::Default.AbsDir)
splitDirName :: DirPath os ar -> Maybe (DirPath os ar, RelDir os)
splitDirName = fmap (mapSnd dirFromFile . splitFileName) . fileFromDir

prop_splitDir_combine :: (System os) => AbsDir os -> Property
prop_splitDir_combine p =
    property $
    (uncurry combine <$> splitDirName p) == toMaybe (not $ isDrive p) p


-- | Get the basename of a file
--
-- prop> Path.takeBaseName (absFile "/tmp/somedir/myfile.txt") == Posix.relFile "myfile"
-- prop> Path.takeBaseName (relFile "./myfile.txt") == Posix.relFile "myfile"
-- prop> Path.takeBaseName (relFile "myfile.txt") == Posix.relFile "myfile"
takeBaseName :: FilePath os ar -> RelFile os
takeBaseName = takeFileName . dropExtension

takeDirectory :: FilePath os ar -> DirPath os ar
takeDirectory = fst . splitFileName

-- prop> Path.takeSuperDirectory (Posix.absDir "/tmp/somedir") == Just (absDir "/tmp")
-- prop> Path.takeSuperDirectory (Posix.absDir "/tmp/") == Just (absDir "/")
-- prop> Path.takeSuperDirectory (Posix.absDir "/") == Nothing
-- prop> Path.takeSuperDirectory (Posix.relDir "tmp/somedir") == Just (relDir "tmp")
-- prop> Path.takeSuperDirectory (Posix.relDir "./somedir") == Just (relDir ".")
-- prop> Path.takeSuperDirectory (Posix.relDir "somedir") == Just Path.currentDir
-- prop> Path.takeSuperDirectory (Posix.relDir "") == Nothing
takeSuperDirectory :: DirPath os ar -> Maybe (DirPath os ar)
takeSuperDirectory = fmap takeDirectory . fileFromDir

-- | Get the extension of a file, returns @\"\"@ for no extension, @.ext@ otherwise.
--
-- prop> forAllAbsRel $ \x -> Path.takeExtension x == snd (Path.splitExtension x)
-- prop> forAllAbsRel $ \x -> Path.takeExtension (Path.addExtension x "ext") == ".ext"
-- prop> forAllAbsRel $ \x -> Path.takeExtension (Path.replaceExtension x "ext") == ".ext"
takeExtension :: FilePath os ar -> String
takeExtension = snd . splitExtension

-- | Get all extensions
--
-- prop> Path.takeExtensions (Posix.relFile "file.tar.gz") == ".tar.gz"
takeExtensions :: FilePath os ar -> String
takeExtensions = snd . splitExtensions

-- | Get the filename component of a file path (ie stripping all parent dirs)
--
-- prop> Path.takeFileName (absFile "/tmp/somedir/myfile.txt") == Posix.relFile "myfile.txt"
-- prop> Path.takeFileName (relFile "./myfile.txt") == Posix.relFile "myfile.txt"
-- prop> Path.takeFileName (relFile "myfile.txt") == Posix.relFile "myfile.txt"
-- prop> \p -> Path.toString (Path.takeFileName p) `isSuffixOf` Path.toString (p::Default.AbsFile)
takeFileName :: FilePath os ar -> RelFile os
takeFileName (Path _ _ fd) = atomicFile fd

-- | > \p -> fmap (\d -> toString d `isSuffixOf` toString p) (takeDirName p) == toMaybe (not $ isDrive p) True
takeDirName :: DirPath os ar -> Maybe (RelDir os)
takeDirName = fmap snd . splitDirName

prop_takeDirName_end :: (System os) => AbsDir os -> Property
prop_takeDirName_end p =
    property $
    fmap (\d -> toString d `isSuffixOf` toString p) (takeDirName p)
    ==
    toMaybe (not $ isDrive p) True

mapFileName :: (String -> String) -> FilePath os ar -> FilePath os ar
mapFileName = mapFilePart . PC.map

mapFileNameF ::
    (Functor f) =>
    (String -> f String) -> FilePath os ar -> f (FilePath os ar)
mapFileNameF = mapFilePartF . PC.mapF


------------------------------------------------------------------------
-- Auxillary Manipulation Functions

-- | Check whether two strings are equal as file paths.
--
-- prop>       Posix.equalFilePath "abc/def" "abc/def"
-- prop>       Posix.equalFilePath "abc/def" "abc//def"
-- prop>       Posix.equalFilePath "/tmp/" "/tmp"
-- prop>       Posix.equalFilePath "/tmp" "//tmp"
-- prop>       Posix.equalFilePath "/tmp" "///tmp"
-- prop> not $ Posix.equalFilePath "abc" "def"
-- prop> not $ Posix.equalFilePath "/tmp" "tmp"
-- prop>       Windows.equalFilePath "abc\\def" "abc\\def"
-- prop>       Windows.equalFilePath "abc\\def" "abc\\\\def"
-- prop>       Windows.equalFilePath "file" "File"
-- prop>       Windows.equalFilePath "\\file" "\\\\file"
-- prop>       Windows.equalFilePath "\\file" "\\\\\\file"
-- prop> not $ Windows.equalFilePath "abc" "def"
-- prop> not $ Windows.equalFilePath "file" "dir"
equalFilePath :: (System os) => Tagged os (String -> String -> Bool)
equalFilePath = equating <$> mkPathAbsOrRelTagged

mkPathAbsOrRelTagged ::
    (System os) =>
    Tagged os (String -> Either (AbsFileDir os) (RelFileDir os))
mkPathAbsOrRelTagged = Tagged mkPathAbsOrRelPriv

-- | Constructs a 'RelPath' from a list of components.
--   It is an unchecked error if the path components contain path separators.
--   It is an unchecked error if a 'RelFile' path is empty.
--
-- prop> Path.joinPath ["tmp","someDir","dir"] == Posix.relDir "tmp/someDir/dir"
-- prop> Path.joinPath ["tmp","someDir","file.txt"] == Posix.relFile "tmp/someDir/file.txt"
joinPath :: (Class.FileDir fd) => [String] -> RelPath os fd
joinPath = mkPathFromComponents Part.Rel . map Component

-- | Currently just transforms:
--
-- prop> Path.normalise (absFile "/tmp/fred/./jim/./file") == Posix.absFile "/tmp/fred/jim/file"
normalise :: (System os) => Path os ar fd -> Path os ar fd
normalise = mapPathDirs (filter (Component currentDirComponent /=))

-- | Deconstructs a path into its components.
--
-- prop> Path.splitPath (Posix.absDir "/tmp/someDir/mydir.dir") == (True, map relDir ["tmp","someDir","mydir.dir"], Nothing)
-- prop> Path.splitPath (Posix.absFile "/tmp/someDir/myfile.txt") == (True, map relDir ["tmp","someDir"], Just $ relFile "myfile.txt")
splitPath ::
    (Class.AbsRel ar, Class.FileOrDir fd) =>
    Path os ar fd -> (Bool, [RelDir os], Maybe (RelFile os))
splitPath (Path ar pcs fd) =
    (Class.isAbsolute ar,
     map (\pc -> Path Part.Rel [pc] Part.Dir) pcs,
     maybeFileDir fd)

maybeFileDir :: (Class.FileOrDir fd) => fd -> Maybe (RelFile os)
maybeFileDir = Class.withFileOrDir (Just . atomicFile . Part.File) Nothing

-- | This function can be used to construct a relative path by removing
--   the supplied 'AbsDir' from the front. It is a runtime 'error' if the
--   supplied 'AbsPath' doesn't start with the 'AbsDir'.
--
-- prop> Path.makeRelative (absDir "/tmp/somedir") (absFile "/tmp/somedir/anotherdir/file.txt") == Posix.relFile "anotherdir/file.txt"
-- prop> Path.makeRelative (absDir "/tmp/somedir") (absDir "/tmp/somedir/anotherdir/dir") == Posix.relDir "anotherdir/dir"
-- prop> Path.makeRelative (absDir "c:\\tmp\\somedir") (absFile "C:\\Tmp\\SomeDir\\AnotherDir\\File.txt") == Windows.relFile "AnotherDir\\File.txt"
-- prop> Path.makeRelative (absDir "c:\\tmp\\somedir") (absDir "c:\\tmp\\somedir\\anotherdir\\dir") == Windows.relDir "anotherdir\\dir"
-- prop> Path.makeRelative (absDir "c:tmp\\somedir") (absDir "c:tmp\\somedir\\anotherdir\\dir") == Windows.relDir "anotherdir\\dir"
makeRelative ::
    (System os, Class.FileDir fd) =>
    AbsDir os -> AbsPath os fd -> RelPath os fd
makeRelative relTo orig =
    fromMaybe
        (error $
            printf "System.Path can't make (%s) relative to (%s)"
                (toString orig) (toString relTo)) $
    makeRelativeMaybe relTo orig

-- prop> Path.makeRelativeMaybe (Posix.absDir "/tmp/somedir") (absFile "/tmp/anotherdir/file.txt") == Nothing
-- prop> Path.makeRelativeMaybe (Posix.absDir "/Tmp") (absFile "/tmp/anotherdir/file.txt") == Nothing
-- prop> Path.makeRelativeMaybe (Windows.absDir "\\Tmp") (absFile "\\tmp\\anotherdir\\file.txt") == Just (relFile "anotherdir\\file.txt")
makeRelativeMaybe ::
    (System os, Class.FileDir fd) =>
    AbsDir os -> AbsPath os fd -> Maybe (RelPath os fd)
makeRelativeMaybe relTo orig =
    case (inspectPath relTo, inspectPath orig) of
        ((relToAR, relToPCs, WrapFileDir Part.Dir),
         (origAR, origPCs, WrapFileDir fd)) ->
            fmap (flip (Path Part.Rel) fd) $
                guard (relToAR == origAR) >> stripPrefix relToPCs origPCs

-- | Joins an absolute directory with a relative path to construct a
--   new absolute path.
--
-- prop> Path.makeAbsolute (absDir "/tmp") (relFile "file.txt")      == Posix.absFile "/tmp/file.txt"
-- prop> Path.makeAbsolute (absDir "/tmp") (relFile "adir/file.txt") == Posix.absFile "/tmp/adir/file.txt"
-- prop> Path.makeAbsolute (absDir "/tmp") (relDir  "adir/dir")      == Posix.absDir "/tmp/adir/dir"
-- prop> \base p -> Default.toString p `isSuffixOf` Path.toString (Path.makeAbsolute base (Path.idFile p))
-- prop> \base p -> Default.toString base `isPrefixOf` Path.toString (Path.makeAbsolute base (Path.idFile p))
makeAbsolute :: (System os) => AbsDir os -> RelPath os fd -> AbsPath os fd
makeAbsolute = genericMakeAbsolute

-- | Converts a relative path into an absolute one by
--   prepending the current working directory.
makeAbsoluteFromCwd :: (System os) => RelPath os fd -> IO (AbsPath os fd)
makeAbsoluteFromCwd = genericMakeAbsoluteFromCwd

dynamicMakeAbsolute ::
    (System os) => AbsDir os -> AbsRelPath os fd -> AbsPath os fd
dynamicMakeAbsolute = genericMakeAbsolute

dynamicMakeAbsoluteFromCwd ::
    (System os) => AbsRelPath os fd -> IO (AbsPath os fd)
dynamicMakeAbsoluteFromCwd = genericMakeAbsoluteFromCwd

-- | As for 'makeAbsolute', but for use when the path may already be
--   absolute (in which case it is left unchanged).
--   You should avoid the use of 'genericMakeAbsolute'-type functions,
--   because then you avoid to absolutize a path that was already absolutized.
--
-- prop> Path.genericMakeAbsolute (absDir "/tmp") (relFile "file.txt")       == Posix.absFile "/tmp/file.txt"
-- prop> Path.genericMakeAbsolute (absDir "/tmp") (relFile "adir/file.txt")  == Posix.absFile "/tmp/adir/file.txt"
-- prop> Path.genericMakeAbsolute (absDir "/tmp") (absFile "/adir/file.txt") == Posix.absFile "/adir/file.txt"
genericMakeAbsolute ::
    (System os, Class.AbsRel ar) => AbsDir os -> Path os ar fd -> AbsPath os fd
genericMakeAbsolute base p = withAbsRel id (base </>) p

-- | As for 'makeAbsoluteFromCwd', but for use when the path may already be
--   absolute (in which case it is left unchanged).
genericMakeAbsoluteFromCwd ::
    (System os, Class.AbsRel ar) => Path os ar fd -> IO (AbsPath os fd)
genericMakeAbsoluteFromCwd p = do
  cwdString <- SD.getCurrentDirectory -- we don't use System.Path.Directory impl here to avoid module cycle
  return $ genericMakeAbsolute (asAbsDir cwdString) p

-- prop_makeAbsoluteFromDir_startSameAbs :: AbsDir os -> AbsFile -> Property
-- prop_makeAbsoluteFromDir_startSameAbs base p = property $ show base `isPrefixOf` show (makeAbsolute base p)


-- | Convert a file to a directory path.
--   Obviously, the corresponding disk object won't change accordingly.
--   The purpose of this function is to be an intermediate step
--   when deriving a directory name from a file name.
dirFromFile :: FilePath os ar -> DirPath os ar
dirFromFile p = uncurry Path (pathComponents p) Part.Dir

-- | Convert a directory to a file path.
--   The function returns 'Nothing' if the directory path is empty.
--   The purpose of this function is to be an intermediate step
--   when deriving a file name from a directory name.
fileFromDir :: DirPath os ar -> Maybe (FilePath os ar)
fileFromDir = fileFromAny

toFileDir :: (Class.FileDir fd) => Path os ar fd -> FileDirPath os ar
toFileDir p = uncurry Path (pathComponents p) Part.FileDir

fromFileDir ::
    (Class.FileDir fd) => FileDirPath os ar -> Maybe (Path os ar fd)
fromFileDir p =
    switchFileDir
        (fileFromFileDir p)
        (Just $ dirFromFileDir p)
        (Just p)

fileFromFileDir :: FileDirPath os ar -> Maybe (FilePath os ar)
fileFromFileDir = fileFromAny

fileFromAny :: Path os ar fd -> Maybe (FilePath os ar)
fileFromAny (Path ar pcs _) =
    fmap (uncurry (Path ar) . mapSnd (Part.File . PC.untag)) $ ListHT.viewR pcs

dirFromFileDir :: FileDirPath os ar -> DirPath os ar
dirFromFileDir (Path ar pcs Part.FileDir) = Path ar pcs Part.Dir


toAbsRel :: (Class.AbsRel ar) => Path os ar fd -> AbsRelPath os fd
toAbsRel (Path ar pcs fd) = Path (Class.toAbsRel ar) pcs fd

fromAbsRel :: (Class.AbsRel ar) => AbsRelPath os fd -> Maybe (Path os ar fd)
fromAbsRel (Path ar0 pcs fd) = (\ar -> Path ar pcs fd) <$> Class.fromAbsRel ar0


------------------------------------------------------------------------
-- NYI - Not Yet Implemented

{-
splitSearchPath  :: String   -> [String]
getSearchPath    :: IO [String]
splitDrive       :: String   -> (String, String)
joinDrive        :: String   -> String -> String
takeDrive        :: String   -> String
hasDrive         :: String   -> Bool
dropDrive        :: String   -> String
isDrive          :: String   -> Bool
isValid          :: String   -> Bool
makeValid        :: String   -> String
-}

isDrive :: AbsDir os -> Bool
isDrive (Path _ pcs _) = null pcs


------------------------------------------------------------------------
-- Path Predicates

-- | Test whether a @'Path' ar fd@ is absolute.
--
-- prop> Path.isAbsolute (Posix.absFile "/fred")
-- prop> Path.isAbsolute (Windows.absFile "\\fred")
-- prop> Path.isAbsolute (Windows.absFile "c:\\fred")
-- prop> Path.isAbsolute (Windows.absFile "c:fred")
isAbsolute :: Class.AbsRel ar => Path os ar fd -> Bool
isAbsolute = withAbsRel (const True) (const False)

-- | Invariant - this should return True iff arg is of type @'Path' Part.Rel _@
--
-- > isRelative = not . isAbsolute
-- prop> Path.isRelative (Posix.relFile "fred")
-- prop> Path.isRelative (Windows.relFile "fred")
isRelative :: Class.AbsRel ar => Path os ar fd -> Bool
isRelative = not . isAbsolute


{- |
Test whether the 'String' would correspond
to an absolute path if interpreted as a 'Path'.
-}
isAbsoluteString :: (System os) => Tagged os (String -> Bool)
isAbsoluteString =
    fmap (\split -> not . null . MS.evalState split) splitAbsolute

{- |
Test whether the 'String' would correspond
to a relative path if interpreted as a 'Path'.

> isRelativeString = not . isAbsoluteString
-}
isRelativeString :: (System os) => Tagged os (String -> Bool)
isRelativeString = (not .) <$> isAbsoluteString


-- | Does the given filename have an extension?
--
-- prop> forAllAbsRel $ \x -> null (Path.takeExtension x) == not (Path.hasAnExtension x)
hasAnExtension :: FilePath os ar -> Bool
hasAnExtension = not . null . snd . splitExtension

-- | Does the given filename have the given extension?
--
-- prop> Path.hasExtension ".hs" (Posix.relFile "MyCode.hs")
-- prop> Path.hasExtension ".hs" (Posix.relFile "MyCode.bak.hs")
-- prop> not $ Path.hasExtension ".hs" (Posix.relFile "MyCode.hs.bak")
hasExtension :: String -> FilePath os ar -> Bool
hasExtension ext = (==ext) . snd . splitExtension


------------------------------------------------------------------------
-- Separators

-- | Part.File extension character
--
-- prop> Posix.extSeparator == '.'
extSeparator :: Char
extSeparator = Sep.extension

-- | The character that is used to separate the entries in the $PATH environment variable.
--
searchPathSeparator :: Char
searchPathSeparator = Sep.searchPath

-- | Is the character an extension character?
--
-- prop> \a -> Posix.isExtSeparator a == (a == Posix.extSeparator)
isExtSeparator :: Char -> Bool
isExtSeparator = Sep.isExtension

-- | Is the character a file separator?
--
-- prop> \a -> Posix.isSearchPathSeparator a == (a == Posix.searchPathSeparator)
isSearchPathSeparator :: Char -> Bool
isSearchPathSeparator = Sep.isSearchPath


------------------------------------------------------------------------
-- Generic Manipulation Functions

-- These functions support manipulation of extensions on directories
-- as well as files. They have looser types than the corresponding
-- 'Basic Manipulation Functions', but it is expected that the basic
-- functions will be used more frequently as they provide more checks.

-- | This is a more flexible variant of 'addExtension' / '<.>' which can
--   work with files or directories
--
-- prop> Path.genericAddExtension (absDir "/") "x" == Posix.absDir "/.x"
-- prop> Path.genericAddExtension (absDir "/a") "x" == Posix.absDir "/a.x"
-- prop> Path.genericAddExtension Path.emptyFile "x" == Posix.relFile ".x"
-- prop> Path.genericAddExtension Path.emptyFile "" == Posix.emptyFile
genericAddExtension ::
    (Class.FileDir fd) => Path os ar fd -> String -> Path os ar fd
genericAddExtension =
    flip $ \ext ->
        appEndo $ MonHT.when (not $ null ext) $
        switchFileDir
            (Endo $ flip addExtension ext)
            (Endo $ componentsAddExtension ext)
            (Endo $ componentsAddExtension ext)

componentsAddExtension :: String -> Path os ar fd -> Path os ar fd
componentsAddExtension ext (Path ar pcs0 fd) =
    let pcs = if null pcs0 then [PC.empty] else pcs0
    in  Path ar (mapLast (flip PC.addExtension ext) pcs) fd

genericDropExtension :: (Class.FileDir fd) => Path os ar fd -> Path os ar fd
genericDropExtension = fst . genericSplitExtension

genericDropExtensions :: (Class.FileDir fd) => Path os ar fd -> Path os ar fd
genericDropExtensions = fst . genericSplitExtensions

genericSplitExtension ::
    (Class.FileDir fd) => Path os ar fd -> (Path os ar fd, String)
genericSplitExtension =
    runSplitExtension $
    switchFileDir
        (SplitExtension splitExtension)
        (SplitExtension componentsSplitExtension)
        (SplitExtension componentsSplitExtension)

componentsSplitExtension :: Path os ar b -> (Path os ar b, String)
componentsSplitExtension (Path ar pcs fd) =
    mapFst (flip (Path ar) fd) $
    mapLastPair
        (error "genericSplitExtension: empty path")
        PC.splitExtension pcs

genericSplitExtensions ::
    (Class.FileDir fd) => Path os ar fd -> (Path os ar fd, String)
genericSplitExtensions =
    runSplitExtension $
    switchFileDir
        (SplitExtension splitExtensions)
        (SplitExtension componentsSplitExtensions)
        (SplitExtension componentsSplitExtensions)

componentsSplitExtensions :: Path os ar b -> (Path os ar b, String)
componentsSplitExtensions (Path ar pcs fd) =
    mapFst (flip (Path ar) fd) $
    mapLastPair
        (error "genericSplitExtensions: empty path")
        PC.splitExtensions pcs

genericTakeExtension :: (Class.FileDir fd) => Path os ar fd -> String
genericTakeExtension = snd . genericSplitExtension

genericTakeExtensions :: (Class.FileDir fd) => Path os ar fd -> String
genericTakeExtensions = snd . genericSplitExtension

newtype
    SplitExtension path =
        SplitExtension {runSplitExtension :: path -> (path, String)}


-- move to utility-ht
mapLast :: (a -> a) -> [a] -> [a]
mapLast f xs = zipWith id (drop 1 $ map (const id) xs ++ [f]) xs

mapLastPair :: b -> (a -> (a,b)) -> [a] -> ([a], b)
mapLastPair b f =
    ListHT.switchR ([], b) (\as a -> mapFst ((as++) . (:[])) $ f a)

mapLastPairFoldr :: b -> (a -> (a,b)) -> [a] -> ([a], b)
mapLastPairFoldr b _ [] = ([], b)
mapLastPairFoldr _ f (x:xs) =
    foldr
        (\y1 go y0 -> mapFst (y0:) $ go y1)
        (\y -> mapFst (:[]) $ f y)
        xs x

mapLastPairRec :: b -> (a -> (a,b)) -> [a] -> ([a], b)
mapLastPairRec b _ [] = ([], b)
mapLastPairRec _ f (x:xs) =
    let go y [] = mapFst (:[]) $ f y
        go y0 (y1:ys) = mapFst (y0:) $ go y1 ys
    in  go x xs

mapLastPairRev :: b -> (a -> (a,b)) -> [a] -> ([a], b)
mapLastPairRev b0 f xs =
    case reverse xs of
        [] -> (xs, b0)
        y:ys ->
            let (a, b) = f y
            in  (reverse ys ++ [a], b)

_prop_mapLastPair :: String -> Int -> [String] -> Bool
_prop_mapLastPair b n strs =
    let f = splitAt n
    in  all (mapLastPair b f strs ==) $
            mapLastPairFoldr b f strs :
            mapLastPairRev b f strs :
            mapLastPairRec b f strs :
            []



{- |
Check internal integrity of the path data structure.
-}
isValid ::
    (System os, Class.AbsRel ar, Class.FileDir fd) =>
    Path os ar fd -> Bool
isValid = untag isValidTagged

isValidTagged ::
    (System os, Class.AbsRel ar, Class.FileDir fd) =>
    Tagged os (Path os ar fd -> Bool)
isValidTagged =
    fmap
        (\isValidPC (Path ar pcs fd) ->
            Class.withAbsRel isValidComponent True ar
            &&
            all isValidPC pcs
            &&
            Class.withFileDir (isValidPC . PC.retag) True True fd)
        isValidPathComponent

isValidComponent :: String -> Bool
isValidComponent = not . null

isValidPathComponent ::
    (System os) => Tagged os (Component os -> Bool)
isValidPathComponent =
    fmap
        (\isSep (Component str) ->
            isValidComponent str  &&  not (any isSep str))
        isPathSeparator

------------------------------------------------------------------------
-- QuickCheck

testAll :: (System os) => os -> [(String, DocTest.T ())]
testAll os =
    ("mkPathFromComponents_pathComponents",
        quickCheck os prop_mkPathFromComponents_pathComponents) :
    ("splitDir_combine",
        quickCheck os prop_splitDir_combine) :
    ("takeDirName_end",
        quickCheck os prop_takeDirName_end) :
    []

quickCheck ::
    (QC.Testable prop, System os, Class.FileDir fd, Class.AbsRel ar) =>
    os -> (Path os ar fd -> prop) -> DocTest.T ()
quickCheck _ = DocTest.property

-- test :: Testable a => a -> IO ()
-- test = quickCheck

qcFileComponent :: Gen (Component os)
qcFileComponent = Component <$> frequency [
                    (1, return "someFile"),
                    (1, return "fileWith.ext"),
                    (1, return "file.with.multiple.exts"),
                    (1, return "file with spcs")
                  ]

qcDirComponent :: Gen (Component os)
qcDirComponent = Component <$> frequency [
                    (1, return "someDir"),
                    (1, return "aDir"),
                    (1, return "aFolder"),
                    (1, return "a folder"),
                    (1, return "directory")
                  ]


qcAbsRel :: (System os, Class.AbsRel ar) => Tagged os (Gen ar)
qcAbsRel =
    flip fmap genDrive $ \drive ->
        Class.switchAbsRel (fmap absPC drive) (return Part.Rel)
            (QC.oneof
                [fmap (Part.AbsO . Component) drive, return Part.RelO])

qcGenPath ::
    Tagged os (Gen ar) ->
    (Gen ar -> Gen (Path os ar fd)) ->
    Gen (Path os ar fd)
qcGenPath qcAR gen = gen $ untag qcAR

qcFilePath :: (System os, Class.AbsRel ar) => Gen (FilePath os ar)
qcFilePath = qcGenPath qcAbsRel $ \qcAR -> do
    ar <- qcAR
    pcs <- QC.listOf qcDirComponent
    pc <- qcFileComponent
    return $ Path ar pcs $ Part.File pc

qcDirPath :: (System os, Class.AbsRel ar) => fd -> Gen (Path os ar fd)
qcDirPath fd = qcGenPath qcAbsRel $ \qcAR -> do
    ar <- qcAR
    pcs <- QC.listOf qcDirComponent
    return $ Path ar pcs fd

qcPath ::
    (System os, Class.AbsRel ar, Class.FileDir fd) => Gen (Path os ar fd)
qcPath =
    switchFileDir qcFilePath (qcDirPath Part.Dir) (qcDirPath Part.FileDir)

instance
    (System os, Class.AbsRel ar, Class.FileDir fd) =>
        Arbitrary (Path os ar fd) where
    arbitrary = qcPath
