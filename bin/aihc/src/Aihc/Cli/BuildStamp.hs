{-# LANGUAGE OverloadedStrings #-}

-- | Records of what a build directory holds, so that the next build can
-- reuse an artifact without reading it.
--
-- A local package builds in place. Every unit of the package writes a stamp
-- beside its artifacts: the digests of the inputs it was built from, the
-- digests of the artifacts it wrote, and the size and content digest of
-- each artifact file. A later build compares the recorded inputs with the
-- digests it has in hand for the same unit, checks that the artifact files
-- are the ones the stamp describes, and reuses them on a match.
--
-- A stamp is a part of the store, and two builds of the same plan must
-- write the same bytes. Thus a stamp records no modification time and no
-- path that names the build directory, the store, or the package source.
-- A path is relative to one of those roots, see 'StampBase'. A local
-- package also writes a times file beside each stamp. The times file
-- records the modification time of each file when its digest was taken,
-- so that a later check reads a file again only when that file changed.
-- A store package writes no times file: nothing checks the stamps of a
-- store package, because the store never builds an entry again in place.
--
-- A package writes one 'PackageDigests' record next to its manifest so that a
-- consumer learns the digests of its interfaces without encoding them again.
module Aihc.Cli.BuildStamp
  ( FileStamp (..),
    StampBase (..),
    StampRoots (..),
    FileTime,
    newStampRoots,
    ResolveStamp (..),
    UnitStamp (..),
    BackendStamp (..),
    PackageDigests (..),
    ModuleDigests (..),
    packageDigestsPath,
    readStamp,
    writeStamp,
    writeRecord,
    stampFiles,
    stampExternalFiles,
    filesMatchStamps,
  )
where

import Aihc.Cbor (Cbor (..), cborArray, cborText, decodeCbor, encodeCbor, expectArray, getText, (<*!>))
import Control.Exception (IOException, evaluate, try)
import Control.Monad (forM, when, (<$!>))
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, atomicModifyIORef', readIORef)
import Data.List (isPrefixOf, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Numeric (showHex)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist, getFileSize, getModificationTime, renameFile)
import System.FilePath (joinPath, splitDirectories, takeDirectory, (<.>), (</>))

-- | The identity of an artifact file as the stamp saw it: where it is, its
-- size, and the SHA-256 digest of its bytes.
data FileStamp = FileStamp
  { fileStampBase :: !StampBase,
    -- | The path relative to the root of the base, or an absolute path
    -- for 'BaseAbsolute'.
    fileStampPath :: !FilePath,
    fileStampSize :: !Integer,
    fileStampDigest :: !Text
  }
  deriving (Eq, Show)

-- | The root that the path of a 'FileStamp' is relative to.
data StampBase
  = -- | The build directory of the package.
    BasePackage
  | -- | The directory that holds the build directory of the package. For a
    -- store package, it is the store root. For a local package, it is the
    -- build root, with the other local packages and the configure outputs.
    BaseSiblings
  | -- | The store root of the target.
    BaseStore
  | -- | The source directory of the package.
    BaseSource
  | -- | No root: a file outside every root, such as a system header.
    BaseAbsolute
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The roots of the paths in the stamps of one package, and what the
-- package build knows about the files it stamped.
data StampRoots = StampRoots
  { stampRootPackage :: !FilePath,
    stampRootSiblings :: !FilePath,
    -- | Directories that hold paths of 'BaseSiblings' too: the staging
    -- directory of a store package, whose dependencies move from there to
    -- the store root when the install ends.
    stampRootSiblingAliases :: ![FilePath],
    stampRootStore :: !FilePath,
    stampRootSource :: !FilePath,
    -- | Whether a stamp gets a times file beside it.
    stampRootsRecordTimes :: !Bool,
    -- | The size, the modification time, and the digest of each file that
    -- this process hashed, by absolute path. A header that many units
    -- include is then read only one time.
    stampRootsDigests :: !(IORef (Map.Map FilePath FileTime))
  }

-- | The size, the modification time, and the digest of a file.
data FileTime = FileTime !Integer !String !Text
  deriving (Eq, Show)

-- | The roots of a package build: the build directory, the directory of
-- the other packages and its aliases, the store root, and the source
-- directory. The roots are canonical paths, so that a canonical path of a
-- header gets the correct root.
newStampRoots :: FilePath -> FilePath -> [FilePath] -> FilePath -> FilePath -> Bool -> IORef (Map.Map FilePath FileTime) -> IO StampRoots
newStampRoots package siblings aliases store source recordTimes digests = do
  package' <- canonicalizePath package
  siblings' <- canonicalizePath siblings
  aliases' <- mapM canonicalizePath aliases
  store' <- canonicalizePath store
  source' <- canonicalizePath source
  pure
    StampRoots
      { stampRootPackage = package',
        stampRootSiblings = siblings',
        stampRootSiblingAliases = aliases',
        stampRootStore = store',
        stampRootSource = source',
        stampRootsRecordTimes = recordTimes,
        stampRootsDigests = digests
      }

stampRoot :: StampRoots -> StampBase -> FilePath
stampRoot roots base =
  case base of
    BasePackage -> stampRootPackage roots
    BaseSiblings -> stampRootSiblings roots
    BaseStore -> stampRootStore roots
    BaseSource -> stampRootSource roots
    BaseAbsolute -> ""

-- | The absolute path of a stamped file.
stampedPath :: StampRoots -> FileStamp -> FilePath
stampedPath roots stamp = stampRoot roots (fileStampBase stamp) </> fileStampPath stamp

-- | What the resolve phase of a unit was built from and produced.
data ResolveStamp = ResolveStamp
  { resolveStampInputs :: ![(Text, Text)],
    -- | The scope digest of each module of the unit.
    resolveStampScopes :: !(Map.Map Text Text),
    resolveStampFiles :: ![FileStamp]
  }
  deriving (Eq, Show)

-- | What the type-check and backend phases of a unit were built from and
-- produced.
data UnitStamp = UnitStamp
  { unitStampInputs :: ![(Text, Text)],
    -- | The interface digest of each module of the unit.
    unitStampTypes :: !(Map.Map Text Text),
    -- | The digest of the instance facts of the unit.
    unitStampFacts :: !Text,
    -- | The type artifacts and the facts artifact.
    unitStampFiles :: ![FileStamp],
    -- | The objects, or nothing when the unit has none.
    unitStampBackend :: !(Maybe BackendStamp)
  }
  deriving (Eq, Show)

-- | The backend outputs of a unit and the options they were built with.
data BackendStamp = BackendStamp
  { backendStampOptions :: !Text,
    backendStampFiles :: ![FileStamp],
    -- | The files outside the unit that its backend read, by path: the
    -- headers the capi wrappers included, and the optimizer interfaces of
    -- the modules the unit imports. Nothing else in the build reads these
    -- files, so without them a changed header would leave a wrapper that
    -- calls the old C API in place, and a changed interface would leave
    -- an object that holds the body of a value that is gone.
    backendStampExternalFiles :: ![FileStamp]
  }
  deriving (Eq, Show)

-- | The digests of the interfaces of one module of an installed package.
data ModuleDigests = ModuleDigests
  { moduleScopeDigest :: !Text,
    moduleTypeDigest :: !Text,
    -- | The digest of the instance facts of the unit of the module. It
    -- covers the facts of every unit below that unit, in the package and
    -- in the packages it depends on.
    moduleFactsDigest :: !Text,
    -- | The facts artifact of the unit of the module, relative to the
    -- package.
    moduleFactsArtifact :: !FilePath
  }
  deriving (Eq, Show)

-- | The digests of everything a consumer of an installed package reads.
newtype PackageDigests = PackageDigests
  { packageDigestsModules :: Map.Map Text ModuleDigests
  }
  deriving (Eq, Show)

instance Cbor FileStamp where
  toCbor value =
    cborArray 5
      <> cborText "aihc-filestamp-2"
      <> toCbor (fileStampBase value)
      <> toCbor (fileStampPath value)
      <> toCbor (fileStampSize value)
      <> toCbor (fileStampDigest value)
  fromCbor = do
    expectArray 5
    "aihc-filestamp-2" <- getText
    FileStamp
      <$!> fromCbor
        <*!> fromCbor
        <*!> fromCbor
        <*!> fromCbor

instance Cbor StampBase where
  toCbor base =
    cborText $
      case base of
        BasePackage -> "package"
        BaseSiblings -> "siblings"
        BaseStore -> "store"
        BaseSource -> "source"
        BaseAbsolute -> "absolute"
  fromCbor = do
    name <- getText
    case name of
      "package" -> pure BasePackage
      "siblings" -> pure BaseSiblings
      "store" -> pure BaseStore
      "source" -> pure BaseSource
      "absolute" -> pure BaseAbsolute
      _ -> fail "unexpected stamp base"

-- | One entry of a times file.
data TimeEntry = TimeEntry !StampBase !FilePath !FileTime

instance Cbor TimeEntry where
  toCbor (TimeEntry base path (FileTime size modified digest)) =
    cborArray 5 <> toCbor base <> toCbor path <> toCbor size <> toCbor modified <> toCbor digest
  fromCbor = do
    expectArray 5
    base <- fromCbor
    path <- fromCbor
    size <- fromCbor
    modified <- fromCbor
    digest <- fromCbor
    pure (TimeEntry base path (FileTime size modified digest))

instance Cbor ResolveStamp where
  toCbor value =
    cborArray 4
      <> cborText "aihc-resolvestamp-1"
      <> toCbor (resolveStampInputs value)
      <> toCbor (resolveStampScopes value)
      <> toCbor (resolveStampFiles value)
  fromCbor = do
    expectArray 4
    "aihc-resolvestamp-1" <- getText
    ResolveStamp
      <$!> fromCbor
        <*!> fromCbor
        <*!> fromCbor

instance Cbor BackendStamp where
  toCbor value =
    cborArray 4
      <> cborText "aihc-backendstamp-1"
      <> toCbor (backendStampOptions value)
      <> toCbor (backendStampFiles value)
      <> toCbor (backendStampExternalFiles value)
  fromCbor = do
    expectArray 4
    "aihc-backendstamp-1" <- getText
    BackendStamp
      <$!> fromCbor
        <*!> fromCbor
        <*!> fromCbor

instance Cbor UnitStamp where
  toCbor value =
    cborArray 6
      <> cborText "aihc-unitstamp-1"
      <> toCbor (unitStampInputs value)
      <> toCbor (unitStampTypes value)
      <> toCbor (unitStampFacts value)
      <> toCbor (unitStampFiles value)
      <> toCbor (unitStampBackend value)
  fromCbor = do
    expectArray 6
    "aihc-unitstamp-1" <- getText
    UnitStamp
      <$!> fromCbor
        <*!> fromCbor
        <*!> fromCbor
        <*!> fromCbor
        <*!> fromCbor

instance Cbor ModuleDigests where
  toCbor value =
    cborArray 5
      <> cborText "aihc-moduledigests-1"
      <> toCbor (moduleScopeDigest value)
      <> toCbor (moduleTypeDigest value)
      <> toCbor (moduleFactsDigest value)
      <> toCbor (moduleFactsArtifact value)
  fromCbor = do
    expectArray 5
    "aihc-moduledigests-1" <- getText
    ModuleDigests
      <$!> fromCbor
        <*!> fromCbor
        <*!> fromCbor
        <*!> fromCbor

instance Cbor PackageDigests where
  toCbor value =
    cborArray 2
      <> cborText "aihc-packagedigests-1"
      <> toCbor (packageDigestsModules value)
  fromCbor = do
    expectArray 2
    "aihc-packagedigests-1" <- getText
    PackageDigests <$!> fromCbor

packageDigestsPath :: FilePath -> FilePath
packageDigestsPath packageRoot = packageRoot </> "digests.cbor"

-- | Read a stamp. An absent or unreadable stamp is no stamp: the artifacts
-- it would describe are rebuilt.
readStamp :: (Cbor stamp) => FilePath -> IO (Maybe stamp)
readStamp path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      result <- try (BL.readFile path) :: IO (Either IOException BL.ByteString)
      pure (either (const Nothing) (either (const Nothing) Just . decodeCbor) result)

-- | Write a stamp after the artifacts it describes, so that a stamp never
-- describes artifacts that are not there. The files are the file stamps in
-- the stamp. For a local package, their times go into the times file.
writeStamp :: (Cbor stamp) => StampRoots -> FilePath -> stamp -> [FileStamp] -> IO ()
writeStamp roots path stamp files = do
  createDirectoryIfMissing True (takeDirectory path)
  writeAtomic path (encodeCbor stamp)
  when (stampRootsRecordTimes roots) $ do
    known <- readIORef (stampRootsDigests roots)
    writeTimes
      path
      [ TimeEntry (fileStampBase file) (fileStampPath file) time
      | file <- files,
        Just time@(FileTime size _ digest) <- [Map.lookup (stampedPath roots file) known],
        size == fileStampSize file,
        digest == fileStampDigest file
      ]

-- | Write a record that describes no files, such as 'PackageDigests'.
writeRecord :: (Cbor record) => FilePath -> record -> IO ()
writeRecord path record = do
  createDirectoryIfMissing True (takeDirectory path)
  writeAtomic path (encodeCbor record)

-- | Write a whole file and then rename it, so a reader never sees a part.
writeAtomic :: FilePath -> BL.ByteString -> IO ()
writeAtomic path bytes = do
  let temporary = path <> ".tmp"
  BL.writeFile temporary bytes
  renameFile temporary path

timesPath :: FilePath -> FilePath
timesPath stampPath = stampPath <.> "times"

writeTimes :: FilePath -> [TimeEntry] -> IO ()
writeTimes stampPath entries = writeAtomic (timesPath stampPath) (encodeCbor entries)

readTimes :: FilePath -> IO (Map.Map (StampBase, FilePath) FileTime)
readTimes stampPath = do
  entries <- readStamp (timesPath stampPath)
  pure (Map.fromList [((base, path), time) | TimeEntry base path time <- concat (entries :: Maybe [TimeEntry])])

-- | Record the artifact files of a unit as they are now. The paths are
-- relative to the build directory of the package.
stampFiles :: StampRoots -> [FilePath] -> IO [FileStamp]
stampFiles roots = mapM (stampFile roots BasePackage)

-- | Record files outside the unit, such as headers and the interfaces of
-- other packages. Each path is absolute, and the stamp records it relative
-- to the most specific root that holds it.
stampExternalFiles :: StampRoots -> [FilePath] -> IO [FileStamp]
stampExternalFiles roots paths = forM paths $ \path -> do
  canonical <- canonicalizePath path
  let (base, relative) = relativeToRoots roots canonical
  stampFile roots base relative

-- | The base and the relative path of a canonical absolute path. The
-- longest root that holds the path wins, so that a build directory inside
-- the source directory gives paths relative to the build directory.
relativeToRoots :: StampRoots -> FilePath -> (StampBase, FilePath)
relativeToRoots roots path =
  case sortOn (Down . length . fst) candidates of
    (_, (base, relative)) : _ -> (base, relative)
    [] -> (BaseAbsolute, path)
  where
    parts = splitDirectories path
    roots' =
      [(base, stampRoot roots base) | base <- [minBound .. maxBound], base /= BaseAbsolute]
        <> [(BaseSiblings, alias) | alias <- stampRootSiblingAliases roots]
    candidates =
      [ (rootParts, (base, joinPath (drop (length rootParts) parts)))
      | (base, root) <- roots',
        let rootParts = splitDirectories root,
        rootParts `isPrefixOf` parts,
        length rootParts < length parts
      ]

stampFile :: StampRoots -> StampBase -> FilePath -> IO FileStamp
stampFile roots base path = do
  let absolute = stampRoot roots base </> path
  FileTime size _ digest <- fileTime roots absolute
  pure (FileStamp base path size digest)

-- | The size, modification time and digest of a file as it is now. The
-- time is read before the bytes: a file that changes during the read gets
-- a new time, and the next check reads it again.
fileTime :: StampRoots -> FilePath -> IO FileTime
fileTime roots path = do
  size <- getFileSize path
  modified <- show <$> getModificationTime path
  known <- readIORef (stampRootsDigests roots)
  case Map.lookup path known of
    Just time@(FileTime knownSize knownModified _)
      | knownSize == size && knownModified == modified -> pure time
    _ -> do
      digest <- fileDigest path
      let time = FileTime size modified digest
      atomicModifyIORef' (stampRootsDigests roots) (\times -> (Map.insert path time times, ()))
      pure time

fileDigest :: FilePath -> IO Text
fileDigest path = do
  bytes <- BL.readFile path
  evaluate (T.pack (concatMap hex (BS.unpack (SHA256.hashlazy bytes))))
  where
    hex byte = let value = showHex byte "" in replicate (2 - length value) '0' <> value

-- | Whether every file is the one the stamp recorded. A file whose size
-- and modification time are those in the times file of the stamp has the
-- recorded digest. Any other file is read again. A file that a build
-- rewrote has a new digest, so a build that stopped between writing an
-- artifact and its stamp does not pass off the new artifact as the old.
filesMatchStamps :: StampRoots -> FilePath -> [FileStamp] -> IO Bool
filesMatchStamps roots stampPath stamps = do
  recorded <- readTimes stampPath
  results <- mapM (matches recorded) stamps
  let matched = all fst results
      refreshed = catMaybes (map snd results)
  -- A file that only got a new modification time, for example from a
  -- checkout, gets its new time, so the next check does not read it again.
  when (matched && stampRootsRecordTimes roots && not (null refreshed)) $ do
    let entries = foldr (\(TimeEntry base path time) -> Map.insert (base, path) time) recorded refreshed
    _ <- try (writeTimes stampPath [TimeEntry base path time | ((base, path), time) <- Map.toAscList entries]) :: IO (Either IOException ())
    pure ()
  pure matched
  where
    matches recorded stamp = do
      let path = stampedPath roots stamp
      exists <- doesFileExist path
      if not exists
        then pure (False, Nothing)
        else do
          size <- getFileSize path
          if size /= fileStampSize stamp
            then pure (False, Nothing)
            else do
              modified <- show <$> getModificationTime path
              case Map.lookup (fileStampBase stamp, fileStampPath stamp) recorded of
                Just (FileTime recordedSize recordedModified recordedDigest)
                  | recordedSize == size && recordedModified == modified && recordedDigest == fileStampDigest stamp ->
                      pure (True, Nothing)
                _ -> do
                  time@(FileTime _ _ digest) <- fileTime roots path
                  let same = digest == fileStampDigest stamp
                  pure (same, if same then Just (TimeEntry (fileStampBase stamp) (fileStampPath stamp) time) else Nothing)
