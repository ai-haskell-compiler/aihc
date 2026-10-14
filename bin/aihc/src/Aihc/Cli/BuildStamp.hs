{-# LANGUAGE OverloadedStrings #-}

-- | Records of what a build directory holds, so that the next build can
-- reuse an artifact without reading it.
--
-- A local package builds in place. Every unit of the package writes a stamp
-- beside its artifacts: the digests of the inputs it was built from, the
-- digests of the artifacts it wrote, and the size and modification time of
-- each artifact file. A later build compares the recorded inputs with the
-- digests it has in hand for the same unit, checks that the artifact files
-- are the ones the stamp describes, and reuses them on a match. Nothing is
-- hashed for the comparison: source digests come from parsing, and artifact
-- digests were taken from the bytes as they were written.
--
-- A package writes one 'PackageDigests' record next to its manifest so that a
-- consumer learns the digests of its interfaces without encoding them again.
module Aihc.Cli.BuildStamp
  ( FileStamp (..),
    ResolveStamp (..),
    UnitStamp (..),
    BackendStamp (..),
    PackageDigests (..),
    ModuleDigests (..),
    packageDigestsPath,
    readStamp,
    writeStamp,
    stampFiles,
    filesMatchStamps,
  )
where

import Aihc.Cbor (Cbor (..), cborArray, cborText, decodeCbor, encodeCbor, expectArray, getText, (<*!>))
import Control.Exception (IOException, try)
import Control.Monad ((<$!>))
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, doesFileExist, getFileSize, getModificationTime, renameFile)
import System.FilePath (takeDirectory, (</>))

-- | The identity of an artifact file as the stamp saw it.
data FileStamp = FileStamp
  { fileStampPath :: !FilePath,
    fileStampSize :: !Integer,
    fileStampModified :: !String
  }
  deriving (Eq, Show)

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
    -- | The headers the capi wrappers of the unit included, by absolute path.
    -- Nothing else in the build reads these files, so without them a changed
    -- header would leave a wrapper that calls the old C API in place.
    backendStampHeaders :: ![FileStamp]
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
    cborArray 4
      <> cborText "aihc-filestamp-1"
      <> toCbor (fileStampPath value)
      <> toCbor (fileStampSize value)
      <> toCbor (fileStampModified value)
  fromCbor = do
    expectArray 4
    "aihc-filestamp-1" <- getText
    FileStamp
      <$!> fromCbor
        <*!> fromCbor
        <*!> fromCbor

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
      <> toCbor (backendStampHeaders value)
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
-- describes artifacts that are not there.
writeStamp :: (Cbor stamp) => FilePath -> stamp -> IO ()
writeStamp path stamp = do
  createDirectoryIfMissing True (takeDirectory path)
  let temporary = path <> ".tmp"
  BL.writeFile temporary (encodeCbor stamp)
  renameFile temporary path

-- | Record the artifact files of a unit as they are now.
stampFiles :: FilePath -> [FilePath] -> IO [FileStamp]
stampFiles root = mapM stampFile
  where
    stampFile path = do
      size <- getFileSize (root </> path)
      modified <- getModificationTime (root </> path)
      pure (FileStamp path size (show modified))

-- | Whether every artifact file is the one the stamp recorded. A rewritten
-- file has a new modification time, so a build that stopped between writing
-- an artifact and its stamp does not pass off the new artifact as the old.
filesMatchStamps :: FilePath -> [FileStamp] -> IO Bool
filesMatchStamps root = fmap and . mapM matches
  where
    matches stamp = do
      let path = root </> fileStampPath stamp
      exists <- doesFileExist path
      if not exists
        then pure False
        else do
          size <- getFileSize path
          modified <- getModificationTime path
          pure (size == fileStampSize stamp && show modified == fileStampModified stamp)
