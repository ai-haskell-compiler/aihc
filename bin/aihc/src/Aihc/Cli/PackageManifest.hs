{-# LANGUAGE OverloadedStrings #-}

module Aihc.Cli.PackageManifest
  ( PackageManifest (..),
    packageManifestPath,
    readPackageManifest,
    writePackageManifest,
  )
where

import Aihc.Cbor (Cbor (..), cborArray, cborText, decodeCbor, encodeCbor, expectArray, getText, (<*!>))
import Control.DeepSeq (NFData)
import Control.Monad ((<$!>))
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.FilePath ((</>))

data PackageManifest = PackageManifest
  { packageManifestName :: !Text,
    packageManifestVersion :: !Text,
    packageManifestIdentity :: !Text,
    packageManifestUnitId :: !Text,
    packageManifestDependencies :: ![Text],
    packageManifestModules :: ![Text],
    -- | Each reexport names its original package unit and module.
    packageManifestReexports :: !(Map Text (Text, Text)),
    -- | Every module the package compiled, exposed or hidden, in the order
    -- the store holds them. A @--lto@ build reads the System FC of each.
    packageManifestCompiledModules :: ![Text],
    -- | The install flags the package was built with, such as @keep-core@
    -- or @no-code@. A store entry never changes, so an install that asks
    -- for an output the entry lacks must rebuild it.
    packageManifestFlags :: ![Text],
    -- | The package has @cxx-sources@, so its objects need the C++
    -- standard library. An executable that links the package links it.
    packageManifestCxxStdLib :: !Bool,
    -- | The arguments that link the system libraries of the package, from
    -- its @extra-libraries@, @extra-lib-dirs@, @frameworks@, and
    -- @ld-options@. An executable that links the package gives them to
    -- the linker.
    packageManifestLinkArguments :: ![Text],
    -- | The cabal flags the dependency plan decided for the package. Every
    -- other flag is at its default.
    packageManifestCabalFlags :: !(Map Text Bool)
  }
  deriving (Eq, Show, Generic)

instance NFData PackageManifest

instance Cbor PackageManifest where
  toCbor value =
    cborArray 13
      <> cborText "aihc-packagemanifest-2"
      <> toCbor (packageManifestName value)
      <> toCbor (packageManifestVersion value)
      <> toCbor (packageManifestIdentity value)
      <> toCbor (packageManifestUnitId value)
      <> toCbor (packageManifestDependencies value)
      <> toCbor (packageManifestModules value)
      <> toCbor (packageManifestReexports value)
      <> toCbor (packageManifestCompiledModules value)
      <> toCbor (packageManifestFlags value)
      <> toCbor (packageManifestCxxStdLib value)
      <> toCbor (packageManifestLinkArguments value)
      <> toCbor (packageManifestCabalFlags value)
  fromCbor = do
    expectArray 13
    "aihc-packagemanifest-2" <- getText
    PackageManifest
      <$!> fromCbor
        <*!> fromCbor
        <*!> fromCbor
        <*!> fromCbor
        <*!> fromCbor
        <*!> fromCbor
        <*!> fromCbor
        <*!> fromCbor
        <*!> fromCbor
        <*!> fromCbor
        <*!> fromCbor
        <*!> fromCbor

packageManifestPath :: FilePath -> FilePath
packageManifestPath packageRoot = packageRoot </> "package.cbor"

readPackageManifest :: FilePath -> IO (Either String PackageManifest)
readPackageManifest path = decodeCbor <$> BL.readFile path

writePackageManifest :: FilePath -> PackageManifest -> IO ()
writePackageManifest path = BL.writeFile path . encodeCbor
