{-# LANGUAGE OverloadedStrings #-}

module Aihc.Cli.PackageManifest
  ( PackageManifest (..),
    packageManifestPath,
    readPackageManifest,
    writePackageManifest,
  )
where

import Control.DeepSeq (NFData)
import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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
    -- | The cabal flags the dependency plan decided for the package. Every
    -- other flag is at its default.
    packageManifestCabalFlags :: !(Map Text Bool)
  }
  deriving (Eq, Show, Generic)

instance NFData PackageManifest

instance Aeson.ToJSON PackageManifest where
  toJSON manifest =
    Aeson.object
      [ "schemaVersion" .= (7 :: Int),
        "name" .= packageManifestName manifest,
        "version" .= packageManifestVersion manifest,
        "identity" .= packageManifestIdentity manifest,
        "unitId" .= packageManifestUnitId manifest,
        "dependencies" .= packageManifestDependencies manifest,
        "modules" .= packageManifestModules manifest,
        "compiledModules" .= packageManifestCompiledModules manifest,
        "flags" .= packageManifestFlags manifest,
        "cxxStdLib" .= packageManifestCxxStdLib manifest,
        "cabalFlags" .= packageManifestCabalFlags manifest
      ]

instance Aeson.FromJSON PackageManifest where
  parseJSON = Aeson.withObject "PackageManifest" $ \object -> do
    schemaVersion <- object .: "schemaVersion"
    case schemaVersion :: Int of
      2 -> do
        name <- object .: "name"
        version <- object .: "version"
        identity <- object .: "identity"
        PackageManifest name version identity identity
          <$> object .: "dependencies"
          <*> object .: "modules"
          <*> pure []
          <*> pure []
          <*> pure False
          <*> pure Map.empty
      3 ->
        PackageManifest
          <$> object .: "name"
          <*> object .: "version"
          <*> object .: "identity"
          <*> object .: "unitId"
          <*> object .: "dependencies"
          <*> object .: "modules"
          <*> pure []
          <*> pure []
          <*> pure False
          <*> pure Map.empty
      4 ->
        PackageManifest
          <$> object .: "name"
          <*> object .: "version"
          <*> object .: "identity"
          <*> object .: "unitId"
          <*> object .: "dependencies"
          <*> object .: "modules"
          <*> pure []
          <*> object .: "flags"
          <*> pure False
          <*> pure Map.empty
      5 ->
        PackageManifest
          <$> object .: "name"
          <*> object .: "version"
          <*> object .: "identity"
          <*> object .: "unitId"
          <*> object .: "dependencies"
          <*> object .: "modules"
          <*> object .: "compiledModules"
          <*> object .: "flags"
          <*> pure False
          <*> pure Map.empty
      6 ->
        PackageManifest
          <$> object .: "name"
          <*> object .: "version"
          <*> object .: "identity"
          <*> object .: "unitId"
          <*> object .: "dependencies"
          <*> object .: "modules"
          <*> object .: "compiledModules"
          <*> object .: "flags"
          <*> object .: "cxxStdLib"
          <*> pure Map.empty
      7 ->
        PackageManifest
          <$> object .: "name"
          <*> object .: "version"
          <*> object .: "identity"
          <*> object .: "unitId"
          <*> object .: "dependencies"
          <*> object .: "modules"
          <*> object .: "compiledModules"
          <*> object .: "flags"
          <*> object .: "cxxStdLib"
          <*> object .: "cabalFlags"
      _ -> fail "unsupported package manifest schema"

packageManifestPath :: FilePath -> FilePath
packageManifestPath packageRoot = packageRoot </> "package.json"

readPackageManifest :: FilePath -> IO (Either String PackageManifest)
readPackageManifest path = Aeson.eitherDecode <$> BL.readFile path

writePackageManifest :: FilePath -> PackageManifest -> IO ()
writePackageManifest path = BL.writeFile path . Aeson.encode
