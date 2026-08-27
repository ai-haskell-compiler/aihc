{-# LANGUAGE OverloadedStrings #-}

module Aihc.Cli.PackageManifest
  ( PackageManifest (..),
    packageManifestPath,
    readPackageManifest,
    writePackageManifest,
  )
where

import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import System.FilePath ((</>))

data PackageManifest = PackageManifest
  { packageManifestName :: !Text,
    packageManifestVersion :: !Text,
    packageManifestIdentity :: !Text,
    packageManifestDependencies :: ![Text],
    packageManifestModules :: ![Text]
  }
  deriving (Eq, Show)

instance Aeson.ToJSON PackageManifest where
  toJSON manifest =
    Aeson.object
      [ "schemaVersion" .= (2 :: Int),
        "name" .= packageManifestName manifest,
        "version" .= packageManifestVersion manifest,
        "identity" .= packageManifestIdentity manifest,
        "dependencies" .= packageManifestDependencies manifest,
        "modules" .= packageManifestModules manifest
      ]

instance Aeson.FromJSON PackageManifest where
  parseJSON = Aeson.withObject "PackageManifest" $ \object -> do
    schemaVersion <- object .: "schemaVersion"
    name <- object .: "name"
    version <- object .: "version"
    identity <- object .: "identity"
    dependencies <- object .: "dependencies"
    modules <-
      case schemaVersion :: Int of
        1 -> pure []
        2 -> object .: "modules"
        _ -> fail "unsupported package manifest schema"
    pure
      PackageManifest
        { packageManifestName = name,
          packageManifestVersion = version,
          packageManifestIdentity = identity,
          packageManifestDependencies = dependencies,
          packageManifestModules = modules
        }

packageManifestPath :: FilePath -> FilePath
packageManifestPath packageRoot = packageRoot </> "package.json"

readPackageManifest :: FilePath -> IO (Either String PackageManifest)
readPackageManifest path = Aeson.eitherDecode <$> BL.readFile path

writePackageManifest :: FilePath -> PackageManifest -> IO ()
writePackageManifest path = BL.writeFile path . Aeson.encode
