{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Aihc.Haddock.Package
-- Description : Load a package's library modules into the documentation model
--
-- Shares the package plan, cabal-file handling and source loading with the
-- compiler through @aihc-package-plan@ and @aihc-hackage@. Every library
-- module is loaded, exposed or not, so that later re-export resolution has
-- the hidden modules available.
module Aihc.Haddock.Package
  ( documentationHeaderTarget,
    loadPackageDoc,
    packageSpecOf,
  )
where

import Aihc.Hackage.Cabal qualified as HackageCabal
import Aihc.Hackage.Cpp (DependencyVersions)
import Aihc.Hackage.Headers (HeaderTarget (..))
import Aihc.Hackage.Types (PackageSpec (..))
import Aihc.Hackage.Util qualified as HackageUtil
import Aihc.Haddock.Build (BuildInput (..), buildModuleDoc)
import Aihc.Haddock.Model
import Aihc.PackagePlan (dependencyVersionsFromManifests, packageSpecFromSource)
import Aihc.PackagePlan.Diagnostic (renderHumanDiagnostic)
import Aihc.PackagePlan.Source (ParsedInterfaceFile (..), parseInterfaceFile)
import Aihc.Parser.Syntax (moduleName)
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import System.FilePath (dropExtension, makeRelative, normalise, splitDirectories)

packageSpecOf :: FilePath -> IO PackageSpec
packageSpecOf = packageSpecFromSource

-- | The machine that documentation describes.
--
-- A module reaches the CPP pass with @#if SIZEOF_VOID_P == 8@ and the like,
-- so the headers must answer for some machine.  This tool documents sources
-- and compiles none, and it takes no target from its caller, so it documents
-- for LP64, which every target of aihc except @wasm32@ matches.
documentationHeaderTarget :: HeaderTarget
documentationHeaderTarget =
  HeaderTarget
    { headerPointerBytes = 8,
      headerLongBytes = 8,
      headerBigEndian = False,
      headerOs = "linux",
      headerArch = "x86_64"
    }

-- | Document the library of the package at the given root. The dependency
-- specs supply the versions that @MIN_VERSION_*@ macros report during CPP,
-- and the header directory holds the headers of the compiler, which a module
-- may include.
loadPackageDoc :: FilePath -> FilePath -> [PackageSpec] -> IO PackageDoc
loadPackageDoc headerDir root dependencies = do
  cabalFiles <- HackageUtil.findCabalFiles root
  cabalFile <-
    case cabalFiles of
      [] -> ioError (userError ("No .cabal file found under " <> root))
      files -> pure (HackageUtil.chooseBestCabalFile root files)
  cabalBytes <- BS.readFile cabalFile
  gpd <-
    case runParseResult (parseGenericPackageDescription cabalBytes) of
      (_, Right value) -> pure value
      (_, Left (_, errors)) -> ioError (userError ("Failed to parse " <> cabalFile <> ": " <> show errors))
  spec <- packageSpecFromSource root
  files <- HackageCabal.collectLibraryFiles gpd root
  let exposed = HackageCabal.collectLibraryExposedModules gpd
      versions = dependencyVersionsFromManifests [(T.pack (pkgName dep), T.pack (pkgVersion dep)) | dep <- dependencies]
  modules <- mapM (loadModule headerDir root versions exposed) files
  pure
    PackageDoc
      { packageDocFormatVersion = docModelFormatVersion,
        packageDocName = T.pack (pkgName spec),
        packageDocVersion = T.pack (pkgVersion spec),
        packageDocDependencies = [T.pack (pkgName dep <> "-" <> pkgVersion dep) | dep <- dependencies],
        packageDocModules = modules
      }

loadModule :: FilePath -> FilePath -> DependencyVersions -> [Text] -> HackageCabal.FileInfo -> IO ModuleDoc
loadModule headerDir root versions exposed fileInfo = do
  ParsedInterfaceFile
    { parsedFilePath = path,
      parsedFileModule = modu,
      parsedFileParseDiagnostics = parseDiagnostics,
      parsedFileCppDiagnostics = cppDiagnostics,
      parsedFileExtensions = extensions,
      parsedFileSource = source
    } <-
    parseInterfaceFile headerDir root versions fileInfo
  let relative = normalise (makeRelative root path)
      fallbackName = T.intercalate "." (map T.pack (splitDirectories (dropExtension relative)))
      name = fromMaybe fallbackName (moduleName modu)
      diagnostics =
        map (T.pack . renderHumanDiagnostic "parse") parseDiagnostics
          <> map (T.pack . renderHumanDiagnostic "cpp") cppDiagnostics
  pure
    ( buildModuleDoc
        BuildInput
          { buildFile = path,
            buildRelativize = normalise . makeRelative root,
            buildModule = modu,
            buildSource = source,
            buildExtensions = extensions,
            buildExposed = name `elem` exposed,
            buildFallbackName = fallbackName,
            buildDiagnostics = map T.strip diagnostics
          }
    )
