{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Aihc.Haddock.Package
-- Description : Load a package's library modules into the documentation model
--
-- The package loader shares source input and preprocessing with the compiler.
-- It reads hidden modules and dependencies before it requests export resolution.
module Aihc.Haddock.Package
  ( documentationHeaderTarget,
    loadPackageDoc,
    packageSpecOf,
  )
where

import Aihc.Hackage.Cabal qualified as HackageCabal
import Aihc.Hackage.Headers (HeaderTarget (..))
import Aihc.Hackage.Package (parsePackageDescription)
import Aihc.Hackage.Types (PackageSpec (..))
import Aihc.Hackage.Util qualified as HackageUtil
import Aihc.Haddock.Build (BuildInput (..), buildModuleDoc)
import Aihc.Haddock.Model
import Aihc.Haddock.Resolve (resolveDocumentation)
import Aihc.PackagePlan (dependencyVersionsFromManifests, packageSpecFromSource)
import Aihc.PackagePlan.Diagnostic (renderHumanDiagnostic)
import Aihc.PackagePlan.Source (ParsedInterfaceFile (..), parseInterfaceFile)
import Aihc.Parser.Syntax (moduleName)
import Aihc.Resolve (ModuleUnit (..), Package (..), PackageId (..))
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
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

-- | Document the package library with its direct dependency artifacts.
-- Dependencies supply resolver interfaces, declarations, and versions for CPP macros.
loadPackageDoc :: FilePath -> FilePath -> [PackageDoc] -> IO PackageDoc
loadPackageDoc headerDir root dependencies = do
  cabalFiles <- HackageUtil.findCabalFiles root
  cabalFile <-
    case cabalFiles of
      [] -> ioError (userError ("No .cabal file found under " <> root))
      files -> pure (HackageUtil.chooseBestCabalFile root files)
  cabalBytes <- BS.readFile cabalFile
  gpd <-
    case parsePackageDescription cabalBytes of
      Right value -> pure value
      Left errors -> ioError (userError ("Failed to parse " <> cabalFile <> ": " <> errors))
  spec <- packageSpecFromSource root
  files <- HackageCabal.collectLibraryFiles gpd root
  let exposed = HackageCabal.collectLibraryExposedModules gpd
      versions = dependencyVersionsFromManifests [(packageDocName dep, packageDocVersion dep) | dep <- dependencies]
      package = Package (T.pack (pkgName spec)) (PackageId (T.pack (pkgName spec <> "-" <> pkgVersion spec)))
  parsed <- mapM (parseInterfaceFile headerDir root versions) files
  let modules = resolveDocumentation package dependencies [(ModuleUnit package (parsedFileExtensions file) (parsedFileModule file), buildParsedModule root exposed file) | file <- parsed]
  pure
    PackageDoc
      { packageDocFormatVersion = docModelFormatVersion,
        packageDocName = T.pack (pkgName spec),
        packageDocVersion = T.pack (pkgVersion spec),
        packageDocDependencies = [packageDocName dep <> "-" <> packageDocVersion dep | dep <- dependencies],
        packageDocModules = modules
      }

buildParsedModule :: FilePath -> [Text] -> ParsedInterfaceFile -> ModuleDoc
buildParsedModule
  root
  exposed
  ParsedInterfaceFile
    { parsedFilePath = path,
      parsedFileModule = modu,
      parsedFileParseDiagnostics = parseDiagnostics,
      parsedFileCppDiagnostics = cppDiagnostics,
      parsedFileExtensions = extensions,
      parsedFileSource = source
    } =
    let relative = normalise (makeRelative root path)
        fallbackName = T.intercalate "." (map T.pack (splitDirectories (dropExtension relative)))
        name = fromMaybe fallbackName (moduleName modu)
        diagnostics =
          map (T.pack . renderHumanDiagnostic "parse") parseDiagnostics
            <> map (T.pack . renderHumanDiagnostic "cpp") cppDiagnostics
     in buildModuleDoc
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
