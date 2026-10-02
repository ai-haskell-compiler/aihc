{-# LANGUAGE OverloadedStrings #-}

-- | The executable of a main module: the packages that the command line
-- names, and the modules that the main module reaches from the source
-- directories.
module Aihc.Cli.BuildModule
  ( mainModuleExecutable,
    plannedPackage,
    writeEntryModule,
  )
where

import Aihc.Cli.Install
  ( ExecutableComponent (..),
    InstalledPackage (..),
  )
import Aihc.Cli.Options (BuildOptions (..))
import Aihc.Cli.PackageManifest (PackageManifest (..))
import Aihc.Cli.Progress (ProgressItem (..))
import Aihc.Hackage.Cabal qualified as HackageCabal
import Aihc.Hackage.Package (PackageName, VersionRange, anyVersion, mkPackageName, parseDependencyString, unPackageName)
import Aihc.PackagePlan (PackagePlan, PlanRequest (..), PlannedPackages (..), canonicalPackageName, planPackages)
import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Syntax
  ( Extension (ImplicitPrelude),
    ImportDecl (..),
    LanguageEdition (Haskell98Edition),
    effectiveExtensions,
    headerExtensionSettings,
    headerLanguageEdition,
    moduleName,
  )
import Aihc.Parser.Syntax qualified as Syntax
import Aihc.Parser.Token (readModuleHeaderPragmas)
import Aihc.Resolve (Package (..), PackageId (..))
import Control.Monad (filterM, foldM, unless, when)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (dropExtension, takeDirectory, takeFileName, (</>))

data PackageConstraint = PackageConstraint
  { constraintName :: !Text,
    constraintRange :: !VersionRange
  }

data SourceModule = SourceModule
  { sourcePath :: !FilePath,
    sourceModuleName :: !Text,
    sourceDependencies :: ![SourceDependency]
  }

data SourceDependency = SourceDependency
  { sourceDependencyPackage :: !(Maybe Text),
    sourceDependencyModule :: !Text
  }
  deriving (Eq, Ord, Show)

data InstalledModule = InstalledModule
  { installedModulePackage :: !InstalledPackage,
    installedModuleName :: !Text
  }

type InstalledModuleIndex = Map.Map Text [InstalledModule]

-- | The executable of the main module that the options name, compiled in
-- the given directory. The packages the constraints name are planned
-- here. The modules are found when the packages below the executable are
-- known, because an import of an installed module ends the search.
mainModuleExecutable :: BuildOptions -> FilePath -> FilePath -> PlanRequest -> IO ExecutableComponent
mainModuleExecutable options sourceRoot outputRoot request = do
  constraints <- mapM parsePackageConstraint (buildPackageConstraints options)
  let goals =
        [ (canonicalPackageName (mkPackageName (T.unpack (constraintName constraint))), constraintRange constraint)
        | constraint <- constraints <> map implicitConstraint ["aihc-base", "aihc-prim"]
        ]
      sourceDirectories = case buildSourceDirectories options of [] -> ["."]; values -> values
  planned <- planPackages request {requestGoals = goals}
  plans <- mapM (plannedPackage planned . fst) goals
  pure
    ExecutableComponent
      { componentPackage = Package "exe" (PackageId "exe"),
        componentSourceRoot = sourceRoot,
        componentOutputRoot = outputRoot,
        componentDependencies = plans,
        -- The executable is named as its file is, with the extension of
        -- the main module removed.
        componentItem = ItemExecutable (T.pack (takeFileName (fromMaybe (dropExtension (buildInput options)) (buildOutput options)))),
        componentInputs = \packages -> do
          let moduleIndex = buildInstalledModuleIndex packages
          sources <- discoverSources sourceDirectories moduleIndex (buildInput options)
          validateInstalledDependencies moduleIndex sources
          files <- sourceFiles outputRoot (map (packageManifestName . installedManifest) packages) sources
          pure (files, noCCompileInfo)
      }

-- | A main module has no Cabal file, so it has no C inputs.
noCCompileInfo :: HackageCabal.CCompileInfo
noCCompileInfo = HackageCabal.CCompileInfo [] [] [] [] [] [] [] [] [] [] [] []

-- | The plan of one package, which the solver must have chosen.
plannedPackage :: PlannedPackages -> PackageName -> IO PackagePlan
plannedPackage planned name =
  maybe
    (ioError (userError ("The dependency plan has no package " <> unPackageName name)))
    pure
    (Map.lookup (canonicalPackageName name) (plannedPlans planned))

implicitConstraint :: Text -> PackageConstraint
implicitConstraint name = PackageConstraint name anyVersion

parsePackageConstraint :: String -> IO PackageConstraint
parsePackageConstraint input =
  case parseDependencyString input of
    Just (name, versionRange) -> pure (PackageConstraint (T.pack (unPackageName name)) versionRange)
    Nothing -> ioError (userError ("Invalid package constraint: " <> input))

buildInstalledModuleIndex :: [InstalledPackage] -> InstalledModuleIndex
buildInstalledModuleIndex packages =
  Map.fromListWith (<>) [(installedModuleName entry, [entry]) | entry <- entries]
  where
    entries =
      [ InstalledModule package name
      | package <- packages,
        name <- packageManifestModules (installedManifest package)
      ]

discoverSources :: [FilePath] -> InstalledModuleIndex -> FilePath -> IO [SourceModule]
discoverSources sourceDirectories moduleIndex mainPath = do
  mainSource <- parseSource mainPath
  unless (sourceModuleName mainSource == "Main") (ioError (userError ("The input file does not define module Main: " <> mainPath)))
  discovered <- visit Map.empty mainSource
  when (Map.member "Aihc.Entry" discovered) (ioError (userError "Source module conflicts with generated module Aihc.Entry"))
  entrySource <- parseSourceText entrySourceName generatedEntryText
  pure (Map.elems discovered <> [entrySource])
  where
    visit found source = do
      let name = sourceModuleName source
      case Map.lookup name found of
        Just previous
          | sourcePath previous == sourcePath source -> pure found
          | otherwise -> ioError (userError ("More than one source file defines module " <> T.unpack name))
        Nothing -> do
          let found' = Map.insert name source found
          foldM visitImport found' (sourceDependencies source)
    visitImport found dependency
      | not (isLocalSourceDependency dependency), Map.member name moduleIndex = pure found
      | isNothing (sourceDependencyPackage dependency), Map.member name moduleIndex = pure found
      | Map.member name found = pure found
      | not (isLocalSourceDependency dependency) = pure found
      | otherwise = do
          path <- findSourceFile sourceDirectories name
          parseSource path >>= visit found
      where
        name = sourceDependencyModule dependency

generatedEntryText :: Text
generatedEntryText = entryModuleText "Main"

-- | The name the generated entry module has among the discovered sources,
-- which have a file each. The entry is written with the other sources.
entrySourceName :: FilePath
entrySourceName = "<aihc-entry>"

-- | The generated module that starts an executable with the function
-- @main@ of its main module.
entryModuleText :: Text -> Text
entryModuleText mainModule =
  T.unlines
    [ "{-# LANGUAGE NoImplicitPrelude #-}",
      "module Aihc.Entry where",
      "import qualified " <> mainModule,
      "import GHC.TopHandler (runMainIO)",
      "entry = runMainIO " <> mainModule <> ".main"
    ]

validateInstalledDependencies :: InstalledModuleIndex -> [SourceModule] -> IO ()
validateInstalledDependencies moduleIndex sources = mapM_ validateDependency externalDependencies
  where
    localNames = Set.fromList (map sourceModuleName sources)
    externalDependencies =
      nub
        [ dependency
        | source <- sources,
          dependency <- sourceDependencies source,
          not (isLocalSourceDependency dependency)
            || sourceDependencyModule dependency `Set.notMember` localNames
        ]
    validateDependency dependency =
      case matchingModules dependency of
        [] ->
          ioError
            ( userError
                ( "Required installed module not found: "
                    <> maybe "" ((<> ":") . T.unpack) (sourceDependencyPackage dependency)
                    <> T.unpack (sourceDependencyModule dependency)
                )
            )
        [_] -> pure ()
        _ -> ioError (userError ("Ambiguous installed module: " <> T.unpack (sourceDependencyModule dependency)))
    matchingModules dependency =
      case sourceDependencyPackage dependency of
        Nothing -> candidates
        Just packageName' ->
          filter
            ((== packageName') . packageManifestName . installedManifest . installedModulePackage)
            candidates
      where
        candidates = Map.findWithDefault [] (sourceDependencyModule dependency) moduleIndex

-- | The source files of the executable: the modules the main module
-- reaches, and the generated entry module.
sourceFiles :: FilePath -> [Text] -> [SourceModule] -> IO [HackageCabal.FileInfo]
sourceFiles outputRoot dependencyNames sources = do
  entry <- writeEntryModule outputRoot "Main" dependencyNames
  pure ([sourceFileInfo dependencyNames (sourcePath source) | source <- sources, sourcePath source /= entrySourceName] <> [entry])

-- | Describe a source file the way the Cabal file of a package describes
-- the sources of an executable.
sourceFileInfo :: [Text] -> FilePath -> HackageCabal.FileInfo
sourceFileInfo dependencyNames path =
  HackageCabal.FileInfo
    { HackageCabal.fileInfoPath = path,
      HackageCabal.fileInfoExtensions = [],
      HackageCabal.fileInfoCppOptions = [],
      HackageCabal.fileInfoIncludeDirs = [],
      HackageCabal.fileInfoLanguage = Nothing,
      HackageCabal.fileInfoDependencies = dependencyNames,
      HackageCabal.fileInfoPreprocessor = Nothing
    }

-- | Write the generated entry module of an executable and describe it the
-- way the Cabal file describes the executable's own sources. The entry
-- calls the function @main@ of the main module.
writeEntryModule :: FilePath -> Text -> [Text] -> IO HackageCabal.FileInfo
writeEntryModule outputRoot mainModule dependencyNames = do
  let path = outputRoot </> "generated" </> "Aihc" </> "Entry.hs"
  createDirectoryIfMissing True (takeDirectory path)
  TIO.writeFile path (entryModuleText mainModule)
  pure (sourceFileInfo dependencyNames path)

findSourceFile :: [FilePath] -> Text -> IO FilePath
findSourceFile directories name = do
  let relative = foldl (</>) "" (map T.unpack (T.splitOn "." name)) <> ".hs"
      candidates = map (</> relative) directories
  matches <- filterM doesFileExist candidates
  case matches of
    [path] -> pure path
    [] -> ioError (userError ("Source module not found: " <> T.unpack name))
    _ -> ioError (userError ("More than one source file provides module " <> T.unpack name))

parseSource :: FilePath -> IO SourceModule
parseSource path = TIO.readFile path >>= parseSourceText path

parseSourceText :: FilePath -> Text -> IO SourceModule
parseSourceText path source = do
  let extensions = sourceExtensions source
      modu = snd (parseModule (parserConfig path source) source)
      name = fromMaybe "Main" (moduleName modu)
      dependencies =
        nub
          ( map importDependency (Syntax.moduleImports modu)
              <> implicitSourceDependencies "exe" extensions
          )
  pure
    SourceModule
      { sourcePath = path,
        sourceModuleName = name,
        sourceDependencies = dependencies
      }

importDependency :: ImportDecl -> SourceDependency
importDependency importDecl =
  SourceDependency
    { sourceDependencyPackage = importDeclPackage importDecl,
      sourceDependencyModule = importDeclModule importDecl
    }

implicitSourceDependencies :: Text -> [Extension] -> [SourceDependency]
implicitSourceDependencies currentPackage extensions =
  compilerDependencies
    <> [ SourceDependency (Just "aihc-base") "Prelude"
       | currentPackage /= "aihc-base",
         ImplicitPrelude `elem` extensions
       ]

compilerDependencies :: [SourceDependency]
compilerDependencies =
  [ SourceDependency (Just "aihc-prim") "GHC.Types",
    SourceDependency (Just "aihc-prim") "GHC.CString",
    SourceDependency (Just "aihc-prim") "GHC.Prim.Base",
    SourceDependency (Just "aihc-prim") "GHC.Prim.Enum",
    SourceDependency (Just "aihc-prim") "GHC.Classes",
    SourceDependency (Just "aihc-prim") "GHC.Prim.Num",
    SourceDependency (Just "aihc-prim") "GHC.Prim.Real",
    SourceDependency (Just "aihc-prim") "GHC.Prim.String"
  ]

isLocalSourceDependency :: SourceDependency -> Bool
isLocalSourceDependency dependency =
  isNothing (sourceDependencyPackage dependency)
    || sourceDependencyPackage dependency == Just "this"

parserConfig :: FilePath -> Text -> ParserConfig
parserConfig path source =
  defaultConfig
    { parserSourceName = path,
      parserExtensions = sourceExtensions source
    }

sourceExtensions :: Text -> [Extension]
sourceExtensions source = effectiveExtensions language (headerExtensionSettings header)
  where
    header = readModuleHeaderPragmas source
    language = fromMaybe Haskell98Edition (headerLanguageEdition header)
