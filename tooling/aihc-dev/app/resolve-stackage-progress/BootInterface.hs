{-# LANGUAGE OverloadedStrings #-}

-- | Loading pre-generated boot package interfaces from JSON files.
--
-- Boot packages (ghc-prim, ghc-internal, ghc-bignum, base) use GHC-internal
-- syntax that aihc-parser cannot handle. Instead of parsing them, we load
-- pre-extracted interface files produced by @aihc-dev extract-resolve-iface@.
--
-- The JSON schema for a boot interface file is:
--
-- @
-- { "package": "base-4.21.0.0"
-- , "modules":
--     [ { "module": "Data.List"
--       , "terms": ["map", "filter", "foldl", ...]
--       , "types": ["NonEmpty"]
--       , "constructors": {"Maybe": ["Just", "Nothing"], ...}
--       , "methods": {"Functor": ["fmap", "<$"], ...}
--       }
--     ]
-- }
-- @
--
-- Terms, types, constructors, and methods are all that the resolver needs:
-- which names exist in which namespace for each module.
module BootInterface
  ( loadBootInterfaces,
    bootPackageNames,
  )
where

import Aihc.Parser.Syntax (NameType (..), mkQualifiedName, mkUnqualifiedName)
import Aihc.Resolve (ModuleExports, ResolvedName (..), Scope (..))
import Control.Exception (IOException, try)
import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Char (isAlphaNum, isSpace, isUpper)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess)

-- | The set of packages that require pre-generated interfaces.
-- These use GHC-internal syntax that cannot be parsed by aihc-parser.
bootPackageNames :: Set Text
bootPackageNames = Set.fromList ["ghc-prim", "ghc-internal", "ghc-bignum", "base"]

-- | Load pre-generated boot interfaces, generating them on demand if not cached.
--
-- Returns a map from package name to its ModuleExports.
-- Missing or broken interfaces are logged to stderr and treated as empty.
loadBootInterfaces :: IO (Map Text ModuleExports)
loadBootInterfaces = do
  cacheDir <- getBootCacheDir
  createDirectoryIfMissing True cacheDir
  ghcVersion <- trim <$> readProcess "ghc" ["--numeric-version"] ""
  let versionedDir = cacheDir </> ghcVersion
  createDirectoryIfMissing True versionedDir
  results <- mapM (loadOneBootPackage versionedDir) (Set.toList bootPackageNames)
  pure (Map.fromList [(name, iface) | (name, Just iface) <- results])

-- | Get the cache directory for boot interfaces.
getBootCacheDir :: IO FilePath
getBootCacheDir = do
  mXdg <- lookupEnv "XDG_CACHE_HOME"
  case mXdg of
    Just dir -> pure (dir </> "aihc-resolve" </> "boot-interfaces")
    Nothing -> do
      home <- lookupEnv "HOME"
      case home of
        Just h -> pure (h </> ".cache" </> "aihc-resolve" </> "boot-interfaces")
        Nothing -> pure (".cache" </> "aihc-resolve" </> "boot-interfaces")

-- | Load a single boot package's interface, generating if needed.
loadOneBootPackage :: FilePath -> Text -> IO (Text, Maybe ModuleExports)
loadOneBootPackage cacheDir pkgName = do
  let path = cacheDir </> T.unpack pkgName <> ".json"
  exists <- doesFileExist path
  if exists
    then loadFromFile pkgName path
    else do
      generated <- generateBootInterface pkgName path
      if generated
        then loadFromFile pkgName path
        else pure (pkgName, Nothing)

-- | Load a boot interface from a JSON file.
loadFromFile :: Text -> FilePath -> IO (Text, Maybe ModuleExports)
loadFromFile pkgName path = do
  result <- Aeson.eitherDecodeFileStrict' path
  case result of
    Left err -> do
      hPutStrLn stderr ("Warning: failed to decode boot interface for " <> T.unpack pkgName <> ": " <> err)
      pure (pkgName, Nothing)
    Right bootIface -> pure (pkgName, Just (bootIfaceToModuleExports bootIface))

-- | Generate a boot interface by running aihc-dev extract-resolve-iface.
generateBootInterface :: Text -> FilePath -> IO Bool
generateBootInterface pkgName outputPath = do
  hPutStrLn stderr ("Generating boot interface for " <> T.unpack pkgName <> "...")
  result <-
    try $
      readProcess
        "cabal"
        ["run", "-v0", "aihc-dev", "--", "extract-resolve-iface", "--package", T.unpack pkgName, "--output", outputPath]
        ""
  case result of
    Left (e :: IOException) -> do
      hPutStrLn stderr ("Warning: failed to generate boot interface for " <> T.unpack pkgName <> ": " <> show e)
      pure False
    Right _ -> do
      hPutStrLn stderr ("  Generated: " <> outputPath)
      pure True

-- | JSON representation of a boot package interface.
data BootPackageInterface = BootPackageInterface
  { _bpiPackage :: Text,
    bpiModules :: [BootModuleInterface]
  }

instance FromJSON BootPackageInterface where
  parseJSON = withObject "BootPackageInterface" $ \o ->
    BootPackageInterface
      <$> o .: "package"
      <*> o .: "modules"

-- | JSON representation of a single module's interface.
data BootModuleInterface = BootModuleInterface
  { bmiModule :: Text,
    bmiTerms :: [Text],
    bmiTypes :: [Text],
    bmiConstructors :: Map Text [Text],
    bmiMethods :: Map Text [Text]
  }

instance FromJSON BootModuleInterface where
  parseJSON = withObject "BootModuleInterface" $ \o ->
    BootModuleInterface
      <$> o .: "module"
      <*> (fromMaybe [] <$> o .:? "terms")
      <*> (fromMaybe [] <$> o .:? "types")
      <*> (fromMaybe Map.empty <$> o .:? "constructors")
      <*> (fromMaybe Map.empty <$> o .:? "methods")

-- | Convert a boot package interface to the resolver's ModuleExports format.
bootIfaceToModuleExports :: BootPackageInterface -> ModuleExports
bootIfaceToModuleExports bpi =
  Map.fromList
    [(bmiModule bmi, bootModuleToScope bmi) | bmi <- bpiModules bpi]

-- | Convert a single module interface to a Scope.
bootModuleToScope :: BootModuleInterface -> Scope
bootModuleToScope bmi =
  Scope
    { scopeTerms = Map.fromList termEntries,
      scopeTypes = Map.fromList typeEntries,
      scopeQualifiedModules = Map.empty
    }
  where
    modName = bmiModule bmi

    termEntries =
      [(n, resolve modName n) | n <- bmiTerms bmi]
        ++ [ (n, resolve modName n)
           | (_, ctors) <- Map.toList (bmiConstructors bmi),
             n <- ctors
           ]
        ++ [ (n, resolve modName n)
           | (_, meths) <- Map.toList (bmiMethods bmi),
             n <- meths
           ]

    typeEntries =
      [(n, resolve modName n) | n <- bmiTypes bmi]
        ++ [(className, resolve modName className) | className <- Map.keys (bmiMethods bmi)]

    resolve :: Text -> Text -> ResolvedName
    resolve qual n =
      ResolvedTopLevel (mkQualifiedName (mkUnqualifiedName (inferNameType n) n) (Just qual))

-- | Infer the NameType from a name's lexical form.
inferNameType :: Text -> NameType
inferNameType n = case T.uncons n of
  Nothing -> NameConId
  Just (c, _)
    | c == ':' -> NameConSym
    | not (isAlphaNum c) && c /= '_' && c /= '\'' -> NameVarSym
    | isUpper c -> NameConId
    | otherwise -> NameVarId

-- | Strip leading and trailing whitespace.
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
