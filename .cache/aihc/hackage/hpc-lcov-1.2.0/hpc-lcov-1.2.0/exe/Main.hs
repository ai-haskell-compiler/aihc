{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

import Control.Exception (ErrorCall (..), evaluate, throwIO, try)
import Control.Monad (forM)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.Char (isDigit)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.List (find, isPrefixOf)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml
import qualified Options.Applicative as Opt
import Path (Abs, Dir, File, Path, Rel, reldir, relfile, (</>))
import qualified Path
import Path.IO (listDir, listDirRecur, resolveFile')
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Trace.Hpc.Lcov (generateLcovFromTix, writeReport)
import Trace.Hpc.Mix (Mix (..), readMix)
import Trace.Hpc.Tix (Tix (..), TixModule, readTix, tixModuleName)

data CLIOptions = CLIOptions
  { cliTixFiles :: [FilePath]
  , cliMainPackage :: Maybe String
  , cliOutput :: FilePath
  }

getCLIOptions :: IO CLIOptions
getCLIOptions =
  Opt.execParser $
    Opt.info (Opt.helper <*> parseCLIOptions) $
      Opt.progDesc description
  where
    parseCLIOptions =
      CLIOptions
        <$> parseCLITixFiles
        <*> parseCLIMainPackage
        <*> parseCLIOutput
    parseCLITixFiles =
      Opt.many $
        Opt.strOption $
          mconcat
            [ Opt.long "file"
            , Opt.short 'f'
            , Opt.metavar "FILE"
            , Opt.help "Manually specify .tix file(s) to convert"
            ]
    parseCLIMainPackage =
      Opt.optional $
        Opt.strOption $
          mconcat
            [ Opt.long "main-package"
            , Opt.metavar "PACKAGE"
            , Opt.help "The package that built the coverage-enabled executable"
            ]
    parseCLIOutput =
      Opt.strOption $
        mconcat
          [ Opt.long "output"
          , Opt.short 'o'
          , Opt.metavar "FILE"
          , Opt.help "The file to save coverage information (default: lcov.info)"
          , Opt.value "lcov.info"
          ]

    description = "Convert HPC coverage output into the LCOV format"

main :: IO ()
main = do
  CLIOptions{..} <- getCLIOptions
  stackRoot <- getStackRoot

  tixFiles <-
    if null cliTixFiles
      then findTixModules
      else mapM resolveFile' cliTixFiles
  tixModules <- filter (not . isPathsModule) . concat <$> mapM readTixPath tixFiles

  distDir <- getStackDistPath
  packages <- getPackages
  let mixDirectories = map (getMixDirectory distDir . snd) packages

  moduleToMixList <- forM tixModules $ \tixModule -> do
    (modulePackageName, Mix fileLoc _ _ _ mixEntries) <-
      -- tixModuleName is either just the module name or "PACKAGE_ID/MODULE"
      case Text.splitOn "/" . Text.pack . tixModuleName $ tixModule of
        [packageId, _] -> do
          mix <- readMixPathThrow mixDirectories (Right tixModule)
          return (Text.unpack $ packageNameFromId packageId, mix)
        ["Main"] -> do
          let pkgName =
                fromMaybe
                  (error "Found executable in coverage file, --main-package was not provided")
                  cliMainPackage
          mix <-
            readMixPath mixDirectories (Right tixModule) >>= \case
              Right x -> return x
              Left (ErrorCallWithLocation msg _)
                | "does not match hash" `Text.isInfixOf` Text.pack msg ->
                    error . unlines $
                      [ "Coverage file for a Main module contained a different hash from the Mix file."
                      , "Did you forget to load the coverage separately?"
                      , "See the README of hpc-lcov for more information."
                      ]
              Left e -> throwIO e
          return (pkgName, mix)
        _ -> error $ "Could not load Mix file from tix module: " ++ show tixModule

    fileLocRelPath <- Path.parseRelFile fileLoc

    modulePath <- case modulePackageName `lookup` packages of
      Just packagePath -> do
        let modulePathAbs = packagePath </> fileLocRelPath
        maybe (fail $ show modulePathAbs ++ " is not a subpath of " ++ show stackRoot) return $
          Path.stripProperPrefix stackRoot modulePathAbs
      Nothing -> fail $ "Could not find package: " ++ modulePackageName

    return (tixModuleName tixModule, (Path.toFilePath modulePath, mixEntries))

  let moduleToMix = Map.toList . Map.fromListWith checkDupeMix $ moduleToMixList
      checkDupeMix mix1 mix2 =
        if mix1 == mix2
          then mix1
          else error $ ".mix files differ: " ++ show (mix1, mix2)
      report = generateLcovFromTix moduleToMix tixModules

  writeReport cliOutput report

-- Parse the input, which is in one of the following formats:
--     * PACKAGE-VERSION-HASH
--     * PACKAGE-VERSION-HASH-EXE_NAME (>= GHC 9.10)
packageNameFromId :: Text -> Text
packageNameFromId = Text.intercalate "-" . takeWhile (not . isVersion) . Text.splitOn "-"
  where
    isVersion s = '.' `Text.elem` s || Text.all isDigit s

{- HPC file discovery -}

-- | Find all .tix files in the HPC root.
findTixModules :: IO [Path Abs File]
findTixModules = do
  hpcRoot <- getStackHpcRoot

  (_, files) <- listDirRecur hpcRoot

  let tixFiles = filter (hasExt ".tix") files
      -- Find all.tix, if one exists, which Stack automatically generates
      -- if multiple .tix files are generated.
      mAllTix = find ((== [relfile|all.tix|]) . Path.filename) tixFiles

  return $ maybe tixFiles (: []) mAllTix

getMixDirectory :: Path Rel Dir -> Path Abs Dir -> Path Abs Dir
getMixDirectory distDir packageDir = packageDir </> distDir </> [reldir|hpc|]

{- HPC file readers -}

readTixPath :: Path b File -> IO [TixModule]
readTixPath path = do
  Tix tixModules <-
    readTix (Path.toFilePath path)
      >>= maybe (fail $ "Could not find tix file: " ++ Path.toFilePath path) return
  return tixModules

readMixPathThrow :: [Path b Dir] -> Either String TixModule -> IO Mix
readMixPathThrow = readMix . map Path.toFilePath

readMixPath :: [Path b Dir] -> Either String TixModule -> IO (Either ErrorCall Mix)
readMixPath dirs tix = try (readMixPathThrow dirs tix >>= evaluate)

{- Haskell package/module helpers -}

isPathsModule :: TixModule -> Bool
isPathsModule = ("Paths_" `isPrefixOf`) . tixModuleName

{- Stack helpers -}

-- | Get the root of the stack project.
getStackRoot :: IO (Path Abs Dir)
getStackRoot = do
  -- assume that the stack.yaml file is at the root of the stack projet
  configPath <- readStack ["path", "--config-location"]
  Path.parent <$> Path.parseAbsFile configPath

getStackHpcRoot :: IO (Path Abs Dir)
getStackHpcRoot = Path.parseAbsDir =<< readStack ["path", "--local-hpc-root"]

getStackDistPath :: IO (Path Rel Dir)
getStackDistPath = Path.parseRelDir =<< readStack ["path", "--dist-dir"]

-- | Get a list of package names in the stack project and their location.
getPackages :: IO [(String, Path Abs Dir)]
getPackages = do
  stackConfigPath <- readStack ["path", "--config-location"]
  stackConfig <-
    Yaml.decodeFileEither @(HashMap String JSON.Value) stackConfigPath
      >>= either (\e -> fail $ "Could not decode file `" ++ stackConfigPath ++ "`: " ++ show e) return

  stackConfigDir <- Path.parent <$> Path.parseAbsFile stackConfigPath

  packagePaths <- case HashMap.lookup "packages" stackConfig of
    Nothing -> pure ["."]
    Just packagesField ->
      case JSON.parseMaybe JSON.parseJSON packagesField of
        Just packages -> return packages
        Nothing -> fail $ "Invalid packages field: " ++ show stackConfig

  forM packagePaths $ \packagePath -> do
    packageDir <- case packagePath of
      -- special case since Path doesn't support `..`. Not spending too much effort on more complex
      -- cases
      ".." -> return $ Path.parent stackConfigDir
      package -> (stackConfigDir </>) <$> Path.parseRelDir package

    (_, files) <- listDir packageDir

    packageName <- case filter (hasExt ".cabal") files of
      [] -> fail $ "No .cabal file found in " ++ Path.toFilePath packageDir
      [cabal] -> Path.toFilePath . Path.filename <$> removeExtension cabal
      _ -> fail $ "Multiple .cabal files found in " ++ Path.toFilePath packageDir

    return (packageName, packageDir)

readStack :: [String] -> IO String
readStack args = do
  (code, stdout, stderr) <- readProcessWithExitCode "stack" args ""
  case (code, lines stdout) of
    (ExitSuccess, line : _) -> return line
    _ ->
      error . unlines $
        [ "Reading Stack output failed."
        , "Code: " ++ show code
        , "Stdout: " ++ stdout
        , "Stderr: " ++ stderr
        ]

{- Utilities -}

hasExt :: String -> Path b File -> Bool
hasExt ext = (== ext') . Path.fileExtension
  where

#if MIN_VERSION_path(0,7,0)
    ext' = Just ext
#else
    ext' = ext
#endif

removeExtension :: Path b File -> IO (Path b File)

#if MIN_VERSION_path(0,7,0)
removeExtension = fmap fst . Path.splitExtension
#else
removeExtension = Path.setFileExtension ""
#endif
