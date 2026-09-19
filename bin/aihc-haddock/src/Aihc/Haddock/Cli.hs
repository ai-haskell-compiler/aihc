{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Aihc.Haddock.Cli
-- Description : The aihc-haddock command line
--
-- @aihc-haddock build TARGET@ documents a local package directory or a
-- Hackage package and its dependencies, writing the model as JSON and a
-- Hoogle database. @compare-json@ and @compare-hoogle@ compare those outputs
-- with mainline Haddock's @--show-interface@ JSON and @--hoogle@ text.
module Aihc.Haddock.Cli
  ( main,
    Command (..),
    BuildOptions (..),
    runCommand,
  )
where

import Aihc.Hackage.IndexCache (defaultIndexOptions, newHackageIndex)
import Aihc.Haddock.Compare
import Aihc.Haddock.Hoogle (renderHoogle)
import Aihc.Haddock.Model
import Aihc.Haddock.Reference.Hoogle (parseHoogleFile)
import Aihc.Haddock.Reference.Json (decodeReferenceInterface)
import Aihc.Haddock.Store
import Aihc.PackagePlan
import Aihc.PackagePlan.Lock (lockFileName)
import Control.Monad (forM, forM_, unless, when)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Distribution.Package qualified as CabalPackage
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.System (buildArch, buildOS)
import Distribution.Version (nullVersion)
import Options.Applicative
import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)

data BuildOptions = BuildOptions
  { buildTarget :: String,
    buildStoreRoot :: Maybe FilePath,
    buildUseCache :: Bool,
    buildDependencies :: Bool,
    buildJsonOutput :: Maybe FilePath,
    buildHoogleOutput :: Maybe FilePath,
    buildVerbose :: Bool
  }

data Command
  = Build BuildOptions
  | CompareJson FilePath FilePath [(T.Text, T.Text)]
  | CompareHoogle FilePath FilePath

main :: IO ()
main = do
  cmd <- execParser (info (commandParser <**> helper) (fullDesc <> progDesc "Documentation tool for aihc packages"))
  runCommand cmd

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "build" (info (Build <$> buildParser) (progDesc "Document a package and its dependencies"))
        <> command "compare-json" (info compareJsonParser (progDesc "Compare a model with Haddock's --show-interface JSON"))
        <> command "compare-hoogle" (info compareHoogleParser (progDesc "Compare a Hoogle database with Haddock's --hoogle output"))
    )

buildParser :: Parser BuildOptions
buildParser =
  BuildOptions
    <$> strArgument (metavar "TARGET" <> help "Package directory, or a Hackage package NAME[-VERSION]")
    <*> optional (strOption (long "store" <> metavar "DIR" <> help "Artifact cache directory (default: XDG cache aihc-haddock)"))
    <*> flag True False (long "no-cache" <> help "Rebuild every artifact")
    <*> flag True False (long "no-deps" <> help "Document only the target package")
    <*> optional (strOption (long "json" <> metavar "FILE" <> help "Write the documentation model as JSON"))
    <*> optional (strOption (long "hoogle" <> metavar "FILE" <> help "Write a Hoogle database"))
    <*> switch (long "verbose" <> help "Report progress")

compareJsonParser :: Parser Command
compareJsonParser =
  CompareJson
    <$> strArgument (metavar "MODEL.json" <> help "aihc-haddock model")
    <*> strArgument (metavar "REFERENCE.json" <> help "Haddock --show-interface output")
    <*> many (option aliasReader (long "module-alias" <> metavar "FROM=TO" <> help "Treat reference module FROM as aihc module TO"))
  where
    aliasReader = eitherReader $ \text ->
      case break (== '=') text of
        (from, '=' : to) | not (null from), not (null to) -> Right (T.pack from, T.pack to)
        _ -> Left "expected FROM=TO"

compareHoogleParser :: Parser Command
compareHoogleParser =
  CompareHoogle
    <$> strArgument (metavar "MODEL.txt" <> help "aihc-haddock Hoogle database")
    <*> strArgument (metavar "REFERENCE.txt" <> help "Haddock --hoogle output")

runCommand :: Command -> IO ()
runCommand cmd =
  case cmd of
    Build options -> runBuild options
    CompareJson modelPath referencePath aliases -> do
      model <- BL.readFile modelPath >>= either (fail . ("model: " <>)) pure . decodePackageDoc
      reference <- BL.readFile referencePath >>= either (fail . ("reference: " <>)) pure . decodeReferenceInterface
      let config = defaultNormalization {normalizationModuleAliases = Map.fromList aliases}
      finish (compareInterface config model reference)
    CompareHoogle modelPath referencePath -> do
      model <- parseHoogleFile <$> TIO.readFile modelPath
      reference <- parseHoogleFile <$> TIO.readFile referencePath
      finish (compareHoogle reference model)
  where
    finish report = do
      TIO.putStr (renderReport report)
      when (reportVerdict report == Fail) exitFailure

runBuild :: BuildOptions -> IO ()
runBuild options = do
  hackageIndex <- newHackageIndex defaultIndexOptions
  (root, lockDirectory) <- resolveTarget (buildTarget options)
  storeRoot' <- maybe defaultStoreRoot pure (buildStoreRoot options)
  let store = Store storeRoot'
      say message = when (buildVerbose options) (hPutStrLn stderr message)
  planned <-
    planPackages
      PlanRequest
        { requestRoots = [root],
          requestGoals = [],
          requestWorkspaces = [],
          requestPlatform = (buildOS, buildArch),
          requestConstraints = [],
          requestLockFile = Just (lockDirectory </> lockFileName),
          requestLockMode = LockNormal,
          requestIndex = hackageIndex,
          requestVerbose = say
        }
  plan <- case plannedRoots planned of
    [rootPlan] -> pure rootPlan
    _ -> ioError (userError "The plan has no root")
  package <- documentPlan store (buildUseCache options) (buildDependencies options) say plan
  forM_ (buildJsonOutput options) $ \path -> BL.writeFile path (encodePackageDoc package)
  forM_ (buildHoogleOutput options) $ \path -> TIO.writeFile path (renderHoogle package)
  let modules = packageDocModules package
      diagnostics = concatMap moduleDocDiagnostics modules
  putStrLn
    ( T.unpack (packageDocName package <> "-" <> packageDocVersion package)
        <> ": "
        <> show (length (filter moduleDocExposed modules))
        <> " exposed modules, "
        <> show (length modules - length (filter moduleDocExposed modules))
        <> " hidden, "
        <> show (length diagnostics)
        <> " diagnostics"
    )
  unless (null diagnostics || not (buildVerbose options)) $
    forM_ modules $ \modu ->
      forM_ (moduleDocDiagnostics modu) $ \diagnostic ->
        hPutStrLn stderr (T.unpack (moduleDocName modu <> ": " <> diagnostic))

-- | A directory is used as-is, with its lock beside its cabal file;
-- anything else is a Hackage package, with its lock in the working
-- directory.
resolveTarget :: String -> IO (PlanRoot, FilePath)
resolveTarget target = do
  isDirectory <- doesDirectoryExist target
  if isDirectory
    then do
      (cabalFile, _) <- parseSourcePackageDescriptionAt target
      pure (RootLocal target, takeDirectory cabalFile)
    else case parsePackageTarget target of
      Nothing -> ioError (userError (target <> " is not a directory nor a Hackage package NAME[-VERSION]"))
      Just (name, requestedVersion) -> do
        version <- forM requestedVersion $ \text ->
          maybe (ioError (userError ("Invalid version " <> text))) pure (simpleParsec text)
        directory <- getCurrentDirectory
        pure (RootHackage name version, directory)

parsePackageTarget :: String -> Maybe (String, Maybe String)
parsePackageTarget target = do
  packageId <- simpleParsec target :: Maybe CabalPackage.PackageIdentifier
  let version = CabalPackage.pkgVersion packageId
  pure
    ( CabalPackage.unPackageName (CabalPackage.pkgName packageId),
      if version == nullVersion then Nothing else Just (prettyShow version)
    )
