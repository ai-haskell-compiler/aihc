module Main (main) where

import Control.Exception (IOException, try)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Ucd2Haskell (generateModule, writeGeneratedModule)

data Options = Options
  { optUnicodeVersion :: Maybe String,
    optUnicodeData :: Maybe FilePath,
    optDerivedCoreProperties :: Maybe FilePath,
    optOutput :: Maybe FilePath
  }

emptyOptions :: Options
emptyOptions =
  Options
    { optUnicodeVersion = Nothing,
      optUnicodeData = Nothing,
      optDerivedCoreProperties = Nothing,
      optOutput = Nothing
    }

main :: IO ()
main = do
  args <- getArgs
  case parseOptions emptyOptions args >>= completeOptions of
    Left err -> failWithUsage err
    Right (version, unicodeDataPath, derivedPropertiesPath, outputPath) -> do
      result <- try (run version unicodeDataPath derivedPropertiesPath outputPath)
      case result of
        Left err -> failWithUsage (show (err :: IOException))
        Right (Left err) -> failWithUsage err
        Right (Right ()) -> pure ()

run :: String -> FilePath -> FilePath -> FilePath -> IO (Either String ())
run version unicodeDataPath derivedPropertiesPath outputPath = do
  unicodeData <- readFile unicodeDataPath
  derivedProperties <- readFile derivedPropertiesPath
  case generateModule version unicodeData derivedProperties of
    Left err -> pure (Left err)
    Right generated -> do
      writeGeneratedModule outputPath generated
      pure (Right ())

parseOptions :: Options -> [String] -> Either String Options
parseOptions options [] = Right options
parseOptions options ("--unicode-version" : value : rest) =
  parseOptions options {optUnicodeVersion = Just value} rest
parseOptions options ("--unicode-data" : value : rest) =
  parseOptions options {optUnicodeData = Just value} rest
parseOptions options ("--derived-core-properties" : value : rest) =
  parseOptions options {optDerivedCoreProperties = Just value} rest
parseOptions options ("--output" : value : rest) =
  parseOptions options {optOutput = Just value} rest
parseOptions _ (unknown : _) = Left ("unknown or incomplete option: " <> unknown)

completeOptions :: Options -> Either String (String, FilePath, FilePath, FilePath)
completeOptions options =
  (,,,)
    <$> require "--unicode-version" (optUnicodeVersion options)
    <*> require "--unicode-data" (optUnicodeData options)
    <*> require "--derived-core-properties" (optDerivedCoreProperties options)
    <*> require "--output" (optOutput options)
  where
    require name = maybe (Left ("missing required option " <> name)) Right

failWithUsage :: String -> IO a
failWithUsage err = do
  hPutStrLn stderr ("ucd2haskell: " <> err)
  hPutStrLn stderr "usage: ucd2haskell --unicode-version VERSION --unicode-data FILE --derived-core-properties FILE --output FILE"
  exitFailure
