{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aihc.BaseApi.Extractor
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as BL
import Options.Applicative
import Data.Text (Text)

data Cli = Cli
  { cliGhcVersion :: !Text,
    cliTarget :: !Text,
    cliCacheDir :: !FilePath,
    cliOutput :: !FilePath,
    cliArchiveUrl :: !(Maybe Text)
  }

main :: IO ()
main = do
  cli <- execParser parserInfo
  snapshot <-
    extractBaseApi
      ExtractOptions
        { eoGhcVersion = cliGhcVersion cli,
          eoTarget = cliTarget cli,
          eoCacheDir = cliCacheDir cli,
          eoArchiveUrl = cliArchiveUrl cli
        }
  BL.writeFile (cliOutput cli) (encode snapshot)

parserInfo :: ParserInfo Cli
parserInfo =
  info (helper <*> cliParser) $
    fullDesc
      <> progDesc "Download a GHC bindist and extract base API data from interface files"

cliParser :: Parser Cli
cliParser =
  Cli
    <$> option str (long "ghc-version" <> metavar "VERSION")
    <*> option str (long "target" <> metavar "TARGET")
    <*> strOption (long "cache-dir" <> value ".cache/aihc-base-api" <> showDefault)
    <*> strOption (long "output" <> value "base-api.json" <> showDefault)
    <*> optional (option str (long "archive-url" <> metavar "URL"))
