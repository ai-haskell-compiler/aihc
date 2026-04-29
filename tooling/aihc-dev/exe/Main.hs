module Main (main) where

import Aihc.Dev.ExtractHi (extractPackage)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as BL
import Data.Yaml qualified as Yaml
import Options.Applicative

main :: IO ()
main = do
  cmd <- execParser opts
  runCommand cmd
  where
    opts =
      info
        (commandParser <**> helper)
        ( fullDesc
            <> header "aihc-dev - developer tools for the aihc compiler"
        )

-- | Top-level command type. New subcommands are added here.
newtype Command
  = ExtractHi ExtractHiOpts

data ExtractHiOpts = ExtractHiOpts
  { ehPackage :: String,
    ehFormat :: OutputFormat
  }

data OutputFormat = YAML | JSON
  deriving (Show)

commandParser :: Parser Command
commandParser =
  subparser
    ( command
        "extract-hi"
        ( info
            (ExtractHi <$> extractHiParser <**> helper)
            (progDesc "Extract scoping and typing information from .hi interface files")
        )
    )

extractHiParser :: Parser ExtractHiOpts
extractHiParser =
  ExtractHiOpts
    <$> strArgument
      ( metavar "PACKAGE"
          <> help "Package name to extract (e.g. 'base', 'containers')"
      )
    <*> flag
      YAML
      JSON
      ( long "json"
          <> help "Output JSON instead of YAML"
      )

runCommand :: Command -> IO ()
runCommand (ExtractHi opts) = do
  pkg <- extractPackage (ehPackage opts)
  case ehFormat opts of
    YAML -> BL.putStr (BL.fromStrict (Yaml.encode pkg))
    JSON -> BL.putStr (encode pkg)
