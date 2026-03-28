{-# LANGUAGE OverloadedStrings #-}

-- | CLI entry point for aihc-parser.
module Aihc.Parser.CLI.Parser
  ( main,
  )
where

import Aihc.Parser.Run.Parser (runParser)
import Aihc.Parser.Syntax (Extension, ExtensionSetting (..), parseExtensionSettingName)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Options.Applicative
import System.Exit (ExitCode (..), exitFailure)

data Options = Options
  { optExtensions :: [Extension],
    optInputFile :: Maybe FilePath
  }

main :: IO ()
main = do
  opts <- execParser optionsParser
  input <- maybe TIO.getContents TIO.readFile (optInputFile opts)
  let (exitCode, output) = runParser (optExtensions opts) input
  putStr (T.unpack output)
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> exitFailure

optionsParser :: ParserInfo Options
optionsParser =
  info
    (optionsP <**> helper)
    ( fullDesc
        <> progDesc "Parse Haskell source code and pretty-print the AST"
        <> header "aihc-parser - Haskell parser"
    )

optionsP :: Parser Options
optionsP =
  Options
    <$> many extensionOption
    <*> optional (argument str (metavar "FILE" <> help "Input file (reads stdin if omitted)"))

extensionOption :: Parser Extension
extensionOption =
  option
    parseExtension
    ( short 'X'
        <> metavar "EXTENSION"
        <> help "Enable a language extension (e.g., -XNegativeLiterals)"
    )

parseExtension :: ReadM Extension
parseExtension = eitherReader $ \s ->
  case parseExtensionSettingName (T.pack s) of
    Just (EnableExtension ext) -> Right ext
    Just (DisableExtension _) -> Left ("Cannot disable extension with -X: " <> s)
    Nothing -> Left ("Unknown extension: " <> s)
