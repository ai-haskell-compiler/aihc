{-# LANGUAGE OverloadedStrings #-}

-- | CLI entry point for aihc-lexer.
module Aihc.Parser.CLI.Lexer
  ( main,
  )
where

import Aihc.Parser.Run.Lexer (runLexer)
import Aihc.Parser.Syntax (Extension, ExtensionSetting (..), parseExtensionSettingName)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Options.Applicative
import System.Exit (ExitCode (..))

data Options = Options
  { optExtensions :: [Extension],
    optInputFile :: Maybe FilePath
  }

main :: IO ()
main = do
  opts <- execParser optionsParser
  input <- maybe TIO.getContents TIO.readFile (optInputFile opts)
  let (exitCode, output) = runLexer (optExtensions opts) input
  putStr (T.unpack output)
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> pure () -- Lexer doesn't fail, but keep consistent API

optionsParser :: ParserInfo Options
optionsParser =
  info
    (optionsP <**> helper)
    ( fullDesc
        <> progDesc "Lex Haskell source code and pretty-print the token stream"
        <> header "aihc-lexer - Haskell lexer"
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
