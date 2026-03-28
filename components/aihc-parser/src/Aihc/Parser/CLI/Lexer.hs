{-# LANGUAGE OverloadedStrings #-}

-- | CLI entry point for aihc-lexer, exposed as a library module for testing.
module Aihc.Parser.CLI.Lexer
  ( main,
    runLexer,
  )
where

import Aihc.Parser.Lex (lexModuleTokensWithExtensions)
import Aihc.Parser.Shorthand (Shorthand (..))
import Aihc.Parser.Syntax (Extension, ExtensionSetting (..), parseExtensionSettingName)
import Data.Text (Text)
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

-- | Run the lexer on input text with given extensions.
-- Returns (ExitCode, output text).
-- This is the pure core that can be tested without IO capture.
runLexer :: [Extension] -> Text -> (ExitCode, Text)
runLexer extensions input =
  let tokens = lexModuleTokensWithExtensions extensions input
      output = T.unlines (map (T.pack . show . shorthand) tokens)
   in (ExitSuccess, output)

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
