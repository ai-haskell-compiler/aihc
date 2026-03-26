{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import AIhc.Parser.Syntax (Extension, ExtensionSetting (..), parseExtensionSettingName)
import Aihc.Parser.Lex (lexTokensWithExtensions)
import Aihc.Parser.Shorthand (Shorthand (..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative

data Options = Options
  { optExtensions :: [Extension],
    optInputFile :: Maybe FilePath
  }

main :: IO ()
main = do
  opts <- execParser optionsParser
  input <- maybe TIO.getContents TIO.readFile (optInputFile opts)
  let tokens = lexTokensWithExtensions (optExtensions opts) input
  mapM_ (print . shorthand) tokens

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
