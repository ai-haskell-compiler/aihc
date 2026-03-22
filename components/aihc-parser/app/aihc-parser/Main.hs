{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aihc.Parser (defaultConfig, errorBundlePretty, parseModule)
import Aihc.Parser.Ast (Extension, ExtensionSetting (..), parseExtensionSettingName)
import Aihc.Parser.PrettyAST (prettyASTModule)
import Aihc.Parser.Types (ParseResult (..), ParserConfig (..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Exit (exitFailure)

data Options = Options
  { optExtensions :: [Extension],
    optInputFile :: Maybe FilePath
  }

main :: IO ()
main = do
  opts <- execParser optionsParser
  input <- maybe TIO.getContents TIO.readFile (optInputFile opts)
  let cfg = defaultConfig {parserExtensions = optExtensions opts}
  case parseModule cfg input of
    ParseOk modu -> TIO.putStrLn (prettyASTModule modu)
    ParseErr bundle -> do
      putStr (errorBundlePretty bundle)
      exitFailure

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
