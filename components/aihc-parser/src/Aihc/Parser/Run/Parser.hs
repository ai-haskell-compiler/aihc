{-# LANGUAGE OverloadedStrings #-}

-- | Pure parser runner for testing and library use.
module Aihc.Parser.Run.Parser
  ( runParser,
  )
where

import Aihc.Parser (ParseResult (..), ParserConfig (..), defaultConfig, errorBundlePretty, parseModule)
import Aihc.Parser.Shorthand (Shorthand (..))
import Aihc.Parser.Syntax (Extension)
import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (ExitCode (..))

-- | Run the parser on input text with given extensions.
-- Returns (ExitCode, output text).
-- This is a pure function that can be tested without IO capture.
runParser :: [Extension] -> Text -> (ExitCode, Text)
runParser extensions input =
  let cfg = defaultConfig {parserExtensions = extensions}
   in case parseModule cfg input of
        ParseOk modu ->
          (ExitSuccess, T.pack (show (shorthand modu)) <> "\n")
        ParseErr bundle ->
          (ExitFailure 1, T.pack (errorBundlePretty bundle))
