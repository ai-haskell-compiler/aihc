-- | Pure lexer runner for testing and library use.
module Aihc.Parser.Run.Lexer
  ( runLexer,
  )
where

import Aihc.Parser.Lex (lexModuleTokensWithExtensions)
import Aihc.Parser.Shorthand (Shorthand (..))
import Aihc.Parser.Syntax (Extension)
import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (ExitCode (..))

-- | Run the lexer on input text with given extensions.
-- Returns (ExitCode, output text).
-- This is a pure function that can be tested without IO capture.
runLexer :: [Extension] -> Text -> (ExitCode, Text)
runLexer extensions input =
  let tokens = lexModuleTokensWithExtensions extensions input
      output = T.unlines (map (T.pack . show . shorthand) tokens)
   in (ExitSuccess, output)
