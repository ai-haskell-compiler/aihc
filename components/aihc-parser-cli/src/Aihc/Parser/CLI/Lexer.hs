-- | CLI entry point for aihc-lexer.
module Aihc.Parser.CLI.Lexer
  ( main,
  )
where

import Aihc.Parser.Run.Lexer (CLIResult (..), runLexer)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (getArgs)
import System.Exit (exitWith)
import System.IO (hPutStr, stderr)

main :: IO ()
main = do
  args <- getArgs
  stdin <- TIO.getContents
  let result = runLexer args stdin
  putStr (T.unpack (cliStdout result))
  hPutStr stderr (T.unpack (cliStderr result))
  exitWith (cliExitCode result)
