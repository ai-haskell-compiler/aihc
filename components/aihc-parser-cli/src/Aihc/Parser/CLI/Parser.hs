-- | CLI entry point for aihc-parser.
--
-- This is the unified CLI that supports both parsing (default) and lexing
-- (via @--lex@ flag) modes.
module Aihc.Parser.CLI.Parser
  ( main,
  )
where

import Aihc.Parser.Run (CLIResult (..), runCLI)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (getArgs)
import System.Exit (exitWith)
import System.IO (hPutStr, stderr)

main :: IO ()
main = do
  args <- getArgs
  stdin <- TIO.getContents
  let result = runCLI args stdin
  putStr (T.unpack (cliStdout result))
  hPutStr stderr (T.unpack (cliStderr result))
  exitWith (cliExitCode result)
