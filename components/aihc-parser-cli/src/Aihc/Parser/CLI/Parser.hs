-- | CLI entry point for aihc-parser.
module Aihc.Parser.CLI.Parser
  ( main,
  )
where

import Aihc.Parser.Run.Parser (CLIResult (..), runParser)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (getArgs)
import System.Exit (exitWith)
import System.IO (hPutStr, stderr)

main :: IO ()
main = do
  args <- getArgs
  stdin <- TIO.getContents
  let result = runParser args stdin
  putStr (T.unpack (cliStdout result))
  hPutStr stderr (T.unpack (cliStderr result))
  exitWith (cliExitCode result)
