module Aihc.Cli
  ( main,
    runCommand,
  )
where

import Aihc.Cli.Install (runInstall)
import Aihc.Cli.Options (Command (..), parseCommandIO)
import Aihc.Cli.Repl (runRepl)

main :: IO ()
main = parseCommandIO >>= runCommand

runCommand :: Command -> IO ()
runCommand (CmdInstall opts) = runInstall opts
runCommand CmdRepl = runRepl
