module Aihc.Cli
  ( main,
    runCommand,
  )
where

import Aihc.Cli.Build (runBuildWith)
import Aihc.Cli.Install (runInstallWith)
import Aihc.Cli.Link (runLinkExe)
import Aihc.Cli.Options (Command (..), parseCommandIO)
import Aihc.Cli.Plan (runPlan)
import Aihc.Cli.Splice (withSplices)
import Aihc.Cli.Test (runTest)
import Control.Exception (IOException, displayException, try)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  result <- try (parseCommandIO >>= runCommand) :: IO (Either IOException ())
  case result of
    Right () -> pure ()
    Left err -> do
      hPutStrLn stderr ("aihc: " <> displayException err)
      exitFailure

runCommand :: Command -> IO ()
runCommand (CmdBuild opts) = runBuildWith withSplices opts
runCommand (CmdInstall opts) = runInstallWith withSplices opts
runCommand (CmdLinkExe opts) = runLinkExe opts
runCommand (CmdPlan opts) = runPlan opts
runCommand (CmdTest opts) = runTest opts
