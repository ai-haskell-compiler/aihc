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
import Aihc.Cli.Repl (runReplWith)
import Aihc.Cli.Splice (withSplices)
import Aihc.Cli.Test (runTest)
import Control.Concurrent (forkIO, myThreadId, threadDelay, throwTo)
import Control.Exception (IOException, displayException, try)
import Control.Monad (void)
import System.Exit (ExitCode (..), exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (exitImmediately)
import System.Posix.Signals (Handler (..), installHandler, sigTERM)

main :: IO ()
main = do
  stopOnTerminate
  result <- try (parseCommandIO >>= runCommand) :: IO (Either IOException ())
  case result of
    Right () -> pure ()
    Left err -> do
      hPutStrLn stderr ("aihc: " <> displayException err)
      exitFailure

-- | Make SIGTERM stop aihc as SIGINT does: the signal becomes an exception
-- in the main thread. Thus, an install that stops publishes the packages
-- that it finished, and the next install does not compile them again. A
-- command such as @timeout@ sends SIGTERM. The exit status is the status
-- of a process that SIGTERM stops.
--
-- The exception cannot stop code that masks exceptions or that waits in a
-- foreign call. If aihc does not stop in 'terminateGrace' microseconds,
-- it stops immediately, as it did before it caught the signal. A second
-- SIGTERM also stops it immediately.
stopOnTerminate :: IO ()
stopOnTerminate = do
  mainThread <- myThreadId
  let stop = do
        void (forkIO (throwTo mainThread (ExitFailure 143)))
        threadDelay terminateGrace
        exitImmediately (ExitFailure 143)
  void (installHandler sigTERM (CatchOnce stop) Nothing)

-- | The time that aihc has to stop after SIGTERM, in microseconds.
terminateGrace :: Int
terminateGrace = 30000000

runCommand :: Command -> IO ()
runCommand (CmdBuild opts) = runBuildWith withSplices opts
runCommand (CmdInstall opts) = runInstallWith withSplices opts
runCommand (CmdLinkExe opts) = runLinkExe opts
runCommand (CmdPlan opts) = runPlan opts
runCommand (CmdRepl opts) = runReplWith withSplices opts
runCommand (CmdTest opts) = runTest opts
