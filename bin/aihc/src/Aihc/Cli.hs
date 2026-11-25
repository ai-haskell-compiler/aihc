module Aihc.Cli
  ( main,
    runCommand,
  )
where

import Aihc.Cli.Build (runBuildWith)
import Aihc.Cli.Install (runInstallWith)
import Aihc.Cli.Link (runLinkExe)
import Aihc.Cli.Options (Command (..), NativeSplicePackage, parseCommandIO)
import Aihc.Cli.Plan (runPlan)
import Aihc.Cli.Repl (runReplWith)
import Aihc.Cli.Splice (withNativeSplices, withSplices)
import Aihc.Cli.Test (runTest)
import Aihc.Hackage.Package (showVersion, unPackageName)
import Control.Concurrent (myThreadId, throwTo)
import Control.Exception (IOException, displayException, try)
import Control.Monad (forM_, void)
import System.Exit (ExitCode (..), exitFailure)
import System.IO (hPutStrLn, stderr)
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
stopOnTerminate :: IO ()
stopOnTerminate = do
  mainThread <- myThreadId
  void (installHandler sigTERM (Catch (throwTo mainThread (ExitFailure 143))) Nothing)

runCommand :: Command -> IO ()
runCommand (CmdBuild packages opts) = warnNativeSplices packages >> runBuildWith (withNativeSplices packages) opts
runCommand (CmdInstall packages opts) = warnNativeSplices packages >> runInstallWith (withNativeSplices packages) opts
runCommand (CmdLinkExe opts) = runLinkExe opts
runCommand (CmdPlan opts) = runPlan opts
runCommand (CmdRepl opts) = runReplWith withSplices opts
runCommand (CmdTest opts) = runTest opts

-- | Tell the user that the splices of the packages have no sandbox.
warnNativeSplices :: [NativeSplicePackage] -> IO ()
warnNativeSplices packages =
  forM_ packages $ \(name, version) ->
    hPutStrLn stderr ("warning: --unsafe-disable-th-sandbox runs the Template Haskell splices of " <> unPackageName name <> maybe "" (("-" <>) . showVersion) version <> " as native code with all the access of the user. Use it only for packages that you trust.")
