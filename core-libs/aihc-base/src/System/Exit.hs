module System.Exit
  ( ExitCode (..),
    exitWith,
    exitFailure,
    exitSuccess,
    die,
  )
where

import Control.Exception (throwIO)
import GHC.IO.Exception (ExitCode (..), ioError, userError)
import System.IO (hPutStr, stderr)
import Prelude

exitWith :: ExitCode -> IO a
exitWith ExitSuccess = throwIO ExitSuccess
exitWith code@(ExitFailure status) =
  case status == 0 of
    True -> ioError (userError "exitWith: invalid argument (ExitFailure 0)")
    False -> throwIO code

exitFailure :: IO a
exitFailure = exitWith (ExitFailure 1)

exitSuccess :: IO a
exitSuccess = exitWith ExitSuccess

die :: String -> IO a
die message = do
  hPutStr stderr (message ++ "\n")
  exitFailure
