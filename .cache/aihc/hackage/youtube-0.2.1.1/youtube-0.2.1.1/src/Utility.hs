module Utility where

import qualified System.Exit as Exit
import qualified System.IO as IO


exitFailureMsg :: String -> IO ()
exitFailureMsg msg = do
   IO.hPutStrLn IO.stderr msg
   Exit.exitFailure
