--------------------------------------------------------------------------------
-- For Unix-like operating systems that provide the xdg-open script on the PATH.
--------------------------------------------------------------------------------

module Web.Browser.OS
  ( openBrowserWithExitCode
  ) where

import System.Exit ( ExitCode (..) )
import System.Process ( readProcessWithExitCode )

-- https://ss64.com/bash/xdg-open.html
openBrowserWithExitCode ::
     String
     -- ^ URL or other item to try to open.
  -> IO (ExitCode, String, String)
openBrowserWithExitCode url = readProcessWithExitCode "xdg-open" [url] ""
