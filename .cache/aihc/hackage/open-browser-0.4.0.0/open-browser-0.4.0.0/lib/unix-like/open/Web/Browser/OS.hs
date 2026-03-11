--------------------------------------------------------------------------------
-- For Unix-like operating systems, such as macOS, that provide the open
-- application on the PATH.
--------------------------------------------------------------------------------

module Web.Browser.OS
  ( openBrowserWithExitCode
  ) where

import System.Exit ( ExitCode (..) )
import System.Process ( readProcessWithExitCode )

-- https://ss64.com/mac/open.html
openBrowserWithExitCode ::
     String
     -- ^ URL or other item to try to open.
  -> IO (ExitCode, String, String)
openBrowserWithExitCode url = readProcessWithExitCode "open" [url] ""
