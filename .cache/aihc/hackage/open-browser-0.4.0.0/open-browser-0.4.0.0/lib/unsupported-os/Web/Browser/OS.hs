--------------------------------------------------------------------------------
-- For unsupported operating systems
--------------------------------------------------------------------------------

module Web.Browser.OS
  ( openBrowserWithExitCode
  ) where

import System.Exit ( ExitCode (..) )

openBrowserWithExitCode ::
     String
     -- ^ URL or other item to try to open.
  -> IO (ExitCode, String, String)
openBrowserWithExitCode =
  -- Operation never succeeds
  const (pure (ExitFailure 1, "", "Unsupported operating system."))
