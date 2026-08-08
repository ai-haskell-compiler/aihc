module GHC.TopHandler (runMainIO) where

import Control.Exception (SomeException, catch, displayException, fromException)
import Data.Maybe (Maybe (..))
import GHC.IO.Output (writeStderrString)
import System.Exit (ExitCode (..))
import Prelude

-- | Run the user entry action and turn its terminal outcome into the status
-- consumed by the executable backend.
runMainIO :: IO a -> IO Int
runMainIO action =
  catch
    (action >> returnStatus 0)
    topHandler

topHandler :: SomeException -> IO Int
topHandler exception =
  case fromException exception of
    Just ExitSuccess -> returnStatus 0
    Just (ExitFailure status) -> returnStatus status
    Nothing -> do
      writeStderrString (displayException exception)
      returnStatus 1

returnStatus :: Int -> IO Int
returnStatus status = status `seq` return status
