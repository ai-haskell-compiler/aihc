module Test.Mockery.Directory (
  inTempDirectory
, inTempDirectoryNamed
, withFile
, touch
) where

import           Control.Exception
import           Control.Monad
import           System.Directory
import           System.FilePath
import           System.IO.Error
import           System.IO hiding (withFile)
import           System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import qualified Data.ByteString as B

-- |
-- Run given action with the current working directory set to a temporary
-- directory.
--
-- The directory is created before the action is run.  After the action has
-- completed the directory is removed and the current working directory is
-- restored to its original value.
inTempDirectory :: IO a -> IO a
inTempDirectory action = withSystemTempDirectory "mockery" $ \path -> do
  bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
    setCurrentDirectory path
    action

-- |
-- Similar to `inTempDirectory`, but the temporary directory will have the
-- specified name.
inTempDirectoryNamed :: FilePath -> IO a -> IO a
inTempDirectoryNamed name action = inTempDirectory $ do
  createDirectory name
  setCurrentDirectory name
  action

-- |
-- Create a temporary file with the given contents and execute the given
-- action.
--
-- The file is removed after the action has completed.
withFile :: String -> (FilePath -> IO a) -> IO a
withFile input action = withSystemTempFile "mockery" $ \file h -> do
  hPutStr h input
  hClose h
  action file

-- |
-- Update the modification time of the specified file.
-- Create an empty file (including any missing directories in the file path) if
-- the file does not exist.
touch :: FilePath -> IO ()
touch file = do
  createDirectoryIfMissing True (takeDirectory file)
  c <- catchJust (guard . isDoesNotExistError) (B.readFile file) (const $ return B.empty)
  B.writeFile file c
