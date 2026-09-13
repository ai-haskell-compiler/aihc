-- | Write an object to a file only after it assembled in full.
module Aihc.Native.ObjectWriter
  ( withObjectWriter,
    checked,
  )
where

import Aihc.Native.Object
import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.ST (RealWorld, stToIO)
import Data.ByteString.Lazy qualified as BL
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.FilePath (takeDirectory)
import System.IO (hClose, openBinaryTempFile)

-- | Publish the object only after all compiler and object checks succeed.
-- An existing file at the destination stays untouched until then.
withObjectWriter :: FilePath -> (Image -> Either ObjectError BL.ByteString) -> (Object RealWorld -> IO value) -> IO value
withObjectWriter destination encode action = do
  createDirectoryIfMissing True directory
  object <- stToIO newObject
  value <- action object
  image <- stToIO (layoutObject object) >>= checked
  bytes <- checked (encode image)
  bracket (openBinaryTempFile directory ".aihc-object") (\(path, handle) -> hClose handle >> removeIfPresent path) $ \(path, handle) -> do
    BL.hPut handle bytes
    hClose handle
    renameFile path destination
  pure value
  where
    directory = takeDirectory destination

checked :: Either ObjectError value -> IO value
checked = either (ioError . userError . show) pure

removeIfPresent :: FilePath -> IO ()
removeIfPresent path = do
  exists <- doesFileExist path
  when exists (removeFile path)
