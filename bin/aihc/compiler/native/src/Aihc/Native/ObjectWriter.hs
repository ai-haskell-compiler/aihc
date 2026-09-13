-- | Write an object to a file only after it assembled in full.
module Aihc.Native.ObjectWriter
  ( writeObjectFile,
  )
where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.ByteString.Lazy qualified as BL
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.FilePath (takeDirectory)
import System.IO (hClose, openBinaryTempFile)

-- | Publish the object only once the bytes exist. An existing file at the
-- destination stays untouched until then.
writeObjectFile :: FilePath -> BL.ByteString -> IO ()
writeObjectFile destination bytes = do
  createDirectoryIfMissing True directory
  bracket (openBinaryTempFile directory ".aihc-object") (\(path, handle) -> hClose handle >> removeIfPresent path) $ \(path, handle) -> do
    BL.hPut handle bytes
    hClose handle
    renameFile path destination
  where
    directory = takeDirectory destination

removeIfPresent :: FilePath -> IO ()
removeIfPresent path = do
  exists <- doesFileExist path
  when exists (removeFile path)
