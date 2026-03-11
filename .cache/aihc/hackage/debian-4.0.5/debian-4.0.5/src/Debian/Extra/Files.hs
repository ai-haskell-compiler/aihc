{-# LANGUAGE PackageImports #-}
-- |Domain independent functions used by the haskell-debian package.
module Debian.Extra.Files
    ( withTemporaryFile
    ) where

import "mtl" Control.Monad.Trans (MonadIO, liftIO)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hPutStr, hClose, openBinaryTempFile)

withTemporaryFile :: MonadIO m
                  => (FilePath -> m a)  -- ^ The function we want to pass a FilePath to
                  -> String             -- ^ The text that the file should contain
                  -> m a                -- ^ The function's return value
withTemporaryFile f text =
    do path <- liftIO writeTemporaryFile
       result <- f path
       liftIO $ removeFile path
       return result
    where
      writeTemporaryFile =
          do dir <- getTemporaryDirectory
             (path, h) <- openBinaryTempFile dir "wtf.tmp"
             hPutStr h text
             hClose h
             return path

-- Example: write the path of the temporary file and its contents into /tmp/result:
-- test =
--   withTemporaryFile f "Some text\n"
--   where f path = readFile path >>= return . (("Contents of " ++ path ++ ":\n") ++) >>= writeFile "/tmp/result"
