module System.IO.Temp
    ( createTempDirectory
    , openBinaryTempFile
    , openTempFile
    ) where

-- NB: this module was extracted directly from "Distribution/Simple/Utils.hs"
-- in a Cabal tree whose most recent commit was on Sun Oct 10 22:00:26
--
-- The files in the Distribution/Compat tree are exact copies of the
-- corresponding file in the Cabal checkout.

import Control.Monad ( when )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Resource ( MonadResource, ReleaseKey, allocate )
import System.Directory
        ( doesDirectoryExist, doesFileExist, getTemporaryDirectory
        , removeDirectory, removeDirectoryRecursive, removeFile )
import System.IO ( Handle )

import qualified Distribution.Compat.TempFile as Compat

-- | Create a temporary directory. The directory will be deleted if empty
-- when the resource is released. If a parent directory is supplied, the
-- temporary directory will be created there; otherwise, it will be created
-- in the system temporary directory returned by 'getTemporaryDirectory'.
createTempDirectory :: MonadResource m
                    => Maybe FilePath  -- ^ optional parent directory
                    -> String
                    -- ^ filename template; for security, a random number
                    -- will be inserted between the filename and any
                    -- extension.
                    -> m (ReleaseKey, FilePath)
createTempDirectory mDir tmpl = do
    dir <- resolveTempDir mDir
    allocate (Compat.createTempDirectory dir tmpl) removeDirectoryRecursive

-- | Open a temporary file in binary mode. The file will be readable and
-- writeable, but only by the current user. The file will be deleted when
-- the resource is released, if it still exists. If a parent directory is
-- supplied, the file will be created there; otherwise, it will be created
-- in the system temporary directory returned by 'getTemporaryDirectory'.
openBinaryTempFile :: MonadResource m
                   => Maybe FilePath  -- ^ optional parent directory
                   -> String
                   -- ^ filename template; for security, a random number
                   -- will be inserted between the filename and any
                   -- extension.
                   -> m (ReleaseKey, FilePath, Handle)
openBinaryTempFile mDir tmpl = do
    dir <- resolveTempDir mDir
    (key, (path, h)) <- allocate (Compat.openBinaryTempFile dir tmpl)
                                 (removeFileIfExists . fst)
    return (key, path, h)

-- | Open a temporary file. The file will be readable and writeable, but
-- only by the current user. The file will be deleted when the resource is
-- released, if it still exists. If a parent directory is supplied, the
-- file will be created there; otherwise, it will be created in the system
-- temporary directory returned by 'getTemporaryDirectory'.
openTempFile :: MonadResource m
             => Maybe FilePath  -- ^ optional parent directory
             -> String
             -- ^ filename template; for security, a random number will be
             -- inserted between the filename and any extension.
             -> m (ReleaseKey, FilePath, Handle)
openTempFile mDir tmpl = do
    dir <- resolveTempDir mDir
    (key, (path, h)) <- allocate (Compat.openTempFile dir tmpl)
                                 (removeFileIfExists . fst)
    return (key, path, h)

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists path = do
    exists <- doesFileExist path
    when exists $ removeFile path

resolveTempDir :: MonadIO m => Maybe FilePath -> m FilePath
resolveTempDir = maybe (liftIO getTemporaryDirectory) return
