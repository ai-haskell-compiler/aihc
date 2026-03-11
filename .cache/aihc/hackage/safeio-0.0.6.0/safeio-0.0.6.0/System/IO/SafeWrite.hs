module System.IO.SafeWrite
    ( withOutputFile
    , syncFile
    , allocateTempFile
    , finalizeTempFile
    ) where

import           System.FilePath (takeDirectory, takeBaseName)
import           Control.Monad.Catch (bracket, bracketOnError, MonadMask(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           System.IO (Handle, hClose, openTempFile)
import           System.Directory (renameFile, removeFile)

#ifndef WINDOWS
import           System.Posix.IO (defaultFileFlags, closeFd, OpenMode(..))
import qualified System.Posix.IO
import qualified System.Posix.Types
import           System.Posix.Unistd (fileSynchronise)
#endif


openFd :: FilePath -> OpenMode -> System.Posix.IO.OpenFileFlags -> IO System.Posix.Types.Fd
#if MIN_VERSION_base (4,18,0)
openFd = System.Posix.IO.openFd
#else
openFd fname mode flags = System.Posix.IO.openFd fname mode Nothing flags
#endif

-- | Sync a file to disk
--
-- On Windows, this is a fake function.
syncFile :: FilePath -- ^ File to sync
            -> IO ()
#ifndef WINDOWS
syncFile fname = do
    bracket (openFd fname ReadWrite defaultFileFlags)
        closeFd
        fileSynchronise
    -- The code below will not work on Windows
    bracket (openFd (takeDirectory fname) ReadOnly defaultFileFlags)
        closeFd
        fileSynchronise
#else
syncFile fname = return ()
#endif

-- | Variation of 'withFile' for output files.
--
-- Output is written to a temporary file. Once the action has completed, this
-- file is then sync'ed to disk (see |syncFile|) and renamed to its final
-- destination. In Posix, this is an atomic operation. If an exception is
-- raised, then the temporary output file will be deleted and not saved to
-- disk. Thus, the result file will either contain the complete result or will
-- be empty.
withOutputFile :: (MonadMask m, MonadIO m) =>
            FilePath -- ^ Final desired file path
            -> (Handle -> m a) -- ^ action to execute
            -> m a
withOutputFile finalname act =
    bracketOnError
        (liftIO $ allocateTempFile finalname)
        (liftIO . finalizeTempFile finalname False)
        (\tdata@(_, th) -> do
            r <- act th
            liftIO $ finalizeTempFile finalname True tdata
            return r)

allocateTempFile :: FilePath -> IO (FilePath, Handle)
allocateTempFile finalname = openTempFile (takeDirectory finalname) (takeBaseName finalname)

finalizeTempFile :: FilePath -> Bool -> (FilePath, Handle) -> IO ()
finalizeTempFile finalname ok (tname, th)
    | ok = do
        hClose th
        syncFile tname
        renameFile tname finalname
    | otherwise = do
        hClose th
        removeFile tname

