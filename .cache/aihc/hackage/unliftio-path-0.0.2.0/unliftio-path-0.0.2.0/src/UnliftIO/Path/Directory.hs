{- |
   Module     : UnliftIO.Path.Directory
   License    : MIT
   Stability  : experimental

`UnliftIO.Directory lifted to `Path`.
-}
module UnliftIO.Path.Directory (
  createDirectory
, createDirectoryIfMissing
, removeDirectory
, removeDirectoryRecursive
, removePathForcibly
, renameDirectory
, listDirectory
, getDirectoryContents
, getCurrentDirectory
, setCurrentDirectory
, withCurrentDirectory
, getHomeDirectory
, getXdgDirectory
, UnliftIO.Directory.XdgDirectory(..)
, getAppUserDataDirectory
, getUserDocumentsDirectory
, getTemporaryDirectory
, removeFile
, renameFile
, renamePath
, copyFile
, copyFileWithMetadata
, makeAbsoluteDir
, makeAbsoluteFile
, makeRelativeToCurrentDirectoryDir
, makeRelativeToCurrentDirectoryFile
, findExecutable
, findExecutables
, UnliftIO.Directory.exeExtension
, getFileSize
, doesPathExist
, doesFileExist
, doesDirectoryExist
, pathIsSymbolicLink
, UnliftIO.Directory.Permissions
, UnliftIO.Directory.emptyPermissions
, UnliftIO.Directory.readable
, UnliftIO.Directory.writable
, UnliftIO.Directory.executable
, UnliftIO.Directory.searchable
, UnliftIO.Directory.setOwnerReadable
, UnliftIO.Directory.setOwnerWritable
, UnliftIO.Directory.setOwnerExecutable
, UnliftIO.Directory.setOwnerSearchable
, getPermissions
, setPermissions
, copyPermissions
, getAccessTime
, getModificationTime
, setAccessTime
, setModificationTime
) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Time
import Path
import UnliftIO
import qualified UnliftIO.Directory

-- Lifted `UnliftIO.Directory.createDirectory`.
createDirectory :: MonadIO m => Path b Dir -> m ()
createDirectory = UnliftIO.Directory.createDirectory . toFilePath

-- Lifted `UnliftIO.Directory.createDirectoryIfMissing`.
createDirectoryIfMissing :: MonadIO m => Bool -> Path b Dir -> m ()
createDirectoryIfMissing b = UnliftIO.Directory.createDirectoryIfMissing b . toFilePath

-- Lifted `UnliftIO.Directory.removeDirectory`.
removeDirectory :: MonadIO m => Path b Dir -> m ()
removeDirectory = UnliftIO.Directory.removeDirectory . toFilePath

-- Lifted `UnliftIO.Directory.removeDirectoryRecursive`.
removeDirectoryRecursive :: MonadIO m => Path b Dir -> m ()
removeDirectoryRecursive = UnliftIO.Directory.removeDirectoryRecursive . toFilePath

-- Lifted `UnliftIO.Directory.removePathForcibly`.
removePathForcibly :: MonadIO m => Path b t -> m ()
removePathForcibly = UnliftIO.Directory.removePathForcibly . toFilePath

-- Lifed `UnliftIO.Directory.renameDirectory`.
renameDirectory :: MonadIO m => Path b Dir -> Path b' Dir -> m ()
renameDirectory x y = UnliftIO.Directory.renameDirectory (toFilePath x) (toFilePath y)

getDirectoryContents_ :: (MonadThrow m, MonadIO m) => Path b Dir -> m ([Path Rel Dir], [Path Rel File])
getDirectoryContents_ d = do
  xs   <- UnliftIO.Directory.listDirectory . toFilePath $ d
  dirs <- mapM parseRelDir =<< filterM UnliftIO.Directory.doesDirectoryExist xs
  fils <- mapM parseRelFile =<< filterM UnliftIO.Directory.doesFileExist xs
  return (dirs, fils)

-- Lifted `UnliftIO.Directory.listDirectory`.
listDirectory :: (MonadThrow m, MonadIO m) => Path b Dir -> m ([Path Rel Dir], [Path Rel File])
listDirectory = getDirectoryContents_

-- Lifted `UnliftIO.Directory.getDirectoryContents`.
getDirectoryContents :: (MonadThrow m, MonadIO m) => Path b Dir -> m ([Path Rel Dir], [Path Rel File])
getDirectoryContents = getDirectoryContents_

-- Lifted `UnliftIO.Directory.getCurrentDirectory`.
getCurrentDirectory :: (MonadThrow m, MonadIO m) => m (Path Abs Dir)
getCurrentDirectory = UnliftIO.Directory.getCurrentDirectory >>= parseAbsDir

-- Lifted `UnliftIO.Directory.setCurrentDirectory`.
setCurrentDirectory :: MonadIO m => Path Abs Dir -> m ()
setCurrentDirectory = UnliftIO.Directory.setCurrentDirectory . toFilePath

-- Lifted `UnliftIO.Directory.withCurrentDirectory`.
withCurrentDirectory :: (MonadThrow m, MonadUnliftIO m) => Path Abs Dir -> m a -> m a
withCurrentDirectory = UnliftIO.Directory.withCurrentDirectory . toFilePath

-- Lifted `UnliftIO.Directory.getHomeDirectory`.
getHomeDirectory :: (MonadThrow m, MonadIO m) => m (Path Abs Dir)
getHomeDirectory = UnliftIO.Directory.getHomeDirectory >>= parseAbsDir

-- Lifted `UnliftIO.Directory.getXdgDirectory`.
getXdgDirectory :: (MonadThrow m, MonadIO m) => UnliftIO.Directory.XdgDirectory -> Path Rel Dir -> m (Path Abs Dir)
getXdgDirectory x = UnliftIO.Directory.getXdgDirectory x . toFilePath >=> parseAbsDir

-- Lifted `UnliftIO.Directory.getAppUserDataDirectory`.
getAppUserDataDirectory :: (MonadThrow m, MonadIO m) => Path Rel Dir -> m (Path Abs Dir)
getAppUserDataDirectory = UnliftIO.Directory.getAppUserDataDirectory . toFilePath >=> parseAbsDir

-- Lifted `UnliftIO.Directory.getUserDocumentsDirectory`.
getUserDocumentsDirectory :: (MonadThrow m, MonadIO m) => m (Path Abs Dir)
getUserDocumentsDirectory = UnliftIO.Directory.getUserDocumentsDirectory >>= parseAbsDir

-- Lifted `UnliftIO.Directory.getTemporaryDirectory`.
getTemporaryDirectory :: (MonadThrow m, MonadIO m) => m (Path Abs Dir)
getTemporaryDirectory = UnliftIO.Directory.getTemporaryDirectory >>= parseAbsDir

-- Lifted `UnliftIO.Directory.removeFile`.
removeFile :: MonadIO m => Path b File -> m ()
removeFile = UnliftIO.Directory.removeFile . toFilePath

-- Lifted `UnliftIO.Directory.renameFile`.
renameFile :: MonadIO m => Path b File -> Path b' File -> m ()
renameFile x y = UnliftIO.Directory.renameFile (toFilePath x) (toFilePath y)

-- Lifted `UnliftIO.Directory.renamePath`.
renamePath :: MonadIO m => Path b t -> Path b' t -> m ()
renamePath x y = UnliftIO.Directory.renamePath (toFilePath x) (toFilePath y)

-- Lifted 'UnliftIO.Directory.copyFile'.
copyFile :: MonadIO m => Path b File -> Path b' File -> m ()
copyFile x y = UnliftIO.Directory.copyFile (toFilePath x) (toFilePath y)

-- Lifted 'UnliftIO.Directory.copyFileWithMetadata`.
copyFileWithMetadata :: MonadIO m => Path b File -> Path b' File -> m ()
copyFileWithMetadata x y = UnliftIO.Directory.copyFile (toFilePath x) (toFilePath y)

-- Lifted `UnliftIO.Directory.makeAbsolute`, `Dir` specific. Takes only `Path Rel Dirs`s.
makeAbsoluteDir :: (MonadThrow m, MonadIO m) => Path Rel Dir -> m (Path Abs Dir)
makeAbsoluteDir = UnliftIO.Directory.makeAbsolute . toFilePath >=> parseAbsDir

-- Lifted `UnliftIO.Directory.makeAbsolute`, `File` specific. Takes only `Path Rel File`s.
makeAbsoluteFile :: (MonadThrow m, MonadIO m) => Path Rel File -> m (Path Abs File)
makeAbsoluteFile = UnliftIO.Directory.makeAbsolute . toFilePath >=> parseAbsFile

-- Lifted `UnliftIO.Directory.makeRelativeToCurrentDirectory`, `Dir` specific. Takes only `Path Abs Dir`s.
makeRelativeToCurrentDirectoryDir :: (MonadThrow m, MonadIO m) => Path Abs Dir -> m (Path Rel Dir)
makeRelativeToCurrentDirectoryDir = UnliftIO.Directory.makeRelativeToCurrentDirectory . toFilePath >=> parseRelDir

-- Lifted `UnliftIO.Directory.makeRelativeToCurrentDirectory`, `File` specific. Takes only `Path Abs File`s.
makeRelativeToCurrentDirectoryFile :: (MonadThrow m, MonadIO m) => Path Abs File -> m (Path Rel File)
makeRelativeToCurrentDirectoryFile = UnliftIO.Directory.makeRelativeToCurrentDirectory . toFilePath >=> parseRelFile

-- Lifted `UnliftIO.Directory.findExecutable`.
findExecutable :: (MonadThrow m, MonadIO m) => String -> m (Maybe (Path Abs File))
findExecutable = UnliftIO.Directory.findExecutable >=> mapM parseAbsFile

-- Lifted `UnliftIO.Directory.findExecutables`.
findExecutables :: (MonadThrow m, MonadIO m) => String -> m [Path Abs File]
findExecutables = UnliftIO.Directory.findExecutables >=> mapM parseAbsFile

-- Lifted `UnliftIO.Directory.getFileSize`.
getFileSize :: MonadIO m => Path b File -> m Integer
getFileSize = UnliftIO.Directory.getFileSize . toFilePath

-- Lifted `UnliftIO.Directory.doesPathExist`.
doesPathExist :: MonadIO m => Path b t -> m Bool
doesPathExist = UnliftIO.Directory.doesPathExist . toFilePath

-- Lifted `UnliftIO.Directory.doesFileExist`.
doesFileExist :: MonadIO m => Path b File -> m Bool
doesFileExist = UnliftIO.Directory.doesFileExist . toFilePath

-- Lifted `UnliftIO.Directory.doesDirectoryExist`.
doesDirectoryExist :: MonadIO m => Path b Dir -> m Bool
doesDirectoryExist = UnliftIO.Directory.doesDirectoryExist . toFilePath

-- Lifted `UnliftIO.Directory.pathIsSymbolicLink`.
pathIsSymbolicLink :: MonadIO m => Path b t -> m Bool
pathIsSymbolicLink = UnliftIO.Directory.pathIsSymbolicLink . toFilePath

-- Lifted `UnliftiO.Directory.getPermissions`.
getPermissions :: MonadIO m => Path b t -> m UnliftIO.Directory.Permissions
getPermissions = UnliftIO.Directory.getPermissions . toFilePath

-- Lifted `UnliftiO.Directory.getPermissions`.
setPermissions :: MonadIO m => Path b t -> UnliftIO.Directory.Permissions -> m ()
setPermissions = UnliftIO.Directory.setPermissions . toFilePath

-- Lifted `UnliftiO.Directory.copyPermissions`.
copyPermissions :: MonadIO m => Path b t -> Path b' t' -> m ()
copyPermissions x y = UnliftIO.Directory.copyPermissions (toFilePath x) (toFilePath y)

-- Lifted `UnliftIO.Directory.getAccessTime`.
getAccessTime :: MonadIO m => Path b t -> m UTCTime
getAccessTime = UnliftIO.Directory.getAccessTime . toFilePath

-- Lifted `UnliftIO.Directory.getModificationTime`.
getModificationTime :: MonadIO m => Path b t -> m UTCTime
getModificationTime = UnliftIO.Directory.getModificationTime . toFilePath

-- Lifted `UnliftIO.Directory.setAccessTime`.
setAccessTime :: MonadIO m => Path b t -> UTCTime -> m ()
setAccessTime x = UnliftIO.Directory.setAccessTime (toFilePath x)

-- Lifted `UnliftIO.Directory.setModificationTime`.
setModificationTime :: MonadIO m => Path b t -> UTCTime -> m ()
setModificationTime x = UnliftIO.Directory.setModificationTime (toFilePath x)

