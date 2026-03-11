{- |
   Module     : Development.Shake.Plus.File
   License    : MIT
   Stability  : experimental

File utilities in "Development.Shake" lifted to `MonadAction` and
well-typed `Path`s.
-}
module Development.Shake.Plus.File (
  copyFile
, copyFileChanged
, readFile'
, readFileLines
, readFileIn'
, writeFile'
, writeFileLines
, writeFileIn'
, writeFileChanged
, writeFileChangedIn
, removeFiles
, removeFilesAfter
) where

import           Control.Exception.Extra
import qualified Development.Shake
import           Development.Shake.Plus.Core
import           Path
import           RIO
import qualified RIO.Text                    as T

-- | Lifted version of `Development.Shake.copyFile` with well-typed filepaths.
copyFile :: (MonadAction m, Partial) => Path b File -> Path b' File -> m ()
copyFile x y = liftAction $ Development.Shake.copyFile' (toFilePath x) (toFilePath y)

-- | Lifted version of `Development.Shake.copyFileChanged'` with well-typed filepaths.
copyFileChanged :: (MonadAction m, Partial) => Path b File -> Path b' File -> m ()
copyFileChanged x y = liftAction $ Development.Shake.copyFileChanged (toFilePath x) (toFilePath y)

-- | Lifted version of `Development.Shake.readFile'` with a well-typed filepath.
readFile' :: (MonadAction m, Partial) => Path b File -> m Text
readFile' = liftAction . fmap T.pack . Development.Shake.readFile' . toFilePath

-- | Lifted version of `Development.Shake.readFileLines` a well-typed filepath.
readFileLines :: (MonadAction m, Partial) => Path b File -> m [Text]
readFileLines = liftAction . fmap (fmap T.pack) . Development.Shake.readFileLines . toFilePath

-- | Like `readFile'`, but with an argument for the parent directory. Used for symmetry with
-- the way `Development.Shake.getDirectoryFiles` takes arguments.
readFileIn' :: (MonadAction m, Partial) => Path b Dir -> Path Rel File -> m Text
readFileIn' x y = readFile' $ x </> y

-- | Lifted version of `Development.Shake.writeFile` a well-typed filepath.
writeFile' :: (MonadAction m, Partial) => Path b File -> Text -> m ()
writeFile' x y = liftAction $ Development.Shake.writeFile' (toFilePath x) (T.unpack y)

-- | Lifted version of `Development.Shake.writeFileLines` with a well-typed filepath..
writeFileLines :: (MonadAction m, Partial) => Path b File -> [Text] -> m ()
writeFileLines x y = liftAction $ Development.Shake.writeFileLines (toFilePath x) (fmap T.unpack y)

-- | Like `writeFile'`, but with an argument for the parent directory. Used for symmetry with
-- the way `Development.Shake.getDirectoryFiles` takes arguments.
writeFileIn' :: (MonadAction m, Partial) => Path b Dir -> Path Rel File -> Text -> m ()
writeFileIn' x y = writeFile' $ x </> y

-- | Lifted version of `Development.Shake.writeFileChanged` with a well-typed filepath.
writeFileChanged :: (MonadAction m, Partial) => Path b File -> Text -> m ()
writeFileChanged x y = liftAction $ Development.Shake.writeFileChanged (toFilePath x) (T.unpack y)

-- | Like `writeFileChanged'`, but with an argument for the parent directory. Used for symmetry with
-- the way `Development.Shake.getDirectoryFiles` takes arguments.
writeFileChangedIn :: (MonadAction m, Partial) => Path b Dir -> Path Rel File -> Text -> m ()
writeFileChangedIn x y = writeFileChanged $ x </> y

-- | Lifted version of `Development.Shake.removeFiles` that accepts a well-typed directory.
removeFiles :: MonadAction m => Path b Dir -> [FilePattern] -> m ()
removeFiles x y = liftAction . liftIO $ Development.Shake.removeFiles (toFilePath x) y

-- | Lifted version of `Development.Shake.removeFilesAfter` that accepts a well-typed directory.
removeFilesAfter :: MonadAction m => Path b Dir -> [FilePattern] -> m ()
removeFilesAfter x y = liftAction $ Development.Shake.removeFilesAfter (toFilePath x) y
