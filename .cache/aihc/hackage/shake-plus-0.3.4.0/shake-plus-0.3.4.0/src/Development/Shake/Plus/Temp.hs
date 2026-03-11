{- |
   Module     : Development.Shake.Plus.Temp
   License    : MIT
   Stability  : experimental

Temp utilities in "Development.Shake" unlifted to `MonadUnliftAction`.
-}
module Development.Shake.Plus.Temp (
  withTempFile
, withTempDir
, withTempFileWithin
, withTempDirWithin
) where

import qualified Development.Shake
import           Development.Shake.Plus.Core
import           Path
import           RIO                         hiding (withTempFile)

-- | Unlifted version of `Development.Shake.withTempFile` with well-typed `Path`.
withTempFile :: (MonadUnliftAction m, MonadThrow m) => (Path Rel File -> m a) -> m a
withTempFile f = withRunInAction $ \run -> Development.Shake.withTempFile $ run . (f <=< parseRelFile)

-- | Unlifted version of `Development.Shake.withTempFile` with well-typed `Path`.
withTempDir :: (MonadUnliftAction m, MonadThrow m) => (Path Rel Dir -> m a) -> m a
withTempDir f = withRunInAction $ \run -> Development.Shake.withTempDir $ run . (f <=< parseRelDir)

-- | Unlifted version of `Development.Shake.withTempFileWithin` with well-typed `Path`s.
withTempFileWithin :: (MonadUnliftAction m, MonadThrow m) => Path b Dir -> (Path Rel File -> m a) -> m a
withTempFileWithin x f = withRunInAction $ \run -> Development.Shake.withTempFileWithin (toFilePath x) $ run . (f <=< parseRelFile)

-- | Unlifted version of `Development.Shake.withTempDirWithin` with well-typed `Path`s.
withTempDirWithin :: (MonadUnliftAction m, MonadThrow m) => Path b Dir -> (Path Rel Dir -> m a) -> m a
withTempDirWithin x f = withRunInAction $ \run -> Development.Shake.withTempDirWithin (toFilePath x) $ run . (f <=< parseRelDir)
