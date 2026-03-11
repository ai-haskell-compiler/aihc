module UnliftIO.Streams.File
  ( withFileAsInput
  , withFileAsOutput
  , withFileAsOutputExt
  ) where

import           Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import           Data.ByteString (ByteString)
import           System.IO (BufferMode, FilePath, IOMode)
import           System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams.File as SF

{-# INLINE withFileAsInput #-}
withFileAsInput :: (MonadUnliftIO m) => FilePath -> (InputStream ByteString -> m a) -> m a
withFileAsInput fp m =
  withRunInIO $ \io ->
    SF.withFileAsInput fp (io . m)

{-# INLINE withFileAsOutput #-}
withFileAsOutput :: (MonadUnliftIO m) => FilePath -> (OutputStream ByteString -> m a) -> m a
withFileAsOutput fp m =
  withRunInIO $ \io ->
    SF.withFileAsOutput fp (io . m)

{-# INLINE withFileAsOutputExt #-}
withFileAsOutputExt :: (MonadUnliftIO m) => FilePath -> IOMode -> BufferMode -> (OutputStream ByteString -> m a) -> m a
withFileAsOutputExt fp ioMode bufMode m =
  withRunInIO $ \io ->
    SF.withFileAsOutputExt fp ioMode bufMode (io . m)
