module UnliftIO.Streams.ByteString
  ( lines
  ) where

import           Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import           Data.ByteString (ByteString)
import           Prelude hiding (lines)
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams.ByteString as SB

{-# INLINE lines #-}
lines :: (MonadUnliftIO m) => InputStream ByteString -> m (InputStream ByteString)
lines = liftIO . SB.lines
