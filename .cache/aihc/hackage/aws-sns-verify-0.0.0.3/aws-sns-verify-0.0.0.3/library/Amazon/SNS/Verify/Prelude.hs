module Amazon.SNS.Verify.Prelude
  ( module X
  , module Amazon.SNS.Verify.Prelude
  ) where

import Prelude as X

import Control.Error (ExceptT, throwE)
import Control.Exception as X (Exception)
import qualified Control.Exception
import Control.Monad as X (join, (<=<))
import Control.Monad.IO.Class as X (MonadIO, liftIO)
import Data.ByteString as X (ByteString)
import Data.Text as X (Text)
import Data.Traversable as X (for)

throwIO :: (MonadIO m, Exception e) => e -> m a
throwIO = liftIO . Control.Exception.throwIO

unTryIO :: (MonadIO m, Exception e) => (a -> e) -> Either a b -> m b
unTryIO e = either (throwIO . e) pure

unTryE :: Monad m => (a -> e) -> Either a b -> ExceptT e m b
unTryE e = either (throwE . e) pure

fromMaybeM :: Monad m => m a -> Maybe a -> m a
fromMaybeM f = maybe f pure
