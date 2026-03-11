module UnliftIO.Streams.Text
  ( decodeUtf8
  , decodeUtf8With
  , encodeUtf8
  ) where

import           Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Data.Text.Encoding.Error (OnDecodeError)
import           System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams.Text as ST

{-# INLINE decodeUtf8 #-}
decodeUtf8 :: (MonadUnliftIO m) => InputStream ByteString -> m (InputStream Text)
decodeUtf8 = liftIO . ST.decodeUtf8

{-# INLINE decodeUtf8With #-}
decodeUtf8With :: (MonadUnliftIO m) => OnDecodeError -> InputStream ByteString -> m (InputStream Text)
decodeUtf8With onDecodeError = liftIO . ST.decodeUtf8With onDecodeError

{-# INLINE encodeUtf8 #-}
encodeUtf8 :: (MonadUnliftIO m) => OutputStream ByteString -> m (OutputStream Text)
encodeUtf8 = liftIO . ST.encodeUtf8
