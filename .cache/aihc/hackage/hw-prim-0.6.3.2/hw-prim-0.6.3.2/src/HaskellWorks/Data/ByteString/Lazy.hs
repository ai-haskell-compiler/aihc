{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.ByteString.Lazy
  ( ToLazyByteString(..)
  , resegment
  , resegmentPadded
  , rechunk
  , rechunkPadded
  , hGetContentsChunkedBy
  ) where

import Data.Word
import HaskellWorks.Data.ByteString (ToByteString (..))

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Internal      as BS
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Internal as LBS
import qualified Data.Vector.Storable          as DVS
import qualified HaskellWorks.Data.ByteString  as BS
import qualified System.IO                     as IO
import qualified System.IO.Unsafe              as IO

class ToLazyByteString a where
  toLazyByteString :: a -> LBS.ByteString

instance ToLazyByteString LBS.ByteString where
  toLazyByteString = id
  {-# INLINE toLazyByteString #-}

instance ToLazyByteString (DVS.Vector Word8) where
  toLazyByteString = LBS.fromStrict . toByteString
  {-# INLINE toLazyByteString #-}

instance ToLazyByteString (DVS.Vector Word16) where
  toLazyByteString = LBS.fromStrict . toByteString
  {-# INLINE toLazyByteString #-}

instance ToLazyByteString (DVS.Vector Word32) where
  toLazyByteString = LBS.fromStrict . toByteString
  {-# INLINE toLazyByteString #-}

instance ToLazyByteString (DVS.Vector Word64) where
  toLazyByteString = LBS.fromStrict . toByteString
  {-# INLINE toLazyByteString #-}

instance ToLazyByteString [DVS.Vector Word8] where
  toLazyByteString vs = LBS.fromChunks (toByteString <$> vs)
  {-# INLINE toLazyByteString #-}

instance ToLazyByteString [DVS.Vector Word16] where
  toLazyByteString vs = LBS.fromChunks (toByteString <$> vs)
  {-# INLINE toLazyByteString #-}

instance ToLazyByteString [DVS.Vector Word32] where
  toLazyByteString vs = LBS.fromChunks (toByteString <$> vs)
  {-# INLINE toLazyByteString #-}

instance ToLazyByteString [DVS.Vector Word64] where
  toLazyByteString vs = LBS.fromChunks (toByteString <$> vs)
  {-# INLINE toLazyByteString #-}

resegment :: Int -> LBS.ByteString -> LBS.ByteString
resegment multiple = LBS.fromChunks . BS.resegment multiple . LBS.toChunks

resegmentPadded :: Int -> LBS.ByteString -> LBS.ByteString
resegmentPadded multiple = LBS.fromChunks . BS.resegmentPadded multiple . LBS.toChunks

rechunk :: Int -> LBS.ByteString -> LBS.ByteString
rechunk multiple = LBS.fromChunks . BS.rechunk multiple . LBS.toChunks

rechunkPadded :: Int -> LBS.ByteString -> LBS.ByteString
rechunkPadded multiple = LBS.fromChunks . BS.rechunkPadded multiple . LBS.toChunks

hGetContentsChunkedBy :: Int -> IO.Handle -> IO LBS.ByteString
hGetContentsChunkedBy k h = lazyRead
  where lazyRead = IO.unsafeInterleaveIO loop
        loop = do
            c <- BS.createAndTrim k $ \p -> IO.hGetBuf h p k
            if BS.null c
              then IO.hClose h >> return LBS.Empty
              else LBS.Chunk c <$> lazyRead
