{-# LANGUAGE CPP #-}

module PtrPoker.Compat.Text where

#if MIN_VERSION_text(2,0,0)
import qualified Data.Text.Array as TextArray
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Internal as TextInternal
import PtrPoker.Prelude

{-# INLINE destruct #-}
destruct :: (ByteArray# -> Int -> Int -> x) -> Text -> x
destruct k (TextInternal.Text (TextArray.ByteArray arr) off len) = k arr off len

{-# INLINE utf8EncodingSize #-}
utf8EncodingSize :: Text -> Int
utf8EncodingSize = destruct $ \_arr _off len -> len

{-# INLINEABLE encodeInUtf8 #-}
encodeInUtf8 :: Text -> ByteString
encodeInUtf8 t = TextEncoding.encodeUtf8 t

{-# INLINE pokeInUtf8 #-}
pokeInUtf8 :: Text -> Ptr Word8 -> IO (Ptr Word8)
pokeInUtf8 (TextInternal.Text arr off len) p =
  stToIO (TextArray.copyToPointer arr off p len) $> plusPtr p len
#else
import qualified Data.ByteString.Internal as ByteStringInternal
import qualified Data.Text.Array as TextArray
import qualified Data.Text.Internal as TextInternal
import qualified PtrPoker.Ffi as Ffi
import PtrPoker.Prelude

{-# INLINE destruct #-}
destruct :: (ByteArray# -> Int -> Int -> x) -> Text -> x
destruct k (TextInternal.Text (TextArray.Array arr) off len) =
  k arr off len

{-# INLINEABLE utf8EncodingSize #-}
utf8EncodingSize :: Text -> Int
utf8EncodingSize (TextInternal.Text (TextArray.Array arr) off len) =
  Ffi.countTextAllocationSize
    arr
    (fromIntegral off)
    (fromIntegral len)
    & unsafeDupablePerformIO
    & fromIntegral

{-# INLINEABLE encodeInUtf8 #-}
encodeInUtf8 :: Text -> ByteString
encodeInUtf8 (TextInternal.Text (TextArray.Array arr) off len) =
  if len == 0
    then mempty
    else ByteStringInternal.unsafeCreateUptoN (len * 3) $ \ptr -> do
      postPtr <- inline Ffi.encodeText ptr arr (fromIntegral off) (fromIntegral len)
      return (minusPtr postPtr ptr)

{-# INLINE pokeInUtf8 #-}
pokeInUtf8 :: Text -> Ptr Word8 -> IO (Ptr Word8)
pokeInUtf8 (TextInternal.Text (TextArray.Array arr) off len) p =
  Ffi.encodeText p arr (fromIntegral off) (fromIntegral len)
#endif
