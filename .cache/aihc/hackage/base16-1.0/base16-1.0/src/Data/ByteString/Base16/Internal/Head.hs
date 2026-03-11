{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.ByteString.Base16.Internal.Head
( encodeBase16_
, decodeBase16_
, decodeBase16Typed_
, decodeBase16Lenient_
, encodeBase16Short_
, decodeBase16Short_
, decodeBase16ShortTyped_
, decodeBase16ShortLenient_
) where


import Data.Base16.Types.Internal (Base16(..))
import qualified Data.ByteString as BS (empty)
import Data.ByteString.Internal
import qualified Data.ByteString.Short as SBS (empty)
import Data.ByteString.Base16.Internal.Utils
import Data.ByteString.Base16.Internal.W16.Loop
import qualified Data.ByteString.Base16.Internal.W16.ShortLoop as Short
import Data.ByteString.Short.Internal (ShortByteString(..))
import Data.Primitive.ByteArray
import Data.Text (Text)

import Foreign.Ptr
import Foreign.ForeignPtr

import GHC.Exts
import GHC.ForeignPtr

import System.IO.Unsafe


-- | Head of the base16 encoding loop - marshal data, assemble loops
--
encodeBase16_ :: ByteString -> ByteString
encodeBase16_ (PS sfp soff slen) =
    unsafeCreate dlen $ \dptr ->
      withForeignPtr sfp $ \sptr -> do
        innerLoop
          dptr
          (plusPtr sptr soff)
          (plusPtr sptr $ slen + soff)
  where
    !dlen = 2 * slen
{-# INLINE encodeBase16_ #-}

decodeBase16_ :: ByteString -> Either Text ByteString
decodeBase16_ (PS sfp soff slen)
  | slen == 0 = Right BS.empty
  | r /= 0 = Left "invalid bytestring size"
  | otherwise = unsafeDupablePerformIO $ do
    dfp <- mallocPlainForeignPtrBytes q
    withForeignPtr dfp $ \dptr ->
      withForeignPtr sfp $ \sptr ->
        decodeLoop
          dfp
          dptr
          (plusPtr sptr soff)
          (plusPtr sptr $ slen + soff)
          q
  where
    !q = slen `quot` 2
    !r = slen `rem` 2
{-# INLINE decodeBase16_ #-}

decodeBase16Typed_ :: Base16 ByteString -> ByteString
decodeBase16Typed_ (Base16 (PS sfp soff slen)) =
  unsafeCreate q $ \dptr ->
    withForeignPtr sfp $ \sptr ->
      decodeLoopTyped
        dptr
        (plusPtr sptr soff)
        (plusPtr sptr $ slen + soff)
  where
    !q = slen `quot` 2
{-# INLINE decodeBase16Typed_ #-}

decodeBase16Lenient_ :: ByteString -> ByteString
decodeBase16Lenient_ (PS sfp soff slen) =
  unsafeDupablePerformIO $ do
    dfp <- mallocPlainForeignPtrBytes q
    withForeignPtr dfp $ \dptr ->
      withForeignPtr sfp $ \sptr ->
        lenientLoop
          dfp
          dptr
          (plusPtr sptr soff)
          (plusPtr sptr $ slen + soff)
          0
  where
    !q = slen `quot` 2
{-# INLINE decodeBase16Lenient_ #-}

-- ---------------------------------------------------------------- --
-- Short encode/decode

encodeBase16Short_ :: ShortByteString -> ShortByteString
encodeBase16Short_ (SBS !ba#) = runShortST $ do
    dst <- newByteArray l'
    Short.innerLoop l dst (MutableByteArray (unsafeCoerce# ba#))
    unsafeFreezeByteArray dst
  where
    !l = I# (sizeofByteArray# ba#)
    !l' = l * 2
{-# INLINE encodeBase16Short_ #-}

decodeBase16Short_ :: ShortByteString -> Either Text ShortByteString
decodeBase16Short_ (SBS !ba#)
    | l == 0 = Right SBS.empty
    | r /= 0 = Left "invalid bytestring size"
    | otherwise = runDecodeST $ do
      dst <- newByteArray q
      Short.decodeLoop l dst (MutableByteArray (unsafeCoerce# ba#))
  where
    !l = I# (sizeofByteArray# ba#)
    !q = l `quot` 2
    !r = l `rem` 2
{-# INLINE decodeBase16Short_ #-}

decodeBase16ShortTyped_ :: Base16 ShortByteString -> ShortByteString
decodeBase16ShortTyped_ (Base16 (SBS !ba#)) = runDecodeST' $ do
    dst <- newByteArray q
    Short.decodeLoopTyped l dst (MutableByteArray (unsafeCoerce# ba#))
  where
    !l = I# (sizeofByteArray# ba#)
    !q = l `quot` 2
{-# INLINE decodeBase16ShortTyped_ #-}


decodeBase16ShortLenient_ :: ShortByteString -> ShortByteString
decodeBase16ShortLenient_ (SBS !ba#)
    | l == 0 = SBS.empty
    | otherwise = runShortST $ do
      dst <- newByteArray q
      q' <- Short.lenientLoop l dst (MutableByteArray (unsafeCoerce# ba#))
      !_ <- resizeMutableByteArray dst q'
      unsafeFreezeByteArray dst
  where
    !l = I# (sizeofByteArray# ba#)
    !q = l `quot` 2
{-# INLINE decodeBase16ShortLenient_ #-}
