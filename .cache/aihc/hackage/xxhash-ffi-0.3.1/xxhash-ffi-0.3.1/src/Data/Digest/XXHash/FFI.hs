{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Copyright:   (c) 2017 Henri Verroken
-- Licence:     BSD3
--
-- This module provides high-level helpers for the xxHash library,
-- see <https://xxhash.com>.
module Data.Digest.XXHash.FFI (
  -- * XXH3 interface
  XXH3 (..),

  -- * Deprecated interface
  XXHash (..),
) where

import Data.Digest.XXHash.FFI.C

import qualified Data.Array.Byte as A
import qualified Data.ByteString as BS
import Data.ByteString.Internal (accursedUnutterablePerformIO)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Digest.XXHash.Internal
import Data.Hashable
import qualified Data.Text.Array as TA
import qualified Data.Text.Internal as TS
import qualified Data.Text.Lazy as TL
import Data.Word (Word32, Word64)
import Foreign.C
import Foreign.Ptr (plusPtr)
import GHC.Exts (ByteArray#, Int (..), Ptr (..), byteArrayContents#, isByteArrayPinned#, isTrue#, sizeofByteArray#)
import GHC.IO

{-# INLINE useBS' #-}
useBS' :: BS.ByteString -> (CString -> CSize -> IO a) -> IO a
useBS' bs k = unsafeUseAsCStringLen bs $ \(ptr, len) -> k ptr (fromIntegral len)

{-# INLINE useBS #-}
useBS :: BS.ByteString -> (CString -> Int -> IO a) -> IO a
useBS bs k = unsafeUseAsCStringLen bs $ \(ptr, len) -> k ptr len

{-# INLINE useBA #-}
useBA :: A.ByteArray -> (CString -> Int -> IO a) -> IO a
useBA (A.ByteArray ba#) k = IO $ \s0 -> keepAliveUnlifted# ba# s0 $ unIO $ k (Ptr (byteArrayContents# ba#)) (I# (sizeofByteArray# ba#))

{-# INLINE isPinnedBA #-}
isPinnedBA :: A.ByteArray -> Bool
isPinnedBA (A.ByteArray ba#) = isTrue# (isByteArrayPinned# ba#)

{-# INLINE useTS #-}
useTS :: TS.Text -> (CString -> Int -> IO a) -> IO a
useTS ts@(TS.Text ba off len) k = IO $ \s0 ->
  keepAliveLifted# ba s0 $
    unIO $
      k
        (Ptr (byteArrayContents# (textArray ts)) `plusPtr` (off * textMult))
        (len * textMult)

{-# INLINE isPinnedTS #-}
isPinnedTS :: TS.Text -> Bool
isPinnedTS ts = isTrue# (isByteArrayPinned# (textArray ts))

{-# INLINE textArray #-}
textArray :: TS.Text -> ByteArray#
#if MIN_VERSION_text(2,0,0)
textArray (TS.Text (TA.ByteArray ba#) _ _) = ba#
#else
textArray (TS.Text (TA.Array ba#) _ _) = ba#
#endif

{-# INLINE textMult #-}
textMult :: Int
#if MIN_VERSION_text(2,0,0)
textMult = 1
#else
textMult = 2
#endif

-- | Class for hashable data types.
class XXHash t where
  -- | Calculate the 32-bit xxHash using a given seed.
  xxh32
    :: t
    -- ^ Data to hash
    -> Word32
    -- ^ Seed
    -> Word32
    -- ^ Resulting hash

  -- | Calculate the 64-bit xxHash using a given seed.
  xxh64
    :: t
    -- ^ Data to hash
    -> Word64
    -- ^ Seed
    -> Word64
    -- ^ Resulting hash

{-# DEPRECATED XXHash "Use new, XXH3-based functions instead" #-}

instance XXHash BS.ByteString where
  xxh32 bs seed = fromIntegral . accursedUnutterablePerformIO . useBS' bs $
    \ptr len -> c_xxh32 ptr len (fromIntegral seed)

  xxh64 bs seed = fromIntegral . accursedUnutterablePerformIO . useBS' bs $
    \ptr len -> c_xxh64 ptr len (fromIntegral seed)

instance XXHash BL.ByteString where
  xxh32 bs seed = fromIntegral . accursedUnutterablePerformIO $
    allocaXXH32State $ \state -> do
      c_xxh32_reset state (fromIntegral seed)
      mapM_ (update state) (BL.toChunks bs)
      c_xxh32_digest state
    where
      update state bs' = useBS' bs' $ c_xxh32_update state

  xxh64 bs seed = fromIntegral . accursedUnutterablePerformIO $
    allocaXXH64State $ \state -> do
      c_xxh64_reset state (fromIntegral seed)
      mapM_ (update state) (BL.toChunks bs)
      c_xxh64_digest state
    where
      update state bs' = useBS' bs' $ c_xxh64_update state

-- | A newtype over 'BS.ByteString' and `TS.Text` to provide convenient access
-- to the `XXH3` hash function via `Hashable` type class.
--
-- @since 0.3
newtype XXH3 a = XXH3 {unXXH3 :: a}
  deriving (Eq, Ord, Show)

instance Hashable (XXH3 BS.ByteString) where
  hashWithSalt salt (XXH3 bs) = fromIntegral . accursedUnutterablePerformIO . useBS bs $
    \ptr len ->
      (if len < 1000000 then c_xxh3_64bits_withSeed else c_xxh3_64bits_withSeed_safe)
        ptr
        (fromIntegral len)
        (fromIntegral salt)

instance Hashable (XXH3 BL.ByteString) where
  hashWithSalt salt (XXH3 bs) = fromIntegral . accursedUnutterablePerformIO $
    allocaXXH3State $ \state -> do
      initXXH3State state
      c_xxh3_64bits_reset_withSeed state (fromIntegral salt)
      mapM_ (update state) (BL.toChunks bs)
      c_xxh3_64bits_digest state
    where
      update state bs' = useBS bs' $ \ptr len ->
        (if len < 1000000 then c_xxh3_64bits_update else c_xxh3_64bits_update_safe)
          state
          ptr
          (fromIntegral len)

instance Hashable (XXH3 A.ByteArray) where
  hashWithSalt salt (XXH3 ba) =
    fromIntegral . accursedUnutterablePerformIO . useBA ba $
      \ptr len ->
        ( if len < 1000000 || not (isPinnedBA ba)
            then c_xxh3_64bits_withSeed
            else c_xxh3_64bits_withSeed_safe
        )
          ptr
          (fromIntegral len)
          (fromIntegral salt)

instance Hashable (XXH3 TS.Text) where
  hashWithSalt salt (XXH3 ts) = fromIntegral . accursedUnutterablePerformIO . useTS ts $
    \ptr len ->
      ( if len < 1000000 || not (isPinnedTS ts)
          then c_xxh3_64bits_withSeed
          else c_xxh3_64bits_withSeed_safe
      )
        ptr
        (fromIntegral len)
        (fromIntegral salt)

instance Hashable (XXH3 TL.Text) where
  hashWithSalt salt (XXH3 ts) = fromIntegral . accursedUnutterablePerformIO $
    allocaXXH3State $ \state -> do
      initXXH3State state
      c_xxh3_64bits_reset_withSeed state (fromIntegral salt)
      mapM_ (update state) (TL.toChunks ts)
      c_xxh3_64bits_digest state
    where
      update state ts' = useTS ts' $ \ptr len ->
        ( if len < 1000000 || not (isPinnedTS ts')
            then c_xxh3_64bits_update
            else c_xxh3_64bits_update_safe
        )
          state
          ptr
          (fromIntegral len)
