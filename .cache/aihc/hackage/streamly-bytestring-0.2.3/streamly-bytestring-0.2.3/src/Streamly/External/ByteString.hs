{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}

module Streamly.External.ByteString
  ( toArray
  , fromArray

  , reader

  , writeN
  , write

  -- Deprecated
  , read
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peek)
import GHC.Exts
    ( Addr#
    , MutableByteArray#
    , RealWorld
    , byteArrayContents#
    , minusAddr#
    , plusAddr#
    , unsafeCoerce#
    )
import GHC.ForeignPtr (ForeignPtr(..), ForeignPtrContents(..))
import GHC.Int (Int(..))
import GHC.Ptr (Ptr(..), nullPtr, plusPtr)
import Streamly.Data.Fold (Fold)
import Streamly.Data.Unfold (Unfold, lmap)

-- Internal imports
import Data.ByteString.Internal (ByteString(..))
import Streamly.Internal.System.IO (unsafeInlineIO)

import qualified Streamly.Data.Array as Array
import qualified Streamly.Internal.Data.Unfold as Unfold (fold, mkUnfoldrM)

#if !(MIN_VERSION_bytestring(0,11,0))
import GHC.ForeignPtr (plusForeignPtr)
#endif

#if MIN_VERSION_streamly_core(0,2,0)
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.Data.MutByteArray (MutByteArray(..))
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutByteArray as MutBA
import qualified Streamly.Internal.Data.Stream as StreamD (Step(Yield))
#else
import Streamly.Internal.Data.Array.Type (Array(..))
import Streamly.Internal.Data.Unboxed (MutableByteArray(..))
import qualified Streamly.Internal.Data.Unboxed as MutBA
import qualified Streamly.Internal.Data.Stream.StreamD as StreamD (Step(Yield))
#endif

import Prelude hiding (read)

#if MIN_VERSION_streamly_core(0,2,0)
#define MUT_BYTE_ARRAY MutByteArray
#else
#define MUT_BYTE_ARRAY MutableByteArray
#endif

#if MIN_VERSION_streamly_core(0,2,2)
#define NIL MutBA.empty
#else
#define NIL MutBA.nil
#endif

#if MIN_VERSION_bytestring(0,11,0)
#define CONSTRUCTOR(a, b, c) BS a c
#define WHEN_0_10_12(x)
#else
#define CONSTRUCTOR(a, b, c) PS a b c
#define WHEN_0_10_12(x) x
#endif


{-# INLINE ensurePinned #-}
ensurePinned :: Array a -> IO (Array a)
{-# INLINE pinnedCreateOf #-}
pinnedCreateOf :: MonadIO m => Int -> Fold m Word8 (Array Word8)
{-# INLINE pinnedCreate #-}
pinnedCreate :: MonadIO m => Fold m Word8 (Array Word8)

#if MIN_VERSION_streamly_core(0,3,0)
ensurePinned = Array.pin
pinnedCreateOf = Array.createOf'
pinnedCreate = Array.create'
#elif MIN_VERSION_streamly_core(0,2,2)
ensurePinned = Array.pin
pinnedCreateOf = Array.pinnedCreateOf
pinnedCreate = Array.pinnedCreate
#elif MIN_VERSION_streamly_core(0,2,0)
ensurePinned = Array.pin
pinnedCreateOf = Array.pinnedWriteN
pinnedCreate = Array.pinnedWrite
#else
ensurePinned = pure
pinnedCreateOf = Array.writeN
pinnedCreate = Array.write
#endif

#if MIN_VERSION_streamly_core(0,3,0)
#define CREATE_N Array.createOf
#else
#define CREATE_N Array.writeN
#endif

{-# INLINE mutableByteArrayContents# #-}
mutableByteArrayContents# :: MutableByteArray# RealWorld -> Addr#
mutableByteArrayContents# marr# = byteArrayContents# (unsafeCoerce# marr#)

-- | Helper function that creates a ForeignPtr
{-# INLINE makeForeignPtr #-}
makeForeignPtr :: MUT_BYTE_ARRAY -> Int -> ForeignPtr a
makeForeignPtr (MUT_BYTE_ARRAY marr#) (I# off#) =
    ForeignPtr
        (mutableByteArrayContents# marr# `plusAddr#` off#)
        (PlainPtr marr#)

-- | Convert a 'ByteString' to an array of 'Word8'. It can be done in constant
-- time only for GHC allocated memory. For foreign allocator allocated memory
-- there is a copy involved.
{-# INLINE toArray #-}
toArray :: ByteString -> Array Word8
toArray (CONSTRUCTOR((ForeignPtr addr# _), _, _))
    | Ptr addr# == nullPtr = Array NIL 0 0
toArray (CONSTRUCTOR((ForeignPtr addr# (PlainPtr marr#)), off0, len)) =
    let off = I# (addr# `minusAddr#` mutableByteArrayContents# marr#)
                  WHEN_0_10_12(+ off0)
     in Array (MUT_BYTE_ARRAY marr#) off (off + len)
toArray (CONSTRUCTOR(fptr, off, len)) =
    unsafeInlineIO
        $ withForeignPtr (fptr WHEN_0_10_12(`plusForeignPtr` off))
        $ Unfold.fold (CREATE_N len) generator

    where

    generator =
        Unfold.mkUnfoldrM
            (\ptr -> flip StreamD.Yield (ptr `plusPtr` 1) <$> peek ptr)

-- | Convert an array of 'Word8' to a 'ByteString'.
--
-- Please ensure that the array is pinned when using this function.

-- If the array is pinned, the operation is performed in constant time. Whereas
-- for an unpinned array a copy is involved to pin it.
--
{-# INLINE fromArray #-}
fromArray :: Array Word8 -> ByteString
fromArray arr
    | aLen == 0 = mempty
    | otherwise = unsafeInlineIO $ do
        Array{..} <- ensurePinned arr
        pure $ CONSTRUCTOR((makeForeignPtr arrContents arrStart), 0, aLen)

    where

    aLen = arrEnd arr - arrStart arr

-- | Unfold a strict ByteString to a stream of Word8.
{-# INLINE reader #-}
reader :: Monad m => Unfold m ByteString Word8
reader = lmap toArray Array.reader

-- | Fold a stream of Word8 to a strict ByteString of given size in bytes.
{-# INLINE writeN #-}
writeN :: MonadIO m => Int -> Fold m Word8 ByteString
writeN i = fromArray <$> pinnedCreateOf i

-- | Fold a stream of Word8 to a strict ByteString of appropriate size.
{-# INLINE write #-}
write :: MonadIO m => Fold m Word8 ByteString
write = fromArray <$> pinnedCreate

--------------------------------------------------------------------------------
-- Deprecated
--------------------------------------------------------------------------------

{-# DEPRECATED read "Please use reader instead." #-}
{-# INLINE read #-}
read :: Monad m => Unfold m ByteString Word8
read = reader
