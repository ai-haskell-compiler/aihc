{-# LANGUAGE CPP #-}

module IsomorphismClass.TextCompat.Array where

import qualified Data.ByteString.Short
import qualified Data.ByteString.Short.Internal
import qualified Data.Primitive.ByteArray
import Data.Text.Array
import GHC.Exts (ByteArray#)

{-# INLINE toUnliftedByteArray #-}
toUnliftedByteArray :: Array -> ByteArray#
#if MIN_VERSION_text(2,0,0)
toUnliftedByteArray (ByteArray a) = a
#else
toUnliftedByteArray (Array a) = a
#endif

{-# INLINE fromUnliftedByteArray #-}
fromUnliftedByteArray :: ByteArray# -> Array
#if MIN_VERSION_text(2,0,0)
fromUnliftedByteArray = ByteArray
#else
fromUnliftedByteArray = Array
#endif

{-# INLINE fromShortByteString #-}
fromShortByteString :: Data.ByteString.Short.ShortByteString -> Array
fromShortByteString (Data.ByteString.Short.Internal.SBS arr) =
  fromUnliftedByteArray arr

{-# INLINE toShortByteString #-}
toShortByteString :: Array -> Data.ByteString.Short.ShortByteString
toShortByteString a =
  Data.ByteString.Short.Internal.SBS (toUnliftedByteArray a)

{-# INLINE fromByteArray #-}
fromByteArray :: Data.Primitive.ByteArray.ByteArray -> Array
fromByteArray (Data.Primitive.ByteArray.ByteArray arr) =
  fromUnliftedByteArray arr

{-# INLINE toByteArray #-}
toByteArray :: Array -> Data.Primitive.ByteArray.ByteArray
toByteArray a =
  Data.Primitive.ByteArray.ByteArray (toUnliftedByteArray a)
