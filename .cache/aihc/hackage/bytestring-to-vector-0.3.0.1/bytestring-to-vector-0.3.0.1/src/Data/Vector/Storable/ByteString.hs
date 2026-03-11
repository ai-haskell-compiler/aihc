-- | Convert between @ByteString@ and @Vector.Storable@
-- without copying.
module Data.Vector.Storable.ByteString
    ( -- | See also the caveats mentioned in the package's
      -- top-level documentation.
      byteStringToVector
    , vectorToByteString
    ) where

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.Vector.Storable     as V

import Foreign.Storable
import Foreign.ForeignPtr

sizeOfElem :: (Storable a) => V.Vector a -> Int
sizeOfElem vec = sizeOf (undefined `asTypeOf` V.head vec)

-- | Convert a @'BS.ByteString'@ to a @'V.Vector'@.
--
-- This function can produce @'Vector'@s which do not obey
-- architectural alignment requirements.  On @x86@ this should
-- not be an issue.
byteStringToVector :: (Storable a) => BS.ByteString -> V.Vector a
byteStringToVector bs = vec where
    vec = V.unsafeFromForeignPtr (castForeignPtr fptr) (scale off) (scale len)
    (fptr, off, len) = BS.toForeignPtr bs
    scale = (`div` sizeOfElem vec)

-- | Convert a @'V.Vector'@ to a @'BS.ByteString'@.
vectorToByteString :: (Storable a) => V.Vector a -> BS.ByteString
vectorToByteString vec
  = BS.fromForeignPtr (castForeignPtr fptr) (scale off) (scale len) where
    (fptr, off, len) = V.unsafeToForeignPtr vec
    scale = (* sizeOfElem vec)
