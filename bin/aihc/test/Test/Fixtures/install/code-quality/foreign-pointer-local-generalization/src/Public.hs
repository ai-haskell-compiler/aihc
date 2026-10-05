module Public (size) where

import Foreign.C.String (CString)
import Foreign.C.Types (CSize)
import Foreign.Ptr (Ptr, castPtr)

foreign import ccall unsafe "string.h strlen" c_strlen :: CString -> IO CSize

withPointer :: Ptr () -> (Ptr () -> IO b) -> IO b
withPointer pointer action = action pointer

-- The local binding generalizes over a type variable with the name of the
-- binder of the Ptr constructor. Its application to () must not change the
-- constructor type that the foreign argument adapter matches.
size :: Ptr () -> IO CSize
size pointer = withPointer pointer measure
  where
    measure string = c_strlen (castPtr string)
