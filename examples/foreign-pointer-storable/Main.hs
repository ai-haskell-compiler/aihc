module Main (main) where

import Data.Word (Word64)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..), CLong (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, minusPtr)
import Foreign.Storable (peek, poke, sizeOf)

foreign import ccall unsafe "strtol" c_strtol :: CString -> Ptr CString -> CInt -> IO CLong

-- C writes the end pointer in the width of its own pointers, which is not
-- the width of a Haskell word on a 32-bit platform. The slot starts as all
-- ones, so a read that is wider than the pointer shows the difference.
main :: IO ()
main =
  withCString "123abc" $ \text ->
    alloca $ \end -> do
      poke (castPtr end) (maxBound :: Word64)
      value <- c_strtol text end 10
      stop <- peek end
      print value
      print (stop `minusPtr` text)
      print (sizeOf text `elem` [4, 8])
