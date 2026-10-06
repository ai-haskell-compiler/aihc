module Main (main) where

import Foreign.C.Types (CSize (..))
import Foreign.ForeignPtr (FinalizerPtr, finalizeForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek, poke)

foreign import ccall unsafe "malloc" c_malloc :: CSize -> IO (Ptr a)

-- A finalizer is the address of a C function, which is data to a wasm
-- target until the import says that it is a function.
foreign import ccall unsafe "&free" p_free :: FinalizerPtr a

main :: IO ()
main = do
  pointer <- c_malloc 16
  managed <- newForeignPtr p_free pointer
  withForeignPtr managed $ \raw -> do
    poke (castPtr raw) (42 :: Int)
    value <- peek (castPtr raw) :: IO Int
    print value
  finalizeForeignPtr managed
  putStrLn "finalized"
