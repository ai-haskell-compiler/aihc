module Triangle (triangle) where

import Foreign.C.Types (CInt (..))

foreign import ccall unsafe "aihc_cxx_triangle" c_triangle :: CInt -> CInt

-- | The sum of the numbers up to the argument, computed in C++.
triangle :: Int -> Int
triangle count = fromIntegral (c_triangle (fromIntegral count))
