module Triangle (triangle, twice) where

import Foreign.C.Types (CInt (..))

foreign import ccall unsafe "aihc_cxx_triangle" c_triangle :: CInt -> CInt

foreign import ccall unsafe "aihc_objc_twice" c_twice :: CInt -> CInt

-- | The sum of the numbers up to the argument, computed in C++.
triangle :: Int -> Int
triangle count = fromIntegral (c_triangle (fromIntegral count))

-- | The argument times two, computed in Objective-C.
twice :: Int -> Int
twice value = fromIntegral (c_twice (fromIntegral value))
