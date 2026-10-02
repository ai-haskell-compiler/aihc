{-# LANGUAGE ForeignFunctionInterface #-}

module Main (main) where

import Foreign.C.Types (CInt (..), CUInt (..))
import Words (greeting)

foreign import ccall unsafe "abs" c_abs :: CInt -> IO CInt

foreign import ccall safe "abs" c_abs_safe :: CInt -> IO CInt

foreign import ccall unsafe "srand" c_srand :: CUInt -> IO ()

main :: IO ()
main = do
  c_srand 7
  result <- c_abs (-42)
  safeResult <- c_abs_safe (-43)
  if result == 42 && safeResult == 43
    then putStrLn (greeting "build")
    else error "foreign call check failed"
