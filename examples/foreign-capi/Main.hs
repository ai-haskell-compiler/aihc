{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE MagicHash #-}

module Main where

import Foreign.C.Types (CDouble (..), CInt (..))
import GHC.Int (Int (..), Int32 (..))
import GHC.Prim
import GHC.Ptr (Ptr (..))
import GHC.Types (Double (..))
import System.IO (hPutBuf, stdout)

-- A capi import reaches its entity through the C API of its header rather
-- than through the platform ABI, so the entity may be something that has no
-- symbol of its own at all.

-- fmax is a function of the C library, but a C library is free to define it
-- as a macro or as a compiler builtin in math.h.
foreign import capi unsafe "math.h fmax" c_fmax :: CDouble -> CDouble -> CDouble

-- EXIT_SUCCESS and EXIT_FAILURE are macros. No call could reach them and no
-- link could find them.
foreign import capi unsafe "stdlib.h value EXIT_SUCCESS" c_exitSuccess :: CInt

foreign import capi unsafe "stdlib.h value EXIT_FAILURE" c_exitFailure :: CInt

-- isdigit is a macro in many C libraries and a function in others.
foreign import capi unsafe "ctype.h isdigit" c_isdigit :: CInt -> CInt

double :: Int# -> CDouble
double value = CDouble (D# (int2Double# value))

integer :: CDouble -> Int
integer (CDouble (D# value)) = I# (double2Int# value)

signed :: CInt -> Int
signed (CInt (I32# value)) = I# (int32ToInt# value)

main :: IO ()
main =
  if integer (c_fmax (double 3#) (double 7#)) == 7
    && signed c_exitSuccess == 0
    && signed c_exitFailure == 1
    && signed (c_isdigit (fromIntegral (fromEnum '7'))) /= 0
    && signed (c_isdigit (fromIntegral (fromEnum 'x'))) == 0
    then hPutBuf stdout (Ptr "ok\n"# :: Ptr ()) 3
    else hPutBuf stdout (Ptr "fail\n"# :: Ptr ()) 5
