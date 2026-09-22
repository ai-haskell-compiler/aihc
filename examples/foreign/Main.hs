{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}

-- Foreign calls through the platform ABI and through the C API of a
-- header, and the errno shim of the runtime.
module Main where

import Control.Exception (SomeException, try)
import Foreign.C.Error (Errno (..), getErrno, resetErrno, throwErrnoIfMinus1_)
import Foreign.C.Types (CDouble (..), CInt (..))
import GHC.Int (Int (..), Int32 (..))
import GHC.Prim
import GHC.Types (Double (..))

foreign import ccall unsafe "sin" c_sin :: CDouble -> CDouble

foreign import ccall unsafe "pow" c_pow :: CDouble -> CDouble -> CDouble

-- The classes of ldexp interleave, so it passes its double in the first
-- float register and its int in the first integer one.
foreign import ccall unsafe "ldexp" c_ldexp :: CDouble -> CInt -> CDouble

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

-- The values are whole numbers, so every result here is exact and the checks
-- need no tolerance.
double :: Int# -> CDouble
double value = CDouble (D# (int2Double# value))

integer :: CDouble -> Int
integer (CDouble (D# value)) = I# (double2Int# value)

signed :: CInt -> Int
signed (CInt (I32# value)) = I# (int32ToInt# value)

ccallChecks :: Bool
ccallChecks =
  integer (c_sin (double 0#)) == 0
    && integer (c_pow (double 2#) (double 10#)) == 1024
    && integer (c_ldexp (double 3#) (CInt (I32# 4#Int32))) == 48

capiChecks :: Bool
capiChecks =
  integer (c_fmax (double 3#) (double 7#)) == 7
    && signed c_exitSuccess == 0
    && signed c_exitFailure == 1
    && signed (c_isdigit (fromIntegral (fromEnum '7'))) /= 0
    && signed (c_isdigit (fromIntegral (fromEnum 'x'))) == 0

-- Reading errno from a real C library call needs a call that fails the same
-- way everywhere, which WASI does not offer; test/Test/Fixtures/eval/base
-- covers that on POSIX. This checks what every target shares: that the
-- runtime shim behind getErrno and resetErrno links and reports what it was
-- given, and that throwErrnoIfMinus1_ raises on -1 and nothing else.
errnoReport :: IO String
errnoReport = do
  resetErrno
  Errno cleared <- getErrno
  raised <- raises (negate 1)
  passed <- raises 0
  pure (report cleared raised passed)

raises :: CInt -> IO Bool
raises result = do
  outcome <- try (throwErrnoIfMinus1_ "probe" (pure result))
  case outcome of
    Left err -> seq (err :: SomeException) (pure True)
    Right () -> pure False

report :: CInt -> Bool -> Bool -> String
report cleared raised passed
  | cleared /= 0 = "resetErrno left errno at " ++ show (fromIntegral cleared :: Int)
  | not raised = "throwErrnoIfMinus1_ did not raise on -1"
  | passed = "throwErrnoIfMinus1_ raised on 0"
  | otherwise = "ok"

main :: IO ()
main = do
  putStrLn (if ccallChecks then "ccall ok" else "ccall fail")
  putStrLn (if capiChecks then "capi ok" else "capi fail")
  errno <- errnoReport
  putStrLn ("errno " ++ errno)
