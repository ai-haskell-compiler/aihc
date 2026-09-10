module Main where

import Control.Exception (SomeException, try)
import Foreign.C.Error (Errno (..), getErrno, resetErrno, throwErrnoIfMinus1_)
import Foreign.C.Types (CInt (..))

-- Reading errno from a real C library call needs a call that fails the same
-- way everywhere, which WASI does not offer; test/Test/Fixtures/eval/base
-- covers that on POSIX. This checks what every target shares: that the
-- runtime shim behind getErrno and resetErrno links and reports what it was
-- given, and that throwErrnoIfMinus1_ raises on -1 and nothing else.
main :: IO ()
main = do
  resetErrno
  Errno cleared <- getErrno
  raised <- raises (negate 1)
  passed <- raises 0
  putStrLn (report cleared raised passed)

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
