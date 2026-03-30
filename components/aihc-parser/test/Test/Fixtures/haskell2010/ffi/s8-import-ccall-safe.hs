{- ORACLE_TEST
id: ffi-s8-import-ccall-safe
category: ffi
expected: pass
reason: parser now supports safe foreign imports
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportCcallSafe where
foreign import ccall safe "puts" c_puts_safe :: String -> IO Int
