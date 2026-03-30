{- ORACLE_TEST
id: ffi-s8-import-ccall-unsafe
category: ffi
expected: pass
reason: parser now supports unsafe foreign imports
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportCcallUnsafe where
foreign import ccall unsafe "puts" c_puts_unsafe :: String -> IO Int
