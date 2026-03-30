{- ORACLE_TEST
id: ffi-s8-import-ccall-basic
category: ffi
expected: pass
reason: parser now supports basic ccall foreign imports
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportCcallBasic where
foreign import ccall "puts" c_puts :: String -> IO Int
