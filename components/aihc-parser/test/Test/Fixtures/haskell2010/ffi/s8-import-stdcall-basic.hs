{- ORACLE_TEST
id: ffi-s8-import-stdcall-basic
category: ffi
expected: pass
reason: parser now supports stdcall foreign imports
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportStdcallBasic where
foreign import stdcall "puts" s_puts :: String -> IO Int
