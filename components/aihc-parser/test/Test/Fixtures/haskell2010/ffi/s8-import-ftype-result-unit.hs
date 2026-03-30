{- ORACLE_TEST
id: ffi-s8-import-ftype-result-unit
category: ffi
expected: pass
reason: parser now supports unit foreign result types
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportFtypeResultUnit where
foreign import ccall "tick" tick :: IO ()
