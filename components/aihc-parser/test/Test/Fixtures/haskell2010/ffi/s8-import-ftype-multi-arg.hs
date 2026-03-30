{- ORACLE_TEST
id: ffi-s8-import-ftype-multi-arg
category: ffi
expected: pass
reason: parser now supports multi-argument foreign function types
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportFtypeMultiArg where
foreign import ccall "plus" plus :: Int -> Int -> IO Int
