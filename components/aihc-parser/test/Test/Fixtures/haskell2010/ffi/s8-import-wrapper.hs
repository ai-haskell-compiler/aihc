{- ORACLE_TEST
id: ffi-s8-import-wrapper
category: ffi
expected: pass
reason: parser now supports wrapper foreign imports
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportWrapper where
foreign import ccall "wrapper" wrapFun :: (Int -> IO Int) -> IO (Ptr (Int -> IO Int))
