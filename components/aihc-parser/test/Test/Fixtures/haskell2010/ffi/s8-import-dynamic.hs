{- ORACLE_TEST
id: ffi-s8-import-dynamic
category: ffi
expected: pass
reason: parser now supports dynamic foreign imports
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportDynamic where
foreign import ccall "dynamic" mkFun :: Ptr (Int -> IO Int) -> (Int -> IO Int)
