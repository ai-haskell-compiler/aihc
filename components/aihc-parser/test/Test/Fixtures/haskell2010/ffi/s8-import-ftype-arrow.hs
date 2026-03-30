{- ORACLE_TEST
id: ffi-s8-import-ftype-arrow
category: ffi
expected: pass
reason: parser now supports foreign function arrow types
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportFtypeArrow where
foreign import ccall "plus1" plus1 :: Int -> IO Int
