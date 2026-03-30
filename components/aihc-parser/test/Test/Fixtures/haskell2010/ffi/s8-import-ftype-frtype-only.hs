{- ORACLE_TEST
id: ffi-s8-import-ftype-frtype-only
category: ffi
expected: pass
reason: parser now supports non-IO foreign result types
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportFtypeFrtypeOnly where
foreign import ccall "get_errno" getErrno :: Int
