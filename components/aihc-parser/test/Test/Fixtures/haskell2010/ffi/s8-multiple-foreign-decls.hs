{- ORACLE_TEST
id: ffi-s8-multiple-foreign-decls
category: ffi
expected: pass
reason: parser now supports multiple foreign declarations per module
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8MultipleForeignDecls where
foreign import ccall unsafe "plus1" plus1 :: Int -> IO Int
foreign import ccall safe "plus2" plus2 :: Int -> IO Int
foreign import ccall "plus3" plus3 :: Int -> IO Int
