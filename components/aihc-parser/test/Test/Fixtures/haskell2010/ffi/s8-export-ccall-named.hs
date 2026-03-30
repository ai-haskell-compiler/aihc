{- ORACLE_TEST
id: ffi-s8-export-ccall-named
category: ffi
expected: pass
reason: parser now supports foreign export declarations
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ExportCcallNamed where
addInt :: Int -> Int -> Int
addInt a b = a + b
foreign export ccall "addInt" addInt :: Int -> Int -> Int
