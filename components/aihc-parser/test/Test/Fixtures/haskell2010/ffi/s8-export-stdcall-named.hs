{- ORACLE_TEST
id: ffi-s8-export-stdcall-named
category: ffi
expected: pass
reason: parser now supports stdcall foreign export declarations
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ExportStdcallNamed where
mulInt :: Int -> Int -> Int
mulInt a b = a * b
foreign export stdcall "mulInt" mulInt :: Int -> Int -> Int
