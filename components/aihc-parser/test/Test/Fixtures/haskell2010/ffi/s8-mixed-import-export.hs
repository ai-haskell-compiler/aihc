{- ORACLE_TEST
id: ffi-s8-mixed-import-export
category: ffi
expected: pass
reason: parser now supports mixed foreign import/export declarations
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8MixedImportExport where
foreign import ccall "atoi" c_atoi :: String -> IO Int
inc :: Int -> Int
inc n = n + 1
foreign export ccall "inc" inc :: Int -> Int
