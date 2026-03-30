{- ORACLE_TEST
id: ffi-s8-export-ccall-omitted-entity
category: ffi
expected: pass
reason: parser now supports foreign export declarations with omitted entity
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ExportCcallOmittedEntity where
addOne :: Int -> Int
addOne n = n + 1
foreign export ccall addOne :: Int -> Int
