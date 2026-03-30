{- ORACLE_TEST
id: ffi-s8-import-impent-omitted
category: ffi
expected: pass
reason: parser now supports foreign imports with omitted entities
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportImpentOmitted where
foreign import ccall c_atoi :: String -> IO Int
