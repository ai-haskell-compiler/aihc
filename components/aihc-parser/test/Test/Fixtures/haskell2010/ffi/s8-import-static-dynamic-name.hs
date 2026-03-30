{- ORACLE_TEST
id: ffi-s8-import-static-dynamic-name
category: ffi
expected: pass
reason: parser now supports static-named foreign imports
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportStaticDynamicName where
foreign import ccall "static dynamic" dynamicFn :: IO Int
