{- ORACLE_TEST
id: ffi-s8-import-address-only
category: ffi
expected: pass
reason: parser now supports foreign import address-only entities
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportAddressOnly where
foreign import ccall "&" errnoPtr :: Ptr Int
