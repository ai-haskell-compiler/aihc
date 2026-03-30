{- ORACLE_TEST
id: ffi-s8-import-address-header-cid
category: ffi
expected: pass
reason: parser now supports foreign import address entities with header strings
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportAddressHeaderCid where
foreign import ccall "errno.h &errno" errnoPtr :: Ptr Int
