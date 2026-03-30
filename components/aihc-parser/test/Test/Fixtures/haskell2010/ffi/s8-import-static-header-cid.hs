{- ORACLE_TEST
id: ffi-s8-import-static-header-cid
category: ffi
expected: pass
reason: parser now supports static foreign imports with headers
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportStaticHeaderCid where
foreign import ccall "static math.h sin" c_sin :: Double -> Double
