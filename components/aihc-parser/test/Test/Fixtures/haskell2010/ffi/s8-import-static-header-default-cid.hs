{- ORACLE_TEST
id: ffi-s8-import-static-header-default-cid
category: ffi
expected: pass
reason: parser now supports static foreign imports with default C identifier
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportStaticHeaderDefaultCid where
foreign import ccall "static math.h" c_cos :: Double -> Double
