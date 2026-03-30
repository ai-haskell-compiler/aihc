{- ORACLE_TEST
id: ffi-s8-import-static-wrapper-name
category: ffi
expected: pass
reason: parser now supports static wrapper-named foreign imports
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportStaticWrapperName where
foreign import ccall "static wrapper" wrapperFn :: IO Int
