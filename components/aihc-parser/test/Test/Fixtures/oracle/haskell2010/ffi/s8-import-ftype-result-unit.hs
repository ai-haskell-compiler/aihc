{- ORACLE_TEST pass -}
{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportFtypeResultUnit where
foreign import ccall "tick" tick :: IO ()