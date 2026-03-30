{- ORACLE_TEST pass -}
{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportImpentOmitted where
foreign import ccall c_atoi :: String -> IO Int