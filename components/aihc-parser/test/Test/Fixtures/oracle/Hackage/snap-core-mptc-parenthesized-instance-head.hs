{- ORACLE_TEST xfail parenthesized multi-param type class instance head not parsed -}
{-# LANGUAGE MultiParamTypeClasses #-}
module A where
class C a b
instance (C Int) Bool where
