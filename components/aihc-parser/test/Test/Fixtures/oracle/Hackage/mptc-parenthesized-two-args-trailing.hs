{- ORACLE_TEST pass -}
{-# LANGUAGE MultiParamTypeClasses #-}
module A where
class C a b c
instance (C Int Bool) Char where
