{- ORACLE_TEST pass -}
{-# LANGUAGE MultiParamTypeClasses #-}
module A where
class C a b
instance (C Int) Bool where
