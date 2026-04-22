{- ORACLE_TEST pass -}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module A where
class C a b
instance (((C)) Int) Bool where
