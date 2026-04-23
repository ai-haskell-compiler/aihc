{- ORACLE_TEST pass -}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
module A where
class C a b
deriving instance (C Int) Bool
