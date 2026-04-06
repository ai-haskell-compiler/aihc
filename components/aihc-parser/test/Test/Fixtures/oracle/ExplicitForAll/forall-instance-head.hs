{- ORACLE_TEST pass -}
{-# LANGUAGE ScopedTypeVariables #-}

module ForallInstanceHead where

class C a

instance forall a. C a where
