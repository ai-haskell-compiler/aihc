{- ORACLE_TEST xfail dependent-sum explicit forall in instance head -}
{-# LANGUAGE ScopedTypeVariables #-}

module ForallInstanceHead where

class C a

instance forall a. C a where
