{- ORACLE_TEST pass -}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

module ForallInstanceWithKindSignature where

class C a

instance forall (a :: *). C a where
