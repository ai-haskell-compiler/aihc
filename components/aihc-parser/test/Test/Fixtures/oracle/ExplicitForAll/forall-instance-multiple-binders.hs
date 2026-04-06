{- ORACLE_TEST pass -}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

module ForallInstanceMultipleBinders where

class C a b

instance forall a b. C a b where
