{- ORACLE_TEST pass -}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module ForallInstanceWithOverlap where

class C a

instance {-# OVERLAPPING #-} forall a. C [a] where
