{- ORACLE_TEST pass -}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module ForallInstanceWithContext where

class C a

instance forall a. Show a => C a where
