{- ORACLE_TEST xfail parser rejects parenthesised backtick-operator instance head with trailing argument -}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
module SopCoreParenBacktickInstanceHead where

class (f `C` g) x
instance (f `C` g) x
