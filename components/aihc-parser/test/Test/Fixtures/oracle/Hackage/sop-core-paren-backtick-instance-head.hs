{- ORACLE_TEST pass -}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
module SopCoreParenBacktickInstanceHead where

class (f `C` g) x
instance (f `C` g) x
