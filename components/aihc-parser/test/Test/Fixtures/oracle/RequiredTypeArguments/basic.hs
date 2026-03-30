{- ORACLE_TEST xfail required type argument in expression -}
{-# LANGUAGE RequiredTypeArguments #-}
module Basic where

x = f (type Int) 5