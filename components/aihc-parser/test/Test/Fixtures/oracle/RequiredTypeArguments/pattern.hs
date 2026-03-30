{- ORACLE_TEST xfail required type argument in pattern -}
{-# LANGUAGE RequiredTypeArguments #-}
module Pattern where

f (type a) x = x