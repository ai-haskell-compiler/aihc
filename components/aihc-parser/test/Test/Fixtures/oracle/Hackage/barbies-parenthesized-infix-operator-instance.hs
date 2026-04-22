{- ORACLE_TEST xfail parser rejects parenthesized infix type operator applied to argument in instance head -}
{-# LANGUAGE TypeOperators #-}
module BarbiesParenthesizedInfixOperatorInstance where

instance (c & d) a
