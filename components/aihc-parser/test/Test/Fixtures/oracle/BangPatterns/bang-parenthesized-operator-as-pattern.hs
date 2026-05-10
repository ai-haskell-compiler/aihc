{- ORACLE_TEST pass -}
{-# LANGUAGE BangPatterns #-}
module BangParenthesizedOperatorAsPattern where

data C = C

fn !(+)@C = ()
