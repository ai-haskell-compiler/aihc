{- ORACLE_TEST pass -}
{-# LANGUAGE MultiWayIf #-}
module OffsideInfix where

f =
    if | True
       ->
        1
     + 2
