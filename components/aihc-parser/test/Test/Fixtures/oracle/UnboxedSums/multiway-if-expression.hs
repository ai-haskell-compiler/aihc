{- ORACLE_TEST pass -}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UnboxedSums #-}

module MultiWayIfExpression where

x = (# | if | True -> () #)
