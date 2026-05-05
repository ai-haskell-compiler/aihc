{- ORACLE_TEST pass -}
{-# LANGUAGE MultiWayIf #-}
module MultiWayIfLeftInfixRoundtrip where

f = (if | True -> ()) `a` 'x'
