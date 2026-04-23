{- ORACLE_TEST pass -}
{-# LANGUAGE MultiWayIf #-}
module MultiWayIfInfixRoundtrip where

f = x ++
  if | True -> ()
