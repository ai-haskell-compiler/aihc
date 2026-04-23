{- ORACLE_TEST xfail roundtrip mismatch: aihc-parser over-parenthesizes MultiWayIf in infix context -}
{-# LANGUAGE MultiWayIf #-}
module MultiWayIfInfixRoundtrip where

f = x ++
  if | True -> ()
