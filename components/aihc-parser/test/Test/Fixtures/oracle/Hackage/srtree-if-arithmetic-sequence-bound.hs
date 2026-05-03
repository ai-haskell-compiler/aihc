{- ORACLE_TEST pass -}
module SrtreeIfArithmeticSequenceBound where

f maxSize =
  randomFrom [if maxSize > 4 then 4 else 1 .. maxSize]
