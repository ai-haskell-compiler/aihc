{- ORACLE_TEST pass -}
{-# LANGUAGE DoAndIfThenElse #-}

module DoInfixContinuation where

-- Infix operator at the same indent level continues the previous
-- expression rather than starting a new statement (parse-error rule).
f x y =
  do
    x
    + y
