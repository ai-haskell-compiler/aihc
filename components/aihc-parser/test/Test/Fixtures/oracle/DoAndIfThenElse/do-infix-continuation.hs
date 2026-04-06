{- ORACLE_TEST xfail do-statement infix continuation (parse-error rule) -}
{-# LANGUAGE DoAndIfThenElse #-}
module DoInfixContinuation where

-- Infix operator at the same indent level continues the previous
-- expression rather than starting a new statement (parse-error rule).
f x y = do
  x
  + y
