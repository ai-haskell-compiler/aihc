{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}

-- Guard expression inside a TH declaration quote with a function-type
-- annotation.  The '->' in 'Int -> Bool' must be parsed as a function
-- type arrow, not confused with a case alternative arrow.  Regression
-- test for a parser bug that caused the outer declaration to misparse
-- when the inner guarded RHS contained ':: T -> T2'.

module THDeclQuoteGuardFunSig where

f = [d|y | z :: Int -> Bool = z|]
