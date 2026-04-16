{- ORACLE_TEST pass -}
-- Guard expression type signatures may contain function arrows.
-- In equation context the arrow is unambiguous (RHS uses '=', not '->'),
-- so ':: T -> T2' must be parsed as a function type, not as the alternative
-- arrow. Regression test for a bug where typeInfixParser was used in all
-- guard contexts, rejecting '->' in types.

module GuardExpressionTypeSignatureFun where

f :: (Int -> Bool) -> Int -> Int
f p x
  | p :: Int -> Bool = x + 1
  | otherwise = x
