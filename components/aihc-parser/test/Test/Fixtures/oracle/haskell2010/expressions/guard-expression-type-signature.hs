{- ORACLE_TEST pass -}

module GuardExpressionTypeSignature where

f :: Bool -> Bool
f x
  | x :: Bool = True
  | otherwise = False
