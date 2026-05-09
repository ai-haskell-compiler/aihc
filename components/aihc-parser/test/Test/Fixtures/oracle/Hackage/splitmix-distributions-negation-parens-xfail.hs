{- ORACLE_TEST pass -}
module SplitmixDistributionsNegationParensXFail where

zipfLike a u =
  let xInt = floor (u ** (- 1 / (a - 1)))
   in xInt

weibullLike a b x =
  return $ (- 1 / a * log (1 - x)) ** 1 / b
