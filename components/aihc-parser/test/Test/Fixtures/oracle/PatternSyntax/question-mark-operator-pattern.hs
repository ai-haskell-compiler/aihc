{- ORACLE_TEST pass -}
module QuestionMarkOperatorPattern where

foldl' (??) z xs = (foldr (?!) id xs) z
  where
    x ?! g = g . (?? x)
