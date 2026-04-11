{- ORACLE_TEST xfail operator pattern with question marks -}
module QuestionMarkOperatorPattern where

foldl' (??) z xs = (foldr (?!) id xs) z
  where
    x ?! g = g . (?? x)
