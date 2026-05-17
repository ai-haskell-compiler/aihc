{- ORACLE_TEST pass -}
{-# LANGUAGE UnicodeSyntax #-}

module UnicodeStarExpressionOperator where

infixl 7 ★

(★) ∷ Int → Int → Int
(★) = (*)

value ∷ Int
value = 2 ★ 3
