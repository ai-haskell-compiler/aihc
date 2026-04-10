{- ORACLE_TEST pass -}
{-# LANGUAGE UnicodeSyntax #-}

-- Unicode type variable in data declaration

module UnicodeTypeVarData where

data Tree α = Leaf α | Node (Tree α) (Tree α)
