{- ORACLE_TEST pass -}
module ExprS312LetNestedLayout where

x = let y =
          let z = 1
           in z
     in y
