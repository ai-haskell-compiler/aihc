{- ORACLE_TEST pass -}
f x y = x .+. y
  where
    infixl 6 .+.
    infixr 5 .*.
    (.+.) a b = a + b
    (.*.) a b = a * b