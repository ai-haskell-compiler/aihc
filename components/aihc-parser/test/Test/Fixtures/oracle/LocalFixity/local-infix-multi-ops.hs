{- ORACLE_TEST pass -}
f x = x .>. (y .<. z)
  where
    infixl 4 .>., .<.
    (.>.) = (>)
    (.<.) = (<)
    y = 10
    z = 5