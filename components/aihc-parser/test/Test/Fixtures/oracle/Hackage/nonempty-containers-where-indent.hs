{- ORACLE_TEST pass -}
module A where
f k m0
  = case compare k 0 of
      GT
        -> Just
             $ case (g m1, g m2) of
                 (Nothing, Nothing) -> x
                 (Just _, Just n2) -> y
      where
          (m1, m2) = split k m0
