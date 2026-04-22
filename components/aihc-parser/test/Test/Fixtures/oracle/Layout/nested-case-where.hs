{- ORACLE_TEST pass -}
module NestedCaseWhere where

-- where clause after nested case expressions should attach to the
-- enclosing function binding, not the outer case alternative.
f k m0
  = case compare k 0 of
      GT
        -> case m0 of
             [] -> Nothing
             _ -> Just k
      where
          g = k + 1
