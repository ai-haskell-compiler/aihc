{- ORACLE_TEST
id: types-nested-maybe-list
category: types
expected: pass
reason: parser now handles nested maybe/list type constructors
-}
module T17 where
normalize :: Maybe [a] -> [a]
normalize mx = case mx of
  Just xs -> xs
  Nothing -> []
