{- ORACLE_TEST pass -}
module T17 where
normalize :: Maybe [a] -> [a]
normalize mx = case mx of
  Just xs -> xs
  Nothing -> []