{- ORACLE_TEST
id: types-maybe-type-constructor
category: types
expected: pass
reason: parser now handles maybe type constructors
-}
module T13 where
withDefault :: Maybe a -> a -> a
withDefault mx fallback = case mx of
  Just x -> x
  Nothing -> fallback
