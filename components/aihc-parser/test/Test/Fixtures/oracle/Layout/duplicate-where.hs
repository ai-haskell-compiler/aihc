{- ORACLE_TEST pass -}
module DuplicateWhere where

selectByEntity :: [Int] -> (String, String) -> Maybe String
selectByEntity inputs (tSel, tName) = case gerArgs (filter areEq inputs) of
  [] -> Nothing
  args -> Just (tSel ++ tName ++ show args)
    where

  where
    gerArgs = map show
    areEq (sel, v) = sel == 0 && tName == v
