{- ORACLE_TEST
id: pat-contexts
category: patterns
expected: pass
reason: parser now supports pattern matching contexts
-}
module P10 where

funcApply a b c = a b c

funcMaybe Nothing (Just a) = a
funcMaybe (Just a) Nothing = a

funcAs (a@b) = b

letPattern = let Just x = Nothing in x
wherePattern = y where Just y = Nothing

lambdaPattern = (\(x:_) -> x) [1, 2, 3]

casePattern value = case value of
  Just x -> x
  Nothing -> 0

listCompPattern xs = [x | Just x <- xs]

doPattern = do
  Just x <- Just 1
  pure x
