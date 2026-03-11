{-# LANGUAGE DoAndIfThenElse #-}

module DoAndIfThenElseNestedDo where

nested :: Bool -> Maybe Int
nested cond = do
  if cond
  then do
    x <- pure 1
    pure x
  else do
    y <- pure 2
    pure y
