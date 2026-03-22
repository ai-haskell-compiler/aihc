{-# LANGUAGE DoAndIfThenElse #-}

module DoAndIfThenElseFollowingStmt where

pipeline :: Bool -> Maybe Int
pipeline cond = do
  x <- if cond
       then pure 10
       else pure 20
  pure (x + 1)
