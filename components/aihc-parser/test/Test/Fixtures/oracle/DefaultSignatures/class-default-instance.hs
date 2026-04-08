{- ORACLE_TEST pass -}
{-# LANGUAGE DefaultSignatures #-}
module Search where

class MonadSearch m where
  fromList :: [a] -> m a

newtype DFS a = DFS { unDFS :: [a] }

instance MonadSearch DFS where
  fromList xs = DFS xs
  {-# INLINE toList #-}
  toList (DFS xs) = xs