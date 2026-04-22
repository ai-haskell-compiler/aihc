{- ORACLE_TEST pass -}
module A where

class (Monad m) => (HasTransform m) where
  liftT :: Int -> m Int
