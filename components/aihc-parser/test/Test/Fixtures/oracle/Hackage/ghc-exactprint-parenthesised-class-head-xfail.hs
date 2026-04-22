{- ORACLE_TEST xfail parser rejects parenthesised class head in class declaration -}
module A where

class (Monad m) => (HasTransform m) where
  liftT :: Int -> m Int
