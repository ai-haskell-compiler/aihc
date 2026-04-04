{- ORACLE_TEST xfail hw-hedgehog typed do bind -}
{-# LANGUAGE ScopedTypeVariables #-}
module ScopedTypeVariablesTypedDoBind where

f :: Monad m => m Int -> m Int
f gen = do
  x :: Int <- gen
  pure x
