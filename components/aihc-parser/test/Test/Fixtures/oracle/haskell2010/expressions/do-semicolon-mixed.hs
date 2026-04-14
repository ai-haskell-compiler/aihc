{- ORACLE_TEST pass -}
{-# LANGUAGE Haskell2010 #-}

-- Explicit semicolons with different statement types
f = do
  ;let x = 1
  ;y <- return x
  ;return y
