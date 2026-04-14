{- ORACLE_TEST pass -}
{-# LANGUAGE Haskell2010 #-}

-- Explicit semicolons followed by let in do notation
f = do
  ;let x = 1
  ;x
