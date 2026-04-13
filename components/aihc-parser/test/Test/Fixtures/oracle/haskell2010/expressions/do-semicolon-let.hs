{- ORACLE_TEST xfail reason="explicit semicolons followed by let in do notation not parsed" -}
{-# LANGUAGE Haskell2010 #-}

-- Parser fails on explicit semicolons followed by let in do notation
f = do
  ;let x = 1
  ;x
