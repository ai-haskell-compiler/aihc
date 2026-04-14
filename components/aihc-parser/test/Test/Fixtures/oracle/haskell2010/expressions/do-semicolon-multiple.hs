{- ORACLE_TEST pass -}
{-# LANGUAGE Haskell2010 #-}

-- Multiple explicit semicolons in do notation
f = do
  ;;let x = 1
  ;;x
