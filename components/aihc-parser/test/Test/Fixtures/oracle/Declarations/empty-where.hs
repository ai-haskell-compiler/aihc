{- ORACLE_TEST pass -}
{-# LANGUAGE Haskell2010 #-}

module EmptyWhere where

-- Empty where clause (syntactically valid, though semantically useless)
test = x
  where {}
