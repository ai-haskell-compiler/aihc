{- ORACLE_TEST xfail UNPACK pragma in record field -}
{-# LANGUAGE NamedFieldPuns #-}

data BQueue a = BQueue
  { bqRead :: {-# UNPACK #-}!Int
  }
