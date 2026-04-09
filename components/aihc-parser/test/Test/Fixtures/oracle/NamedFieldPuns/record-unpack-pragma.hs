{- ORACLE_TEST pass -}
{-# LANGUAGE NamedFieldPuns #-}

data BQueue a = BQueue
  { bqRead :: {-# UNPACK #-}!Int
  }
