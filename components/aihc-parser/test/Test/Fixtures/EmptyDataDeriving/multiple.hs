{- ORACLE_TEST
id: multiple
category: declarations
expected: pass
-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE EmptyDataDeriving #-}
module Multiple where

data Empty
  deriving (Eq, Ord, Show)
