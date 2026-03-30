{- ORACLE_TEST
id: basic
category: declarations
expected: pass
-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE EmptyDataDeriving #-}
module Basic where

data Empty
  deriving Show
