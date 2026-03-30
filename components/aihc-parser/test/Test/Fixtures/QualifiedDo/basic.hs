{- ORACLE_TEST
id: basic
category: expressions
expected: pass
-}
{-# LANGUAGE QualifiedDo #-}
module Basic where

x = M.do
  return ()
