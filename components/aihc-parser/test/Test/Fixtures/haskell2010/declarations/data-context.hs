{- ORACLE_TEST
id: decls-data-context
category: declarations
expected: pass
-}
{-# LANGUAGE Haskell2010 #-}
module D12 where
data Eq a => Set a = NilSet | ConsSet a (Set a)
