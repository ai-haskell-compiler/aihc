{- ORACLE_TEST xfail SCC pragma in expression silently dropped during parse -}
module A where
f x = {-# SCC "name" #-} x
