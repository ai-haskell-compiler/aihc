{- ORACLE_TEST xfail "we shouldn't wrap this expression in parens" -}
module M where

n = {-# SCC "tag" #-} fn arg :: ()
