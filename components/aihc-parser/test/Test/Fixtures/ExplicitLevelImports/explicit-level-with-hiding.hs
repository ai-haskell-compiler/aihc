{- ORACLE_TEST
id: explicit-level-with-hiding
category: modules
expected: pass
reason: parser now supports explicit-level imports with hiding clauses
-}
{-# LANGUAGE ExplicitLevelImports #-}

module ExplicitLevelWithHiding where

import quote Prelude hiding (map)
import splice Data.List hiding (foldl)

useFilter :: (a -> Bool) -> [a] -> [a]
useFilter = filter
