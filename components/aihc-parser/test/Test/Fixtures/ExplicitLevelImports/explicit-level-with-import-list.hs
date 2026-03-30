{- ORACLE_TEST
id: explicit-level-with-import-list
category: modules
expected: pass
reason: parser now supports explicit-level imports with import lists
-}
{-# LANGUAGE ExplicitLevelImports #-}

module ExplicitLevelWithImportList where

import quote Data.List (map)
import splice Data.Maybe (fromMaybe)

useMap :: [Int] -> [Int]
useMap = map
