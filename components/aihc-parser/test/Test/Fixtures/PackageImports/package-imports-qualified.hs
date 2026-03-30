{- ORACLE_TEST
id: package-imports-qualified
category: modules
expected: pass
-}
{-# LANGUAGE PackageImports #-}

module PackageImportsQualified where

import "base" Prelude hiding (map)
import "containers" Data.Map as M

mapSize :: M.Map Int Int -> Int
mapSize = M.size
