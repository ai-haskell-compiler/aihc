{- ORACLE_TEST
id: package-imports-basic
category: modules
expected: pass
-}
{-# LANGUAGE PackageImports #-}

module PackageImportsBasic where

import "base" Prelude
import "containers" Data.Map (Map)

usesMap :: Maybe (Map Int Int)
usesMap = Nothing
