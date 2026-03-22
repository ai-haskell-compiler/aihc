{-# LANGUAGE PackageImports #-}

module PackageImportsQualified where

import "containers" Data.Map as M
import "base" Prelude hiding (map)

mapSize :: M.Map Int Int -> Int
mapSize = M.size
