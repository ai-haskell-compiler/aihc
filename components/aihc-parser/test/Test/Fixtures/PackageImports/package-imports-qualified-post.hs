{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PackageImports #-}

module PackageImportsQualifiedPost where

import "containers" Data.Map qualified as M
import "base" Prelude

fromListMap :: [(Int, Int)] -> M.Map Int Int
fromListMap = M.fromList
