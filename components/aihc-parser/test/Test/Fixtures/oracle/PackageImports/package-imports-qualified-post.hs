{- ORACLE_TEST pass -}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ImportQualifiedPost #-}

module PackageImportsQualifiedPost where

import "base" Prelude
import "containers" Data.Map qualified as M

fromListMap :: [(Int, Int)] -> M.Map Int Int
fromListMap = M.fromList