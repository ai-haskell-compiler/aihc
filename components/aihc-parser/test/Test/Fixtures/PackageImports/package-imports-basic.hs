{- ORACLE_TEST pass -}
{-# LANGUAGE PackageImports #-}

module PackageImportsBasic where

import "base" Prelude
import "containers" Data.Map (Map)

usesMap :: Maybe (Map Int Int)
usesMap = Nothing