{-# LANGUAGE PackageImports #-}

module PackageImportsBasic where

import "containers" Data.Map (Map)
import "base" Prelude

usesMap :: Maybe (Map Int Int)
usesMap = Nothing
