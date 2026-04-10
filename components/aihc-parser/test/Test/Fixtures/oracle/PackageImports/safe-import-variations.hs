{- ORACLE_TEST pass -}
{-# LANGUAGE Safe, PackageImports #-}

module SafeImportVariations where

-- Safe import with package
import safe "base" Control.Applicative (pure)

-- Safe import without package
import safe Data.Maybe (Maybe)

-- Safe qualified import
import safe qualified Data.List as L

-- Safe qualified import with package
import safe qualified "base" Data.List as List
