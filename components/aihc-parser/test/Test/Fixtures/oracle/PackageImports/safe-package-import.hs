{- ORACLE_TEST xfail parser does not recognize 'safe' keyword in import declaration -}
{-# LANGUAGE Safe, PackageImports #-}

module SafePackageImport where

import safe "base" Control.Applicative (pure)
