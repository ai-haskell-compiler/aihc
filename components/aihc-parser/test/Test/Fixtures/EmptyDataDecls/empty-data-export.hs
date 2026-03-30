{- ORACLE_TEST
id: empty-data-export
category: modules
expected: pass
-}
{-# LANGUAGE EmptyDataDecls #-}

module EmptyDataDeclsExport (Empty, Phantom) where

data Empty

data Phantom a
