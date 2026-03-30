{- ORACLE_TEST
id: empty-data-with-kind
category: declarations
expected: pass
-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE KindSignatures #-}

module EmptyDataDeclsWithKind where

import Data.Kind (Type)

data Tagged (a :: Type)
