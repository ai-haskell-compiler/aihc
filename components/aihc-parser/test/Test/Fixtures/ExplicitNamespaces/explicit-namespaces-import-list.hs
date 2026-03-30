{- ORACLE_TEST
id: explicit-namespaces-import-list
category: modules
expected: pass
reason: parser now supports type namespace import items
-}
{-# LANGUAGE ExplicitNamespaces #-}

module ExplicitNamespacesImportList where

import Data.Kind (type Type)
