{- ORACLE_TEST
id: explicit-namespaces-import-type
category: modules
expected: pass
reason: parser now supports type namespace import items
-}
{-# LANGUAGE ExplicitNamespaces #-}

module ExplicitNamespacesImportType where

import Data.Proxy (type Proxy (..))

mkProxy :: Proxy Int
mkProxy = Proxy
