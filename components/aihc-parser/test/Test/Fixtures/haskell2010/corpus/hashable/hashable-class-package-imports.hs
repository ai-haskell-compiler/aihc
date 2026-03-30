{- ORACLE_TEST
id: hashable-class-package-imports
category: corpus
expected: pass
reason: from hashable/src/Data/Hashable/Class.hs; parser now supports package-qualified imports with type constructor wildcards
-}
{-# LANGUAGE PackageImports #-}
module X where

import "filepath" System.OsString.Internal.Types (OsString (..))
