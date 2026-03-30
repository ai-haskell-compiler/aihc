{- ORACLE_TEST
id: pattern-synonyms-import-export-bundled
category: modules
expected: pass
reason: parser now supports type constructor explicit member imports/exports
-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitNamespaces #-}

module PatternSynonymsImportExportBundled
  ( Nat (Zero, Succ),
  ) where

import PatternSynonymsSource (Nat (Zero, Succ))

fromNat Zero = 0
fromNat (Succ n) = 1 + fromNat n
