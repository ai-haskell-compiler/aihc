{- ORACLE_TEST
id: pattern-synonyms-import-export-bundled-data
category: modules
expected: pass
reason: parser support pending
-}
{-# LANGUAGE PatternSynonyms #-}

module PatternSynonymsImportExportBundledData
  ( Nat (Zero, Succ),
  ) where

import PatternSynonymsSource (Nat (Zero, Succ))

fromNat Zero = 0
fromNat (Succ n) = 1 + fromNat n
