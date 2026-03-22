{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}

module PatternSynonymsImportExportBundled
  ( Nat (Zero, Succ),
  )
where

import PatternSynonymsSource (Nat (Succ, Zero))

fromNat Zero = 0
fromNat (Succ n) = 1 + fromNat n
