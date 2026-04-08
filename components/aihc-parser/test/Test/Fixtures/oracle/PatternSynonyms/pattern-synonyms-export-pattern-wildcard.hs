{- ORACLE_TEST pass -}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitNamespaces #-}

module PatternSynonymsExportPatternWildcard
  ( pattern Zero,
    -- GHC 9.16 adds namespace wildcards in import/export items.
    -- Enable this when the oracle switches to GHC 9.16:
    -- pattern ..,
  ) where

pattern Zero :: ()
pattern Zero = ()
