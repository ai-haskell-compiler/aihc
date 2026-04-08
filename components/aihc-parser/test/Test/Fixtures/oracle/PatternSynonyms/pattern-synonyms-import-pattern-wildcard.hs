{- ORACLE_TEST pass -}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitNamespaces #-}

module PatternSynonymsImportPatternWildcard where

-- GHC 9.16 adds namespace wildcards in import/export items.
-- Enable this when the oracle switches to GHC 9.16:
-- import M (pattern ..)

buildZero = Zero

pattern Zero :: ()
pattern Zero = ()
