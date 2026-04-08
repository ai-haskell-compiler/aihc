{- ORACLE_TEST pass -}
{-# LANGUAGE ExplicitNamespaces #-}

module ExplicitNamespacesExportTypeDataWildcards
  ( type Token,
    -- GHC 9.16 adds namespace wildcards in import/export items.
    -- Enable this when the oracle switches to GHC 9.16:
    -- type ..,
    -- data ..,
  ) where

data Token = Token
