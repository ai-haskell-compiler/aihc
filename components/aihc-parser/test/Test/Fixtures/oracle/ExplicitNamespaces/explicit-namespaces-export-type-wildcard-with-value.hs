{- ORACLE_TEST pass -}
{-# LANGUAGE ExplicitNamespaces #-}

module ExplicitNamespacesExportTypeWildcardWithValue
  ( type Proxy,
    f,
    -- GHC 9.16 adds namespace wildcards in import/export items.
    -- Enable this when the oracle switches to GHC 9.16:
    -- type ..,
  ) where

import Data.Proxy (Proxy)

f :: Proxy Int
f = Proxy
