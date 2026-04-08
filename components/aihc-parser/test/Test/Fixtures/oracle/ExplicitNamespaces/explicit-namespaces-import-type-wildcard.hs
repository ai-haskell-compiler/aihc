{- ORACLE_TEST pass -}
{-# LANGUAGE ExplicitNamespaces #-}

module ExplicitNamespacesImportTypeWildcard where

-- GHC 9.16 adds namespace wildcards in import/export items.
-- Enable this when the oracle switches to GHC 9.16:
-- import Data.Proxy (type ..)

import Data.Proxy (type Proxy (..))

mkProxy :: Proxy Int
mkProxy = Proxy
