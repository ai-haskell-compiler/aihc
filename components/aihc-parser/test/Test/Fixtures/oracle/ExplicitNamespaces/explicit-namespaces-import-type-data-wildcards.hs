{- ORACLE_TEST pass -}
{-# LANGUAGE ExplicitNamespaces #-}

module ExplicitNamespacesImportTypeDataWildcards where

-- GHC 9.16 adds namespace wildcards in import/export items.
-- Enable this when the oracle switches to GHC 9.16:
-- import M (type .., data ..)

import Data.Proxy (type Proxy (..))

mkProxy :: Proxy Int
mkProxy = Proxy
