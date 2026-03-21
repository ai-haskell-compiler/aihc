{-# LANGUAGE ExplicitNamespaces #-}

module ExplicitNamespacesImportType where

import Data.Proxy (type Proxy (..))

mkProxy :: Proxy Int
mkProxy = Proxy
