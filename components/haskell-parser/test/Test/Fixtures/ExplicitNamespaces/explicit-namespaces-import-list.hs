{-# LANGUAGE ExplicitNamespaces #-}

module ExplicitNamespacesImportList where

import Data.Kind (type Type)

newtype Wrap (a :: Type) = Wrap a
