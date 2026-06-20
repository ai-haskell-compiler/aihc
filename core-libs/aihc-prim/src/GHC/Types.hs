{-# LANGUAGE PackageImports #-}

module GHC.Types
  ( TYPE,
    RuntimeRep (..),
  )
where

import "ghc-prim" GHC.Types (RuntimeRep (..), TYPE)
