{-# LANGUAGE PolyKinds #-}

module Data.Proxy (Proxy (..)) where

import GHC.Base (Functor (..))

data Proxy (a :: k) = Proxy

instance Functor Proxy where
  fmap _ _ = Proxy
