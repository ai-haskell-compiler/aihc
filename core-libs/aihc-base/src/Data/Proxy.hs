{-# LANGUAGE PolyKinds #-}

module Data.Proxy (Proxy (..)) where

import Data.Kind (Type)
import GHC.Base (Functor (..))

data Proxy (a :: k) = Proxy

-- | GHC writes this head as @Functor Proxy@. aihc cannot yet work out the
-- invisible kind argument of a poly-kinded type constructor that an
-- instance head applies to no visible arguments, so the kind is written
-- out.
instance Functor (Proxy :: Type -> Type) where
  fmap _ _ = Proxy
