{-# LANGUAGE PolyKinds #-}

module Data.Proxy (Proxy (..)) where

import Data.Kind (Type)
import GHC.Base (Functor (..))
import GHC.Classes (Eq (..), Ord (..))
import GHC.Types (Bool (..), Ordering (..))

data Proxy (a :: k) = Proxy

-- | Every 'Proxy' carries the same (absent) information, so any two of one
-- type are equal.
--
-- @base@ gives 'Proxy' many more instances. They are not here because
-- 'Data.Typeable' imports this module and sits below 'Prelude': an import
-- of 'Prelude' or of 'Data.Semigroup.Internal' from here closes a cycle.
instance Eq (Proxy a) where
  _ == _ = True
  _ /= _ = False

instance Ord (Proxy a) where
  compare _ _ = EQ
  _ < _ = False
  _ <= _ = True
  _ > _ = False
  _ >= _ = True
  max _ _ = Proxy
  min _ _ = Proxy

-- | GHC writes this head as @Functor Proxy@. aihc cannot yet work out the
-- invisible kind argument of a poly-kinded type constructor that an
-- instance head applies to no visible arguments, so the kind is written
-- out.
instance Functor (Proxy :: Type -> Type) where
  fmap _ _ = Proxy
