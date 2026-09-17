{-# LANGUAGE PolyKinds #-}

module Data.Proxy (Proxy (..)) where

import GHC.Base (Functor (..))
import GHC.Classes (Eq (..), Ord (..))
import GHC.Types (Bool (..), Ordering (..))

-- | Written as GHC writes it, with the kind of @t@ left to inference: the
-- kind variable is then an inferred binder of the constructor, so
-- @Proxy \@a@ applies to @t@ rather than to its kind.
data Proxy t = Proxy

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

instance Functor Proxy where
  fmap _ _ = Proxy
