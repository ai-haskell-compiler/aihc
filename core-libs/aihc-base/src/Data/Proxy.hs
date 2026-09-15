{-# LANGUAGE PolyKinds #-}

module Data.Proxy (Proxy (..)) where

import GHC.Classes (Eq (..), Ord (..))
import GHC.Types (Bool (..), Ordering (..))

data Proxy (a :: k) = Proxy

-- | Every 'Proxy' carries the same (absent) information, so any two of one
-- type are equal.
--
-- @base@ gives 'Proxy' many more instances. They are not here because
-- 'Data.Typeable' imports this module, and 'Data.Typeable' is below
-- 'Prelude': anything this module imports beyond the wired-in classes
-- closes an import cycle.
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
