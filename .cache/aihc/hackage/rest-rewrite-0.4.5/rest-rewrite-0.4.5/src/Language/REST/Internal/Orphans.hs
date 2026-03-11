{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.REST.Internal.Orphans() where

#if !MIN_VERSION_hashable(1,3,4)
import Data.Hashable
import Data.Hashable.Lifted
import Data.Set as Set
import Data.Map as Map

instance Hashable1 Set where
    liftHashWithSalt h s x = Set.foldl' h (hashWithSalt s (Set.size x)) x

instance (Hashable a) => Hashable (Set a) where
  hashWithSalt = hashWithSalt1

instance Hashable2 Map.Map where
    liftHashWithSalt2 hk hv s m = Map.foldlWithKey'
        (\s' k v -> hv (hk s' k) v)
        (hashWithSalt s (Map.size m))
        m

instance Hashable k => Hashable1 (Map.Map k) where
    liftHashWithSalt h s m = Map.foldlWithKey'
        (\s' k v -> h (hashWithSalt s' k) v)
        (hashWithSalt s (Map.size m))
        m

-- | @since 1.3.4.0
instance (Hashable k, Hashable v) => Hashable (Map.Map k v) where
    hashWithSalt = hashWithSalt2

#endif
