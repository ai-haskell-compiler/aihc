{-# language AllowAmbiguousTypes    #-}
{-# language DataKinds              #-}
{-# language FunctionalDependencies #-}
{-# language GADTs                  #-}
{-# language PolyKinds              #-}
{-# language ScopedTypeVariables    #-}
{-# language TypeApplications       #-}
{-# language TypeFamilies           #-}
{-# language TypeOperators          #-}
-- | Poly-kinded 'Functor' type class.
-- 'KFunctor' generalizes functors, bifunctors, profunctors, ...
-- by declaring a list of 'Variance's for a type constructor.
module Data.PolyKinded.Functor where

import           Data.PolyKinded

-- | Declares that the type constructor @f@ is a generalized
-- functor whose variances for each type argument are given by @v@.
class KFunctor (f :: k) (v :: Variances) (as :: LoT k) (bs :: LoT k) | f -> v where
  -- | The generalized version of 'fmap', 'bimap', 'dimap', and so on.
  kfmap :: Mappings v as bs -> f :@@: as -> f :@@: bs

-- | The generalized version of 'fmap', 'bimap', 'dimap', and so on.
-- This version uses 'Split' to obtain better type inference.
kmapo :: forall f v as bs. (KFunctor f v as bs)
      => Mappings v as bs -> f :@@: as -> f :@@: bs
kmapo = kfmap @_ @f

-- ** Mappings of different variance

-- | Possible variances for each argument of a type constructor.
data Variance = Co       -- ^ The functor is covariant in this position.
              | Contra   -- ^ The functor is contravariant in this position.
              | Phantom  -- ^ This position is not used in any constructor.
type Variances = [Variance]

-- | If a 'KFunctor' needs to map an @f ... a ...@ to an @f ... b ...@,
-- a @Mapping v a b@ specifies which function needs to be provided
-- for that position depending on its variance @v@.
type family Mapping (v :: Variance) a b where
  Mapping 'Co      a b = a -> b
  Mapping 'Contra  a b = b -> a
  Mapping 'Phantom a b = ()

infixr 5 :^:
-- | List of mappings for the list of variances @v@.
data Mappings (v :: Variances) (x :: LoT k) (y :: LoT k) where
  M0    :: Mappings '[] 'LoT0 'LoT0
  (:^:) :: Mapping v a b -> Mappings vs as bs
        -> Mappings (v ': vs) (a ':&&: as) (b ':&&: bs)
