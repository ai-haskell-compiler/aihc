{-# LANGUAGE CPP           #-}
#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE TypeOperators #-}
#endif

module Data.Semigroup.Commutative where

import Data.IntSet (IntSet)
import Data.Monoid
import Data.Ord
import Data.List (unfoldr)
import Data.Set
#if MIN_VERSION_base(4,7,0)
import Data.Proxy
#endif
#if MIN_VERSION_base(4,8,0)
import Data.Void (Void)
#endif
#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup(..))
#endif
#if MIN_VERSION_base(4,9,0)
import Data.Functor.Const
import Data.Functor.Identity
import Data.Semigroup (Max, Min, WrappedMonoid)
#endif
#if MIN_VERSION_base(4,12,0)
import Data.Functor.Contravariant (Op(Op))
import GHC.Generics
#endif
import Numeric.Product.Commutative ( CommutativeProduct )

-- | A 'Commutative' semigroup is a 'Semigroup' that follows the rule:
--
-- @a \<> b == b \<> a@
class
#if MIN_VERSION_base(4,9,0)
     Semigroup g
#else
     Monoid g
#endif
  => Commutative g

-- | Trivial commutative semigroup.
instance Commutative ()

instance Commutative All
instance Commutative Any

#if MIN_VERSION_base(4,8,0)
instance Commutative Void
#endif

#if MIN_VERSION_base(4,9,0)
instance Ord a => Commutative (Max a)
instance Ord a => Commutative (Min a)
instance (Commutative a, Monoid a) => Commutative (WrappedMonoid a)
#endif

-- | @since 0.0.1.0
instance Commutative a => Commutative (Maybe a)

instance Num a => Commutative (Sum a)

instance CommutativeProduct a => Commutative (Product a)

instance Commutative a => Commutative (Dual a)

-- | Functions lift commutative semigroups pointwise.
instance Commutative b => Commutative (a -> b)

-- | Product commutative semigroup.
-- A Pair of commutative semigroups gives rise to a commutative semigroup
instance (Commutative a, Commutative b) => Commutative (a, b)

instance (Commutative a, Commutative b, Commutative c) => Commutative (a, b, c)

instance (Commutative a, Commutative b, Commutative c, Commutative d) => Commutative (a, b, c, d)

instance (Commutative a, Commutative b, Commutative c, Commutative d, Commutative e) => Commutative (a, b, c, d, e)

-- | @since 0.0.2.0
instance Ord a => Commutative (Set a)

-- | @since 0.0.2.0
instance Commutative IntSet

#if MIN_VERSION_base(4,11,0)
instance Commutative a => Commutative (Down a)
#endif

#if MIN_VERSION_base(4,7,0)
-- | Trivial commutative semigroup, Functor style.
instance Commutative (Proxy x)
#endif

-- 'Const' has existed for a long time, but the Monoid instance
-- arrives in base-4.9.0.0. Similarly, 'Identity' was defined in
-- base-4.8.0.0 but doesn't get the Monoid instance until base-4.9.0.0
#if MIN_VERSION_base(4,9,0)
-- | 'Const' lifts commutative semigroups into a functor.
instance Commutative a => Commutative (Const a x)

-- | 'Identity' lifts commutative semigroups pointwise (at only one point).
instance Commutative a => Commutative (Identity a)
#endif

-- (:*:) and (:.:) exist since base-4.6.0.0 but the Monoid instances
-- arrive in base-4.12.0.0.
-- Also, contravariant was moved into base in this version.
#if MIN_VERSION_base(4,12,0)
-- | Product of commutative semigroups, Functor style.
instance (Commutative (f a), Commutative (g a)) => Commutative ((f :*: g) a)

-- See https://gitlab.haskell.org/ghc/ghc/issues/11135#note_111802 for the reason Compose is not also provided.
-- Base does not define Monoid (Compose f g a) so this is the best we can
-- really do for functor composition.
instance Commutative (f (g a)) => Commutative ((f :.: g) a)

instance Commutative a => Commutative (Op a b)
#endif
