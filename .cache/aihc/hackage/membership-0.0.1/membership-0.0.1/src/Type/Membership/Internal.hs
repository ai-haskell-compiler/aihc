{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Type.Membership.Internal (
  -- * Membership
  Membership
  , getMemberId
  , mkMembership
  , leadership
  , nextMembership
  , testMembership
  , compareMembership
  , impossibleMembership
  , reifyMembership
  -- * Member class
  , Member(..)
  , type (∈)
  , FindType
  -- * Association
  , Assoc(..)
  , type (>:)
  , Lookup(..)
  , FindAssoc
  -- * Sugar
  , Elaborate
  , Elaborated(..)
  -- * HList
  , HList(..)
  , hfoldrWithIndex
  , htraverseWithIndex
  -- * Miscellaneous
  , module Data.Type.Equality
  , module Data.Proxy
  ) where
import Control.DeepSeq (NFData)
import Data.Type.Equality
import Data.Proxy
import Control.Monad
import Unsafe.Coerce
import qualified Data.Kind as K
import Data.Hashable
import Data.Text.Prettyprint.Doc
import Data.Typeable
import Language.Haskell.TH hiding (Pred)
import Language.Haskell.TH.Lift
#if MIN_VERSION_template_haskell(2,17,0)
import Language.Haskell.TH.Syntax
#endif
import Data.Semigroup (Semigroup(..))
import GHC.TypeLits

-- | A witness that of @x@ is a member of a type level set @xs@.
newtype Membership (xs :: [k]) (x :: k) = Membership
  { getMemberId :: Int -- ^ get the position as an 'Int'.
  } deriving (Typeable, NFData)

instance Show (Membership xs x) where
  show (Membership n) = "$(mkMembership " ++ show n ++ ")"

instance Pretty (Membership xs x) where
  pretty (Membership n) = "$(mkMembership " <> pretty n <> ")"

instance Eq (Membership xs x) where
  _ == _ = True

instance Ord (Membership xs x) where
  compare _ _ = EQ

instance Semigroup (Membership xs x) where
  x <> _ = x

-- | Generates a 'Membership' that corresponds to the given ordinal (0-origin).
#if MIN_VERSION_template_haskell(2,17,0)
mkMembership :: Quote m => Int -> m Exp
#else
mkMembership :: Int -> Q Exp
#endif
mkMembership n = do
  let names = map mkName $ take (n + 1) $ concatMap (flip replicateM ['a'..'z']) [1..]
  let rest = mkName "any"
  let cons x xs = PromotedConsT `AppT` x `AppT` xs
  let t = foldr cons (VarT rest) (map VarT names)
  sigE (conE 'Membership `appE` litE (IntegerL $ toInteger n))
#if MIN_VERSION_template_haskell(2,17,0)
    $ forallT (PlainTV rest SpecifiedSpec : map (PlainTV `flip` SpecifiedSpec) names) (pure [])
#else
    $ forallT (PlainTV rest : map PlainTV names) (pure [])
#endif
    $ conT ''Membership `appT` pure t `appT` varT (names !! n)

instance Lift (Membership xs x) where
  lift (Membership i) = mkMembership i
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped (Membership i) = Code $ TExp <$> mkMembership i
#endif
-- | @x@ is a member of @xs@
class Member xs x where
  membership :: Membership xs x

instance (Elaborate x (FindType x xs) ~ 'Expecting pos, KnownNat pos) => Member xs x where
  membership = Membership (fromInteger $ natVal (Proxy :: Proxy pos))
  {-# INLINE membership #-}

instance Hashable (Membership xs x) where
  hashWithSalt s = hashWithSalt s . getMemberId

-- | A readable type search result
data Elaborated k v = Expecting v | Missing k | Duplicate k

-- | Make the result more readable
type family Elaborate (key :: k) (xs :: [v]) :: Elaborated k v where
  Elaborate k '[] = 'Missing k
  Elaborate k '[x] = 'Expecting x
  Elaborate k xs = 'Duplicate k

-- | Find a type associated to the specified key.
type family FindAssoc (n :: Nat) (key :: k) (xs :: [Assoc k v]) where
  FindAssoc n k ((k ':> v) ': xs) = (n ':> v) ': FindAssoc (1 + n) k xs
  FindAssoc n k ((k' ':> v) ': xs) = FindAssoc (1 + n) k xs
  FindAssoc n k '[] = '[]

-- | Embodies a type equivalence to ensure that the 'Membership' points the first element.
testMembership :: Membership (y ': xs) x -> (x :~: y -> r) -> (Membership xs x -> r) -> r
testMembership (Membership 0) l _ = l (unsafeCoerce Refl)
testMembership (Membership n) _ r = r (Membership (n - 1))
{-# INLINE testMembership #-}

-- | Compare two 'Membership's.
compareMembership :: Membership xs x -> Membership xs y -> Either Ordering (x :~: y)
compareMembership (Membership m) (Membership n) = case compare m n of
  EQ -> Right (unsafeCoerce Refl)
  x -> Left x
{-# INLINE compareMembership #-}

-- | There is no 'Membership' of an empty list.
impossibleMembership :: Membership '[] x -> r
impossibleMembership _ = error "Impossible"

-- | This 'Membership' points to the first element
leadership :: Membership (x ': xs) x
leadership = Membership 0
{-# INLINE leadership #-}

-- | The next membership
nextMembership :: Membership xs y -> Membership (x ': xs) y
nextMembership (Membership n) = Membership (n + 1)
{-# INLINE nextMembership #-}

-- | Make up a 'Membership' from an integer.
reifyMembership :: Int -> (forall x. Membership xs x -> r) -> r
reifyMembership n k = k (Membership n)

-- | Unicode flipped alias for 'Member'
type x ∈ xs = Member xs x

-- | FindType types
type family FindType (x :: k) (xs :: [k]) :: [Nat] where
  FindType x (x ': xs) = 0 ': MapSucc (FindType x xs)
  FindType x (y ': ys) = MapSucc (FindType x ys)
  FindType x '[] = '[]

-- | Ideally, it will be 'Map Succ'
type family MapSucc (xs :: [Nat]) :: [Nat] where
  MapSucc '[] = '[]
  MapSucc (x ': xs) = (1 + x) ': MapSucc xs

-- | The kind of key-value pairs
data Assoc k v = k :> v
infix 0 :>

-- | A synonym for (':>')
type (>:) = '(:>)

-- | @'Lookup' xs k v@ is essentially identical to @(k :> v) ∈ xs@
-- , but the type @v@ is inferred from @k@ and @xs@.
class Lookup xs k v | k xs -> v where
  association :: Membership xs (k ':> v)

instance (Elaborate k (FindAssoc 0 k xs) ~ 'Expecting (n ':> v), KnownNat n) => Lookup xs k v where
  association = Membership (fromInteger $ natVal (Proxy :: Proxy n))

data HList (h :: k -> K.Type) (xs :: [k]) where
  HNil :: HList h '[]
  HCons :: h x -> HList h xs -> HList h (x ': xs)

infixr 5 `HCons`

hfoldrWithIndex :: forall h r xs. (forall x. Membership xs x -> h x -> r -> r) -> r -> HList h xs -> r
hfoldrWithIndex f r = go 0 where
  go :: Int -> HList h t -> r
  go !k (HCons x xs) = f (Membership k) x $ go (k + 1) xs
  go _ HNil = r
{-# INLINE hfoldrWithIndex #-}

htraverseWithIndex :: forall f g h xs. Applicative f
    => (forall x. Membership xs x -> g x -> f (h x)) -> HList g xs -> f (HList h xs)
htraverseWithIndex f = go 0 where
  go :: Int -> HList g t -> f (HList h t)
  go !k (HCons x xs) = HCons <$> f (Membership k) x <*> go (k + 1) xs
  go _ HNil = pure HNil
{-# INLINE htraverseWithIndex #-}
