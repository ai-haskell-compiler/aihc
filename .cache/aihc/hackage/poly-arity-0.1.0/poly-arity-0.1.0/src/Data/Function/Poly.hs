{-# LANGUAGE
    GADTs
  , TypeFamilies
  , KindSignatures
  , DataKinds
  , ConstraintKinds
  , TypeOperators
  , MultiParamTypeClasses
  , FunctionalDependencies
  , PolyKinds
  , FlexibleInstances
  , UndecidableInstances
  , UndecidableSuperClasses
  #-}

module Data.Function.Poly where

import Data.Constraint

-- | Provide a type-level list of /types/ @xs@, and a final result type @r@,
-- construct a chain of arrows @->@ / n-ary function (which is right-associative)
-- of each type in @xs@, ending in @r@.
type family TypeListToArity (xs :: [*]) (r :: *) :: * where
  TypeListToArity '[] r = r -- basis
  TypeListToArity (x ': xs) r = x -> TypeListToArity xs r

-- | The inverse of @TypeListToArity@.
type family ArityToTypeList (r :: *) :: [*] where
  ArityToTypeList (x -> r) = x ': ArityToTypeList r
  ArityToTypeList r = '[]


type family Result (f :: *) :: * where
  Result (x -> r) = Result r
  Result r = r

-- | Trim an n-ary function / chain of arrows @->@ with a type-level list of
-- types @xs@, where each element of @xs@ __must__ unify with each element of
-- the cons-list made with @->@.
type family ArityMinusTypeList (r :: *) (xs :: [*]) :: * where
  ArityMinusTypeList r '[] = r -- basis
  ArityMinusTypeList (x -> r) (x ': xs) = ArityMinusTypeList r xs


type ArityTypeListIso c l r =
  ( ArityMinusTypeList c l ~ r
  , c ~ TypeListToArity l r
  )


-- | Injects a type to the base of the function arity chain.
type family InjectLast (x :: *) (f :: *) :: * where
  InjectLast x f = TypeListToArity (Append (ArityToTypeList f) x) (Result f)


type family Append (xs :: [*]) (x :: *) :: [*] where
  Append '[] y = y ': '[]
  Append (x ': xs) y = x ': Append xs y



-- | Inductively constrain a function's initial arity to match a type list;
-- as a read-only style of static arity assurance.
type family ExpectArity (xs :: [*]) (f :: *) :: Constraint where
  ExpectArity '[] f = () -- basis
  ExpectArity (x ': xs) (x -> remainder) = ExpectArity xs remainder

-- | Expect the last parameter in your stack of arity to have a type.
type family ExpectLast (x :: *) (f :: *) :: Constraint where
  ExpectLast x (x -> remainder) = () -- basis
  ExpectLast x (y -> remainder) = ExpectLast x remainder

-- | Duplicate of <http://hackage.haskell.org/package/singletons-1.1.2.1/docs/Data-Promotion-Prelude-List.html#g:1 singletons>
-- @Head@ function for kind-polymorphic type-level lists.
type family Head (xs :: [k]) :: k where
  Head (x ': xs) = x

type family Tail (xs :: [k]) :: [k] where
  Tail (x ': xs) = xs


data HList (xs :: [*]) where
  HNil  :: HList '[]
  HCons :: (x :: *) -> HList xs -> HList (x ': xs)


-- | Lift the @HList@'s internal type-level list of types to a constraint context.
class ExpectArity xs f => ConsumeArity (xs :: [*]) (f :: *) result | xs f -> result where
  -- | Use a /heterogeneously-typed/ list of values as input to an n-ary function,
  -- where types must unify statically.
  appN :: f -> HList xs -> result

instance ConsumeArity '[] r r where
  appN r _ = r

instance ( ConsumeArity xs f r
         , ExpectArity (x ': xs) (x -> f)
         ) => ConsumeArity (x ': xs) (x -> f) r where
  appN f (HCons x xs) = appN (f x) xs


-- | Shows that an n-ary function @f@ /precisely/ ends with @r@.
type family HasResult (f :: *) (r :: *) :: Constraint where
  HasResult r r = ()
  HasResult (x -> r') r = HasResult r' r
