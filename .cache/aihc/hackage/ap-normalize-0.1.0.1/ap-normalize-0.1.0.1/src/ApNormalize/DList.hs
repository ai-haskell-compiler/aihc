{-# LANGUAGE
  RankNTypes #-}

-- | This structure is part of the definition of 'ApNormalize.Aps'.

module ApNormalize.DList
  ( -- * Applicative difference lists
    ApDList(..)
  , liftApDList
  , lowerApDList
  , Yoneda(..)
  ) where

-- | Type of applicative difference lists.
--
-- An applicative transformer which accumulates @f@-actions in
-- a left-nested composition using @('<*>')@.
--
-- 'ApDList' represents a sequence of @f@-actions
-- @u1 :: f x1@, ... @un :: f xn@ as "term with a hole"
-- @(_ \<*> u1 \<*> ... \<*> un) :: f r@.
--
-- That hole must have type  @_ :: f (x1 -> ... -> un -> r)@;
-- the variable number of arrows is hidden by existential quantification
-- and continuation passing.
--
-- To help ensure that syntactic invariant,
-- the 'Functor' and 'Applicative' instances for 'ApDList' have no constraints.
-- 'liftApDList' is the only function whose signature requires an
-- @'Applicative' f@ constraint, wrapping each action @u@ inside one @('<*>')@.
newtype ApDList f a = ApDList (forall r. Yoneda f (a -> r) -> f r)

-- | A difference list with one element @u@, denoted @_ \<*> u@.
liftApDList :: Applicative f => f a -> ApDList f a
liftApDList u = ApDList (\(Yoneda t) -> t id <*> u)
{-# INLINE liftApDList #-}

-- | Complete a difference list, filling the hole with the first argument.
lowerApDList :: Yoneda f (b -> c) -> ApDList f b -> f c
lowerApDList u (ApDList v) = v u
{-# INLINE lowerApDList #-}

instance Functor (ApDList f) where
  fmap f (ApDList u) = ApDList (\t -> u (fmap (. f) t))
  {-# INLINE fmap #-}

instance Applicative (ApDList f) where
  pure x = ApDList (\(Yoneda t) -> t (\k -> k x))
  ApDList uf <*> ApDList ux = ApDList (\t -> ux (Yoneda (\c -> uf (fmap (\d e -> c (d . e)) t))))
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

-- | A delayed application of 'fmap' which can be fused with an inner 'fmap' or
-- 'Control.Applicative.liftA2'.
--
-- This is the same definition as in the kan-extensions library, but we
-- redefine it to not pay for all the dependencies.
newtype Yoneda f a = Yoneda (forall x. (a -> x) -> f x)

instance Functor (Yoneda f) where
  fmap f (Yoneda u) = Yoneda (\g -> u (g . f))
