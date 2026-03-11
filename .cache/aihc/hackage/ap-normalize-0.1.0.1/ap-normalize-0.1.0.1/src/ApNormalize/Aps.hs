{-# LANGUAGE
  GADTs #-}

-- |
-- The definition of 'Aps'.
-- Most of this is reexported by "ApNormalize".

module ApNormalize.Aps
  ( -- * Normalizing applicative functors
    Aps(..)
  , (<$>^)
  , (<*>^)
  , liftAps
  , lowerAps
  , liftA2Aps
  , apsToApDList
  ) where

import Control.Applicative (liftA2, liftA3)
import ApNormalize.DList

-- | An applicative functor transformer which accumulates @f@-actions (things of type @f x@)
-- in a normal form.
--
-- It constructs a value of type @f a@ with the following syntactic invariant.
-- It depends on the number of @f@-actions @a1 ... an@ composing it,
-- which are delimited using 'liftAps':
--
-- - Zero action: @pure x@
-- - One action: @f \<$> a1@
-- - Two or more actions: @liftA2 f a1 a2 \<*> a3 \<*> ... \<*> an@
data Aps f a where
  Pure :: a -> Aps f a
  FmapLift :: (x -> a) -> f x -> Aps f a
  LiftA2Aps :: (x -> y -> z -> a) -> f x -> f y -> ApDList f z -> Aps f a

infixl 4 <$>^, <*>^

-- | @f \<$>^ u :: Aps f b@ is a delayed representation of @f \<$> u :: f b@,
-- so that it can be fused with other applicative operations.
--
-- @f \<$>^ u@ is a shorthand for @f \<$> 'liftAps' u@.
(<$>^) :: (a -> b) -> f a -> Aps f b
(<$>^) = FmapLift
{-# INLINE (<$>^) #-}

-- | @u \<*>^ v@ appends an @f@-action @v@ to the right of an @('Aps' f)@-action @u@.
--
-- @u \<*>^ v@ is a shorthand for @u \<*> 'liftAps' v@.
(<*>^) :: Applicative f => Aps f (a -> b) -> f a -> Aps f b
u <*>^ v = u <*> liftAps v
{-# INLINE (<*>^) #-}

-- | Lift an @f@-action into @'Aps' f@.
liftAps :: f a -> Aps f a
liftAps = FmapLift id
{-# INLINE liftAps #-}

-- | Lower an @f@-action from @'Aps' f@.
lowerAps :: Applicative f => Aps f a -> f a
lowerAps (Pure x) = pure x
lowerAps (FmapLift f u) = fmap f u
lowerAps (LiftA2Aps f u v w) =
   lowerApDList (Yoneda (\k -> liftA2 (\x y -> k (f x y)) u v)) w
{-# INLINE lowerAps #-}

instance Functor (Aps f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (FmapLift g u) = FmapLift (f . g) u
  fmap f (LiftA2Aps g u v w) = LiftA2Aps ((fmap . fmap . fmap) f g) u v w
  {-# INLINE fmap #-}

instance Applicative f => Applicative (Aps f) where
  pure = Pure
  Pure f <*> uy = fmap f uy
  FmapLift f ux <*> uy = liftA2Aps f ux uy
  LiftA2Aps f u v w <*> ww =
    LiftA2Aps (\x y (z, zz) -> f x y z zz) u v (liftA2 (,) w (apsToApDList ww))
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

-- | Append an action to the left of an 'Aps'.
liftA2Aps :: Applicative f => (a -> b -> c) -> f a -> Aps f b -> Aps f c
liftA2Aps f ux (Pure y) = FmapLift (\x -> f x y) ux
liftA2Aps f ux (FmapLift g uy) = LiftA2Aps (\x y _ -> f x (g y)) ux uy (pure ())
liftA2Aps f ux (LiftA2Aps g u v w) =
  LiftA2Aps (\x y (z, zz) -> f x (g y z zz)) ux u (liftA2 (,) (liftApDList v) w)
{-# INLINE liftA2Aps #-}

-- | Conversion from 'Aps' to 'ApDList'.
apsToApDList :: Applicative f => Aps f a -> ApDList f a
apsToApDList (Pure x) = pure x
apsToApDList (FmapLift f u) = fmap f (liftApDList u)
apsToApDList (LiftA2Aps f u v w) = liftA3 f (liftApDList u) (liftApDList v) w
{-# INLINE apsToApDList #-}
