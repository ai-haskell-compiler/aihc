-- | This module is provided for "Haskell 2022" compatibility.
-- If you are able to use @Rank2Types@, I advise you to instead use the rank 2 aliases
--
-- * @Adapter@, @Adapter'@
--
-- * @Prism@, @Prism'@
--
-- * @Lens@, @Lens'@
--
-- * @Traversal@, @Traversal'@
--
-- * @Setter@, @Setter'@
--
-- * @Grate@, @Grate'@
--
-- * @Resetter@, @Resetter'@
--
-- * @Grid@, @Grid'@
--
-- * @Fold@, @Fold'@
--
-- * @Getter@, @Getter'@
--
-- * @Reviewer@, @Reviewer'@
--
-- from the @lens-family@ package instead.
--
-- 'cloneLens' allows one to circumvent the need for rank 2 types by allowing one to take a universal monomorphic lens instance and rederive a polymorphic instance.
-- When you require a lens family parameter you use the type @'ALens' s t a b@ (or @'ALens'' s a@).
-- Then, inside a @where@ clause, you use 'cloneLens' to create a 'Lens' type.
--
-- For example.
--
-- > example :: ALens s t a b -> Example
-- > example l = ... x^.cl ... cl .~ y ...
-- >  where
-- >   cl x = cloneLens l x
--
-- /Note/: It is important to eta-expand the definition of 'cl' to avoid the dreaded monomorphism restriction.
--
-- 'cloneAdapter', 'cloneGrate', 'cloneTraversal', 'cloneSetter', 'cloneResetter', 'cloneGetter', and 'cloneFold' provides similar functionality for adapters, grates, traversals, setters, resetters, getters, and folds respectively.  Unfortunately, it is not yet known how to clone prisms and grids.
--
-- /Note/: Cloning is only need if you use a functional reference multiple times with different instances.
module Lens.Family.Clone
  ( cloneAdapter, cloneLens, cloneGrate, cloneTraversal, cloneSetter, cloneResetter, cloneGetter, cloneFold
  -- * Types
  , AnAdapter, AnAdapter'
  , ALens, ALens'
  , ATraversal, ATraversal'
  , AGetter, AGetter'
  , AFold, AFold'
  , PStore, PKleeneStore
  -- * Re-exports
  , LensLike, LensLike', GrateLike, GrateLike', FoldLike, FoldLike', AGrate, ASetter, AResetter
  , Phantom, Identical
  ) where

import Lens.Family.Unchecked
import Lens.Family

data PStore i j a = PStore (j -> a) i
instance Functor (PStore i j) where
  fmap f (PStore g i) = PStore (f . g) i

-- | AnAdapter s t a b is a universal Adapter s t a b instance
type AnAdapter s t a b = AdapterLike (PStore (s -> a) b) ((->) s) s t a b
-- | AnAdapter' s a is a universal Adapter' s a instance
type AnAdapter' s a = AdapterLike' (PStore (s -> a) a) ((->) s) s a

-- | Converts a universal adapter instance back into a polymorphic adapter.
cloneAdapter :: (Functor f, Functor g) => AnAdapter s t a b -> AdapterLike f g s t a b
cloneAdapter univ = adapter yin yang
 where
  PStore yang yin = univ (PStore id) id

-- | ALens s t a b is a universal Lens s t a b instance
type ALens s t a b = LensLike (PStore a b) s t a b

-- | ALens' s a is a universal Lens' s a instance
type ALens' s a = LensLike' (PStore a a) s a

-- | Converts a universal lens instance back into a polymorphic lens.
cloneLens :: Functor f => ALens s t a b -> LensLike f s t a b
cloneLens univ f = experiment f . univ (PStore id)

experiment :: Functor f => (a -> f b) -> PStore a b t -> f t
experiment f (PStore g a) = g <$> f a

data PKleeneStore i j a = Unit a
                        | Battery (PKleeneStore i j (j -> a)) i

instance Functor (PKleeneStore i j) where
  fmap f (Unit a) = Unit (f a)
  fmap f (Battery g i) = Battery (fmap (f .) g) i

instance Applicative (PKleeneStore i j) where
  pure = Unit
  Unit f <*> a = f <$> a
  Battery f b <*> a = Battery (flip <$> f <*> a) b

-- | ATraversal s t a b is a universal Traversal s t a b instance
type ATraversal s t a b = LensLike (PKleeneStore a b) s t a b

-- | ATraversal' a b is a universal Traversal' a b instance
type ATraversal' s a = LensLike' (PKleeneStore a a) s a

-- | Converts a universal traversal instance back into a polymorphic traversal.
cloneTraversal :: Applicative f => ATraversal s t a b -> LensLike f s t a b
cloneTraversal univ f = research f . univ (Battery (Unit id))

research :: Applicative f => (a -> f b) -> PKleeneStore a b t -> f t
research _ (Unit a) = pure a
research f (Battery g b) = research f g <*> f b

-- | Converts a universal setter instance back into a polymorphic setter.
cloneSetter :: Identical f => ASetter s t a b -> LensLike f s t a b
cloneSetter = setting . over

-- | AFold s t a b is a universal Fold s t a b instance
type AFold s t a b = FoldLike [a] s t a b

-- | AFold' s a is a universal Fold' s a instance
type AFold' s a = FoldLike' [a] s a

-- | Converts a universal fold instance back into a polymorphic fold.
cloneFold :: (Phantom f, Applicative f) => AFold s t a b -> LensLike f s t a b
cloneFold univ = folding (toListOf univ)

-- | Converts a universal resetter instance back into a polymorphic resetter.
cloneResetter :: Identical f => AResetter s t a b -> GrateLike f s t a b
cloneResetter = resetting . under

-- | AGetter s t a b is a universal Getter s t a b instance
type AGetter s t a b = FoldLike a s t a b

-- | AGetter' s a is a universal Getter' s a instance
type AGetter' s a = FoldLike' a s a

-- | Converts a universal getter instance back into a polymorphic getter.
cloneGetter :: Phantom f => AGetter s t a b -> LensLike f s t a b
cloneGetter univ = to (view univ)

-- | Converts a universal grate instance back into a polymorphic grater.
cloneGrate :: Functor g => AGrate s t a b -> GrateLike g s t a b
cloneGrate = grate . degrating
