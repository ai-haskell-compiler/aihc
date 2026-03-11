{-# LANGUAGE Rank2Types #-}
-- | /Caution/: Improper use of this module can lead to unexpected behaviour if the preconditions of the functions are not met.
module Lens.Family2.Unchecked (
-- * Adapters
-- | An adapter represents a isomorphism between two types or a parametric isomorphism between two families of types.
-- For example we can build an adapter between the type families @'Either' a a@ and @('Bool', a)@ as follows:
--
-- > timesTwo :: Adapter (Either a a) (Either b b) (Bool, a) (Bool b)
-- > timesTwo f x = fmap yang . f . fmap yin
-- >  where
-- >   yin (True, a) = Left a
-- >   yin (False, a) = Right a
-- >   yang (Left a) = (True, a)
-- >   yang (Right a) = (False, a)
--
-- /Note/: It is possible to adapters without even depending on @lens-family@ by expanding away the type synonym.
--
-- > timesTwo :: (Functor f, Functor g) => (g (Either a a) -> f (Either b b)) -> g (Bool, a) -> f (Bool, b)
--
-- The function 'adapter' can also be used to construct adapters from a pair of mutually inverse functions.

-- * Lenses
-- | A lens focuses on a field of record type.
-- Lenses can be used to get and/or set the focused field.
-- How to create a lens family is best illustrated by the common example of a field of a record:
--
-- > data MyRecord a = MyRecord { _myA :: a, _myInt :: Int }
-- >
-- > -- The use of type variables a and b allow for polymorphic updates.
-- > myA :: Lens (MyRecord a) (MyRecord b) a b
-- > myA f (MyRecord a i) = (\b -> MyRecord b i) <$> f a
-- >
-- > -- The field _myInt is monomorphic, so we can use a 'Lens'' type.
-- > -- However, the structure of the function is exactly the same as for Lens.
-- > myInt :: Lens' (MyRecord a) Int
-- > myInt f (MyRecord a i) = (\i' -> MyRecord a i') <$> f i
--
-- See the @lens-family-th@ package to generate this sort of code using Template Haskell.
--
-- /Note/: It is possible to build lenses without even depending on @lens-family@ by expanding away the type synonym.
--
-- > myA :: Functor f => (a -> f b) -> (MyRecord a) -> f (MyRecord b)
--
-- You can build lenses for more than just fields of records.
-- Any value @l :: Lens a a' b b'@ is well-defined when it satisfies the two van Laarhoven lens laws:
--
-- * @l Identity === Identity@
--
-- * @l (Compose . fmap f . g) === Compose . fmap (l f) . (l g)@
--
-- The function 'lens' can also be used to construct lenses.
-- The resulting lenses will be well-defined so long as their preconditions are satisfied.

-- * Traversals
-- | If you have zero or more fields of the same type of a record, a traversal can be used to refer to all of them in order.
-- Multiple references are made by replacing the 'Functor' constraint of lenses with an 'Control.Applicative.Applicative' constraint.
-- Consider the following example of a record with two 'Int' fields.
--
-- > data MyRecord = MyRecord { _myA :: Int, _myB :: Int, _myC :: Bool }
-- >
-- > -- myInts is a traversal over both fields of MyRecord.
-- > myInts :: Traversal' MyRecord Int
-- > myInts f (MyRecord a b c) = MyRecord <$> f a <*> f b <*> pure c
--
-- If the record and the referenced fields are parametric, you can can build polymrphic traversals.
-- Consider the following example of a record with two 'Maybe' fields.
--
-- > data MyRecord a = MyRecord { _myA0 :: Maybe a, _myA1 :: Maybe a, myC :: Bool }
-- >
-- > -- myMaybes is a traversal over both fields of MyRecord.
-- > myMaybes :: Traversal (MyRecord a) (MyRecord b) (Maybe a) (Maybe b)
-- > myMaybes f (MyRecord a0 a1 c) = MyRecord <$> f a0 <*> f a1 <*> pure c
--
-- /Note/: It is possible to build traversals without even depending on @lens-family@ by expanding away the type synonym.
--
-- > myMaybes :: Applicative f => (Maybe a -> f (Maybe b)) -> MyRecord a -> f (MyRecord b)
-- > myMaybes f (MyRecord a0 a1 c) = MyRecord <$> f a0 <*> f a1 <*> pure c
--
-- Unfortunately, there are no helper functions for making traversals.
-- In most cases, you must make them by hand.
--
-- Any value @t :: Traversal s t a b@ is well-defined when it satisfies the two van Laarhoven traversal laws:
--
-- * @t Identity === Identity@
--
-- * @t (Compose . fmap f . g) === Compose . fmap (t f) . (t g)@
--
-- 'Data.Traversable.traverse' is the canonical traversal for various containers.

-- * Prisms
-- | A prism focuses on a single variant of a type.
-- They can be used to 'Lens.Family2.matching' / 'Lens.Family2.review' the focused variant.
-- Consider the following example.
--
-- > data MySum a = MyA a | MyB Int
-- >
-- > -- myA is a prism for the MyA variant of MySum
-- > myA :: Prism (MySum a) (MySum b) a b
-- > myA f = either pure (fmap MyA . f) . traverse h
-- >  where
-- >   h (MyA a) = Right a
-- >   h (MyB n) = Left (MyB n)
--
-- This prism can be used with 'Lens.Family2.matching' via 'Lens.Family2.under':
--
-- @ 'Lens.Family2.matching' ('Lens.Family2.under' myA) :: MySum a -> Either (MySum b) a @
--
-- This prism can be used with 'Lens.Family2.review' via 'Lens.Family2.over':
--
-- @ 'Lens.Family2.review' ('Lens.Family2.over' myA) :: a -> MySum a @
--
-- /Note/: It is possible to build prisms without even depending on @lens-family@ by expanding away the type synonym.
--
-- > myA :: (Appicative f, Traversable g) => (g a -> f b) -> g (MySum a) -> f (MySum b)
--
-- You can build prism for more than just constructors of sum types.
-- Any value @p :: Prism s t a b@ is well-defined when it satisfies the prism laws:
--
-- * @matching (under p) (review (over p) b) === Right b@
--
-- * @(id ||| review (over p)) (matching (under p) s) === s@
--
-- * @left (match (under p)) (matching (under p) s) === left Left (matching (under p) s)@
--
-- The function 'prism' can also be used to construct prisms.
-- The resulting prisms will be well-defined so long as their preconditions are satisfied.

-- * Grates
-- | A grate focuses on the contents of a representable functor.
-- In other words, a grate focuses on the codomain of a function type or something isomorphic to a function type.
-- They are used to lift operations on this codomain to operations on the larger structure via zipping.
-- Consider the following example of a stream of 'Int's.
--
-- > data IntStream = IntStream { hd :: Int, tl :: IntStream }
-- >
-- > -- myInts is a grate over the Ints of IntStream.
-- > myInts :: Grate g IntStream Int
-- > myInts f s = IntStream (f (hd <$> s)) (myInts f (tl <$> s))
--
-- If the contents are parametric, you can can build polymorphic grates.
-- Consider the following example of a generic stream.
--
-- > data Stream a = Stream { hd :: a, tl :: Stream a }
-- >
-- > -- myStream is a grate over the contents of a Stream.
-- > myStream :: Grate (Stream a) (Stream b) a b
-- > myStream f s = Stream (f (hd <$> s)) (myStream f (tl <$> s))
--
-- /Note/: It is possible to build grates without even depending on @lens-family@ by expanding away the type synonym.
--
-- > myStream :: Functor g => (g a -> b) -> g (Stream a) -> (Stream b)
--
-- Any value @t :: Grate s t a b@ is a well-defined grate when it satisfies the two van Laarhoven traversal laws:
--
-- * @t runIdentity === runIdentity@
--
-- * @t (f . fmap g . runCompose) === (t f) . fmap (t g) . runCompose@
--
-- The function 'grate' can also be used to construct grates from graters.
-- The resulting grates will be well-defined so long as the preconditions are satisfied.

-- * Grids
-- | A grid is both a traversal and a grate.
-- When you have a type that is isomorphic to a fixed and finite number of copies of another type, a grid can be used to zip or traverse them.
-- Consider the following example of a record with exactly two 'Int' fields.
--
-- > data MyRecord = MyRecord { _myA :: Int, _myB :: Int }
-- >
-- > -- myInts is a grid over both fields of MyRecord.
-- > myInts :: Grid f g MyRecord Int
-- > myInts f r = MyRecord <$> f (_myA <$> r) <*> f (_myB <$> r)
--
-- If the record and the referenced fields are parametric, you can can build polymorphic grids.
-- Consider the following example of a record with exactly two 'Maybe' fields.
--
-- > data MyRecord a = MyRecord { _myA0 :: Maybe a, _myA1 :: Maybe a }
-- >
-- > -- myMaybes is a traversal over both fields of MyRecord.
-- > myMaybes :: Grid (MyRecord a) (MyRecord b) (Maybe a) (Maybe b)
-- > myMaybes f r = MyRecord <$> f (_myA0 <$> r) <*> f (_myA1 <$> r)
--
-- A grid is converted into a grate by using the 'Lens.Family2.over' function, and it is converted to a traversal by using the 'Lens.Family2.under' function.
--
-- /Note/: It is possible to build grids without even depending on @lens-family@ by expanding away the type synonym.
--
-- > myMaybes :: (Applicative f, Functor g) => (g (Maybe a) -> f (Maybe b)) -> g (MyRecord a) -> f (MyRecord b)
--
-- Unfortunately, there are no helper functions for making grids.
-- In most cases, you must make them by hand.

-- * Documentation
    adapter
  , lens
  , prism
  , grate
  , setting
  , resetting
-- * Types
  , Adapter, Adapter'
  , Prism, Prism'
  , Lens, Lens'
  , Traversal, Traversal'
  , Setter, Setter'
  , Grate, Grate'
  , Resetter, Resetter'
  , LF.AdapterLike, LF.AdapterLike'
  , LF.LensLike, LF.LensLike'
  , LF.GrateLike, LF.GrateLike'
  , LF.Identical
  ) where

import qualified Lens.Family.Unchecked as LF

type Adapter s t a b = forall f g. (Functor f, Functor g) => LF.AdapterLike f g s t a b
type Adapter' s a = forall f g. (Functor f, Functor g) => LF.AdapterLike' f g s a

type Prism s t a b = forall f g. (Applicative f, Traversable g) => LF.AdapterLike f g s t a b
type Prism' s a = forall f g. (Applicative f, Traversable g) => LF.AdapterLike' f g s a

type Lens s t a b = forall f. Functor f => LF.LensLike f s t a b
type Lens' s a = forall f. Functor f => LF.LensLike' f s a

type Traversal s t a b = forall f. Applicative f => LF.LensLike f s t a b
type Traversal' s a = forall f. Applicative f => LF.LensLike' f s a

type Setter s t a b = forall f. LF.Identical f => LF.LensLike f s t a b
type Setter' s a = forall f. LF.Identical f => LF.LensLike' f s a

type Grate s t a b = forall g. Functor g => LF.GrateLike g s t a b
type Grate' s a = forall g. Functor g => LF.GrateLike' g s a

type Resetter s t a b = forall g. LF.Identical g => LF.GrateLike g s t a b
type Resetter' s a = forall g. LF.Identical g => LF.GrateLike' g s a

-- | Build an adapter from an isomorphism family.
--
-- /Caution/: In order for the generated adapter family to be well-defined, you must ensure that the two isomorphism laws hold:
--
-- * @yin . yang === id@
--
-- * @yang . yin === id@
adapter :: (s -> a) -- ^ yin
        -> (b -> t) -- ^ yang
        -> Adapter s t a b
adapter sa bt = LF.adapter sa bt

-- | Build a lens from a @getter@ and @setter@ family.
--
-- /Caution/: In order for the generated lens family to be well-defined, you must ensure that the three lens laws hold:
--
-- * @getter (setter s a) === a@
--
-- * @setter s (getter s) === s@
--
-- * @setter (setter s a1) a2 === setter s a2@
lens :: (s -> a) -- ^ getter
     -> (s -> b -> t) -- ^ setter
     -> Lens s t a b
lens sa sbt = LF.lens sa sbt

grate :: (((s -> a) -> b) -> t) -- ^ grater
      -> Grate s t a b
-- ^ Build a grate from a @grater@ family.
--
-- /Caution/: In order for the generated grate family to be well-defined, you must ensure that the two grater laws hold:
--
-- * @grater ($ s) === s@
--
-- * @grater (\k -> h (k . grater)) === grater (\k -> h ($ k))@
--
-- Note: The grater laws are that of an algebra for the parameterised continuation monad, `Lens.Family.PCont`.
grate sabt = LF.grate sabt

-- | Build a prism from a @matcher@ and @reviewer@ family.
--
-- /Caution/: In order for the generated prism family to be well-defined, you must ensure that the three prism laws hold:
--
-- * @matcher (reviewer b) === Right b@
--
-- * @(id ||| reviewer) (matcher s) === s@
--
-- * @left matcher (matcher s) === left Left (matcher s)@
prism :: (s -> Either t a) -- ^ matcher
      -> (b -> t) -- ^ reviewer
      -> Prism s t a b
prism sta bt = LF.prism sta bt

-- | 'setting' promotes a \"semantic editor combinator\" to a modify-only lens.
-- To demote a lens to a semantic edit combinator, use the section @(l %~)@ or @over l@ from "Lens.Family2".
--
-- >>> [("The",0),("quick",1),("brown",1),("fox",2)] & setting map . fstL %~ length
-- [(3,0),(5,1),(5,1),(3,2)]
--
-- /Caution/: In order for the generated family to be well-defined, you must ensure that the two functors laws hold:
--
-- * @sec id === id@
--
-- * @sec f . sec g === sec (f . g)@
setting :: ((a -> b) -> s -> t) -- ^ sec (semantic editor combinator)
        -> Setter s t a b
setting abst = LF.setting abst

-- | 'resetting' promotes a \"semantic editor combinator\" to a form of grate that can only lift unary functions.
-- To demote a grate to a semantic edit combinator, use @under l@ from "Lens.Family2".
--
-- /Caution/: In order for the generated family to be well-defined, you must ensure that the two functors laws hold:
--
-- * @sec id === id@
--
-- * @sec f . sec g === sec (f . g)@
resetting :: ((a -> b) -> s -> t) -- ^ sec (semantic editor combinator)
        -> Resetter s t a b
resetting abst = LF.resetting abst
