module Control.Applicative.HT where

import qualified Data.Tuple.HT as Tuple

import Control.Applicative (Applicative, liftA2, liftA3, (<$>), (<*>), )

mapPair :: (Applicative f) => (a -> f c, b -> f d) -> (a,b) -> f (c,d)
mapPair fg = uncurry (liftA2 (,)) . Tuple.mapPair fg

mapTriple ::
   (Applicative m) => (a -> m d, b -> m e, c -> m f) -> (a,b,c) -> m (d,e,f)
mapTriple fgh = Tuple.uncurry3 (liftA3 (,,)) . Tuple.mapTriple fgh


curry :: (Applicative f) => (f (a,b) -> g) -> f a -> f b -> g
curry f a b = f $ lift2 (,) a b

curry3 :: (Applicative f) => (f (a,b,c) -> g) -> f a -> f b -> f c -> g
curry3 f a b c = f $ lift3 (,,) a b c


{-# INLINE lift #-}
lift :: Applicative m => (a -> r) -> m a -> m r
lift = fmap

{-# INLINE lift2 #-}
lift2 ::
   Applicative m => (a -> b -> r) -> m a -> m b -> m r
lift2 = liftA2

{-# INLINE lift3 #-}
lift3 ::
   Applicative m => (a -> b -> c -> r) -> m a -> m b -> m c -> m r
lift3 = liftA3

{-# INLINE lift4 #-}
lift4 ::
   Applicative m =>
   (a -> b -> c -> d -> r) -> m a -> m b -> m c -> m d -> m r
lift4 fn a b c d = fn <$> a <*> b <*> c <*> d

{-# INLINE lift5 #-}
lift5 ::
   Applicative m =>
   (a -> b -> c -> d -> e -> r) ->
   m a -> m b -> m c -> m d -> m e -> m r
lift5 fn a b c d e = fn <$> a <*> b <*> c <*> d <*> e

{-# INLINE lift6 #-}
lift6 ::
   Applicative m =>
   (a -> b -> c -> d -> e -> f -> r) ->
   m a -> m b -> m c -> m d -> m e -> m f -> m r
lift6 fn a b c d e f = fn <$> a <*> b <*> c <*> d <*> e <*> f


{-# DEPRECATED liftA4 "use App.lift4" #-}
{-# INLINE liftA4 #-}
liftA4 :: Applicative f =>
   (a -> b -> c -> d -> e) ->
   f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = f <$> a <*> b <*> c <*> d

{-# DEPRECATED liftA5 "use App.lift5" #-}
{-# INLINE liftA5 #-}
liftA5 :: Applicative f =>
   (a -> b -> c -> d -> e -> g) ->
   f a -> f b -> f c -> f d -> f e -> f g
liftA5 f a b c d e = f <$> a <*> b <*> c <*> d <*> e

{-# DEPRECATED liftA6 "use App.lift6" #-}
{-# INLINE liftA6 #-}
liftA6 :: Applicative f =>
   (a -> b -> c -> d -> e -> g -> h) ->
   f a -> f b -> f c -> f d -> f e -> f g -> f h
liftA6 f a b c d e g = f <$> a <*> b <*> c <*> d <*> e <*> g
