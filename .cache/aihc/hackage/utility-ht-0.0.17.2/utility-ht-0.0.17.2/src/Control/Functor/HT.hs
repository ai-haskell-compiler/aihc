module Control.Functor.HT where

import qualified Data.Tuple.HT as Tuple
import Data.Tuple.HT (fst3, snd3, thd3)

import qualified Prelude as P
import Prelude (Functor, fmap, flip, const, (.), ($), fst, snd)


void :: Functor f => f a -> f ()
void = fmap (const ())

map :: Functor f => (a -> b) -> f a -> f b
map = fmap

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap


{- |
Caution:
Every pair member has a reference to the argument of 'unzip'.
Depending on the consumption pattern this may cause a memory leak.
For lists, I think, you should generally prefer 'List.unzip'.
-}
unzip :: Functor f => f (a, b) -> (f a, f b)
unzip x = (fmap fst x, fmap snd x)

{- |
Caution: See 'unzip'.
-}
unzip3 :: Functor f => f (a, b, c) -> (f a, f b, f c)
unzip3 x = (fmap fst3 x, fmap snd3 x, fmap thd3 x)


{- |
Caution: See 'unzip'.
-}
uncurry :: Functor f => (f a -> f b -> g) -> f (a, b) -> g
uncurry f = P.uncurry f . unzip

{- |
Caution: See 'unzip'.
-}
uncurry3 :: Functor f => (f a -> f b -> f c -> g) -> f (a, b, c) -> g
uncurry3 f = Tuple.uncurry3 f . unzip3


mapFst :: Functor f => (a -> f c) -> (a, b) -> f (c, b)
mapFst f ~(a,b) = fmap (flip (,) b) $ f a

mapSnd :: Functor f => (b -> f c) -> (a, b) -> f (a, c)
mapSnd f ~(a,b) = fmap ((,) a) $ f b


mapFst3 :: Functor f => (a -> f d) -> (a,b,c) -> f (d,b,c)
mapFst3 f ~(a,b,c) = fmap (\x -> (x,b,c)) $ f a

mapSnd3 :: Functor f => (b -> f d) -> (a,b,c) -> f (a,d,c)
mapSnd3 f ~(a,b,c) = fmap (\x -> (a,x,c)) $ f b

mapThd3 :: Functor f => (c -> f d) -> (a,b,c) -> f (a,b,d)
mapThd3 f ~(a,b,c) = fmap ((,,) a b) $ f c


{- |
Generalization of 'Data.List.HT.outerProduct'.
-}
outerProduct ::
   (Functor f, Functor g) =>
   (a -> b -> c) -> f a -> g b -> f (g c)
outerProduct f xs ys = fmap (flip fmap ys . f) xs
