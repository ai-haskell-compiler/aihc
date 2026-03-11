module Data.Tuple.HT (
   -- * Pair
   mapPair,
   mapFst,
   mapSnd,
   swap,
   sortPair,
   forcePair,
   double,

   -- * Triple
   fst3,
   snd3,
   thd3,
   mapTriple,
   mapFst3,
   mapSnd3,
   mapThd3,
   curry3,
   uncurry3,
   triple,
   ) where

import Data.Tuple.Lazy


{- |
Known as @dup@ in the 'Arrow' literature.
-}
{-# INLINE double #-}
double :: a -> (a,a)
double a = (a,a)

{-# INLINE triple #-}
triple :: a -> (a,a,a)
triple a = (a,a,a)

{-# INLINE fst3 #-}
fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

{-# INLINE snd3 #-}
snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

{-# INLINE thd3 #-}
thd3 :: (a,b,c) -> c
thd3 (_,_,x) = x

{-# INLINE curry3 #-}
curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a,b,c)

{- |
This is convenient for quick hacks
but I suggest that you better define a type for an ordered pair
for your application at hand.
This way, you can clearly see from the type that a pair is ordered.
-}
sortPair, _sortPairMinMax :: (Ord a) => (a,a) -> (a,a)
sortPair (x,y) = if x<=y then (x,y) else (y,x)
_sortPairMinMax (x,y) = (min x y, max x y)
