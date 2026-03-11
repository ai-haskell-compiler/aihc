module Data.Tuple.Strict where

-- * Pair

{-# INLINE mapPair #-}
mapPair :: (a -> c, b -> d) -> (a,b) -> (c,d)
mapPair (f,g) (a,b) = (f a, g b)

{-# INLINE mapFst #-}
mapFst :: (a -> c) -> (a,b) -> (c,b)
mapFst f (a,b) = (f a, b)

{-# INLINE mapSnd #-}
mapSnd :: (b -> c) -> (a,b) -> (a,c)
mapSnd f (a,b) = (a, f b)

{-# INLINE zipPair #-}
zipPair :: (a,b) -> (c,d) -> ((a,c),(b,d))
zipPair (a,b) (c,d) = ((a,c),(b,d))

{-# INLINE zipWithPair #-}
zipWithPair :: (a -> c -> e, b -> d -> f) -> (a,b) -> (c,d) -> (e,f)
zipWithPair (e,f) (a,b) (c,d) = (e a c, f b d)


{-# INLINE swap #-}
swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)


-- * Triple

{-# INLINE mapTriple #-}
mapTriple :: (a -> d, b -> e, c -> f) -> (a,b,c) -> (d,e,f)
mapTriple (f,g,h) (a,b,c) = (f a, g b, h c)

{-# INLINE mapFst3 #-}
mapFst3 :: (a -> d) -> (a,b,c) -> (d,b,c)
mapFst3 f (a,b,c) = (f a, b, c)

{-# INLINE mapSnd3 #-}
mapSnd3 :: (b -> d) -> (a,b,c) -> (a,d,c)
mapSnd3 f (a,b,c) = (a, f b, c)

{-# INLINE mapThd3 #-}
mapThd3 :: (c -> d) -> (a,b,c) -> (a,b,d)
mapThd3 f (a,b,c) = (a, b, f c)

{-# INLINE zipWithTriple #-}
zipWithTriple ::
   (a -> d -> g, b -> e -> h, c -> f -> i) -> (a,b,c) -> (d,e,f) -> (g,h,i)
zipWithTriple (g,h,i) (a,b,c) (d,e,f) = (g a d, h b e, i c f)

{-# INLINE uncurry3 #-}
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (a,b,c) = f a b c
