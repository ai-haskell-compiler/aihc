module Data.Strictness.HT where

{-# INLINE arguments1 #-}
arguments1 :: (a -> x) -> a -> x
arguments1 f a = f $! a

{-# INLINE arguments2 #-}
arguments2 :: (a -> b -> x) -> a -> b -> x
arguments2 f a b = (f $! a) $! b

{-# INLINE arguments3 #-}
arguments3 :: (a -> b -> c -> x) -> a -> b -> c -> x
arguments3 f a b c = ((f $! a) $! b) $! c

{-# INLINE arguments4 #-}
arguments4 :: (a -> b -> c -> d -> x) -> a -> b -> c -> d -> x
arguments4 f a b c d = (((f $! a) $! b) $! c) $! d

{-# INLINE arguments5 #-}
arguments5 :: (a -> b -> c -> d -> e -> x) -> a -> b -> c -> d -> e -> x
arguments5 f a b c d e = ((((f $! a) $! b) $! c) $! d) $! e
