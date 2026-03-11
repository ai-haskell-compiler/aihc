module Data.Function.HT (
   Id, nest, powerAssociative, compose2,
   ) where

import Data.Function.HT.Private (nest, powerAssociative, )


{- |
Useful for adding type annotations like in

> f . (id :: Id Char) . g
-}
type Id a = a -> a

{- |
Known as @on@ in newer versions of the @base@ package.
-}
{-# INLINE compose2 #-}
compose2 :: (b -> b -> c) -> (a -> b) -> (a -> a -> c)
compose2 g f x y = g (f x) (f y)
