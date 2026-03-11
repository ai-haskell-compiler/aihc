{- |
Utility functions based only on 'Arrow' class.
-}
module Synthesizer.Causal.Utility where

import Control.Arrow (Arrow, arr, (>>>), (&&&), (^<<), )

import Data.Function.HT (nest, )


map :: (Arrow arrow) => (b -> c) -> arrow a b -> arrow a c
map = (^<<)

pure :: (Arrow arrow) => b -> arrow a b
pure x = arr (const x)

apply :: (Arrow arrow) => arrow a (b -> c) -> arrow a b -> arrow a c
apply f x = uncurry ($) ^<< f&&&x


{-# INLINE chainControlled #-}
chainControlled ::
   (Arrow arrow) =>
   [arrow (c,x) x] -> arrow (c,x) x
chainControlled =
   foldr
      (\p rest -> arr fst &&& p  >>>  rest)
      (arr snd)

{-# INLINE replicateControlled #-}
replicateControlled ::
   (Arrow arrow) =>
   Int -> arrow (c,x) x -> arrow (c,x) x
replicateControlled n p =
   nest n
      (arr fst &&& p  >>> )
      (arr snd)
