-- |
-- Module      :  Data.IntermediateStructures1
-- Copyright   :  (c) Oleksandr Zhabenko 2019-2024
-- License     :  MIT
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- Some simple functions to deal with transformations from structures to other ones, basically lists.
--
-- The function 'mapI' is taken from the [mmsyn5](https://hackage.haskell.org/package/mmsyn5) package. This inspired some more general functionality.

{-# LANGUAGE NoImplicitPrelude, BangPatterns #-}
{-# OPTIONS_HADDOCK -show-extensions #-}
{-# OPTIONS_GHC -O #-}

module Data.IntermediateStructures1
 (
    -- * Operations to apply a function or two different functions to an element of the outer list (some of them create inner list)  
       mapI
       , map2I
    -- * Generalized construction functions for some other ones (generative functions)
       , inter
       , inter'
       , swapinter
       , swapinter'
  )
where

import GHC.Base
import GHC.List (concatMap)

-- | Function that applies additional function @f :: a -> [a]@ to @a@ if @p :: a -> Bool@ and @p a = True@
mapI :: (a -> Bool) -> (a -> [a]) -> [a] -> [a]
mapI p f (x:xs)  = 
     case p x of
       True -> f x `mappend` mapI p f xs
       _    -> x:mapI p f xs
mapI _ _ _ = []
{-# NOINLINE mapI #-}

-- | Function that applies additional function @f :: a -> [[a]]@ to @a@ if @p :: a -> Bool@ and @p a = True@
map2I :: (a -> Bool) -> (a -> [[a]]) -> [a] -> [a]
map2I p f (x:xs) = 
   case p x of
       True -> (mconcat . f $ x) `mappend` map2I p f xs
       _    -> x : map2I p f xs
map2I _ _ _ = []
{-# NOINLINE map2I #-}

-- | Some general transformation where the arguments that are not present are calculated from the one data argument @a@ being just present. Can be used to contstruct function @a -> d@ from some additional ideas.
inter :: (a -> b) -> (a -> c) -> (a -> b -> c -> d) -> a -> d
inter fb fc f3d x = f3d x (fb x) (fc x) 
{-# INLINE inter #-}

-- | A variant of the 'inter' with swapped two first arguments. The following takes place:
-- > swapinter f g == inter g f
swapinter :: (a -> c) -> (a -> b) -> (a -> b -> c -> d) -> a -> d
swapinter fc fb f3d x = f3d x (fb x) (fc x)
{-# INLINE swapinter #-}

-- | A variant of 'inter' with \'stricter\' calculation scheme. Can be in many cases more efficient.
inter' :: (a -> b) -> (a -> c) -> (a -> b -> c -> d) -> a -> d
inter' fb fc f3d !x = f3d x (fb x) (fc x) 
{-# INLINE inter' #-}

-- | A variant of the 'swapinter' with \'stricter\' calculation scheme. Can be in many cases more efficient.
swapinter' :: (a -> c) -> (a -> b) -> (a -> b -> c -> d) -> a -> d
swapinter' fc fb f3d !x = f3d x (fb x) (fc x)
{-# INLINE swapinter' #-}

