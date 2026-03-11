{- 
    Copyright 2010-2014 Mario Blazevic

    This file is part of the Streaming Component Combinators (SCC) project.

    The SCC project is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
    License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
    version.

    SCC is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
    of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along with SCC.  If not, see
    <http://www.gnu.org/licenses/>.
-}

-- | A coroutine can choose to launch another coroutine. In this case, the nested coroutines always suspend to their
-- invoker. If a function from this module, such as 'pogoStickNested', is used to run a nested coroutine, the parent
-- coroutine can be automatically suspended as well. A single suspension can thus suspend an entire chain of nested
-- coroutines.
-- 
-- Nestable coroutines of this kind should group their suspension functors into a 'Sum'. A simple coroutine
-- suspension can be converted to a nested one using functions 'mapSuspension' and 'liftAncestor'. To run nested
-- coroutines, use 'pogoStickNested', or 'weave' with a 'NestWeaveStepper'.

{-# LANGUAGE ScopedTypeVariables, Rank2Types, MultiParamTypeClasses, TypeFamilies, TypeOperators,
             FlexibleContexts, FlexibleInstances, UndecidableInstances
 #-}

module Control.Monad.Coroutine.Nested
   (
      eitherFunctor, mapNestedSuspension, pogoStickNested,
      NestWeaveStepper,
      ChildFunctor(..), AncestorFunctor(..),
      liftParent, liftAncestor
   )
where

import Control.Monad (liftM)
import Data.Functor.Sum (Sum(InL, InR))

import Control.Monad.Coroutine

-- | Like 'either' for the 'Sum' data type.
eitherFunctor :: (l x -> y) -> (r x -> y) -> Sum l r x -> y
eitherFunctor left _ (InL f) = left f
eitherFunctor _ right (InR f) = right f

-- | Change the suspension functor of a nested 'Coroutine'.
mapNestedSuspension :: (Functor s0, Functor s, Monad m) => (forall y. s y -> s' y) ->
                       Coroutine (Sum s0 s) m x -> Coroutine (Sum s0 s') m x
mapNestedSuspension f cort = Coroutine {resume= liftM map' (resume cort)}
   where map' (Right r) = Right r
         map' (Left (InL s)) = Left (InL $ fmap (mapNestedSuspension f) s)
         map' (Left (InR s)) = Left (InR (f $ fmap (mapNestedSuspension f) s))

-- | Run a nested 'Coroutine' that can suspend both itself and the current 'Coroutine'.
pogoStickNested :: forall s1 s2 m x. (Functor s1, Functor s2, Monad m) => 
                   (s2 (Coroutine (Sum s1 s2) m x) -> Coroutine (Sum s1 s2) m x)
                   -> Coroutine (Sum s1 s2) m x -> Coroutine s1 m x
pogoStickNested reveal t = 
   Coroutine{resume= resume t
                      >>= \s-> case s
                               of Right result -> return (Right result)
                                  Left (InL s') -> return (Left (fmap (pogoStickNested reveal) s'))
                                  Left (InR c) -> resume (pogoStickNested reveal (reveal c))}

-- | Type of functions capable of combining two child coroutines' 'CoroutineStepResult' values into a parent coroutine.
-- Use with the function 'weave'.
type NestWeaveStepper s0 s1 s2 m x y z = WeaveStepper (Sum s0 s1) (Sum s0 s2) s0 m x y z

-- | Class of functors that can contain another functor.
class Functor c => ChildFunctor c where
   type Parent c :: * -> *
   wrap :: Parent c x -> c x
instance (Functor p, Functor s) => ChildFunctor (Sum p s) where
   type Parent (Sum p s) = p
   wrap = InL

-- | Class of functors that can be lifted.
class (Functor a, Functor d) => AncestorFunctor a d where
   -- | Convert the ancestor functor into its descendant. The descendant functor typically contains the ancestor.
   liftFunctor :: a x -> d x
instance {-# OVERLAPPING #-} Functor a => AncestorFunctor a a where
   liftFunctor = id
instance {-# OVERLAPPABLE #-} (Functor a, ChildFunctor d, d' ~ Parent d, AncestorFunctor a d') => AncestorFunctor a d where
   liftFunctor = wrap . (liftFunctor :: a x -> d' x)

-- | Converts a coroutine into a child nested coroutine.
liftParent :: forall m p c x. (Monad m, Functor p, ChildFunctor c, p ~ Parent c) => Coroutine p m x -> Coroutine c m x
liftParent = mapSuspension wrap
{-# INLINE liftParent #-}

-- | Converts a coroutine into a descendant nested coroutine.
liftAncestor :: forall m a d x. (Monad m, Functor a, AncestorFunctor a d) => Coroutine a m x -> Coroutine d m x
liftAncestor = mapSuspension liftFunctor
{-# INLINE liftAncestor #-}
