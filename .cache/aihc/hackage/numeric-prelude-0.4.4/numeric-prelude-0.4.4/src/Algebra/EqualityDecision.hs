{- |
Combination of @(==)@ and @if then else@
that can be instantiated for more types than @Eq@
or can be instantiated in a way
that allows more defined results (\"more total\" functions):

* Reader like types for representing a context
  like 'Number.ResidueClass.Reader'

* Expressions in an EDSL

* More generally every type based on an applicative functor

* Tuples and Vector types

* Positional and Peano numbers,
  a common prefix of two numbers can be emitted
  before the comparison is done.
  (This could also be done with an overloaded 'if',
   what we also do not have.)
-}
module Algebra.EqualityDecision where

import qualified NumericPrelude.Elementwise as Elem
import Control.Applicative (Applicative(pure, (<*>)), )
import Data.Tuple.HT (fst3, snd3, thd3, )
import Data.List (zipWith4, )


{- |
For atomic types this could be a superclass of 'Eq'.
However for composed types like tuples, lists, functions
we do elementwise comparison
which is incompatible with the complete comparison performed by '(==)'.
-}
class C a where
   {- |
   It holds

   > (a ==? b) eq noteq  ==  if a==b then eq else noteq

   for atomic types where the right hand side can be defined.
   -}
   (==?) :: a -> a -> a -> a -> a



{-# INLINE deflt #-}
deflt :: Eq a => a -> a -> a -> a -> a
deflt a b eq noteq =
   if a==b then eq else noteq



instance C Int where
   {-# INLINE (==?) #-}
   (==?) = deflt

instance C Integer where
   {-# INLINE (==?) #-}
   (==?) = deflt

instance C Float where
   {-# INLINE (==?) #-}
   (==?) = deflt

instance C Double where
   {-# INLINE (==?) #-}
   (==?) = deflt

instance C Bool where
   {-# INLINE (==?) #-}
   (==?) = deflt

instance C Ordering where
   {-# INLINE (==?) #-}
   (==?) = deflt



{-# INLINE element #-}
element ::
   (C x) =>
   (v -> x) -> Elem.T (v,v,v,v) x
element f =
   Elem.element (\(x,y,eq,noteq) -> (f x ==? f y) (f eq) (f noteq))

{-# INLINE (<*>.==?) #-}
(<*>.==?) ::
   (C x) =>
   Elem.T (v,v,v,v) (x -> a) -> (v -> x) -> Elem.T (v,v,v,v) a
(<*>.==?) f acc =
   f <*> element acc


instance (C a, C b) => C (a,b) where
   {-# INLINE (==?) #-}
   (==?) = Elem.run4 $ pure (,) <*>.==?  fst <*>.==?  snd

instance (C a, C b, C c) => C (a,b,c) where
   {-# INLINE (==?) #-}
   (==?) = Elem.run4 $ pure (,,) <*>.==?  fst3 <*>.==?  snd3 <*>.==?  thd3

instance C a => C [a] where
   {-# INLINE (==?) #-}
   (==?) = zipWith4 (==?)

instance (C a) => C (b -> a) where
   {-# INLINE (==?) #-}
   (==?) x y eq noteq c  =  (x c ==? y c) (eq c) (noteq c)
