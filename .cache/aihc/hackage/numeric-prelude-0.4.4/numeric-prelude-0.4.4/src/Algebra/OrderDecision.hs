{- |
Combination of @compare@ and @if then else@
that can be instantiated for more types than @Ord@
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
module Algebra.OrderDecision where

import qualified Algebra.EqualityDecision as Equality
import Algebra.EqualityDecision ((==?), )

import qualified NumericPrelude.Elementwise as Elem
import Control.Applicative (Applicative(pure, (<*>)), )
import Data.Tuple.HT (fst3, snd3, thd3, )
import Data.List (zipWith4, zipWith5, )

import Prelude hiding (compare, min, max, minimum, maximum, )
import qualified Prelude as P



{- |
For atomic types this could be a superclass of 'Ord'.
However for composed types like tuples, lists, functions
we do elementwise comparison
which is incompatible with the complete comparison performed by 'P.compare'.
-}
class Equality.C a => C a where
   {- |
   It holds

   > (compare a b) lt eq gt  ==
   >    case Prelude.compare a b of
   >       LT -> lt
   >       EQ -> eq
   >       GT -> gt

   for atomic types where the right hand side can be defined.

   Minimal complete definition:
   'compare' or '(<=?)'.
   -}
   compare :: a -> a -> a -> a -> a -> a
   compare x y lt eq gt =
      (x ==? y) eq ((x <=? y) lt gt)

   {-# INLINE (<=?) #-}
   (<=?) :: a -> a -> a -> a -> a
   (<=?) x y le gt =
      compare x y le le gt

   {-# INLINE (>=?) #-}
   (>=?) :: a -> a -> a -> a -> a
   (>=?) = flip (<=?)

   (<?) :: a -> a -> a -> a -> a
   (<?) x y = flip (x >=? y)

   {-# INLINE (>?) #-}
   (>?) :: a -> a -> a -> a -> a
   (>?) = flip (<?)

{-
   (<?) :: a -> a -> a -> a -> a
   (<?) x y lt ge =
      compare x y lt ge ge

   (>?) :: a -> a -> a -> a -> a
   (>?) x y gt le =
      compare x y le le gt

   (<=?) :: a -> a -> a -> a -> a
   (<=?) x y le gt =
      compare x y le le gt

   (>=?) :: a -> a -> a -> a -> a
   (>=?) x y ge lt =
      compare x y lt ge ge
-}


max :: C a => a -> a -> a
max x y = (x>?y) x y

min :: C a => a -> a -> a
min x y = (x<?y) x y

maximum :: C a => a -> [a] -> a
maximum x xs = foldl max x xs

minimum :: C a => a -> [a] -> a
minimum x xs = foldl min x xs



{-# INLINE compareOrd #-}
compareOrd :: Ord a => a -> a -> a -> a -> a -> a
compareOrd a b lt eq gt =
   case P.compare a b of
      LT -> lt
      EQ -> eq
      GT -> gt

instance C Int where
   {-# INLINE compare #-}
   compare = compareOrd

instance C Integer where
   {-# INLINE compare #-}
   compare = compareOrd

instance C Float where
   {-# INLINE compare #-}
   compare = compareOrd

instance C Double where
   {-# INLINE compare #-}
   compare = compareOrd

instance C Bool where
   {-# INLINE compare #-}
   compare = compareOrd

instance C Ordering where
   {-# INLINE compare #-}
   compare = compareOrd



{-# INLINE elementCompare #-}
elementCompare ::
   (C x) =>
   (v -> x) -> Elem.T (v,v,v,v,v) x
elementCompare f =
   Elem.element (\(x,y,lt,eq,gt) ->
      compare (f x) (f y) (f lt) (f eq) (f gt))

{-# INLINE (<*>.<=>?) #-}
(<*>.<=>?) ::
   (C x) =>
   Elem.T (v,v,v,v,v) (x -> a) -> (v -> x) -> Elem.T (v,v,v,v,v) a
(<*>.<=>?) f acc =
   f <*> elementCompare acc


{-# INLINE element #-}
element ::
   (C x) =>
   (x -> x -> x -> x -> x) ->
   (v -> x) -> Elem.T (v,v,v,v) x
element cmp f =
   Elem.element (\(x,y,true,false) -> cmp (f x) (f y) (f true) (f false))

{-# INLINE (<*>.<=?) #-}
(<*>.<=?) ::
   (C x) =>
   Elem.T (v,v,v,v) (x -> a) -> (v -> x) -> Elem.T (v,v,v,v) a
(<*>.<=?) f acc =
   f <*> element (<=?) acc

{-# INLINE (<*>.>=?) #-}
(<*>.>=?) ::
   (C x) =>
   Elem.T (v,v,v,v) (x -> a) -> (v -> x) -> Elem.T (v,v,v,v) a
(<*>.>=?) f acc =
   f <*> element (>=?) acc

{-# INLINE (<*>.<?) #-}
(<*>.<?) ::
   (C x) =>
   Elem.T (v,v,v,v) (x -> a) -> (v -> x) -> Elem.T (v,v,v,v) a
(<*>.<?) f acc =
   f <*> element (<?) acc

{-# INLINE (<*>.>?) #-}
(<*>.>?) ::
   (C x) =>
   Elem.T (v,v,v,v) (x -> a) -> (v -> x) -> Elem.T (v,v,v,v) a
(<*>.>?) f acc =
   f <*> element (>?) acc


instance (C a, C b) => C (a,b) where
   {-# INLINE compare #-}
   compare = Elem.run5 $ pure (,) <*>.<=>? fst <*>.<=>? snd
   {-# INLINE (<=?) #-}
   (<=?)   = Elem.run4 $ pure (,) <*>.<=?  fst <*>.<=?  snd
   {-# INLINE (>=?) #-}
   (>=?)   = Elem.run4 $ pure (,) <*>.>=?  fst <*>.>=?  snd
   {-# INLINE (<?) #-}
   (<?)    = Elem.run4 $ pure (,) <*>.<?   fst <*>.<?   snd
   {-# INLINE (>?) #-}
   (>?)    = Elem.run4 $ pure (,) <*>.>?   fst <*>.>?   snd

instance (C a, C b, C c) => C (a,b,c) where
   {-# INLINE compare #-}
   compare = Elem.run5 $ pure (,,) <*>.<=>? fst3 <*>.<=>? snd3 <*>.<=>? thd3
   {-# INLINE (<=?) #-}
   (<=?)   = Elem.run4 $ pure (,,) <*>.<=?  fst3 <*>.<=?  snd3 <*>.<=?  thd3
   {-# INLINE (>=?) #-}
   (>=?)   = Elem.run4 $ pure (,,) <*>.>=?  fst3 <*>.>=?  snd3 <*>.>=?  thd3
   {-# INLINE (<?) #-}
   (<?)    = Elem.run4 $ pure (,,) <*>.<?   fst3 <*>.<?   snd3 <*>.<?   thd3
   {-# INLINE (>?) #-}
   (>?)    = Elem.run4 $ pure (,,) <*>.>?   fst3 <*>.>?   snd3 <*>.>?   thd3

instance C a => C [a] where
   {-# INLINE compare #-}
   compare = zipWith5 compare
   {-# INLINE (<=?) #-}
   (<=?) = zipWith4 (<=?)
   {-# INLINE (>=?) #-}
   (>=?) = zipWith4 (>=?)
   {-# INLINE (<?) #-}
   (<?)  = zipWith4 (<?)
   {-# INLINE (>?) #-}
   (>?)  = zipWith4 (>?)

instance (C a) => C (b -> a) where
   {-# INLINE compare #-}
   compare x y lt eq gt c  =  compare (x c) (y c) (lt c) (eq c) (gt c)
   {-# INLINE (<=?) #-}
   (<=?) x y true false c  =  (x c <=? y c) (true c) (false c)
   {-# INLINE (>=?) #-}
   (>=?) x y true false c  =  (x c >=? y c) (true c) (false c)
   {-# INLINE (<?) #-}
   (<?)  x y true false c  =  (x c <?  y c) (true c) (false c)
   {-# INLINE (>?) #-}
   (>?)  x y true false c  =  (x c >?  y c) (true c) (false c)
