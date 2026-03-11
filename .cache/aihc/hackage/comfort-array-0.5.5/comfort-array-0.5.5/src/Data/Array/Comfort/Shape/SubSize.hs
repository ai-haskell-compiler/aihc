{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{- |
Framework for extracting subsize in 'Array.unsafeCreateWithSizes'.
-}
module Data.Array.Comfort.Shape.SubSize (
   T(Cons, measure),
   auto,
   atom,
   Sub(Sub),
   sub,
   pair,
   triple,
   append,

   C(ToShape),
   Atom(Atom),
   evaluate,
   ) where

import qualified Data.Array.Comfort.Shape as Shape
import Data.Array.Comfort.Shape ((::+)((::+)))


newtype T sh nsize = Cons {measure :: sh -> (Int,nsize)}

auto :: (C nsize) => T (ToShape nsize) nsize
auto = Cons evaluate

atom :: (Shape.C sh) => T sh Int
atom = Cons $ \sh -> let n = Shape.size sh in (n,n)

data Sub nsize = Sub Int nsize

sub :: T sh nsize -> T sh (Sub nsize)
sub (Cons s) =
   Cons $ \sh ->
      let (n,subSizes) = s sh
      in (n, Sub n subSizes)

pair ::
   T sh0 nsize0 ->
   T sh1 nsize1 ->
   T (sh0,sh1) (nsize0,nsize1)
pair (Cons s0) (Cons s1) =
   Cons $ \(sh0,sh1) ->
      let (n0,sub0) = s0 sh0
          (n1,sub1) = s1 sh1
      in (n0*n1, (sub0,sub1))

triple ::
   T sh0 nsize0 ->
   T sh1 nsize1 ->
   T sh2 nsize2 ->
   T (sh0,sh1,sh2) (nsize0,nsize1,nsize2)
triple (Cons s0) (Cons s1) (Cons s2) =
   Cons $ \(sh0,sh1,sh2) ->
      let (n0,sub0) = s0 sh0
          (n1,sub1) = s1 sh1
          (n2,sub2) = s2 sh2
      in (n0*n1*n2, (sub0,sub1,sub2))

append ::
   T sh0 nsize0 ->
   T sh1 nsize1 ->
   T (sh0::+sh1) (nsize0::+nsize1)
append (Cons s0) (Cons s1) =
   Cons $ \(sh0::+sh1) ->
      let (n0,sub0) = s0 sh0
          (n1,sub1) = s1 sh1
      in (n0+n1, sub0::+sub1)




class C nsize where
   type ToShape nsize
   {- |
   Compute the sizes of a shape and some sub-shapes.
   -}
   evaluate :: ToShape nsize -> (Int, nsize)

newtype Atom sh = Atom Int

instance (Shape.C sh) => C (Atom sh) where
   type ToShape (Atom sh) = sh
   evaluate sh = let n = Shape.size sh in (n, Atom n)

instance (C sub) => C (Sub sub) where
   type ToShape (Sub sub) = ToShape sub
   evaluate = measure $ sub auto

instance (C nsize0, C nsize1) => C (nsize0,nsize1) where
   type ToShape (nsize0,nsize1) =
            (ToShape nsize0, ToShape nsize1)
   evaluate = measure $ pair auto auto

instance (C nsize0, C nsize1, C nsize2) => C (nsize0,nsize1,nsize2) where
   type ToShape (nsize0,nsize1,nsize2) =
            (ToShape nsize0, ToShape nsize1, ToShape nsize2)
   evaluate = measure $ triple auto auto auto

instance (C nsize0, C nsize1) => C (nsize0::+nsize1) where
   type ToShape (nsize0::+nsize1) = (ToShape nsize0 ::+ ToShape nsize1)
   evaluate = measure $ append auto auto
