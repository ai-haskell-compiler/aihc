{-# LANGUAGE TypeFamilies #-}
module Data.Array.Comfort.Shape.Tuple where

import qualified Data.Array.Comfort.Shape as Shape
import Data.Complex (Complex((:+)))

import qualified Control.Monad.Trans.State as MS
import qualified Control.Applicative.HT as App
import Control.Applicative ((<$>))


get :: MS.State [a] a
get =
   MS.state $ \at ->
      case at of
         a:as -> (a,as)
         [] -> error "Shape.Tuple.get: no element left"

cons ::
   (Shape.ElementTuple shape) =>
   shape -> MS.State [a] (Shape.DataTuple shape a)
cons = Shape.indexTupleA (const get)


next :: MS.State Shape.Element Shape.Element
next = do
   ix <- MS.get
   MS.modify (\(Shape.Element k) -> Shape.Element (k+1))
   return ix


class (Shape.ElementTuple shape) => NestedTuple shape where
   decons :: Shape.DataTuple shape a -> MS.State Shape.Element (shape, [a])

instance NestedTuple () where
   decons () = return ((),[])

instance NestedTuple Shape.Element where
   decons a = flip (,) [a] <$> next

instance (NestedTuple a, NestedTuple b) => NestedTuple (a,b) where
   decons (a,b) =
      App.lift2 (\(ta,as) (tb,bs) -> ((ta,tb), as++bs)) (decons a) (decons b)

instance
   (NestedTuple a, NestedTuple b, NestedTuple c) =>
      NestedTuple (a,b,c) where
   decons (a,b,c) =
      App.lift3
         (\(ta,as) (tb,bs) (tc,cs) -> ((ta,tb,tc), as++bs++cs))
         (decons a) (decons b) (decons c)

instance
   (NestedTuple a, NestedTuple b, NestedTuple c, NestedTuple d) =>
      NestedTuple (a,b,c,d) where
   decons (a,b,c,d) =
      App.lift4
         (\(ta,as) (tb,bs) (tc,cs) (td,ds) -> ((ta,tb,tc,td), as++bs++cs++ds))
         (decons a) (decons b) (decons c) (decons d)

instance (NestedTuple a) => NestedTuple (Complex a) where
   decons (a:+b) =
      App.lift2 (\(ta,as) (tb,bs) -> ((ta:+tb), as++bs)) (decons a) (decons b)
