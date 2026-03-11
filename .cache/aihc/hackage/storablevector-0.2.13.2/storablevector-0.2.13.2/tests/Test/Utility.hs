{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.Utility where

import Test.QuickCheck (Property, (==>), )
import Text.Show.Functions ()

import Data.Word (Word8)
import Data.Int (Int64)

import qualified Data.ByteString      as P
import qualified Data.StorableVector  as V



{- |
Class allows to compare StorableVector implementation
with implementations for similar data structures,
here ByteString and [Word8].
-}
class Model a b where
   model :: a -> b

instance Model P [W]    where model = P.unpack
instance Model V [W]    where model = V.unpack
instance Model V P      where model = P.pack . V.unpack

instance Model Bool  Bool         where model = id
instance Model Int   Int          where model = id
instance Model Int64 Int64        where model = id
instance Model Word8 Word8        where model = id
instance Model Ordering Ordering  where model = id
instance Model Char Char          where model = id

instance (Model x y) => Model (a -> x) (a -> y) where
   model f = model . f

instance (Model x y) => Model (Maybe x) (Maybe y) where
   model = fmap model

instance (Model x y) => Model [x] [y] where
   model = fmap model

instance
   (Model x0 y0, Model x1 y1) =>
      Model (x0,x1) (y0,y1) where
   model (x0,x1) = (model x0, model x1)


type X = Int
type W = Word8
type P = P.ByteString
type V = V.Vector Word8


infixl 0 `applyId`, `applyModel`

applyId :: (a -> x, a -> y) -> a -> (x,y)
applyId (f,g) a = (f a, g a)

applyModel :: (Model x0 y0) => (x0 -> x, y0 -> y) -> x0 -> (x,y)
applyModel (f,g) x = (f x, g $ model x)


{-
These comparison functions handle wrapping and equality automatically.
-}
eq0 ::
   (Model x y, Eq y) =>
   x -> y -> Bool

eq1 ::
   (Model x1 y1, Model x y, Eq y) =>
   (x1 -> x) -> (y1 -> y) -> x1 -> Bool

eq2 ::
   (Model x2 y2, Model x1 y1, Model x y, Eq y) =>
   (x2 -> x1 -> x) -> (y2 -> y1 -> y) -> x2 -> x1 -> Bool

eq3 ::
   (Model x3 y3, Model x2 y2, Model x1 y1, Model x y, Eq y) =>
   (x3 -> x2 -> x1 -> x) -> (y3 -> y2 -> y1 -> y) -> x3 -> x2 -> x1 -> Bool

eq4 ::
   (Model x4 y4, Model x3 y3, Model x2 y2, Model x1 y1, Model x y, Eq y) =>
   (x4 -> x3 -> x2 -> x1 -> x) ->
   (y4 -> y3 -> y2 -> y1 -> y) ->
   x4 -> x3 -> x2 -> x1 -> Bool

eq5 ::
   (Model x5 y5, Model x4 y4, Model x3 y3, Model x2 y2, Model x1 y1, Model x y, Eq y) =>
   (x5 -> x4 -> x3 -> x2 -> x1 -> x) ->
   (y5 -> y4 -> y3 -> y2 -> y1 -> y) ->
   x5 -> x4 -> x3 -> x2 -> x1 -> Bool


infix 4 `eq1`, `eq2`, `eq3`, `eq4`, `eq5`

eq0 f g =
    model  f            == g
eq1 f g = \a         ->
    model (f a)         == g (model a)
eq2 f g = \a b       ->
    model (f a b)       == g (model a) (model b)
eq3 f g = \a b c     ->
    model (f a b c)     == g (model a) (model b) (model c)
eq4 f g = \a b c d   ->
    model (f a b c d)   == g (model a) (model b) (model c) (model d)
eq5 f g = \a b c d e ->
    model (f a b c d e) == g (model a) (model b) (model c) (model d) (model e)


{-
Handle functions that require non-empty input.
-}
eqnotnull1 ::
   (Model x y, Model x1 y1, IsNull x1, Eq y) =>
   (x1 -> x) -> (y1 -> y) -> x1 -> Property

eqnotnull2 ::
   (Model x y, Model x1 y1, Model x2 y2, IsNull x1, Eq y) =>
   (x2 -> x1 -> x) -> (y2 -> y1 -> y) -> x2 -> x1 -> Property

eqnotnull3 ::
   (Model x y, Model x1 y1, Model x2 y2, Model x3 y3, IsNull x1,
    Eq y) =>
   (x3 -> x2 -> x1 -> x) ->
   (y3 -> y2 -> y1 -> y) ->
   x3 -> x2 -> x1 -> Property

eqnotnull1 f g = \x     -> not (isNull x) ==> eq1 f g x
eqnotnull2 f g = \x y   -> not (isNull y) ==> eq2 f g x y
eqnotnull3 f g = \x y z -> not (isNull z) ==> eq3 f g x y z

class    IsNull t            where isNull :: t -> Bool
instance IsNull P.ByteString where isNull = P.null
instance IsNull V            where isNull = V.null
