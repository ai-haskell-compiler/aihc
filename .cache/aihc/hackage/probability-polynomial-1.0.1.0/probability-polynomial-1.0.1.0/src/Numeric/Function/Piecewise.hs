{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Copyright   : Predictable Network Solutions Ltd., 2020-2024
License     : BSD-3-Clause
Description : Piecewise functions on the number line. 
-}
module Numeric.Function.Piecewise
    ( -- * Type
      Piecewise

      -- * Basic operations
    , zero
    , fromInterval
    , fromAscPieces
    , toAscPieces
    , intervals

      -- * Structure
    , mapPieces
    , mergeBy
    , trim

      -- * Numerical
    , evaluate
    , translateWith

      -- * Zip
    , zipPointwise
    ) where

import Control.DeepSeq
    ( NFData
    )
import GHC.Generics
    ( Generic
    )

import qualified Data.Function.Class as Fun

{-----------------------------------------------------------------------------
    Type
------------------------------------------------------------------------------}
-- | Internal representation of a single piece,
-- starting at a basepoint of type @a@
-- and containing an object of type @o@.
data Piece a o = Piece
    { basepoint :: a
    , object :: o
    }
    deriving (Eq, Show, Generic, NFData)

{- | A function defined piecewise on numerical intervals.
 
* @o@ = type of function on every piece
    e.g. polynomials or other specialized representations of functions
* @'Fun.Domain' o@ = numerical type for the number line, e.g. 'Rational' or 'Double'

A value @f :: Piecewise o@ represents a function

> eval f x = { 0           if -∞ <  x < x1
>            { eval o1 x   if x1 <= x < x2
>            { eval o2 x   if x2 <= x < x3
>            { …
>            { eval on x   if xn <= x < +∞

where @x1, …, xn@ are points on the real number line
(in strictly increasing order)
and where @o1, …, on@ are specialized representations functions,
e.g. polynomials.

In other words, the value @f@ represents a function that
is defined piecewise on half-open intervals.

The function 'intervals' returns the half-open intervals in the middle:

> intervals f = [(x1,x2), (x2,x3), …, (xn-1, xn)]

No attempt is made to merge intervals if the piecewise objects are equal,
e.g. the situation @o1 == o2@ may occur.

-}
data Piecewise o
    = Pieces [Piece (Fun.Domain o) o]
    deriving (Generic)

deriving instance (Show (Fun.Domain o), Show o) => Show (Piecewise o)
deriving instance (NFData (Fun.Domain o), NFData o) => NFData (Piecewise o)

{-$Piecewise Invariants

* The empty list represents the zero function.
* The 'basepoint's are in strictly increasing order.
* The internal representation of the function mentioned in the definition is

    > f = Pieces [Piece x1 o1, Piece x2 o2, …, Piece xn on]
-}

{-----------------------------------------------------------------------------
    Operations
------------------------------------------------------------------------------}
-- | The function which is zero everywhere.
zero :: Piecewise o
zero = Pieces []

-- | @fromInterval (x1,x2) o@ creates a 'Piecewise' function
-- from a single function @o@ by restricting it to the
-- to half-open interval @y1 <= x < y2@
-- where @y1 = min x1 x2@ and @y2 = max x1 x2@.
--
-- The result is zero outside this interval.
-- As a special case, the result is 'zero' when @x1 == x2@.
fromInterval
    :: (Ord (Fun.Domain o), Num o)
    => (Fun.Domain o, Fun.Domain o) -> o -> Piecewise o
fromInterval (x,y) o
    | start < end = Pieces [Piece start o, Piece end 0]
    | otherwise = zero
  where
    start = min x y
    end = max x y

-- | Build a piecewise function from an ascending list of contiguous pieces.
--
-- /The precondition (`map fst` of input list is ascending) is not checked./
fromAscPieces :: Ord (Fun.Domain o) => [(Fun.Domain o, o)] -> Piecewise o
fromAscPieces = Pieces . map (uncurry Piece)

-- | Convert the piecewise function to a list of contiguous pieces
-- where the starting points of the pieces are in ascending order.
toAscPieces :: Ord (Fun.Domain o) => Piecewise o -> [(Fun.Domain o, o)]
toAscPieces (Pieces xos) = [ (x, o) | Piece x o <- xos ]

-- | Intervals on which the piecewise function is defined, in sequence.
-- The last half-open interval, @xn <= x < +∞@, is omitted.
intervals :: Piecewise o -> [(Fun.Domain o, Fun.Domain o)]
intervals (Pieces ys) =
    zip (map basepoint ys) (drop 1 $ map basepoint ys)

{-----------------------------------------------------------------------------
    Operations
    Structure
------------------------------------------------------------------------------}
-- | Map the objects of pieces.
mapPieces
    :: Fun.Domain o ~ Fun.Domain o'
    => (o -> o') -> Piecewise o -> Piecewise o'
mapPieces f (Pieces ps) = Pieces [ Piece x (f o) | Piece x o <- ps ]

-- | Merge all adjacent pieces whose functions are considered
-- equal by the given predicate.
mergeBy :: Num o => (o -> o -> Bool) -> Piecewise o -> Piecewise o
mergeBy eq (Pieces pieces) = Pieces $ go 0 pieces
  where
    go _ [] = []
    go before (p : ps)
        | before `eq` object p = go before ps
        | otherwise = p : go (object p) ps

-- | Merge all adjacent pieces whose functions are equal according to '(==)'.
trim :: (Eq o, Num o) => Piecewise o -> Piecewise o
trim = mergeBy (==)

{-----------------------------------------------------------------------------
    Operations
    Evaluation
------------------------------------------------------------------------------}
{-|
Evaluate a piecewise function at a point.

* @'Fun.Domain' ('Piecewise' o) = 'Fun.Domain' o@
* @'Fun.Codomain' ('Piecewise' o) = 'Fun.Codomain' o@
-}
instance (Fun.Function o, Num o, Ord (Fun.Domain o), Num (Fun.Codomain o))
    => Fun.Function (Piecewise o)
  where
    type instance Domain (Piecewise o) = Fun.Domain o
    type instance Codomain (Piecewise o) = Fun.Codomain o
    eval = evaluate

-- | Evaluate the piecewise function at a point.
-- See 'Piecewise' for the semantics.
evaluate
    :: (Fun.Function o, Num o, Ord (Fun.Domain o), Num (Fun.Codomain o))
    => Piecewise o -> Fun.Domain o -> Fun.Codomain o
evaluate (Pieces pieces) x = go 0 pieces
 where
    go before [] = Fun.eval before x
    go before (p:ps)
        | basepoint p <= x = go (object p) ps
        | otherwise = Fun.eval before x

-- | Translate a piecewise function,
-- given a way to translate each piece.
--
-- >  eval (translate' y o) = eval o (x - y)
-- >    implies
-- >    eval (translateWith translate' y p) = eval p (x - y)
translateWith
    :: (Ord (Fun.Domain o), Num (Fun.Domain o), Num o)
    => (Fun.Domain o -> o -> o)
    -> Fun.Domain o -> Piecewise o -> Piecewise o
translateWith trans y (Pieces pieces) =
    Pieces [ Piece (x + y) (trans y o) | Piece x o <- pieces ]

{-----------------------------------------------------------------------------
    Operations
    Zip
------------------------------------------------------------------------------}
-- | Combine two piecewise functions by combining the pieces
-- with a pointwise operation that preserves @0@.
--
-- For example, `(+)` and `(*)` are pointwise operations on functions,
-- but convolution is not a pointwise operation.
--
-- Preconditions on the argument @f@:
--
-- * @f 0 0 = 0@
-- * @f@ is a pointwise operations on functions,
--   e.g. commutes with pointwise evaluation.
--
-- /The preconditions are not checked!/
zipPointwise
    :: (Ord (Fun.Domain o), Num o)
    => (o -> o -> o)
        -- ^ @f@
    -> Piecewise o -> Piecewise o -> Piecewise o
zipPointwise f (Pieces xs') (Pieces ys') =
    Pieces $ go 0 xs' 0 ys'
  where
    -- We split the intervals and combine the pieces in a single pass.
    --
    -- The algorithm is similar to mergesort:
    -- We walk both lists in parallel and generate a new piece by
    -- * taking the basepoint of the nearest piece
    -- * and combining it with the object that was overhanging from
    --   the previous piece (`xhang`, `yhang`)
    go _ [] _ [] = []
    go _ (Piece x ox : xstail) yhang [] =
        Piece x (f ox yhang) : go ox xstail yhang []
    go xhang [] _ (Piece y oy : ystail) =
        Piece y (f xhang oy) : go xhang [] oy ystail
    go xhang xs@(Piece x ox : xstail) yhang ys@(Piece y oy : ystail) =
        case compare x y of
            LT -> Piece x (f ox    yhang) : go ox xstail yhang ys
            EQ -> Piece x (f ox    oy   ) : go ox xstail oy ystail
            GT -> Piece y (f xhang oy   ) : go xhang xs  oy ystail

{-----------------------------------------------------------------------------
    Operations
    Numeric
------------------------------------------------------------------------------}
{-| Algebraic operations '(+)', '(*)' and 'negate' on piecewise functions.

The functions 'abs' and 'signum' are defined using 'abs' and 'signum'
for every piece.

TODO: 'fromInteger' is __undefined__
-}
instance (Ord (Fun.Domain o), Num o) => Num (Piecewise o) where
    (+) = zipPointwise (+)
    (*) = zipPointwise (*)
    negate = mapPieces negate
    abs = mapPieces abs
    signum = mapPieces signum
    fromInteger 0 = zero
    fromInteger _ = error "TODO: fromInteger not implemented"
