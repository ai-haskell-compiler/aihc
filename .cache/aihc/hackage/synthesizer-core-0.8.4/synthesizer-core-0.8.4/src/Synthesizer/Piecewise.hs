{-# LANGUAGE NoImplicitPrelude #-}
{- |
Construction of a data type that describes piecewise defined curves.
-}
module Synthesizer.Piecewise where

import Data.Ix (Ix, )

import qualified Algebra.RealRing as RealRing

import NumericPrelude.Numeric
import NumericPrelude.Base


{-
ToDo:
Make it a new data type with Monoid and Generic.Cut instances.
However there is no fast and generic way for splitting a piece.
-}
type T t y sig = [PieceData t y sig]

{- |
The curve type of a piece of a piecewise defined control curve.
-}
newtype Piece t y sig =
   Piece {computePiece :: y  {- y0 -}
                       -> y  {- y1 -}
                       -> t  {- duration -}
                       -> sig}

pieceFromFunction :: (y -> y -> t -> sig) -> Piece t y sig
pieceFromFunction = Piece


{- |
The full description of a control curve piece.
-}
data PieceData t y sig =
     PieceData {pieceType :: Piece t y sig,
                pieceY0 :: y,
                pieceY1 :: y,
                pieceDur :: t}
--   deriving (Eq, Show)


newtype PieceRightSingle y = PRS y
newtype PieceRightDouble y = PRD y

data PieceDist t y sig = PD t (Piece t y sig) y


-- precedence and associativity like (:)
infixr 5 -|#, #|-, =|#, #|=, |#, #|

{- |
The 6 operators simplify constructing a list of @PieceData a@.
The description consists of nodes (namely the curve values at nodes)
and the connecting curve types.
The naming scheme is as follows:
In the middle there is a bar @|@.
With respect to the bar,
the pad symbol @\#@ is at the side of the curve type,
at the other side there is nothing, a minus sign @-@, or an equality sign @=@.

 (1) Nothing means that here is the start or the end node of a curve.

 (2) Minus means that here is a node where left and right curve meet at the same value.
     The node description is thus one value.

 (3) Equality sign means that here is a split node,
     where left and right curve might have different ending and beginning values, respectively.
     The node description consists of a pair of values.
-}

-- the leading space is necessary for the Haddock parser

( #|-) :: (t, Piece t y sig) -> (PieceRightSingle y, T t y sig) ->
   (PieceDist t y sig, T t y sig)
(d,c) #|- (PRS y1, xs)  =  (PD d c y1, xs)

(-|#) :: y -> (PieceDist t y sig, T t y sig) ->
   (PieceRightSingle y, T t y sig)
y0 -|# (PD d c y1, xs)  =  (PRS y0, PieceData c y0 y1 d : xs)

( #|=) :: (t, Piece t y sig) -> (PieceRightDouble y, T t y sig) ->
   (PieceDist t y sig, T t y sig)
(d,c) #|= (PRD y1, xs)  =  (PD d c y1, xs)

(=|#) :: (y,y) -> (PieceDist t y sig, T t y sig) ->
   (PieceRightDouble y, T t y sig)
(y01,y10) =|# (PD d c y11, xs)  =  (PRD y01, PieceData c y10 y11 d : xs)

( #|) :: (t, Piece t y sig) -> y ->
   (PieceDist t y sig, T t y sig)
(d,c) #| y1  =  (PD d c y1, [])

(|#) :: y -> (PieceDist t y sig, T t y sig) ->
   T t y sig
y0 |# (PD d c y1, xs)  =  PieceData c y0 y1 d : xs


data FlatPosition = FlatLeft | FlatRight
   deriving (Show, Eq, Ord, Ix, Enum)

splitDurations :: (RealRing.C t) => [t] -> [(Int, t)]
splitDurations ts0 =
   let (ds,ts) =
           unzip $ scanl
              (\(_,fr) d -> RealRing.splitFraction (fr+d))
              (0,1) ts0
   in  zip (tail ds) (map (subtract 1) ts)
