module Numeric.Interpolation.Sample (
   T,
   linear,
   hermite1,
   cubicLinear,
   cubicParabola,
   ) where

import qualified Numeric.Interpolation.NodeList as Nodes
import qualified Numeric.Interpolation.Piece as Piece
import Numeric.Interpolation.Private.List (mapAdjacentMaybe3, )
import Numeric.Interpolation.Private.Basis (
   parabolaBasisDerivativeRight,
   parabolaBasisDerivativeCenter,
   parabolaBasisDerivativeLeft,
   )


type T x y = [x] -> x -> [(Int, y)]

linear :: (Fractional a, Ord a) => T a a
linear nodeXs =
   let nodes = Nodes.fromList $ zip nodeXs [0..]
   in  \x ->
          case Nodes.lookup nodes x of
             (Just (l,nl), Just (r,nr)) ->
                [(nl, Piece.linear (l,1) (r,0) x),
                 (nr, Piece.linear (l,0) (r,1) x)]
             (Just (_l,nl), Nothing) -> [(nl, 1)]
             (Nothing, Just (_r,nr)) -> [(nr, 1)]
             (Nothing, Nothing) -> []

hermite1 :: (Fractional a, Ord a) => T a a
hermite1 nodeXs =
   let nodes = Nodes.fromList $ zip nodeXs [0..]
   in  \x ->
          case Nodes.lookup nodes x of
             (Just (l,nl), Just (r,nr)) ->
                [(2*nl+0, Piece.hermite1 (l,(1,0)) (r,(0,0)) x),
                 (2*nl+1, Piece.hermite1 (l,(0,1)) (r,(0,0)) x),
                 (2*nr+0, Piece.hermite1 (l,(0,0)) (r,(1,0)) x),
                 (2*nr+1, Piece.hermite1 (l,(0,0)) (r,(0,1)) x)]
             (Just (_l,nl), Nothing) -> [(2*nl, 1)]
             (Nothing, Just (_r,nr)) -> [(2*nr, 1)]
             (Nothing, Nothing) -> []

cubicLinear :: (Fractional a, Ord a) => T a a
cubicLinear nodeXs =
   let nodes =
          Nodes.fromList $ zip nodeXs $ zip [0..] $
          mapAdjacentMaybe3 (\l _ r -> (l,r)) nodeXs
   in  \x ->
          case Nodes.lookup nodes x of
             (Nothing, Nothing) -> []
             (Just (_l,(nl,_)), Nothing) -> [(nl-1, 1)]
             (Nothing, Just (_r,(nr,_))) -> [(nr+1, 1)]
             (Just (l,(nl,(mll,_))), Just (r,(nr,(_,mrr)))) ->
                let interL ll =
                       (nl-1, Piece.hermite1 (l,(0,recip(ll-r))) (r,(0,0)) x)
                    interR rr =
                       (nr+1, Piece.hermite1 (l,(0,0)) (r,(0,recip(rr-l))) x)
                in  case (mll,mrr) of
                       (Just ll, Just rr) ->
                          interL ll :
                          (nl, Piece.hermite1 (l,(1,0)) (r,(0,recip(l-rr))) x) :
                          (nr, Piece.hermite1 (l,(0,recip(r-ll))) (r,(1,0)) x) :
                          interR rr :
                          []
                       (Just ll, Nothing) -> interL ll : [(nl, 1)]
                       (Nothing, Just rr) -> interR rr : [(nr, 1)]
                       (Nothing, Nothing) -> []

cubicParabola :: (Fractional a, Ord a) => T a a
cubicParabola nodeXs =
   let nodes =
          Nodes.fromList $ zip nodeXs $ zip [0..] $
          mapAdjacentMaybe3 (\l _ r -> (l,r)) nodeXs
   in  \x ->
          case Nodes.lookup nodes x of
             (Nothing, Nothing) -> []
             (Just (_l,(nl,_)), Nothing) -> [(nl-1, 1)]
             (Nothing, Just (_r,(nr,_))) -> [(nr+1, 1)]
             (Just (l,(nl,(mll,_))), Just (r,(nr,(_,mrr)))) ->
                let interL ll =
                       (nl-1,
                        Piece.hermite1
                           (l,(0, parabolaBasisDerivativeLeft ll l r))
                           (r,(0, 0))
                           x)
                    interR rr =
                       (nr+1,
                        Piece.hermite1
                           (l,(0, 0))
                           (r,(0, parabolaBasisDerivativeRight l r rr))
                           x)
                in  case (mll,mrr) of
                       (Just ll, Just rr) ->
                          interL ll :
                          (nl,
                           Piece.hermite1
                              (l, (1, parabolaBasisDerivativeCenter ll l r))
                              (r, (0, parabolaBasisDerivativeLeft l r rr))
                              x) :
                          (nr,
                           Piece.hermite1
                              (l, (0, parabolaBasisDerivativeRight ll l r))
                              (r, (1, parabolaBasisDerivativeCenter l r rr))
                              x) :
                          interR rr :
                          []
                       (Just ll, Nothing) -> interL ll : [(nl, 1)]
                       (Nothing, Just rr) -> interR rr : [(nr, 1)]
                       (Nothing, Nothing) -> []
