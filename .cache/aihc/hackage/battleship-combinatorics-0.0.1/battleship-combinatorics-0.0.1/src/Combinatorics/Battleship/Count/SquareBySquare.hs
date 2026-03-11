{- |
This can be made working by managing the whole (broken) frontier.
It has a form like this one:

> xxxx
>    xxxxxxx

However, if we need to maintain all these information
this approach will be less efficient than the one with a straight frontier
as used in ShortenShip.hs.
-}
module Combinatorics.Battleship.Count.SquareBySquare where

import qualified Combinatorics.Battleship.Fleet as Fleet


data Orientation = Horizontal | Vertical
   deriving (Eq, Ord, Show)

type Square = Maybe (Orientation, Fleet.ShipSize)


{-
initLeftBottom and updateLeftBorder are special cases of updateInside
with Nothing arguments.
We could rename updateInside to simple 'update'
and handle all special cases with this universal function.
-}
initLeftBottom :: [(Square, Fleet.T)]
initLeftBottom =
   [(Nothing, Fleet.empty),
    (Just (Horizontal, 1), Fleet.empty), (Just (Vertical, 1), Fleet.empty)]

updateLeftBorder :: Fleet.T -> Square -> [(Square, Fleet.T)]
updateLeftBorder fleet below =
   case below of
      Nothing ->
         [(Nothing, fleet),
          (Just (Horizontal, 1), fleet), (Just (Vertical, 1), fleet)]
      Just (Vertical, k) ->
         [(Nothing, Fleet.inc k fleet), (Just (Vertical, succ k), fleet)]
      Just (Horizontal, _k) ->
         [(Nothing, fleet)]

updateInside :: Fleet.T -> Square -> Bool -> Square -> [(Square, Fleet.T)]
updateInside fleet left leftBelow below =
   case (left, leftBelow, below) of
      (Just _, _, Just _) -> []
      (Nothing, False, Nothing) ->
         [(Nothing, fleet),
          (Just (Horizontal, 1), fleet), (Just (Vertical, 1), fleet)]
      (Just (Vertical, _), _, Nothing) -> [(Nothing, fleet)]
      (_, True, _) -> [(Nothing, fleet)]
      (Nothing, _, Just (Horizontal, _k)) -> [(Nothing, fleet)]
      (Nothing, False, Just (Vertical, k)) ->
         [(Nothing, Fleet.inc k fleet),
          (Just (Vertical, succ k), fleet)]
      (Just (Horizontal, k), False, Nothing) ->
         [(Nothing, Fleet.inc k fleet),
          (Just (Horizontal, succ k), fleet)]

insertSquare :: Square -> Fleet.T -> Fleet.T
insertSquare = maybe id $ Fleet.inc . snd

check :: Fleet.T -> (Square, Fleet.T) -> Bool
check maxFleet =
   let cumMaxFleet = Fleet.cumulate maxFleet
   in  \(square, fleet) ->
         Fleet.subset fleet maxFleet
         &&
         Fleet.subset (Fleet.cumulate (insertSquare square fleet)) cumMaxFleet
