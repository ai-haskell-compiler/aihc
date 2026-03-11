module Data.List.PointedList.Circular
  ( -- Re-export many of the regular PointedList features
  module Data.List.PointedList

  -- And, of course, export the alternatives here
  , next
  , previous
  , delete
  , deleteLeft
  , deleteRight
  , moveN
  ) where

import Data.List.PointedList
  ( PointedList(..)
  , focus
  , singleton
  , fromList
  , fromListEnd
  , replace
  , insert
  , insertLeft
  , insertRight
  , deleteOthers
  , length
  , positions
  , contextMap
  , withFocus
  , find
  , index
  )
import qualified Data.List.PointedList as PL

-- | Move the focus to the next element in the list. If the last element is
--   currently focused, loop to the first element.
next :: PointedList a -> PointedList a
next pl@(PointedList [] b []) = pl
next    (PointedList a  b []) = let (x:xs) = reverse a in
                                  PointedList [] x (xs ++ [b])
next pl = PL.tryNext pl 

-- | Move the focus to the previous element in the list. If the first element is
--   currently focused, loop to the last element.
previous :: PointedList a -> PointedList a
previous pl@(PointedList [] b []) = pl
previous    (PointedList [] b c ) = let (x:xs) = reverse c in
                                      PointedList (xs ++ [b]) x []
previous pl = PL.tryPrevious pl

-- | An alias of 'deleteRight'.
delete :: PointedList a -> Maybe (PointedList a)
delete = deleteRight

-- | Possibly delete the element at the focus, then move the element on the
--   left to the focus. If no element is on the left, focus on the element to
--   the right. If the deletion will cause the list to be empty, return
--   @Nothing@.
deleteLeft :: PointedList a -> Maybe (PointedList a)
deleteLeft (PointedList []     _ []) = Nothing
deleteLeft (PointedList (l:ls) _ rs) = Just $ PointedList ls l rs
deleteLeft (PointedList []     _ rs) = let (x:xs) = reverse rs in
                                         Just $ PointedList xs x []

-- | Possibly delete the element at the focus, then move the element on the
--   right to the focus. If no element is on the right, focus on the element to
--   the left. If the deletion will cause the list to be empty, return
--   @Nothing@.
deleteRight :: PointedList a -> Maybe (PointedList a)
deleteRight (PointedList [] _ []    ) = Nothing
deleteRight (PointedList ls _ (r:rs)) = Just $ PointedList ls r rs
deleteRight (PointedList ls _ []    ) = let (x:xs) = reverse ls in
                                            Just $ PointedList [] x xs

-- | Move
moveN :: Int -> PointedList a -> PointedList a
moveN 0 pl = pl
moveN n pl | n > 0     = moveN (n-1) $ next pl
           | otherwise = moveN (n+1) $ previous pl
