{-# LANGUAGE TypeOperators #-}

-- | An implementation of a zipper-like non-empty list structure that tracks
--   an index position in the list (the 'focus').
module Data.List.PointedList where

import Prelude hiding (foldl, foldr, elem)

import Control.Applicative
import Control.Monad
import Data.Binary
import Data.Foldable hiding (find)
import Data.List hiding (length, foldl, foldr, find, elem)
import qualified Data.List as List
import Data.Traversable

-- | The implementation of the pointed list structure which tracks the current
--   position in the list structure.
data PointedList a = PointedList
  { _reversedPrefix :: [a]
  , _focus          :: a
  , _suffix         :: [a]
  } deriving (Eq)

instance Binary a => Binary (PointedList a) where
  put (PointedList x1 x2 x3) = do put x1; put x2; put x3
  get = do liftM3 PointedList get get get

-- | Lens compatible with Control.Lens.
reversedPrefix :: Functor f => ([a] -> f [a]) -> PointedList a -> f (PointedList a)
reversedPrefix f (PointedList ls x rs) = (\ls' -> PointedList ls' x rs) <$> f ls

-- | Lens compatible with Control.Lens.
focus :: Functor f => (a -> f a) -> PointedList a -> f (PointedList a)
focus f (PointedList ls x rs) = (\x' -> PointedList ls x' rs) <$> f x

-- | Lens compatible with Control.Lens.
suffix :: Functor f => ([a] -> f [a]) -> PointedList a -> f (PointedList a)
suffix f (PointedList ls x rs) = (\rs' -> PointedList ls x rs') <$> f rs

-- | Lens compatible with Control.Lens.
-- Internally reversing the prefix list.
prefix :: Functor f => ([a] -> f [a]) -> PointedList a -> f (PointedList a)
prefix f (PointedList ls x rs) = (\ls' -> PointedList (reverse ls') x rs) <$> f (reverse ls)

instance (Show a) => Show (PointedList a) where
 show (PointedList ls x rs) = show (reverse ls) ++ " " ++ show x ++ " " ++ show rs

instance Functor PointedList where
 fmap f (PointedList ls x rs) = PointedList (map f ls) (f x) (map f rs)

instance Foldable PointedList where
 foldr f z (PointedList ls x rs) = foldl (flip f) (foldr f z (x:rs)) ls

instance Traversable PointedList where
 traverse f (PointedList ls x rs) = PointedList <$>
    (reverse <$> traverse f (reverse ls)) <*> f x <*> traverse f rs

-- | Create a 'PointedList' with a single element.
singleton :: a -> PointedList a
singleton x = PointedList [] x []

-- | Possibly create a @'Just' 'PointedList'@ if the provided list has at least
--   one element; otherwise, return Nothing.
--
--   The provided list's head will be the focus of the list, and the rest of
--   list will follow on the right side.
fromList :: [a] -> Maybe (PointedList a)
fromList []     = Nothing
fromList (x:xs) = Just $ PointedList [] x xs

-- | Possibly create a @'Just' 'PointedList'@ if the provided list has at least
--   one element; otherwise, return Nothing.
--
--   The provided list's last element will be the focus of the list, following
--   the rest of the list in order, to the left.
fromListEnd :: [a] -> Maybe (PointedList a)
fromListEnd [] = Nothing
fromListEnd xs = Just $ PointedList xs' x []
 where (x:xs') = reverse xs

-- | Replace the focus of the list, retaining the prefix and suffix.
replace :: a -> PointedList a -> PointedList a
replace x (PointedList ls _ rs) = PointedList ls x rs
-- replace = set focus

-- | Possibly move the focus to the next element in the list.
next :: PointedList a -> Maybe (PointedList a)
next (PointedList _  _ []) = Nothing
next p = (Just . tryNext) p -- GHC doesn't allow PL form here

-- | Attempt to move the focus to the next element, or 'error' if there are
--   no more elements.
tryNext :: PointedList a -> PointedList a
tryNext p@(PointedList _  _ []    ) = error "cannot move to next element"
tryNext   (PointedList ls x (r:rs)) = PointedList (x:ls) r rs

-- | Possibly move the focus to the previous element in the list.
previous :: PointedList a -> Maybe (PointedList a)
previous (PointedList [] _ _ ) = Nothing
previous p = (Just . tryPrevious) p

-- | Attempt to move the focus to the previous element, or 'error' if there are
--   no more elements.
tryPrevious :: PointedList a -> PointedList a
tryPrevious p@(PointedList []     _ _ ) =
  error "cannot move to previous element" 
tryPrevious   (PointedList (l:ls) x rs) = PointedList ls l (x:rs)

-- | An alias for 'insertRight'.
insert :: a -> PointedList a -> PointedList a
insert = insertRight

-- | Insert an element to the left of the focus, then move the focus to the new
--   element.
insertLeft :: a -> PointedList a -> PointedList a
insertLeft y (PointedList ls x rs) = PointedList ls y (x:rs)

-- | Insert an element to the right of the focus, then move the focus to the
--   new element.
insertRight :: a -> PointedList a -> PointedList a
insertRight y (PointedList ls x rs) = PointedList (x:ls) y rs

-- | An alias of 'deleteRight'.
delete :: PointedList a -> Maybe (PointedList a)
delete = deleteRight

-- | Possibly delete the element at the focus, then move the element on the
--   left to the focus. If no element is on the left, focus on the element to
--   the right. If the deletion will cause the list to be empty, return
--   'Nothing'.
deleteLeft :: PointedList a -> Maybe (PointedList a)
deleteLeft (PointedList [] _ []    ) = Nothing
deleteLeft (PointedList (l:ls) _ rs) = Just $ PointedList ls l rs
deleteLeft (PointedList [] _ (r:rs)) = Just $ PointedList [] r rs

-- | Possibly delete the element at the focus, then move the element on the
--   right to the focus. If no element is on the right, focus on the element to
--   the left. If the deletion will cause the list to be empty, return
--   'Nothing'.
deleteRight :: PointedList a -> Maybe (PointedList a)
deleteRight (PointedList [] _ []    ) = Nothing
deleteRight (PointedList ls _ (r:rs)) = Just $ PointedList ls r rs
deleteRight (PointedList (l:ls) _ []) = Just $ PointedList ls l []

-- | Delete all elements in the list except the focus.
deleteOthers :: PointedList a -> PointedList a
deleteOthers (PointedList _ b _) = PointedList [] b []

-- | The length of the list.
length :: PointedList a -> Int
length = foldr (const (+1)) 0

-- | Whether the focus is the first element.
atStart :: PointedList a -> Bool
atStart (PointedList [] _ _) = True
atStart _ = False

-- | Whether the focus is the last element.
atEnd :: PointedList a -> Bool
atEnd (PointedList _ _ []) = True
atEnd _ = False

-- | Create a 'PointedList' of variations of the provided 'PointedList', in
--   which each element is focused, with the provided 'PointedList' as the
--   focus of the sets.
positions :: PointedList a -> PointedList (PointedList a)
positions p@(PointedList ls x rs) = PointedList left p right
  where left  = unfoldr (\p -> fmap (join (,)) $ previous p) p
        right = unfoldr (\p -> fmap (join (,)) $ next p) p

-- | Map over the 'PointedList's created via 'positions', such that @f@ is	
--   called with each element of the list focused in the provided
--   'PointedList'. An example makes this easier to understand:
--
-- > contextMap atStart (fromJust $ fromList [1..5])
contextMap :: (PointedList a -> b) -> PointedList a -> PointedList b
contextMap f z = fmap f $ positions z

-- | Create a @'PointedList' a@ of @(a, 'Bool')@, in which the boolean values
--   specify whether the current element has the focus. That is, all of the
--   booleans will be 'False', except the focused element.
withFocus :: PointedList a -> PointedList (a, Bool)
withFocus (PointedList a b c) =
    PointedList (zip a (repeat False)) (b, True) (zip c (repeat False))

-- | Move the focus to the specified index. The first element is at index 0.
moveTo :: Int -> PointedList a -> Maybe (PointedList a)
moveTo n pl = moveN (n - (index pl)) pl 

-- | Move the focus by @n@, relative to the current index. Negative values move
--   the focus backwards, positive values more forwards through the list.
moveN :: Int -> PointedList a -> Maybe (PointedList a)
moveN n pl@(PointedList left x right) = go n left x right 
  where
  go n left x right = case compare n 0 of
   GT -> case right of
     [] -> Nothing
     (r:rs) -> go (n-1) (x:left) r rs
   LT -> case left of
     [] -> Nothing
     (l:ls) -> go (n+1) ls l (x:right)
   EQ -> Just $ PointedList left x right

-- | Move the focus to the specified element, if it is present.
--
--   Patch with much faster algorithm provided by Runar Bjarnason for version
--   0.3.2. Improved again by Runar Bjarnason for version 0.3.3 to support
--   infinite lists on both sides of the focus.
find :: Eq a => a -> PointedList a -> Maybe (PointedList a)
find x pl = find' ((x ==) . _focus) $ positions pl
  where find' pred (PointedList a b c) =
          if pred b then Just b
                    else List.find pred (merge a c)
        merge []     ys = ys
        merge (x:xs) ys = x : merge ys xs

-- | The index of the focus, leftmost is 0.
index :: PointedList a -> Int
index (PointedList a _ _) = Prelude.length a
