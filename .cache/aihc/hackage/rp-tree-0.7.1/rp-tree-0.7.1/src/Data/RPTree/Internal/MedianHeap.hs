{-# language CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Data.RPTree.Internal.MedianHeap (
  MedianHeap,
  insert, fromList,
  median) where

import Data.Bifunctor (Bifunctor(..))
import Data.Ord (Down(..))

import qualified Data.Heap as H (Heap, Entry(..), viewMin, insert, singleton)

type MinHeap p a = H.Heap (H.Entry p a)
type MaxHeap p a = H.Heap (H.Entry (Down p) a)
-- | The items @a@ in a 'MedianHeap' have type @p@
data MedianHeap p a = MedianHeap (MaxHeap p a) (MinHeap p a) deriving (Eq, Show)
instance Semigroup (MedianHeap p a) where
  MedianHeap l1 r1 <> MedianHeap l2 r2 = MedianHeap (l1 <> l2) (r1 <> r2)
instance Monoid (MedianHeap p a) where
  mempty = MedianHeap mempty mempty
insertMax :: Ord p => p -> a -> MaxHeap p a -> MaxHeap p a
insertMax p x = H.insert (H.Entry (Down p) x)
insertMin :: Ord p => p -> a -> MinHeap p a -> MinHeap p a
insertMin p x = H.insert (H.Entry p x)

fromList :: (Foldable t, Fractional p, Ord p) => t (p, a) -> MedianHeap p a
fromList = foldr (uncurry insert) mempty

-- | Insert a weighted entry in the heap
insert :: (Fractional p, Ord p) =>
          p -- ^ weight
       -> a
       -> MedianHeap p a
       -> MedianHeap p a
insert p x heap@(MedianHeap ll rr) =
  case median heap of
    Nothing -> MedianHeap (insertMax p x mempty) rr
    Just q ->
      if q > p
      then balance $ MedianHeap ll (insertMin p x rr)
      else balance $ MedianHeap (insertMax p x ll) rr

balance :: Ord p => MedianHeap p a -> MedianHeap p a
balance (MedianHeap ll rr)
  | length ll == length rr = MedianHeap ll rr
  | length ll > length rr =
    let (H.Entry (Down p) x, xs) = deconstruct ll
     in MedianHeap xs (insertMin p x rr)
  | otherwise =
    let (H.Entry p x, xs) = deconstruct rr
     in MedianHeap (insertMax p x ll) xs
  where
    deconstruct heap = case H.viewMin heap of
      Nothing -> error "cannot view empty heap"
      Just (x, xs) -> (x, xs)

-- | Compute the median weight
median :: Fractional p => MedianHeap p a -> Maybe p
median (MedianHeap lesser greater)
  | length lesser > length greater = H.priority . getHD <$> viewHead lesser
  | length lesser < length greater = H.priority <$> viewHead greater
  | otherwise = do
      leftHead <- getHD <$> viewHead lesser
      rightHead <- viewHead greater
      return $ (H.priority leftHead + H.priority rightHead) / 2

getHD :: H.Entry (Down b) c -> H.Entry b c
getHD = first getDown

#if MIN_VERSION_base(4,14,0)
#else
getDown :: Down a -> a
getDown (Down x) = x
#endif

viewHead :: H.Heap b -> Maybe b
viewHead h = fst <$> H.viewMin h
