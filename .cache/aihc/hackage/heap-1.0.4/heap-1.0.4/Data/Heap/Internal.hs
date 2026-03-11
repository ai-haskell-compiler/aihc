{-# LANGUAGE DeriveDataTypeable #-}

-- | This module provides a simple leftist-heap implementation based on Chris
-- Okasaki's book \"Purely Functional Data Structures\", Cambridge University
-- Press, 1998, chapter 3.1.
--
-- A @'HeapT' prio val@ associates a priority @prio@ to a value @val@. A
-- priority-value pair with minimum priority will always be the head of the
-- 'HeapT', so this module implements minimum priority heaps. Note that the value
-- associated to the priority has no influence on the ordering of elements, only
-- the priority does.
module Data.Heap.Internal
    ( -- * A basic heap type
      HeapT(..)
      -- * Query
    , isEmpty, rank, size
      -- * Construction
    , empty, singleton, union, unions
      -- * Deconstruction
    , view
      -- * Filter
    , partition
      -- * Subranges
    , splitAt, span
      -- * Conversion
    , fromList, toList
    , fromDescList, toAscList
    ) where

import Control.Exception
import Data.Foldable ( Foldable(foldl, foldr, foldMap), foldl' )
import Data.List ( groupBy, sortBy )
import Data.Semigroup
import Data.Monoid
import Data.Ord
import Data.Typeable
import Prelude hiding ( foldl, foldr, span, splitAt, foldMap )
import Text.Read

-- | The basic heap type. It stores priority-value pairs @(prio, val)@ and
-- always keeps the pair with minimal priority on top. The value associated to
-- the priority does not have any influence on the ordering of elements.
data HeapT prio val
    = Empty  -- ^ An empty 'HeapT'.
    | Tree { _rank     :: {-# UNPACK #-} !Int -- ^ Rank of the leftist heap.
           , _size     :: {-# UNPACK #-} !Int -- ^ Number of elements in the heap.
           , _priority :: !prio               -- ^ Priority of the entry.
           , _value    :: val                 -- ^ Value of the entry.
           , _left     :: !(HeapT prio val)   -- ^ Left subtree.
           , _right    :: !(HeapT prio val)   -- ^ Right subtree.
           } -- ^ A tree node of a non-empty 'HeapT'.
    deriving (Typeable)

instance (Read prio, Read val, Ord prio) => Read (HeapT prio val) where
    readPrec     = parens $ prec 10 $ do
        Ident "fromList" <- lexP
        fmap fromList readPrec
    readListPrec = readListPrecDefault

instance (Show prio, Show val) => Show (HeapT prio val) where
    showsPrec d heap = showParen (d > 10)
        $ showString "fromList " . (showsPrec 11 (toList heap))

instance (Ord prio, Ord val) => Eq (HeapT prio val) where
    heap1 == heap2 = size heap1 == size heap2 && EQ == compare heap1 heap2

instance (Ord prio, Ord val) => Ord (HeapT prio val) where
    compare = comparing toPairAscList

instance (Ord prio) => Semigroup (HeapT prio val) where
    (<>) = union

instance (Ord prio) => Monoid (HeapT prio val) where
    mempty  = empty
    mappend = union
    mconcat = unions

instance Functor (HeapT prio) where
    fmap _ Empty = Empty
    fmap f heap  = heap { _value = f (_value heap)
                        , _left  = fmap f (_left heap)
                        , _right = fmap f (_right heap)
                        }

instance (Ord prio) => Foldable (HeapT prio) where
    foldMap f = foldMap f . fmap snd . toAscList
    foldr f z = foldl (flip f) z . fmap snd . reverse . toAscList
    foldl f z = foldl f z . fmap snd . toAscList

-- | /O(1)/. Is the 'HeapT' empty?
isEmpty :: HeapT prio val -> Bool
isEmpty Empty = True
isEmpty _     = False

-- | /O(1)/. Find the rank of a 'HeapT' (the length of its right spine).
rank :: HeapT prio val -> Int
rank Empty = 0
rank heap  = _rank heap

-- | /O(1)/. The total number of elements in the 'HeapT'.
size :: HeapT prio val -> Int
size Empty = 0
size heap  = _size heap

-- | /O(1)/. Construct an empty 'HeapT'.
empty :: HeapT prio val
empty = Empty

-- | /O(1)/. Create a singleton 'HeapT'.
singleton :: prio -> val -> HeapT prio val
singleton p v = Tree { _rank     = 1
                     , _size     = 1
                     , _priority = p
                     , _value    = v
                     , _left     = empty
                     , _right    = empty
                     }
{-# INLINE singleton #-}

-- | /O(1)/. Insert an priority-value pair into the 'HeapT', whose /priority is
-- less or equal/ to all other priorities on the 'HeapT', i. e. a pair that is a
-- valid head of the 'HeapT'.
--
-- /The precondition is not checked/.
uncheckedCons :: (Ord prio) => prio -> val -> HeapT prio val -> HeapT prio val
uncheckedCons p v heap = assert (maybe True (\(p', _, _) -> p <= p') (view heap))
    Tree { _rank     = 1
         , _size     = 1 + size heap
         , _priority = p
         , _value    = v
         , _left     = heap
         , _right    = empty
         }
{-# INLINE uncheckedCons #-}

-- | /O(log max(n, m))/. Form the union of two 'HeapT's.
union :: (Ord prio) => HeapT prio val -> HeapT prio val -> HeapT prio val
union heap  Empty = heap
union Empty heap  = heap
union heap1 heap2 = let
    p1 = _priority heap1
    p2 = _priority heap2
    in if p1 < p2
        then makeT p1 (_value heap1) (_left heap1) (union (_right heap1) heap2)
        else makeT p2 (_value heap2) (_left heap2) (union (_right heap2) heap1)

-- | Build a 'HeapT' from a priority, a value and two more 'HeapT's. Therefore,
-- the /priority has to be less or equal/ than all priorities in both 'HeapT'
-- parameters.
--
-- /The precondition is not checked/.
makeT :: (Ord prio) => prio -> val -> HeapT prio val -> HeapT prio val -> HeapT prio val
makeT p v a b = let
    ra = rank a
    rb = rank b
    s  = size a + size b + 1
    in assert (checkPrio a && checkPrio b) $ if ra > rb
        then Tree (rb + 1) s p v a b
        else Tree (ra + 1) s p v b a
    where
    checkPrio = maybe True (\(p', _, _) -> p <= p') . view
{-# INLINE makeT #-}

-- | Build the union of all given 'HeapT's.
unions :: (Ord prio) => [HeapT prio val] -> HeapT prio val
unions heaps = case tournamentFold' heaps of
    []  -> empty
    [h] -> h
    hs  -> unions hs
    where
    tournamentFold' :: (Monoid m) => [m] -> [m]
    tournamentFold' (x1:x2:xs) = (: tournamentFold' xs) $! mappend x1 x2
    tournamentFold' xs         = xs

-- | /O(log n)/ for the tail, /O(1)/ for the head. Find the priority-value pair
-- with minimal priority and delete it from the 'HeapT' (i. e. find head and tail
-- of the heap) if it is not empty. Otherwise, 'Nothing' is returned.
view :: (Ord prio) => HeapT prio val -> Maybe (prio, val, HeapT prio val)
view Empty = Nothing
view heap  = Just (_priority heap, _value heap, union (_left heap) (_right heap))
{-# INLINE view #-}

-- | Partition the 'HeapT' into two. @'partition' p h = (h1, h2)@: All
-- priority-value pairs in @h1@ fulfil the predicate @p@, those in @h2@ don't.
-- @'union' h1 h2 = h@.
partition :: (Ord prio) => ((prio, val) -> Bool) -> HeapT prio val
    -> (HeapT prio val, HeapT prio val)
partition _ Empty  = (empty, empty)
partition f heap
    | f (p, v)  = (makeT p v l1 r1, union l2 r2)
    | otherwise = (union l1 r1, makeT p v l2 r2)
    where
    (p, v)   = (_priority heap, _value heap)
    (l1, l2) = partition f (_left heap)
    (r1, r2) = partition f (_right heap)
{-# INLINE partition #-}

-- | @'splitAt' n h@: A list of the lowest @n@ priority-value pairs of @h@, in
--  ascending order of priority, and @h@, with those elements removed.
splitAt :: (Ord prio) => Int -> HeapT prio val -> ([(prio, val)], HeapT prio val)
splitAt n heap
    | n > 0     = case view heap of
        Nothing         -> ([], empty)
        Just (p, v, hs) -> let (xs, heap') = splitAt (n-1) hs in ((p, v):xs, heap')
    | otherwise = ([], heap)
{-# INLINE splitAt #-}

-- | @'span' p h@: The longest prefix of priority-value pairs of @h@, in
-- ascending order of priority, that satisfy @p@ and @h@, with those elements
-- removed.
span :: (Ord prio) => ((prio, val) -> Bool) -> HeapT prio val
     -> ([(prio, val)], HeapT prio val)
span f heap = case view heap of
    Nothing         -> ([], empty)
    Just (p, v, hs) -> let pv = (p, v)
        in if f pv
            then let (xs, heap') = span f hs in (pv:xs, heap')
            else ([], heap)
{-# INLINE span #-}

-- | /O(n log n)/. Build a 'HeapT' from the given priority-value pairs.
fromList :: (Ord prio) => [(prio, val)] -> HeapT prio val
fromList = fromDescList . sortBy (flip (comparing fst))
{-# INLINE fromList #-}

-- | /O(n log n)/. List all priority-value pairs of the 'HeapT' in no specific
-- order.
toList :: HeapT prio val -> [(prio, val)]
toList Empty = []
toList heap  = let
    left  = _left heap
    right = _right heap
    in
    (_priority heap, _value heap) : if (size right) < (size left)
        then toList right ++ toList left
        else toList left  ++ toList right
{-# INLINE toList #-}

-- | /O(n)/. Create a 'HeapT' from a list providing its priority-value pairs in
-- descending order of priority.
--
-- /The precondition is not checked/.
fromDescList :: (Ord prio) => [(prio, val)] -> HeapT prio val
fromDescList = foldl' (\h (p, v) -> uncheckedCons p v h) empty
{-# INLINE fromDescList #-}

-- | /O(n log n)/. List the priority-value pairs of the 'HeapT' in ascending
-- order of priority.
toAscList :: (Ord prio) => HeapT prio val -> [(prio, val)]
toAscList = fst . span (const True)
{-# INLINE toAscList #-}

-- | List the priority-value pairs of the 'HeapT' just like 'toAscList' does,
-- but don't ignore the value @val@ when sorting.
toPairAscList :: (Ord prio, Ord val) => HeapT prio val -> [(prio, val)]
toPairAscList = concat
    . fmap (sortBy (comparing snd))
    . groupBy (\x y -> fst x == fst y)
    . toAscList
