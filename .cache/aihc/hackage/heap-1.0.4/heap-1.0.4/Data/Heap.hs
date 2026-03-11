-- | A flexible implementation of min-, max-, min-priority, max-priority and
-- custom-priority heaps based on the leftist-heaps from Chris Okasaki's book
-- \"Purely Functional Data Structures\", Cambridge University Press, 1998,
-- chapter 3.1.
--
-- There are different flavours of 'Heap's, each of them following a different
-- strategy when ordering its elements:
--
--  * Choose 'MinHeap' or 'MaxHeap' if you need a simple minimum or maximum heap
--    (which always keeps the minimum/maximum element at the head of the 'Heap').
--
--  * If you wish to manually annotate a value with a priority, e. g. an @IO ()@
--    action with an 'Int' use 'MinPrioHeap' or 'MaxPrioHeap'. They manage
--    @(prio, val)@ tuples so that only the priority (and not the value)
--    influences the order of elements.
--
--  * If you still need something different, define a custom order for the heap
--    elements by implementing an instance of 'HeapItem' and let the maintainer
--    know what's missing.
--
-- All sorts of heaps mentioned above ('MinHeap', 'MaxHeap', 'MinPrioHeap' and
-- 'MaxPrioHeap') are built on the same underlying type: @'HeapT' prio val@. It is
-- a simple minimum priority heap. The trick is, that you never insert @(prio,
-- val)@ pairs directly: You only insert an \"external representation\", usually
-- called @item@, and an appropriate 'HeapItem' instance is used to 'split' the
-- @item@ to a @(prio, val)@ pair. For details refer to the documentation of
-- 'HeapItem'.
module Data.Heap
    ( -- * Types
      -- ** Various heap flavours
      HeapT, Heap
    , MinHeap, MaxHeap, MinPrioHeap, MaxPrioHeap
      -- ** Ordering strategies
    , HeapItem(..), MinPolicy, MaxPolicy, FstMinPolicy, FstMaxPolicy
      -- * Query
    , I.isEmpty, null, I.size
      -- * Construction
    , I.empty, singleton, insert, I.union, I.unions
      -- * Deconstruction
    , view, viewHead, viewTail
      -- * Filter
    , filter, partition
      -- * Subranges
    , take, drop, splitAt
    , takeWhile, dropWhile, span, break
      -- * Conversion
      -- ** List
    , fromList, toList
      -- ** Ordered list
    , fromAscList, toAscList
    , fromDescList, toDescList
    ) where

import Data.Heap.Item
import Data.Heap.Internal ( HeapT )
import qualified Data.Heap.Internal as I
import Prelude hiding
    ( break, drop, dropWhile, filter, null, span, splitAt, take, takeWhile )

-- | /O(1)/. Is the 'HeapT' empty?
null :: HeapT prio val -> Bool
null = I.isEmpty

-- | /O(1)/. Create a singleton 'HeapT'.
singleton :: (HeapItem pol item) => item -> Heap pol item
singleton = (uncurry I.singleton) . split

-- | /O(log n)/. Insert a single item into the 'HeapT'.
insert :: (HeapItem pol item) => item -> Heap pol item -> Heap pol item
insert = I.union . singleton

-- | /O(1)/ for the head, /O(log n)/ for the tail. Find the item with minimal
-- associated priority and remove it from the 'Heap' (i. e. find head and tail
-- of the heap) if it is not empty. Otherwise, 'Nothing' is returned.
view :: (HeapItem pol item) => Heap pol item -> Maybe (item, Heap pol item)
view = fmap (\(p, v, h) -> (merge (p, v), h)) . I.view

-- | /O(1)/. Find the item with minimal associated priority on the 'Heap' (i. e.
-- its head) if it is not empty. Otherwise, 'Nothing' is returned.
viewHead :: (HeapItem pol item) => Heap pol item -> Maybe item
viewHead = fmap fst . view

-- | /O(log n)/. Remove the item with minimal associated priority and from the
-- 'Heap' (i. e. its tail) if it is not empty. Otherwise, 'Nothing' is returned.
viewTail :: (HeapItem pol item) => Heap pol item -> Maybe (Heap pol item)
viewTail = fmap snd . view

-- | Remove all items from a 'HeapT' not fulfilling a predicate.
filter :: (HeapItem pol item) => (item -> Bool) -> Heap pol item -> Heap pol item
filter p = fst . (partition p)

-- | Partition the 'Heap' into two. @'partition' p h = (h1, h2)@: All items in
-- @h1@ fulfil the predicate @p@, those in @h2@ don't. @'union' h1 h2 = h@.
partition :: (HeapItem pol item)
    => (item -> Bool) -> Heap pol item -> (Heap pol item, Heap pol item)
partition = I.partition . splitF

-- | Take the first @n@ items from the 'Heap'.
take :: (HeapItem pol item) => Int -> Heap pol item -> [item]
take n = fst . splitAt n

-- | Remove first @n@ items from the 'Heap'.
drop :: (HeapItem pol item) => Int -> Heap pol item -> Heap pol item
drop n = snd . splitAt n

-- | @'splitAt' n h@: Return a list of the first @n@ items of @h@ and @h@, with
-- those elements removed.
splitAt :: (HeapItem pol item) => Int -> Heap pol item -> ([item], Heap pol item)
splitAt n heap = let (xs, heap') = I.splitAt n heap in (fmap merge xs, heap')

-- | @'takeWhile' p h@: List the longest prefix of items in @h@ that satisfy @p@.
takeWhile :: (HeapItem pol item) => (item -> Bool) -> Heap pol item -> [item]
takeWhile p = fst . (span p)

-- | @'dropWhile' p h@: Remove the longest prefix of items in @h@ that satisfy
-- @p@.
dropWhile :: (HeapItem pol item)
    => (item -> Bool) -> Heap pol item -> Heap pol item
dropWhile p = snd . (span p)

-- | @'span' p h@: Return the longest prefix of items in @h@ that satisfy @p@ and
-- @h@, with those elements removed.
span :: (HeapItem pol item)
    => (item -> Bool) -> Heap pol item -> ([item], Heap pol item)
span p heap = let (xs, heap') = I.span (splitF p) heap in (fmap merge xs, heap')

-- | @'break' p h@: The longest prefix of items in @h@ that do /not/ satisfy @p@
-- and @h@, with those elements removed.
break :: (HeapItem pol item)
    => (item -> Bool) -> Heap pol item -> ([item], Heap pol item)
break p = span (not . p)

-- | /O(n log n)/. Build a 'Heap' from the given items. Assuming you have a
-- sorted list, you probably want to use 'fromDescList' or 'fromAscList', they
-- are faster than this function.
fromList :: (HeapItem pol item) => [item] -> Heap pol item
fromList = I.fromList . fmap split

-- | /O(n log n)/. List all items of the 'Heap' in no specific order.
toList :: (HeapItem pol item) => Heap pol item -> [item]
toList = fmap merge . I.toList

-- | /O(n)/. Create a 'Heap' from a list providing its items in ascending order
-- of priority (i. e. in the same order they will be removed from the 'Heap').
-- This function is faster than 'fromList' but not as fast as 'fromDescList'.
--
-- /The precondition is not checked/.
fromAscList :: (HeapItem pol item) => [item] -> Heap pol item
fromAscList = fromDescList . reverse

-- | /O(n log n)/. List the items of the 'Heap' in ascending order of priority.
toAscList :: (HeapItem pol item) => Heap pol item -> [item]
toAscList = fmap merge . I.toAscList

-- | /O(n)/. Create a 'Heap' from a list providing its items in descending order
-- of priority (i. e. they will be removed inversely from the 'Heap'). Prefer
-- this function over 'fromList' and 'fromAscList', it's faster.
--
-- /The precondition is not checked/.
fromDescList :: (HeapItem pol item) => [item] -> Heap pol item
fromDescList = I.fromDescList . fmap split

-- | /O(n log n)/. List the items of the 'Heap' in descending order of priority.
-- Note that this function is not especially efficient (it is implemented in
-- terms of 'reverse' and 'toAscList'), it is provided as a counterpart of the
-- efficient 'fromDescList' function.
toDescList :: (HeapItem pol item) => Heap pol item -> [item]
toDescList = reverse . toAscList
