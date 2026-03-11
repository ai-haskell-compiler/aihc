module Test.Heap.Internal
    ( runTests
    , leftistHeapProperty
    ) where

import Data.Char
import Data.Heap.Internal as Heap
import qualified Data.List as List
import Test.Heap.Common
import Test.QuickCheck

runTests :: IO ()
runTests = do
    qc "Eq" (eqProperty :: HeapT Int Char -> HeapT Int Char -> HeapT Int Char -> Bool)
    qc "Ord" (ordProperty :: HeapT Int Char -> HeapT Int Char -> HeapT Int Char -> Bool)
    qc "leftist heap" (leftistHeapProperty :: HeapT Int Char -> Bool)
    qc "read/show" (readShowProperty :: [HeapT Int Char] -> Bool)
    qc "Monoid" (monoidProperty :: HeapT Int Char -> HeapT Int Char -> HeapT Int Char -> Bool)
    qc "union" (unionProperty :: HeapT Int Char -> HeapT Int Char -> Bool)
    qc "Functor" (functorProperty (subtract 1000) (*42) :: HeapT Char Int -> Bool)
    qc "fmap" (fmapProperty (subtract 1000) :: HeapT Char Int -> Bool)
    qc "Foldable" (foldableProperty :: HeapT Char Int -> Bool)
    qc "size" sizeProperty
    qc "view" viewProperty
    qc "singleton" (singletonProperty :: Char -> Int -> Bool)
    qc "partition" (partitionProperty testProp :: HeapT Char Int -> Bool)
    qc "splitAt" splitAtProperty
    qc "span" spanProperty
    qc "fromList/toList" (listProperty :: [Char] -> Bool)
    qc "fromDescList/toAscList" (sortedListProperty :: [Char] -> Bool)
    where
    testProp :: Char -> Int -> Bool
    testProp c i = even i && isLetter c

instance (Arbitrary prio, Arbitrary val, Ord prio) => Arbitrary (HeapT prio val) where
    arbitrary = fmap (fromList . take 100) arbitrary
    shrink    = fmap fromList . shrink . toList

leftistHeapProperty :: (Ord prio) => HeapT prio val -> Bool
leftistHeapProperty Empty = True
leftistHeapProperty heap  =
    (maybe True (\(p, _, _) -> p >= _priority heap) (view (_left heap)))
        && (maybe True (\(p, _, _) -> p >= _priority heap) (view (_right heap)))
        && _rank heap == 1 + rank (_right heap)    -- rank == length of right spine
        && rank (_left heap) >= rank (_right heap) -- leftist property
        && _size heap == 1 + size (_left heap) + size (_right heap)
        && leftistHeapProperty (_left heap)
        && leftistHeapProperty (_right heap)

unionProperty :: (Ord prio, Ord val) => HeapT prio val -> HeapT prio val -> Bool
unionProperty a b = let ab = a `union` b
    in leftistHeapProperty ab && size ab == size a + size b
        && ab == ab `union` empty
        && ab == empty `union` ab
        && a == unions (fmap (uncurry singleton) (toList a))

fmapProperty :: (Ord prio) => (val -> val) -> HeapT prio val -> Bool
fmapProperty f = leftistHeapProperty . fmap f

sizeProperty :: Int -> Bool
sizeProperty n = let
    n' = abs n `mod` 100
    h  = fromList (zip [1..n'] (repeat ())) :: HeapT Int ()
    in
    size h == n' && if n' == 0 then isEmpty h else not (isEmpty h)

viewProperty :: [Int] -> Bool
viewProperty []   = True
viewProperty list = let
    heap = fromList (zip list (repeat ()))
    m    = minimum list
    in case view heap of
        Nothing          -> False -- list is not empty
        Just (p, (), hs) -> p == m
            && heap == union (singleton p ()) hs
            && viewProperty (tail list)

singletonProperty :: (Ord prio, Ord val) => prio -> val -> Bool
singletonProperty p v = let
    heap = singleton p v
    in
    leftistHeapProperty heap && size heap == 1 && view heap == Just (p, v, empty)

partitionProperty :: (Ord prio, Ord val) => (prio -> val -> Bool) -> HeapT prio val -> Bool
partitionProperty p heap = let
    (yes,  no)  = partition (uncurry p) heap
    (yes', no') = List.partition (uncurry p) (toList heap)
    in
    (heap, empty) == partition (const True) heap
        && (empty, heap) == partition (const False) heap
        && yes == fromList yes'
        && no == fromList no'
        && yes `union` no == heap -- nothing gets lost

splitAtProperty :: Int -> Int -> Bool
splitAtProperty i n = let
    i'     = i `mod` 100
    n'     = n `mod` 100
    ab     = [1..n']
    (a, b) = List.splitAt i' ab
    heap   = fromList $ zip ab (repeat ())
    in
    Heap.splitAt i' heap == (zip a (repeat ()), fromList (zip b (repeat ())))

spanProperty :: Int -> Int -> Bool
spanProperty i n = let
    i'      = i `mod` 100
    n'      = n `mod` 100
    ab      = [1..n']
    (a, b)  = List.span (<= i') ab
    (a', h) = Heap.span ((<=i') . fst) $ fromList (zip ab (repeat ()))
    in
    a == (fmap fst a') && h == fromList (zip b (repeat ()))

listProperty :: (Ord prio) => [prio] -> Bool
listProperty xs = let
    list = List.sort xs
    heap = fromList (zip xs [(1 :: Int) ..])
    in
    list == fmap fst (List.sort (toList heap))

sortedListProperty :: (Ord prio) => [prio] -> Bool
sortedListProperty xs = let
    list = List.sort xs
    heap = fromDescList (zip (reverse list) [(1 :: Int) ..])
    in
    list == fmap fst (toAscList heap)
