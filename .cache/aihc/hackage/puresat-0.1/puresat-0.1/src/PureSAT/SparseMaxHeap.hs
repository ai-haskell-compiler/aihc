{-# LANGUAGE CPP             #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
-- {-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
module PureSAT.SparseMaxHeap (
    SparseHeap,
    Weight,
    sizeofSparseHeap,
    newSparseHeap,
    cloneSparseHeap,
    memberSparseHeap,
    insertSparseHeap,
    deleteSparseHeap,
    popSparseHeap,
    popSparseHeap_,
    elemsSparseHeap,
    clearSparseHeap,
    extendSparseHeap,
    drainSparseHeap,
    modifyWeightSparseHeap,
    scaleWeightsSparseHeap,
) where

import Data.Bits
import Data.Primitive.PrimVar

import PureSAT.Base
import PureSAT.Utils
import PureSAT.Prim

type Weight = Word

-- import Debug.Trace

-- #define CHECK_INVARIANTS

-- $setup
-- >>> import Control.Monad.ST (runST)

-- | Like sparse set https://research.swtch.com/sparse,
-- but also a max heap https://en.wikipedia.org/wiki/Heap_(data_structure)
--
-- i.e. pop returns minimum element.
--
data SparseHeap s = SH
    { size   :: {-# UNPACK #-} !(PrimVar s Int)
    , dense  :: {-# UNPACK #-} !(MutablePrimArray s Int)
    , sparse :: {-# UNPACK #-} !(MutablePrimArray s Int)
    , weight :: {-# UNPACK #-} !(MutablePrimArray s Word)
    }

le :: Int -> Weight -> Int -> Weight -> Bool
le _ !u _y !v = u >= v
{-
le !x !u !y !v = u >= v
    | u > v          = True
    | u == v, x <= y = True
    | otherwise      = False
-}

checking :: String -> SparseHeap s -> ST s a -> ST s a
{-# INLINE checking #-}

#ifdef CHECK_INVARIANTS

#define CHECK(tag,heap) _invariant tag heap

checking tag heap m = do
    _invariant (tag ++ " pre") heap
    x <- m
    _invariant (tag ++ " post") heap
    return x

#else

#define CHECK(tag,heap)

checking _tag _heap m = m

#endif

_invariant :: String -> SparseHeap s -> ST s ()
_invariant tag SH {..} = do
    n         <- readPrimVar size
    capacity  <- getSizeofMutablePrimArray dense
    capacity1 <- getSizeofMutablePrimArray sparse
    capacity2 <- getSizeofMutablePrimArray weight

    unless (n <= capacity && capacity == capacity1 && capacity == capacity2) $
        error $ "capacities " ++ show (n, capacity, capacity1, capacity2)

    checkStructure capacity n 0
    checkHeaps n 0
  where
    checkStructure capacity n i =
        if i >= n
        then return ()
        else do
            x <- readPrimArray dense i
            unless (x < capacity) $ error $ "x < capacity" ++ show (x, capacity)
            j <- readPrimArray sparse x
            unless (i == j) $ error $ "i == j" ++ show (i, j)
            checkStructure capacity n (i + 1)

    checkHeaps n i =
        if i >= n
        then return ()
        else do
            x <- readPrimArray dense i
            u <- readPrimArray weight x
            heap n i x u
            checkHeaps n (i + 1)

    heap n i x u = do
        let !j = 2 * i + 1
        let !k = 2 * i + 2

        when (j < n) $ do
            y <- readPrimArray dense j
            v <- readPrimArray weight y
            unless (le x u y v) $ error $ "heap 1 " ++ tag ++ " " ++ show (n, i, x, u, j, y, v)

        when (k < n) $ do
            z <- readPrimArray dense k
            w <- readPrimArray weight z
            unless (le x u z w) $ error $ "heap 2 " ++ tag ++ " " ++ show (n, i, x, u, k, z, w)

-- | Create new sparse heap.
--
-- >>> runST $ newSparseHeap 100 >>= elemsSparseHeap
-- []
--
newSparseHeap
    :: Int -- ^ max integer
    -> ST s (SparseHeap s)
newSparseHeap !capacity' = do
    let !capacity = max 1024 capacity'
    size <- newPrimVar 0
    dense <- newPrimArray capacity
    sparse <- newPrimArray capacity
    weight <- newPrimArray capacity
    setPrimArray weight 0 capacity 0

    return SH {..}

cloneSparseHeap :: SparseHeap s -> ST s (SparseHeap s)
cloneSparseHeap SH {..} = do
    capacity <- getSizeofMutablePrimArray dense
    
    size'   <- readPrimVar size >>= newPrimVar
    dense'  <- resizeMutablePrimArray dense capacity
    sparse' <- resizeMutablePrimArray sparse capacity
    weight' <- resizeMutablePrimArray weight capacity

    copyMutablePrimArray dense' 0 dense 0 capacity
    copyMutablePrimArray sparse' 0 sparse 0 capacity
    copyMutablePrimArray weight' 0 weight 0 capacity

    return SH { size = size', dense = dense', sparse = sparse', weight = weight' }

-- | Size of sparse heap.
--
-- >>> runST $ do { set <- newSparseHeap 100; mapM_ (insertSparseHeap set) [3,5,7,11,13,11]; sizeofSparseHeap set }
-- 5
--
sizeofSparseHeap :: SparseHeap s -> ST s Int
sizeofSparseHeap SH {..} = readPrimVar size

-- | Extend sparse heap to fit new capacity.
extendSparseHeap
    :: Int -- ^ new capacity
    -> SparseHeap s
    -> ST s (SparseHeap s)
extendSparseHeap capacity1 heap@SH {..} = do
    capacity2 <- getSizeofMutablePrimArray dense
    let capacity = nextPowerOf2 (max capacity2 capacity1)

    if capacity <= capacity2
    then return heap
    else do

        dense' <- resizeMutablePrimArray dense capacity
        sparse' <- resizeMutablePrimArray sparse capacity
        weight' <- resizeMutablePrimArray weight capacity
        setPrimArray weight' capacity2 (capacity - capacity2) 0

        return SH { size, dense = dense', sparse = sparse', weight = weight' }

-- | Test for membership.
--
-- >>> runST $ do { set <- newSparseHeap 100; mapM_ (insertSparseHeap set) [3,5,7,11,13,11]; memberSparseHeap set 10 }
-- False
--
-- >>> runST $ do { set <- newSparseHeap 100; mapM_ (insertSparseHeap set) [3,5,7,11,13,11]; memberSparseHeap set 13 }
-- True
--
memberSparseHeap :: SparseHeap s -> Int -> ST s Bool
memberSparseHeap heap@SH {..} x = checking "member" heap $ do
    n <- readPrimVar size
    i <- readPrimArray sparse x
    if 0 <= i && i < n
    then do
        x' <- readPrimArray dense i
        return (x' == x)
    else return False

-- | Insert into the heap.
--
-- >>> runST $ do { set <- newSparseHeap 100; mapM_ (insertSparseHeap set) [3,5,7,11,13,11]; elemsSparseHeap set }
-- [3,5,7,11,13]
--
insertSparseHeap :: SparseHeap s -> Int -> ST s ()
insertSparseHeap heap@SH {..} x = checking "insert" heap $ do
    n <- readPrimVar size
    i <- readPrimArray sparse x
    if 0 <= i && i < n
    then do
        x' <- readPrimArray dense i
        if x == x' then return () else insert n
    else insert n
  where
    {-# INLINE insert #-}
    insert !n = do
        writePrimArray dense n x
        writePrimArray sparse x n
        writePrimVar size (n + 1)
        u <- readPrimArray weight x
        swim (n + 1) dense sparse weight n x u

-- | Delete element from the heap.
--
-- >>> runST $ do { set <- newSparseHeap 100; deleteSparseHeap set 10; elemsSparseHeap set }
-- []
--
-- >>> let insert heap x = modifyWeightSparseHeap heap x (\_ -> fromIntegral $ 100 - x) >> insertSparseHeap heap x;
--
-- >>> runST $ do { set <- newSparseHeap 100; mapM_ (insert set) [3,5,7,11,13,11]; deleteSparseHeap set 10; elemsSparseHeap set }
-- [3,5,7,11,13]
--
-- >>> runST $ do { set <- newSparseHeap 100; mapM_ (insert set) [3,5,7,11,13,11]; deleteSparseHeap set 13; elemsSparseHeap set }
-- [3,5,7,11]
--
-- >>> runST $ do { set <- newSparseHeap 100; mapM_ (insert set) [3,5,7,11,13,11]; deleteSparseHeap set 11; elemsSparseHeap set }
-- [3,5,7,13]
--
-- >>> runST $ do { set <- newSparseHeap 100; mapM_ (insert set) [3,5,7,11,13,11]; deleteSparseHeap set 3; elemsSparseHeap set }
-- [5,11,7,13]
--
-- >>> runST $ do { set <- newSparseHeap 100; mapM_ (insert set) $ [0,2..20] ++ [19,17..3]; deleteSparseHeap set 10; elemsSparseHeap set }
-- [0,2,4,5,3,17,12,9,6,8,20,19,18,15,13,14,11,16,7]
--
deleteSparseHeap :: SparseHeap s -> Int -> ST s ()
deleteSparseHeap heap@SH {..} x = checking "delete" heap $ do
    n <- readPrimVar size
    i <- readPrimArray sparse x
    if 0 <= i && i < n
    then do
        x' <- readPrimArray dense i
        if x == x' then delete i n else return ()
    else return ()
  where
    {-# INLINE delete #-}
    delete !i !n = do
        let !n' = n - 1
        writePrimVar size n'

        if i == n'
        then return ()
        else swimSink n' i

    -- to delete element we swim it up, as if it had maximum weight, and then we pop it
    swimSink n i
        | i > 0
        = do
            -- j = floor (i - 1 / 2)
            let !j = unsafeShiftR (i - 1) 1
            y <- readPrimArray dense j
            swap' dense sparse i x j y
            swimSink n j

        | otherwise -- i == 0
        = do
            let j = n
            writePrimVar size j

            y <- readPrimArray dense j
            v <- readPrimArray weight y
            swap' dense sparse 0 x j y
            sink j dense sparse weight 0 y v


{-# INLINE swap' #-}
swap' :: MutablePrimArray s Int -> MutablePrimArray s Int -> Int -> Int -> Int -> Int -> ST s ()
swap' !dense !sparse !i !x !j !y  = do
    writePrimArray dense j x
    writePrimArray dense i y
    writePrimArray sparse x j
    writePrimArray sparse y i

-- sift down
sink :: Int -> MutablePrimArray s Int -> MutablePrimArray s Int -> MutablePrimArray s Weight  -> Int -> Int -> Weight -> ST s ()
sink !n !dense !sparse !weight !i !x !u
    | k < n
    = do
        l <- readPrimArray dense j
        r <- readPrimArray dense k
        v <- readPrimArray weight l
        w <- readPrimArray weight r

        -- traceM $ "sink" ++ show ((i, x, u), (j, l, v), (k, r, w))

        if le x u l v -- x <= l
        then do
            if le x u r w -- x <= r
            then return ()
            else do
                 -- r < x <= l; swap x and r
                swap' dense sparse i x k r
                sink n dense sparse weight k x u

        else do
            if le l v r w -- l <= r
            then do
                -- l < x, l <= r; swap x and l
                swap' dense sparse i x j l
                sink n dense sparse weight j x u

            else do
                -- r < l <= x; swap x and r
                swap' dense sparse i x k r
                sink n dense sparse weight k x u

    | j < n
    = do
        l <- readPrimArray dense j
        v <- readPrimArray weight l
        if le x u l v -- x <= l
        then return ()
        else do
            swap' dense sparse i x j l
            -- no need to sink further, as we sinked to the last element.

    | otherwise
    = return ()
  where
    !j = 2 * i + 1
    !k = j + 1

-- sift up
swim :: Int -> MutablePrimArray s Int -> MutablePrimArray s Int -> MutablePrimArray s Weight  -> Int -> Int -> Weight -> ST s ()
swim !_n !dense !sparse !weight !i !x !u
    | i <= 0
    = return ()

    | otherwise
    = do
        -- j = floor (i - 1 / 2)
        let !j = unsafeShiftR (i - 1) 1
        y <- readPrimArray dense j
        v <- readPrimArray weight y

        unless (le y v x u) $ do
            swap' dense sparse i x j y
            swim _n dense sparse weight j x u

-- | Modify weight of the element.
--
-- >>> let insert heap x = modifyWeightSparseHeap heap x (\_ -> fromIntegral $ 100 - x) >> insertSparseHeap heap x;
-- >>> let populate heap = mapM_ (insert heap) [5,3,7,11,13,11]
-- >>> let populate' heap = mapM_ (insertSparseHeap heap) [5,3,7,11,13,11]
--
-- >>> runST $ do { heap <- newSparseHeap 100; populate heap; popSparseHeap heap }
-- Just 3
--
-- >>> runST $ do { heap <- newSparseHeap 100; populate heap; modifyWeightSparseHeap heap 3 (\_ -> 0); popSparseHeap heap }
-- Just 5
--
-- Weight are preserved even if element is not in the heap at the moment
--
-- >>> runST $ do { heap <- newSparseHeap 100; modifyWeightSparseHeap heap 7 (\_ -> 100); populate' heap; popSparseHeap heap }
-- Just 7
--
modifyWeightSparseHeap :: forall s. SparseHeap s -> Int -> (Weight -> Weight) -> ST s ()
modifyWeightSparseHeap heap@SH {..} !x f = checking "modify" heap $ do
    u' <- readPrimArray weight x
    let !u = f u'
    writePrimArray weight x u

    if u == u'
    then return ()
    else do
        n <- readPrimVar size
        i <- readPrimArray sparse x
        if 0 <= i && i < n
        then do
            x' <- readPrimArray dense i
            if x == x' then balance n i u u' else return ()
        else return ()
  where
    balance :: Int -> Int -> Weight -> Weight -> ST s ()
    balance !n !i !u !u'
        | u >= u'
        = swim n dense sparse weight i x u

        | otherwise
        = sink n dense sparse weight i x u
{-# INLINE modifyWeightSparseHeap #-}

scaleWeightsSparseHeap :: forall s. SparseHeap s -> (Weight -> Weight) -> ST s ()
scaleWeightsSparseHeap heap@SH{..} f = checking "scale" heap $ do
    !capacity <- getSizeofMutablePrimArray weight
    go capacity 0
  where
    go !n !i
        | i >= n    = return ()
        | otherwise = do
            u <- readPrimArray weight i
            writePrimArray weight i (f u)

-- | Pop element from the heap.
--
-- >>> let insert heap x = modifyWeightSparseHeap heap x (\_ -> - fromIntegral x) >> insertSparseHeap heap x;
--
-- >>> runST $ do { heap <- newSparseHeap 100; mapM_ (insert heap) [5,3,7,11,13,11]; popSparseHeap heap }
-- Just 3
--
-- >>> runST $ do { heap <- newSparseHeap 500; mapM_ (insert heap) [1..400]; drainSparseHeap heap }
-- [1,2...,400]
popSparseHeap :: SparseHeap s -> ST s (Maybe Int)
popSparseHeap heap = popSparseHeap_ heap (return Nothing) (return . Just)

{-# INLINE popSparseHeap_ #-}
popSparseHeap_ :: SparseHeap s -> ST s r -> (Int -> ST s r) -> ST s r
popSparseHeap_ _heap@SH {..} no yes = do
    CHECK("pop pre", _heap)

    n <- readPrimVar size

    -- xs <- freezePrimArray dense 0 n
    -- traceM $ "pop" ++ show (take 15 $ primArrayToList xs)

    if n <= 0
    then no
    else do
        let !j = n - 1
        writePrimVar size j

        x <- readPrimArray dense 0
        y <- readPrimArray dense j
        v <- readPrimArray weight y
        swap' dense sparse 0 x j y
        sink j dense sparse weight 0 y v

        CHECK("pop post", _heap)
        yes x

-- | Clear sparse heap.
--
-- >>> runST $ do { set <- newSparseHeap 100; mapM_ (insertSparseHeap set) [3,5,7,11,13,11]; clearSparseHeap set; elemsSparseHeap set }
-- []
--
clearSparseHeap :: SparseHeap s -> ST s ()
clearSparseHeap SH {..} = do
    writePrimVar size 0

-- | Elements of the heap.
--
-- Returns elements as they are internally stored.
--
elemsSparseHeap :: SparseHeap s -> ST s [Int]
elemsSparseHeap SH {..} = do
    n <- readPrimVar size
    go [] 0 n
  where
    go !acc !i !n
        | i < n
        = do
            x <- readPrimArray dense i
            go (x : acc) (i + 1) n

        | otherwise
        = return (reverse acc)

-- | Drain element from the heap.
--
-- >>> let insert heap x = modifyWeightSparseHeap heap x (\_ -> - fromIntegral x) >> insertSparseHeap heap x;
--
-- >>> runST $ do { set <- newSparseHeap 100; mapM_ (insert set) [3,5,7,11,13,11]; drainSparseHeap set }
-- [3,5,7,11,13]
--
drainSparseHeap :: SparseHeap s -> ST s [Int]
drainSparseHeap heap = go id where
    go acc = popSparseHeap_ heap
        (return (acc []))
        (\x -> go (acc . (x :)))
