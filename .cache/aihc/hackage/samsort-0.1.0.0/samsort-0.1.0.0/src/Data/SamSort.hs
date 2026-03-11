{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Copyright: (c) 2024 Soumik Sarkar
-- License: BSD-3-Clause
--
-- A stable adaptive mergesort implementation.
--
-- The merging strategy used is "2-merge" as described by
--
-- * Sam Buss, Alexander Knop,
--   /\"Strategies for Stable Merge Sorting\"/,
--   2018,
--   https://arxiv.org/abs/1801.04641
--
module Data.SamSort
  ( sortArrayBy
  , sortIntArrayBy
  ) where

import Control.Monad (when)
import Data.Bits (finiteBitSize, countLeadingZeros, shiftR)

import GHC.ST (ST(..))
import GHC.Exts
  ( Int#
  , Int(..)
  , MutableArray#
  , MutableByteArray#
  , (*#)
  , copyMutableArray#
  , copyMutableByteArray#
  , newArray#
  , newByteArray#
  , readArray#
  , readIntArray#
  , writeArray#
  , writeIntArray#
  )

-- | \(O(n \log n)\). Sort a slice of a @MutableArray#@ using a comparison
-- function.
--
-- The comparison must form a total order, as required by the 'Ord' laws.
--
-- @offset@ and @length@ must be valid, i.e.
--
-- * @0 <= offset < array size@ .
-- * @0 <= length@ .
-- * @offset + length <= array size@ .
--
-- This function will inline to get the best performance out of statically
-- known comparison functions. To avoid code duplication, create a wrapping
-- definition and reuse it as necessary.
--
sortArrayBy
  :: (a -> a -> Ordering)  -- ^ comparison
  -> MutableArray# s a
  -> Int                   -- ^ offset
  -> Int                   -- ^ length
  -> ST s ()
sortArrayBy cmp =  -- Inline with 1 arg
  \ma# !off !len -> sortArrayBy' cmp (MA ma#) off len
{-# INLINE sortArrayBy #-}

sortArrayBy'
  :: (a -> a -> Ordering)
  -> MA s a
  -> Int
  -> Int
  -> ST s ()
sortArrayBy' _ !_ !_ len | len < 2 = pure ()
sortArrayBy' cmp ma off len = do
  -- See Note [Algorithm overview]

  !swp <- newA (len `shiftR` 1) errorElement
  !stk <- newI (lg len)

  let -- Merge [i1,i2) and [i2,i3)
      -- Precondition: i1 < i2 < i3
      merge !i1 !i2 !i3
        | i2-i1 <= i3-i2 = mergeCopyLeft1 i1 i2 i3
        | otherwise = mergeCopyRight1 i1 i2 i3

      mergeCopyLeft1 !i1 !i2 !i3 = do
        x0 <- readA ma i1 -- See Note [First iteration]
        y <- readA ma i2
        if y `lt` x0
        then mergeCopyLeft2 i1 i2 i3
        else do
          let skip i | i >= i2 = pure ()
              skip i = do
                x <- readA ma i
                if y `lt` x
                then mergeCopyLeft2 i i2 i3
                else skip (i+1)
          skip (i1+1)

      -- Precondition: i1 < i2 < i3, (ma!i2) `lt` (ma!i1)
      mergeCopyLeft2 !i1 !i2 !i3 = do
        copyA ma i1 swp 0 (i2-i1)
        readA ma i2 >>= writeA ma i1
        if i2+1 < i3
        then loop 0 (i2+1) (i1+1)
        else copyA swp 0 ma (i1+1) len1
        where
          !len1 = i2-i1
          loop !h !j !k = do
            x <- readA swp h
            y0 <- readA ma j -- See Note [First iteration]
            let nxt !j1 !k1 = do
                  writeA ma k1 x
                  when (h+1 < len1) $
                    loop (h+1) j1 (k1+1)
            if y0 `lt` x
            then do
              let loop2 j1 !k1 | j1 >= i3 = copyA swp h ma k1 (len1-h)
                  loop2 j1 k1 = do
                    y <- readA ma j1
                    if y `lt` x
                    then do
                      writeA ma k1 y
                      loop2 (j1+1) (k1+1)
                    else
                      nxt j1 k1
              writeA ma k y0
              loop2 (j+1) (k+1)
            else
              nxt j k

      mergeCopyRight1 !i1 !i2 !i3 = do
        x <- readA ma (i2-1)
        y0 <- readA ma (i3-1) -- See Note [First iteration]
        if y0 `lt` x
        then mergeCopyRight2 i1 i2 i3
        else do
          let skip j | j < i2 = pure ()
              skip j = do
                y <- readA ma j
                if y `lt` x
                then mergeCopyRight2 i1 i2 (j+1)
                else skip (j-1)
          skip (i3-2)

      -- Precondition: i1 < i2 < i3, (ma!(i3-1)) `lt` (ma!(i2-1))
      mergeCopyRight2 !i1 !i2 !i3 = do
        copyA ma i2 swp 0 (i3-i2)
        readA ma (i2-1) >>= writeA ma (i3-1)
        if i2-2 >= i1
        then loop (i2-2) (i3-i2-1) (i3-2)
        else copyA swp 0 ma i1 (i3-i2)
        where
          loop !h !j !k = do
            x0 <- readA ma h -- See Note [First iteration]
            y <- readA swp j
            let nxt !h1 !k1 = do
                  writeA ma k1 y
                  when (j > 0) $
                    loop h1 (j-1) (k1-1)
            if y `lt` x0
            then do
              let loop2 h1 !_ | h1 < i1 = copyA swp 0 ma i1 (j+1)
                  loop2 h1 k1 = do
                    x <- readA ma h1
                    if y `lt` x
                    then do
                      writeA ma k1 x
                      loop2 (h1-1) (k1-1)
                    else
                      nxt h1 k1
              writeA ma k x0
              loop2 (h-1) (k-1)
            else
              nxt h k

  mergeStrategy merge getRun stk off end

  where
    lt x y = case cmp x y of LT -> True; _ -> False
    {-# INLINE lt #-}
    -- Note: Use lt instead of gt. Why? Because `compare` for types like Int and
    -- Word are defined in a way that needs one `<` op for LT but two (`<`,`==`)
    -- for GT.

    !end = off + len

    getRun = mkGetRun lt (readA ma) (writeA ma) (reverseA ma) end
{-# INLINE sortArrayBy' #-}

-- | \(O(n \log n)\). Sort a slice of a @MutableByteArray#@ interpreted as an
-- array of @Int#@s using a comparison function.
--
-- The comparison must form a total order, as required by the 'Ord' laws.
--
-- @offset@ and @length@ must be valid, i.e.
--
-- * @0 <= offset < array size@ .
-- * @0 <= length@ .
-- * @offset + length <= array size@ .
--
-- This function will inline to get the best performance out of statically
-- known comparison functions. To avoid code duplication, create a wrapping
-- definition and reuse it as necessary.
--
sortIntArrayBy
  :: (Int -> Int -> Ordering)  -- ^ comparison
  -> MutableByteArray# s
  -> Int                       -- ^ offset in @Int#@s
  -> Int                       -- ^ length in @Int#@s
  -> ST s ()
sortIntArrayBy cmp =  -- Inline with 1 arg
  \ma# !off !len -> sortIntArrayBy' cmp (MIA ma#) off len
{-# INLINE sortIntArrayBy #-}

sortIntArrayBy'
  :: (Int -> Int -> Ordering)
  -> MIA s
  -> Int
  -> Int
  -> ST s ()
sortIntArrayBy' _ !_ !_ len | len < 2 = pure ()
sortIntArrayBy' cmp ma off len = do
  -- See Note [Algorithm overview]

  !swp <- newI (len `shiftR` 1)
  !stk <- newI (lg len)

  let -- Merge [i1,i2) and [i2,i3)
      -- Precondition: i1 < i2 < i3
      merge !i1 !i2 !i3
        | i2-i1 <= i3-i2 = mergeCopyLeft1 i1 i2 i3
        | otherwise = mergeCopyRight1 i1 i2 i3

      mergeCopyLeft1 !i1 !i2 !i3 = readI ma i2 >>= skip i1
        where
          skip !i !y = do
            x <- readI ma i
            if y `lt` x
            then mergeCopyLeft2 i i2 i3
            else
              when (i < i2) $
                skip (i+1) y

      -- Precondition: i1 < i2 < i3, (ma!i2) `lt` (ma!i1)
      mergeCopyLeft2 !i1 !i2 !i3 = do
        copyI ma i1 swp 0 (i2-i1)
        readI ma i2 >>= writeI ma i1
        if i2+1 < i3
        then loop 0 (i2+1) (i1+1)
        else copyI swp 0 ma (i1+1) len1
        where
          !len1 = i2-i1
          loop !h !j !k = readI swp h >>= loop2 j k h
          loop2 j1 !k1 !h !_ | j1 >= i3 = copyI swp h ma k1 (len1-h)
          loop2 j1 k1 h x = do
            y <- readI ma j1
            if y `lt` x
            then do
              writeI ma k1 y
              loop2 (j1+1) (k1+1) h x
            else do
              writeI ma k1 x
              when (h+1 < len1) $
                loop (h+1) j1 (k1+1)

      mergeCopyRight1 !i1 !i2 !i3 = readI ma (i2-1) >>= skip (i3-1)
        where
          skip !j !x = do
            y <- readI ma j
            if y `lt` x
            then mergeCopyRight2 i1 i2 (j+1)
            else
              when (j >= i2) $
                skip (j-1) x

      -- Precondition: i1 < i2 < i3, (ma!(i3-1)) `lt` (ma!(i2-1))
      mergeCopyRight2 !i1 !i2 !i3 = do
        copyI ma i2 swp 0 (i3-i2)
        readI ma (i2-1) >>= writeI ma (i3-1)
        if i2-2 >= i1
        then loop (i2-2) (i3-i2-1) (i3-2)
        else copyI swp 0 ma i1 (i3-i2)
        where
          loop !h !j !k = readI swp j >>= loop2 h k j
          loop2 h1 !_ !j !_ | h1 < i1 = copyI swp 0 ma i1 (j+1)
          loop2 h1 k1 j y = do
            x <- readI ma h1
            if y `lt` x
            then do
              writeI ma k1 x
              loop2 (h1-1) (k1-1) j y
            else do
              writeI ma k1 y
              when (j > 0) $
                loop h1 (j-1) (k1-1)

  mergeStrategy merge getRun stk off end

  where
    lt !x !y = case cmp x y of LT -> True; _ -> False
    {-# INLINE lt #-}
    -- Note: Use lt instead of gt. Why? Because `compare` for types like Int and
    -- Word are defined in a way that needs one `<` op for LT but two (`<`,`==`)
    -- for GT.

    !end = off + len

    getRun = mkGetRun lt (readI ma) (writeI ma) (reverseI ma) end
{-# INLINE sortIntArrayBy' #-}

mkGetRun
  :: (a -> a -> Bool)        -- comparison
  -> (Int -> ST s a)         -- read
  -> (Int -> a -> ST s ())   -- write
  -> (Int -> Int -> ST s ()) -- reverse
  -> Int                     -- end
  -> (Int -> ST s Int)
mkGetRun lt rd wt rev !end = getRun
  where
    runAsc i | i >= end = pure i
    runAsc i = do
      x <- rd (i-1)
      y <- rd i
      if y `lt` x
      then pure i
      else runAsc (i+1)

    runDesc i | i >= end = pure i
    runDesc i = do
      x <- rd (i-1)
      y <- rd i
      if y `lt` x
      then runDesc (i+1)
      else pure i

    -- Insertion sort [i2,i3) into [i1,i3)
    -- Precondition: i1 < i2, i1 < i3
    insLoop !_ i2 i3 | i2 >= i3 = pure i2
    insLoop i1 i2 i3 = do
      x0 <- rd (i2-1)
      y <- rd i2
      when (y `lt` x0) $ do
        let ins j | j <= i1 = wt j y
            ins j = do
              x <- rd (j-1)
              if y `lt` x
              then wt j x *> ins (j-1)
              else wt j y
        wt i2 x0 *> ins (i2-1)
      insLoop i1 (i2+1) i3

    getRun i | i >= end || i+1 >= end = pure end
    getRun i = do
      x <- rd i
      y <- rd (i+1)
      !j <- if y `lt` x
        then do
          j <- runDesc (i+2)
          j <$ rev i (j-1)
        else runAsc (i+2)
      let k = i + minRunLen
          k' = if k <= 0 -- overflowed
               then end
               else min end k
      insLoop i j k'
{-# INLINE mkGetRun #-}

minRunLen :: Int
minRunLen = 8

mergeStrategy
  :: (Int -> Int -> Int -> ST s ()) -- merge
  -> (Int -> ST s Int)              -- get next run
  -> MIA s                          -- stack
  -> Int                            -- offset
  -> Int                            -- end
  -> ST s ()
mergeStrategy merge getRun !stk !off !end = getRun off >>= mergeRuns (-1) off
  where
    -- [i,j) is the last run. Runs before it are on the stack.
    mergeRuns !top !i j
      | j >= end = finish top i
      | otherwise = getRun j >>= popPush top i j

    -- Maintain stack invariants
    popPush !top !i2 !i3 !i4
      | not (badYZ i2 i3 i4) = do
          writeI stk (top+1) i2
          mergeRuns (top+1) i3 i4
      | top < 0 = do
          merge i2 i3 i4
          mergeRuns top i2 i4
      | otherwise = do
          i1 <- readI stk top
          if mergeWithLeft i1 i2 i3 i4
          then do
            merge i1 i2 i3
            popPush (top-1) i1 i3 i4
          else do
            merge i2 i3 i4
            popPush (top-1) i1 i2 i4

    finish top !_ | top < 0 = pure ()
    finish top j = do
      i <- readI stk top
      merge i j end
      finish (top-1) i
{-# INLINE mergeStrategy #-}

badYZ :: Int -> Int -> Int -> Bool
badYZ i1 i2 i3 = (i2-i1) `shiftR` 1 < (i3-i2)
{-# INLINE badYZ #-}

mergeWithLeft :: Int -> Int -> Int -> Int -> Bool
mergeWithLeft i1 i2 i3 i4 = (i2-i1) < (i4-i3)
{-# INLINE mergeWithLeft #-}

reverseA
  :: MA s a
  -> Int     -- ^ Start
  -> Int     -- ^ End (inclusive)
  -> ST s ()
reverseA !ma = loop
  where
    loop i j | i >= j = pure ()
    loop i j = do
      x <- readA ma i
      readA ma j >>= writeA ma i
      writeA ma j x
      loop (i+1) (j-1)

reverseI
  :: MIA s
  -> Int     -- ^ Start
  -> Int     -- ^ End (inclusive)
  -> ST s ()
reverseI !ma = loop
  where
    loop i j | i >= j = pure ()
    loop i j = do
      x <- readI ma i
      readI ma j >>= writeI ma i
      writeI ma j x
      loop (i+1) (j-1)

lg :: Int -> Int
lg 0 = 0
lg i = finiteBitSize i - 1 - countLeadingZeros i
{-# INLINE lg #-}

errorElement :: a
errorElement = error "errorElement"

--------------------

-- The boxed wrappers MA, MIA, and functions operating on them are for the
-- convenience of working in ST. All of it should get optimized away.

data MA s a = MA (MutableArray# s a)

newA :: Int -> a -> ST s (MA s a)
newA (I# n#) x = ST $ \s ->
  case newArray# n# x s of (# s1, ma# #) -> (# s1, MA ma# #)
{-# INLINE newA #-}

readA :: MA s a -> Int -> ST s a
readA (MA ma#) (I# i#) = ST $ readArray# ma# i#
{-# INLINE readA #-}

writeA :: MA s a -> Int -> a -> ST s ()
writeA (MA ma#) (I# i#) x = ST $ \s ->
  case writeArray# ma# i# x s of s1 -> (# s1, () #)
{-# INLINE writeA #-}

copyA :: MA s a -> Int -> MA s a -> Int -> Int -> ST s ()
copyA (MA src#) (I# srcOff#) (MA dst#) (I# dstOff#) (I# len#) = ST $ \s ->
  case copyMutableArray# src# srcOff# dst# dstOff# len# s of s1 -> (# s1, () #)
{-# INLINE copyA #-}

data MIA s = MIA (MutableByteArray# s)

newI :: Int -> ST s (MIA s)
newI (I# n#) = ST $ \s ->
  case newByteArray# (n# *# intSize# (# #)) s of (# s1, ma# #) -> (# s1, MIA ma# #)
{-# INLINE newI #-}

readI :: MIA s -> Int -> ST s Int
readI (MIA ma#) (I# i#) = ST $ \s ->
  case readIntArray# ma# i# s of (# s1, x# #) -> (# s1, I# x# #)
{-# INLINE readI #-}

writeI :: MIA s -> Int -> Int -> ST s ()
writeI (MIA ma#) (I# i#) (I# x#) = ST $ \s ->
  case writeIntArray# ma# i# x# s of s1 -> (# s1, () #)
{-# INLINE writeI #-}

copyI :: MIA s -> Int -> MIA s -> Int -> Int -> ST s ()
copyI (MIA src#) (I# srcOff#) (MIA dst#) (I# dstOff#) (I# len#) = ST $ \s ->
  case copyMutableByteArray#
         src#
         (srcOff# *# intSize# (# #))
         dst#
         (dstOff# *# intSize# (# #))
         (len# *# intSize# (# #))
         s of
    s1 -> (# s1, () #)
{-# INLINE copyI #-}

intSize# :: (# #) -> Int#
intSize# _ = case finiteBitSize (0 :: Int) `shiftR` 3 of I# wsz# -> wsz#

--------------------

-- Note [Algorithm overview]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- Find non-decreasing and decreasing runs. Decreasing runs are reversed in
-- place. If a run is shorter than minRunLen, extend it to minRunLen using
-- insertion sort. Maintain a stack of runs. As each run is found, add it to
-- the stack and maintain stack invariants according to the 2-merge strategy.
-- This involves merging adjacent runs. Merging two runs is done by copying the
-- smaller run to a swap array, then merging into the main array. Elements of
-- the smaller array that can stay in place are skipped and not copied. After
-- all runs are found, runs on the stack are merged to get the final sorted
-- array.

-- Note [First iteration]
-- ~~~~~~~~~~~~~~~~~~~~~~
-- In certain places, the first iteration of a loop is pulled out of the loop
-- when many elements need to be compared with one element. This is to make GHC
-- aware that if the comparison is strict, the one element can be evaluated and
-- perhaps unboxed for subsequent comparisons. This could also be achieved by
-- being strict in the element, but we want to allow the comparison function
-- to be potentially lazy.

-- Note [Integer overflows]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
-- We (reasonably) assume that end=off+len fits in an Int.
-- If that holds, this implementation /should/ work without encountering any
-- bugs due to overflow. But it is unclear how that can be tested without too
-- much trouble.
