{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | This module provides a simple, efficient supply of integers using atomic fetch-and-add.
--
-- To use this module, first create an @IntSupply@. This is often done once at the top level of an application, in
-- global scope.
--
-- > import IntSupply (IntSupply)
-- > import IntSupply qualified
-- > import System.IO.Unsafe (unsafePerformIO)
-- >
-- > myIntSupply :: IntSupply
-- > myIntSupply = unsafePerformIO IntSupply.new
-- > {-# NOINLINE myIntSupply #-}
--
-- Next, call @IntSupply.next@ on the supply, which will return 0, then 1, and so on.
--
-- > > IntSupply.next myIntSupply
-- > 0
-- > > IntSupply.next myIntSupply
-- > 1
--
-- If desired, you can reset the count to 0.
--
-- > > IntSupply.reset myIntSupply
-- > > IntSupply.next myIntSupply
-- > 0
--
-- On a 64-bit machine, for many applications, these integers can be treated as effectively unique: even if
-- 1,000,000,000 integers were generated per second, it would still take over 580 years to wrap around.
--
-- On a 32-bit machine, more care must be taken, of course: even if only 1,000 integers were generated per second, it
-- would only take 50 days to wrap around.
module IntSupply
  ( IntSupply,
    new,
    next,
    reset,
  )
where

import Data.Bits (finiteBitSize)
import GHC.Base
  ( IO (IO),
    Int (I#),
    MutableByteArray#,
    RealWorld,
    atomicWriteIntArray#,
    fetchAddIntArray#,
    newByteArray#,
    writeIntArray#,
  )

-- | A thread-safe supply of integers.
data IntSupply
  = IntSupply (MutableByteArray# RealWorld)

-- | Create a supply of integers.
new :: IO IntSupply
new =
  IO \s0 ->
    case newByteArray# size s0 of
      (# s1, supply #) ->
        (# writeIntArray# supply 0# 0# s1, IntSupply supply #)
  where
    !(I# size) =
      finiteBitSize (undefined :: Int) `div` 8
{-# INLINEABLE new #-}

-- | Get the next integer from a supply of integers.
next :: IntSupply -> IO Int
next (IntSupply supply) =
  IO \s0 ->
    case fetchAddIntArray# supply 0# 1# s0 of
      (# s1, n #) -> (# s1, I# n #)
{-# INLINEABLE next #-}

-- | Reset a supply of integers to 0.
reset :: IntSupply -> IO ()
reset (IntSupply arr#) =
  IO \s0 ->
    (# atomicWriteIntArray# arr# 0# 0# s0, () #)
{-# INLINEABLE reset #-}
