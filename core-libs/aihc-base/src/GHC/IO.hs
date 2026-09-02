{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.IO
  ( module GHC.Prim.IO,
    unIO,
    ioToST,
    unsafePerformIO,
    unsafeDupablePerformIO,
    unsafeInterleaveIO,
    unsafeDupableInterleaveIO,
  )
where

import GHC.IO.Unsafe (unsafeDupableInterleaveIO, unsafeDupablePerformIO, unsafeInterleaveIO, unsafePerformIO)
import GHC.Prim (RealWorld, State#)
import GHC.Prim.IO

unIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
unIO (IO action) = action

ioToST :: IO a -> ST RealWorld a
ioToST (IO action) = ST action
