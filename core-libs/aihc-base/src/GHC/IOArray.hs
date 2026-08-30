module GHC.IOArray
  ( IOArray (..),
    newIOArray,
    unsafeReadIOArray,
    unsafeWriteIOArray,
  )
where

import GHC.Arr (Ix, STArray, newSTArray, unsafeReadSTArray, unsafeWriteSTArray)
import GHC.Base (Functor (fmap))
import GHC.IO (IO, stToIO)
import GHC.Int (Int)
import GHC.Prim (RealWorld)

newtype IOArray i e = IOArray (STArray RealWorld i e)

newIOArray :: (Ix i) => (i, i) -> e -> IO (IOArray i e)
newIOArray bounds initial = stToIO (fmap IOArray (newSTArray bounds initial))

unsafeReadIOArray :: IOArray i e -> Int -> IO e
unsafeReadIOArray (IOArray array) index = stToIO (unsafeReadSTArray array index)

unsafeWriteIOArray :: IOArray i e -> Int -> e -> IO ()
unsafeWriteIOArray (IOArray array) index value = stToIO (unsafeWriteSTArray array index value)
