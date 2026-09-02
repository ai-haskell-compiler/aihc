module System.IO.Unsafe
  ( unsafePerformIO,
    unsafeDupablePerformIO,
    unsafeInterleaveIO,
  )
where

import GHC.IO.Unsafe (unsafeDupablePerformIO, unsafeInterleaveIO, unsafePerformIO)
