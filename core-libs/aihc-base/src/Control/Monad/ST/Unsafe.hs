-- | The unsafe entry points of the strict 'ST' monad.
module Control.Monad.ST.Unsafe
  ( unsafeInterleaveST,
    unsafeDupableInterleaveST,
    unsafeIOToST,
    unsafeSTToIO,
  )
where

import GHC.IO (unsafeIOToST, unsafeSTToIO)
import GHC.ST (unsafeDupableInterleaveST, unsafeInterleaveST)
