module Data.STRef
  ( STRef,
    newSTRef,
    readSTRef,
    writeSTRef,
    modifySTRef,
    modifySTRef',
  )
where

import GHC.ST (ST)
import GHC.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Prelude (seq, (>>=))

-- | Mutate the contents of an 'STRef' without forcing the new value.
modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef reference transform =
  readSTRef reference >>= \value -> writeSTRef reference (transform value)

-- | Mutate the contents of an 'STRef', forcing the new value to weak head
-- normal form before storing it.
modifySTRef' :: STRef s a -> (a -> a) -> ST s ()
modifySTRef' reference transform =
  readSTRef reference >>= \value ->
    let updated = transform value
     in updated `seq` writeSTRef reference updated
