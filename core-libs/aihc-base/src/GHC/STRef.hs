{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.STRef
  ( STRef (..),
    newSTRef,
    readSTRef,
    writeSTRef,
  )
where

import Data.Bool (Bool (..), not)
import GHC.Internal.Classes (Eq (..))
import GHC.Prim (MutVar#, newMutVar#, readMutVar#, sameMutVar#, writeMutVar#)
import GHC.ST (ST (..))

-- | A mutable variable in state thread @s@ containing a value of type @a@.
data STRef s a = STRef (MutVar# s a)

-- | Build a new 'STRef' in the current state thread.
newSTRef :: a -> ST s (STRef s a)
newSTRef initial =
  ST
    ( \state ->
        case newMutVar# initial state of
          (# nextState, reference #) -> (# nextState, STRef reference #)
    )

-- | Read the value of an 'STRef'.
readSTRef :: STRef s a -> ST s a
readSTRef (STRef reference) = ST (readMutVar# reference)

-- | Write a new value into an 'STRef'.
writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef (STRef reference) value =
  ST
    ( \state ->
        case writeMutVar# reference value state of
          nextState -> (# nextState, () #)
    )

-- Pointer equality, matching @base@.
instance Eq (STRef s a) where
  STRef left == STRef right =
    case sameMutVar# left right of
      0# -> False
      _ -> True

  left /= right = not (left == right)
