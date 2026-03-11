{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
-- | An imperative integer set written in Haskell.
--
-- See \"/Making Haskell as fast as C: Imperative programming in Haskell/\" for a more detailed discussion, <https://deliquus.com/posts/2018-07-30-imperative-programming-in-haskell.html>.
module Data.IntSet.Bounded.Imperative (
  -- * Types
  IntSet
, IOIntSet
, intSetMinBound
, intSetMaxBound

  -- * Construction
, empty

  -- * Insertion
, insert

  -- * Query
, member
, notMember

  -- * Deletion
, delete
) where

import Control.DeepSeq (deepseq)
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.ST (ST)

import Data.Bits
import Data.Primitive.ByteArray
import Data.Primitive.MutVar
import Data.Word (Word64)

-- | A strict bounded integer set.
--
-- The set is very efficient when accessing elements within the bounds
-- of the set. It uses a regular list to hold numbers outside of this
-- range.
--
-- The type parameter @s@ is determined by the monad the data structure
-- lives in.
data IntSet s = IntSet {
    intSetMinBound#  :: {-# UNPACK #-} !Word64
  , intSetMaxBound#  :: {-# UNPACK #-} !Word64
  , intSetInBounds#  :: {-# UNPACK #-} !(MutableByteArray s)
  , intSetOutBounds# :: {-# UNPACK #-} !(MutVar s [Word64])
  }

-- | An 'IntSet' inside the 'IO' monad.
type IOIntSet = IntSet (PrimState IO)

-- | Get the minimum efficient bound of the integer set.
intSetMinBound :: IntSet s -> Word64
intSetMinBound = intSetMinBound#
{-# INLINE intSetMinBound #-}

-- | Get the maximum efficient bound of the integer set.
intSetMaxBound :: IntSet s -> Word64
intSetMaxBound = intSetMaxBound#
{-# INLINE intSetMaxBound #-}

-- | Construct an empty integer set.
empty :: PrimMonad m
      => Word64           -- ^ Minimum bound of the integer set
      -> Word64           -- ^ Maximum bound of the integer set
      -> m (IntSet (PrimState m))
empty !minB !maxB = do
    let !numInBounds = (maxB - minB) `div` 8 + 1
    set <- newByteArray (fromIntegral numInBounds)
    fillByteArray set 0 (fromIntegral numInBounds) 0
    outBounds <- newMutVar []
    return $! IntSet {
        intSetMinBound#  = minB
      , intSetMaxBound#  = maxB
      , intSetInBounds#  = set
      , intSetOutBounds# = outBounds
      }
{-# SPECIALIZE empty :: Word64 -> Word64 -> IO (IntSet (PrimState IO)) #-}
{-# SPECIALIZE empty :: Word64 -> Word64 -> ST s (IntSet s) #-}

-- | Insert the integer in a set.
insert :: PrimMonad m
       => IntSet (PrimState m)
       -> Word64
       -> m ()
insert !set !n = do
    if n >= intSetMinBound# set && n <= intSetMaxBound# set then do
        let !n'   = n - intSetMinBound# set
        let !o    = fromIntegral $ n' `shiftR` 6
        let !i    = fromIntegral $ n' .&. 63
        let !mask = (1 :: Word64) `shiftL` i
        b <- readByteArray (intSetInBounds# set) o
        let !b' = b .|. mask
        writeByteArray (intSetInBounds# set) o b'

    else do
        ns <- readMutVar (intSetOutBounds# set)
        let !ns' = if n `elem` ns then ns else (n:ns)
        writeMutVar (intSetOutBounds# set) ns'
{-# SPECIALIZE insert :: IntSet (PrimState IO) -> Word64 -> IO () #-}
{-# SPECIALIZE insert :: IntSet s -> Word64 -> ST s () #-}

-- | Delete the integer from the set.
delete :: PrimMonad m
       => IntSet (PrimState m)
       -> Word64
       -> m ()
delete !set !n = do
    if n >= intSetMinBound# set && n <= intSetMaxBound# set then do
        let !n'   = n - intSetMinBound# set
        let !o    = fromIntegral $ n' `shiftR` 6
        let !i    = fromIntegral $ n' .&. 63
        let !mask = (1 :: Word64) `shiftL` i
        b <- readByteArray (intSetInBounds# set) o
        let !b' = b .&. (complement mask)
        writeByteArray (intSetInBounds# set) o b'

    else do
        ns <- readMutVar (intSetOutBounds# set)
        let ns' = filter (/= n) ns
        ns' `deepseq` writeMutVar (intSetOutBounds# set) ns'
{-# SPECIALIZE delete :: IntSet (PrimState IO) -> Word64 -> IO () #-}
{-# SPECIALIZE delete :: IntSet s -> Word64 -> ST s () #-}

-- | Is the integer in the set?
member :: PrimMonad m
       => IntSet (PrimState m)
       -> Word64
       -> m Bool
member !set !n = do
    if n >= intSetMinBound# set && n <= intSetMaxBound# set then do
        let !n'   = n - intSetMinBound# set
        let !o    = fromIntegral $ n' `shiftR` 6
        let !i    = fromIntegral $ n' .&. 63
        let !mask = (1 :: Word64) `shiftL` i
        b <- readByteArray (intSetInBounds# set) o
        return $! (b .&. mask) /= 0

    else do
        ns <- readMutVar (intSetOutBounds# set)
        return $! n `elem` ns
{-# SPECIALIZE member :: IntSet (PrimState IO) -> Word64 -> IO Bool #-}
{-# SPECIALIZE member :: IntSet s -> Word64 -> ST s Bool #-}

-- | Is the integer not in the set?
notMember :: PrimMonad m
          => IntSet (PrimState m)
          -> Word64
          -> m Bool
notMember !set !n = not <$> member set n
{-# INLINE notMember #-}
{-# SPECIALIZE notMember :: IntSet (PrimState IO) -> Word64 -> IO Bool #-}
{-# SPECIALIZE notMember :: IntSet s -> Word64 -> ST s Bool #-}
