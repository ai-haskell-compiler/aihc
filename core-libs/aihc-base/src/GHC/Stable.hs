{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Stable pointers: references to Haskell values that survive garbage
-- collection and that foreign code can hold.
--
-- A stable pointer here is an index into a table this module keeps, offset
-- by one so that no stable pointer is null. The table is an ordinary
-- Haskell value, so the collector follows what it holds, and 'freeStablePtr'
-- drops the entry and puts its index on a free list.
--
-- GHC instead hands out an address into a table of the runtime, which
-- @hs_deref_stable_ptr@ reads. This runtime has no such table, so foreign
-- code can carry one of these pointers and give it back, but it cannot
-- dereference one itself.
module GHC.Stable
  ( StablePtr (..),
    newStablePtr,
    deRefStablePtr,
    freeStablePtr,
    castStablePtrToPtr,
    castPtrToStablePtr,
  )
where

import Data.Bool ((||))
import GHC.Base (Monad (..))
import GHC.Err (errorWithoutStackTrace)
import GHC.IO (IO (..), mask_, onException, unsafePerformIO)
import GHC.Int (Int (..))
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.MVar (MVar, newMVar, putMVar, takeMVar)
import GHC.Num (Num (..))
import GHC.Prim (Addr#, MutableArray#, RealWorld, StablePtr#, addr2Int#, copyMutableArray#, int2Addr#, newArray#, readArray#, unsafeCoerce#, writeArray#)
import GHC.Ptr (Ptr (..))
import GHC.Types (Bool (..))

-- | A reference the collector follows, which foreign code may hold.
data StablePtr a = StablePtr (StablePtr# a)

-- | A value of any type, as the table stores it. Nothing reads one back
-- except through the entry that put it there, which knows its type.
data Referent

-- | The table's slots, the head of its free list and how many slots it has.
--
-- The head is @-1@ when every slot is taken; the next allocation grows the
-- table.
data StableTable = StableTable StableSlots Int Int

-- | The slots themselves. An unlifted array cannot be the result of an 'IO'
-- action, so it travels in this box.
data StableSlots = StableSlots (MutableArray# RealWorld StableEntry)

-- | A slot: the next free slot, or the value the stable pointer names.
data StableEntry = StableFree Int | StableUsed Referent

-- | The table of every live stable pointer.
--
-- This is the one piece of state the stable pointers of a process share, so
-- it is a top-level 'MVar', as the standard input and output handles are.
stableTable :: MVar StableTable
stableTable = unsafePerformIO (newEmptyTable >>= newMVar)
{-# NOINLINE stableTable #-}

-- | A table with no slots. The first allocation grows it.
newEmptyTable :: IO StableTable
newEmptyTable = do
  slots <- newEntries 0
  return (StableTable slots (negate 1) 0)

-- | A stable pointer to the value, which keeps the value alive until
-- 'freeStablePtr'.
newStablePtr :: a -> IO (StablePtr a)
newStablePtr value =
  mask_
    ( do
        table <- takeMVar stableTable
        grown <- onException (ensureFreeSlot table) (putMVar stableTable table)
        case grown of
          StableTable slots free capacity -> do
            following <- readEntry slots free
            case following of
              StableUsed _ -> errorWithoutStackTrace "GHC.Stable: the free list reached a slot in use"
              StableFree next -> do
                writeEntry slots free (StableUsed (unsafeCoerce# value))
                putMVar stableTable (StableTable slots next capacity)
                return (stablePtrOfIndex free)
    )

-- | The value the stable pointer names.
deRefStablePtr :: StablePtr a -> IO a
deRefStablePtr pointer =
  mask_
    ( do
        table <- takeMVar stableTable
        case table of
          StableTable slots _ capacity -> do
            let index = indexOfStablePtr pointer
            entry <- onException (readValidEntry slots capacity index) (putMVar stableTable table)
            putMVar stableTable table
            return (unsafeCoerce# entry)
    )

-- | Drop the stable pointer, so that its value may be collected.
--
-- Freeing a pointer twice is what GHC calls undefined behaviour; here the
-- second free puts the slot on the free list a second time, which the next
-- allocation reports rather than handing the slot out twice.
freeStablePtr :: StablePtr a -> IO ()
freeStablePtr pointer =
  mask_
    ( do
        table <- takeMVar stableTable
        case table of
          StableTable slots free capacity -> do
            let index = indexOfStablePtr pointer
            _ <- onException (readValidEntry slots capacity index) (putMVar stableTable table)
            writeEntry slots index (StableFree free)
            putMVar stableTable (StableTable slots index capacity)
    )

-- | The address a foreign call carries the stable pointer as.
castStablePtrToPtr :: StablePtr a -> Ptr ()
castStablePtrToPtr (StablePtr address) = Ptr (stablePtrAddress address)

-- | The stable pointer that 'castStablePtrToPtr' gave the address of.
castPtrToStablePtr :: Ptr () -> StablePtr a
castPtrToStablePtr (Ptr address) = StablePtr (addressStablePtr address)

-- | A table with at least one free slot, growing the table when it has none.
--
-- The table doubles, and an empty one starts at eight slots, so that a
-- program making many stable pointers copies the slots a number of times
-- that grows with the logarithm of how many it makes.
ensureFreeSlot :: StableTable -> IO StableTable
ensureFreeSlot table =
  case table of
    StableTable slots free capacity ->
      case free >= 0 of
        True -> return table
        False -> do
          let grown = case capacity == 0 of
                True -> 8
                False -> capacity + capacity
          replacement <- newEntries grown
          copyEntries slots replacement capacity
          linkFreeSlots replacement capacity grown
          return (StableTable replacement capacity grown)

-- | Put every slot from the first index onwards on the free list, in order,
-- so that the slot at that index is the head.
linkFreeSlots :: StableSlots -> Int -> Int -> IO ()
linkFreeSlots slots index limit =
  case index >= limit of
    True -> return ()
    False -> do
      let next = index + 1
      writeEntry
        slots
        index
        ( case next == limit of
            True -> StableFree (negate 1)
            False -> StableFree next
        )
      linkFreeSlots slots next limit

-- | The entry at an index that a live stable pointer must name.
readValidEntry :: StableSlots -> Int -> Int -> IO Referent
readValidEntry slots capacity index =
  case index < 0 || index >= capacity of
    True -> errorWithoutStackTrace "GHC.Stable: the stable pointer is not one this program made"
    False -> do
      entry <- readEntry slots index
      case entry of
        StableFree _ -> errorWithoutStackTrace "GHC.Stable: the stable pointer has already been freed"
        StableUsed value -> return value

-- | A table's index of a stable pointer, undoing the offset that keeps a
-- stable pointer from being null.
indexOfStablePtr :: StablePtr a -> Int
indexOfStablePtr (StablePtr address) = I# (addr2Int# (stablePtrAddress address)) - 1

-- | The stable pointer for a table index.
stablePtrOfIndex :: Int -> StablePtr a
stablePtrOfIndex index =
  case index + 1 of
    I# offset -> StablePtr (addressStablePtr (int2Addr# offset))

-- | A stable pointer and an address have the same representation, so the two
-- conversions below are the identity on the machine.
stablePtrAddress :: StablePtr# a -> Addr#
stablePtrAddress = unsafeCoerce#

addressStablePtr :: Addr# -> StablePtr# a
addressStablePtr = unsafeCoerce#

newEntries :: Int -> IO StableSlots
newEntries (I# size) =
  IO
    ( \state ->
        case newArray# size (StableFree (negate 1)) state of
          (# nextState, slots #) -> (# nextState, StableSlots slots #)
    )

readEntry :: StableSlots -> Int -> IO StableEntry
readEntry (StableSlots slots) (I# index) =
  IO
    ( \state ->
        case readArray# slots index state of
          (# nextState, entry #) -> (# nextState, entry #)
    )

writeEntry :: StableSlots -> Int -> StableEntry -> IO ()
writeEntry (StableSlots slots) (I# index) entry =
  IO
    ( \state ->
        case writeArray# slots index entry state of
          nextState -> (# nextState, () #)
    )

copyEntries :: StableSlots -> StableSlots -> Int -> IO ()
copyEntries (StableSlots source) (StableSlots destination) (I# count) =
  IO
    ( \state ->
        case copyMutableArray# source 0# destination 0# count state of
          nextState -> (# nextState, () #)
    )
