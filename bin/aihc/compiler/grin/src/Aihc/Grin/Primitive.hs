{-# LANGUAGE OverloadedStrings #-}

-- | Which primitives allocate.
--
-- A heap reservation may span any operation that leaves the reservation
-- alone. Most primitives do: arithmetic, comparisons, bit operations, the
-- accesses of memory that is already allocated, and the runtime calls that
-- only read or copy, such as the floating-point library functions and the
-- array copies. The primitives named here are the ones that make a new
-- object, so a reservation must not reach across them.
--
-- The allocation classification includes both movable and pinned objects.
-- Both forms consume a prior reservation in the shared heap budget.
--
-- 'Aihc.Native' records how each primitive is lowered, but sits above GRIN
-- and cannot be read from this side, so this list is a second statement of
-- what those tables imply. The test suite holds the two together: every
-- primitive that is handed the machine pointer, and so can reach
-- @aihc_gc_allocate@, has to appear here, and nothing may appear here that
-- is not a runtime call at all. Getting it wrong is quiet -- a primitive
-- that allocates outside a reservation aborts inside @aihc_gc_allocate@
-- rather than failing a lint -- which is why the list is checked rather
-- than trusted.
module Aihc.Grin.Primitive
  ( primitiveAllocates,
    allocatingPrimitives,
    primitiveHeapWords,
  )
where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

-- | Whether a call to the named primitive can allocate, and so ends the
-- heap reservation that reaches it.
primitiveAllocates :: Text -> Bool
primitiveAllocates name = name `Set.member` allocatingPrimitives

-- | Maximum heap slots consumed by each fixed-size primitive.
-- Heap slots have eight bytes on every target. C size assertions check
-- the record bounds in aihc_runtime_internal.h.
-- The caller reserves these slots. The primitive must not collect.
primitiveHeapWords :: Text -> Maybe Int
primitiveHeapWords name =
  lookup
    name
    [ ("stmWaitRequest#", 18),
      ("submitIORead#", 18),
      ("submitIOWrite#", 18),
      ("submitIOOpen#", 18),
      ("stmBegin#", 3),
      ("writeTVar#", 4),
      ("newDelayTVar#", 8),
      ("newPromptTag#", 1),
      ("makeStableName#", 4),
      ("newMutVar#", 3),
      ("fork#", 9),
      ("newMVar#", 9),
      ("readMVar#", 5),
      ("takeMVar#", 5),
      ("putMVar#", 5)
    ]

-- | The primitives whose lowering allocates.
allocatingPrimitives :: Set Text
allocatingPrimitives =
  Set.fromList
    ( concat
        [ arrayPrimitives,
          byteArrayPrimitives,
          referencePrimitives,
          transactionPrimitives,
          concurrencyPrimitives,
          controlPrimitives
        ]
    )

-- | The boxed arrays, whose elements are managed slots. The small-array
-- family shares their representation. Only the operations that make a new
-- array are here: a read, a write, a copy between two arrays, and the
-- unsafe freeze and thaw all keep the array they are given.
arrayPrimitives :: [Text]
arrayPrimitives =
  [ "newArray#",
    "cloneArray#",
    "cloneMutableArray#",
    "freezeArray#",
    "thawArray#",
    "newSmallArray#",
    "cloneSmallArray#",
    "cloneSmallMutableArray#",
    "freezeSmallArray#",
    "thawSmallArray#",
    "resizeSmallMutableArray#"
  ]

-- | Byte-array allocation and resize use dynamic reservations.
-- Shrink remains a reservation boundary, but it does not allocate.
byteArrayPrimitives :: [Text]
byteArrayPrimitives =
  [ "newByteArray#",
    "newPinnedByteArray#",
    "newAlignedPinnedByteArray#",
    "resizeMutableByteArray#",
    "shrinkMutableByteArray#"
  ]

-- | A mutable reference is a boxed array of one element, and a stable name
-- is an object of its own. Reading and writing a reference is a load and a
-- store, so neither is here.
referencePrimitives :: [Text]
referencePrimitives =
  [ "newMutVar#",
    "makeStableName#"
  ]

-- | The transaction log grows as a transaction runs, so every operation
-- that records an entry in it belongs here, not only the ones that make a
-- variable.
transactionPrimitives :: [Text]
transactionPrimitives =
  [ "newTVar#",
    "readTVar#",
    "readTVarIO#",
    "writeTVar#",
    "newDelayTVar#",
    "stmBegin#",
    "stmCommit#",
    "stmAbort#",
    "stmActive#",
    "stmWaitRequest#",
    "stmWaitResult#"
  ]

-- | The MVar operations and the scheduler. The ones that can block hand
-- over the continuation, which suspends the thread outright; the ones that
-- never block still allocate.
concurrencyPrimitives :: [Text]
concurrencyPrimitives =
  [ "fork#",
    -- myThreadId# only reads a field of the machine. It is here because it
    -- takes the machine, which is the rule this list must agree with.
    "myThreadId#",
    "yield#",
    "awaitIO#",
    "submitIORead#",
    "submitIOWrite#",
    "submitIOOpen#",
    "newMVar#",
    "readMVar#",
    "takeMVar#",
    "putMVar#",
    "tryTakeMVar#",
    "tryPutMVar#",
    -- The delimited-continuation runtime calls. A prompt tag is a heap
    -- object, a capture allocates the record of the captured frames, and a
    -- resume copies them.
    "newPromptTag#",
    "aihcControl0#",
    "aihcResume#"
  ]

-- | The primitives the lowering compiles from their argument expressions
-- rather than as a call, so that none of them reaches GRIN as a primitive
-- call today. They are named here because the classification defaults the
-- other way: a control transfer that did arrive as a call must not be
-- mistaken for straight-line code.
controlPrimitives :: [Text]
controlPrimitives =
  [ "aihcExit#",
    "unsafeCoerce#",
    "raise#",
    "raiseIO#",
    "catch#",
    "runRW#",
    "keepAlive#",
    "seq#",
    -- The CPS conversion turns prompt# into a prompt frame, so like catch#
    -- it never reaches a backend as a call.
    "prompt#"
  ]
