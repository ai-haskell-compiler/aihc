{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}

module GHC.Exts.Heap.Closures
  ( -- * Closures
    Closure,
    GenClosure (..),
    PrimType (..),
    WhatNext (..),
    WhyBlocked (..),
    TsoFlags (..),
    allClosures,
    closureSize,

    -- * Stack
    StgStackClosure,
    GenStgStackClosure (..),
    StackFrame,
    GenStackFrame (..),
    StackField,
    GenStackField (..),

    -- * Boxes
    Box (..),
    areBoxesEqual,
    asBox,
  )
where

import Data.Foldable (toList)
import Data.Int
import Data.Word
import GHC.Exts
import GHC.Exts.Heap.InfoTable
import GHC.Exts.Heap.ProfInfo.Types
import GHC.Exts.Heap.Unsupported
import GHC.Generics
import Prelude

------------------------------------------------------------------------
-- Boxes

-- | An arbitrary Haskell value in a safe Box. The point is that even
-- unevaluated thunks can safely be moved around inside the Box, and when
-- required, e.g. in 'getBoxedClosureData', the function knows how far it has
-- to evaluate the argument.
data Box = Box Any

instance Show Box where
  -- From libraries/base/GHC/Ptr.lhs
  -- A heap address is not available in AIHC.
  showsPrec _ _ _ = unsupported "Show Box"

-- | This takes an arbitrary value and puts it into a box.
--  Note that calls like
--
--  > asBox (head list)
--
--  will put the thunk \"head list\" into the box, /not/ the element at the head
--  of the list. For that, use careful case expressions:
--
--  > case list of x:_ -> asBox x
asBox :: a -> Box
asBox x = Box (unsafeCoerce# x)

-- | Boxes can be compared, but this is not pure, as different heap objects can,
-- after garbage collection, become the same object.
areBoxesEqual :: Box -> Box -> IO Bool
areBoxesEqual (Box a) (Box b) = case reallyUnsafePtrEquality# a b of
  0# -> pure False
  _ -> pure True

------------------------------------------------------------------------
-- Closures
type Closure = GenClosure Box

-- | This is the representation of a Haskell value on the heap. It reflects
-- <https://gitlab.haskell.org/ghc/ghc/blob/master/rts/include/rts/storage/Closures.h>
--
-- The data type is parametrized by `b`: the type to store references in.
-- Usually this is a 'Box' with the type synonym 'Closure'.
--
-- All Heap objects have the same basic layout. A header containing a pointer to
-- the info table and a payload with various fields. The @info@ field below
-- always refers to the info table pointed to by the header. The remaining
-- fields are the payload.
--
-- See
-- <https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/heap-objects>
-- for more information.
data GenClosure b
  = -- | A data constructor
    ConstrClosure
      { info :: !StgInfoTable,
        -- | Pointer arguments
        ptrArgs :: ![b],
        -- | Non-pointer arguments
        dataArgs :: ![Word],
        -- | Package name
        pkg :: !String,
        -- | Module name
        modl :: !String,
        -- | Constructor name
        name :: !String
      }
  | -- | A function
    FunClosure
      { info :: !StgInfoTable,
        -- | Pointer arguments
        ptrArgs :: ![b],
        -- | Non-pointer arguments
        dataArgs :: ![Word]
      }
  | -- | A thunk, an expression not obviously in head normal form
    ThunkClosure
      { info :: !StgInfoTable,
        -- | Pointer arguments
        ptrArgs :: ![b],
        -- | Non-pointer arguments
        dataArgs :: ![Word]
      }
  | -- | A thunk which performs a simple selection operation
    SelectorClosure
      { info :: !StgInfoTable,
        -- | Pointer to the object being
        --   selected from
        selectee :: !b
      }
  | -- | An unsaturated function application
    PAPClosure
      { info :: !StgInfoTable,
        -- | Arity of the partial application
        arity :: !HalfWord,
        -- | Size of the payload in words
        n_args :: !HalfWord,
        -- | Pointer to a 'FunClosure'
        fun :: !b,
        -- | Sequence of already applied
        --   arguments
        payload :: ![b]
      }
  | -- In GHCi, if Linker.h would allow a reverse lookup, we could for exported
    -- functions fun actually find the name here.
    -- At least the other direction works via "lookupSymbol
    -- base_GHCziBase_zpzp_closure" and yields the same address (up to tags)

    -- | A function application
    APClosure
      { info :: !StgInfoTable,
        -- | Always 0
        arity :: !HalfWord,
        -- | Size of payload in words
        n_args :: !HalfWord,
        -- | Pointer to a 'FunClosure'
        fun :: !b,
        -- | Sequence of already applied
        --   arguments
        payload :: ![b]
      }
  | -- | A suspended thunk evaluation
    APStackClosure
      { info :: !StgInfoTable,
        -- | Function closure
        fun :: !b,
        -- | Stack right before suspension
        payload :: ![b]
      }
  | -- | A pointer to another closure, introduced when a thunk is updated
    -- to point at its value
    IndClosure
      { info :: !StgInfoTable,
        -- | Target closure
        indirectee :: !b
      }
  | -- | A byte-code object (BCO) which can be interpreted by GHC's byte-code
    -- interpreter (e.g. as used by GHCi)
    BCOClosure
      { info :: !StgInfoTable,
        -- | A pointer to an ArrWords
        --   of instructions
        instrs :: !b,
        -- | A pointer to an ArrWords
        --   of literals
        literals :: !b,
        -- | A pointer to an ArrWords
        --   of byte code objects
        bcoptrs :: !b,
        -- | The arity of this BCO
        arity :: !HalfWord,
        -- | The size of this BCO in words
        size :: !HalfWord,
        -- | An StgLargeBitmap describing the
        --   pointerhood of its args/free vars
        bitmap :: ![Word]
      }
  | -- | A thunk under evaluation by another thread
    BlackholeClosure
      { info :: !StgInfoTable,
        -- | The target closure
        indirectee :: !b
      }
  | -- | A @ByteArray#@
    ArrWordsClosure
      { info :: !StgInfoTable,
        -- | Size of array in bytes
        bytes :: !Word,
        -- | Array payload
        arrWords :: ![Word]
      }
  | -- | A @MutableByteArray#@
    MutArrClosure
      { info :: !StgInfoTable,
        -- | Number of pointers
        mccPtrs :: !Word,
        -- | ?? Closures.h vs ClosureMacros.h
        mccSize :: !Word,
        -- | Array payload
        -- Card table ignored
        mccPayload :: ![b]
      }
  | -- | A @SmallMutableArray#@
    --
    -- @since 8.10.1
    SmallMutArrClosure
      { info :: !StgInfoTable,
        -- | Number of pointers
        mccPtrs :: !Word,
        -- | Array payload
        mccPayload :: ![b]
      }
  | -- | An @MVar#@, with a queue of thread state objects blocking on them
    MVarClosure
      { info :: !StgInfoTable,
        -- | Pointer to head of queue
        queueHead :: !b,
        -- | Pointer to tail of queue
        queueTail :: !b,
        -- | Pointer to closure
        value :: !b
      }
  | -- | An @IOPort#@, with a queue of thread state objects blocking on them
    IOPortClosure
      { info :: !StgInfoTable,
        -- | Pointer to head of queue
        queueHead :: !b,
        -- | Pointer to tail of queue
        queueTail :: !b,
        -- | Pointer to closure
        value :: !b
      }
  | -- | A @MutVar#@
    MutVarClosure
      { info :: !StgInfoTable,
        -- | Pointer to contents
        var :: !b
      }
  | -- | An STM blocking queue.
    BlockingQueueClosure
      { info :: !StgInfoTable,
        -- | ?? Here so it looks like an IND
        link :: !b,
        -- | The blackhole closure
        blackHole :: !b,
        -- | The owning thread state object
        owner :: !b,
        -- | ??
        queue :: !b
      }
  | WeakClosure
      { info :: !StgInfoTable,
        cfinalizers :: !b,
        key :: !b,
        value :: !b,
        finalizer :: !b,
        -- | next weak pointer for the capability
        weakLink :: !(Maybe b)
      }
  | -- | Representation of StgTSO: A Thread State Object. The values for
    -- 'what_next', 'why_blocked' and 'flags' are defined in @Constants.h@.
    TSOClosure
      { info :: !StgInfoTable,
        -- pointers
        link :: !b,
        global_link :: !b,
        -- | stackobj from StgTSO
        tsoStack :: !b,
        trec :: !b,
        blocked_exceptions :: !b,
        bq :: !b,
        thread_label :: !(Maybe b),
        -- values
        what_next :: !WhatNext,
        why_blocked :: !WhyBlocked,
        flags :: ![TsoFlags],
        threadId :: !Word64,
        saved_errno :: !Word32,
        -- | non-zero => dirty
        tso_dirty :: !Word32,
        alloc_limit :: !Int64,
        tot_stack_size :: !Word32,
        prof :: !(Maybe StgTSOProfInfo)
      }
  | -- | Representation of StgStack: The 'tsoStack ' of a 'TSOClosure'.
    StackClosure
      { info :: !StgInfoTable,
        -- | stack size in *words*
        stack_size :: !Word32,
        -- | non-zero => dirty
        stack_dirty :: !Word8,
        stack_marking :: !Word8
      }
  | ------------------------------------------------------------
    -- Unboxed unlifted closures

    -- | Primitive Int
    IntClosure
      { ptipe :: PrimType,
        intVal :: !Int
      }
  | -- | Primitive Word
    WordClosure
      { ptipe :: PrimType,
        wordVal :: !Word
      }
  | -- | Primitive Int64
    Int64Closure
      { ptipe :: PrimType,
        int64Val :: !Int64
      }
  | -- | Primitive Word64
    Word64Closure
      { ptipe :: PrimType,
        word64Val :: !Word64
      }
  | -- | Primitive Addr
    AddrClosure
      { ptipe :: PrimType,
        addrVal :: !(Ptr ())
      }
  | -- | Primitive Float
    FloatClosure
      { ptipe :: PrimType,
        floatVal :: !Float
      }
  | -- | Primitive Double
    DoubleClosure
      { ptipe :: PrimType,
        doubleVal :: !Double
      }
  | -----------------------------------------------------------
    -- Anything else

    -- | Another kind of closure
    OtherClosure
      { info :: !StgInfoTable,
        hvalues :: ![b],
        rawWords :: ![Word]
      }
  | UnsupportedClosure
      { info :: !StgInfoTable
      }
  | -- | A primitive word from a bitmap encoded stack frame payload
    --
    -- The type itself cannot be restored (i.e. it might represent a Word8#
    -- or an Int#).
    UnknownTypeWordSizedPrimitive
      {wordVal :: !Word}
  deriving (Show, Generic, Functor, Foldable, Traversable)

type StgStackClosure = GenStgStackClosure Box

-- | A decoded @StgStack@ with `StackFrame`s
--
-- Stack related data structures (`GenStgStackClosure`, `GenStackField`,
-- `GenStackFrame`) are defined separately from `GenClosure` as their related
-- functions are very different. Though, both are closures in the sense of RTS
-- structures, their decoding logic differs: While it's safe to keep a reference
-- to a heap closure, the garbage collector does not update references to stack
-- located closures.
--
-- Additionally, stack frames don't appear outside of the stack. Thus, keeping
-- `GenStackFrame` and `GenClosure` separated, makes these types more precise
-- (in the sense what values to expect.)
data GenStgStackClosure b = GenStgStackClosure
  { ssc_info :: !StgInfoTable,
    -- | stack size in *words*
    ssc_stack_size :: !Word32,
    ssc_stack :: ![GenStackFrame b]
  }
  deriving (Foldable, Functor, Generic, Show, Traversable)

type StackField = GenStackField Box

-- | Bitmap-encoded payload on the stack
data GenStackField b
  = -- | A non-pointer field
    StackWord !Word
  | -- | A pointer field
    StackBox !b
  deriving (Foldable, Functor, Generic, Show, Traversable)

type StackFrame = GenStackFrame Box

-- | A single stack frame
data GenStackFrame b
  = UpdateFrame
      { info_tbl :: !StgInfoTable,
        updatee :: !b
      }
  | CatchFrame
      { info_tbl :: !StgInfoTable,
        handler :: !b
      }
  | CatchStmFrame
      { info_tbl :: !StgInfoTable,
        catchFrameCode :: !b,
        handler :: !b
      }
  | CatchRetryFrame
      { info_tbl :: !StgInfoTable,
        running_alt_code :: !Word,
        first_code :: !b,
        alt_code :: !b
      }
  | AtomicallyFrame
      { info_tbl :: !StgInfoTable,
        atomicallyFrameCode :: !b,
        result :: !b
      }
  | UnderflowFrame
      { info_tbl :: !StgInfoTable,
        nextChunk :: !(GenStgStackClosure b)
      }
  | StopFrame
      {info_tbl :: !StgInfoTable}
  | RetSmall
      { info_tbl :: !StgInfoTable,
        stack_payload :: ![GenStackField b]
      }
  | RetBig
      { info_tbl :: !StgInfoTable,
        stack_payload :: ![GenStackField b]
      }
  | RetFun
      { info_tbl :: !StgInfoTable,
        retFunSize :: !Word,
        retFunFun :: !b,
        retFunPayload :: ![GenStackField b]
      }
  | RetBCO
      { info_tbl :: !StgInfoTable,
        -- | always a BCOClosure
        bco :: !b,
        bcoArgs :: ![GenStackField b]
      }
  deriving (Foldable, Functor, Generic, Show, Traversable)

data PrimType
  = PInt
  | PWord
  | PInt64
  | PWord64
  | PAddr
  | PFloat
  | PDouble
  deriving (Eq, Show, Generic, Ord)

data WhatNext
  = ThreadRunGHC
  | ThreadInterpret
  | ThreadKilled
  | ThreadComplete
  | -- | Please report this as a bug
    WhatNextUnknownValue Word16
  deriving (Eq, Show, Generic, Ord)

data WhyBlocked
  = NotBlocked
  | BlockedOnMVar
  | BlockedOnMVarRead
  | BlockedOnBlackHole
  | BlockedOnRead
  | BlockedOnWrite
  | BlockedOnDelay
  | BlockedOnSTM
  | BlockedOnDoProc
  | BlockedOnCCall
  | BlockedOnCCall_Interruptible
  | BlockedOnMsgThrowTo
  | ThreadMigrating
  | -- | Please report this as a bug
    WhyBlockedUnknownValue Word16
  deriving (Eq, Show, Generic, Ord)

data TsoFlags
  = TsoLocked
  | TsoBlockx
  | TsoInterruptible
  | TsoStoppedOnBreakpoint
  | TsoMarked
  | TsoSqueezed
  | TsoAllocLimit
  | -- | Please report this as a bug
    TsoFlagsUnknownValue Word32
  deriving (Eq, Show, Generic, Ord)

-- | For generic code, this function returns all referenced closures.
allClosures :: GenClosure b -> [b]
allClosures (ConstrClosure {..}) = ptrArgs
allClosures (ThunkClosure {..}) = ptrArgs
allClosures (SelectorClosure {..}) = [selectee]
allClosures (IndClosure {..}) = [indirectee]
allClosures (BlackholeClosure {..}) = [indirectee]
allClosures (APClosure {..}) = fun : payload
allClosures (PAPClosure {..}) = fun : payload
allClosures (APStackClosure {..}) = fun : payload
allClosures (BCOClosure {..}) = [instrs, literals, bcoptrs]
allClosures (ArrWordsClosure {}) = []
allClosures (MutArrClosure {..}) = mccPayload
allClosures (SmallMutArrClosure {..}) = mccPayload
allClosures (MutVarClosure {..}) = [var]
allClosures (MVarClosure {..}) = [queueHead, queueTail, value]
allClosures (IOPortClosure {..}) = [queueHead, queueTail, value]
allClosures (FunClosure {..}) = ptrArgs
allClosures (BlockingQueueClosure {..}) = [link, blackHole, owner, queue]
allClosures (WeakClosure {..}) = [cfinalizers, key, value, finalizer] ++ Data.Foldable.toList weakLink
allClosures (OtherClosure {..}) = hvalues
allClosures _ = []

-- | Get the size of the top-level closure in words.
-- Includes header and payload. Does not follow pointers.
--
-- @since 8.10.1
closureSize :: Box -> Int
closureSize _ = unsupported "closureSize"
