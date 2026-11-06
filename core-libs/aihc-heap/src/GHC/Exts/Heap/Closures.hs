{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}

-- | The description of a heap object as a Haskell value. AIHC cannot read
-- the heap in the way that GHC does, so most functions that make these values
-- raise an exception. The types are the same as in ghc-heap.
module GHC.Exts.Heap.Closures
  ( Closure,
    GenClosure (..),
    PrimType (..),
    WhatNext (..),
    WhyBlocked (..),
    TsoFlags (..),
    allClosures,
    closureSize,
    StgStackClosure,
    GenStgStackClosure (..),
    StackFrame,
    GenStackFrame (..),
    StackField,
    GenStackField (..),
    Box (..),
    areBoxesEqual,
    asBox,
  )
where

import Data.Foldable (toList)
import Data.Int
import Data.Word
import GHC.Exts hiding (toList)
import GHC.Exts.Heap.InfoTable
import GHC.Exts.Heap.ProfInfo.Types
import GHC.Exts.Heap.Unsupported
import GHC.Generics (Generic)
import Prelude

-- | A value of any type, kept without evaluation.
data Box = Box Any

-- AIHC does not give the address of a heap object.
instance Show Box where
  showsPrec _ _ _ = unsupported "Show Box"

-- | Put a value into a 'Box' without evaluation.
asBox :: a -> Box
asBox x = Box (unsafeCoerce# x)

-- | Test if two boxes hold the same heap object.
areBoxesEqual :: Box -> Box -> IO Bool
areBoxesEqual (Box a) (Box b) = pure (isTrue# (reallyUnsafePtrEquality# a b))

type Closure = GenClosure Box

-- | The description of a heap object. The parameter @b@ is the type of a
-- reference to another object.
data GenClosure b
  = ConstrClosure
      { info :: !StgInfoTable,
        ptrArgs :: ![b],
        dataArgs :: ![Word],
        pkg :: !String,
        modl :: !String,
        name :: !String
      }
  | FunClosure
      { info :: !StgInfoTable,
        ptrArgs :: ![b],
        dataArgs :: ![Word]
      }
  | ThunkClosure
      { info :: !StgInfoTable,
        ptrArgs :: ![b],
        dataArgs :: ![Word]
      }
  | SelectorClosure
      { info :: !StgInfoTable,
        selectee :: !b
      }
  | PAPClosure
      { info :: !StgInfoTable,
        arity :: !HalfWord,
        n_args :: !HalfWord,
        fun :: !b,
        payload :: ![b]
      }
  | APClosure
      { info :: !StgInfoTable,
        arity :: !HalfWord,
        n_args :: !HalfWord,
        fun :: !b,
        payload :: ![b]
      }
  | APStackClosure
      { info :: !StgInfoTable,
        fun :: !b,
        payload :: ![b]
      }
  | IndClosure
      { info :: !StgInfoTable,
        indirectee :: !b
      }
  | BCOClosure
      { info :: !StgInfoTable,
        instrs :: !b,
        literals :: !b,
        bcoptrs :: !b,
        arity :: !HalfWord,
        size :: !HalfWord,
        bitmap :: ![Word]
      }
  | BlackholeClosure
      { info :: !StgInfoTable,
        indirectee :: !b
      }
  | ArrWordsClosure
      { info :: !StgInfoTable,
        bytes :: !Word,
        arrWords :: ![Word]
      }
  | MutArrClosure
      { info :: !StgInfoTable,
        mccPtrs :: !Word,
        mccSize :: !Word,
        mccPayload :: ![b]
      }
  | SmallMutArrClosure
      { info :: !StgInfoTable,
        mccPtrs :: !Word,
        mccPayload :: ![b]
      }
  | MVarClosure
      { info :: !StgInfoTable,
        queueHead :: !b,
        queueTail :: !b,
        value :: !b
      }
  | IOPortClosure
      { info :: !StgInfoTable,
        queueHead :: !b,
        queueTail :: !b,
        value :: !b
      }
  | MutVarClosure
      { info :: !StgInfoTable,
        var :: !b
      }
  | BlockingQueueClosure
      { info :: !StgInfoTable,
        link :: !b,
        blackHole :: !b,
        owner :: !b,
        queue :: !b
      }
  | WeakClosure
      { info :: !StgInfoTable,
        cfinalizers :: !b,
        key :: !b,
        value :: !b,
        finalizer :: !b,
        weakLink :: !(Maybe b)
      }
  | TSOClosure
      { info :: !StgInfoTable,
        link :: !b,
        global_link :: !b,
        tsoStack :: !b,
        trec :: !b,
        blocked_exceptions :: !b,
        bq :: !b,
        thread_label :: !(Maybe b),
        what_next :: !WhatNext,
        why_blocked :: !WhyBlocked,
        flags :: ![TsoFlags],
        threadId :: !Word64,
        saved_errno :: !Word32,
        tso_dirty :: !Word32,
        alloc_limit :: !Int64,
        tot_stack_size :: !Word32,
        prof :: !(Maybe StgTSOProfInfo)
      }
  | StackClosure
      { info :: !StgInfoTable,
        stack_size :: !Word32,
        stack_dirty :: !Word8,
        stack_marking :: !Word8
      }
  | IntClosure
      { ptipe :: PrimType,
        intVal :: !Int
      }
  | WordClosure
      { ptipe :: PrimType,
        wordVal :: !Word
      }
  | Int64Closure
      { ptipe :: PrimType,
        int64Val :: !Int64
      }
  | Word64Closure
      { ptipe :: PrimType,
        word64Val :: !Word64
      }
  | AddrClosure
      { ptipe :: PrimType,
        addrVal :: !(Ptr ())
      }
  | FloatClosure
      { ptipe :: PrimType,
        floatVal :: !Float
      }
  | DoubleClosure
      { ptipe :: PrimType,
        doubleVal :: !Double
      }
  | OtherClosure
      { info :: !StgInfoTable,
        hvalues :: ![b],
        rawWords :: ![Word]
      }
  | UnsupportedClosure
      { info :: !StgInfoTable
      }
  | UnknownTypeWordSizedPrimitive
      { wordVal :: !Word
      }
  deriving (Show, Generic, Functor, Foldable, Traversable)

type StgStackClosure = GenStgStackClosure Box

-- | A decoded stack: the info table, the size in words, and the frames.
data GenStgStackClosure b = GenStgStackClosure
  { ssc_info :: !StgInfoTable,
    ssc_stack_size :: !Word32,
    ssc_stack :: ![GenStackFrame b]
  }
  deriving (Foldable, Functor, Generic, Show, Traversable)

type StackField = GenStackField Box

-- | A word of a stack frame: a plain word or a reference.
data GenStackField b
  = StackWord !Word
  | StackBox !b
  deriving (Foldable, Functor, Generic, Show, Traversable)

type StackFrame = GenStackFrame Box

-- | One frame of a stack.
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
      { info_tbl :: !StgInfoTable
      }
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
        bco :: !b,
        bcoArgs :: ![GenStackField b]
      }
  deriving (Foldable, Functor, Generic, Show, Traversable)

-- | The type of the value in a primitive closure.
data PrimType
  = PInt
  | PWord
  | PInt64
  | PWord64
  | PAddr
  | PFloat
  | PDouble
  deriving (Eq, Show, Generic, Ord)

-- | What a thread does next.
data WhatNext
  = ThreadRunGHC
  | ThreadInterpret
  | ThreadKilled
  | ThreadComplete
  | WhatNextUnknownValue Word16
  deriving (Eq, Show, Generic, Ord)

-- | Why a thread is blocked.
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
  | WhyBlockedUnknownValue Word16
  deriving (Eq, Show, Generic, Ord)

-- | The flags of a thread.
data TsoFlags
  = TsoLocked
  | TsoBlockx
  | TsoInterruptible
  | TsoStoppedOnBreakpoint
  | TsoMarked
  | TsoSqueezed
  | TsoAllocLimit
  | TsoFlagsUnknownValue Word32
  deriving (Eq, Show, Generic, Ord)

-- | The references that a closure holds.
allClosures :: GenClosure b -> [b]
allClosures ConstrClosure {..} = ptrArgs
allClosures ThunkClosure {..} = ptrArgs
allClosures FunClosure {..} = ptrArgs
allClosures SelectorClosure {..} = [selectee]
allClosures IndClosure {..} = [indirectee]
allClosures BlackholeClosure {..} = [indirectee]
allClosures APClosure {..} = fun : payload
allClosures PAPClosure {..} = fun : payload
allClosures APStackClosure {..} = fun : payload
allClosures BCOClosure {..} = [instrs, literals, bcoptrs]
allClosures MutArrClosure {..} = mccPayload
allClosures SmallMutArrClosure {..} = mccPayload
allClosures MutVarClosure {..} = [var]
allClosures MVarClosure {..} = [queueHead, queueTail, value]
allClosures IOPortClosure {..} = [queueHead, queueTail, value]
allClosures BlockingQueueClosure {..} = [link, blackHole, owner, queue]
allClosures WeakClosure {..} = [cfinalizers, key, value, finalizer] ++ toList weakLink
allClosures OtherClosure {..} = hvalues
allClosures _ = []

-- | The size of a closure in words. AIHC closures do not have the GHC layout.
closureSize :: Box -> Int
closureSize _ = unsupported "closureSize"
