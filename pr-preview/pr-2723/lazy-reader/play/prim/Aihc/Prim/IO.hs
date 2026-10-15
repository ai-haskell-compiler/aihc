{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnboxedTuples #-}

module Aihc.Prim.IO (IOHandle#, IORequest#, awaitIO#, stmWait#) where

import GHC.Prim (Int#, RealWorld, State#)
import GHC.Types (UnliftedType)

-- | Managed runtime references. The collector traces these values.
type IOHandle# :: UnliftedType
data IOHandle#

type IORequest# :: UnliftedType
data IORequest#

-- | Suspend the thread until its managed request completes.
foreign import prim awaitIO# :: IORequest# -> State# RealWorld -> State# RealWorld

-- The request preserves the continuation while the host waits for a timer.
stmWait# :: State# RealWorld -> (# State# RealWorld, Int# #)
stmWait# state = case stmWaitRequest# state of
  (# next, request #) -> case awaitIO# request next of
    ready -> stmWaitResult# request ready

foreign import prim stmWaitRequest# :: State# RealWorld -> (# State# RealWorld, IORequest# #)

foreign import prim stmWaitResult# :: IORequest# -> State# RealWorld -> (# State# RealWorld, Int# #)
