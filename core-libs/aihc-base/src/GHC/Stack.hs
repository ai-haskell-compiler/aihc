{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

module GHC.Stack
  ( SrcLoc (..),
    CallStack,
    HasCallStack,
    currentCallStack,
    emptyCallStack,
    freezeCallStack,
    fromCallSiteList,
    getCallStack,
    pushCallStack,
    popCallStack,
    callStack,
    withFrozenCallStack,
    prettyCallStack,
    prettySrcLoc,
  )
where

import GHC.Base (Monad (..), String)
import GHC.Internal.Stack (popCallStack, prettyCallStack, prettySrcLoc)
import GHC.Stack.Types
  ( CallStack (..),
    HasCallStack,
    SrcLoc (..),
    emptyCallStack,
    freezeCallStack,
    fromCallSiteList,
    getCallStack,
    pushCallStack,
  )
import GHC.Types (IO)

-- | The cost-centre stack of the current program location.
--
-- aihc has no cost-centre profiling. The result is always empty, as it is
-- in GHC when a program is not built for profiling.
currentCallStack :: IO [String]
currentCallStack = return []

-- | The call stack of the enclosing function.
--
-- The entry for the call of @callStack@ itself is not included.
callStack :: (HasCallStack) => CallStack
callStack =
  case ?callStack of
    EmptyCallStack -> EmptyCallStack
    _ -> popCallStack ?callStack

-- | Run an action with a frozen call stack.
--
-- Functions called by the action do not add entries to the call stack.
withFrozenCallStack :: (HasCallStack) => ((HasCallStack) => a) -> a
withFrozenCallStack action =
  let ?callStack = freezeCallStack (popCallStack callStack)
   in action
