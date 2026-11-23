{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

module GHC.Stack
  ( SrcLoc (..),
    CallStack,
    HasCallStack,
    emptyCallStack,
    freezeCallStack,
    fromCallSiteList,
    getCallStack,
    pushCallStack,
    popCallStack,
    callStack,
    currentCallStack,
    withFrozenCallStack,
    prettyCallStack,
    prettySrcLoc,
    whoCreated,
    CostCentreStack,
    CostCentre,
    getCurrentCCS,
    getCCSOf,
    clearCCS,
    ccsCC,
    ccsParent,
    ccLabel,
    ccModule,
    ccSrcSpan,
    ccsToStrings,
    renderStack,
  )
where

import GHC.Internal.Stack (popCallStack, prettyCallStack, prettySrcLoc)
import GHC.Stack.CCS
  ( CostCentre,
    CostCentreStack,
    ccLabel,
    ccModule,
    ccSrcSpan,
    ccsCC,
    ccsParent,
    ccsToStrings,
    clearCCS,
    currentCallStack,
    getCCSOf,
    getCurrentCCS,
    renderStack,
    whoCreated,
  )
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
