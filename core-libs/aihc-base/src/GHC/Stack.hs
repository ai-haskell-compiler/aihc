module GHC.Stack
  ( SrcLoc (..),
    CallStack,
    emptyCallStack,
    freezeCallStack,
    fromCallSiteList,
    getCallStack,
    pushCallStack,
    callStack,
    withFrozenCallStack,
    prettyCallStack,
    prettySrcLoc,
  )
where

import GHC.Stack.Types
  ( CallStack,
    SrcLoc (..),
    emptyCallStack,
    freezeCallStack,
    fromCallSiteList,
    getCallStack,
    pushCallStack,
  )
import Prelude (String, show, (++))

-- | Call stacks are not tracked. The result is always empty.
callStack :: CallStack
callStack = emptyCallStack

withFrozenCallStack :: a -> a
withFrozenCallStack value = value

prettyCallStack :: CallStack -> String
prettyCallStack _ = "CallStack (from HasCallStack)"

prettySrcLoc :: SrcLoc -> String
prettySrcLoc location =
  srcLocFile location
    ++ ":"
    ++ show (srcLocStartLine location)
    ++ ":"
    ++ show (srcLocStartCol location)
    ++ " in "
    ++ srcLocPackage location
    ++ ":"
    ++ srcLocModule location
