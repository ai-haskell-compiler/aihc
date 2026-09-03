{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Stack.Types
  ( SrcLoc (..),
    CallStack (..),
    HasCallStack,
    emptyCallStack,
    freezeCallStack,
    fromCallSiteList,
    getCallStack,
    pushCallStack,
    pushCallSite,
    popCallStack,
    prettyCallStack,
    prettyCallStackLines,
    prettySrcLoc,
    appendCallStack,
  )
where

import GHC.Base (String)
import GHC.Prim (Word#, chr#, eqWord#, int2Word#, minusWord#, quotRemWord#, word2Int#, (+#), (<#))
import GHC.Types (Char (..), Int (..), List (..))

-- | A source location for one call site.
data SrcLoc = SrcLoc
  { srcLocPackage :: String,
    srcLocModule :: String,
    srcLocFile :: String,
    srcLocStartLine :: Int,
    srcLocStartCol :: Int,
    srcLocEndLine :: Int,
    srcLocEndCol :: Int
  }

-- | A call stack with an optional freeze marker.
data CallStack
  = EmptyCallStack
  | PushCallStack String SrcLoc CallStack
  | FreezeCallStack CallStack

-- | A function with this constraint gets the call stack of its call site.
type HasCallStack = (?callStack :: CallStack)

-- | Make an empty call stack.
emptyCallStack :: CallStack
emptyCallStack = EmptyCallStack

-- | Prevent subsequent pushes to a call stack.
freezeCallStack :: CallStack -> CallStack
freezeCallStack stack@(FreezeCallStack _) = stack
freezeCallStack stack = FreezeCallStack stack

-- | Make a call stack from entries in most-recent-first order.
fromCallSiteList :: [(String, SrcLoc)] -> CallStack
fromCallSiteList [] = EmptyCallStack
fromCallSiteList ((name, location) : entries) =
  PushCallStack name location (fromCallSiteList entries)

-- | Get call-stack entries in most-recent-first order.
getCallStack :: CallStack -> [(String, SrcLoc)]
getCallStack EmptyCallStack = []
getCallStack (PushCallStack name location stack) =
  (name, location) : getCallStack stack
getCallStack (FreezeCallStack stack) = getCallStack stack

-- | Add one entry unless the call stack is frozen.
pushCallStack :: (String, SrcLoc) -> CallStack -> CallStack
pushCallStack _ stack@(FreezeCallStack _) = stack
pushCallStack (name, location) stack = PushCallStack name location stack

-- | Add one call site.
--
-- The compiler calls this function for each use of a function that has a
-- @HasCallStack@ constraint.
pushCallSite :: String -> SrcLoc -> CallStack -> CallStack
pushCallSite name location = pushCallStack (name, location)

-- | Remove the most recent entry unless the call stack is frozen.
popCallStack :: CallStack -> CallStack
popCallStack EmptyCallStack = EmptyCallStack
popCallStack (PushCallStack _ _ stack) = stack
popCallStack stack@(FreezeCallStack _) = stack

-- | Render a source location as @file:line:column in package:module@.
prettySrcLoc :: SrcLoc -> String
prettySrcLoc location =
  srcLocFile location
    ++ (':' : showInt (srcLocStartLine location))
    ++ (':' : showInt (srcLocStartCol location))
    ++ " in "
    ++ srcLocPackage location
    ++ (':' : srcLocModule location)

-- | Render a call stack with one line for each entry.
prettyCallStack :: CallStack -> String
prettyCallStack stack = joinLines (prettyCallStackLines stack)

-- | The lines of a rendered call stack. An empty call stack has no lines.
prettyCallStackLines :: CallStack -> [String]
prettyCallStackLines stack =
  case getCallStack stack of
    [] -> []
    entries -> "CallStack (from HasCallStack):" : mapList prettyEntry entries
  where
    prettyEntry (name, location) = "  " ++ name ++ ", called at " ++ prettySrcLoc location

-- | Add a rendered call stack to an error message.
appendCallStack :: String -> CallStack -> String
appendCallStack message stack =
  case prettyCallStackLines stack of
    [] -> message
    entries -> message ++ ('\n' : joinLines entries)

infixr 5 ++

(++) :: [a] -> [a] -> [a]
(++) [] suffix = suffix
(++) (value : values) suffix = value : (values ++ suffix)

mapList :: (a -> b) -> [a] -> [b]
mapList _ [] = []
mapList function (value : values) = function value : mapList function values

joinLines :: [String] -> String
joinLines [] = []
joinLines [line] = line
joinLines (line : lines') = line ++ ('\n' : joinLines lines')

showInt :: Int -> String
showInt (I# value) =
  case (<#) value 0# of
    0# -> showUnsignedInt (int2Word# value) []
    _ -> '-' : showUnsignedInt (minusWord# (int2Word# 0#) (int2Word# value)) []

showUnsignedInt :: Word# -> String -> String
showUnsignedInt value suffix =
  case quotRemWord# value (int2Word# 10#) of
    (# quotient, remainder #) ->
      case eqWord# quotient (int2Word# 0#) of
        1# -> digitChar remainder : suffix
        _ -> showUnsignedInt quotient (digitChar remainder : suffix)

digitChar :: Word# -> Char
digitChar digit = C# (chr# ((+#) (word2Int# digit) 48#))
