{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ImplicitParams #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#define HasCallStack_ HasCallStack =>
#else
#define HasCallStack_
#endif

module Data.CallStack (
#if __GLASGOW_HASKELL__ >= 704
  HasCallStack,
#endif
  CallStack
, SrcLoc(..)
, callStack
, callSite
) where

import Data.Maybe
import Data.SrcLoc

#ifdef WINDOWS
import System.FilePath
#endif

#if MIN_VERSION_base(4,8,1)
import qualified GHC.Stack as GHC
#endif

#if MIN_VERSION_base(4,9,0)
import           GHC.Stack (HasCallStack)
#elif MIN_VERSION_base(4,8,1)
type HasCallStack = (?callStack :: GHC.CallStack)
#elif __GLASGOW_HASKELL__ >= 704
import GHC.Exts (Constraint)
type HasCallStack = (() :: Constraint)
#endif

type CallStack = [(String, SrcLoc)]

callStack :: HasCallStack_ CallStack
callStack = workaroundForIssue19236 $
#if MIN_VERSION_base(4,9,0)
  drop 1 $ GHC.getCallStack GHC.callStack
#elif MIN_VERSION_base(4,8,1)
  drop 2 $ GHC.getCallStack ?callStack
#else
  []
#endif

callSite :: HasCallStack_ Maybe (String, SrcLoc)
callSite = listToMaybe (reverse callStack)

workaroundForIssue19236 :: CallStack -> CallStack -- https://gitlab.haskell.org/ghc/ghc/-/issues/19236
workaroundForIssue19236 =
#ifdef WINDOWS
  map (fmap fixSrcLoc)
  where
    fixSrcLoc :: SrcLoc -> SrcLoc
    fixSrcLoc loc = loc { srcLocFile = fixPath $ srcLocFile loc }

    fixPath :: FilePath -> FilePath
    fixPath =
      joinPath . splitDirectories
#else
  id
#endif
