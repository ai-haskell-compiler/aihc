{-# Language Trustworthy #-}
{-# Language ImplicitParams #-}
{-# Language TemplateHaskell #-}
module Panic
  ( Panic(..)
  , PanicComponent(..)
  , useGitRevision
  , HasCallStack
  , panic
  ) where

import Development.GitRev
import Language.Haskell.TH
import Data.Typeable

import Control.Exception(Exception, throw)
import Data.Maybe(fromMaybe,listToMaybe)
import GHC.Stack

-- | Throw an exception for the given component.
panic :: (PanicComponent a, HasCallStack) =>
  a        {- ^ Component identification -} ->
  String   {- ^ Location of problem -} ->
  [String] {- ^ Problem description (lines) -} ->
  b
panic comp loc msg =
  throw Panic { panicComponent = comp
              , panicLoc       = loc
              , panicMsg       = msg
              , panicStack     = freezeCallStack ?callStack
              }

-- | The exception thrown when panicing.
data Panic a = Panic { panicComponent :: a
                     , panicLoc       :: String
                     , panicMsg       :: [String]
                     , panicStack     :: CallStack
                     }

-- | Description of a component.
class Typeable a => PanicComponent a where
  panicComponentName     :: a -> String
  -- ^ Name of the panicing component.

  panicComponentIssues   :: a -> String
  -- ^ Issue tracker for the panicking component.

  panicComponentRevision :: a -> (String,String)
  -- ^ Information about the component's revision.
  -- (commit hash, branch info)

-- | An expression of type @a -> (String,String)@.
-- Uses template Haskell to query Git for the current state of the repo.
-- Note that the state reported depends on when the module containing
-- the splice was compiled.
useGitRevision :: Q Exp
useGitRevision = [| \_ -> ($gitHash, $gitBranch ++ $dirty) |]
  where dirty = [| if $gitDirty then " (uncommited files present)" else "" |]


instance (PanicComponent a) => Show (Panic a) where
  show p = unlines $
    [ "You have encountered a bug in " ++
        panicComponentName comp ++ "'s implementation."
    , "*** Please create an issue at " ++
        panicComponentIssues comp
    , ""
    , "%< --------------------------------------------------- "
    ] ++ rev ++
    [ locLab ++ panicLoc p
    , msgLab ++ fromMaybe "" (listToMaybe msgLines)
    ]
    ++ map (tabs ++) (drop 1 msgLines)
    ++ [ prettyCallStack (panicStack p) ] ++
    [ "%< --------------------------------------------------- "
    ]
    where comp      = panicComponent p
          msgLab    = "  Message:   "
          locLab    = "  Location:  "
          revLab    = "  Revision:  "
          branchLab = "  Branch:    "
          msgLines  = panicMsg p
          tabs      = map (const ' ') msgLab

          (commitHash,commitBranch) = panicComponentRevision comp

          rev | null commitHash = []
              | otherwise       = [ revLab ++ commitHash
                                  , branchLab ++ commitBranch
                                  ]


instance PanicComponent a => Exception (Panic a)

