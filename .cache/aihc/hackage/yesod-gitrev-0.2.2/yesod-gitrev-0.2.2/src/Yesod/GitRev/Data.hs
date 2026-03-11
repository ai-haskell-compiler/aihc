{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

module Yesod.GitRev.Data where

import GitHash
import Language.Haskell.TH.Syntax (Lift, Q, Exp, unTypeQ)
import Yesod.Core

#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
import Language.Haskell.TH (Code, examineCode)
#else
import Language.Haskell.TH.Syntax(TExp)
#endif

-- | You should not construct one of these yourself.
-- Instead, use gitRev or tGitRev.
-- Fields added to this record are treated as a minor version bump.
data GitRev = GitRev
  { gitRevHash :: String
  , gitRevBranch :: String
  , gitRevDirty :: Bool
  , gitRevCommitDate :: String
    -- ^ @since 0.2.1
  , gitRevCommitCount :: Int
    -- ^ @since 0.2.1
  , gitRevCommitMessage :: String
    -- ^ @since 0.2.1
  }
  deriving Lift

type TSpliceable a =
#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
  Code Q a
#else
  Q (TExp a)
#endif

mkYesodSubData "GitRev" [parseRoutes|
/ GitRevR GET
|]

-- | A typed splice for creating a GitRev.
-- Example: $$(tGitRev) :: GitRev
tGitRev :: TSpliceable GitRev
tGitRev = [|| gitRevFromGitInfo $$(tGitInfoCwd) ||]

-- | An untyped splice for creating a GitRev.
-- Example: $(gitRev) :: GitRev
gitRev :: Q Exp
gitRev = unTypeQ . examine $ tGitRev
  where
#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
    examine = examineCode
#else
    examine = id
#endif

-- This function is considered to be "internal".
-- Changes to this function will not be accounted for in minor version bumps.
gitRevFromGitInfo :: GitInfo -> GitRev
gitRevFromGitInfo gi = GitRev
  { gitRevHash = giHash gi
  , gitRevBranch = giBranch gi
  , gitRevDirty = giDirty gi
  , gitRevCommitDate = giCommitDate gi
  , gitRevCommitCount = giCommitCount gi
  , gitRevCommitMessage = giCommitMessage gi
  }
