-- | What the resolver gives for a compilation unit.
module Aihc.Resolve.Result
  ( ResolvedModule (..),
    ResolvedUnit (..),
    ResolveFailure (..),
  )
where

import Aihc.Parser.Syntax (Extension)
import Aihc.Resolve.Syntax qualified as R
import Aihc.Resolve.Types (GlobalName, Package, ResolveError)

-- | One module after name resolution: its syntax with every name resolved,
-- and the top-level terms it can see, including through qualified imports.
data ResolvedModule = ResolvedModule
  { resolvedModulePackage :: !Package,
    resolvedModuleExtensions :: ![Extension],
    -- | The resolved syntax tree, with each resolution in an explicit field.
    resolvedModuleSyntax :: !R.Module,
    resolvedVisibleTerms :: ![GlobalName]
  }
  deriving (Show)

-- | A compilation unit after name resolution. Every name of every module
-- resolved: a unit with a failed resolution is a 'ResolveFailure' instead.
newtype ResolvedUnit = ResolvedUnit
  { resolvedModules :: [ResolvedModule]
  }
  deriving (Show)

-- | A compilation unit that did not resolve. The modules carry the
-- resolutions that succeeded and each 'ResolveError'. Tools render them.
-- The compiler does not go on from here.
data ResolveFailure = ResolveFailure
  { failureErrors :: [ResolveError],
    failureModules :: [ResolvedModule]
  }
  deriving (Show)
