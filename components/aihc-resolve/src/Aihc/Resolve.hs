module Aihc.Resolve
  ( resolve,
    ResolveError (..),
    ResolveResult (..),
  )
where

import Aihc.Parser.Syntax (Module)
import Aihc.Resolve.Types

-- | Resolve names in a group of modules.
--
-- Takes a list of parsed modules and returns the same modules
-- with resolution annotations attached, or errors.
resolve :: [Module] -> ResolveResult
resolve _modules =
  ResolveResult
    { resolvedModules = [],
      resolveErrors = [ResolveNotImplemented "name resolution is not yet implemented"]
    }
