module Aihc.Resolve.Types
  ( ResolveError (..),
    ResolveResult (..),
  )
where

import Aihc.Parser.Syntax (Module)

data ResolveError
  = ResolveNotImplemented String
  deriving (Eq, Show)

data ResolveResult = ResolveResult
  { resolvedModules :: [Module],
    resolveErrors :: [ResolveError]
  }
  deriving (Show)
