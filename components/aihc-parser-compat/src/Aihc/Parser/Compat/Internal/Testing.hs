-- | Internal compatibility helpers used by generative tests.
--
-- This module is exposed so the test-only @fuzz@ component can exercise the
-- GHC conversion code compiled into the main library. It is not a stable API.
module Aihc.Parser.Compat.Internal.Testing
  ( compatGhcExtensions,
    normalizeGhcAst,
    parseGhcLocatedDecl,
    parseGhcLocatedExpr,
  )
where

import Aihc.Parser.Compat.Internal.Ghc
