module Aihc.Parser.Compat
  ( toGhcHsExpr,
  )
where

import Aihc.Parser.Compat.Internal.Convert qualified as Convert
import Aihc.Parser.Syntax (Expr)
import GHC.Hs (GhcPs)
import Language.Haskell.Syntax.Expr (HsExpr)

-- | Convert an aihc-parser expression into the matching ghc-lib-parser AST.
--
-- All locations and exact-print comments in the result are empty/default GHC
-- values.
toGhcHsExpr :: Expr -> HsExpr GhcPs
toGhcHsExpr = Convert.toGhcHsExpr
