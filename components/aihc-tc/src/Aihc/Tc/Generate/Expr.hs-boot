module Aihc.Tc.Generate.Expr (inferExprAt) where

import Aihc.Parser.Syntax (Expr, SourceSpan)
import Aihc.Tc.Constraint (Ct)
import Aihc.Tc.Monad (TcM)
import Aihc.Tc.Types (TcType)

inferExprAt :: SourceSpan -> Expr -> TcM (Expr, TcType, [Ct])
