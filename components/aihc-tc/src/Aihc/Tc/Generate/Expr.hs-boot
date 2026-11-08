module Aihc.Tc.Generate.Expr (checkRhs, inferExprAt) where

import Aihc.Parser.Syntax (Expr, Rhs, SourceSpan)
import Aihc.Tc.Constraint (Ct)
import Aihc.Tc.Monad (TcM)
import Aihc.Tc.Types (TcType)

inferExprAt :: Maybe SourceSpan -> Expr -> TcM (Expr, TcType, [Ct])

checkRhs :: TcType -> Rhs Expr -> TcM (Rhs Expr, TcType, [Ct])
