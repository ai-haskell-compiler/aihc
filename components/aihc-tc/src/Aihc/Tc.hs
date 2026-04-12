-- | Entry point for the aihc type checker.
--
-- The type checker consumes a parsed (and optionally name-resolved) AST
-- and produces the same AST annotated with typing information. It does
-- not transform the tree structure.
--
-- The implementation follows the OutsideIn(X) algorithm:
--
-- 1. Generate wanted constraints by walking the AST.
-- 2. Solve the constraints using the worklist/inert-set architecture.
-- 3. Zonk meta-variables.
-- 4. Attach type annotations to AST nodes.
module Aihc.Tc
  ( -- * Entry point
    typecheck,
    typecheckExpr,
    typecheckModule,

    -- * Result types
    TcResult (..),
    TcModuleResult (..),
    TcBindingResult (..),

    -- * Re-exports for convenience
    TcType (..),
    TyCon (..),
    TyVarId (..),
    TypeScheme (..),
    Pred (..),
    Unique (..),
    TcAnnotation (..),
    TcDiagnostic (..),
    TcErrorKind (..),
    TcSeverity (..),
    renderTcType,
  )
where

import Aihc.Parser.Syntax (Expr, Module (..))
import Aihc.Tc.Annotations (TcAnnotation (..), renderTcType)
import Aihc.Tc.Error (TcDiagnostic (..), TcErrorKind (..), TcSeverity (..))
import Aihc.Tc.Generate.Decl (TcBindingResult (..), tcModule)
import Aihc.Tc.Generate.Expr (inferExpr)
import Aihc.Tc.Monad
import Aihc.Tc.Solve (solveConstraints)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)

-- | Result of type checking.
data TcResult = TcResult
  { -- | The inferred type of the top-level expression or binding.
    tcResultType :: !TcType,
    -- | Diagnostics (errors and warnings) produced.
    tcResultDiagnostics :: ![TcDiagnostic],
    -- | Whether type checking succeeded (no errors).
    tcResultSuccess :: !Bool
  }
  deriving (Show)

-- | Type-check a single expression in an empty environment.
--
-- This is the primary entry point for testing. For full module
-- type-checking, use 'typecheck'.
typecheckExpr :: Expr -> TcResult
typecheckExpr expr =
  case runTcM emptyTcEnv initTcState (typecheckExprM expr) of
    Left _abort ->
      TcResult
        { tcResultType = TcMetaTv (Unique (-1)),
          tcResultDiagnostics = [],
          tcResultSuccess = False
        }
    Right (ty, st) ->
      let diags = reverse (tcsDiagnostics st)
          hasErrors = any isError diags
       in TcResult
            { tcResultType = ty,
              tcResultDiagnostics = diags,
              tcResultSuccess = not hasErrors
            }
  where
    isError d = diagSeverity d == TcError

-- | Internal: type-check an expression in TcM.
typecheckExprM :: Expr -> TcM TcType
typecheckExprM expr = do
  -- 1. Generate constraints.
  (ty, cts) <- inferExpr expr
  -- 2. Solve constraints.
  _result <- solveConstraints cts
  -- 3. Zonk the result type.
  zonkType ty

-- | Result of type-checking a module.
data TcModuleResult = TcModuleResult
  { -- | Inferred types for each top-level binding.
    tcmBindings :: ![TcBindingResult],
    -- | Diagnostics (errors and warnings) produced.
    tcmDiagnostics :: ![TcDiagnostic],
    -- | Whether type checking succeeded (no errors).
    tcmSuccess :: !Bool
  }
  deriving (Show)

-- | Type-check a single module, processing data declarations and
-- value bindings.
typecheckModule :: Module -> TcModuleResult
typecheckModule m =
  case runTcM emptyTcEnv initTcState (tcModule m) of
    Left _abort ->
      TcModuleResult
        { tcmBindings = [],
          tcmDiagnostics = [],
          tcmSuccess = False
        }
    Right (bindings, st) ->
      let diags = reverse (tcsDiagnostics st)
          hasErrors = any isError diags
       in TcModuleResult
            { tcmBindings = bindings,
              tcmDiagnostics = diags,
              tcmSuccess = not hasErrors
            }
  where
    isError d = diagSeverity d == TcError

-- | Type-check a list of modules.
typecheck :: [Module] -> [TcModuleResult]
typecheck = map typecheckModule
