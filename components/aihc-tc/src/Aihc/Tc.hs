-- | Entry point for the aihc type checker.
--
-- The type checker consumes a parsed and name-resolved AST
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
    typecheckModuleWithEnv,
    typecheckModulesWithEnv,

    -- * Result types
    TcResult (..),
    TcModuleResult (..),
    TcBindingResult (..),
    tcmBindings,
    tcModuleDiagnostics,

    -- * Re-exports for convenience
    TcType (..),
    Kind (..),
    TyCon (..),
    TyVarId (..),
    TypeScheme (..),
    Pred (..),
    Unique (..),
    TcAnnotation (..),
    TcBindingAnnotation (..),
    TcDiagnostic (..),
    TcErrorKind (..),
    TcSeverity (..),
    renderTcType,
    renderTcSignature,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    Expr (..),
    Extension (..),
    Module (..),
    applyExtensionSetting,
    applyImpliedExtensions,
    fromAnnotation,
  )
import Aihc.Tc.Annotations (TcAnnotation (..), TcBindingAnnotation (..), renderTcSignature, renderTcType)
import Aihc.Tc.Error (TargetedDiagnostic (..), TcDiagnostic (..), TcErrorKind (..), TcSeverity (..))
import Aihc.Tc.Generate.Decl (TcBindingResult (..), moduleBindings, tcModule)
import Aihc.Tc.Generate.Expr (inferExpr)
import Aihc.Tc.Monad
import Aihc.Tc.NodeId (assignTcNodeIds, attachTargetedAnnotations, stripTcNodeIds)
import Aihc.Tc.Solve (solveConstraints)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
import Data.Data (Data, cast, gmapQ)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Text (Text)

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
      let diags = map targetedDiagnosticDiagnostic (reverse (tcsDiagnostics st))
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
  { -- | Module annotated with type-checker elaboration data.
    tcmModule :: !Module,
    -- | Whether type checking succeeded (no errors).
    tcmSuccess :: !Bool
  }
  deriving (Show)

tcmBindings :: TcModuleResult -> [TcBindingResult]
tcmBindings =
  moduleBindings . tcmModule

-- | Type-check a single module, processing data declarations and
-- value bindings.
typecheckModule :: Module -> TcModuleResult
typecheckModule =
  typecheckModuleWithEnv []

-- | Type-check a single module with preloaded top-level term bindings.
typecheckModuleWithEnv :: [(Text, TypeScheme)] -> Module -> TcModuleResult
typecheckModuleWithEnv importedTerms m =
  case typecheckModulesWithEnv importedTerms [m] of
    [result] -> result
    _ ->
      TcModuleResult
        { tcmModule = m,
          tcmSuccess = False
        }

-- | Type-check modules in order while sharing the accumulated top-level
-- environment. This is intentionally pragmatic: callers that have already
-- resolved a dependency-ordered module list can feed it here so later modules
-- see earlier data constructors and value bindings.
typecheckModulesWithEnv :: [(Text, TypeScheme)] -> [Module] -> [TcModuleResult]
typecheckModulesWithEnv importedTerms =
  go initState
  where
    initState =
      initTcState
        { tcsGlobalTerms =
            Map.fromList
              [ (name, TcIdBinder name scheme Closed)
              | (name, scheme) <- importedTerms
              ]
              <> tcsGlobalTerms initTcState
        }

    go _ [] = []
    go st (m : ms) =
      let (result, st') = typecheckModuleWithState st m
       in result : go st' ms

typecheckModuleWithState :: TcState -> Module -> (TcModuleResult, TcState)
typecheckModuleWithState st m =
  case runTcM tcEnv (st {tcsDiagnostics = []}) (tcModule taggedModule) of
    Left _abort ->
      ( TcModuleResult
          { tcmModule = stripTcNodeIds taggedModule,
            tcmSuccess = False
          },
        st
      )
    Right (annotatedModule, st') ->
      let targetedDiagnostics = reverse (tcsDiagnostics st')
          diags = map targetedDiagnosticDiagnostic targetedDiagnostics
          hasErrors = any isError diags
          result =
            TcModuleResult
              { tcmModule = attachTargetedAnnotations (map targetedDiagnosticPayload targetedDiagnostics) annotatedModule,
                tcmSuccess = not hasErrors
              }
          nextState =
            st'
              { tcsDiagnostics = [],
                tcsMetaSolutions = Map.empty,
                tcsKindSolutions = Map.empty,
                tcsEvBinds = Map.empty,
                tcsOccurrenceElaborations = Map.empty,
                tcsBindingElaborations = Map.empty
              }
       in (result, nextState)
  where
    taggedModule =
      assignTcNodeIds m

    tcEnv =
      emptyTcEnv
        { tcEnvMonoLocalBinds = MonoLocalBinds `elem` enabledExtensions,
          tcEnvMonomorphismRestriction = MonomorphismRestriction `elem` enabledExtensions
        }
    enabledExtensions =
      applyImpliedExtensions $
        foldr applyExtensionSetting [MonoLocalBinds, MonomorphismRestriction] (moduleLanguagePragmas m)
    isError d = diagSeverity d == TcError

    targetedDiagnosticPayload diagnostic =
      (targetedDiagnosticTarget diagnostic, targetedDiagnosticDiagnostic diagnostic)

-- | Type-check a list of modules.
typecheck :: [Module] -> [TcModuleResult]
typecheck = typecheckModulesWithEnv []

tcModuleDiagnostics :: Module -> [TcDiagnostic]
tcModuleDiagnostics =
  collectDiagnostics
  where
    collectDiagnostics :: (Data a) => a -> [TcDiagnostic]
    collectDiagnostics node =
      maybe [] annotationDiagnostics (cast node)
        <> concat (gmapQ collectDiagnostics node)

    annotationDiagnostics :: Annotation -> [TcDiagnostic]
    annotationDiagnostics annotation =
      maybeToList (fromAnnotation annotation)
