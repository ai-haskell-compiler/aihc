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
    TcBindingResult (..),

    -- * Module result projections
    tcModuleBindings,
    tcModuleDiagnostics,
    tcModuleSuccess,

    -- * Re-exports for convenience
    TcType (..),
    Kind (..),
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
    renderTcSignature,
  )
where

import Aihc.Parser.Syntax (Expr, Extension (..), Module (..), applyExtensionSetting, applyImpliedExtensions, fromAnnotation, mkAnnotation)
import Aihc.Tc.Annotations (TcAnnotation (..), renderTcSignature, renderTcType)
import Aihc.Tc.Error (TcDiagnostic (..), TcErrorKind (..), TcSeverity (..))
import Aihc.Tc.Generate.Decl (TcBindingResult (..), moduleBindings, tcModule)
import Aihc.Tc.Generate.Expr (inferExpr)
import Aihc.Tc.Monad
import Aihc.Tc.Solve (solveConstraints)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
import Data.Data (Data, gmapQ)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Typeable (cast)

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
  (_expr', ty, cts) <- inferExpr expr
  -- 2. Solve constraints.
  _result <- solveConstraints cts
  -- 3. Zonk the result type.
  zonkType ty

-- | Top-level bindings recovered from a type-checked module's annotations.
tcModuleBindings :: Module -> [TcBindingResult]
tcModuleBindings =
  moduleBindings

-- | Diagnostics recovered from type-checker annotations in a module.
tcModuleDiagnostics :: Module -> [TcDiagnostic]
tcModuleDiagnostics =
  collectTcDiagnostics

-- | Whether an annotated module contains no type-checker errors.
tcModuleSuccess :: Module -> Bool
tcModuleSuccess =
  not . any isError . tcModuleDiagnostics
  where
    isError diagnostic = diagSeverity diagnostic == TcError

-- | Type-check a single module, processing data declarations and
-- value bindings.
typecheckModule :: Module -> Module
typecheckModule =
  typecheckModuleWithEnv []

-- | Type-check a single module with preloaded top-level term bindings.
typecheckModuleWithEnv :: [(Text, TypeScheme)] -> Module -> Module
typecheckModuleWithEnv importedTerms m =
  case typecheckModulesWithEnv importedTerms [m] of
    [result] -> result
    _ ->
      annotateModuleDiagnostics [internalAbortDiagnostic "type checker returned unexpected module count"] m

-- | Type-check modules in order while sharing the accumulated top-level
-- environment. This is intentionally pragmatic: callers that have already
-- resolved a dependency-ordered module list can feed it here so later modules
-- see earlier data constructors and value bindings.
typecheckModulesWithEnv :: [(Text, TypeScheme)] -> [Module] -> [Module]
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

typecheckModuleWithState :: TcState -> Module -> (Module, TcState)
typecheckModuleWithState st m =
  case runTcM tcEnv (st {tcsDiagnostics = []}) (tcModule m) of
    Left abort ->
      ( annotateModuleDiagnostics [internalAbortDiagnostic (tcAbortMessage abort)] m,
        st
      )
    Right (annotatedModule, st') ->
      let diags = reverse (tcsDiagnostics st')
          result = annotateModuleDiagnostics diags annotatedModule
          nextState =
            st'
              { tcsDiagnostics = [],
                tcsMetaSolutions = Map.empty,
                tcsKindSolutions = Map.empty,
                tcsEvBinds = Map.empty
              }
       in (result, nextState)
  where
    tcEnv =
      emptyTcEnv
        { tcEnvMonoLocalBinds = MonoLocalBinds `elem` enabledExtensions,
          tcEnvMonomorphismRestriction = MonomorphismRestriction `elem` enabledExtensions
        }
    enabledExtensions =
      applyImpliedExtensions $
        foldr applyExtensionSetting [MonoLocalBinds, MonomorphismRestriction] (moduleLanguagePragmas m)

-- | Type-check a list of modules.
typecheck :: [Module] -> [Module]
typecheck = typecheckModulesWithEnv []

annotateModuleDiagnostics :: [TcDiagnostic] -> Module -> Module
annotateModuleDiagnostics diagnostics m =
  m {moduleAnns = moduleAnns m <> map mkAnnotation diagnostics}

collectTcDiagnostics :: (Data a) => a -> [TcDiagnostic]
collectTcDiagnostics value =
  case cast value of
    Just ann -> maybeToList (fromAnnotation ann)
    Nothing -> concat (gmapQ collectTcDiagnostics value)

internalAbortDiagnostic :: String -> TcDiagnostic
internalAbortDiagnostic msg =
  TcDiagnostic
    { diagLoc = Nothing,
      diagSeverity = TcError,
      diagKind = OtherError ("internal type checker abort: " <> msg)
    }
