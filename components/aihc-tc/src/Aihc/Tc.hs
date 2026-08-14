{-# LANGUAGE ScopedTypeVariables #-}

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
    typecheckExprWithConfig,
    typecheckModule,
    typecheckModuleWithEnv,
    typecheckModuleWithEnvAndInstances,
    typecheckModulesWithEnv,
    typecheckModulesWithEnvAndInstances,
    typecheckModulesWithFullEnv,
    typecheckModuleSccWithFullEnv,
    typecheckModulesWithClassEnv,
    typecheckModuleSccWithClassEnv,
    typecheckModulesWithInterface,
    typecheckModulesWithInterfaceConfig,
    typecheckModuleSccWithInterface,
    typecheckModuleSccWithInterfaceConfig,

    -- * Result types
    TcResult (..),
    TcConfig,
    tcConfig,
    TcBindingResult (..),
    TcTermKey (..),
    tcTermKeyIdentifier,
    TcInterface (..),
    emptyTcInterface,

    -- * Module result projections
    tcModuleBindings,
    tcModuleDiagnostics,
    tcModuleInstances,
    tcModuleClasses,
    tcModuleSuccess,

    -- * Re-exports for convenience
    TcType (..),
    Kind (..),
    RuntimeRep (..),
    Levity (..),
    VecCount (..),
    VecElem (..),
    TyCon (..),
    tyConKind,
    tyConKindScheme,
    TyVarId (..),
    tvKind,
    TypeScheme (..),
    boxedTupleTyConName,
    Pred (..),
    InstanceInfo (..),
    DataFamilyInstanceInfo (..),
    DataTypeInfo (..),
    DataConInfo (..),
    DataConFieldInfo (..),
    DataConFieldUnpack (..),
    DataConSourceForm (..),
    dataConArgTypes,
    dataFamilyAxiomName,
    dataFamilyRepresentationName,
    ClassInfo (..),
    TyConFlavor (..),
    TyConInfo (..),
    Unique (..),
    liftedRuntimeRep,
    liftedTypeKind,
    typeKind,
    runtimeRepOfType,
    isLiftedType,
    isUnliftedType,
    TcAnnotation (..),
    TcDerivingAnnotation (..),
    TcDerivingContext (..),
    TcDerivingPlan (..),
    TcDerivingStrategy (..),
    TcStockDerivingPlan (..),
    TcDiagnostic (..),
    TcErrorKind (..),
    TcSeverity (..),
    renderPred,
    renderTcSignature,
    renderTcType,
    renderTcTypeInModule,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    ArithSeq (..),
    ClassDeclItem (..),
    Cmd (..),
    CompStmt (..),
    DataConDecl (..),
    Decl (..),
    DoStmt (..),
    ExportSpec (..),
    Expr (..),
    Extension (..),
    GuardQualifier (..),
    ImportItem (..),
    InstanceDeclItem (..),
    Literal (..),
    Module (..),
    Pattern (..),
    SourceSpan (..),
    Type (..),
    applyExtensionSetting,
    applyImpliedExtensions,
    fromAnnotation,
    mkAnnotation,
  )
import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Annotations (TcAnnotation (..), TcDerivingAnnotation (..), TcDerivingContext (..), TcDerivingPlan (..), TcDerivingStrategy (..), TcStockDerivingPlan (..), renderPred, renderTcSignature, renderTcType, renderTcTypeInModule)
import Aihc.Tc.Env (ClassInfo (..), DataConFieldInfo (..), DataConFieldUnpack (..), DataConInfo (..), DataConSourceForm (..), DataFamilyInstanceInfo (..), DataTypeInfo (..), InstanceInfo (..), TyConFlavor (..), TyConInfo (..), dataConArgTypes, dataFamilyAxiomName, dataFamilyRepresentationName)
import Aihc.Tc.Error (TcDiagnostic (..), TcErrorKind (..), TcSeverity (..))
import Aihc.Tc.Generate.Decl (TcBindingResult (..), moduleBindings, moduleClasses, moduleInstances, tcModule, tcModuleScc)
import Aihc.Tc.Generate.Expr (inferExpr)
import Aihc.Tc.Monad
import Aihc.Tc.Solve (solveConstraints)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import Control.Monad.Trans.State.Strict (State, get, put, runState)
import Data.Bifunctor qualified as Bifunctor
import Data.Data (Data, gmapM, gmapQ)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
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

-- | The complete semantic interface shared between independently checked
-- module groups. Implementations never cross this boundary: only the facts
-- needed to type-check downstream source are retained.
data TcInterface = TcInterface
  { tcInterfaceTerms :: ![(TcTermKey, TypeScheme)],
    tcInterfaceTyCons :: ![TyConInfo],
    tcInterfaceDataTypes :: ![DataTypeInfo],
    tcInterfaceClasses :: ![ClassInfo],
    tcInterfaceInstances :: ![InstanceInfo],
    tcInterfaceDataFamilyInstances :: ![DataFamilyInstanceInfo]
  }
  deriving (Show, Read)

emptyTcInterface :: TcInterface
emptyTcInterface =
  TcInterface
    { tcInterfaceTerms = [],
      tcInterfaceTyCons = [],
      tcInterfaceDataTypes = [],
      tcInterfaceClasses = [],
      tcInterfaceInstances = [],
      tcInterfaceDataFamilyInstances = []
    }

instance Semigroup TcInterface where
  left <> right =
    TcInterface
      { tcInterfaceTerms = mergeInterfaceEntries fst (tcInterfaceTerms left <> tcInterfaceTerms right),
        tcInterfaceTyCons = mergeInterfaceEntries tciTyCon (tcInterfaceTyCons left <> tcInterfaceTyCons right),
        tcInterfaceDataTypes = mergeInterfaceEntries dtiName (tcInterfaceDataTypes left <> tcInterfaceDataTypes right),
        tcInterfaceClasses = mergeInterfaceEntries ciName (tcInterfaceClasses left <> tcInterfaceClasses right),
        tcInterfaceInstances = mergeInterfaceEntries iiDictName (tcInterfaceInstances left <> tcInterfaceInstances right),
        tcInterfaceDataFamilyInstances = mergeInterfaceEntries dfiiAxiomName (tcInterfaceDataFamilyInstances left <> tcInterfaceDataFamilyInstances right)
      }

instance Monoid TcInterface where
  mempty = emptyTcInterface

mergeInterfaceEntries :: (Ord key) => (value -> key) -> [value] -> [value]
mergeInterfaceEntries key = Map.elems . Map.fromList . map (\value -> (key value, value))

tcTermKeyIdentifier :: TcTermKey -> Maybe Text
tcTermKeyIdentifier key =
  case key of
    TcTermLocal {} -> Nothing
    TcTermGlobal _ _ identifier -> Just identifier

importedTermEntries :: [(Text, TypeScheme)] -> [(TcTermKey, TypeScheme)]
importedTermEntries = map (Bifunctor.first unqualifiedTermKey)

exportedTermEntries :: TcInterface -> [(Text, TypeScheme)]
exportedTermEntries interface =
  Map.toList $
    Map.fromList
      [ (name, scheme)
      | (key, scheme) <- tcInterfaceTerms interface,
        Just name <- [tcTermKeyIdentifier key]
      ]

-- | Type-check a single expression in an empty environment.
--
-- This is the primary entry point for testing. For full module
-- type-checking, use 'typecheck'.
typecheckExpr :: Expr -> TcResult
typecheckExpr = typecheckExprWithConfig defaultTcConfig

typecheckExprWithConfig :: TcConfig -> Expr -> TcResult
typecheckExprWithConfig config expr =
  case runTcM emptyTcEnv {tcEnvConfig = config} initTcState (typecheckExprM expr) of
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

-- | Class instances recovered from a type-checked module's annotations.
tcModuleInstances :: Module -> [InstanceInfo]
tcModuleInstances =
  moduleInstances

-- | Type classes recovered from a type-checked module's annotations.
tcModuleClasses :: Module -> [ClassInfo]
tcModuleClasses = moduleClasses

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
typecheckModule = typecheckModuleWithEnv []

-- | Type-check a single module with preloaded top-level term bindings.
typecheckModuleWithEnv :: [(Text, TypeScheme)] -> Module -> Module
typecheckModuleWithEnv importedTerms = typecheckModuleWithEnvAndInstances importedTerms []

-- | Type-check a single module with preloaded terms and class instances.
typecheckModuleWithEnvAndInstances :: [(Text, TypeScheme)] -> [InstanceInfo] -> Module -> Module
typecheckModuleWithEnvAndInstances importedTerms importedInstances m =
  case typecheckModulesWithEnvAndInstances importedTerms importedInstances [m] of
    [result] -> result
    _ ->
      annotateModuleDiagnostics [internalAbortDiagnostic "type checker returned unexpected module count"] m

-- | Type-check modules in order while sharing the accumulated top-level
-- environment. This is intentionally pragmatic: callers that have already
-- resolved a dependency-ordered module list can feed it here so later modules
-- see earlier data constructors and value bindings.
typecheckModulesWithEnv :: [(Text, TypeScheme)] -> [Module] -> [Module]
typecheckModulesWithEnv importedTerms = typecheckModulesWithEnvAndInstances importedTerms []

-- | Type-check modules in order with preloaded terms and class instances.
typecheckModulesWithEnvAndInstances :: [(Text, TypeScheme)] -> [InstanceInfo] -> [Module] -> [Module]
typecheckModulesWithEnvAndInstances importedTerms importedInstances =
  fst
    . typecheckModulesWithInterface
      emptyTcInterface
        { tcInterfaceTerms = importedTermEntries importedTerms,
          tcInterfaceInstances = importedInstances
        }

-- | Type-check modules with a complete imported type-checker interface and
-- return the accumulated term schemes and type constructors for downstream
-- modules.
typecheckModulesWithFullEnv :: [(Text, TypeScheme)] -> [TyConInfo] -> [InstanceInfo] -> [Module] -> ([Module], [(Text, TypeScheme)], [TyConInfo])
typecheckModulesWithFullEnv importedTerms importedTyCons importedInstances modules =
  let (checkedModules, interface) =
        typecheckModulesWithInterface
          emptyTcInterface
            { tcInterfaceTerms = importedTermEntries importedTerms,
              tcInterfaceTyCons = importedTyCons,
              tcInterfaceInstances = importedInstances
            }
          modules
   in (checkedModules, exportedTermEntries interface, tcInterfaceTyCons interface)

typecheckModulesWithClassEnv :: [(Text, TypeScheme)] -> [TyConInfo] -> [ClassInfo] -> [InstanceInfo] -> [Module] -> ([Module], [(Text, TypeScheme)], [TyConInfo], [ClassInfo])
typecheckModulesWithClassEnv importedTerms importedTyCons importedClasses importedInstances modules =
  let (checkedModules, interface) =
        typecheckModulesWithInterface
          TcInterface
            { tcInterfaceTerms = importedTermEntries importedTerms,
              tcInterfaceTyCons = importedTyCons,
              tcInterfaceDataTypes = [],
              tcInterfaceClasses = importedClasses,
              tcInterfaceInstances = importedInstances,
              tcInterfaceDataFamilyInstances = []
            }
          modules
   in (checkedModules, exportedTermEntries interface, tcInterfaceTyCons interface, tcInterfaceClasses interface)

-- | Type-check dependency-ordered modules with a complete imported semantic
-- interface and return the accumulated interface for downstream modules.
typecheckModulesWithInterface :: TcInterface -> [Module] -> ([Module], TcInterface)
typecheckModulesWithInterface = typecheckModulesWithInterfaceConfig defaultTcConfig

typecheckModulesWithInterfaceConfig :: TcConfig -> TcInterface -> [Module] -> ([Module], TcInterface)
typecheckModulesWithInterfaceConfig config imported modules =
  let (checkedModules, finalState) = go (initialTcState imported) modules
   in (checkedModules, tcInterfaceFromState finalState)
  where
    go st [] = ([], st)
    go st (m : ms) =
      let (result, st') = typecheckModuleWithState config st m
          (results, finalState) = go st' ms
       in (result : results, finalState)

-- | Type-check the modules in one strongly connected import component as a
-- single incremental unit. Only the supplied imported interface is visible;
-- implementations from predecessor components are never consumed.
typecheckModuleSccWithFullEnv :: [(Text, TypeScheme)] -> [TyConInfo] -> [InstanceInfo] -> [Module] -> ([Module], [(Text, TypeScheme)], [TyConInfo])
typecheckModuleSccWithFullEnv importedTerms importedTyCons importedInstances modules =
  let (checkedModules, interface) =
        typecheckModuleSccWithInterface
          emptyTcInterface
            { tcInterfaceTerms = importedTermEntries importedTerms,
              tcInterfaceTyCons = importedTyCons,
              tcInterfaceInstances = importedInstances
            }
          modules
   in (checkedModules, exportedTermEntries interface, tcInterfaceTyCons interface)

typecheckModuleSccWithClassEnv :: [(Text, TypeScheme)] -> [TyConInfo] -> [ClassInfo] -> [InstanceInfo] -> [Module] -> ([Module], [(Text, TypeScheme)], [TyConInfo], [ClassInfo])
typecheckModuleSccWithClassEnv importedTerms importedTyCons importedClasses importedInstances modules =
  let (checkedModules, interface) =
        typecheckModuleSccWithInterface
          TcInterface
            { tcInterfaceTerms = importedTermEntries importedTerms,
              tcInterfaceTyCons = importedTyCons,
              tcInterfaceDataTypes = [],
              tcInterfaceClasses = importedClasses,
              tcInterfaceInstances = importedInstances,
              tcInterfaceDataFamilyInstances = []
            }
          modules
   in (checkedModules, exportedTermEntries interface, tcInterfaceTyCons interface, tcInterfaceClasses interface)

-- | Type-check one strongly connected module component using only the
-- supplied imported interface.
typecheckModuleSccWithInterface :: TcInterface -> [Module] -> ([Module], TcInterface)
typecheckModuleSccWithInterface = typecheckModuleSccWithInterfaceConfig defaultTcConfig

typecheckModuleSccWithInterfaceConfig :: TcConfig -> TcInterface -> [Module] -> ([Module], TcInterface)
typecheckModuleSccWithInterfaceConfig config imported modules =
  let (checkedModules, finalState) = typecheckModuleSccWithState config (initialTcState imported) modules
   in (checkedModules, tcInterfaceFromState finalState)

initialTcState :: TcInterface -> TcState
initialTcState imported =
  initTcState
    { tcsGlobalTerms =
        Map.fromList
          [ (key, TcIdBinder scheme Closed)
          | (key, scheme) <- tcInterfaceTerms imported
          ]
          <> tcsGlobalTerms initTcState,
      tcsGlobalTyCons =
        Map.fromList
          [ (tciTyCon tyCon, tyCon)
          | tyCon <- tcInterfaceTyCons imported
          ]
          <> tcsGlobalTyCons initTcState,
      tcsDataTypes = Map.fromList [(dtiName dataType, dataType) | dataType <- tcInterfaceDataTypes imported],
      tcsClasses = Map.fromList [(ciName classInfo, classInfo) | classInfo <- tcInterfaceClasses imported],
      tcsInstances = tcInterfaceInstances imported,
      tcsDataFamilyInstances = tcInterfaceDataFamilyInstances imported
    }

tcInterfaceFromState :: TcState -> TcInterface
tcInterfaceFromState state =
  TcInterface
    { tcInterfaceTerms =
        [ (key, scheme)
        | (key, TcIdBinder scheme _) <- Map.toList (tcsGlobalTerms state)
        ],
      tcInterfaceTyCons = Map.elems (tcsGlobalTyCons state),
      tcInterfaceDataTypes = Map.elems (tcsDataTypes state),
      tcInterfaceClasses = Map.elems (tcsClasses state),
      tcInterfaceInstances = mergeInterfaceEntries iiDictName (tcsInstances state),
      tcInterfaceDataFamilyInstances = mergeInterfaceEntries dfiiAxiomName (tcsDataFamilyInstances state)
    }

typecheckModuleSccWithState :: TcConfig -> TcState -> [Module] -> ([Module], TcState)
typecheckModuleSccWithState config st modules =
  case runTcM tcEnv (st {tcsDiagnostics = []}) (tcModuleScc modules) of
    Left abort ->
      ( case modules of
          [] -> []
          first : rest -> annotateModuleDiagnostics [internalAbortDiagnostic (tcAbortMessage abort)] first : rest,
        st
      )
    Right (annotatedModules, st') ->
      let diags = reverse (tcsDiagnostics st')
          results = attachSccDiagnostics diags annotatedModules
          nextState =
            st'
              { tcsDiagnostics = [],
                tcsMetaSolutions = Map.empty,
                tcsKindSolutions = Map.empty,
                tcsEvBinds = Map.empty
              }
       in (results, nextState)
  where
    tcEnv =
      emptyTcEnv
        { tcEnvConfig = config,
          tcEnvMonoLocalBinds = any (elem MonoLocalBinds . moduleExtensions) modules,
          tcEnvMonomorphismRestriction = any (elem MonomorphismRestriction . moduleExtensions) modules
        }
    moduleExtensions m =
      applyImpliedExtensions $
        foldr applyExtensionSetting [MonoLocalBinds, MonomorphismRestriction] (moduleLanguagePragmas m)

attachSccDiagnostics :: [TcDiagnostic] -> [Module] -> [Module]
attachSccDiagnostics diagnostics modules = foldl attachOne modules diagnostics
  where
    attachOne [] _ = []
    attachOne current@(first : rest) diagnostic =
      case diagLoc diagnostic of
        Nothing -> annotateModuleDiagnostics [diagnostic] first : rest
        Just span' ->
          let sourceName = sourceSpanSourceName span'
              matches m = sourceName `elem` moduleSourceNames m
           in if any matches current
                then map (\m -> if matches m then annotateModuleDiagnostics [diagnostic] m else m) current
                else annotateModuleDiagnostics [internalAbortDiagnostic "SCC diagnostic source did not match a module"] first : rest

moduleSourceNames :: Module -> [FilePath]
moduleSourceNames modu =
  case spanFromAnnotations (moduleAnns modu) of
    SourceSpan {sourceSpanSourceName = sourceName} -> [sourceName]
    NoSourceSpan -> []

typecheckModuleWithState :: TcConfig -> TcState -> Module -> (Module, TcState)
typecheckModuleWithState config st m =
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
        { tcEnvConfig = config,
          tcEnvMonoLocalBinds = MonoLocalBinds `elem` enabledExtensions,
          tcEnvMonomorphismRestriction = MonomorphismRestriction `elem` enabledExtensions
        }
    enabledExtensions =
      applyImpliedExtensions $
        foldr applyExtensionSetting [MonoLocalBinds, MonomorphismRestriction] (moduleLanguagePragmas m)

-- | Type-check a list of modules.
typecheck :: [Module] -> [Module]
typecheck = typecheckModulesWithEnv []

defaultTcConfig :: TcConfig
defaultTcConfig = tcConfig (PackageId (T.pack "aihc-prim"))

annotateModuleDiagnostics :: [TcDiagnostic] -> Module -> Module
annotateModuleDiagnostics diagnostics m =
  let (located, unlocated) = partitionDiagnostics diagnostics
      moduleWithLocated = foldl attachLocatedDiagnostic m located
   in moduleWithLocated {moduleAnns = moduleAnns moduleWithLocated <> map mkAnnotation unlocated}

partitionDiagnostics :: [TcDiagnostic] -> ([(SourceSpan, TcDiagnostic)], [TcDiagnostic])
partitionDiagnostics =
  foldr partitionOne ([], [])
  where
    partitionOne diagnostic (located, unlocated) =
      case diagLoc diagnostic of
        Just sp -> ((sp, diagnostic) : located, unlocated)
        Nothing -> (located, diagnostic : unlocated)

attachLocatedDiagnostic :: Module -> (SourceSpan, TcDiagnostic) -> Module
attachLocatedDiagnostic m (sp, diagnostic) =
  case runState (attachDiagnosticAt sp diagnostic m) False of
    (m', True) -> m'
    (_, False) ->
      error ("type checker diagnostic has no matching syntax node for source span: " <> show sp)

-- Attach bottom-up so an exact child span wins over an exact parent span.
-- Located diagnostics must never guess: if no exact syntax span exists, abort.
attachDiagnosticAt :: forall a. (Data a) => SourceSpan -> TcDiagnostic -> a -> State Bool a
attachDiagnosticAt sp diagnostic value = do
  value' <- gmapM (attachDiagnosticAt sp diagnostic) value
  alreadyAttached <- get
  if alreadyAttached
    then pure value'
    else case attachDiagnosticHere sp diagnostic value' of
      Just value'' -> do
        put True
        pure value''
      Nothing ->
        pure value'

attachDiagnosticHere :: forall a. (Data a) => SourceSpan -> TcDiagnostic -> a -> Maybe a
attachDiagnosticHere sp diagnostic value =
  attachAnnotationList
    <|> attachExpr
    <|> attachPattern
    <|> attachType
    <|> attachDecl
    <|> attachDataConDecl
    <|> attachLiteral
    <|> attachGuardQualifier
    <|> attachDoStmtExpr
    <|> attachDoStmtCmd
    <|> attachCompStmt
    <|> attachArithSeq
    <|> attachClassDeclItem
    <|> attachInstanceDeclItem
    <|> attachCmd
    <|> attachExportSpec
    <|> attachImportItem
  where
    diagnosticAnn = mkAnnotation diagnostic
    atExactSpan span' wrap =
      if span' == sp
        then cast wrap
        else Nothing
    attachTyped :: forall node. (Data node) => (node -> Maybe node) -> Maybe a
    attachTyped f = do
      node <- cast value
      node' <- f node
      cast node'
    attachAnnotationList =
      attachTyped $ \(anns :: [Annotation]) ->
        atExactSpan (spanFromAnnotations anns) (anns <> [diagnosticAnn])
    attachExpr =
      attachTyped $ \(expr :: Expr) ->
        atExactSpan (wrappedSpan peelExprAnnOnce expr) (EAnn diagnosticAnn expr)
    attachPattern =
      attachTyped $ \(pat :: Pattern) ->
        atExactSpan (wrappedSpan peelPatternAnnOnce pat) (PAnn diagnosticAnn pat)
    attachType =
      attachTyped $ \(ty :: Type) ->
        atExactSpan (wrappedSpan peelTypeAnnOnce ty) (TAnn diagnosticAnn ty)
    attachDecl =
      attachTyped $ \(decl :: Decl) ->
        atExactSpan (wrappedSpan peelDeclAnnOnce decl) (DeclAnn diagnosticAnn decl)
    attachDataConDecl =
      attachTyped $ \(decl :: DataConDecl) ->
        atExactSpan (wrappedSpan peelDataConAnnOnce decl) (DataConAnn diagnosticAnn decl)
    attachLiteral =
      attachTyped $ \(lit :: Literal) ->
        atExactSpan (wrappedSpan peelLiteralAnnOnce lit) (LitAnn diagnosticAnn lit)
    attachGuardQualifier =
      attachTyped $ \(qualifier :: GuardQualifier) ->
        atExactSpan (wrappedSpan peelGuardAnnOnce qualifier) (GuardAnn diagnosticAnn qualifier)
    attachDoStmtExpr =
      attachTyped $ \(stmt :: DoStmt Expr) ->
        atExactSpan (wrappedSpan peelDoAnnOnce stmt) (DoAnn diagnosticAnn stmt)
    attachDoStmtCmd =
      attachTyped $ \(stmt :: DoStmt Cmd) ->
        atExactSpan (wrappedSpan peelDoAnnOnce stmt) (DoAnn diagnosticAnn stmt)
    attachCompStmt =
      attachTyped $ \(stmt :: CompStmt) ->
        atExactSpan (wrappedSpan peelCompAnnOnce stmt) (CompAnn diagnosticAnn stmt)
    attachArithSeq =
      attachTyped $ \(seq' :: ArithSeq) ->
        atExactSpan (wrappedSpan peelArithSeqAnnOnce seq') (ArithSeqAnn diagnosticAnn seq')
    attachClassDeclItem =
      attachTyped $ \(item :: ClassDeclItem) ->
        atExactSpan (wrappedSpan peelClassItemAnnOnce item) (ClassItemAnn diagnosticAnn item)
    attachInstanceDeclItem =
      attachTyped $ \(item :: InstanceDeclItem) ->
        atExactSpan (wrappedSpan peelInstanceItemAnnOnce item) (InstanceItemAnn diagnosticAnn item)
    attachCmd =
      attachTyped $ \(cmd :: Cmd) ->
        atExactSpan (wrappedSpan peelCmdAnnOnce cmd) (CmdAnn diagnosticAnn cmd)
    attachExportSpec =
      attachTyped $ \(spec :: ExportSpec) ->
        atExactSpan (wrappedSpan peelExportAnnOnce spec) (ExportAnn diagnosticAnn spec)
    attachImportItem =
      attachTyped $ \(item :: ImportItem) ->
        atExactSpan (wrappedSpan peelImportAnnOnce item) (ImportAnn diagnosticAnn item)

wrappedSpan :: (node -> Maybe (Annotation, node)) -> node -> SourceSpan
wrappedSpan peel =
  spanFromAnnotations . fst . peelLeading peel

peelLeading :: (node -> Maybe (Annotation, node)) -> node -> ([Annotation], node)
peelLeading peel =
  go []
  where
    go anns node =
      case peel node of
        Just (ann, inner) -> go (ann : anns) inner
        Nothing -> (reverse anns, node)

peelExprAnnOnce :: Expr -> Maybe (Annotation, Expr)
peelExprAnnOnce (EAnn ann inner) = Just (ann, inner)
peelExprAnnOnce _ = Nothing

peelPatternAnnOnce :: Pattern -> Maybe (Annotation, Pattern)
peelPatternAnnOnce (PAnn ann inner) = Just (ann, inner)
peelPatternAnnOnce _ = Nothing

peelTypeAnnOnce :: Type -> Maybe (Annotation, Type)
peelTypeAnnOnce (TAnn ann inner) = Just (ann, inner)
peelTypeAnnOnce _ = Nothing

peelDeclAnnOnce :: Decl -> Maybe (Annotation, Decl)
peelDeclAnnOnce (DeclAnn ann inner) = Just (ann, inner)
peelDeclAnnOnce _ = Nothing

peelDataConAnnOnce :: DataConDecl -> Maybe (Annotation, DataConDecl)
peelDataConAnnOnce (DataConAnn ann inner) = Just (ann, inner)
peelDataConAnnOnce _ = Nothing

peelLiteralAnnOnce :: Literal -> Maybe (Annotation, Literal)
peelLiteralAnnOnce (LitAnn ann inner) = Just (ann, inner)
peelLiteralAnnOnce _ = Nothing

peelGuardAnnOnce :: GuardQualifier -> Maybe (Annotation, GuardQualifier)
peelGuardAnnOnce (GuardAnn ann inner) = Just (ann, inner)
peelGuardAnnOnce _ = Nothing

peelDoAnnOnce :: DoStmt body -> Maybe (Annotation, DoStmt body)
peelDoAnnOnce (DoAnn ann inner) = Just (ann, inner)
peelDoAnnOnce _ = Nothing

peelCompAnnOnce :: CompStmt -> Maybe (Annotation, CompStmt)
peelCompAnnOnce (CompAnn ann inner) = Just (ann, inner)
peelCompAnnOnce _ = Nothing

peelArithSeqAnnOnce :: ArithSeq -> Maybe (Annotation, ArithSeq)
peelArithSeqAnnOnce (ArithSeqAnn ann inner) = Just (ann, inner)
peelArithSeqAnnOnce _ = Nothing

peelClassItemAnnOnce :: ClassDeclItem -> Maybe (Annotation, ClassDeclItem)
peelClassItemAnnOnce (ClassItemAnn ann inner) = Just (ann, inner)
peelClassItemAnnOnce _ = Nothing

peelInstanceItemAnnOnce :: InstanceDeclItem -> Maybe (Annotation, InstanceDeclItem)
peelInstanceItemAnnOnce (InstanceItemAnn ann inner) = Just (ann, inner)
peelInstanceItemAnnOnce _ = Nothing

peelCmdAnnOnce :: Cmd -> Maybe (Annotation, Cmd)
peelCmdAnnOnce (CmdAnn ann inner) = Just (ann, inner)
peelCmdAnnOnce _ = Nothing

peelExportAnnOnce :: ExportSpec -> Maybe (Annotation, ExportSpec)
peelExportAnnOnce (ExportAnn ann inner) = Just (ann, inner)
peelExportAnnOnce _ = Nothing

peelImportAnnOnce :: ImportItem -> Maybe (Annotation, ImportItem)
peelImportAnnOnce (ImportAnn ann inner) = Just (ann, inner)
peelImportAnnOnce _ = Nothing

spanFromAnnotations :: [Annotation] -> SourceSpan
spanFromAnnotations =
  fromMaybe NoSourceSpan . foldr ((<|>) . spanFromAnnotation) Nothing

spanFromAnnotation :: Annotation -> Maybe SourceSpan
spanFromAnnotation =
  concreteSpan <=< fromAnnotation

concreteSpan :: SourceSpan -> Maybe SourceSpan
concreteSpan NoSourceSpan = Nothing
concreteSpan sp = Just sp

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
