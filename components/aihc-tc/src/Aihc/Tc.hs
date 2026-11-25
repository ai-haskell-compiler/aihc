-- | Entry point for the aihc type checker.
--
-- The type checker consumes a parsed and name-resolved AST, together with
-- the language extensions in force for it, and produces the same AST
-- annotated with typing information. It does not transform the tree
-- structure, and it does not read the module's language pragmas: whoever
-- read the source folded the language edition, the package's default
-- extensions and the pragmas into the extension set of each module.
--
-- The type checker reads the resolved syntax tree of "Aihc.Resolve.Syntax".
-- It gives the checked modules as the syntax tree of "Aihc.Tc.Syntax".
--
-- The implementation follows the OutsideIn(X) algorithm:
--
-- 1. Generate wanted constraints by walking the AST.
-- 2. Solve the constraints using the worklist/inert-set architecture.
-- 3. Zonk meta-variables.
-- 4. Attach type annotations to AST nodes.
module Aihc.Tc
  ( -- * Entry point
    typecheckModulesWithInterface,
    typecheckModuleSccWithInterface,
    typecheckModuleSccWithFacts,

    -- * Result types
    TcConfig,
    mkTcConfig,
    TcWiring (..),
    DerivingReferences (..),
    GenericReferences (..),
    DataReferences (..),
    UnliftedFieldReferences (..),
    DerivingReference (..),
    ReferencePackage (..),
    StockClassLocation (..),
    derivingReferenceList,
    TcBindingResult (..),
    defaultMethodName,
    Entity (..),
    TcInterface (..),
    InstanceKey,
    tcInterfaceTerms,
    tcInterfaceTyCons,
    tcInterfaceDataTypes,
    tcInterfaceClasses,
    tcInterfaceInstances,
    tcInterfaceDataFamilyInstances,
    tcInterfaceTypeFamilyInstances,
    tcInterfacePatSyns,
    tcInterfaceForeignImports,
    tcInterfaceFromLists,
    ModuleOrigin,
    FactEnv,
    emptyFactEnv,
    factEnvFromModules,
    factEnvFromInterface,
    factEnvInterface,
    emptyTcInterface,
    MergeCheck (..),
    mergeTcInterface,
    mergeTcInterfaces,

    -- * Module result projections
    tcModuleBindings,
    tcModuleDiagnostics,
    tcModuleSuccess,

    -- * Re-exports for convenience
    TcType (..),
    GlobalName,
    TcAxiomKey (..),
    TcKindEnv,
    TyCon (..),
    mkTyConWithNamespace,
    tyConKey,
    tyConPackageId,
    tyConModuleName,
    TyVarId (..),
    tvKind,
    TypeScheme (..),
    Pred (..),
    InstanceInfo (..),
    DataFamilyInstanceInfo (..),
    DataTypeInfo (..),
    CType (..),
    dataTypeKey,
    DataConInfo (..),
    PatSynDirection (..),
    PatSynInfo (..),
    patSynKey,
    DataConFieldInfo (..),
    DataConFieldUnpack (..),
    FieldRep (..),
    applySubstRep,
    repHasUnpack,
    repLeaves,
    unpackableConstructor,
    DataConSourceForm (..),
    dataConArgTypes,
    dataFamilyAxiomKey,
    dataFamilyAxiomName,
    dataFamilyRepresentationName,
    TypeFamilyInstanceInfo (..),
    typeFamilyAxiomKey,
    typeFamilyAxiomName,
    typeFamilyAxiomIndex,
    ClassInfo (..),
    AssociatedTypeInfo (..),
    FunDep (..),
    TyConFlavor (..),
    TyConInfo (..),
    Unique (..),
    TcKinds (..),
    typeKind,
    liftedRep,
    mkTcKinds,
    typeKindInEnv,
    familyEquationRightKind,
    runtimeRepOfTypeInEnv,
    isUnliftedTypeInEnv,
    TcAnnotation (..),
    TcDerivingAnnotation (..),
    TcDerivingContext (..),
    TcDerivingPlan (..),
    TcDerivingStrategy (..),
    TcDiagnostic (..),
    TcErrorKind (..),
    TcSeverity (..),
    renderFunDepNames,
    renderPred,
    renderTcSignature,
    renderTcType,
    renderTyLit,
    renderTcTypeInModule,
  )
where

import Aihc.Parser.Syntax (Extension (..))
import Aihc.Resolve (ResolvedModule (..))
import Aihc.Resolve.Syntax qualified as R
import Aihc.Tc.Annotations (TcAnnotation (..), TcDerivingAnnotation (..), TcDerivingContext (..), TcDerivingPlan (..), TcDerivingStrategy (..), renderFunDepNames, renderPred, renderTcSignature, renderTcType, renderTcTypeInModule, renderTyLit)
import Aihc.Tc.Bindings (moduleBindings, moduleDiagnostics)
import Aihc.Tc.Deriving.References (DataReferences (..), DerivingReference (..), DerivingReferences (..), GenericReferences (..), ReferencePackage (..), StockClassLocation (..), UnliftedFieldReferences (..), derivingReferenceList)
import Aihc.Tc.Diagnostics (annotateModuleDiagnostics, attachSccDiagnostics, internalAbortDiagnostic)
import Aihc.Tc.Env (AssociatedTypeInfo (..), CType (..), ClassInfo (..), DataConFieldInfo (..), DataConFieldUnpack (..), DataConInfo (..), DataConSourceForm (..), DataFamilyInstanceInfo (..), DataTypeInfo (..), FieldRep (..), FunDep (..), InstanceInfo (..), PatSynDirection (..), PatSynInfo (..), TyConFlavor (..), TyConInfo (..), TypeFamilyInstanceInfo (..), applySubstRep, dataConArgTypes, dataFamilyAxiomKey, dataFamilyAxiomName, dataFamilyRepresentationName, dataTypeKey, instanceEnvFromList, instanceEnvSince, instanceInfoKey, patSynKey, repHasUnpack, repLeaves, typeFamilyAxiomIndex, typeFamilyAxiomKey, typeFamilyAxiomName)
import Aihc.Tc.Error (TcDiagnostic (..), TcErrorKind (..), TcSeverity (..))
import Aihc.Tc.Generate.Decl (TcBindingResult (..), defaultMethodName, tcModule, tcModuleScc, withModuleIdentity)
import Aihc.Tc.Interface
import Aihc.Tc.Monad
import Aihc.Tc.Syntax (Module)
import Aihc.Tc.Syntax qualified as Final
import Aihc.Tc.Syntax.FromPending (Finalizer (..), FromPending (..))
import Aihc.Tc.Syntax.FromResolved (FromResolved (..))
import Aihc.Tc.Syntax.Pending qualified as Pending
import Aihc.Tc.Types
import Aihc.Tc.Unpack (unpackableConstructor)
import Aihc.Tc.Wiring (mkTcKinds)
import Aihc.Tc.Zonk (finalizeDiagnostics)
import Data.Functor.Identity (Identity (..))
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

-- | Top-level bindings recovered from a type-checked module's annotations.
tcModuleBindings :: TcWiring -> Module -> [TcBindingResult]
tcModuleBindings =
  moduleBindings

-- | Diagnostics recovered from type-checker annotations in a module.
tcModuleDiagnostics :: Module -> [TcDiagnostic]
tcModuleDiagnostics =
  moduleDiagnostics

-- | Whether an annotated module contains no type-checker errors.
tcModuleSuccess :: Module -> Bool
tcModuleSuccess =
  not . any isError . tcModuleDiagnostics
  where
    isError diagnostic = diagSeverity diagnostic == TcError

-- | Type-check dependency-ordered modules with an imported semantic interface.
-- Return only facts that the specified modules define.
typecheckModulesWithInterface :: TcConfig -> TcInterface -> [ResolvedModule] -> ([Module], TcInterface)
typecheckModulesWithInterface config = typecheckWithFacts checkModules . factEnvFromInterface
  where
    checkModules initialState resolved =
      let (finalState, checkedModules) = List.mapAccumL check initialState resolved
       in (checkedModules, finalState)
    check st m =
      let (result, st') = typecheckModuleWithState config st m
       in (st', result)

-- | Type-check one strongly connected module component using only the
-- supplied imported interface.
typecheckModuleSccWithInterface :: TcConfig -> TcInterface -> [ResolvedModule] -> ([Module], TcInterface)
typecheckModuleSccWithInterface config = typecheckModuleSccWithFacts config . factEnvFromInterface

-- | Type-check one strongly connected module component with the facts of
-- the imported modules, by the module of their origin.
typecheckModuleSccWithFacts :: TcConfig -> FactEnv -> [ResolvedModule] -> ([Module], TcInterface)
typecheckModuleSccWithFacts config = typecheckWithFacts (typecheckModuleSccWithState config)

-- | Use the same module identity and interface rules for both check orders.
--
-- The check gives final syntax trees of the type checker.
typecheckWithFacts :: (TcState -> [ResolvedModule] -> ([Final.Module], TcState)) -> FactEnv -> [ResolvedModule] -> ([Module], TcInterface)
typecheckWithFacts check imported resolved
  -- Name resolution already checked imports and exports. Without declarations,
  -- the modules add no types, evidence, or diagnostics.
  | all (null . R.moduleDecls . resolvedModuleSyntax) resolved = (map annotateIdentity resolved, emptyTcInterface)
  | otherwise =
      let initialState = initialTcState imported
          (checkedModules, finalState) = check initialState resolved
       in (checkedModules, tcInterfaceDifference initialState finalState)
  where
    annotateIdentity unit = uncheckedModule (withModuleIdentity (resolvedModulePackage unit) (fromResolved (resolvedModuleSyntax unit)))

-- | A module that the type checker did not check. It keeps only the
-- identity of the module.
uncheckedModule :: Pending.Module -> Final.Module
uncheckedModule =
  runIdentity
    . fromPending
      Finalizer
        { finalizeItem = pure . plainInfo,
          missingType = \node -> error ("uncheckedModule: a module without declarations has the node " <> T.unpack node),
          recover = fmap Just
        }
  where
    plainInfo item =
      case item of
        Pending.InfoModuleIdentity identity -> Just (Final.InfoModuleIdentity identity)
        _ -> Nothing

-- | The state before the check of a module group. The tables of terms,
-- type constructors, data types, classes, pattern synonyms, and foreign
-- imports start empty, because a lookup finds an imported fact in the
-- fact environment. The instances and the family instances come from the
-- extra facts, because a search reads all of them.
initialTcState :: FactEnv -> TcState
initialTcState facts =
  initTcState
    { tcsNextUnique = factEnvUniqueBound facts,
      tcsFacts = facts,
      tcsInstances = instanceEnvFromList (tcInterfaceInstances imported),
      tcsDataFamilyInstances = tcInterfaceDataFamilyInstanceMap imported,
      tcsTypeFamilyInstances = tcInterfaceTypeFamilyInstanceMap imported,
      tcsFamilyEquations = tcsFamilyEquations (setTypeFamilyInstances (tcInterfaceTypeFamilyInstanceMap imported) initTcState)
    }
  where
    imported = factEnvExtra facts

tcInterfaceDifference :: TcState -> TcState -> TcInterface
tcInterfaceDifference initial state =
  TcInterface
    { tcInterfaceTermMap = exportedGlobalTerms (newEntries (tcsGlobalTerms state) (tcsGlobalTerms initial)),
      tcInterfaceTyConMap = Map.withoutKeys (newEntries (tcsGlobalTyCons state) (tcsGlobalTyCons initial)) (tcsReplacedImports state),
      tcInterfaceDataTypeMap = newEntries (tcsDataTypes state) (tcsDataTypes initial),
      tcInterfaceClassMap = newEntries (tcsClasses state) (tcsClasses initial),
      tcInterfaceInstanceMap =
        Map.fromList
          [ (instanceInfoKey info, info)
          | info <- instanceEnvSince (tcsInstances state) (tcsInstances initial)
          ],
      tcInterfaceDataFamilyInstanceMap = newEntries (tcsDataFamilyInstances state) (tcsDataFamilyInstances initial),
      tcInterfaceTypeFamilyInstanceMap = newEntries (tcsTypeFamilyInstances state) (tcsTypeFamilyInstances initial),
      tcInterfacePatSynMap = newEntries (tcsPatSyns state) (tcsPatSyns initial),
      tcInterfaceForeignImportMap = newEntries (tcsForeignImports state) (tcsForeignImports initial),
      tcInterfaceUniqueBound = tcsNextUnique state
    }
  where
    -- These tables only gain keys. Equal sizes thus mean no new facts.
    newEntries current previous
      | Map.size current == Map.size previous = Map.empty
      | otherwise = Map.difference current previous

exportedGlobalTerms :: Map.Map Entity TcBinder -> Map.Map Entity TypeScheme
exportedGlobalTerms = Map.mapMaybe binderScheme
  where
    binderScheme binder =
      case binder of
        TcIdBinder scheme _ -> Just scheme
        _ -> Nothing

typecheckModuleSccWithState :: TcConfig -> TcState -> [ResolvedModule] -> ([Final.Module], TcState)
typecheckModuleSccWithState config st resolved =
  case runTcM tcEnv (st {tcsDiagnostics = []}) (tcModuleScc resolved <* finalizeDiagnostics) of
    Left abort ->
      ( case map (uncheckedModule . fromResolved . resolvedModuleSyntax) resolved of
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
                tcsMetaSolutions = mempty,
                tcsTrackedKindMetas = mempty,
                tcsEvBinds = Map.empty
              }
       in (results, nextState)
  where
    extensions = map resolvedModuleExtensions resolved
    tcEnv =
      (emptyTcEnv config)
        { tcEnvMonoLocalBinds = any (elem MonoLocalBinds) extensions,
          tcEnvMonomorphismRestriction = any (elem MonomorphismRestriction) extensions,
          tcEnvScopedTypeVariables = any (elem ScopedTypeVariables) extensions,
          tcEnvUndecidableInstances = any (elem UndecidableInstances) extensions,
          tcEnvPostfixOperators = any (elem PostfixOperators) extensions
        }

typecheckModuleWithState :: TcConfig -> TcState -> ResolvedModule -> (Final.Module, TcState)
typecheckModuleWithState config st resolved =
  case runTcM tcEnv (st {tcsDiagnostics = []}) (tcModule resolved <* finalizeDiagnostics) of
    Left abort ->
      ( annotateModuleDiagnostics [internalAbortDiagnostic (tcAbortMessage abort)] (uncheckedModule (fromResolved (resolvedModuleSyntax resolved))),
        st
      )
    Right (annotatedModule, st') ->
      let diags = reverse (tcsDiagnostics st')
          result = annotateModuleDiagnostics diags annotatedModule
          nextState =
            st'
              { tcsDiagnostics = [],
                tcsMetaSolutions = mempty,
                tcsTrackedKindMetas = mempty,
                tcsEvBinds = Map.empty
              }
       in (result, nextState)
  where
    tcEnv =
      (emptyTcEnv config)
        { tcEnvMonoLocalBinds = MonoLocalBinds `elem` enabledExtensions,
          tcEnvMonomorphismRestriction = MonomorphismRestriction `elem` enabledExtensions,
          tcEnvScopedTypeVariables = ScopedTypeVariables `elem` enabledExtensions,
          tcEnvUndecidableInstances = UndecidableInstances `elem` enabledExtensions,
          tcEnvPostfixOperators = PostfixOperators `elem` enabledExtensions
        }
    enabledExtensions = resolvedModuleExtensions resolved
