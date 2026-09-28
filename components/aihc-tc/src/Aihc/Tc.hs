-- | Entry point for the aihc type checker.
--
-- The type checker consumes a parsed and name-resolved AST, together with
-- the language extensions in force for it, and produces the same AST
-- annotated with typing information. It does not transform the tree
-- structure, and it does not read the module's language pragmas: whoever
-- read the source folded the language edition, the package's default
-- extensions and the pragmas into the extension set of each 'ModuleUnit'.
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

    -- * Result types
    TcConfig,
    mkTcConfig,
    TcWiring (..),
    DerivingReferences (..),
    GenericReferences (..),
    UnliftedFieldReferences (..),
    DerivingReference (..),
    ReferencePackage (..),
    StockClassLocation (..),
    derivingReferenceList,
    TcBindingResult (..),
    defaultMethodName,
    TcTermKey (..),
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
    TcTypeKey,
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
    DataConSourceForm (..),
    dataConArgTypes,
    dataFamilyAxiomKey,
    dataFamilyAxiomName,
    dataFamilyRepresentationName,
    TypeFamilyInstanceInfo (..),
    typeFamilyAxiomKey,
    typeFamilyAxiomName,
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

import Aihc.Parser.Syntax (Extension (..), Module (moduleDecls))
import Aihc.Resolve (ModuleUnit (..))
import Aihc.Tc.Annotations (TcAnnotation (..), TcDerivingAnnotation (..), TcDerivingContext (..), TcDerivingPlan (..), TcDerivingStrategy (..), renderFunDepNames, renderPred, renderTcSignature, renderTcType, renderTcTypeInModule, renderTyLit)
import Aihc.Tc.Deriving.References (DerivingReference (..), DerivingReferences (..), GenericReferences (..), ReferencePackage (..), StockClassLocation (..), UnliftedFieldReferences (..), derivingReferenceList)
import Aihc.Tc.Diagnostics (annotateModuleDiagnostics, attachSccDiagnostics, collectTcDiagnostics, internalAbortDiagnostic)
import Aihc.Tc.Env (AssociatedTypeInfo (..), CType (..), ClassInfo (..), DataConFieldInfo (..), DataConFieldUnpack (..), DataConInfo (..), DataConSourceForm (..), DataFamilyInstanceInfo (..), DataTypeInfo (..), FunDep (..), InstanceInfo (..), PatSynDirection (..), PatSynInfo (..), TyConFlavor (..), TyConInfo (..), TypeFamilyInstanceInfo (..), dataConArgTypes, dataFamilyAxiomKey, dataFamilyAxiomName, dataFamilyRepresentationName, dataTypeKey, instanceEnvFromList, instanceEnvSince, instanceInfoKey, patSynKey, typeFamilyAxiomKey, typeFamilyAxiomName)
import Aihc.Tc.Error (TcDiagnostic (..), TcErrorKind (..), TcSeverity (..))
import Aihc.Tc.Generate.Decl (TcBindingResult (..), defaultMethodName, moduleBindings, tcModule, tcModuleScc)
import Aihc.Tc.Interface
import Aihc.Tc.Monad
import Aihc.Tc.Types
import Aihc.Tc.Wiring (mkTcKinds)
import Aihc.Tc.Zonk (finalizeDiagnostics)
import Data.List qualified as List
import Data.Map.Strict qualified as Map

-- | Top-level bindings recovered from a type-checked module's annotations.
tcModuleBindings :: TcWiring -> Module -> [TcBindingResult]
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

-- | Type-check dependency-ordered modules with an imported semantic interface.
-- Return only facts that the specified modules define.
typecheckModulesWithInterface :: TcConfig -> TcInterface -> [ModuleUnit] -> ([Module], TcInterface)
typecheckModulesWithInterface config imported units
  | all (null . moduleDecls . moduleUnitAst) units = (map moduleUnitAst units, emptyTcInterface)
  | otherwise =
      let initialState = initialTcState imported
          (finalState, checkedModules) = List.mapAccumL check initialState units
       in (checkedModules, tcInterfaceDifference initialState finalState)
  where
    check st m =
      let (result, st') = typecheckModuleWithState config st m
       in (st', result)

-- | Type-check one strongly connected module component using only the
-- supplied imported interface.
typecheckModuleSccWithInterface :: TcConfig -> TcInterface -> [ModuleUnit] -> ([Module], TcInterface)
typecheckModuleSccWithInterface config imported units
  -- Name resolution already checked imports and exports. Without declarations,
  -- the component adds no types, evidence, or diagnostics.
  | all (null . moduleDecls . moduleUnitAst) units = (map moduleUnitAst units, emptyTcInterface)
  | otherwise =
      let initialState = initialTcState imported
          (checkedModules, finalState) = typecheckModuleSccWithState config initialState units
       in (checkedModules, tcInterfaceDifference initialState finalState)

initialTcState :: TcInterface -> TcState
initialTcState imported =
  initTcState
    { tcsGlobalTerms = Map.map (`TcIdBinder` Closed) (tcInterfaceTermMap imported) <> tcsGlobalTerms initTcState,
      tcsGlobalTyCons = tcInterfaceTyConMap imported <> tcsGlobalTyCons initTcState,
      tcsDataTypes = tcInterfaceDataTypeMap imported,
      tcsClasses = tcInterfaceClassMap imported,
      tcsInstances = instanceEnvFromList (tcInterfaceInstances imported),
      tcsDataFamilyInstances = tcInterfaceDataFamilyInstanceMap imported,
      tcsTypeFamilyInstances = tcInterfaceTypeFamilyInstanceMap imported,
      tcsPatSyns = tcInterfacePatSynMap imported,
      tcsForeignImports = tcInterfaceForeignImportMap imported
    }

tcInterfaceDifference :: TcState -> TcState -> TcInterface
tcInterfaceDifference initial state =
  TcInterface
    { tcInterfaceTermMap = exportedGlobalTerms (newEntries (tcsGlobalTerms state) (tcsGlobalTerms initial)),
      tcInterfaceTyConMap = newEntries (tcsGlobalTyCons state) (tcsGlobalTyCons initial),
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
      tcInterfaceForeignImportMap = newEntries (tcsForeignImports state) (tcsForeignImports initial)
    }
  where
    -- These tables only gain keys. Equal sizes thus mean no new facts.
    newEntries current previous
      | Map.size current == Map.size previous = Map.empty
      | otherwise = Map.difference current previous

exportedGlobalTerms :: Map.Map TcTermKey TcBinder -> Map.Map TcTermKey TypeScheme
exportedGlobalTerms = Map.mapMaybe binderScheme
  where
    binderScheme binder =
      case binder of
        TcIdBinder scheme _ -> Just scheme
        _ -> Nothing

typecheckModuleSccWithState :: TcConfig -> TcState -> [ModuleUnit] -> ([Module], TcState)
typecheckModuleSccWithState config st units =
  case runTcM tcEnv (st {tcsDiagnostics = []}) (tcModuleScc units <* finalizeDiagnostics) of
    Left abort ->
      ( case map moduleUnitAst units of
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
    tcEnv =
      (emptyTcEnv config)
        { tcEnvMonoLocalBinds = any (elem MonoLocalBinds . moduleUnitExtensions) units,
          tcEnvMonomorphismRestriction = any (elem MonomorphismRestriction . moduleUnitExtensions) units,
          tcEnvScopedTypeVariables = any (elem ScopedTypeVariables . moduleUnitExtensions) units,
          tcEnvUndecidableInstances = any (elem UndecidableInstances . moduleUnitExtensions) units
        }

typecheckModuleWithState :: TcConfig -> TcState -> ModuleUnit -> (Module, TcState)
typecheckModuleWithState config st unit =
  case runTcM tcEnv (st {tcsDiagnostics = []}) (tcModule unit <* finalizeDiagnostics) of
    Left abort ->
      ( annotateModuleDiagnostics [internalAbortDiagnostic (tcAbortMessage abort)] (moduleUnitAst unit),
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
          tcEnvScopedTypeVariables = ScopedTypeVariables `elem` enabledExtensions
        }
    enabledExtensions = moduleUnitExtensions unit
