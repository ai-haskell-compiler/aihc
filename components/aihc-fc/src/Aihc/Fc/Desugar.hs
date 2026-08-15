{-# LANGUAGE OverloadedStrings #-}

-- | Desugaring from type-checked surface AST to System FC Core.
module Aihc.Fc.Desugar
  ( -- * Entry point
    desugarModuleWithInterface,
    DesugarConfig (..),
    DesugarResult (..),
  )
where

import Aihc.Fc.Desugar.Deriving (dsDerivingPlans, moduleDerivingPlans)
import Aihc.Fc.Desugar.Dictionary (checkedConstraintType, classMethodFieldType, defaultMethodName, peelForAlls, peelQuals, predType)
import Aihc.Fc.Desugar.Expr (ClassDict (..), DsM, DsState (..), desugarBug, dsEvidence, dsMatches, dsMatchesWithEnclosingDicts, freshUnique, freshVar, lookupType, withDicts)
import Aihc.Fc.Desugar.Match (dsDataConPure)
import Aihc.Fc.External (declareExternalSymbols)
import Aihc.Fc.Lower (lowerPseudoOps)
import Aihc.Fc.Newtype (lowerNewtypes)
import Aihc.Fc.Subst (freeRigidTyVars, substType)
import Aihc.Fc.Syntax
import Aihc.Parser.Syntax
  ( CallConv (..),
    ClassDecl (..),
    ClassDeclItem (..),
    DataConDecl (..),
    DataDecl (..),
    DataFamilyInst (..),
    Decl (..),
    Expr,
    FieldDecl (..),
    ForeignDecl (..),
    ForeignDirection (..),
    ForeignEntitySpec (..),
    ForeignSafety (..),
    GadtBody (..),
    InstanceDecl (..),
    InstanceDeclItem (..),
    Match (..),
    MatchHeadForm (..),
    Module (..),
    NewtypeDecl (..),
    Pattern (..),
    Rhs,
    UnqualifiedName (..),
    ValueDecl (..),
    binderHeadName,
    fromAnnotation,
    moduleName,
    nameQualifier,
    peelDeclAnn,
    unqualifiedNameText,
  )
import Aihc.Resolve (PackageId (..), ResolutionAnnotation (..), ResolvedName (..), packageIdText)
import Aihc.Tc (DataConFieldInfo (..), DataConInfo (..), DataFamilyInstanceInfo (..), DataTypeInfo (..), TcBindingResult (..), TcInterface (..), TcTermKey (..), TyConFlavor (..), TyConInfo (..), renderTcSignature, tcModuleBindings, tcModuleDiagnostics, tcModuleSuccess)
import Aihc.Tc.Annotations (TcAnnotation (..), TcClassAnnotation (..), TcClassMethodAnnotation (..), TcDictBinderAnnotation (..), TcForeignAbiType (..), TcForeignEffect (..), TcForeignImportAnnotation (..), TcForeignMarshal (..), TcInstanceAnnotation (..), TcInstanceMethodAnnotation (..))
import Aihc.Tc.Evidence (Coercion (..))
import Aihc.Tc.TypeScheme (equivalentTypeSchemes, parseTypeScheme, typeSchemeArity, typeSchemeFromType)
import Aihc.Tc.Types
  ( Kind (KConstraint, KFun, KMeta, KTYPE, KType),
    Pred (..),
    RuntimeRep (..),
    TcType (..),
    TyCon (..),
    TyVarId (..),
    TypeScheme (..),
    Unique (..),
    liftedRuntimeRep,
    mkTyCon,
    runtimeRepOfType,
    setTyConKindScheme,
    tvKind,
    tyConKind,
    tyConKindScheme,
    tyConModuleName,
    tyConPackageId,
    typeKind,
    unboxedTupleTyConName,
  )
import Control.Applicative ((<|>))
import Control.Monad (foldM, unless, zipWithM)
import Control.Monad.Trans.State.Strict (gets, modify', runStateT)
import Data.Either (fromRight)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- | Result of desugaring.
data DesugarResult = DesugarResult
  { dsProgram :: !FcProgram,
    dsSuccess :: !Bool,
    dsErrors :: ![String]
  }
  deriving (Show)

-- | Configuration for desugaring.
newtype DesugarConfig = DesugarConfig
  { primPackageId :: PackageId
  }
  deriving (Eq, Show)

-- | Desugar with the complete type-checker interface.
desugarModuleWithInterface :: DesugarConfig -> [TcBindingResult] -> TcInterface -> Module -> DesugarResult
desugarModuleWithInterface config bindings interface =
  desugarModule
    config
    bindings
    (tcInterfaceDataTypes interface)
    (interfaceTyConEnv interface)
    (interfaceGlobalVars interface)

desugarModule :: DesugarConfig -> [TcBindingResult] -> [DataTypeInfo] -> Map.Map (PackageId, Text, Text) TyCon -> Map.Map FcSymbolOrigin Var -> Module -> DesugarResult
desugarModule config bindings dataTypes globalTyConEnv globalVars tcResult =
  if not (tcModuleSuccess tcResult)
    then
      DesugarResult
        { dsProgram = FcProgram (sourceModuleId tcResult) [],
          dsSuccess = False,
          dsErrors = showTcFailure tcResult
        }
    else
      let typeEnv = Map.fromList (concatMap bindingTypeEntries bindings)
          (packageId, currentModuleName) = resolvedModuleOrigin tcResult
          constructorFields =
            Map.fromList
              [ (dciName constructor, dciFields constructor)
              | dataType <- dataTypes,
                dtiFlavor dataType == DataTyCon,
                constructor <- dtiConstructors dataType
              ]
          initialState =
            DsState
              { dsNextUnique = 1000,
                dsPrimPackageId = primPackageId config,
                dsModulePackage = packageId,
                dsModuleName = currentModuleName,
                dsTypeEnv = typeEnv,
                dsGlobalTyConEnv = globalTyConEnv,
                dsDataTypes = dataTypes,
                dsGlobalVars = globalVars,
                dsLocalVars = Map.empty,
                dsLocalDicts = Map.empty,
                dsConstructorFields = constructorFields,
                dsTupleConstructorOrigin = Nothing
              }
       in case runStateT (dsModule tcResult) initialState of
            Left err ->
              DesugarResult
                { dsProgram = FcProgram (sourceModuleId tcResult) [],
                  dsSuccess = False,
                  dsErrors = [err]
                }
            Right (binds, _) ->
              let program = FcProgram (FcModuleId packageId currentModuleName) binds
               in case lowerConstraintProgram (lowerNewtypes program) of
                    Left err ->
                      DesugarResult
                        { dsProgram = FcProgram (FcModuleId packageId currentModuleName) [],
                          dsSuccess = False,
                          dsErrors = [err]
                        }
                    Right loweredProgram ->
                      DesugarResult
                        { dsProgram = declareExternalSymbols (lowerPseudoOps loweredProgram),
                          dsSuccess = True,
                          dsErrors = []
                        }

interfaceTyConEnv :: TcInterface -> Map.Map (PackageId, Text, Text) TyCon
interfaceTyConEnv interface =
  Map.fromList
    [ ( (tyConPackageId tyCon, tyConModuleName tyCon, tyConName tyCon),
        tyCon
      )
    | tyConInfo <- tcInterfaceTyCons interface,
      let tyCon = tciTyCon tyConInfo
    ]

interfaceGlobalVars :: TcInterface -> Map.Map FcSymbolOrigin Var
interfaceGlobalVars interface =
  Map.fromList
    [ (origin, fcExternalVar origin (typeSchemeType scheme))
    | (key, scheme) <- tcInterfaceTerms interface,
      Just origin <- [termOrigin key]
    ]
  where
    termOrigin key =
      case key of
        TcTermLocal {} -> Nothing
        TcTermGlobal packageId moduleName' identifier
          | packageId == PackageId "" && moduleName' == "" -> Just (FcBuiltinOrigin identifier)
          | otherwise -> Just (FcTopLevelOrigin (packageIdText packageId) moduleName' identifier)

    typeSchemeType (ForAll variables predicates body) =
      foldr TcForAllTy (if null predicates then body else TcQualTy predicates body) variables

resolvedModuleOrigin :: Module -> (PackageId, Text)
resolvedModuleOrigin resolvedModule =
  fromMaybe ("", fromMaybe "Main" (moduleName resolvedModule)) $ do
    resolved <- listToMaybe (mapMaybe definitionResolution (moduleDecls resolvedModule))
    case resolutionTarget resolved of
      ResolvedTopLevel packageId name ->
        pure (packageId, fromMaybe (fromMaybe "Main" (moduleName resolvedModule)) (nameQualifier name))
      _ -> Nothing

sourceModuleId :: Module -> FcModuleId
sourceModuleId modu = FcModuleId "" (fromMaybe "Main" (moduleName modu))

definitionResolution :: Decl -> Maybe ResolutionAnnotation
definitionResolution declaration =
  case peelDeclAnn declaration of
    DeclValue (FunctionBind name _) -> nameResolution name
    DeclValue (PatternBind _ pattern' _) -> patternResolution pattern'
    DeclData dataDeclaration -> nameResolution (binderHeadName (dataDeclHead dataDeclaration))
    DeclNewtype newtypeDeclaration -> nameResolution (binderHeadName (newtypeDeclHead newtypeDeclaration))
    DeclClass classDeclaration -> nameResolution (binderHeadName (classDeclHead classDeclaration))
    _ -> Nothing

patternResolution :: Pattern -> Maybe ResolutionAnnotation
patternResolution pattern' =
  case pattern' of
    PVar name -> nameResolution name
    PAnn _ inner -> patternResolution inner
    PParen inner -> patternResolution inner
    PStrict inner -> patternResolution inner
    PIrrefutable inner -> patternResolution inner
    PAs name _ -> nameResolution name
    PTypeSig inner _ -> patternResolution inner
    _ -> Nothing

nameResolution :: UnqualifiedName -> Maybe ResolutionAnnotation
nameResolution = listToMaybe . mapMaybe fromAnnotation . unqualifiedNameAnns

-- | Type-class evidence is ordinary term-level data in FC. Replace qualified
-- source types with explicit dictionary arrows after desugaring has consumed
-- their predicate structure.
lowerConstraintProgram :: FcProgram -> Either String FcProgram
lowerConstraintProgram (FcProgram moduleId topBinds) =
  FcProgram moduleId <$> mapM lowerTopBind topBinds
  where
    lowerTopBind topBind =
      case topBind of
        FcExternal origin ty -> FcExternal origin <$> lowerConstraintType ty
        FcData declaration ->
          FcData . (\constructors -> declaration {fcDataConstructors = constructors})
            <$> mapM lowerDataConstructor (fcDataConstructors declaration)
        FcAxiom declaration ->
          do
            left <- lowerConstraintType (fcAxiomLeft declaration)
            right <- lowerConstraintType (fcAxiomRight declaration)
            pure (FcAxiom declaration {fcAxiomLeft = left, fcAxiomRight = right})
        FcNewtype declaration ->
          do
            representation <- lowerConstraintType (fcNewtypeRepresentation declaration)
            result <- lowerConstraintType (fcNewtypeResult declaration)
            pure (FcNewtype declaration {fcNewtypeRepresentation = representation, fcNewtypeResult = result})
        FcPrimitive var arity -> (`FcPrimitive` arity) <$> lowerVar var
        FcForeignImport foreignCall -> pure (FcForeignImport foreignCall)
        FcTopBind bind -> FcTopBind <$> lowerBind bind

    lowerDataConstructor constructor = do
      fields <- mapM lowerConstraintType (fcDataConFields constructor)
      pure constructor {fcDataConFields = fields}

    lowerBind bind =
      case bind of
        FcNonRec var expression -> FcNonRec <$> lowerVar var <*> lowerExpr expression
        FcRec bindings -> FcRec <$> mapM (\(var, expression) -> (,) <$> lowerVar var <*> lowerExpr expression) bindings

    lowerExpr expression =
      case expression of
        FcVar var -> FcVar <$> lowerVar var
        FcLit {} -> pure expression
        FcApp function argument -> FcApp <$> lowerExpr function <*> lowerExpr argument
        FcTyApp function ty -> FcTyApp <$> lowerExpr function <*> lowerConstraintType ty
        FcLam var body -> FcLam <$> lowerVar var <*> lowerExpr body
        FcTyLam tyVar body -> FcTyLam tyVar <$> lowerExpr body
        FcLet bind body -> FcLet <$> lowerBind bind <*> lowerExpr body
        FcCase scrutinee binder alternatives ->
          FcCase <$> lowerExpr scrutinee <*> lowerVar binder <*> mapM lowerAlt alternatives
        FcCast inner coercion -> FcCast <$> lowerExpr inner <*> lowerCoercion coercion
        FcCallForeign foreignCall arguments -> FcCallForeign foreignCall <$> mapM lowerExpr arguments

    lowerAlt alternative = do
      binders <- mapM lowerVar (altBinders alternative)
      rhs <- lowerExpr (altRhs alternative)
      pure alternative {altBinders = binders, altRhs = rhs}

    lowerVar var = (\ty -> var {varType = ty}) <$> lowerConstraintType (varType var)

lowerConstraintType :: TcType -> Either String TcType
lowerConstraintType ty =
  case ty of
    TcTyVar {} -> pure ty
    TcMetaTv {} -> Left ("non-final checked type reached constraint lowering: " <> show ty)
    TcTyCon tyCon arguments -> do
      arguments' <- mapM lowerConstraintType arguments
      tyCon' <- lowerConstraintTyCon tyCon
      pure (TcTyCon tyCon' arguments')
    TcFunTy argument result -> TcFunTy <$> lowerConstraintType argument <*> lowerConstraintType result
    TcForAllTy tyVar body -> TcForAllTy tyVar <$> lowerConstraintType body
    TcQualTy predicates body -> do
      body' <- lowerConstraintType body
      predicateTypes <- mapM lowerPredicateType predicates
      pure (foldr TcFunTy body' predicateTypes)
    TcAppTy function argument -> TcAppTy <$> lowerConstraintType function <*> lowerConstraintType argument
    TcBuiltinTyCon name arity arguments -> TcBuiltinTyCon name arity <$> mapM lowerConstraintType arguments

lowerPredicateType :: Pred -> Either String TcType
lowerPredicateType predicate =
  case predicate of
    ClassPred classTyCon arguments -> do
      classTyCon' <- lowerClassTyCon classTyCon
      TcTyCon classTyCon' <$> mapM lowerConstraintType arguments
    EqPred {} -> Left "equality constraint lowering requires explicit coercion evidence"

lowerConstraintTyCon :: TyCon -> Either String TyCon
lowerConstraintTyCon tyCon =
  case terminalKind (tyConKind tyCon) of
    KConstraint -> lowerClassTyCon tyCon
    KMeta {} -> Left ("non-final checked kind for type constructor " <> T.unpack (tyConName tyCon))
    _ -> pure tyCon

lowerClassTyCon :: TyCon -> Either String TyCon
lowerClassTyCon tyCon = do
  scheme <- lowerClassKindScheme (tyConKindScheme tyCon)
  pure (setTyConKindScheme scheme tyCon)

lowerClassKindScheme :: TypeScheme -> Either String TypeScheme
lowerClassKindScheme (ForAll tyVars [] body) = ForAll tyVars [] <$> lowerResult body
  where
    lowerResult kindType =
      case kindType of
        TcFunTy argument result -> TcFunTy argument <$> lowerResult result
        TcBuiltinTyCon "Constraint" 0 [] -> pure (TcBuiltinTyCon "Type" 0 [])
        TcMetaTv {} -> Left "class type constructor has a non-final checked result kind"
        _ -> Left ("class type constructor does not have an authoritative Constraint result kind: " <> show kindType)
lowerClassKindScheme ForAll {} = Left "class type constructor kind scheme contains predicates"

terminalKind :: Kind -> Kind
terminalKind kind =
  case kind of
    KFun _ result -> terminalKind result
    _ -> kind

lowerCoercion :: Coercion -> Either String Coercion
lowerCoercion coercion =
  case coercion of
    CoVar {} -> pure coercion
    Refl ty -> Refl <$> lowerConstraintType ty
    Sym inner -> Sym <$> lowerCoercion inner
    Trans left right -> Trans <$> lowerCoercion left <*> lowerCoercion right
    TyConAppCo tyCon coercions -> TyConAppCo tyCon <$> mapM lowerCoercion coercions
    AxiomInstCo name types -> AxiomInstCo name <$> mapM lowerConstraintType types

-- | Format a binding result for error messages.
showBinding :: TcBindingResult -> String
showBinding b = renderTcSignature (tbDisplayName b) (tbType b)

showTcFailure :: Module -> [String]
showTcFailure tcResult =
  case map show (tcModuleDiagnostics tcResult) of
    [] -> map showBinding (tcModuleBindings tcResult)
    diagnostics -> diagnostics

bindingTypeEntries :: TcBindingResult -> [(Text, TcType)]
bindingTypeEntries b =
  [(tbName b, tbType b)]

-- | Desugar a module's declarations.
dsModule :: Module -> DsM [FcTopBind]
dsModule m = do
  let decls = moduleDecls m
  -- Phase 1: data declarations and class method selectors.
  dataTops <- concat <$> mapM dsDecl decls
  -- Phase 2: instance dictionaries.
  instanceTops <- concat <$> mapM dsInstanceDecl (moduleInstances decls)
  derivingTops <- dsDerivingPlans (moduleDerivingPlans decls)
  -- Phase 3: group and desugar value bindings.
  let grouped = groupFunctionBinds decls
  groupedVars <- mapM allocateGroupVar grouped
  let globalEntries = [(localValueOrigin m name, var) | (group, var) <- groupedVars, let name = dgName group]
  modify' (\state -> state {dsGlobalVars = foldr (uncurry Map.insert) (dsGlobalVars state) globalEntries})
  valueBindings <- mapM dsGroup groupedVars
  let valueTops = [FcTopBind (FcRec valueBindings) | not (null valueBindings)]
  pure (dataTops ++ instanceTops ++ derivingTops ++ valueTops)

allocateGroupVar :: DeclGroup -> DsM (DeclGroup, Var)
allocateGroupVar group = do
  ty <- lookupType (dgName group)
  var <- freshVar (dgName group) ty
  pure (group, var)

localValueOrigin :: Module -> Text -> FcSymbolOrigin
localValueOrigin modu name =
  let (packageId, moduleName') = resolvedModuleOrigin modu
   in FcTopLevelOrigin (packageIdText packageId) moduleName' name

-- | Desugar a single declaration (data types only; values handled by groups).
dsDecl :: Decl -> DsM [FcTopBind]
dsDecl (DeclData dd) = dsDataDeclM dd
dsDecl (DeclNewtype nd) = dsNewtypeDeclM nd
dsDecl (DeclAnn ann (DeclDataFamilyInst familyInst))
  | Just familyInfo <- fromAnnotation ann = dsDataFamilyInstM familyInfo familyInst
dsDecl (DeclAnn ann inner)
  | Just foreignAnn <- fromAnnotation ann,
    Just (tcAnn, foreignDecl) <- annotatedForeignDecl inner =
      dsForeignImport tcAnn (Just foreignAnn) foreignDecl
dsDecl (DeclAnn ann (DeclForeign foreignDecl))
  | Just tcAnn <- fromAnnotation ann = dsForeignImport tcAnn Nothing foreignDecl
dsDecl (DeclAnn ann (DeclClass classDecl))
  | Just classAnn <- fromAnnotation ann = dsClassDeclM classDecl classAnn
dsDecl (DeclAnn _ inner) = dsDecl inner
dsDecl DeclClass {} = desugarBug "missing type-checker annotation for class declaration"
dsDecl DeclDataFamilyInst {} = desugarBug "missing type-checker annotation for data-family instance"
dsDecl _ = pure []

-- | Desugar a data declaration.
dsDataDeclM :: DataDecl -> DsM [FcTopBind]
dsDataDeclM dd = do
  let tyName = unqualifiedNameText (binderHeadName (dataDeclHead dd))
  dataType <- lookupCheckedDataType DataTyCon tyName
  let tyOrigin = checkedDataTypeOrigin dataType
      typeVariables = dtiTyVars dataType
      resultKind = dtiResultKind dataType
      constructorInfos = map checkedConstructorInfo (dtiConstructors dataType)
      constructors = map checkedFcDataCon (dtiConstructors dataType)
  selectors <- dsRecordSelectors [(name, variables, fields) | (name, variables, fields, _) <- constructorInfos] (dataDeclConstructors dd)
  pure (FcData (FcDataDecl tyOrigin tyName typeVariables resultKind constructors) : selectors)

lookupCheckedDataType :: TyConFlavor -> Text -> DsM DataTypeInfo
lookupCheckedDataType expectedFlavor expectedName = do
  packageId <- gets dsModulePackage
  moduleName' <- gets dsModuleName
  dataTypes <- gets dsDataTypes
  let matches =
        [ info
        | info <- dataTypes,
          dtiName info == expectedName,
          tyConPackageId (dtiTyCon info) == packageId,
          tyConModuleName (dtiTyCon info) == moduleName'
        ]
      qualifiedName = T.unpack moduleName' <> "." <> T.unpack expectedName
  case matches of
    [] -> desugarBug ("missing checked data type information for " <> qualifiedName)
    [info] -> do
      validateCheckedDataType expectedFlavor qualifiedName info
      pure info
    _ -> desugarBug ("duplicate checked data type information for " <> qualifiedName)

validateCheckedDataType :: TyConFlavor -> String -> DataTypeInfo -> DsM ()
validateCheckedDataType expectedFlavor qualifiedName info = do
  unless (dtiFlavor info == expectedFlavor) $
    desugarBug ("invalid checked data type flavor for " <> qualifiedName)
  unless (length (dtiTyVars info) == tyConArity (dtiTyCon info)) $
    desugarBug ("invalid checked data type arity for " <> qualifiedName)
  unless (all (isFinalKind . tvKind) (dtiTyVars info)) $
    desugarBug ("non-final checked parameter kind for " <> qualifiedName)
  unless (isFinalValueKind (dtiResultKind info)) $
    desugarBug ("invalid checked result kind for " <> qualifiedName)

isFinalValueKind :: Kind -> Bool
isFinalValueKind kind =
  case kind of
    KTYPE runtimeRep -> isFinalRuntimeRep runtimeRep
    _ -> False

isFinalKind :: Kind -> Bool
isFinalKind kind =
  case kind of
    KTYPE runtimeRep -> isFinalRuntimeRep runtimeRep
    KFun argument result -> isFinalKind argument && isFinalKind result
    KMeta {} -> False
    _ -> True

isFinalRuntimeRep :: RuntimeRep -> Bool
isFinalRuntimeRep runtimeRep =
  case runtimeRep of
    RuntimeRepMeta {} -> False
    TupleRep fields -> all isFinalRuntimeRep fields
    SumRep fields -> all isFinalRuntimeRep fields
    _ -> True

checkedDataTypeOrigin :: DataTypeInfo -> FcSymbolOrigin
checkedDataTypeOrigin info =
  let tyCon = dtiTyCon info
   in FcTopLevelOrigin (packageIdText (tyConPackageId tyCon)) (tyConModuleName tyCon) (dtiName info)

checkedConstructorInfo :: DataConInfo -> (Text, [TyVarId], [TcType], TcType)
checkedConstructorInfo info =
  ( dciName info,
    dciUnivTyVars info,
    map predType (dciTheta info) <> map dcfiType (dciFields info),
    dciResTy info
  )

checkedFcDataCon :: DataConInfo -> FcDataConDecl
checkedFcDataCon info =
  let (packageId, moduleName') = dciOrigin info
      origin = FcTopLevelOrigin (packageIdText packageId) moduleName' (dciName info)
   in FcDataConDecl (fcConstructorIdFromSymbol origin) (dciName info) (map predType (dciTheta info) <> map dcfiType (dciFields info))

-- | Retain a data-family instance as a fresh representation type and a
-- nominal axiom connecting that representation to the family application.
dsDataFamilyInstM :: DataFamilyInstanceInfo -> DataFamilyInst -> DsM [FcTopBind]
dsDataFamilyInstM familyInfo familyInst = do
  constructorInfos <- mapM dsDataConM (dataFamilyInstConstructors familyInst)
  case constructorInfos of
    [] -> desugarBug "data-family instance has no constructors"
    _ -> do
      let representationTyCon = dfiiRepresentationTyCon familyInfo
          representationName = tyConName representationTyCon
          representationTyVars = dfiiTyVars familyInfo
          representationType = TcTyCon representationTyCon (map TcTyVar representationTyVars)
          axiom =
            FcAxiom
              FcAxiomDecl
                { fcAxiomName = dfiiAxiomName familyInfo,
                  fcAxiomTyVars = representationTyVars,
                  fcAxiomRole = FcNominal,
                  fcAxiomLeft = dfiiFamilyType familyInfo,
                  fcAxiomRight = representationType
                }
      representation <- dataFamilyRepresentation familyInst representationName representationTyVars representationType constructorInfos
      pure [representation, axiom]

dataFamilyRepresentation :: DataFamilyInst -> Text -> [TyVarId] -> TcType -> [(Text, [TyVarId], [TcType], TcType)] -> DsM FcTopBind
dataFamilyRepresentation familyInst representationName representationTyVars representationType constructorInfos
  | dataFamilyInstIsNewtype familyInst =
      case constructorInfos of
        [(constructorName, _, [fieldType], _)] -> do
          newtypeOrigin <- localDeclarationOrigin representationName
          constructorOrigin <- localDeclarationOrigin constructorName
          pure
            ( FcNewtype
                FcNewtypeDecl
                  { fcNewtypeOrigin = newtypeOrigin,
                    fcNewtypeName = representationName,
                    fcNewtypeTyVars = representationTyVars,
                    fcNewtypeConstructorOrigin = fcConstructorIdFromSymbol constructorOrigin,
                    fcNewtypeConstructor = constructorName,
                    fcNewtypeRepresentation = fieldType,
                    fcNewtypeResult = representationType
                  }
            )
        _ -> desugarBug "newtype family instance does not have exactly one constructor with one field"
  | otherwise =
      do
        dataOrigin <- localDeclarationOrigin representationName
        constructors <- mapM (\(name, _, fields, _) -> (FcDataConDecl . fcConstructorIdFromSymbol <$> localDeclarationOrigin name) <*> pure name <*> pure fields) constructorInfos
        pure
          ( FcData
              (FcDataDecl dataOrigin representationName representationTyVars (typeKind representationType) constructors)
          )

-- | Retain the nominal declaration and its representation type as an FC axiom.
-- 'lowerNewtypes' turns all term-level construction and matching into casts.
dsNewtypeDeclM :: NewtypeDecl -> DsM [FcTopBind]
dsNewtypeDeclM nd = do
  let tyName = unqualifiedNameText (binderHeadName (newtypeDeclHead nd))
  dataType <- lookupCheckedDataType NewtypeTyCon tyName
  case (newtypeDeclConstructor nd, dtiConstructors dataType) of
    (Nothing, _) -> desugarBug ("newtype " <> T.unpack tyName <> " has no constructor")
    (Just con, [constructorInfo]) ->
      case (dciTheta constructorInfo, dciFields constructorInfo) of
        ([], [field]) -> do
          let conName = dciName constructorInfo
              tyVars = dtiTyVars dataType
              fieldTy = dcfiType field
              resultTy = dciResTy constructorInfo
              constructorOrigin = fcDataConOrigin (checkedFcDataCon constructorInfo)
              constructorFields = [checkedConstructorInfo constructorInfo]
          selectors <- dsRecordSelectors [(name, variables, fields) | (name, variables, fields, _) <- constructorFields] [con]
          pure
            ( FcNewtype
                FcNewtypeDecl
                  { fcNewtypeOrigin = checkedDataTypeOrigin dataType,
                    fcNewtypeName = tyName,
                    fcNewtypeTyVars = tyVars,
                    fcNewtypeConstructorOrigin = constructorOrigin,
                    fcNewtypeConstructor = conName,
                    fcNewtypeRepresentation = fieldTy,
                    fcNewtypeResult = resultTy
                  }
                : selectors
            )
        _ -> desugarBug ("newtype constructor " <> T.unpack (dciName constructorInfo) <> " does not have exactly one checked field")
    (Just _, _) -> desugarBug ("newtype " <> T.unpack tyName <> " does not have exactly one checked constructor")

dsRecordSelectors :: [(Text, [TyVarId], [TcType])] -> [DataConDecl] -> DsM [FcTopBind]
dsRecordSelectors constructorInfos declarations =
  mapM dsSelector (Map.toList selectorLayouts)
  where
    selectorLayouts =
      Map.fromListWith
        (++)
        [ (label, [(constructorName, fieldIndex)])
        | declaration <- declarations,
          (constructorName, fields) <- recordConstructorLayouts declaration,
          (fieldIndex, label) <- zip [0 :: Int ..] fields
        ]
    dsSelector (selectorName, layouts) = do
      selectorType <- lookupType selectorName
      let (typeVariables, qualifiedBody) = peelForAlls selectorType
          (predicates, bodyType) = peelQuals qualifiedBody
      (recordType, _fieldType) <-
        case bodyType of
          TcFunTy argument result -> pure (argument, result)
          _ -> desugarBug ("record selector is not a function: " <> T.unpack selectorName)
      selectorVar <- freshVar selectorName selectorType
      dictionaryVars <-
        zipWithM
          (\index predicate -> freshVar ("$d" <> T.pack (show index)) (predType predicate))
          [0 :: Int ..]
          predicates
      recordVar <- freshVar "$record" recordType
      caseBinder <- freshVar "$record_case" recordType
      alternatives <- mapM (selectorAlternative layouts) constructorInfos
      let matchingAlternatives = catMaybes alternatives
      failureBinder <- freshVar "$record_selector_failure" recordType
      let needsDefault = length matchingAlternatives < length constructorInfos
          failure = FcCase (FcVar recordVar) failureBinder []
          completeAlternatives =
            matchingAlternatives
              <> [FcAlt DefaultAlt [] failure | needsDefault]
          selection = FcCase (FcVar recordVar) caseBinder completeAlternatives
          body = foldr FcTyLam (foldr FcLam (FcLam recordVar selection) dictionaryVars) typeVariables
      pure (FcTopBind (FcNonRec selectorVar body))
    selectorAlternative layouts (constructorName, _, fieldTypes) =
      case lookup constructorName layouts of
        Nothing -> pure Nothing
        Just fieldIndex -> do
          let sourceArity = maybe 0 length (lookup constructorName allConstructorFields)
              evidenceCount = length fieldTypes - sourceArity
              selectedIndex = evidenceCount + fieldIndex
          fieldBinders <-
            zipWithM
              (\index fieldType -> freshVar ("$field" <> T.pack (show index)) fieldType)
              [0 :: Int ..]
              fieldTypes
          constructorOrigin <- localDeclarationOrigin constructorName
          case drop selectedIndex fieldBinders of
            selected : _ -> pure (Just (FcAlt (DataAlt (fcConstructorIdFromSymbol constructorOrigin)) fieldBinders (FcVar selected)))
            [] -> desugarBug ("record selector field index is out of range: " <> T.unpack constructorName)
    allConstructorFields = concatMap recordConstructorLayouts declarations

recordConstructorLayouts :: DataConDecl -> [(Text, [Text])]
recordConstructorLayouts declaration =
  case declaration of
    DataConAnn _ inner -> recordConstructorLayouts inner
    RecordCon _ _ constructor fields ->
      [(unqualifiedNameText constructor, concatMap (map unqualifiedNameText . fieldNames) fields)]
    GadtCon _ _ constructors (GadtRecordBody fields _) ->
      [ (unqualifiedNameText constructor, concatMap (map unqualifiedNameText . fieldNames) fields)
      | constructor <- constructors
      ]
    _ -> []

annotatedForeignDecl :: Decl -> Maybe (TcAnnotation, ForeignDecl)
annotatedForeignDecl = go Nothing
  where
    go maybeTc decl =
      case decl of
        DeclAnn ann inner -> go (fromAnnotation ann <|> maybeTc) inner
        DeclForeign foreignDecl -> (,foreignDecl) <$> maybeTc
        _ -> Nothing

dsForeignImport :: TcAnnotation -> Maybe TcForeignImportAnnotation -> ForeignDecl -> DsM [FcTopBind]
dsForeignImport tcAnn foreignPlan foreignDecl
  | foreignDirection foreignDecl /= ForeignImport =
      desugarBug "unsupported foreign export after type checking"
  | otherwise =
      case foreignCallConv foreignDecl of
        CPrim -> (: []) <$> dsForeignPrim tcAnn foreignDecl
        CCall ->
          case foreignPlan of
            Just plan -> dsForeignCcall tcAnn plan foreignDecl
            Nothing -> desugarBug "missing type-checker foreign import plan"
        callConv -> desugarBug ("unsupported foreign calling convention after type checking: " <> show callConv)

dsForeignPrim :: TcAnnotation -> ForeignDecl -> DsM FcTopBind
dsForeignPrim tcAnn foreignDecl = do
  let name = unqualifiedNameText (foreignName foreignDecl)
      ty = tcAnnType tcAnn
  arity <- validatePrimitiveImport name ty
  unique <- freshUnique
  pure (FcPrimitive (Var name unique ty) arity)

dsForeignCcall :: TcAnnotation -> TcForeignImportAnnotation -> ForeignDecl -> DsM [FcTopBind]
dsForeignCcall tcAnn foreignPlan foreignDecl = do
  if foreignSafety foreignDecl == Just Unsafe
    then pure ()
    else desugarBug "only unsafe foreign imports are supported"
  symbol <-
    case foreignEntity foreignDecl of
      ForeignEntityNamed name -> pure name
      ForeignEntityStatic (Just name) -> pure name
      ForeignEntityOmitted -> pure (unqualifiedNameText (foreignName foreignDecl))
      _ -> desugarBug "only statically named foreign imports are supported"
  let name = unqualifiedNameText (foreignName foreignDecl)
      wrapperType = tcAnnType tcAnn
      signature =
        FcForeignSignature
          { fcForeignArgumentTypes = map (lowerForeignAbiType . tcForeignAbiType) (tcForeignArguments foreignPlan),
            fcForeignResultType = lowerForeignAbiType (tcForeignAbiType (tcForeignResult foreignPlan)),
            fcForeignEffect = lowerForeignEffect (tcForeignEffect foreignPlan)
          }
      foreignCall =
        FcForeignCall
          { fcForeignCallName = "$ffi$" <> name,
            fcForeignCallSymbol = symbol,
            fcForeignCallSignature = signature
          }
  wrapperVar <- freshVar name wrapperType
  argumentVars <-
    mapM
      (\(index, marshal) -> freshVar ("$ffi_arg_" <> T.pack (show index)) (tcForeignSourceType marshal))
      (zip [0 :: Int ..] (tcForeignArguments foreignPlan))
  wrapperBody <-
    unboxForeignArguments (zip argumentVars (tcForeignArguments foreignPlan)) $ \arguments ->
      case tcForeignEffect foreignPlan of
        TcForeignPure ->
          boxForeignValue (tcForeignResult foreignPlan) (FcCallForeign foreignCall arguments)
        TcForeignRealWorld -> makeForeignIoWrapper foreignCall (tcForeignResult foreignPlan) arguments
  pure
    [ FcForeignImport foreignCall,
      FcTopBind (FcNonRec wrapperVar (foldr FcLam wrapperBody argumentVars))
    ]

lowerForeignAbiType :: TcForeignAbiType -> FcForeignType
lowerForeignAbiType foreignType =
  case foreignType of
    TcForeignInt -> FcForeignInt
    TcForeignInt32 -> FcForeignInt32
    TcForeignWord64 -> FcForeignWord64
    TcForeignAddr -> FcForeignAddr

lowerForeignEffect :: TcForeignEffect -> FcForeignEffect
lowerForeignEffect effect =
  case effect of
    TcForeignPure -> FcForeignPure
    TcForeignRealWorld -> FcForeignRealWorld

unboxForeignArguments :: [(Var, TcForeignMarshal)] -> ([FcExpr] -> DsM FcExpr) -> DsM FcExpr
unboxForeignArguments arguments continuation = go arguments []
  where
    go [] values = continuation (reverse values)
    go ((var, marshal) : rest) values =
      unboxForeignValue (varName var) marshal (FcVar var) $ \value -> go rest (value : values)

unboxForeignValue :: Text -> TcForeignMarshal -> FcExpr -> (FcExpr -> DsM FcExpr) -> DsM FcExpr
unboxForeignValue binderName marshal expression continuation =
  go (tcForeignSourceType marshal) (tcForeignConstructors marshal) expression
  where
    go _ [] value = continuation value
    go valueType (constructor : constructors) value = do
      constructorType <- dropForAlls <$> lookupType constructor
      fieldType <-
        case constructorType of
          TcFunTy field _ -> pure field
          _ -> desugarBug ("foreign marshalling constructor is not unary: " <> T.unpack constructor)
      caseBinder <- freshVar (binderName <> "_case") valueType
      fieldBinder <- freshVar (binderName <> "_field") fieldType
      rhs <- go fieldType constructors (FcVar fieldBinder)
      constructorOrigin <- typeDataConOrigin valueType constructor
      pure
        ( FcCase
            value
            caseBinder
            [FcAlt (DataAlt (fcConstructorIdFromSymbol constructorOrigin)) [fieldBinder] rhs]
        )

boxForeignValue :: TcForeignMarshal -> FcExpr -> DsM FcExpr
boxForeignValue marshal =
  applyConstructors (tcForeignSourceType marshal) (tcForeignConstructors marshal)
  where
    applyConstructors _ [] value = pure value
    applyConstructors resultType (constructor : constructors) value = do
      constructorType <- lookupType constructor
      constructorVar <- freshVar constructor constructorType
      (typeArguments, fieldType) <- instantiateUnaryConstructor constructor resultType constructorType
      fieldValue <- applyConstructors fieldType constructors value
      constructorOrigin <- typeDataConOrigin resultType constructor
      pure (FcApp (foldl FcTyApp (FcVar constructorVar {varResolvedName = Just constructorOrigin}) typeArguments) fieldValue)

instantiateUnaryConstructor :: Text -> TcType -> TcType -> DsM ([TcType], TcType)
instantiateUnaryConstructor constructor expectedResult constructorType = do
  let (typeVariables, body) = collectForAlls constructorType
  case body of
    TcFunTy fieldType resultType ->
      case matchConstructorResult typeVariables resultType expectedResult Map.empty of
        Just substitution
          | Just typeArguments <- traverse (`Map.lookup` substitution) typeVariables ->
              pure (typeArguments, substType substitution fieldType)
        _ ->
          desugarBug
            ( "foreign marshalling constructor result does not match "
                <> show expectedResult
                <> ": "
                <> T.unpack constructor
            )
    _ -> desugarBug ("foreign marshalling constructor is not unary: " <> T.unpack constructor)

matchConstructorResult :: [TyVarId] -> TcType -> TcType -> Map.Map TyVarId TcType -> Maybe (Map.Map TyVarId TcType)
matchConstructorResult quantified patternType actualType substitution =
  case patternType of
    TcTyVar tyVar
      | tyVar `elem` quantified ->
          case Map.lookup tyVar substitution of
            Nothing -> Just (Map.insert tyVar actualType substitution)
            Just existing
              | existing == actualType -> Just substitution
              | otherwise -> Nothing
    TcTyVar tyVar ->
      case actualType of
        TcTyVar actualTyVar | tyVar == actualTyVar -> Just substitution
        _ -> Nothing
    TcMetaTv meta ->
      case actualType of
        TcMetaTv actualMeta | meta == actualMeta -> Just substitution
        _ -> Nothing
    TcTyCon tyCon arguments ->
      case actualType of
        TcTyCon actualTyCon actualArguments
          | tyCon == actualTyCon,
            length arguments == length actualArguments ->
              foldM
                (\current (expectedArgument, actualArgument) -> matchConstructorResult quantified expectedArgument actualArgument current)
                substitution
                (zip arguments actualArguments)
        _ -> Nothing
    TcFunTy argument result ->
      case actualType of
        TcFunTy actualArgument actualResult -> do
          substitution' <- matchConstructorResult quantified argument actualArgument substitution
          matchConstructorResult quantified result actualResult substitution'
        _ -> Nothing
    TcAppTy function argument ->
      case actualType of
        TcAppTy actualFunction actualArgument -> do
          substitution' <- matchConstructorResult quantified function actualFunction substitution
          matchConstructorResult quantified argument actualArgument substitution'
        _ -> Nothing
    TcForAllTy {} -> Nothing
    TcQualTy {} -> Nothing
    TcBuiltinTyCon name arity arguments ->
      case actualType of
        TcBuiltinTyCon actualName actualArity actualArguments
          | name == actualName,
            arity == actualArity,
            length arguments == length actualArguments ->
              foldM
                (\current (expectedArgument, actualArgument) -> matchConstructorResult quantified expectedArgument actualArgument current)
                substitution
                (zip arguments actualArguments)
        _ -> Nothing

makeForeignIoWrapper :: FcForeignCall -> TcForeignMarshal -> [FcExpr] -> DsM FcExpr
makeForeignIoWrapper foreignCall resultMarshal arguments = do
  stateVar <- freshVar "$ffi_state" statePrimRealWorldTy
  tupleBinder <- freshVar "$ffi_result" (fcForeignCallResultType (fcForeignCallSignature foreignCall))
  nextStateVar <- freshVar "$ffi_next_state" statePrimRealWorldTy
  rawResultVar <- freshVar "$ffi_raw_result" (tcForeignPrimitiveType resultMarshal)
  boxedResult <- boxForeignValue resultMarshal (FcVar rawResultVar)
  tupleOrigin <- primitiveConstructorOrigin "(#,#)"
  tupleConstructorVar <-
    freshVar
      "(#,#)"
      (TcFunTy statePrimRealWorldTy (TcFunTy (tcForeignSourceType resultMarshal) (unboxedTupleTy [statePrimRealWorldTy, tcForeignSourceType resultMarshal])))
  let tupleConstructor = tupleConstructorVar
  ioConstructorType <- lookupType "IO"
  ioConstructor <- freshVar "IO" ioConstructorType
  let resultTuple = FcApp (FcApp (FcVar tupleConstructor) (FcVar nextStateVar)) boxedResult
      call = FcCallForeign foreignCall (arguments <> [FcVar stateVar])
      stateAction =
        FcLam
          stateVar
          ( FcCase
              call
              tupleBinder
              [FcAlt (DataAlt (fcConstructorIdFromSymbol tupleOrigin)) [nextStateVar, rawResultVar] resultTuple]
          )
  pure (FcApp (FcTyApp (FcVar ioConstructor) (tcForeignSourceType resultMarshal)) stateAction)

validatePrimitiveImport :: Text -> TcType -> DsM Int
validatePrimitiveImport name ty =
  case Map.lookup name primitiveImportSpecs of
    Nothing ->
      desugarBug ("unknown foreign import prim: " <> T.unpack name)
    Just spec
      | equivalentTypeSchemes (primitiveSpecExpected spec) (typeSchemeFromType ty) ->
          pure (typeSchemeArity (primitiveSpecExpected spec))
      | otherwise ->
          desugarBug
            ( "incorrect type for foreign import prim "
                <> T.unpack name
                <> "; expected "
                <> T.unpack (primitiveSpecSource spec)
                <> ", got "
                <> renderTcSignature "" ty
            )

data PrimitiveSpec = PrimitiveSpec
  { primitiveSpecSource :: !Text,
    primitiveSpecExpected :: !TypeScheme
  }

primitiveImportSpecs :: Map.Map Text PrimitiveSpec
primitiveImportSpecs =
  Map.fromList
    [ primitive "+#" "Int# -> Int# -> Int#",
      primitive "-#" "Int# -> Int# -> Int#",
      primitive "*#" "Int# -> Int# -> Int#",
      primitive "addIntC#" "Int# -> Int# -> (# Int#, Int# #)",
      primitive "subIntC#" "Int# -> Int# -> (# Int#, Int# #)",
      primitive "compareInt#" "Int# -> Int# -> Int#",
      primitive "<#" "Int# -> Int# -> Int#",
      primitive "==#" "Int# -> Int# -> Int#",
      primitive "ord#" "Char# -> Int#",
      primitive "chr#" "Int# -> Char#",
      primitive "plusWord#" "Word# -> Word# -> Word#",
      primitive "minusWord#" "Word# -> Word# -> Word#",
      primitive "timesWord#" "Word# -> Word# -> Word#",
      primitive "addWordC#" "Word# -> Word# -> (# Word#, Int# #)",
      primitive "subWordC#" "Word# -> Word# -> (# Word#, Int# #)",
      primitive "timesWord2#" "Word# -> Word# -> (# Word#, Word# #)",
      primitive "quotWord#" "Word# -> Word# -> Word#",
      primitive "remWord#" "Word# -> Word# -> Word#",
      primitive "quotRemWord#" "Word# -> Word# -> (# Word#, Word# #)",
      primitive "quotRemWord2#" "Word# -> Word# -> Word# -> (# Word#, Word# #)",
      primitive "and#" "Word# -> Word# -> Word#",
      primitive "or#" "Word# -> Word# -> Word#",
      primitive "xor#" "Word# -> Word# -> Word#",
      primitive "not#" "Word# -> Word#",
      primitive "uncheckedShiftL#" "Word# -> Int# -> Word#",
      primitive "uncheckedShiftRL#" "Word# -> Int# -> Word#",
      primitive "int2Word#" "Int# -> Word#",
      primitive "word2Int#" "Word# -> Int#",
      primitive "word8ToWord#" "Word8# -> Word#",
      primitive "word32ToWord#" "Word32# -> Word#",
      primitive "word64ToWord#" "Word64# -> Word#",
      primitive "eqWord#" "Word# -> Word# -> Int#",
      primitive "neWord#" "Word# -> Word# -> Int#",
      primitive "ltWord#" "Word# -> Word# -> Int#",
      primitive "leWord#" "Word# -> Word# -> Int#",
      primitive "gtWord#" "Word# -> Word# -> Int#",
      primitive "geWord#" "Word# -> Word# -> Int#",
      primitive "clz#" "Word# -> Word#",
      primitive "ctz#" "Word# -> Word#",
      primitive "popCnt#" "Word# -> Word#",
      primitive "raise#" "a -> b",
      primitive "aihcExit#" "Int# -> State# RealWorld -> (# State# RealWorld, a #)",
      primitive "unsafeCoerce#" "a -> b",
      seqPrimitive,
      primitive "realWorld#" "State# RealWorld",
      primitive "noDuplicate#" "State# d -> State# d",
      primitive "makeStableName#" "a -> State# RealWorld -> (# State# RealWorld, StableName# a #)",
      primitive "stableNameToInt#" "StableName# a -> Int#",
      primitive "eqStableName#" "StableName# a -> StableName# b -> Int#",
      primitive
        "catch#"
        "(State# RealWorld -> (# State# RealWorld, a #)) -> (b -> State# RealWorld -> (# State# RealWorld, a #)) -> State# RealWorld -> (# State# RealWorld, a #)",
      primitive
        "fork#"
        "(State# RealWorld -> (# State# RealWorld, a #)) -> State# RealWorld -> (# State# RealWorld, ThreadId# #)",
      primitive "awaitIO#" "Addr# -> State# RealWorld -> State# RealWorld",
      primitive "newMVar#" "State# d -> (# State# d, MVar# d a #)",
      primitive "readMVar#" "MVar# d a -> State# d -> (# State# d, a #)",
      primitive "takeMVar#" "MVar# d a -> State# d -> (# State# d, a #)",
      primitive "putMVar#" "MVar# d a -> a -> State# d -> State# d",
      primitive
        "newMutVar#"
        "a -> State# d -> (# State# d, MutVar# d a #)",
      primitive
        "readMutVar#"
        "MutVar# d a -> State# d -> (# State# d, a #)",
      primitive
        "writeMutVar#"
        "MutVar# d a -> a -> State# d -> State# d",
      primitive
        "casMutVar#"
        "MutVar# d a -> a -> a -> State# d -> (# State# d, Int#, a #)",
      primitive "sameMutVar#" "MutVar# d a -> MutVar# d a -> Int#",
      primitive "newArray#" "Int# -> a -> State# d -> (# State# d, MutableArray# d a #)",
      primitive "indexArray#" "Array# a -> Int# -> a",
      primitive "readArray#" "MutableArray# d a -> Int# -> State# d -> (# State# d, a #)",
      primitive "writeArray#" "MutableArray# d a -> Int# -> a -> State# d -> State# d",
      primitive "unsafeFreezeArray#" "MutableArray# d a -> State# d -> (# State# d, Array# a #)",
      primitive "unsafeThawArray#" "Array# a -> State# d -> (# State# d, MutableArray# d a #)",
      primitive "sameMutableArray#" "MutableArray# d a -> MutableArray# d a -> Int#",
      primitive "newByteArray#" "Int# -> State# d -> (# State# d, MutableByteArray# d #)",
      primitive "newPinnedByteArray#" "Int# -> State# d -> (# State# d, MutableByteArray# d #)",
      primitive "newAlignedPinnedByteArray#" "Int# -> Int# -> State# d -> (# State# d, MutableByteArray# d #)",
      primitive "isMutableByteArrayPinned#" "MutableByteArray# d -> Int#",
      primitive "isByteArrayPinned#" "ByteArray# -> Int#",
      primitive "byteArrayContents#" "ByteArray# -> Addr#",
      primitive "mutableByteArrayContents#" "MutableByteArray# d -> Addr#",
      primitive "shrinkMutableByteArray#" "MutableByteArray# d -> Int# -> State# d -> State# d",
      primitive "resizeMutableByteArray#" "MutableByteArray# d -> Int# -> State# d -> (# State# d, MutableByteArray# d #)",
      primitive "unsafeFreezeByteArray#" "MutableByteArray# d -> State# d -> (# State# d, ByteArray# #)",
      primitive "unsafeThawByteArray#" "ByteArray# -> State# d -> (# State# d, MutableByteArray# d #)",
      primitive "sizeofByteArray#" "ByteArray# -> Int#",
      primitive "getSizeofMutableByteArray#" "MutableByteArray# d -> State# d -> (# State# d, Int# #)",
      primitive "copyAddrToByteArray#" "Addr# -> MutableByteArray# d -> Int# -> Int# -> State# d -> State# d",
      primitive "indexWord8OffAddr#" "Addr# -> Int# -> Word8#",
      primitive "indexWord32OffAddr#" "Addr# -> Int# -> Word32#",
      primitive "indexWord64OffAddr#" "Addr# -> Int# -> Word64#",
      primitive "indexWordArray#" "ByteArray# -> Int# -> Word#",
      primitive "readWordArray#" "MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)",
      primitive "writeWordArray#" "MutableByteArray# d -> Int# -> Word# -> State# d -> State# d",
      primitive "copyByteArray#" "ByteArray# -> Int# -> MutableByteArray# d -> Int# -> Int# -> State# d -> State# d",
      primitive "yield#" "State# RealWorld -> State# RealWorld"
    ]

primitive :: Text -> Text -> (Text, PrimitiveSpec)
primitive name source =
  let expected = parsePrimitiveTypeScheme source
   in ( name,
        PrimitiveSpec
          { primitiveSpecSource = source,
            primitiveSpecExpected = expected
          }
      )

seqPrimitive :: (Text, PrimitiveSpec)
seqPrimitive =
  primitive "seq" "forall (r :: RuntimeRep) a (b :: TYPE r). a -> b -> b"

parsePrimitiveTypeScheme :: Text -> TypeScheme
parsePrimitiveTypeScheme source =
  case parseTypeScheme source of
    Right scheme -> scheme
    Left err -> invalidPrimitiveType err
  where
    invalidPrimitiveType err =
      error ("invalid primitive type specification `" <> T.unpack source <> "`: " <> err)

statePrimRealWorldTy :: TcType
statePrimRealWorldTy = TcTyCon (TyCon "State#" 1) [realWorldTy]

realWorldTy :: TcType
realWorldTy = TcTyCon (TyCon "RealWorld" 0) []

unboxedTupleTy :: [TcType] -> TcType
unboxedTupleTy tys =
  TcTyCon
    (mkTyCon (unboxedTupleTyConName (length tys)) (length tys) tupleKind)
    tys
  where
    tupleKind = foldr (KFun . typeKind) (KTYPE (TupleRep (map runtimeRep tys))) tys
    runtimeRep ty = fromRight liftedRuntimeRep (runtimeRepOfType ty)

collectForAlls :: TcType -> ([TyVarId], TcType)
collectForAlls (TcForAllTy tv body) =
  let (tvs, inner) = collectForAlls body
   in (tv : tvs, inner)
collectForAlls ty = ([], ty)

dsDataConM :: DataConDecl -> DsM (Text, [TyVarId], [TcType], TcType)
dsDataConM con = do
  let (name, arity) = dsDataConPure con
  ty <- lookupType name
  let (quantifiedVariables, qualifiedConstructorTy) = collectForAlls ty
      (predicates, constructorTy) = splitConstructorContext qualifiedConstructorTy
      resultType = constructorResultType constructorTy
      resultVariables = freeRigidTyVars resultType
      resultRepUniques = typeRuntimeRepVariables resultType
      universalVariables = filter (\variable -> variable `elem` resultVariables || tvUnique variable `elem` resultRepUniques) quantifiedVariables
  fields <- dataConFieldTypes name arity constructorTy
  pure (name, universalVariables, map predType predicates <> fields, resultType)

typeRuntimeRepVariables :: TcType -> [Unique]
typeRuntimeRepVariables ty =
  case ty of
    TcTyVar variable -> kindRuntimeRepVariables (tvKind variable)
    TcMetaTv {} -> []
    TcTyCon tyCon arguments -> kindRuntimeRepVariables (tyConKind tyCon) <> concatMap typeRuntimeRepVariables arguments
    TcFunTy argument result -> typeRuntimeRepVariables argument <> typeRuntimeRepVariables result
    TcForAllTy _ body -> typeRuntimeRepVariables body
    TcQualTy _ body -> typeRuntimeRepVariables body
    TcAppTy function argument -> typeRuntimeRepVariables function <> typeRuntimeRepVariables argument
    TcBuiltinTyCon _ _ arguments -> concatMap typeRuntimeRepVariables arguments

kindRuntimeRepVariables :: Kind -> [Unique]
kindRuntimeRepVariables kind =
  case kind of
    KTYPE runtimeRep -> runtimeRepVariables runtimeRep
    KFun argument result -> kindRuntimeRepVariables argument <> kindRuntimeRepVariables result
    _ -> []

runtimeRepVariables :: RuntimeRep -> [Unique]
runtimeRepVariables runtimeRep =
  case runtimeRep of
    RuntimeRepVar unique -> [unique]
    TupleRep fields -> concatMap runtimeRepVariables fields
    SumRep fields -> concatMap runtimeRepVariables fields
    _ -> []

splitConstructorContext :: TcType -> ([Pred], TcType)
splitConstructorContext (TcQualTy predicates body) = (predicates, body)
splitConstructorContext ty = ([], ty)

constructorResultType :: TcType -> TcType
constructorResultType (TcFunTy _ result) = constructorResultType result
constructorResultType ty = ty

dataConFieldTypes :: Text -> Int -> TcType -> DsM [TcType]
dataConFieldTypes _ 0 _ = pure []
dataConFieldTypes name arity (TcFunTy arg rest) =
  (arg :) <$> dataConFieldTypes name (arity - 1) rest
dataConFieldTypes name arity ty =
  desugarBug ("missing field type information for data constructor " <> T.unpack name <> ": expected " <> show arity <> " more field(s) in " <> show ty)

dsClassDeclM :: ClassDecl -> TcClassAnnotation -> DsM [FcTopBind]
dsClassDeclM classDecl classAnn = do
  let classTyVars = tcClassTyVars classAnn
  superClassFieldTypes <- mapM (checkedConstraintType "class superclass" . tcDictBinderType) (tcClassSuperClasses classAnn)
  methodFieldTypes <- mapM (classMethodFieldType className classTyVars . tcClassMethodType) methods
  let fieldTypes = superClassFieldTypes <> methodFieldTypes
  selectors <- mapM (dsClassSelector dictionaryConstructor (length superClassFieldTypes) classTyVars fieldTypes) methods
  defaults <- mapM dsClassDefault (classDefaultGroups classDecl)
  classOrigin <- localDeclarationOrigin className
  constructorOrigin <- localDeclarationOrigin dictionaryConstructor
  let dictionaryDeclaration = FcData (FcDataDecl classOrigin className classTyVars KType [FcDataConDecl (fcConstructorIdFromSymbol constructorOrigin) dictionaryConstructor fieldTypes])
  pure (dictionaryDeclaration : selectors <> defaults)
  where
    className = unqualifiedNameText (binderHeadName (classDeclHead classDecl))
    methods = tcClassMethods classAnn
    dictionaryConstructor = fcDictionaryConstructorName className

localDeclarationOrigin :: Text -> DsM FcSymbolOrigin
localDeclarationOrigin declarationName = do
  packageId <- gets dsModulePackage
  moduleName' <- gets dsModuleName
  pure (FcTopLevelOrigin (packageIdText packageId) moduleName' declarationName)

primitiveConstructorOrigin :: Text -> DsM FcSymbolOrigin
primitiveConstructorOrigin declarationName = do
  packageId <- gets dsPrimPackageId
  pure (FcTopLevelOrigin (packageIdText packageId) "GHC.Types" declarationName)

typeDataConOrigin :: TcType -> Text -> DsM FcSymbolOrigin
typeDataConOrigin ty constructorName =
  case ty of
    TcTyCon tyCon _ ->
      pure
        ( FcTopLevelOrigin
            (packageIdText (tyConPackageId tyCon))
            (tyConModuleName tyCon)
            constructorName
        )
    _ -> primitiveConstructorOrigin constructorName

dsClassSelector :: Text -> Int -> [TyVarId] -> [TcType] -> TcClassMethodAnnotation -> DsM FcTopBind
dsClassSelector dictionaryConstructor superClassCount classTyVars fieldTypes methodAnn = do
  methodUnique <- freshUnique
  dictVars <- zipWithM mkSelectorDict [0 :: Int ..] dictPreds
  classDictionaryVar <-
    case dictVars of
      dictVar : _ -> pure dictVar
      [] -> freshVar "$d" (tcClassMethodDictType methodAnn)
  caseBinder <- freshVar "$dict" (varType classDictionaryVar)
  fieldBinders <- zipWithM (\index -> freshVar ("$method" <> T.pack (show index))) [0 :: Int ..] fieldTypes
  selectedField <-
    case drop (superClassCount + tcClassMethodIndex methodAnn) fieldBinders of
      selected : _ -> pure selected
      [] -> desugarBug ("invalid class method index for " <> T.unpack (tcClassMethodName methodAnn))
  constructorOrigin <- localDeclarationOrigin dictionaryConstructor
  let extraTyVars = filter (`notElem` classTyVars) (tcClassMethodTyVars methodAnn)
      extraDictVars = drop 1 dictVars
      selected =
        foldl
          FcApp
          (foldl FcTyApp (FcVar selectedField) (map TcTyVar extraTyVars))
          (map FcVar extraDictVars)
      selection = FcCase (FcVar classDictionaryVar) caseBinder [FcAlt (DataAlt (fcConstructorIdFromSymbol constructorOrigin)) fieldBinders selected]
      methodVar = Var (tcClassMethodName methodAnn) methodUnique (tcClassMethodType methodAnn)
      body = foldr FcTyLam (foldr FcLam selection dictVars) (tcClassMethodTyVars methodAnn)
  pure (FcTopBind (FcNonRec methodVar body))
  where
    (_tyVars, afterForAlls) = peelForAlls (tcClassMethodType methodAnn)
    (dictPreds, _bodyTy) = peelQuals afterForAlls
    mkSelectorDict ix pred' =
      freshVar ("$d" <> T.pack (show ix)) (predType pred')

dsClassDefault :: (TcInstanceMethodAnnotation, [Match]) -> DsM FcTopBind
dsClassDefault (methodAnn, matches) = do
  let methodName = tcInstanceMethodName methodAnn
      methodType = tcInstanceMethodType methodAnn
      workerName = defaultMethodName methodName
  worker <- freshVar workerName methodType
  body <- dsMatches methodType matches
  pure (FcTopBind (FcNonRec worker body))

classDefaultGroups :: ClassDecl -> [(TcInstanceMethodAnnotation, [Match])]
classDefaultGroups classDecl = concatMap classDefaultGroup (classDeclItems classDecl)

classDefaultGroup :: ClassDeclItem -> [(TcInstanceMethodAnnotation, [Match])]
classDefaultGroup item =
  case item of
    ClassItemAnn ann inner
      | Just methodAnn <- fromAnnotation ann -> classDefaultWithAnnotation methodAnn inner
      | otherwise -> classDefaultGroup inner
    _ -> []

classDefaultWithAnnotation :: TcInstanceMethodAnnotation -> ClassDeclItem -> [(TcInstanceMethodAnnotation, [Match])]
classDefaultWithAnnotation methodAnn item =
  case item of
    ClassItemAnn _ inner -> classDefaultWithAnnotation methodAnn inner
    ClassItemDefault (FunctionBind _ matches) -> [(methodAnn, matches)]
    ClassItemDefault (PatternBind _ _ rhs) -> [(methodAnn, [zeroArgumentMatch rhs])]
    _ -> []

zeroArgumentMatch :: Rhs Expr -> Match
zeroArgumentMatch rhs =
  Match
    { matchAnns = [],
      matchHeadForm = MatchHeadPrefix,
      matchPats = [],
      matchRhs = rhs
    }

dsInstanceDecl :: Decl -> DsM [FcTopBind]
dsInstanceDecl decl =
  case decl of
    DeclAnn ann (DeclInstance instanceDecl)
      | Just instAnn <- fromAnnotation ann -> (: []) <$> dsInstanceDict instAnn instanceDecl
    DeclAnn _ inner -> dsInstanceDecl inner
    DeclInstance {} -> desugarBug "missing type-checker annotation for instance declaration"
    _ -> pure []

dsInstanceDict :: TcInstanceAnnotation -> InstanceDecl -> DsM FcTopBind
dsInstanceDict instAnn instanceDecl = do
  let methods = Map.fromListWith combineMethods (instanceMethodGroups instanceDecl)
  contextDicts <- zipWithM mkContextDict [0 :: Int ..] (tcInstanceContextDicts instAnn)
  className <-
    case dictionaryClassName (tcInstanceDictType instAnn) of
      Just name -> pure name
      Nothing -> desugarBug ("cannot determine class for instance dictionary " <> T.unpack (tcInstanceDictName instAnn))
  let methodOrder = tcInstanceMethodOrder instAnn
      usesDefaultMethod =
        any
          (\methodName -> Map.notMember methodName methods && methodName `elem` tcInstanceDefaultMethods instAnn)
          methodOrder
      selfDictionary dictVar =
        foldl
          FcApp
          (foldl FcTyApp (FcVar dictVar) (map TcTyVar (tcInstanceTyVars instAnn)))
          (map (FcVar . classDictVar) contextDicts)
      desugarFields maybeSelfDictionary = do
        superClassFields <- withDicts contextDicts (mapM (dsEvidence . snd) (tcInstanceSuperClasses instAnn))
        methodFields <-
          mapM
            (dsInstanceMethod contextDicts methods maybeSelfDictionary (tcInstanceHeadTypes instAnn) (tcInstanceClassOrigin instAnn) (tcInstanceDefaultMethods instAnn))
            methodOrder
        pure (superClassFields <> methodFields)
      buildDictionary recursive dictVar fields = do
        methodTypes <- mapM lookupType methodOrder
        superClassFieldTypes <- mapM (checkedConstraintType "instance superclass" . tcDictBinderType) (tcInstanceClassSuperClasses instAnn)
        (classTyVars, fieldTypes) <-
          case methodTypes of
            [] -> pure (tcInstanceClassTyVars instAnn, superClassFieldTypes)
            _ -> do
              (tyVars, methodFieldTypes) <- classDictionaryLayout className methodTypes
              pure (tyVars, superClassFieldTypes <> methodFieldTypes)
        constructorUnique <- freshUnique
        let dictionaryConstructor = fcDictionaryConstructorName className
            dictionaryType = TcTyCon (tcInstanceClassTyCon instAnn) (map TcTyVar classTyVars)
            constructorType = foldr TcForAllTy (foldr TcFunTy dictionaryType fieldTypes) classTyVars
            constructorVar =
              (Var dictionaryConstructor constructorUnique constructorType)
                { varResolvedName =
                    Just
                      ( FcTopLevelOrigin
                          (packageIdText (tyConPackageId (tcInstanceClassTyCon instAnn)))
                          (tyConModuleName (tcInstanceClassTyCon instAnn))
                          dictionaryConstructor
                      )
                }
            constructor = foldl FcTyApp (FcVar constructorVar) (tcInstanceHeadTypes instAnn)
            dictionary = foldl FcApp constructor fields
            dictBody = foldr FcTyLam (foldr (FcLam . classDictVar) dictionary contextDicts) (tcInstanceTyVars instAnn)
            bindingGroup
              | recursive = FcRec [(dictVar, dictBody)]
              | otherwise = FcNonRec dictVar dictBody
        pure (FcTopBind bindingGroup)
  if usesDefaultMethod
    then do
      dictVar <- freshVar (tcInstanceDictName instAnn) (tcInstanceDictType instAnn)
      fields <- desugarFields (Just (selfDictionary dictVar))
      buildDictionary True dictVar fields
    else do
      fields <- desugarFields Nothing
      dictVar <- freshVar (tcInstanceDictName instAnn) (tcInstanceDictType instAnn)
      buildDictionary False dictVar fields
  where
    combineMethods (newTy, newMatches) (_oldTy, oldMatches) = (newTy, oldMatches <> newMatches)

dictionaryClassName :: TcType -> Maybe Text
dictionaryClassName ty =
  case ty of
    TcForAllTy _ body -> dictionaryClassName body
    TcQualTy _ body -> dictionaryClassName body
    TcTyCon (TyCon className _) _ -> Just className
    _ -> Nothing

classDictionaryLayout :: Text -> [TcType] -> DsM ([TyVarId], [TcType])
classDictionaryLayout className methodTypes = do
  classTyVars <-
    case methodTypes of
      firstMethod : _ -> classTypeVariables className firstMethod
      [] -> pure []
  fieldTypes <- mapM (classMethodFieldType className classTyVars) methodTypes
  pure (classTyVars, fieldTypes)

classTypeVariables :: Text -> TcType -> DsM [TyVarId]
classTypeVariables className methodType =
  case [args | ClassPred predicateClass args <- predicates, tyConName predicateClass == className] of
    args : _ ->
      case traverse asTyVar args of
        Just tyVars -> pure tyVars
        Nothing -> desugarBug ("class predicate has non-variable parameters for " <> T.unpack className)
    [] -> desugarBug ("class method lacks its class predicate for " <> T.unpack className)
  where
    (_, afterForAlls) = peelForAlls methodType
    (predicates, _) = peelQuals afterForAlls
    asTyVar (TcTyVar tyVar) = Just tyVar
    asTyVar _ = Nothing

mkContextDict :: Int -> TcDictBinderAnnotation -> DsM ClassDict
mkContextDict ix dictAnn = do
  dictionaryType <- checkedConstraintType "instance context" (tcDictBinderType dictAnn)
  dictVar <- freshVar ("$d" <> T.pack (show ix)) dictionaryType
  case dictionaryType of
    TcTyCon classTyCon _ -> pure (ClassDict classTyCon (tcDictBinderArgs dictAnn) dictVar)
    other -> desugarBug ("invalid checked class dictionary type: " <> show other)

dsInstanceMethod :: [ClassDict] -> Map.Map Text (TcType, [Match]) -> Maybe FcExpr -> [TcType] -> Maybe (Text, Text) -> [Text] -> Text -> DsM FcExpr
dsInstanceMethod contextDicts methods maybeSelfDictionary headTypes classOrigin defaults methodName =
  case Map.lookup methodName methods of
    Just (expected, matches) ->
      dsMatchesWithEnclosingDicts contextDicts expected matches
    Nothing
      | methodName `elem` defaults -> do
          selfDictionary <-
            case maybeSelfDictionary of
              Just dictionary -> pure dictionary
              Nothing -> desugarBug ("default method " <> T.unpack methodName <> " requires a recursive instance dictionary")
          let workerName = defaultMethodName methodName
          workerType <- lookupType workerName
          worker <- freshVar workerName workerType
          let origin = fmap (\(packageName, originModule) -> FcTopLevelOrigin packageName originModule workerName) classOrigin
          pure (FcApp (foldl FcTyApp (FcVar worker {varResolvedName = origin}) headTypes) selfDictionary)
      | otherwise ->
          desugarBug ("missing method " <> T.unpack methodName <> " in instance dictionary")

moduleInstances :: [Decl] -> [Decl]
moduleInstances = filter isInstance
  where
    isInstance decl =
      case peelDeclAnn decl of
        DeclInstance {} -> True
        _ -> False

instanceMethodGroups :: InstanceDecl -> [(Text, (TcType, [Match]))]
instanceMethodGroups instanceDecl =
  concatMap itemMethods (instanceDeclItems instanceDecl)

itemMethods :: InstanceDeclItem -> [(Text, (TcType, [Match]))]
itemMethods item =
  case item of
    InstanceItemAnn ann inner
      | Just methodAnn <- fromAnnotation ann -> itemMethodWithAnnotation methodAnn inner
      | otherwise -> itemMethods inner
    _ -> []

itemMethodWithAnnotation :: TcInstanceMethodAnnotation -> InstanceDeclItem -> [(Text, (TcType, [Match]))]
itemMethodWithAnnotation methodAnn item =
  case item of
    InstanceItemAnn _ inner -> itemMethodWithAnnotation methodAnn inner
    InstanceItemBind (FunctionBind _ matches) ->
      [(tcInstanceMethodName methodAnn, (tcInstanceMethodType methodAnn, matches))]
    InstanceItemBind (PatternBind _ _ rhs) ->
      [ ( tcInstanceMethodName methodAnn,
          ( tcInstanceMethodType methodAnn,
            [ Match
                { matchAnns = [],
                  matchHeadForm = MatchHeadPrefix,
                  matchPats = [],
                  matchRhs = rhs
                }
            ]
          )
        )
      ]
    _ -> []

dropForAlls :: TcType -> TcType
dropForAlls (TcForAllTy _ body) = dropForAlls body
dropForAlls ty = ty

-- | A group of top-level value declarations.
data DeclGroup
  = DeclFunction !Text ![Match]
  | DeclPattern !Text !(Rhs Expr)

dgName :: DeclGroup -> Text
dgName group =
  case group of
    DeclFunction name _ -> name
    DeclPattern name _ -> name

-- | Group consecutive FunctionBind declarations with the same name and keep
-- simple top-level pattern binds.
groupFunctionBinds :: [Decl] -> [DeclGroup]
groupFunctionBinds [] = []
groupFunctionBinds (d : ds) = case extractFunBind d of
  Just (name, matches) ->
    let (sameNameDecls, rest) = span (hasSameName name) ds
        allMatches = matches ++ concatMap (maybe [] snd . extractFunBind) sameNameDecls
     in DeclFunction name allMatches : groupFunctionBinds rest
  Nothing ->
    case extractPatternBind d of
      Just group -> group : groupFunctionBinds ds
      Nothing -> groupFunctionBinds ds

-- | Extract function bind info from a declaration.
extractFunBind :: Decl -> Maybe (Text, [Match])
extractFunBind decl = case peelDeclAnn decl of
  DeclValue (FunctionBind name matches) ->
    Just (unqualifiedNameText name, matches)
  _ -> Nothing

-- | Check if a declaration is a FunctionBind with the given name.
hasSameName :: Text -> Decl -> Bool
hasSameName name d = case extractFunBind d of
  Just (n, _) -> n == name
  Nothing -> False

extractPatternBind :: Decl -> Maybe DeclGroup
extractPatternBind decl =
  case peelDeclAnn decl of
    DeclValue (PatternBind _ pat rhs) ->
      DeclPattern <$> barePatternName pat <*> pure rhs
    _ -> Nothing

barePatternName :: Pattern -> Maybe Text
barePatternName pat =
  case pat of
    PVar name -> Just (unqualifiedNameText name)
    PAnn _ inner -> barePatternName inner
    PParen inner -> barePatternName inner
    _ -> Nothing

-- | Desugar a function binding group.
dsGroup :: (DeclGroup, Var) -> DsM (Var, FcExpr)
dsGroup (grp, var) = do
  body <-
    case grp of
      DeclFunction _ matches -> dsMatches (varType var) matches
      DeclPattern _ rhs -> dsMatches (varType var) [rhsAsMatch rhs]
  pure (var, body)

rhsAsMatch :: Rhs Expr -> Match
rhsAsMatch rhs =
  Match
    { matchAnns = [],
      matchHeadForm = MatchHeadPrefix,
      matchPats = [],
      matchRhs = rhs
    }
