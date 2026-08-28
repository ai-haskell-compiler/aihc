{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Convert a checked module into System FC 2 types, axioms, and values.
module Aihc.Fc2.Desugar
  ( desugarModuleFc2,
    typeEnvFromTcInterface,
    DesugarConfig (..),
    Fc2DesugarResult (..),
  )
where

import Aihc.Fc2.Convert
import Aihc.Fc2.Desugar.Value (desugarValues)
import Aihc.Fc2.Imports (emptyImports, importsForProgram)
import Aihc.Fc2.Name
import Aihc.Fc2.Syntax
import Aihc.Fc2.Tidy (tidyProgram)
import Aihc.Fc2.TypeOf qualified as TypeOf
import Aihc.Parser.Syntax
  ( DataDecl (..),
    Module (..),
    TypeFamilyDecl (..),
    TypeSynDecl (..),
    UnqualifiedName,
    binderHeadName,
    binderHeadParams,
    fromAnnotation,
    nameQualifier,
    peelDeclAnn,
    tyVarBinderName,
    unqualifiedNameAnns,
    unqualifiedNameText,
  )
import Aihc.Parser.Syntax qualified as Syn
import Aihc.Resolve (PackageId (..), ResolutionAnnotation (..), ResolvedName (..))
import Aihc.Tc
  ( ClassInfo (..),
    DataConFieldInfo (..),
    DataConInfo (..),
    DataFamilyInstanceInfo (..),
    DataTypeInfo (..),
    InstanceInfo (..),
    TcBindingResult (..),
    TcInterface (..),
    TcTermKey (..),
    TyConFlavor (..),
    TyConInfo (..),
    TypeFamilyInstanceInfo (..),
    tcInterfaceBindings,
    tcModuleDiagnostics,
    tcModuleSuccess,
  )
import Aihc.Tc.Env (TypeSynonymInfo (..))
import Aihc.Tc.Types
  ( Pred (..),
    TcType (..),
    TyVarId,
    TypeScheme (..),
    Unique (..),
    tyConKey,
    tyConModuleName,
    tyConName,
    tyConPackageId,
    typeSchemeBody,
    pattern KFun,
    pattern KType,
  )
import Control.Monad (zipWithM)
import Data.List (nub, sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

data Fc2DesugarResult = Fc2DesugarResult
  { ds2Program :: !Program,
    ds2Success :: !Bool,
    ds2Errors :: ![String]
  }
  deriving (Show)

newtype DesugarConfig = DesugarConfig
  { primPackageId :: PackageId
  }
  deriving (Eq, Show)

typeEnvFromTcInterface :: DesugarConfig -> TcInterface -> Either String TypeOf.TypeEnv
typeEnvFromTcInterface config interface = do
  declarations <- interfaceTypeDeclarations conversionEnv interface
  typeHeaders <- mapM (convertTyConHeader conversionEnv) (tcInterfaceTyCons interface)
  termHeaders <- mapMaybeM (convertTermHeader conversionEnv) (tcInterfaceTerms interface)
  instanceHeaders <- mapM (convertInstanceHeader conversionEnv) (tcInterfaceInstances interface)
  defaultMethodHeaders <- concat <$> mapM (convertDefaultMethodHeaders conversionEnv) (tcInterfaceClasses interface)
  let declarationEnv = TypeOf.typeEnvFromProgram (Program emptyScopeTable emptyImports declarations)
  pure
    declarationEnv
      { TypeOf.tePrimPackage = Just (primPackageId config),
        TypeOf.teHeaders = TypeOf.teHeaders declarationEnv <> Map.fromList (typeHeaders <> termHeaders <> instanceHeaders <> defaultMethodHeaders)
      }
  where
    conversionEnv = interfaceConvertEnv config interface

interfaceConvertEnv :: DesugarConfig -> TcInterface -> ConvertEnv
interfaceConvertEnv config interface =
  withAxioms
    (axiomEntriesFromInterface interface)
    ( withKindEnv
        (Map.fromList [(tyConKey (tciTyCon info), tciKindScheme info) | info <- tcInterfaceTyCons interface])
        (withClassTyCons (map (tyConKey . ciTyCon) (tcInterfaceClasses interface)) (emptyConvertEnv (primPackageId config)))
    )

axiomEntriesFromInterface :: TcInterface -> [(Text, Name)]
axiomEntriesFromInterface interface =
  concatMap newtypeEntry (tcInterfaceDataTypes interface)
    <> concatMap dataFamilyEntry (tcInterfaceDataFamilyInstances interface)
  where
    newtypeEntry info
      | dtiFlavor info == NewtypeTyCon =
          let tyCon = dtiTyCon info
              name = Name ("$ax$" <> dtiName info) SortAxiom (OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon))
           in [(dtiName info, name), ("$ax$" <> dtiName info, name)]
      | otherwise = []
    dataFamilyEntry info =
      let tyCon = dfiiRepresentationTyCon info
          origin = OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon)
          familyName = Name (dfiiAxiomName info) SortAxiom origin
          representationName = tyConName tyCon
          representationAxiom = Name ("$ax$" <> T.drop 1 representationName) SortAxiom origin
       in [ (dfiiAxiomName info, familyName),
            (representationName, representationAxiom),
            ("$ax$" <> T.drop 1 representationName, representationAxiom)
          ]

interfaceTypeDeclarations :: ConvertEnv -> TcInterface -> Either String [Decl]
interfaceTypeDeclarations env interface = do
  dataDeclarations <- concat <$> mapM convertDataDeclaration (tcInterfaceDataTypes interface)
  synonymDeclarations <- mapM (convertSynonym env) (filter ((== SynonymTyCon) . tciFlavor) (tcInterfaceTyCons interface))
  classDeclarations <- mapM (convertClass env) (tcInterfaceClasses interface)
  dataFamilyDeclarations <- concat <$> mapM convertDataFamilyDeclaration (tcInterfaceDataFamilyInstances interface)
  pure (dataDeclarations <> synonymDeclarations <> classDeclarations <> dataFamilyDeclarations)
  where
    convertDataDeclaration info =
      case dtiFlavor info of
        DataTyCon -> (: []) <$> convertDataType env info
        NewtypeTyCon -> convertNewtype env info
        _ -> pure []
    convertDataFamilyDeclaration info =
      let tyCon = dfiiRepresentationTyCon info
       in convertDataFamilyInst env (tyConPackageId tyCon) (tyConModuleName tyCon) (tcInterfaceBindings interface) info

convertTyConHeader :: ConvertEnv -> TyConInfo -> Either String (Name, Type)
convertTyConHeader env info = do
  converted <- convertKindScheme env (tciKindScheme info)
  pure (tyConNameFc2 env (tciTyCon info), converted)

convertTermHeader :: ConvertEnv -> (TcTermKey, TypeScheme) -> Either String (Maybe (Name, Type))
convertTermHeader env (key, scheme) =
  case key of
    TcTermGlobal package moduleName' identifier -> do
      converted <- convertTypeScheme env scheme
      pure (Just (Name identifier SortValue (OriginTop package moduleName'), converted))
    TcTermLocal {} -> pure Nothing

convertInstanceHeader :: ConvertEnv -> InstanceInfo -> Either String (Name, Type)
convertInstanceHeader env info = do
  converted <- convertType env (iiDictType info)
  let (package, moduleName') = iiDictOrigin info
  pure (Name (iiDictName info) SortValue (OriginTop (PackageId package) moduleName'), converted)

convertDefaultMethodHeaders :: ConvertEnv -> ClassInfo -> Either String [(Name, Type)]
convertDefaultMethodHeaders env info =
  case ciOrigin info of
    Nothing -> pure []
    Just (package, moduleName') -> mapM (convertDefaultMethod package moduleName') methods
  where
    methods =
      [ (methodName, maybe methodScheme (defaultWorkerScheme methodScheme) (lookup methodName (ciDefaultSignatures info)))
      | methodName <- ciDefaultMethods info,
        Just methodScheme <- [lookup methodName (ciMethods info)]
      ]
    convertDefaultMethod package moduleName' (methodName, scheme) = do
      converted <- convertTypeScheme env scheme
      pure (Name ("$dm" <> methodName) SortValue (OriginTop (PackageId package) moduleName'), converted)
    defaultWorkerScheme ordinaryScheme (ForAll variables predicates body) =
      case ordinaryScheme of
        ForAll _ (classPredicate : _) _ -> ForAll variables (classPredicate : predicates) body
        _ -> ForAll variables predicates body

convertKindScheme :: ConvertEnv -> TypeScheme -> Either String Type
convertKindScheme env (ForAll tyVars predicates body) = do
  let bindersEnv = withTyVars tyVars env
  binders <- mapM (tyVarBinder bindersEnv) tyVars
  convertedPredicates <- mapM (convertPred bindersEnv) predicates
  convertedBody <- convertKind bindersEnv body
  pure (foldr TyForAll (foldr (funType bindersEnv) convertedBody convertedPredicates) binders)

convertTypeScheme :: ConvertEnv -> TypeScheme -> Either String Type
convertTypeScheme env (ForAll tyVars predicates body) = do
  let bindersEnv = withTyVars tyVars env
  binders <- mapM (tyVarBinder bindersEnv) tyVars
  convertedPredicates <- mapM (convertPred bindersEnv) predicates
  convertedBody <- convertType bindersEnv body
  pure (foldr TyForAll (foldr (funType bindersEnv) convertedBody convertedPredicates) binders)

mapMaybeM :: (value -> Either String (Maybe result)) -> [value] -> Either String [result]
mapMaybeM action values = catMaybes <$> mapM action values

desugarModuleFc2 :: DesugarConfig -> [TcBindingResult] -> TcInterface -> Module -> Fc2DesugarResult
desugarModuleFc2 config bindings interface checked =
  if not (tcModuleSuccess checked)
    then
      Fc2DesugarResult
        { ds2Program = Program emptyScopeTable emptyImports [],
          ds2Success = False,
          ds2Errors = fmap show (tcModuleDiagnostics checked)
        }
    else case desugarChecked config bindings interface checked of
      Left errors ->
        Fc2DesugarResult
          { ds2Program = Program emptyScopeTable emptyImports [],
            ds2Success = False,
            ds2Errors = [errors]
          }
      Right program ->
        Fc2DesugarResult
          { ds2Program = program,
            ds2Success = True,
            ds2Errors = []
          }

desugarChecked :: DesugarConfig -> [TcBindingResult] -> TcInterface -> Module -> Either String Program
desugarChecked config bindings interface checked = do
  let (packageId, currentModule) = resolvedModuleOrigin checked
      moduleOrigin = (packageId, currentModule)
      dataTypes = tcInterfaceDataTypes interface
      tyCons = tcInterfaceTyCons interface
      classes = tcInterfaceClasses interface
      dataFamilyInstances = tcInterfaceDataFamilyInstances interface
      typeFamilyInstances = tcInterfaceTypeFamilyInstances interface
      env =
        withAxioms
          (axiomEntries packageId currentModule dataTypes dataFamilyInstances typeFamilyInstances)
          ( withKindEnv
              (Map.fromList [(tyConKey (tciTyCon info), tciKindScheme info) | info <- tyCons])
              (withClassTyCons (map (tyConKey . ciTyCon) classes) (emptyConvertEnv (primPackageId config)))
          )
  typeDecls <-
    concat
      <$> mapM
        (dsDecl env packageId currentModule dataTypes tyCons classes dataFamilyInstances typeFamilyInstances bindings)
        (Syn.moduleDecls checked)
  valueDecls <- desugarValues env bindings interface moduleOrigin checked
  available <- typeEnvFromTcInterface config interface
  let decls = typeDecls <> valueDecls
      baseProgram = Program emptyScopeTable emptyImports decls
      imports = importsForProgram available baseProgram
      scopes = buildScopes moduleOrigin imports decls
  pure (tidyProgram (Program scopes imports decls))

axiomEntries :: PackageId -> Text -> [DataTypeInfo] -> [DataFamilyInstanceInfo] -> [TypeFamilyInstanceInfo] -> [(Text, Name)]
axiomEntries package moduleName' dataTypes dataFamilyInstances typeFamilyInstances =
  concatMap newtypeAxiom dataTypes
    <> concatMap (dataFamilyAxiom package moduleName') dataFamilyInstances
    <> map (typeFamilyAxiom package moduleName') typeFamilyInstances
  where
    newtypeAxiom info
      | dtiFlavor info == NewtypeTyCon =
          let tyCon = dtiTyCon info
              axiomName = Name ("$ax$" <> dtiName info) SortAxiom (OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon))
           in [(dtiName info, axiomName), ("$ax$" <> dtiName info, axiomName)]
      | otherwise = []
    dataFamilyAxiom currentPackage currentModule info =
      let axiomName = Name (dfiiAxiomName info) SortAxiom (OriginTop currentPackage currentModule)
          representationName = tyConName (dfiiRepresentationTyCon info)
          representationAxiom =
            Name ("$ax$" <> T.drop 1 representationName) SortAxiom (OriginTop currentPackage currentModule)
       in [ (dfiiAxiomName info, axiomName),
            (representationName, representationAxiom),
            ("$ax$" <> T.drop 1 representationName, representationAxiom)
          ]
    typeFamilyAxiom currentPackage currentModule info =
      (tfiiAxiomName info, Name (tfiiAxiomName info) SortAxiom (OriginTop currentPackage currentModule))

dsDecl ::
  ConvertEnv ->
  PackageId ->
  Text ->
  [DataTypeInfo] ->
  [TyConInfo] ->
  [ClassInfo] ->
  [DataFamilyInstanceInfo] ->
  [TypeFamilyInstanceInfo] ->
  [TcBindingResult] ->
  Syn.Decl ->
  Either String [Decl]
dsDecl env package moduleName' dataTypes tyCons classes dataFamilyInstances typeFamilyInstances bindings decl =
  case decl of
    Syn.DeclAnn ann inner
      | Just familyInfo <- fromAnnotation ann ->
          convertDataFamilyInst env package moduleName' bindings familyInfo
      | Just familyEquation <- fromAnnotation ann ->
          (: []) <$> convertTypeFamilyEquation env package moduleName' familyEquation
      | otherwise ->
          dsDecl env package moduleName' dataTypes tyCons classes dataFamilyInstances typeFamilyInstances bindings inner
    _ ->
      case peelDeclAnn decl of
        Syn.DeclData dataDecl -> do
          info <- lookupDataType DataTyCon package moduleName' (unqualifiedNameText (binderHeadName (dataDeclHead dataDecl))) dataTypes
          (: []) <$> convertDataType env info
        Syn.DeclTypeSyn synonymDecl -> do
          info <- lookupSynonym package moduleName' (unqualifiedNameText (binderHeadName (typeSynHead synonymDecl))) tyCons
          (: []) <$> convertSynonym env info
        Syn.DeclClass classDecl -> do
          info <- lookupClassInfo package moduleName' (unqualifiedNameText (binderHeadName (Syn.classDeclHead classDecl))) classes
          (: []) <$> convertClass env info
        Syn.DeclNewtype newtypeDecl ->
          convertNewtype env
            =<< lookupDataType NewtypeTyCon package moduleName' (unqualifiedNameText (binderHeadName (Syn.newtypeDeclHead newtypeDecl))) dataTypes
        Syn.DeclDataFamilyDecl familyDecl -> do
          info <- lookupTyConFlavor DataFamilyTyCon package moduleName' (unqualifiedNameText (binderHeadName (Syn.dataFamilyDeclHead familyDecl))) tyCons
          (: []) <$> convertEmptyFamily env (map tyVarBinderName (binderHeadParams (Syn.dataFamilyDeclHead familyDecl))) Nominal info
        Syn.DeclTypeFamilyDecl familyDecl -> do
          let familyName = typeFamilyDeclName familyDecl
          info <- lookupTyConFlavor TypeFamilyTyCon package moduleName' familyName tyCons
          typeDecl <- convertEmptyFamily env (map tyVarBinderName (typeFamilyDeclParams familyDecl)) Nominal info
          axioms <-
            mapM
              (convertTypeFamilyEquation env package moduleName')
              [equation | equation <- typeFamilyInstances, tfiiFamilyName equation == familyName, tfiiClosed equation]
          pure (typeDecl : axioms)
        Syn.DeclForeign foreignDecl ->
          case Syn.foreignCallConv foreignDecl of
            Syn.CPrim -> Right []
            Syn.CCall -> Right []
            callConv -> Left ("unsupported System FC 2 foreign calling convention: " <> show callConv)
        _ -> Right []

lookupDataType :: TyConFlavor -> PackageId -> Text -> Text -> [DataTypeInfo] -> Either String DataTypeInfo
lookupDataType flavor package moduleName' name dataTypes =
  case matches of
    [info] -> Right info
    [] -> Left ("missing checked data type " <> T.unpack moduleName' <> "." <> T.unpack name)
    _ -> Left ("duplicate checked data type " <> T.unpack moduleName' <> "." <> T.unpack name)
  where
    matches =
      [ info
      | info <- dataTypes,
        dtiName info == name,
        dtiFlavor info == flavor,
        tyConPackageId (dtiTyCon info) == package,
        tyConModuleName (dtiTyCon info) == moduleName'
      ]

lookupClassInfo :: PackageId -> Text -> Text -> [ClassInfo] -> Either String ClassInfo
lookupClassInfo package moduleName' name classes =
  case matches of
    [info] -> Right info
    [] -> Left ("missing checked class " <> T.unpack moduleName' <> "." <> T.unpack name)
    _ -> Left ("duplicate checked class " <> T.unpack moduleName' <> "." <> T.unpack name)
  where
    matches =
      [ info
      | info <- classes,
        ciName info == name,
        tyConPackageId (ciTyCon info) == package,
        tyConModuleName (ciTyCon info) == moduleName'
      ]

lookupTyConFlavor :: TyConFlavor -> PackageId -> Text -> Text -> [TyConInfo] -> Either String TyConInfo
lookupTyConFlavor flavor package moduleName' name tyCons =
  case matches of
    [info] -> Right info
    [] -> Left ("missing checked type constructor " <> T.unpack moduleName' <> "." <> T.unpack name)
    _ -> Left ("duplicate checked type constructor " <> T.unpack moduleName' <> "." <> T.unpack name)
  where
    matches =
      [ info
      | info <- tyCons,
        tciName info == name,
        tciFlavor info == flavor,
        tyConPackageId (tciTyCon info) == package,
        tyConModuleName (tciTyCon info) == moduleName'
      ]

typeFamilyDeclName :: TypeFamilyDecl -> Text
typeFamilyDeclName familyDecl =
  fromMaybe "<type-family>" (familyHeadName (typeFamilyDeclHead familyDecl))

familyHeadName :: Syn.Type -> Maybe Text
familyHeadName ty =
  case Syn.peelTypeHead ty of
    Syn.TCon name _ -> Just (Syn.nameText name)
    Syn.TInfix _ name _ _ -> Just (Syn.nameText name)
    Syn.TApp function _ -> familyHeadName function
    Syn.TTypeApp function _ -> familyHeadName function
    _ -> Nothing

convertClass :: ConvertEnv -> ClassInfo -> Either String Decl
convertClass env info = do
  let tyVars = ciTyVars info
      bindersEnv = withTyVars tyVars env
      dictName = classDictTypeName (ciTyCon info)
  binders <- mapM (tyVarBinder bindersEnv) tyVars
  result <- convertKind bindersEnv KType
  superFields <- mapM (convertType bindersEnv) (ciSuperClassTypes info)
  methodFields <- mapM (convertMethodField bindersEnv (ciName info) tyVars) (ciMethods info)
  let dictApp = foldl TyApp (TyCon dictName) (map (TyVar . binderName) binders)
      body = foldr (funType bindersEnv) dictApp (superFields <> methodFields)
      constructorType = foldr TyForAll body binders
  pure
    ( DeclType
        TypeDecl
          { typeVis = Pub,
            typeName = dictName,
            typeBinders = binders,
            typeResult = result,
            typeRoles = replicate (length binders) Representational,
            typeCons = [ConDecl Pub (classDictConName (ciTyCon info)) constructorType]
          }
    )

convertMethodField :: ConvertEnv -> Text -> [TyVarId] -> (Text, TypeScheme) -> Either String Type
convertMethodField env className classTyVars (_methodName, scheme) = do
  fieldType <- classMethodFieldType className classTyVars scheme
  convertType env fieldType

classMethodFieldType :: Text -> [TyVarId] -> TypeScheme -> Either String TcType
classMethodFieldType className classTyVars (ForAll methodTyVars predicates body) = do
  remaining <- removeClassPredicate className predicates
  let extraTyVars = filter (`notElem` classTyVars) methodTyVars
      qualifiedBody =
        if null remaining
          then body
          else TcQualTy remaining body
  Right (foldr TcForAllTy qualifiedBody extraTyVars)

removeClassPredicate :: Text -> [Pred] -> Either String [Pred]
removeClassPredicate className predicates =
  case predicates of
    [] -> Left ("class method lacks its class predicate for " <> T.unpack className)
    ClassPred tyCon _ : rest
      | tyConName tyCon == className -> Right rest
    predicate : rest -> (predicate :) <$> removeClassPredicate className rest

convertNewtype :: ConvertEnv -> DataTypeInfo -> Either String [Decl]
convertNewtype env info = do
  let tyCon = dtiTyCon info
  kindVars <- extraKindVars env tyCon (dtiTyVars info)
  let tyVars = kindVars <> dtiTyVars info
      bindersEnv = withTyVars tyVars env
  binders <- mapM (tyVarBinder bindersEnv) tyVars
  result <- convertKind bindersEnv (dtiResultKind info)
  representation <-
    case dtiConstructors info of
      [constructor]
        | [field] <- dciFields constructor ->
            convertType bindersEnv (dcfiType field)
      _ -> Left ("newtype " <> T.unpack (dtiName info) <> " does not have exactly one checked field")
  let typeName = tyConNameFc2 env tyCon
      lhs = foldl TyApp (TyCon typeName) (map (TyVar . binderName) binders)
      axiomName = Name ("$ax$" <> dtiName info) SortAxiom (OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon))
  pure
    [ DeclType
        TypeDecl
          { typeVis = Pub,
            typeName = typeName,
            typeBinders = binders,
            typeResult = result,
            typeRoles = replicate (length binders) Representational,
            typeCons = []
          },
      DeclAxiom
        AxiomDecl
          { axiomVis = Private,
            axiomName = axiomName,
            axiomBinders = binders,
            axiomRole = Representational,
            axiomLeft = lhs,
            axiomRight = representation
          }
    ]

convertEmptyFamily :: ConvertEnv -> [Text] -> Role -> TyConInfo -> Either String Decl
convertEmptyFamily env paramNames roles info = do
  let tyCon = tciTyCon info
      constructorKind = typeSchemeBody (tciKindScheme info)
      argKinds = take (tciArity info) (visibleArgKinds constructorKind)
      names =
        if length paramNames == length argKinds
          then paramNames
          else ["a" <> T.pack (show index) | index <- [1 .. length argKinds]]
  binders <- zipWithM (kindBinder env) names argKinds
  result <- convertKind env (dropKindParams (length binders) constructorKind)
  pure
    ( DeclType
        TypeDecl
          { typeVis = Pub,
            typeName = tyConNameFc2 env tyCon,
            typeBinders = binders,
            typeResult = result,
            typeRoles = replicate (length binders) roles,
            typeCons = []
          }
    )

kindBinder :: ConvertEnv -> Text -> TcType -> Either String Binder
kindBinder env name kind = do
  converted <- convertKind env kind
  pure (Binder (Name name SortTypeVariable (OriginLocal (Unique 0))) converted)

visibleArgKinds :: TcType -> [TcType]
visibleArgKinds kind =
  case kind of
    KFun argument result -> argument : visibleArgKinds result
    _ -> []

dropKindParams :: Int -> TcType -> TcType
dropKindParams remaining kind
  | remaining <= 0 = kind
dropKindParams remaining (KFun _ result) = dropKindParams (remaining - 1) result
dropKindParams _ kind = kind

convertDataFamilyInst :: ConvertEnv -> PackageId -> Text -> [TcBindingResult] -> DataFamilyInstanceInfo -> Either String [Decl]
convertDataFamilyInst env package moduleName' bindings info = do
  let tyVars = dfiiTyVars info
      bindersEnv = withTyVars tyVars env
      representationTyCon = dfiiRepresentationTyCon info
      representationName = tyConNameFc2 env representationTyCon
  binders <- mapM (tyVarBinder bindersEnv) tyVars
  representationKind <- typeKindInEnv bindersEnv (TcTyCon representationTyCon (map TcTyVar tyVars))
  result <- convertKind bindersEnv representationKind
  familyType <- convertType bindersEnv (dfiiFamilyType info)
  let representationType = foldl TyApp (TyCon representationName) (map (TyVar . binderName) binders)
      familyAxiom =
        DeclAxiom
          AxiomDecl
            { axiomVis = Private,
              axiomName = Name (dfiiAxiomName info) SortAxiom (OriginTop package moduleName'),
              axiomBinders = binders,
              axiomRole = Nominal,
              axiomLeft = familyType,
              axiomRight = representationType
            }
  if dfiiIsNewtype info
    then do
      fieldType <-
        case dfiiConstructorNames info of
          constructorName : _ -> do
            constructorType <- lookupBindingType bindings constructorName
            converted <- convertType bindersEnv constructorType
            constructorFieldType converted
          [] -> Left "newtype family instance has no constructor"
      let representationAxiomName =
            Name ("$ax$" <> T.drop 1 (tyConName representationTyCon)) SortAxiom (OriginTop package moduleName')
      pure
        [ DeclType
            TypeDecl
              { typeVis = Private,
                typeName = representationName,
                typeBinders = binders,
                typeResult = result,
                typeRoles = replicate (length binders) Representational,
                typeCons = []
              },
          DeclAxiom
            AxiomDecl
              { axiomVis = Private,
                axiomName = representationAxiomName,
                axiomBinders = binders,
                axiomRole = Representational,
                axiomLeft = representationType,
                axiomRight = fieldType
              },
          familyAxiom
        ]
    else do
      constructors <- mapM (convertFamilyConstructor bindersEnv bindings package moduleName' representationType) (dfiiConstructorNames info)
      pure
        [ DeclType
            TypeDecl
              { typeVis = Private,
                typeName = representationName,
                typeBinders = binders,
                typeResult = result,
                typeRoles = replicate (length binders) Representational,
                typeCons = constructors
              },
          familyAxiom
        ]

convertFamilyConstructor :: ConvertEnv -> [TcBindingResult] -> PackageId -> Text -> Type -> Text -> Either String ConDecl
convertFamilyConstructor bindersEnv bindings package moduleName' representationType constructorName = do
  constructorType <- lookupBindingType bindings constructorName
  converted <- convertType bindersEnv constructorType
  replaced <- replaceResultType converted representationType
  pure
    ConDecl
      { conVis = Private,
        conName = Name constructorName SortDataConstructor (OriginTop package moduleName'),
        conType = replaced
      }

lookupBindingType :: [TcBindingResult] -> Text -> Either String TcType
lookupBindingType bindings name =
  case [tbType binding | binding <- bindings, tbName binding == name] of
    ty : _ -> Right ty
    [] -> Left ("missing checked constructor type " <> T.unpack name)

replaceResultType :: Type -> Type -> Either String Type
replaceResultType ty result =
  case ty of
    TyForAll binder body -> TyForAll binder <$> replaceResultType body result
    TyFun r1 r2 argument body -> TyFun r1 r2 argument <$> replaceResultType body result
    _ -> Right result

constructorFieldType :: Type -> Either String Type
constructorFieldType ty =
  case ty of
    TyForAll _ body -> constructorFieldType body
    TyFun _ _ argument _ -> Right argument
    _ -> Left "newtype family constructor is not a function"

convertTypeFamilyEquation :: ConvertEnv -> PackageId -> Text -> TypeFamilyInstanceInfo -> Either String Decl
convertTypeFamilyEquation env package moduleName' info = do
  let bindersEnv = withTyVars (tfiiTyVars info) env
  binders <- mapM (tyVarBinder bindersEnv) (tfiiTyVars info)
  left <- convertType bindersEnv (tfiiLeft info)
  right <- convertType bindersEnv (tfiiRight info)
  pure
    ( DeclAxiom
        AxiomDecl
          { axiomVis = Private,
            axiomName = Name (tfiiAxiomName info) SortAxiom (OriginTop package moduleName'),
            axiomBinders = binders,
            axiomRole = Nominal,
            axiomLeft = left,
            axiomRight = right
          }
    )

lookupSynonym :: PackageId -> Text -> Text -> [TyConInfo] -> Either String TyConInfo
lookupSynonym package moduleName' name tyCons =
  case matches of
    [info] -> Right info
    [] -> Left ("missing checked type synonym " <> T.unpack moduleName' <> "." <> T.unpack name)
    _ -> Left ("duplicate checked type synonym " <> T.unpack moduleName' <> "." <> T.unpack name)
  where
    matches =
      [ info
      | info <- tyCons,
        tciName info == name,
        tciFlavor info == SynonymTyCon,
        tyConPackageId (tciTyCon info) == package,
        tyConModuleName (tciTyCon info) == moduleName'
      ]

convertDataType :: ConvertEnv -> DataTypeInfo -> Either String Decl
convertDataType env info = do
  let tyCon = dtiTyCon info
  kindVars <- extraKindVars env tyCon (dtiTyVars info)
  let tyVars = kindVars <> dtiTyVars info
      bindersEnv = withTyVars tyVars env
  binders <- mapM (tyVarBinder bindersEnv) tyVars
  result <- convertKind bindersEnv (dtiResultKind info)
  constructors <- mapM (convertConstructor env) (dtiConstructors info)
  pure
    ( DeclType
        TypeDecl
          { typeVis = Pub,
            typeName = tyConNameFc2 env tyCon,
            typeBinders = binders,
            typeResult = result,
            typeRoles = replicate (length binders) Representational,
            typeCons = constructors
          }
    )

convertConstructor :: ConvertEnv -> DataConInfo -> Either String ConDecl
convertConstructor env info = do
  let tyVars = dciUnivTyVars info <> dciExTyVars info
      bindersEnv = withTyVars tyVars env
  binders <- mapM (tyVarBinder bindersEnv) tyVars
  predicates <- mapM (convertPred bindersEnv) (dciTheta info)
  fields <- mapM (convertType bindersEnv . dcfiType) (dciFields info)
  result <- convertType bindersEnv (dciResTy info)
  body <-
    constructorFun
      bindersEnv
      (replicate (length predicates) Nothing <> map (Just . dcfiType) (dciFields info))
      (predicates <> fields)
      (dciResTy info)
      result
  let constructorType = foldr TyForAll body binders
      (package, moduleName') = dciOrigin info
  pure
    ConDecl
      { conVis = Pub,
        conName = Name (dciName info) SortDataConstructor (OriginTop package moduleName'),
        conType = constructorType
      }

constructorFun :: ConvertEnv -> [Maybe TcType] -> [Type] -> TcType -> Type -> Either String Type
constructorFun env fieldTys convertedFields resultTy convertedResult =
  go (zip fieldTys convertedFields)
  where
    go [] = Right convertedResult
    go ((maybeField, converted) : rest) = do
      restType <- go rest
      r1 <- maybe (Right (liftedRepType env)) (typeRepOrLifted env) maybeField
      r2 <-
        if null rest
          then typeRepOrLifted env resultTy
          else Right (liftedRepType env)
      Right (TyFun r1 r2 converted restType)

typeRepOrLifted :: ConvertEnv -> TcType -> Either String Type
typeRepOrLifted env ty =
  case typeRep env ty of
    Right representation -> Right representation
    Left _ -> Right (liftedRepType env)

convertSynonym :: ConvertEnv -> TyConInfo -> Either String Decl
convertSynonym env info =
  case tciTypeSynonym info of
    Just synonym
      | Just body <- tsiBody synonym -> do
          let bindersEnv = withTyVars (tsiParams synonym) env
          binders <- mapM (tyVarBinder bindersEnv) (tsiParams synonym)
          result <- synonymResult bindersEnv (tciKindScheme info) (tsiParams synonym)
          convertedBody <- convertType bindersEnv body
          pure
            ( DeclSynonym
                SynonymDecl
                  { synVis = Pub,
                    synName = Name (tciName info) SortSynonym (OriginTop (tyConPackageId (tciTyCon info)) (tyConModuleName (tciTyCon info))),
                    synBinders = binders,
                    synResult = result,
                    synBody = convertedBody
                  }
            )
      | otherwise -> Left ("type synonym " <> T.unpack (tciName info) <> " has no body")
    Nothing -> Left ("type synonym " <> T.unpack (tciName info) <> " has no synonym info")

synonymResult :: ConvertEnv -> TypeScheme -> [TyVarId] -> Either String Type
synonymResult env scheme params =
  convertKind env (dropParams (length params) (typeSchemeBody scheme))
  where
    dropParams remaining kind
      | remaining <= 0 = kind
    dropParams remaining (KFun _ result) = dropParams (remaining - 1) result
    dropParams _ kind = kind

buildScopes :: (PackageId, Text) -> Imports -> [Decl] -> ScopeTable
buildScopes moduleOrigin imports decls =
  foldl
    ( \table (index, (package, moduleName')) ->
        insertScope index package moduleName' table
    )
    emptyScopeTable
    (zip [1 ..] origins)
  where
    origins =
      sort
        ( nub
            ( [moduleOrigin]
                <> importsOrigins imports
                <> concatMap declOrigins decls
            )
        )

importsOrigins :: Imports -> [(PackageId, Text)]
importsOrigins imports =
  concatMap (\(name, ty) -> nameOriginPair name <> typeOrigins ty) (Map.toList (importHeaders imports))
    <> concatMap (\(name, ty) -> nameOriginPair name <> typeOrigins ty) (Map.toList (importSynonyms imports))
    <> concatMap (\(name, declaration) -> nameOriginPair name <> axiomOrigins declaration) (Map.toList (importAxioms imports))
    <> concatMap (\(name, ty) -> nameOriginPair name <> typeOrigins ty) (Map.toList (importBinders imports))

axiomOrigins :: AxiomDecl -> [(PackageId, Text)]
axiomOrigins declaration =
  concatMap binderOrigins (axiomBinders declaration)
    <> typeOrigins (axiomLeft declaration)
    <> typeOrigins (axiomRight declaration)

declOrigins :: Decl -> [(PackageId, Text)]
declOrigins decl =
  case decl of
    DeclType typeDecl ->
      nameOriginPair (typeName typeDecl)
        <> concatMap binderOrigins (typeBinders typeDecl)
        <> typeOrigins (typeResult typeDecl)
        <> concatMap conOrigins (typeCons typeDecl)
    DeclSynonym synonymDecl ->
      nameOriginPair (synName synonymDecl)
        <> concatMap binderOrigins (synBinders synonymDecl)
        <> typeOrigins (synResult synonymDecl)
        <> typeOrigins (synBody synonymDecl)
    DeclAxiom axiomDecl ->
      nameOriginPair (axiomName axiomDecl)
        <> concatMap binderOrigins (axiomBinders axiomDecl)
        <> typeOrigins (axiomLeft axiomDecl)
        <> typeOrigins (axiomRight axiomDecl)
    DeclVal valDecl ->
      nameOriginPair (valName valDecl)
        <> typeOrigins (valType valDecl)
        <> exprOrigins (valBody valDecl)
    DeclForeignImport foreignImportDecl ->
      nameOriginPair (foreignImportName foreignImportDecl)
        <> concatMap foreignImportDependencyOrigins (foreignImportDependencies foreignImportDecl)
        <> typeOrigins (foreignImportType foreignImportDecl)

foreignImportDependencyOrigins :: ForeignImportDependency -> [(PackageId, Text)]
foreignImportDependencyOrigins dependency =
  case dependency of
    ForeignAxiom name -> nameOriginPair name
    ForeignConstructor name -> nameOriginPair name

conOrigins :: ConDecl -> [(PackageId, Text)]
conOrigins constructor =
  nameOriginPair (conName constructor) <> typeOrigins (conType constructor)

binderOrigins :: Binder -> [(PackageId, Text)]
binderOrigins binder = typeOrigins (binderType binder)

exprOrigins :: Expr -> [(PackageId, Text)]
exprOrigins expr =
  case expr of
    ExVar name -> nameOriginPair name
    ExLit literal -> literalOrigins literal
    ExApp function argument -> exprOrigins function <> exprOrigins argument
    ExTyApp function ty -> exprOrigins function <> typeOrigins ty
    ExLam binder body -> binderOrigins binder <> exprOrigins body
    ExTyLam binder body -> binderOrigins binder <> exprOrigins body
    ExLet bind body -> bindOrigins bind <> exprOrigins body
    ExRec binds body -> concatMap bindOrigins binds <> exprOrigins body
    ExCase scrutinee binder resultType alts ->
      exprOrigins scrutinee <> binderOrigins binder <> typeOrigins resultType <> concatMap altOrigins alts
    ExCast inner coercion -> exprOrigins inner <> coercionOrigins coercion

bindOrigins :: Bind -> [(PackageId, Text)]
bindOrigins bind = binderOrigins (bindBinder bind) <> exprOrigins (bindRhs bind)

altOrigins :: Alt -> [(PackageId, Text)]
altOrigins alternative =
  altConOrigins (altCon alternative)
    <> concatMap binderOrigins (altTypeBinders alternative)
    <> concatMap binderOrigins (altBinders alternative)
    <> exprOrigins (altRhs alternative)

altConOrigins :: AltCon -> [(PackageId, Text)]
altConOrigins alternative =
  case alternative of
    AltData name -> nameOriginPair name
    AltLit literal -> literalOrigins literal
    AltDefault -> []

literalOrigins :: Literal -> [(PackageId, Text)]
literalOrigins literal =
  case literal of
    LitInt representation _ -> typeOrigins representation
    LitChar representation _ -> typeOrigins representation
    LitAddr representation _ -> typeOrigins representation

coercionOrigins :: Coercion -> [(PackageId, Text)]
coercionOrigins coercion =
  case coercion of
    CoVar name -> nameOriginPair name
    CoRefl ty -> typeOrigins ty
    CoSym inner -> coercionOrigins inner
    CoTrans left right -> coercionOrigins left <> coercionOrigins right
    CoTyConApp name arguments -> nameOriginPair name <> concatMap coercionOrigins arguments
    CoAxiom name arguments -> nameOriginPair name <> concatMap typeOrigins arguments

typeOrigins :: Type -> [(PackageId, Text)]
typeOrigins ty =
  case ty of
    TyVar name -> nameOriginPair name
    TyCon name -> nameOriginPair name
    TyApp function argument -> typeOrigins function <> typeOrigins argument
    TyFun r1 r2 argument result ->
      typeOrigins r1 <> typeOrigins r2 <> typeOrigins argument <> typeOrigins result
    TyForAll binder body -> binderOrigins binder <> typeOrigins body
    TyEq left right -> typeOrigins left <> typeOrigins right

nameOriginPair :: Name -> [(PackageId, Text)]
nameOriginPair name =
  case nameOrigin name of
    OriginTop package moduleName' -> [(package, moduleName')]
    OriginLocal {} -> []

resolvedModuleOrigin :: Module -> (PackageId, Text)
resolvedModuleOrigin resolvedModule =
  fromMaybe ("", fromMaybe "Main" (Syn.moduleName resolvedModule)) $ do
    resolved <- listToMaybe (mapMaybe definitionResolution (Syn.moduleDecls resolvedModule))
    case resolutionTarget resolved of
      ResolvedTopLevel packageId name ->
        pure (packageId, fromMaybe (fromMaybe "Main" (Syn.moduleName resolvedModule)) (nameQualifier name))
      _ -> Nothing

definitionResolution :: Syn.Decl -> Maybe ResolutionAnnotation
definitionResolution declaration =
  case peelDeclAnn declaration of
    Syn.DeclValue (Syn.FunctionBind name _) -> nameResolution name
    Syn.DeclValue (Syn.PatternBind _ pattern' _) -> patternResolution pattern'
    Syn.DeclData dataDeclaration -> nameResolution (binderHeadName (dataDeclHead dataDeclaration))
    Syn.DeclTypeSyn synonymDeclaration -> nameResolution (binderHeadName (typeSynHead synonymDeclaration))
    Syn.DeclNewtype newtypeDeclaration -> nameResolution (binderHeadName (Syn.newtypeDeclHead newtypeDeclaration))
    Syn.DeclClass classDeclaration -> nameResolution (binderHeadName (Syn.classDeclHead classDeclaration))
    Syn.DeclDataFamilyDecl familyDeclaration -> nameResolution (binderHeadName (Syn.dataFamilyDeclHead familyDeclaration))
    Syn.DeclForeign foreignDecl -> nameResolution (Syn.foreignName foreignDecl)
    _ -> Nothing

patternResolution :: Syn.Pattern -> Maybe ResolutionAnnotation
patternResolution pattern' =
  case pattern' of
    Syn.PVar name -> nameResolution name
    Syn.PAnn _ inner -> patternResolution inner
    Syn.PParen inner -> patternResolution inner
    Syn.PStrict inner -> patternResolution inner
    Syn.PIrrefutable inner -> patternResolution inner
    Syn.PAs name _ -> nameResolution name
    Syn.PTypeSig inner _ -> patternResolution inner
    _ -> Nothing

nameResolution :: UnqualifiedName -> Maybe ResolutionAnnotation
nameResolution = listToMaybe . mapMaybe fromAnnotation . unqualifiedNameAnns
