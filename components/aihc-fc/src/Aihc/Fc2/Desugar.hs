{-# LANGUAGE OverloadedStrings #-}

-- | Convert a checked module into System FC 2 types, axioms, and values.
module Aihc.Fc2.Desugar
  ( desugarModuleFc2,
    Fc2DesugarResult (..),
  )
where

import Aihc.Fc.Desugar (DesugarConfig (..), DesugarResult (..), desugarModuleWithInterface)
import Aihc.Fc.Syntax (fcProgramModule, fcTopBinds)
import Aihc.Fc2.Convert
import Aihc.Fc2.FromFc (convertValueDecls)
import Aihc.Fc2.Name
import Aihc.Fc2.Syntax
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
    TcBindingResult (..),
    TcInterface (..),
    TyConFlavor (..),
    TyConInfo (..),
    TypeFamilyInstanceInfo (..),
    tcModuleDiagnostics,
    tcModuleSuccess,
  )
import Aihc.Tc.Env (TypeSynonymInfo (..))
import Aihc.Tc.Types
  ( Kind (..),
    Pred (..),
    TcType (..),
    TyCon,
    TyVarId,
    TypeScheme (..),
    Unique (..),
    tyConKey,
    tyConKind,
    tyConModuleName,
    tyConName,
    tyConPackageId,
    typeKind,
  )
import Control.Monad (zipWithM)
import Data.List (nub, sort)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

data Fc2DesugarResult = Fc2DesugarResult
  { ds2Program :: !Program,
    ds2Success :: !Bool,
    ds2Errors :: ![String]
  }
  deriving (Show)

desugarModuleFc2 :: DesugarConfig -> [TcBindingResult] -> TcInterface -> Module -> Fc2DesugarResult
desugarModuleFc2 config bindings interface checked =
  if not (tcModuleSuccess checked)
    then
      Fc2DesugarResult
        { ds2Program = Program (ModuleId "" (fromMaybe "Main" (Syn.moduleName checked))) emptyScopeTable [],
          ds2Success = False,
          ds2Errors = fmap show (tcModuleDiagnostics checked)
        }
    else case desugarChecked config bindings interface checked of
      Left errors ->
        Fc2DesugarResult
          { ds2Program = Program (ModuleId "" (fromMaybe "Main" (Syn.moduleName checked))) emptyScopeTable [],
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
      moduleId = ModuleId packageId currentModule
      dataTypes = tcInterfaceDataTypes interface
      tyCons = tcInterfaceTyCons interface
      classes = tcInterfaceClasses interface
      dataFamilyInstances = tcInterfaceDataFamilyInstances interface
      typeFamilyInstances = tcInterfaceTypeFamilyInstances interface
      env =
        withAxioms
          (axiomEntries packageId currentModule dataTypes dataFamilyInstances typeFamilyInstances)
          (withClassTyCons (map (tyConKey . ciTyCon) classes) (emptyConvertEnv (primPackageId config)))
  typeDecls <-
    concat
      <$> mapM
        (dsDecl env packageId currentModule dataTypes tyCons classes dataFamilyInstances typeFamilyInstances bindings)
        (Syn.moduleDecls checked)
  valueDecls <- convertFcValues config bindings interface checked env
  let decls = typeDecls <> valueDecls
      scopes = buildScopes moduleId decls
  pure (Program moduleId scopes decls)

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

convertFcValues :: DesugarConfig -> [TcBindingResult] -> TcInterface -> Module -> ConvertEnv -> Either String [Decl]
convertFcValues config bindings interface checked env =
  let fcResult = desugarModuleWithInterface config bindings interface checked
   in if not (dsSuccess fcResult)
        then
          if null (dsErrors fcResult)
            then Right []
            else Left (unlines (dsErrors fcResult))
        else convertValueDecls env (fcProgramModule (dsProgram fcResult)) (fcTopBinds (dsProgram fcResult))

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
            _ -> Left "System FC 2 accepts only foreign import prim"
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
      tyVars = extraKindVars tyCon (dtiTyVars info) <> dtiTyVars info
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
      argKinds = take (tciArity info) (visibleArgKinds (tyConKind tyCon))
      names =
        if length paramNames == length argKinds
          then paramNames
          else ["a" <> T.pack (show index) | index <- [1 .. length argKinds]]
  binders <- zipWithM (kindBinder env) names argKinds
  result <- convertKind env (dropKindParams (length binders) (tyConKind tyCon))
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

kindBinder :: ConvertEnv -> Text -> Kind -> Either String Binder
kindBinder env name kind = do
  converted <- convertKind env kind
  pure (Binder (Name name SortTypeVariable (OriginLocal (Unique 0))) converted)

visibleArgKinds :: Kind -> [Kind]
visibleArgKinds kind =
  case kind of
    KFun argument result -> argument : visibleArgKinds result
    _ -> []

dropKindParams :: Int -> Kind -> Kind
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
  result <- convertKind bindersEnv (typeKind (TcTyCon representationTyCon (map TcTyVar tyVars)))
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
      tyVars = extraKindVars tyCon (dtiTyVars info) <> dtiTyVars info
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
          result <- synonymResult bindersEnv (tciTyCon info) (tsiParams synonym)
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

synonymResult :: ConvertEnv -> TyCon -> [TyVarId] -> Either String Type
synonymResult env tyCon params =
  convertKind env (dropParams (length params) (tyConKind tyCon))
  where
    dropParams remaining kind
      | remaining <= 0 = kind
    dropParams remaining (KFun _ result) = dropParams (remaining - 1) result
    dropParams _ kind = kind

buildScopes :: ModuleId -> [Decl] -> ScopeTable
buildScopes moduleId decls =
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
            ( (modulePackage moduleId, Aihc.Fc2.Name.moduleName moduleId)
                : concatMap declOrigins decls
            )
        )

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
    DeclPrim primDecl ->
      nameOriginPair (primName primDecl)
        <> typeOrigins (primType primDecl)

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
    LitString {} -> []
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
    Syn.DeclData dataDeclaration -> nameResolution (binderHeadName (dataDeclHead dataDeclaration))
    Syn.DeclTypeSyn synonymDeclaration -> nameResolution (binderHeadName (typeSynHead synonymDeclaration))
    Syn.DeclNewtype newtypeDeclaration -> nameResolution (binderHeadName (Syn.newtypeDeclHead newtypeDeclaration))
    Syn.DeclClass classDeclaration -> nameResolution (binderHeadName (Syn.classDeclHead classDeclaration))
    Syn.DeclDataFamilyDecl familyDeclaration -> nameResolution (binderHeadName (Syn.dataFamilyDeclHead familyDeclaration))
    Syn.DeclForeign foreignDecl -> nameResolution (Syn.foreignName foreignDecl)
    _ -> Nothing

nameResolution :: UnqualifiedName -> Maybe ResolutionAnnotation
nameResolution = listToMaybe . mapMaybe fromAnnotation . unqualifiedNameAnns
