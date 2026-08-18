{-# LANGUAGE OverloadedStrings #-}

-- | Convert a checked module into System FC 2 data types and synonyms.
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
    TypeSynDecl (..),
    UnqualifiedName,
    binderHeadName,
    fromAnnotation,
    nameQualifier,
    peelDeclAnn,
    unqualifiedNameAnns,
    unqualifiedNameText,
  )
import Aihc.Parser.Syntax qualified as Syn
import Aihc.Resolve (PackageId (..), ResolutionAnnotation (..), ResolvedName (..))
import Aihc.Tc
  ( DataConFieldInfo (..),
    DataConInfo (..),
    DataTypeInfo (..),
    TcBindingResult (..),
    TcInterface (..),
    TyConFlavor (..),
    TyConInfo (..),
    tcModuleDiagnostics,
    tcModuleSuccess,
  )
import Aihc.Tc.Env (TypeSynonymInfo (..))
import Aihc.Tc.Types
  ( Kind (..),
    TyCon,
    TyVarId,
    tyConKind,
    tyConModuleName,
    tyConPackageId,
  )
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
      env = emptyConvertEnv (primPackageId config)
      dataTypes = tcInterfaceDataTypes interface
      tyCons = tcInterfaceTyCons interface
  typeDecls <- concat <$> mapM (dsDecl env packageId currentModule dataTypes tyCons) (Syn.moduleDecls checked)
  valueDecls <- convertFcValues config bindings interface checked env
  let decls = typeDecls <> valueDecls
      scopes = buildScopes moduleId decls
  pure (Program moduleId scopes decls)

convertFcValues :: DesugarConfig -> [TcBindingResult] -> TcInterface -> Module -> ConvertEnv -> Either String [Decl]
convertFcValues config bindings interface checked env =
  let fcResult = desugarModuleWithInterface config bindings interface checked
   in if not (dsSuccess fcResult)
        then
          if null (dsErrors fcResult)
            then Right []
            else Left (unlines (dsErrors fcResult))
        else convertValueDecls env (fcProgramModule (dsProgram fcResult)) (fcTopBinds (dsProgram fcResult))

dsDecl :: ConvertEnv -> PackageId -> Text -> [DataTypeInfo] -> [TyConInfo] -> Syn.Decl -> Either String [Decl]
dsDecl env package moduleName' dataTypes tyCons decl =
  case peelDeclAnn decl of
    Syn.DeclData dataDecl -> do
      info <- lookupDataType DataTyCon package moduleName' (unqualifiedNameText (binderHeadName (dataDeclHead dataDecl))) dataTypes
      (: []) <$> convertDataType env info
    Syn.DeclTypeSyn synonymDecl -> do
      info <- lookupSynonym package moduleName' (unqualifiedNameText (binderHeadName (typeSynHead synonymDecl))) tyCons
      (: []) <$> convertSynonym env info
    Syn.DeclAnn _ inner -> dsDecl env package moduleName' dataTypes tyCons inner
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
      bindersEnv = withTyVars (dtiTyVars info) env
  binders <- mapM (tyVarBinder bindersEnv) (dtiTyVars info)
  result <- convertKind bindersEnv (dtiResultKind info)
  constructors <- mapM (convertConstructor env) (dtiConstructors info)
  pure
    ( DeclType
        TypeDecl
          { typeVis = Pub,
            typeName = tyConNameFc2 tyCon,
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
  let body = foldr (funType bindersEnv) result (predicates <> fields)
      constructorType = foldr TyForAll body binders
      (package, moduleName') = dciOrigin info
  pure
    ConDecl
      { conVis = Pub,
        conName = Name (dciName info) SortDataConstructor (OriginTop package moduleName'),
        conType = constructorType
      }

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
    ExCase scrutinee binder alts ->
      exprOrigins scrutinee <> binderOrigins binder <> concatMap altOrigins alts
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
    _ -> Nothing

nameResolution :: UnqualifiedName -> Maybe ResolutionAnnotation
nameResolution = listToMaybe . mapMaybe fromAnnotation . unqualifiedNameAnns
