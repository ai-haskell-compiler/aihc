{-# LANGUAGE OverloadedStrings #-}

-- | Lowering from System FC 2 to strict, runtime-explicit GRIN.
module Aihc.Grin.Lower2
  ( lowerProgram,
    lowerProgramWithDependencies,
  )
where

import Aihc.Fc.Syntax qualified as Fc
import Aihc.Fc2 qualified as Fc2
import Aihc.Fc2.TypeOf qualified as Fc2Type
import Aihc.Grin.Lower qualified as Grin
import Aihc.Grin.Syntax (GrinProgram (..), GrinVar (..))
import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Types
  ( Kind (..),
    Levity (..),
    RuntimeRep (..),
    TcType (..),
    TyVarId (..),
    Unique (..),
    liftedRuntimeRep,
    setTyVarKind,
  )
import Control.Applicative ((<|>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)

data ConvertEnv = ConvertEnv
  { convertTypes :: !Fc2Type.TypeEnv,
    convertGlobals :: !(Map Fc2.Name Fc.Var),
    convertForeignCalls :: !(Map Fc2.Name Fc.FcForeignCall)
  }

type TypeVars = Map Fc2.Name TyVarId

-- | Lower one self-contained System FC 2 program.
lowerProgram :: Fc2.Program -> Either String GrinProgram
lowerProgram = lowerProgramWithDependencies []

-- | Lower one System FC 2 program with runtime facts from dependency programs.
lowerProgramWithDependencies :: [Fc2.Program] -> Fc2.Program -> Either String GrinProgram
lowerProgramWithDependencies dependencies source = do
  env <- conversionEnvironment (source : dependencies)
  convertedSource <- convertProgram env source
  convertedDependencies <- traverse (convertProgram env) dependencies
  let imported =
        mconcat
          [ Grin.extractGrinInterfaceWithLinkNames (programLinkNames dependency) dependency
          | dependency <- convertedDependencies
          ]
      linkNames = programLinkNames convertedSource
      lowered = Grin.lowerProgramWithInterfaceAndLinkNames linkNames imported convertedSource
  pure lowered {grinPrimitives = mergePrimitives (grinPrimitives lowered) (concatMap dependencyPrimitives convertedDependencies)}

dependencyPrimitives :: Fc.FcProgram -> [(GrinVar, Int)]
dependencyPrimitives program =
  concatMap lowerPrimitive (Fc.fcTopBinds program)
  where
    lowerPrimitive topBind =
      case topBind of
        Fc.FcPrimitive var _
          | Fc.varName var == "casMutVar#" -> [(GrinVar "aihcCasMutVarFlag" (sourceUnique var) IntRep, 3)]
        Fc.FcPrimitive var arity
          | Fc.varName var `notElem` ["aihcExit#", "unsafeCoerce#", "raise#", "catch#", "seq"] ->
              [(GrinVar (Fc.varName var) (sourceUnique var) liftedRuntimeRep, arity)]
        _ -> []
    sourceUnique var =
      case Fc.varUnique var of
        Unique unique -> unique

mergePrimitives :: [(GrinVar, Int)] -> [(GrinVar, Int)] -> [(GrinVar, Int)]
mergePrimitives local imported =
  Map.elems (Map.fromList [(grinVarName var, (var, arity)) | (var, arity) <- imported <> local])

conversionEnvironment :: [Fc2.Program] -> Either String ConvertEnv
conversionEnvironment programs = do
  let types = Fc2Type.typeEnvFromPrograms programs
      names = concatMap programTermNames programs
  globals <- Map.fromList <$> traverse (globalEntry types) names
  foreignCalls <- Map.fromList <$> traverse (foreignEntry types) (concatMap programForeignImports programs)
  pure
    ConvertEnv
      { convertTypes = types,
        convertGlobals = globals,
        convertForeignCalls = foreignCalls
      }

globalEntry :: Fc2Type.TypeEnv -> Fc2.Name -> Either String (Fc2.Name, Fc.Var)
globalEntry types name = do
  ty <- maybe (Left ("System FC 2 has no type for " <> show name)) Right (Fc2Type.lookupHeaderType types name)
  convertedType <- convertType types Map.empty ty
  pure (name, topVar name convertedType)

foreignEntry :: Fc2Type.TypeEnv -> Fc2.ForeignImportDecl -> Either String (Fc2.Name, Fc.FcForeignCall)
foreignEntry _ declaration =
  case Fc2.foreignImportCallingConvention declaration of
    Fc2.Prim -> Left "internal error: primitive import entered the C call table"
    Fc2.CCall specification ->
      pure
        ( Fc2.foreignImportName declaration,
          Fc.FcForeignCall
            { Fc.fcForeignCallName = Fc2.nameText (Fc2.foreignImportName declaration),
              Fc.fcForeignCallSymbol = Fc2.ccallSymbol specification,
              Fc.fcForeignCallSignature = convertForeignSignature specification
            }
        )

programTermNames :: Fc2.Program -> [Fc2.Name]
programTermNames program =
  concatMap declarationNames (Fc2.programDecls program)
  where
    declarationNames declaration =
      case declaration of
        Fc2.DeclType dataDeclaration -> map Fc2.conName (Fc2.typeCons dataDeclaration)
        Fc2.DeclVal valueDeclaration -> [Fc2.valName valueDeclaration]
        Fc2.DeclForeignImport foreignDeclaration -> [Fc2.foreignImportName foreignDeclaration]
        _ -> []

programForeignImports :: Fc2.Program -> [Fc2.ForeignImportDecl]
programForeignImports program =
  [ declaration
  | Fc2.DeclForeignImport declaration <- Fc2.programDecls program,
    Fc2.CCall {} <- [Fc2.foreignImportCallingConvention declaration]
  ]

convertProgram :: ConvertEnv -> Fc2.Program -> Either String Fc.FcProgram
convertProgram env program = do
  moduleId <- programModuleId program
  declarations <- concat <$> traverse (convertDecl env) (Fc2.programDecls program)
  pure (Fc.FcProgram moduleId declarations)

programModuleId :: Fc2.Program -> Either String Fc.FcModuleId
programModuleId program =
  case listToMaybe (concatMap declarationOrigins (Fc2.programDecls program)) of
    Just (package, moduleName) -> pure (Fc.FcModuleId package moduleName)
    Nothing ->
      case Fc2.scopeEntries (Fc2.programScopes program) of
        (_, package, moduleName) : _ -> pure (Fc.FcModuleId package moduleName)
        [] -> Left "System FC 2 program has no module identity"
  where
    declarationOrigins declaration =
      case declaration of
        Fc2.DeclType value -> originPair (Fc2.typeName value)
        Fc2.DeclSynonym value -> originPair (Fc2.synName value)
        Fc2.DeclAxiom value -> originPair (Fc2.axiomName value)
        Fc2.DeclVal value -> originPair (Fc2.valName value)
        Fc2.DeclForeignImport value -> originPair (Fc2.foreignImportName value)
    originPair name =
      case Fc2.nameOrigin name of
        Fc2.OriginTop package moduleName -> [(package, moduleName)]
        Fc2.OriginLocal {} -> []

programLinkNames :: Fc.FcProgram -> Grin.GrinLinkNames
programLinkNames program =
  Grin.linkNamesForProgram
    [packageName | let packageName = packageIdText (Fc.fcModulePackage (Fc.fcProgramModule program)), packageName /= ""]
    (T.splitOn "." (Fc.fcModuleName (Fc.fcProgramModule program)))
    program

convertDecl :: ConvertEnv -> Fc2.Decl -> Either String [Fc.FcTopBind]
convertDecl env declaration =
  case declaration of
    Fc2.DeclType dataDeclaration -> pure . Fc.FcData <$> convertDataDecl env dataDeclaration
    Fc2.DeclSynonym {} -> pure []
    Fc2.DeclAxiom {} -> pure []
    Fc2.DeclVal valueDeclaration -> pure . Fc.FcTopBind <$> convertValueDecl env valueDeclaration
    Fc2.DeclForeignImport foreignDeclaration -> pure <$> convertForeignDecl env foreignDeclaration

convertDataDecl :: ConvertEnv -> Fc2.TypeDecl -> Either String Fc.FcDataDecl
convertDataDecl env declaration = do
  (typeVars, _) <- convertTypeBinders (convertTypes env) Map.empty (Fc2.typeBinders declaration)
  resultKind <- convertKind (foldl Fc2Type.extendBinder (convertTypes env) (Fc2.typeBinders declaration)) (Fc2.typeResult declaration)
  constructors <- traverse (convertConstructor env) (Fc2.typeCons declaration)
  pure
    Fc.FcDataDecl
      { Fc.fcDataOrigin = symbolOrigin (Fc2.typeName declaration),
        Fc.fcDataName = Fc2.nameText (Fc2.typeName declaration),
        Fc.fcDataTyVars = typeVars,
        Fc.fcDataResultKind = resultKind,
        Fc.fcDataConstructors = constructors
      }

convertConstructor :: ConvertEnv -> Fc2.ConDecl -> Either String Fc.FcDataConDecl
convertConstructor env declaration = do
  fields <- constructorFields (convertTypes env) Map.empty (Fc2.conType declaration)
  pure
    Fc.FcDataConDecl
      { Fc.fcDataConOrigin = constructorOrigin (Fc2.conName declaration),
        Fc.fcDataConName = Fc2.nameText (Fc2.conName declaration),
        Fc.fcDataConFields = fields
      }

constructorFields :: Fc2Type.TypeEnv -> TypeVars -> Fc2.Type -> Either String [TcType]
constructorFields types typeVars ty =
  case Fc2Type.reduceType types ty of
    Fc2.TyForAll binder body -> do
      (typeVar, types', typeVars') <- convertTypeBinder types typeVars binder
      typeVar `seq` constructorFields types' typeVars' body
    Fc2.TyFun _ _ argument result ->
      (:) <$> convertType types typeVars argument <*> constructorFields types typeVars result
    _ -> pure []

convertValueDecl :: ConvertEnv -> Fc2.ValDecl -> Either String Fc.FcBind
convertValueDecl env declaration = do
  var <- lookupGlobal env (Fc2.valName declaration)
  rhs <- convertExpr env Map.empty Map.empty (Fc2.valBody declaration)
  pure (Fc.FcNonRec var rhs)

convertForeignDecl :: ConvertEnv -> Fc2.ForeignImportDecl -> Either String Fc.FcTopBind
convertForeignDecl env declaration =
  case Fc2.foreignImportCallingConvention declaration of
    Fc2.Prim -> do
      var <- lookupGlobal env (Fc2.foreignImportName declaration)
      pure (Fc.FcPrimitive var (termArity (Fc2.foreignImportType declaration)))
    Fc2.CCall {} ->
      Fc.FcForeignImport <$> lookupForeignCall env (Fc2.foreignImportName declaration)

termArity :: Fc2.Type -> Int
termArity ty =
  case ty of
    Fc2.TyForAll _ body -> termArity body
    Fc2.TyFun _ _ _ result -> 1 + termArity result
    _ -> 0

convertExpr :: ConvertEnv -> TypeVars -> Map Fc2.Name Fc.Var -> Fc2.Expr -> Either String Fc.FcExpr
convertExpr env typeVars locals expression =
  case foreignApplication (convertForeignCalls env) expression of
    Just (foreignCall, arguments) ->
      Fc.FcCallForeign foreignCall <$> traverse (convertExpr env typeVars locals) arguments
    Nothing -> convertOrdinaryExpr env typeVars locals expression

convertOrdinaryExpr :: ConvertEnv -> TypeVars -> Map Fc2.Name Fc.Var -> Fc2.Expr -> Either String Fc.FcExpr
convertOrdinaryExpr env typeVars locals expression =
  case expression of
    Fc2.ExVar name
      | name `Map.member` convertForeignCalls env ->
          Left ("foreign import ccall is not saturated: " <> T.unpack (Fc2.nameText name))
      | otherwise -> Fc.FcVar <$> lookupTerm env locals name
    Fc2.ExLit literal -> do
      converted <- convertLiteral (convertTypes env) typeVars literal
      pure (Fc.FcLit converted (literalType converted))
    Fc2.ExApp function argument ->
      Fc.FcApp <$> convertExpr env typeVars locals function <*> convertExpr env typeVars locals argument
    Fc2.ExTyApp function argument ->
      Fc.FcTyApp <$> convertExpr env typeVars locals function <*> convertType (convertTypes env) typeVars argument
    Fc2.ExLam binder body -> do
      var <- convertTermBinder (convertTypes env) typeVars binder
      Fc.FcLam var <$> convertExpr env typeVars (Map.insert (Fc2.binderName binder) var locals) body
    Fc2.ExTyLam binder body -> do
      (typeVar, _, typeVars') <- convertTypeBinder (convertTypes env) typeVars binder
      Fc.FcTyLam typeVar <$> convertExpr env typeVars' locals body
    Fc2.ExLet binding body -> do
      convertedRhs <- convertExpr env typeVars locals (Fc2.bindRhs binding)
      var <- convertTermBinder (convertTypes env) typeVars (Fc2.bindBinder binding)
      convertedBody <- convertExpr env typeVars (Map.insert (Fc2.binderName (Fc2.bindBinder binding)) var locals) body
      pure (Fc.FcLet (Fc.FcNonRec var convertedRhs) convertedBody)
    Fc2.ExRec bindings body -> do
      vars <- traverse (convertTermBinder (convertTypes env) typeVars . Fc2.bindBinder) bindings
      let locals' = Map.fromList (zip (map (Fc2.binderName . Fc2.bindBinder) bindings) vars) <> locals
      rhss <- traverse (convertExpr env typeVars locals' . Fc2.bindRhs) bindings
      Fc.FcLet (Fc.FcRec (zip vars rhss)) <$> convertExpr env typeVars locals' body
    Fc2.ExCase scrutinee binder _ alternatives -> do
      scrutinee' <- convertExpr env typeVars locals scrutinee
      binder' <- convertTermBinder (convertTypes env) typeVars binder
      alternatives' <- traverse (convertAlt env typeVars (Map.insert (Fc2.binderName binder) binder' locals)) alternatives
      pure (Fc.FcCase scrutinee' binder' alternatives')
    Fc2.ExCast inner _ -> convertExpr env typeVars locals inner

foreignApplication :: Map Fc2.Name Fc.FcForeignCall -> Fc2.Expr -> Maybe (Fc.FcForeignCall, [Fc2.Expr])
foreignApplication foreignCalls expression =
  case collectApplications expression of
    (Fc2.ExVar name, arguments)
      | Just foreignCall <- Map.lookup name foreignCalls,
        length arguments == length (Fc.fcForeignOperandTypes (Fc.fcForeignCallSignature foreignCall)) ->
          Just (foreignCall, arguments)
    _ -> Nothing

collectApplications :: Fc2.Expr -> (Fc2.Expr, [Fc2.Expr])
collectApplications expression =
  case expression of
    Fc2.ExApp function argument ->
      let (headExpression, arguments) = collectApplications function
       in (headExpression, arguments <> [argument])
    Fc2.ExTyApp function _ -> collectApplications function
    _ -> (expression, [])

convertAlt :: ConvertEnv -> TypeVars -> Map Fc2.Name Fc.Var -> Fc2.Alt -> Either String Fc.FcAlt
convertAlt env typeVars locals alternative = do
  binders <- traverse (convertTermBinder (convertTypes env) typeVars) (Fc2.altBinders alternative)
  let locals' = Map.fromList (zip (map Fc2.binderName (Fc2.altBinders alternative)) binders) <> locals
  rhs <- convertExpr env typeVars locals' (Fc2.altRhs alternative)
  altCon <- convertAltCon (convertTypes env) typeVars (Fc2.altCon alternative)
  pure (Fc.FcAlt altCon binders rhs)

convertAltCon :: Fc2Type.TypeEnv -> TypeVars -> Fc2.AltCon -> Either String Fc.FcAltCon
convertAltCon types typeVars altCon =
  case altCon of
    Fc2.AltData name -> pure (Fc.DataAlt (constructorOrigin name))
    Fc2.AltLit literal -> do
      converted <- convertLiteral types typeVars literal
      pure (Fc.LitAlt converted (literalType converted))
    Fc2.AltDefault -> pure Fc.DefaultAlt

convertTermBinder :: Fc2Type.TypeEnv -> TypeVars -> Fc2.Binder -> Either String Fc.Var
convertTermBinder types typeVars binder = do
  ty <- convertType types typeVars (Fc2.binderType binder)
  pure (localVar (Fc2.binderName binder) ty)

convertTypeBinders :: Fc2Type.TypeEnv -> TypeVars -> [Fc2.Binder] -> Either String ([TyVarId], TypeVars)
convertTypeBinders _ typeVars [] = pure ([], typeVars)
convertTypeBinders types typeVars (binder : binders) = do
  (typeVar, types', typeVars') <- convertTypeBinder types typeVars binder
  (rest, finalTypeVars) <- convertTypeBinders types' typeVars' binders
  pure (typeVar : rest, finalTypeVars)

convertTypeBinder :: Fc2Type.TypeEnv -> TypeVars -> Fc2.Binder -> Either String (TyVarId, Fc2Type.TypeEnv, TypeVars)
convertTypeBinder types typeVars binder = do
  kind <- convertKind types (Fc2.binderType binder)
  unique <- nameUnique (Fc2.binderName binder)
  let typeVar = setTyVarKind kind (TyVarId (Fc2.nameText (Fc2.binderName binder)) unique)
  pure (typeVar, Fc2Type.extendBinder types binder, Map.insert (Fc2.binderName binder) typeVar typeVars)

convertType :: Fc2Type.TypeEnv -> TypeVars -> Fc2.Type -> Either String TcType
convertType types typeVars sourceType =
  case Fc2Type.reduceType types sourceType of
    Fc2.TyVar name ->
      maybe (Left ("unbound System FC 2 type variable: " <> show name)) (pure . TcTyVar) (Map.lookup name typeVars)
    Fc2.TyFun _ _ argument result ->
      TcFunTy <$> convertType types typeVars argument <*> convertType types typeVars result
    Fc2.TyForAll binder body -> do
      (typeVar, types', typeVars') <- convertTypeBinder types typeVars binder
      TcForAllTy typeVar <$> convertType types' typeVars' body
    reduced -> do
      kind <- kindOfType types reduced
      let name = syntheticTypeName reduced kind
      pure (TcTyCon (Fc.legacyTyConWithKind name 0 kind) [])

kindOfType :: Fc2Type.TypeEnv -> Fc2.Type -> Either String Kind
kindOfType types ty =
  case Fc2Type.typeOf types ty of
    Nothing -> Left ("System FC 2 type has no kind: " <> show ty)
    Just kind -> convertKind types kind

convertKind :: Fc2Type.TypeEnv -> Fc2.Type -> Either String Kind
convertKind types sourceKind =
  case Fc2Type.reduceType types sourceKind of
    Fc2.TyApp (Fc2.TyCon name) representation
      | Fc2.nameText name == "TYPE" -> KTYPE <$> convertRep types representation
    Fc2.TyFun _ _ argument result -> KFun <$> convertKind types argument <*> convertKind types result
    Fc2.TyCon name ->
      case Fc2.nameText name of
        "RuntimeRep" -> pure KRuntimeRep
        "Levity" -> pure KLevity
        "VecCount" -> pure KVecCount
        "VecElem" -> pure KVecElem
        other -> Left ("unsupported System FC 2 kind constructor: " <> T.unpack other)
    kind -> Left ("unsupported System FC 2 kind: " <> show kind)

convertRep :: Fc2Type.TypeEnv -> Fc2.Type -> Either String RuntimeRep
convertRep types sourceRep =
  case Fc2Type.reduceType types sourceRep of
    Fc2.TyVar name -> RuntimeRepVar <$> nameUnique name
    Fc2.TyCon name -> simpleRep (Fc2.nameText name)
    Fc2.TyApp (Fc2.TyCon name) levity
      | Fc2.nameText name == "BoxedRep" -> BoxedRep <$> convertLevity levity
    Fc2.TyApp (Fc2.TyCon name) fields
      | Fc2.nameText name == "TupleRep" -> TupleRep <$> convertRepList types fields
      | Fc2.nameText name == "SumRep" -> SumRep <$> convertRepList types fields
    Fc2.TyApp (Fc2.TyApp (Fc2.TyCon name) count) element
      | Fc2.nameText name == "VecRep" -> VecRep <$> readNamed "vector count" count <*> readNamed "vector element" element
    rep -> Left ("unsupported System FC 2 runtime representation: " <> show rep)

simpleRep :: Text -> Either String RuntimeRep
simpleRep name =
  case name of
    "LiftedRep" -> pure (BoxedRep Lifted)
    "UnliftedRep" -> pure (BoxedRep Unlifted)
    "IntRep" -> pure IntRep
    "Int8Rep" -> pure Int8Rep
    "Int16Rep" -> pure Int16Rep
    "Int32Rep" -> pure Int32Rep
    "Int64Rep" -> pure Int64Rep
    "WordRep" -> pure WordRep
    "Word8Rep" -> pure Word8Rep
    "Word16Rep" -> pure Word16Rep
    "Word32Rep" -> pure Word32Rep
    "Word64Rep" -> pure Word64Rep
    "AddrRep" -> pure AddrRep
    "FloatRep" -> pure FloatRep
    "DoubleRep" -> pure DoubleRep
    _ -> Left ("unknown System FC 2 runtime representation: " <> T.unpack name)

convertLevity :: Fc2.Type -> Either String Levity
convertLevity levity =
  case levity of
    Fc2.TyCon name
      | Fc2.nameText name == "Lifted" -> pure Lifted
      | Fc2.nameText name == "Unlifted" -> pure Unlifted
    _ -> Left ("unsupported System FC 2 levity: " <> show levity)

convertRepList :: Fc2Type.TypeEnv -> Fc2.Type -> Either String [RuntimeRep]
convertRepList types list =
  case list of
    Fc2.TyApp (Fc2.TyCon name) _
      | Fc2.nameText name == "[]" -> pure []
    Fc2.TyApp (Fc2.TyApp (Fc2.TyApp (Fc2.TyCon name) _) item) rest
      | Fc2.nameText name == ":" -> (:) <$> convertRep types item <*> convertRepList types rest
    _ -> Left ("unsupported System FC 2 runtime representation list: " <> show list)

readNamed :: (Read value) => String -> Fc2.Type -> Either String value
readNamed label ty =
  case ty of
    Fc2.TyCon name ->
      maybe (Left ("unknown " <> label <> ": " <> T.unpack (Fc2.nameText name))) pure (readMaybe (T.unpack (Fc2.nameText name)))
    _ -> Left ("unsupported " <> label <> ": " <> show ty)

syntheticTypeName :: Fc2.Type -> Kind -> Text
syntheticTypeName ty kind =
  case (ty, kind) of
    (Fc2.TyCon name, KRuntimeRep) -> "'" <> Fc2.nameText name
    _ -> T.pack (show ty)

convertLiteral :: Fc2Type.TypeEnv -> TypeVars -> Fc2.Literal -> Either String Fc.Literal
convertLiteral types _ literal =
  case literal of
    Fc2.LitInt representation value -> Fc.LitInt <$> convertRep types representation <*> pure value
    Fc2.LitChar representation value -> Fc.LitChar <$> convertRep types representation <*> pure value
    Fc2.LitString value -> pure (Fc.LitString value)
    Fc2.LitAddr representation value -> do
      rep <- convertRep types representation
      if rep == AddrRep
        then pure (Fc.LitAddr value)
        else Left ("System FC 2 address literal has representation " <> show rep)

literalType :: Fc.Literal -> TcType
literalType literal =
  TcTyCon (Fc.legacyTyConWithKind "$fc2-literal" 0 (KTYPE (Fc.literalRuntimeRep literal))) []

convertForeignSignature :: Fc2.CCallSpec -> Fc.FcForeignSignature
convertForeignSignature specification =
  Fc.FcForeignSignature
    { Fc.fcForeignArgumentTypes = map convertForeignType (Fc2.ccallArgumentTypes specification),
      Fc.fcForeignResultType = convertForeignType (Fc2.ccallResultType specification),
      Fc.fcForeignEffect = convertForeignEffect (Fc2.ccallEffect specification)
    }

convertForeignEffect :: Fc2.ForeignEffect -> Fc.FcForeignEffect
convertForeignEffect effect =
  case effect of
    Fc2.ForeignPure -> Fc.FcForeignPure
    Fc2.ForeignRealWorld -> Fc.FcForeignRealWorld

convertForeignType :: Fc2.CAbiType -> Fc.FcForeignType
convertForeignType foreignType =
  case foreignType of
    Fc2.CAbiInt -> Fc.FcForeignInt
    Fc2.CAbiInt32 -> Fc.FcForeignInt32
    Fc2.CAbiWord64 -> Fc.FcForeignWord64
    Fc2.CAbiAddr -> Fc.FcForeignAddr

lookupTerm :: ConvertEnv -> Map Fc2.Name Fc.Var -> Fc2.Name -> Either String Fc.Var
lookupTerm env locals name =
  maybe (Left ("unbound System FC 2 value: " <> show name)) pure (Map.lookup name locals <|> Map.lookup name (convertGlobals env))

lookupGlobal :: ConvertEnv -> Fc2.Name -> Either String Fc.Var
lookupGlobal env name =
  maybe (Left ("missing System FC 2 global: " <> show name)) pure (Map.lookup name (convertGlobals env))

lookupForeignCall :: ConvertEnv -> Fc2.Name -> Either String Fc.FcForeignCall
lookupForeignCall env name =
  maybe (Left ("missing System FC 2 foreign call: " <> show name)) pure (Map.lookup name (convertForeignCalls env))

topVar :: Fc2.Name -> TcType -> Fc.Var
topVar name ty =
  case Fc2.nameOrigin name of
    Fc2.OriginTop package moduleName ->
      Fc.fcExternalVar (Fc.FcTopLevelOrigin (packageIdText package) moduleName (Fc2.nameText name)) ty
    Fc2.OriginLocal unique -> Fc.Var (Fc2.nameText name) unique ty

localVar :: Fc2.Name -> TcType -> Fc.Var
localVar name ty =
  case Fc2.nameOrigin name of
    Fc2.OriginLocal unique -> Fc.Var (Fc2.nameText name) unique ty
    Fc2.OriginTop {} -> topVar name ty

symbolOrigin :: Fc2.Name -> Fc.FcSymbolOrigin
symbolOrigin name =
  case Fc2.nameOrigin name of
    Fc2.OriginTop package moduleName -> Fc.FcTopLevelOrigin (packageIdText package) moduleName (Fc2.nameText name)
    Fc2.OriginLocal {} -> Fc.FcBuiltinOrigin (Fc2.nameText name)

constructorOrigin :: Fc2.Name -> Fc.FcConstructorId
constructorOrigin name =
  case Fc2.nameOrigin name of
    Fc2.OriginTop package moduleName -> Fc.FcConstructorId package moduleName (Fc2.nameText name)
    Fc2.OriginLocal {} -> error "System FC 2 data constructor has a local origin"

nameUnique :: Fc2.Name -> Either String Unique
nameUnique name =
  case Fc2.nameOrigin name of
    Fc2.OriginLocal unique -> pure unique
    Fc2.OriginTop {} -> Left ("System FC 2 binder has a top-level origin: " <> show name)
