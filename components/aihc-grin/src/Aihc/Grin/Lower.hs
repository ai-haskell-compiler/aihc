{-# LANGUAGE OverloadedStrings #-}

-- | Lowering from System FC 2 to strict, runtime-explicit GRIN.
module Aihc.Grin.Lower
  ( lowerProgram,
    lowerProgramWithDependencies,
  )
where

import Aihc.Fc2 qualified as Fc2
import Aihc.Fc2.TypeOf qualified as Fc2Type
import Aihc.Grin.Lower.Closure qualified as Closure
import Aihc.Grin.Lower.Runtime
import Aihc.Grin.Lower.Runtime qualified as Runtime
import Aihc.Grin.Syntax (GrinLevity (..), GrinProgram (..), GrinRep (..), GrinVar (..))
import Aihc.Resolve (PackageId (..))
import Control.Applicative ((<|>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)

data ConvertEnv = ConvertEnv
  { convertTypes :: !Fc2Type.TypeEnv,
    convertGlobals :: !(Map Fc2.Name Runtime.RuntimeVar),
    convertForeignCalls :: !(Map Fc2.Name Runtime.RuntimeForeignCall)
  }

type TypeVars = Map Fc2.Name RuntimeType

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
          [ Closure.extractGrinInterfaceWithLinkNames (programLinkNames dependency) dependency
          | dependency <- convertedDependencies
          ]
      linkNames = programLinkNames convertedSource
      lowered = Closure.lowerProgramWithInterfaceAndLinkNames linkNames imported convertedSource
  pure lowered {grinPrimitives = mergePrimitives (grinPrimitives lowered) (concatMap dependencyPrimitives convertedDependencies)}

dependencyPrimitives :: Runtime.RuntimeProgram -> [(GrinVar, Int)]
dependencyPrimitives program =
  concatMap lowerPrimitive (Runtime.runtimeTopBinds program)
  where
    lowerPrimitive topBind =
      case topBind of
        Runtime.RuntimePrimitive var _
          | Runtime.varName var == "casMutVar#" -> [(GrinVar "aihcCasMutVarFlag" (sourceUnique var) IntRep, 3)]
        Runtime.RuntimePrimitive var arity
          | Runtime.varName var `notElem` ["aihcExit#", "unsafeCoerce#", "raise#", "catch#", "seq"] ->
              [(GrinVar (Runtime.varName var) (sourceUnique var) liftedRuntimeRep, arity)]
        _ -> []
    sourceUnique var =
      case Runtime.varUnique var of
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

globalEntry :: Fc2Type.TypeEnv -> Fc2.Name -> Either String (Fc2.Name, Runtime.RuntimeVar)
globalEntry types name = do
  ty <- maybe (Left ("System FC 2 has no type for " <> show name)) Right (Fc2Type.lookupHeaderType types name)
  convertedType <- convertType types Map.empty ty
  pure (name, topVar name convertedType)

foreignEntry :: Fc2Type.TypeEnv -> Fc2.ForeignImportDecl -> Either String (Fc2.Name, Runtime.RuntimeForeignCall)
foreignEntry _ declaration =
  case Fc2.foreignImportCallingConvention declaration of
    Fc2.Prim -> Left "internal error: primitive import entered the C call table"
    Fc2.CCall specification ->
      pure
        ( Fc2.foreignImportName declaration,
          Runtime.RuntimeForeignCall
            { Runtime.runtimeForeignCallName = Fc2.nameText (Fc2.foreignImportName declaration),
              Runtime.runtimeForeignCallSymbol = Fc2.ccallSymbol specification,
              Runtime.runtimeForeignCallSignature = convertForeignSignature specification
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

convertProgram :: ConvertEnv -> Fc2.Program -> Either String Runtime.RuntimeProgram
convertProgram env program = do
  moduleId <- programModuleId program
  declarations <- concat <$> traverse (convertDecl env) (Fc2.programDecls program)
  pure (Runtime.RuntimeProgram moduleId declarations)

programModuleId :: Fc2.Program -> Either String Runtime.RuntimeModuleId
programModuleId program =
  case listToMaybe (concatMap declarationOrigins (Fc2.programDecls program)) of
    Just (package, moduleName) -> pure (Runtime.RuntimeModuleId package moduleName)
    Nothing ->
      case Fc2.scopeEntries (Fc2.programScopes program) of
        (_, package, moduleName) : _ -> pure (Runtime.RuntimeModuleId package moduleName)
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

programLinkNames :: Runtime.RuntimeProgram -> Closure.GrinLinkNames
programLinkNames program =
  Closure.linkNamesForProgram
    [packageName | let packageName = packageIdText (Runtime.runtimeModulePackage (Runtime.runtimeProgramModule program)), packageName /= ""]
    (T.splitOn "." (Runtime.runtimeModuleName (Runtime.runtimeProgramModule program)))
    program

convertDecl :: ConvertEnv -> Fc2.Decl -> Either String [Runtime.RuntimeTopBind]
convertDecl env declaration =
  case declaration of
    Fc2.DeclType dataDeclaration -> pure . Runtime.RuntimeData <$> convertDataDecl env dataDeclaration
    Fc2.DeclSynonym {} -> pure []
    Fc2.DeclAxiom {} -> pure []
    Fc2.DeclVal valueDeclaration -> pure . Runtime.RuntimeTopValue <$> convertValueDecl env valueDeclaration
    Fc2.DeclForeignImport foreignDeclaration -> pure <$> convertForeignDecl env foreignDeclaration

convertDataDecl :: ConvertEnv -> Fc2.TypeDecl -> Either String Runtime.RuntimeDataDecl
convertDataDecl env declaration = do
  (types, _) <- extendTypeBinders (convertTypes env) Map.empty (Fc2.typeBinders declaration)
  resultLayout <- convertValueLayout types (Fc2.typeResult declaration)
  constructors <- traverse (convertConstructor env) (Fc2.typeCons declaration)
  pure
    Runtime.RuntimeDataDecl
      { Runtime.runtimeDataIsUnboxedTuple = isTupleLayout resultLayout,
        Runtime.runtimeDataConstructors = constructors
      }
  where
    isTupleLayout runtimeTypeInfo =
      case runtimeTypeInfo of
        RuntimeValue (ConcreteLayout TupleRep {}) -> True
        _ -> False

convertConstructor :: ConvertEnv -> Fc2.ConDecl -> Either String Runtime.RuntimeDataConDecl
convertConstructor env declaration = do
  fields <- constructorFields (convertTypes env) Map.empty (Fc2.conType declaration)
  pure
    Runtime.RuntimeDataConDecl
      { Runtime.runtimeDataConOrigin = constructorOrigin (Fc2.conName declaration),
        Runtime.runtimeDataConName = Fc2.nameText (Fc2.conName declaration),
        Runtime.runtimeDataConFields = fields
      }

constructorFields :: Fc2Type.TypeEnv -> TypeVars -> Fc2.Type -> Either String [RuntimeType]
constructorFields types typeVars ty =
  case Fc2Type.reduceType types ty of
    Fc2.TyForAll binder body -> do
      (_, types', typeVars') <- convertTypeBinder types typeVars binder
      constructorFields types' typeVars' body
    Fc2.TyFun _ _ argument result ->
      (:) <$> convertType types typeVars argument <*> constructorFields types typeVars result
    _ -> pure []

convertValueDecl :: ConvertEnv -> Fc2.ValDecl -> Either String Runtime.RuntimeBind
convertValueDecl env declaration = do
  var <- lookupGlobal env (Fc2.valName declaration)
  rhs <- convertExpr env Map.empty Map.empty (Fc2.valBody declaration)
  pure (Runtime.RuntimeNonRec var rhs)

convertForeignDecl :: ConvertEnv -> Fc2.ForeignImportDecl -> Either String Runtime.RuntimeTopBind
convertForeignDecl env declaration =
  case Fc2.foreignImportCallingConvention declaration of
    Fc2.Prim -> do
      var <- lookupGlobal env (Fc2.foreignImportName declaration)
      pure (Runtime.RuntimePrimitive var (termArity (Fc2.foreignImportType declaration)))
    Fc2.CCall {} ->
      Runtime.RuntimeForeignImport <$> lookupForeignCall env (Fc2.foreignImportName declaration)

termArity :: Fc2.Type -> Int
termArity ty =
  case ty of
    Fc2.TyForAll _ body -> termArity body
    Fc2.TyFun _ _ _ result -> 1 + termArity result
    _ -> 0

convertExpr :: ConvertEnv -> TypeVars -> Map Fc2.Name Runtime.RuntimeVar -> Fc2.Expr -> Either String Runtime.RuntimeExpr
convertExpr env typeVars locals expression =
  case foreignApplication (convertForeignCalls env) expression of
    Just (foreignCall, arguments) ->
      Runtime.RuntimeCallForeign foreignCall <$> traverse (convertExpr env typeVars locals) arguments
    Nothing -> convertOrdinaryExpr env typeVars locals expression

convertOrdinaryExpr :: ConvertEnv -> TypeVars -> Map Fc2.Name Runtime.RuntimeVar -> Fc2.Expr -> Either String Runtime.RuntimeExpr
convertOrdinaryExpr env typeVars locals expression =
  case expression of
    Fc2.ExVar name
      | name `Map.member` convertForeignCalls env ->
          Left ("foreign import ccall is not saturated: " <> T.unpack (Fc2.nameText name))
      | otherwise -> Runtime.RuntimeVarExpr <$> lookupTerm env locals name
    Fc2.ExLit literal -> do
      converted <- convertLiteral (convertTypes env) typeVars literal
      pure (Runtime.RuntimeLit converted (literalType converted))
    Fc2.ExApp function argument ->
      Runtime.RuntimeApp <$> convertExpr env typeVars locals function <*> convertExpr env typeVars locals argument
    Fc2.ExTyApp function argument ->
      Runtime.RuntimeTyApp <$> convertExpr env typeVars locals function <*> convertType (convertTypes env) typeVars argument
    Fc2.ExLam binder body -> do
      var <- convertTermBinder (convertTypes env) typeVars binder
      Runtime.RuntimeLam var <$> convertExpr env typeVars (Map.insert (Fc2.binderName binder) var locals) body
    Fc2.ExTyLam binder body -> do
      (typeVar, _, typeVars') <- convertTypeBinder (convertTypes env) typeVars binder
      Runtime.RuntimeTyLam typeVar <$> convertExpr env typeVars' locals body
    Fc2.ExLet binding body -> do
      convertedRhs <- convertExpr env typeVars locals (Fc2.bindRhs binding)
      var <- convertTermBinder (convertTypes env) typeVars (Fc2.bindBinder binding)
      convertedBody <- convertExpr env typeVars (Map.insert (Fc2.binderName (Fc2.bindBinder binding)) var locals) body
      pure (Runtime.RuntimeLet (Runtime.RuntimeNonRec var convertedRhs) convertedBody)
    Fc2.ExRec bindings body -> do
      vars <- traverse (convertTermBinder (convertTypes env) typeVars . Fc2.bindBinder) bindings
      let locals' = Map.fromList (zip (map (Fc2.binderName . Fc2.bindBinder) bindings) vars) <> locals
      rhss <- traverse (convertExpr env typeVars locals' . Fc2.bindRhs) bindings
      Runtime.RuntimeLet (Runtime.RuntimeRec (zip vars rhss)) <$> convertExpr env typeVars locals' body
    Fc2.ExCase scrutinee binder _ alternatives -> do
      scrutinee' <- convertExpr env typeVars locals scrutinee
      binder' <- convertTermBinder (convertTypes env) typeVars binder
      alternatives' <- traverse (convertAlt env typeVars (Map.insert (Fc2.binderName binder) binder' locals)) alternatives
      pure (Runtime.RuntimeCase scrutinee' binder' alternatives')
    Fc2.ExCast inner _ -> convertExpr env typeVars locals inner

foreignApplication :: Map Fc2.Name Runtime.RuntimeForeignCall -> Fc2.Expr -> Maybe (Runtime.RuntimeForeignCall, [Fc2.Expr])
foreignApplication foreignCalls expression =
  case collectApplications expression of
    (Fc2.ExVar name, arguments)
      | Just foreignCall <- Map.lookup name foreignCalls,
        length arguments == length (Runtime.runtimeForeignOperandTypes (Runtime.runtimeForeignCallSignature foreignCall)) ->
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

convertAlt :: ConvertEnv -> TypeVars -> Map Fc2.Name Runtime.RuntimeVar -> Fc2.Alt -> Either String Runtime.RuntimeAlt
convertAlt env typeVars locals alternative = do
  binders <- traverse (convertTermBinder (convertTypes env) typeVars) (Fc2.altBinders alternative)
  let locals' = Map.fromList (zip (map Fc2.binderName (Fc2.altBinders alternative)) binders) <> locals
  rhs <- convertExpr env typeVars locals' (Fc2.altRhs alternative)
  altCon <- convertAltCon (convertTypes env) typeVars (Fc2.altCon alternative)
  pure (Runtime.RuntimeAlt altCon binders rhs)

convertAltCon :: Fc2Type.TypeEnv -> TypeVars -> Fc2.AltCon -> Either String Runtime.RuntimeAltCon
convertAltCon types typeVars altCon =
  case altCon of
    Fc2.AltData name -> pure (Runtime.DataAlt (constructorOrigin name))
    Fc2.AltLit literal -> do
      converted <- convertLiteral types typeVars literal
      pure (Runtime.LitAlt converted (literalType converted))
    Fc2.AltDefault -> pure Runtime.DefaultAlt

convertTermBinder :: Fc2Type.TypeEnv -> TypeVars -> Fc2.Binder -> Either String Runtime.RuntimeVar
convertTermBinder types typeVars binder = do
  ty <- convertType types typeVars (Fc2.binderType binder)
  pure (localVar (Fc2.binderName binder) ty)

extendTypeBinders :: Fc2Type.TypeEnv -> TypeVars -> [Fc2.Binder] -> Either String (Fc2Type.TypeEnv, TypeVars)
extendTypeBinders types typeVars [] = pure (types, typeVars)
extendTypeBinders types typeVars (binder : binders) = do
  (_, types', typeVars') <- convertTypeBinder types typeVars binder
  extendTypeBinders types' typeVars' binders

convertTypeBinder :: Fc2Type.TypeEnv -> TypeVars -> Fc2.Binder -> Either String (Maybe Unique, Fc2Type.TypeEnv, TypeVars)
convertTypeBinder types typeVars binder = do
  unique <- nameUnique (Fc2.binderName binder)
  runtimeTypeInfo <- convertBinderRuntimeType types unique (Fc2.binderType binder)
  let runtimeRepBinder =
        case Fc2Type.reduceType types (Fc2.binderType binder) of
          Fc2.TyCon name
            | Fc2.nameText name == "RuntimeRep" -> Just unique
          _ -> Nothing
  pure
    ( runtimeRepBinder,
      Fc2Type.extendBinder types binder,
      Map.insert (Fc2.binderName binder) runtimeTypeInfo typeVars
    )

convertBinderRuntimeType :: Fc2Type.TypeEnv -> Unique -> Fc2.Type -> Either String RuntimeType
convertBinderRuntimeType types unique binderKind =
  case Fc2Type.reduceType types binderKind of
    Fc2.TyCon name
      | Fc2.nameText name == "RuntimeRep" -> pure (runtimeRepVariable unique)
    reduced -> convertValueLayout types reduced

convertType :: Fc2Type.TypeEnv -> TypeVars -> Fc2.Type -> Either String RuntimeType
convertType types typeVars sourceType =
  case Fc2Type.reduceType types sourceType of
    Fc2.TyVar name ->
      maybe (Left ("unbound System FC 2 type variable: " <> show name)) pure (Map.lookup name typeVars)
    Fc2.TyFun _ _ argument result ->
      RuntimeFunction <$> convertType types typeVars argument <*> convertType types typeVars result
    Fc2.TyForAll binder body -> do
      (runtimeRepBinder, types', typeVars') <- convertTypeBinder types typeVars binder
      RuntimeForAll runtimeRepBinder <$> convertType types' typeVars' body
    reduced ->
      case Fc2Type.typeOf types reduced of
        Nothing -> Left ("System FC 2 type has no runtime layout: " <> show reduced)
        Just kind ->
          case Fc2Type.reduceType types kind of
            Fc2.TyCon name
              | Fc2.nameText name == "RuntimeRep" -> runtimeRepArgument <$> convertRep types reduced
            reducedKind -> convertValueLayout types reducedKind

convertValueLayout :: Fc2Type.TypeEnv -> Fc2.Type -> Either String RuntimeType
convertValueLayout types sourceKind =
  case Fc2Type.reduceType types sourceKind of
    Fc2.TyApp (Fc2.TyCon name) (Fc2.TyVar representation)
      | Fc2.nameText name == "TYPE" -> RuntimeValue . VariableLayout <$> nameUnique representation
    Fc2.TyApp (Fc2.TyCon name) representation
      | Fc2.nameText name == "TYPE" -> runtimeType <$> convertRep types representation
    _ -> pure RuntimeErased

convertRep :: Fc2Type.TypeEnv -> Fc2.Type -> Either String RuntimeRep
convertRep types sourceRep =
  case Fc2Type.reduceType types sourceRep of
    Fc2.TyVar _ -> pure liftedRuntimeRep
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

convertLiteral :: Fc2Type.TypeEnv -> TypeVars -> Fc2.Literal -> Either String Runtime.Literal
convertLiteral types _ literal =
  case literal of
    Fc2.LitInt representation value -> Runtime.LitInt <$> convertRep types representation <*> pure value
    Fc2.LitChar representation value -> Runtime.LitChar <$> convertRep types representation <*> pure value
    Fc2.LitString value -> pure (Runtime.LitString value)
    Fc2.LitAddr representation value -> do
      rep <- convertRep types representation
      if rep == AddrRep
        then pure (Runtime.LitAddr value)
        else Left ("System FC 2 address literal has representation " <> show rep)

literalType :: Runtime.Literal -> RuntimeType
literalType = runtimeType . Runtime.literalRuntimeRep

convertForeignSignature :: Fc2.CCallSpec -> Runtime.RuntimeForeignSignature
convertForeignSignature specification =
  Runtime.RuntimeForeignSignature
    { Runtime.runtimeForeignArgumentTypes = map convertForeignType (Fc2.ccallArgumentTypes specification),
      Runtime.runtimeForeignResultType = convertForeignType (Fc2.ccallResultType specification),
      Runtime.runtimeForeignEffect = convertForeignEffect (Fc2.ccallEffect specification)
    }

convertForeignEffect :: Fc2.ForeignEffect -> Runtime.RuntimeForeignEffect
convertForeignEffect effect =
  case effect of
    Fc2.ForeignPure -> Runtime.RuntimeForeignPure
    Fc2.ForeignRealWorld -> Runtime.RuntimeForeignRealWorld

convertForeignType :: Fc2.CAbiType -> Runtime.RuntimeForeignType
convertForeignType foreignType =
  case foreignType of
    Fc2.CAbiInt -> Runtime.RuntimeForeignInt
    Fc2.CAbiInt32 -> Runtime.RuntimeForeignInt32
    Fc2.CAbiWord64 -> Runtime.RuntimeForeignWord64
    Fc2.CAbiAddr -> Runtime.RuntimeForeignAddr

lookupTerm :: ConvertEnv -> Map Fc2.Name Runtime.RuntimeVar -> Fc2.Name -> Either String Runtime.RuntimeVar
lookupTerm env locals name =
  maybe (Left ("unbound System FC 2 value: " <> show name)) pure (Map.lookup name locals <|> Map.lookup name (convertGlobals env))

lookupGlobal :: ConvertEnv -> Fc2.Name -> Either String Runtime.RuntimeVar
lookupGlobal env name =
  maybe (Left ("missing System FC 2 global: " <> show name)) pure (Map.lookup name (convertGlobals env))

lookupForeignCall :: ConvertEnv -> Fc2.Name -> Either String Runtime.RuntimeForeignCall
lookupForeignCall env name =
  maybe (Left ("missing System FC 2 foreign call: " <> show name)) pure (Map.lookup name (convertForeignCalls env))

topVar :: Fc2.Name -> RuntimeType -> Runtime.RuntimeVar
topVar name ty =
  case Fc2.nameOrigin name of
    Fc2.OriginTop package moduleName ->
      Runtime.runtimeExternalVar (Runtime.RuntimeTopLevelOrigin (packageIdText package) moduleName (Fc2.nameText name)) ty
    Fc2.OriginLocal unique -> Runtime.RuntimeVar (Fc2.nameText name) unique ty

localVar :: Fc2.Name -> RuntimeType -> Runtime.RuntimeVar
localVar name ty =
  case Fc2.nameOrigin name of
    Fc2.OriginLocal unique -> Runtime.RuntimeVar (Fc2.nameText name) unique ty
    Fc2.OriginTop {} -> topVar name ty

constructorOrigin :: Fc2.Name -> Runtime.RuntimeConstructorId
constructorOrigin name =
  case Fc2.nameOrigin name of
    Fc2.OriginTop package moduleName -> Runtime.RuntimeConstructorId package moduleName (Fc2.nameText name)
    Fc2.OriginLocal {} -> error "System FC 2 data constructor has a local origin"

nameUnique :: Fc2.Name -> Either String Unique
nameUnique name =
  case Fc2.nameOrigin name of
    Fc2.OriginLocal unique -> pure unique
    Fc2.OriginTop {} -> Left ("System FC 2 binder has a top-level origin: " <> show name)
