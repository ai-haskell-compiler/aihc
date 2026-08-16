{-# LANGUAGE OverloadedStrings #-}

-- | Human-readable, canonical System FC syntax.
module Aihc.Fc.Pretty (renderProgram, renderExpr, renderType, renderTopBind) where

import Aihc.Fc.Syntax
import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Evidence (Coercion (..), EvVar (..))
import Aihc.Tc.Types
import Data.ByteString qualified as BS
import Data.Char (chr)
import Data.List (intercalate)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T

type Scope = (T.Text, T.Text)

type ScopeTable = Map.Map Scope Int

renderProgram :: FcProgram -> String
renderProgram program = intercalate "\n\n" (renderScopes scopes : renderModuleDeclaration scopes (fcProgramModule program) : map (renderTopBindWith scopes) (fcTopBinds program))
  where
    scopes = scopeTable program

scopeTable :: FcProgram -> ScopeTable
scopeTable = Map.fromAscList . flip zip [1 ..] . Set.toAscList . programScopes

programScopes :: FcProgram -> Set.Set Scope
programScopes program = moduleScopes (fcProgramModule program) <> foldMap topBindScopes (fcTopBinds program)

moduleScopes :: FcModuleId -> Set.Set Scope
moduleScopes moduleId = Set.singleton (fcModulePackageText moduleId, fcModuleName moduleId)

topBindScopes :: FcTopBind -> Set.Set Scope
topBindScopes topBind = case topBind of
  FcExternal origin ty -> originScopes origin <> typeScopes ty
  FcData declaration -> originScopes (fcDataOrigin declaration) <> foldMap dataConScopes (fcDataConstructors declaration)
  FcAxiom declaration -> typeScopes (fcAxiomLeft declaration) <> typeScopes (fcAxiomRight declaration)
  FcNewtype declaration -> originScopes (fcNewtypeOrigin declaration) <> constructorScopes (fcNewtypeConstructorOrigin declaration) <> typeScopes (fcNewtypeRepresentation declaration) <> typeScopes (fcNewtypeResult declaration)
  FcPrimitive var _ -> varScopes var
  FcForeignImport foreignCall -> foreignCallScopes foreignCall
  FcTopBind bind -> bindScopes bind

dataConScopes :: FcDataConDecl -> Set.Set Scope
dataConScopes declaration = constructorScopes (fcDataConOrigin declaration) <> foldMap typeScopes (fcDataConFields declaration)

foreignCallScopes :: FcForeignCall -> Set.Set Scope
foreignCallScopes = typeScopes . fcForeignCallType . fcForeignCallSignature

bindScopes :: FcBind -> Set.Set Scope
bindScopes bind = case bind of
  FcNonRec var rhs -> varScopes var <> exprScopes rhs
  FcRec bindings -> foldMap (\(var, rhs) -> varScopes var <> exprScopes rhs) bindings

exprScopes :: FcExpr -> Set.Set Scope
exprScopes expression = case expression of
  FcVar var -> varScopes var
  FcLit _ ty -> typeScopes ty
  FcApp function argument -> exprScopes function <> exprScopes argument
  FcTyApp function argument -> exprScopes function <> typeScopes argument
  FcLam var body -> varScopes var <> exprScopes body
  FcTyLam _ body -> exprScopes body
  FcLet bind body -> bindScopes bind <> exprScopes body
  FcCase scrutinee binder alternatives -> exprScopes scrutinee <> varScopes binder <> foldMap altScopes alternatives
  FcCast body coercion -> exprScopes body <> coercionScopes coercion
  FcCallForeign foreignCall arguments -> foreignCallScopes foreignCall <> foldMap exprScopes arguments

altScopes :: FcAlt -> Set.Set Scope
altScopes alternative = altConScopes (altCon alternative) <> foldMap varScopes (altBinders alternative) <> exprScopes (altRhs alternative)

altConScopes :: FcAltCon -> Set.Set Scope
altConScopes alternative = case alternative of
  DataAlt constructor -> constructorScopes constructor
  LitAlt _ ty -> typeScopes ty
  DefaultAlt -> mempty

varScopes :: Var -> Set.Set Scope
varScopes var = typeScopes (varType var) <> maybe mempty originScopes (varResolvedName var)

originScopes :: FcSymbolOrigin -> Set.Set Scope
originScopes origin = case origin of
  FcTopLevelOrigin packageName moduleName _ -> Set.singleton (packageName, moduleName)
  FcBuiltinOrigin {} -> mempty

constructorScopes :: FcConstructorId -> Set.Set Scope
constructorScopes constructor = Set.singleton (packageIdText (fcConstructorPackage constructor), fcConstructorModule constructor)

typeScopes :: TcType -> Set.Set Scope
typeScopes ty = case ty of
  TcTyVar {} -> mempty
  TcMetaTv {} -> mempty
  TcTyCon tyCon arguments -> tyConScopes tyCon <> foldMap typeScopes arguments
  TcFunTy argument result -> typeScopes argument <> typeScopes result
  TcForAllTy _ body -> typeScopes body
  TcQualTy predicates body -> foldMap predScopes predicates <> typeScopes body
  TcAppTy function argument -> typeScopes function <> typeScopes argument
  TcBuiltinTyCon _ _ arguments -> foldMap typeScopes arguments

tyConScopes :: TyCon -> Set.Set Scope
tyConScopes tyCon = Set.singleton (packageIdText (tyConPackageId tyCon), tyConModuleName tyCon)

predScopes :: Pred -> Set.Set Scope
predScopes predicate = case predicate of
  ClassPred tyCon arguments -> tyConScopes tyCon <> foldMap typeScopes arguments
  EqPred left right -> typeScopes left <> typeScopes right

coercionScopes :: Coercion -> Set.Set Scope
coercionScopes coercion = case coercion of
  CoVar {} -> mempty
  Refl ty -> typeScopes ty
  Sym inner -> coercionScopes inner
  Trans left right -> coercionScopes left <> coercionScopes right
  TyConAppCo tyCon arguments -> tyConScopes tyCon <> foldMap coercionScopes arguments
  AxiomInstCo _ arguments -> foldMap typeScopes arguments

renderScopes :: ScopeTable -> String
renderScopes = intercalate "\n" . map renderScope . Map.toAscList
  where
    renderScope ((packageName, moduleName), scopeId) = "scope " <> show scopeId <> " = " <> show (T.unpack packageName) <> " " <> T.unpack moduleName

renderModuleDeclaration :: ScopeTable -> FcModuleId -> String
renderModuleDeclaration scopes moduleId = "module " <> scopeReference scopes (fcModulePackageText moduleId) (fcModuleName moduleId) "" <> T.unpack (fcModuleName moduleId) <> " where"

scopeReference :: ScopeTable -> T.Text -> T.Text -> T.Text -> String
scopeReference scopes packageName moduleName name = case Map.lookup (packageName, moduleName) scopes of
  Just scopeId -> show scopeId <> "." <> T.unpack name
  Nothing -> show (T.unpack packageName) <> " " <> T.unpack moduleName <> "." <> T.unpack name

renderTopBind :: FcTopBind -> String
renderTopBind = renderTopBindWith mempty

renderTopBindWith :: ScopeTable -> FcTopBind -> String
renderTopBindWith scopes topBind = case topBind of
  FcExternal origin ty -> "external " <> renderOrigin scopes origin <> " : " <> renderTypeWith scopes ty
  FcData declaration -> "data " <> renderOrigin scopes (fcDataOrigin declaration) <> concatMap ((" " <>) . renderTyVarBinder scopes) (fcDataTyVars declaration) <> " : " <> renderKind scopes (fcDataResultKind declaration) <> renderConstructors scopes (fcDataConstructors declaration)
  FcAxiom declaration -> "axiom " <> T.unpack (fcAxiomName declaration) <> concatMap ((" " <>) . renderTyVarBinder scopes) (fcAxiomTyVars declaration) <> " : " <> renderTypeWith scopes (fcAxiomLeft declaration) <> renderAxiomRole (fcAxiomRole declaration) <> renderTypeWith scopes (fcAxiomRight declaration)
  FcNewtype declaration -> "newtype " <> renderOrigin scopes (fcNewtypeOrigin declaration) <> concatMap ((" " <>) . renderTyVarBinder scopes) (fcNewtypeTyVars declaration) <> " : " <> renderTypeWith scopes (fcNewtypeResult declaration) <> " = " <> renderConstructorId scopes (fcNewtypeConstructorOrigin declaration) <> " " <> renderTypeAtom scopes (fcNewtypeRepresentation declaration)
  FcPrimitive var arity -> "foreign prim " <> renderBinder scopes var <> "/" <> show arity <> " : " <> renderTypeWith scopes (varType var)
  FcForeignImport foreignCall -> renderForeignImport scopes foreignCall
  FcTopBind bind -> renderBind scopes 0 bind

renderConstructors :: ScopeTable -> [FcDataConDecl] -> String
renderConstructors _ [] = ""
renderConstructors scopes constructors = "\n" <> intercalate "\n" (zipWith renderConstructor (" = " : repeat " | ") constructors)
  where
    renderConstructor prefix declaration = prefix <> renderConstructorId scopes (fcDataConOrigin declaration) <> concatMap (\field -> " (" <> renderTypeWith scopes field <> ")") (fcDataConFields declaration)

renderAxiomRole :: FcAxiomRole -> String
renderAxiomRole FcNominal = " ~N "
renderAxiomRole FcRepresentational = " ~R "

renderForeignImport :: ScopeTable -> FcForeignCall -> String
renderForeignImport scopes foreignCall = "foreign ccall " <> renderForeignCallHeader foreignCall <> " : " <> renderTypeWith scopes (fcForeignCallType signature)
  where
    signature = fcForeignCallSignature foreignCall

renderForeignType :: FcForeignType -> String
renderForeignType foreignType = case foreignType of
  FcForeignInt -> "Int"
  FcForeignInt32 -> "Int32"
  FcForeignWord64 -> "Word64"
  FcForeignAddr -> "Addr"

renderForeignEffect :: FcForeignEffect -> String
renderForeignEffect FcForeignPure = "pure"
renderForeignEffect FcForeignRealWorld = "real-world"

renderBind :: ScopeTable -> Int -> FcBind -> String
renderBind scopes indentation bind = case bind of
  FcNonRec var rhs -> renderOne scopes indentation var rhs
  FcRec [] -> indent indentation <> "rec {}"
  FcRec bindings -> indent indentation <> "rec {\n" <> intercalate ";\n" [indent (indentation + 2) <> renderBinder scopes var <> " : " <> renderTypeWith scopes (varType var) | (var, _) <- bindings] <> ";\n" <> intercalate ";\n" [indent (indentation + 2) <> renderBinder scopes var <> " =\n" <> renderExprIndented scopes (indentation + 4) rhs | (var, rhs) <- bindings] <> "\n" <> indent indentation <> "}"

renderOne :: ScopeTable -> Int -> Var -> FcExpr -> String
renderOne scopes indentation var rhs = indent indentation <> renderBinder scopes var <> " : " <> renderTypeWith scopes (varType var) <> " =\n" <> renderExprIndented scopes (indentation + 2) rhs

renderExpr :: FcExpr -> String
renderExpr = renderExprWith mempty 0 False

renderExprIndented :: ScopeTable -> Int -> FcExpr -> String
renderExprIndented scopes indentation expression = indent indentation <> renderExprWith scopes indentation False expression

renderExprWith :: ScopeTable -> Int -> Bool -> FcExpr -> String
renderExprWith scopes indentation parenthesize expression = case expression of
  FcVar var -> renderOccurrence scopes var
  FcLit literal ty -> "(" <> renderLiteral literal <> " : " <> renderTypeWith scopes ty <> ")"
  FcApp function argument -> paren parenthesize (renderExprWith scopes indentation True function <> " " <> renderExprWith scopes indentation True argument)
  FcTyApp function argument -> paren parenthesize (renderExprWith scopes indentation True function <> " @" <> renderTypeAtom scopes argument)
  FcLam var body -> paren parenthesize ("λ(" <> renderBinder scopes var <> " : " <> renderTypeWith scopes (varType var) <> ").\n" <> renderExprIndented scopes (indentation + 2) body)
  FcTyLam tyVar body -> paren parenthesize ("Λ" <> renderTyVarBinder scopes tyVar <> ".\n" <> renderExprIndented scopes (indentation + 2) body)
  FcLet bind body -> paren parenthesize ("let {\n" <> renderBind scopes (indentation + 2) bind <> "\n" <> indent indentation <> "} in\n" <> renderExprIndented scopes (indentation + 2) body)
  FcCase scrutinee binder alternatives -> paren parenthesize ("case " <> renderExprWith scopes indentation False scrutinee <> " as (" <> renderBinder scopes binder <> " : " <> renderTypeWith scopes (varType binder) <> ") of " <> renderAlternatives alternatives)
  FcCast body coercion -> paren parenthesize (renderExprWith scopes indentation True body <> " ▷ " <> renderCoercion scopes coercion)
  FcCallForeign foreignCall arguments -> paren parenthesize ("foreign-call " <> renderForeignCallHeader foreignCall <> concatMap ((" " <>) . renderExprWith scopes indentation True) arguments)
  where
    renderAlternatives [] = "{}"
    renderAlternatives alternatives = "{\n" <> intercalate ";\n" (map (renderAlt scopes (indentation + 2)) alternatives) <> "\n" <> indent indentation <> "}"

renderAlt :: ScopeTable -> Int -> FcAlt -> String
renderAlt scopes indentation alternative = indent indentation <> renderAltCon scopes (altCon alternative) <> concatMap (\binder -> " (" <> renderBinder scopes binder <> " : " <> renderTypeWith scopes (varType binder) <> ")") (altBinders alternative) <> " →\n" <> renderExprIndented scopes (indentation + 2) (altRhs alternative)

renderAltCon :: ScopeTable -> FcAltCon -> String
renderAltCon scopes alternative = case alternative of
  DataAlt constructor -> renderConstructorId scopes constructor
  LitAlt literal ty -> "(" <> renderLiteral literal <> " : " <> renderTypeWith scopes ty <> ")"
  DefaultAlt -> "_"

renderConstructorId :: ScopeTable -> FcConstructorId -> String
renderConstructorId scopes constructor = scopeReference scopes (packageIdText (fcConstructorPackage constructor)) (fcConstructorModule constructor) (fcConstructorName constructor)

renderBinder :: ScopeTable -> Var -> String
renderBinder scopes var = T.unpack (varName var) <> "{unique " <> renderUnique (varUnique var) <> "}" <> maybe "" (renderVarOrigin scopes) (varResolvedName var)

renderOccurrence :: ScopeTable -> Var -> String
renderOccurrence scopes var = "(" <> renderBinder scopes var <> " : " <> renderTypeWith scopes (varType var) <> ")"

renderVarOrigin :: ScopeTable -> FcSymbolOrigin -> String
renderVarOrigin scopes origin = "{origin " <> renderOrigin scopes origin <> "}"

renderOrigin :: ScopeTable -> FcSymbolOrigin -> String
renderOrigin scopes origin = case origin of
  FcTopLevelOrigin packageName moduleName symbolName -> scopeReference scopes packageName moduleName symbolName
  FcBuiltinOrigin symbolName -> "builtin." <> T.unpack symbolName

renderForeignCallHeader :: FcForeignCall -> String
renderForeignCallHeader foreignCall = show (T.unpack (fcForeignCallSymbol foreignCall)) <> " " <> T.unpack (fcForeignCallName foreignCall) <> " [" <> intercalate ", " (map renderForeignType (fcForeignArgumentTypes signature)) <> " → " <> renderForeignType (fcForeignResultType signature) <> "; " <> renderForeignEffect (fcForeignEffect signature) <> "]"
  where
    signature = fcForeignCallSignature foreignCall

renderLiteral :: Literal -> String
renderLiteral literal = case literal of
  LitInt runtimeRep value -> show value <> "#" <> renderRuntimeRep runtimeRep
  LitChar runtimeRep value -> show value <> "#" <> renderRuntimeRep runtimeRep
  LitString value -> show (T.unpack value)
  LitAddr value -> show (map (chr . fromIntegral) (BS.unpack value)) <> "#AddrRep"

renderType :: TcType -> String
renderType = renderTypeWith mempty

renderTypeWith :: ScopeTable -> TcType -> String
renderTypeWith scopes ty = case ty of
  TcTyVar tyVar -> renderTyVarBinder scopes tyVar
  TcMetaTv (Unique unique) -> "?" <> show unique
  TcTyCon tyCon [] -> renderTyConHead scopes tyCon
  TcTyCon tyCon arguments -> renderTyConHead scopes tyCon <> "[" <> intercalate ", " (map (renderTypeWith scopes) arguments) <> "]"
  TcFunTy argument result -> renderTypeAtom scopes argument <> " → " <> renderTypeWith scopes result
  TcForAllTy tyVar body -> "∀ " <> renderTyVarBinder scopes tyVar <> ". " <> renderTypeWith scopes body
  TcQualTy predicates body -> "(" <> intercalate ", " (map (renderPred scopes) predicates) <> ") ⇒ " <> renderTypeWith scopes body
  TcAppTy function argument -> renderTypeAtom scopes function <> " · " <> renderTypeAtom scopes argument
  TcBuiltinTyCon name arity arguments -> "builtin " <> T.unpack name <> "/" <> show arity <> "[" <> intercalate ", " (map (renderTypeWith scopes) arguments) <> "]"

renderTyConHead :: ScopeTable -> TyCon -> String
renderTyConHead scopes tyCon = "tycon " <> scopeName <> "/" <> show (tyConArity tyCon) <> " { :: " <> renderKindScheme scopes (tyConKindScheme tyCon) <> " }"
  where
    scopeName = case Map.lookup (packageIdText (tyConPackageId tyCon), tyConModuleName tyCon) scopes of
      Just scopeId -> show scopeId <> "." <> T.unpack (tyConName tyCon)
      Nothing -> show (T.unpack (packageIdText (tyConPackageId tyCon))) <> " " <> show (T.unpack (tyConModuleName tyCon)) <> " " <> T.unpack (tyConName tyCon)

renderKindScheme :: ScopeTable -> TypeScheme -> String
renderKindScheme scopes scheme@(ForAll tyVars _ _) = prefix <> renderKind scopes (kindFromTypeScheme scheme)
  where
    prefix = case tyVars of [] -> ""; _ -> "∀ " <> unwords (map (renderTyVarBinder scopes) tyVars) <> ". "

renderTypeAtom :: ScopeTable -> TcType -> String
renderTypeAtom scopes ty = case ty of
  TcTyVar {} -> renderTypeWith scopes ty
  TcMetaTv {} -> renderTypeWith scopes ty
  TcTyCon {} -> renderTypeWith scopes ty
  TcBuiltinTyCon {} -> renderTypeWith scopes ty
  _ -> "(" <> renderTypeWith scopes ty <> ")"

renderTyVarBinder :: ScopeTable -> TyVarId -> String
renderTyVarBinder scopes tyVar = "(" <> T.unpack (tvName tyVar) <> "{unique " <> renderUnique (tvUnique tyVar) <> "} : " <> renderKind scopes (tvKind tyVar) <> ")"

renderPred :: ScopeTable -> Pred -> String
renderPred scopes predicate = case predicate of
  ClassPred classTyCon arguments -> renderTypeAtom scopes (TcTyCon classTyCon arguments)
  EqPred left right -> renderTypeAtom scopes left <> " ~ " <> renderTypeAtom scopes right

renderKind :: ScopeTable -> Kind -> String
renderKind scopes kind = case kind of
  KTYPE (BoxedRep Lifted) -> "Type"
  KTYPE (BoxedRep Unlifted) -> "TYPE UnliftedRep"
  KTYPE runtimeRep -> "TYPE " <> renderRuntimeRep runtimeRep
  KConstraint -> "Constraint"
  KRuntimeRep -> "RuntimeRep"
  KLevity -> "Levity"
  KVecCount -> "VecCount"
  KVecElem -> "VecElem"
  KFun argument result -> renderKindAtom scopes argument <> " → " <> renderKind scopes result
  KMeta (Unique unique) -> "?k" <> show unique

renderKindAtom :: ScopeTable -> Kind -> String
renderKindAtom scopes kind = case kind of
  KFun {} -> "(" <> renderKind scopes kind <> ")"
  _ -> renderKind scopes kind

renderRuntimeRep :: RuntimeRep -> String
renderRuntimeRep runtimeRep = case runtimeRep of
  VecRep count element -> "VecRep " <> show count <> " " <> show element
  TupleRep fields -> "TupleRep [" <> intercalate ", " (map renderRuntimeRep fields) <> "]"
  SumRep fields -> "SumRep [" <> intercalate ", " (map renderRuntimeRep fields) <> "]"
  BoxedRep levity -> "BoxedRep " <> show levity
  IntRep -> "IntRep"
  Int8Rep -> "Int8Rep"
  Int16Rep -> "Int16Rep"
  Int32Rep -> "Int32Rep"
  Int64Rep -> "Int64Rep"
  WordRep -> "WordRep"
  Word8Rep -> "Word8Rep"
  Word16Rep -> "Word16Rep"
  Word32Rep -> "Word32Rep"
  Word64Rep -> "Word64Rep"
  AddrRep -> "AddrRep"
  FloatRep -> "FloatRep"
  DoubleRep -> "DoubleRep"
  RuntimeRepVar (Unique unique) -> "RuntimeRepVar " <> show unique
  RuntimeRepMeta (Unique unique) -> "RuntimeRepMeta " <> show unique

renderCoercion :: ScopeTable -> Coercion -> String
renderCoercion scopes coercion = case coercion of
  CoVar (EvVar (Unique unique)) -> "co#" <> show unique
  Refl ty -> "refl (" <> renderTypeWith scopes ty <> ")"
  Sym inner -> "sym (" <> renderCoercion scopes inner <> ")"
  Trans left right -> "trans (" <> renderCoercion scopes left <> ") (" <> renderCoercion scopes right <> ")"
  TyConAppCo tyCon arguments -> "tycon-co " <> renderTyConHead scopes tyCon <> concatMap (\argument -> " (" <> renderCoercion scopes argument <> ")") arguments
  AxiomInstCo name arguments -> "axiom-co " <> T.unpack name <> concatMap (\argument -> " @" <> renderTypeAtom scopes argument) arguments

paren :: Bool -> String -> String
paren False value = value
paren True value = "(" <> value <> ")"

indent :: Int -> String
indent count = replicate count ' '

renderUnique :: Unique -> String
renderUnique (Unique unique) = show unique
