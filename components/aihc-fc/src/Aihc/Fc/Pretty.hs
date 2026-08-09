{-# LANGUAGE OverloadedStrings #-}

-- | Human-readable, canonical System FC syntax.
module Aihc.Fc.Pretty
  ( renderProgram,
    renderExpr,
    renderType,
    renderTopBind,
    declareExternalSymbols,
  )
where

import Aihc.Fc.Subst (freeRigidTyVarsOf)
import Aihc.Fc.Syntax
import Aihc.Tc.Evidence (Coercion (..), EvVar (..))
import Aihc.Tc.Types
import Data.ByteString qualified as BS
import Data.Char (chr)
import Data.List (find, intercalate)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T

renderProgram :: FcProgram -> String
renderProgram program =
  intercalate "\n\n" (renderModuleDeclaration (fcProgramModule program) : map (renderTopBindWith symbols) canonicalBinds)
  where
    canonicalBinds = fcTopBinds (declareExternalSymbols program)
    symbols = renderSymbols (fcProgramModule program) canonicalBinds

renderModuleDeclaration :: FcModuleId -> String
renderModuleDeclaration moduleId =
  "module " <> renderModuleOrigin (fcModulePackage moduleId) (fcModuleName moduleId) <> " where"

-- | Materialize the one-per-origin external signature table represented by
-- the canonical syntax. Desugaring calls this as well as rendering so the
-- declarations are part of the System FC tree, not merely presentation.
declareExternalSymbols :: FcProgram -> FcProgram
declareExternalSymbols program =
  FcProgram (fcProgramModule program) (map (normalizeExternalTopBind externalVars) canonicalBinds)
  where
    canonicalBinds = canonicalProgramBinds program
    externalVars = Map.fromList [(origin, fcExternalVar origin ty) | FcExternal origin ty <- canonicalBinds]

normalizeExternalTopBind :: Map.Map FcSymbolOrigin Var -> FcTopBind -> FcTopBind
normalizeExternalTopBind externalVars topBind =
  case topBind of
    FcTopBind bind -> FcTopBind (normalizeExternalBind externalVars bind)
    _ -> topBind

normalizeExternalBind :: Map.Map FcSymbolOrigin Var -> FcBind -> FcBind
normalizeExternalBind externalVars bind =
  case bind of
    FcNonRec var rhs -> FcNonRec var (normalizeExternalExpr externalVars rhs)
    FcRec bindings -> FcRec [(var, normalizeExternalExpr externalVars rhs) | (var, rhs) <- bindings]

normalizeExternalExpr :: Map.Map FcSymbolOrigin Var -> FcExpr -> FcExpr
normalizeExternalExpr externalVars expression =
  case expression of
    FcVar var ->
      FcVar (fromMaybe var (varResolvedName var >>= (`Map.lookup` externalVars)))
    FcLit {} -> expression
    FcApp function argument -> FcApp (recur function) (recur argument)
    FcTyApp function ty -> FcTyApp (recur function) ty
    FcLam var body -> FcLam var (recur body)
    FcTyLam tyVar body -> FcTyLam tyVar (recur body)
    FcLet bind body -> FcLet (normalizeExternalBind externalVars bind) (recur body)
    FcCase scrutinee binder alternatives -> FcCase (recur scrutinee) binder (map normalizeAlternative alternatives)
    FcCast body coercion -> FcCast (recur body) coercion
    FcCallForeign foreignCall arguments -> FcCallForeign foreignCall (map recur arguments)
  where
    recur = normalizeExternalExpr externalVars
    normalizeAlternative alternative = alternative {altRhs = recur (altRhs alternative)}

renderTopBind :: FcTopBind -> String
renderTopBind = renderTopBindWith emptyRenderSymbols

renderTopBindWith :: RenderSymbols -> FcTopBind -> String
renderTopBindWith symbols topBind =
  case topBind of
    FcExternal origin ty ->
      "external " <> renderOrigin origin <> " : " <> renderType ty
    FcData declaration ->
      "data "
        <> renderDeclarationName symbols (fcDataOrigin declaration) (fcDataName declaration)
        <> concatMap (" " <>) (renderTyVarBinders [] (fcDataTyVars declaration))
        <> renderDataResultKind declaration
        <> renderConstructors symbols (fcDataTyVars declaration) (fcDataConstructors declaration)
    FcAxiom declaration ->
      "axiom "
        <> T.unpack (fcAxiomName declaration)
        <> concatMap (" " <>) (renderTyVarBinders [] (fcAxiomTyVars declaration))
        <> " : "
        <> renderTypeWith (fcAxiomTyVars declaration) (fcAxiomLeft declaration)
        <> renderAxiomRole (fcAxiomRole declaration)
        <> renderTypeWith (fcAxiomTyVars declaration) (fcAxiomRight declaration)
    FcNewtype declaration ->
      "newtype "
        <> renderDeclarationName symbols (fcNewtypeOrigin declaration) (fcNewtypeName declaration)
        <> concatMap (" " <>) (renderTyVarBinders [] (fcNewtypeTyVars declaration))
        <> " : "
        <> renderTypeWith (fcNewtypeTyVars declaration) (fcNewtypeResult declaration)
        <> " = "
        <> renderDeclarationName symbols (fcNewtypeConstructorOrigin declaration) (fcNewtypeConstructor declaration)
        <> " "
        <> renderTypeAtomWith (fcNewtypeTyVars declaration) (fcNewtypeRepresentation declaration)
    FcPrimitive var arity ->
      "foreign prim " <> renderBinder var <> "/" <> show arity <> " : " <> renderType (varType var)
    FcForeignImport foreignCall -> renderForeignImport foreignCall
    FcTopBind bind -> renderBind symbols 0 [] [] bind

renderDataResultKind :: FcDataDecl -> String
renderDataResultKind declaration =
  case fcDataResultKind declaration of
    KType -> ""
    resultKind -> " : " <> renderKindWith (fcDataTyVars declaration) resultKind

data RenderSymbols = RenderSymbols
  { renderExternalOrigins :: !(Set FcSymbolOrigin),
    renderLocalOrigin :: !(Maybe (T.Text, T.Text)),
    renderLocalNames :: !(Set T.Text)
  }

emptyRenderSymbols :: RenderSymbols
emptyRenderSymbols = RenderSymbols Set.empty Nothing Set.empty

renderSymbols :: FcModuleId -> [FcTopBind] -> RenderSymbols
renderSymbols moduleId topBinds =
  RenderSymbols
    { renderExternalOrigins = Set.fromList [origin | FcExternal origin _ <- topBinds],
      renderLocalOrigin = Just (fcModulePackage moduleId, fcModuleName moduleId),
      renderLocalNames = Set.fromList (concatMap topBindDefinedNames topBinds)
    }

renderModuleOrigin :: T.Text -> T.Text -> String
renderModuleOrigin packageName moduleName =
  (if packageName == "" then "" else show (T.unpack packageName) <> " ") <> T.unpack moduleName

canonicalProgramBinds :: FcProgram -> [FcTopBind]
canonicalProgramBinds (FcProgram moduleId topBinds) =
  [FcExternal origin ty | (origin, ty) <- Map.toAscList externalTypes, not (originIsLocal origin)]
    <> definitions
  where
    definitions = [topBind | topBind <- topBinds, not (isHeader topBind)]
    moduleOrigin = Just (fcModulePackage moduleId, fcModuleName moduleId)
    declaredTypes = Map.fromList [(origin, ty) | FcExternal origin ty <- topBinds]
    referencedTypes = Map.fromList [(origin, varType var) | var <- concatMap topBindOccurrences definitions, Just origin <- [varResolvedName var]]
    externalTypes = Map.union declaredTypes referencedTypes
    definedNames = Set.fromList (concatMap topBindDefinedNames definitions)
    localOrigins =
      Set.fromList [origin | topBind <- definitions, var <- topBindDefinedVars topBind, Just origin <- [varResolvedName var]]
        <> Set.fromList (concatMap topBindDefinedOrigins definitions)
        <> Set.fromList
          [ origin
          | topBind <- definitions,
            var <- topBindOccurrences topBind,
            varName var `Set.member` definedNames,
            Just origin <- [varResolvedName var]
          ]
    originIsLocal origin@(FcTopLevelOrigin packageName moduleName _) = origin `Set.member` localOrigins || Just (packageName, moduleName) == moduleOrigin
    originIsLocal origin@FcBuiltinOrigin {} = origin `Set.member` localOrigins
    isHeader FcExternal {} = True
    isHeader _ = False

topBindDefinedNames :: FcTopBind -> [T.Text]
topBindDefinedNames topBind =
  case topBind of
    FcExternal {} -> []
    FcData declaration -> map fcDataConName (fcDataConstructors declaration)
    FcAxiom {} -> []
    FcNewtype declaration -> [fcNewtypeConstructor declaration]
    FcPrimitive var _ -> [varName var]
    FcForeignImport {} -> []
    FcTopBind bind -> map varName (bindersOf bind)

topBindDefinedOrigins :: FcTopBind -> [FcSymbolOrigin]
topBindDefinedOrigins topBind =
  case topBind of
    FcData declaration -> fcDataOrigin declaration : map fcDataConOrigin (fcDataConstructors declaration)
    FcNewtype declaration -> [fcNewtypeOrigin declaration, fcNewtypeConstructorOrigin declaration]
    _ -> []

topBindDefinedVars :: FcTopBind -> [Var]
topBindDefinedVars topBind =
  case topBind of
    FcPrimitive var _ -> [var]
    FcTopBind bind -> bindersOf bind
    _ -> []

topBindOccurrences :: FcTopBind -> [Var]
topBindOccurrences topBind =
  case topBind of
    FcTopBind bind -> bindOccurrences bind
    _ -> []

bindOccurrences :: FcBind -> [Var]
bindOccurrences bind =
  case bind of
    FcNonRec _ rhs -> expressionOccurrences rhs
    FcRec bindings -> concatMap (expressionOccurrences . snd) bindings

bindersOf :: FcBind -> [Var]
bindersOf bind =
  case bind of
    FcNonRec var _ -> [var]
    FcRec bindings -> map fst bindings

expressionOccurrences :: FcExpr -> [Var]
expressionOccurrences expression =
  case expression of
    FcVar var -> [var]
    FcLit {} -> []
    FcApp function argument -> expressionOccurrences function <> expressionOccurrences argument
    FcTyApp function _ -> expressionOccurrences function
    FcLam _ body -> expressionOccurrences body
    FcTyLam _ body -> expressionOccurrences body
    FcLet bind body -> bindOccurrences bind <> expressionOccurrences body
    FcCase scrutinee _ alternatives -> expressionOccurrences scrutinee <> concatMap (expressionOccurrences . altRhs) alternatives
    FcCast body _ -> expressionOccurrences body
    FcCallForeign _ arguments -> concatMap expressionOccurrences arguments

isLocalOrigin :: RenderSymbols -> FcSymbolOrigin -> Bool
isLocalOrigin symbols origin =
  case (renderLocalOrigin symbols, origin) of
    (Just (localPackage, localModule), FcTopLevelOrigin packageName moduleName _) ->
      localPackage == packageName && localModule == moduleName
    _ -> False

renderConstructors :: RenderSymbols -> [TyVarId] -> [FcDataConDecl] -> String
renderConstructors _ _ [] = ""
renderConstructors symbols tyVars constructors =
  "\n" <> intercalate "\n" (zipWith renderConstructor (" = " : repeat " | ") constructors)
  where
    renderConstructor prefix declaration =
      prefix
        <> renderExistentials fields
        <> renderDeclarationName symbols (fcDataConOrigin declaration) (fcDataConName declaration)
        <> concatMap (\field -> " (" <> renderTypeWith (tyVars <> freeRigidTyVarsOf fields) field <> ")") fields
      where
        fields = fcDataConFields declaration
    renderExistentials fields =
      case filter (`notElem` tyVars) (freeRigidTyVarsOf fields) of
        [] -> ""
        existentialTyVars -> "∀ " <> unwords (renderTyVarBinders tyVars existentialTyVars) <> ". "

renderDeclarationName :: RenderSymbols -> FcSymbolOrigin -> T.Text -> String
renderDeclarationName symbols origin declarationName
  | isLocalOrigin symbols origin = T.unpack declarationName
  | otherwise = renderOrigin origin

renderAxiomRole :: FcAxiomRole -> String
renderAxiomRole role =
  case role of
    FcNominal -> " ~N "
    FcRepresentational -> " ~R "

renderForeignImport :: FcForeignCall -> String
renderForeignImport foreignCall =
  "foreign ccall "
    <> show (T.unpack (fcForeignCallSymbol foreignCall))
    <> " "
    <> T.unpack (fcForeignCallName foreignCall)
    <> " ["
    <> intercalate ", " (map renderForeignType (fcForeignArgumentTypes signature))
    <> " → "
    <> renderForeignType (fcForeignResultType signature)
    <> "; "
    <> renderForeignEffect (fcForeignEffect signature)
    <> "] : "
    <> renderType (fcForeignCallType signature)
  where
    signature = fcForeignCallSignature foreignCall

renderForeignType :: FcForeignType -> String
renderForeignType foreignType =
  case foreignType of
    FcForeignInt -> "Int"
    FcForeignInt32 -> "Int32"
    FcForeignWord64 -> "Word64"
    FcForeignAddr -> "Addr"

renderForeignEffect :: FcForeignEffect -> String
renderForeignEffect effect =
  case effect of
    FcForeignPure -> "pure"
    FcForeignRealWorld -> "real-world"

type TermScope = [(Unique, T.Text)]

renderBind :: RenderSymbols -> Int -> TermScope -> [TyVarId] -> FcBind -> String
renderBind symbols indentation scope tyScope bind =
  case bind of
    FcNonRec var rhs -> renderOne symbols indentation scope tyScope var rhs
    FcRec [] -> indent indentation <> "rec {}"
    FcRec bindings ->
      indent indentation
        <> "rec {\n"
        <> intercalate ";\n" [indent (indentation + 2) <> renderBinder var <> " : " <> renderTypeWith tyScope (varType var) | (var, _) <- bindings]
        <> ";\n"
        <> intercalate
          ";\n"
          [ indent (indentation + 2)
              <> renderBinder var
              <> " =\n"
              <> renderExprIndented symbols (indentation + 4) recursiveScope tyScope rhs
          | (var, rhs) <- bindings
          ]
        <> "\n"
        <> indent indentation
        <> "}"
      where
        recursiveScope = map (scopeEntry . fst) bindings <> scope

renderOne :: RenderSymbols -> Int -> TermScope -> [TyVarId] -> Var -> FcExpr -> String
renderOne symbols indentation scope tyScope var rhs =
  indent indentation
    <> renderBinder var
    <> " : "
    <> renderTypeWith tyScope (varType var)
    <> " =\n"
    <> renderExprIndented symbols (indentation + 2) (scopeEntry var : scope) tyScope rhs

renderExpr :: FcExpr -> String
renderExpr = renderExprWith emptyRenderSymbols [] [] 0 False

renderExprIndented :: RenderSymbols -> Int -> TermScope -> [TyVarId] -> FcExpr -> String
renderExprIndented symbols indentation scope tyScope expression =
  indent indentation <> renderExprWith symbols scope tyScope indentation False expression

renderExprWith :: RenderSymbols -> TermScope -> [TyVarId] -> Int -> Bool -> FcExpr -> String
renderExprWith symbols scope tyScope indentation parenthesize expression =
  case expression of
    FcVar var -> renderOccurrence symbols scope tyScope var
    FcLit literal -> renderLiteral literal
    FcApp function argument ->
      paren parenthesize (renderExprWith symbols scope tyScope indentation True function <> " " <> renderExprWith symbols scope tyScope indentation True argument)
    FcTyApp function argument ->
      paren parenthesize (renderExprWith symbols scope tyScope indentation True function <> " @" <> renderTypeAtomWith tyScope argument)
    FcLam var body ->
      paren parenthesize $
        "λ("
          <> renderBinder var
          <> " : "
          <> renderTypeWith tyScope (varType var)
          <> ").\n"
          <> renderExprIndented symbols (indentation + 2) (scopeEntry var : scope) tyScope body
    FcTyLam tyVar body ->
      paren parenthesize $
        "Λ"
          <> renderTyVarBinderWith tyScope tyVar
          <> ".\n"
          <> renderExprIndented symbols (indentation + 2) scope (tyVar : tyScope) body
    FcLet bind body ->
      paren parenthesize $
        "let {\n"
          <> renderBind symbols (indentation + 2) scope tyScope bind
          <> "\n"
          <> indent indentation
          <> "} in\n"
          <> renderExprIndented symbols (indentation + 2) (bindScope bind <> scope) tyScope body
    FcCase scrutinee binder alternatives ->
      paren parenthesize $
        "case "
          <> renderExprWith symbols scope tyScope indentation False scrutinee
          <> " as ("
          <> renderBinder binder
          <> " : "
          <> renderTypeWith tyScope (varType binder)
          <> ") of "
          <> renderAlternatives binder alternatives
    FcCast body coercion ->
      paren parenthesize (renderExprWith symbols scope tyScope indentation True body <> " ▷ " <> renderCoercionWith tyScope coercion)
    FcCallForeign foreignCall arguments ->
      paren parenthesize $
        "foreign-call "
          <> renderForeignCallHeader foreignCall
          <> concatMap ((" " <>) . renderExprWith symbols scope tyScope indentation True) arguments
  where
    renderAlternatives _ [] = "{}"
    renderAlternatives binder' alternatives' =
      "{\n"
        <> intercalate ";\n" (map (renderAlt symbols (indentation + 2) (scopeEntry binder' : scope) tyScope) alternatives')
        <> "\n"
        <> indent indentation
        <> "}"

bindScope :: FcBind -> TermScope
bindScope bind =
  case bind of
    FcNonRec var _ -> [scopeEntry var]
    FcRec bindings -> map (scopeEntry . fst) bindings

renderAlt :: RenderSymbols -> Int -> TermScope -> [TyVarId] -> FcAlt -> String
renderAlt symbols indentation scope tyScope alternative =
  indent indentation
    <> renderAltCon (altCon alternative)
    <> concatMap (\binder -> " (" <> renderBinder binder <> " : " <> renderTypeWith tyScope (varType binder) <> ")") binders
    <> " →\n"
    <> renderExprIndented symbols (indentation + 2) (map scopeEntry binders <> scope) tyScope (altRhs alternative)
  where
    binders = altBinders alternative

renderAltCon :: FcAltCon -> String
renderAltCon alternative =
  case alternative of
    DataAlt name -> T.unpack name
    LitAlt literal -> renderLiteral literal
    DefaultAlt -> "_"

renderBinder :: Var -> String
renderBinder = T.unpack . varName

renderOccurrence :: RenderSymbols -> TermScope -> [TyVarId] -> Var -> String
renderOccurrence symbols scope tyScope var
  | scopeEntry var `elem` scope = renderBinder var
  | Just origin <- varResolvedName var,
    origin `Set.member` renderExternalOrigins symbols =
      renderOrigin origin
  | Just origin <- varResolvedName var,
    isLocalOrigin symbols origin =
      renderBinder var
  | varName var `Set.member` renderLocalNames symbols = renderBinder var
  | otherwise =
      "("
        <> maybe (renderBinder var) renderOrigin (varResolvedName var)
        <> " : "
        <> renderTypeWith tyScope (varType var)
        <> ")"

renderOrigin :: FcSymbolOrigin -> String
renderOrigin origin =
  case origin of
    FcTopLevelOrigin packageName moduleName symbolName ->
      (if packageName == "" then "" else show (T.unpack packageName) <> " ")
        <> T.unpack moduleName
        <> "."
        <> T.unpack symbolName
    FcBuiltinOrigin symbolName -> "builtin." <> T.unpack symbolName

scopeEntry :: Var -> (Unique, T.Text)
scopeEntry var = (varUnique var, varName var)

renderForeignCallHeader :: FcForeignCall -> String
renderForeignCallHeader foreignCall =
  show (T.unpack (fcForeignCallSymbol foreignCall))
    <> " "
    <> T.unpack (fcForeignCallName foreignCall)
    <> " ["
    <> intercalate ", " (map renderForeignType (fcForeignArgumentTypes signature))
    <> " → "
    <> renderForeignType (fcForeignResultType signature)
    <> "; "
    <> renderForeignEffect (fcForeignEffect signature)
    <> "]"
  where
    signature = fcForeignCallSignature foreignCall

renderLiteral :: Literal -> String
renderLiteral literal =
  case literal of
    LitInt runtimeRep value -> show value <> "#" <> renderRuntimeRep runtimeRep
    LitChar runtimeRep value -> show value <> "#" <> renderRuntimeRep runtimeRep
    LitString value -> show (T.unpack value)
    LitAddr value -> show (map (chr . fromIntegral) (BS.unpack value)) <> "#AddrRep"

renderType :: TcType -> String
renderType = renderTypeWith []

renderTypeWith :: [TyVarId] -> TcType -> String
renderTypeWith scope ty =
  case ty of
    TcTyVar tyVar -> renderTyVarOccurrence scope tyVar
    TcMetaTv (Unique unique) -> "?" <> show unique
    TcTyCon tyCon arguments
      | unboxedTupleTyConArity (tyConName tyCon) == Just (length arguments) ->
          unwords (unboxedTupleConstructorName (length arguments) : map (renderTypeAtomWith scope) arguments)
    TcTyCon tyCon [] -> T.unpack (tyConName tyCon)
    TcTyCon (TyCon "[]" _) [argument] -> "[" <> renderTypeWith scope argument <> "]"
    TcTyCon tyCon arguments ->
      unwords (T.unpack (tyConName tyCon) : map (renderTypeAtomWith scope) arguments)
    TcFunTy argument result -> renderTypeAtomWith scope argument <> " → " <> renderTypeWith scope result
    TcForAllTy tyVar body ->
      let (tyVars, inner) = collectForAlls body
          allTyVars = tyVar : tyVars
       in "∀ " <> unwords (renderTyVarBinders scope allTyVars) <> ". " <> renderTypeWith (allTyVars <> scope) inner
    TcQualTy predicates body ->
      "(" <> intercalate ", " (map (renderPred scope) predicates) <> ") ⇒ " <> renderTypeWith scope body
    TcAppTy function argument ->
      renderTypeAtomWith scope function <> " · " <> renderTypeAtomWith scope argument

unboxedTupleConstructorName :: Int -> String
unboxedTupleConstructorName arity =
  "(#" <> replicate (max 0 (arity - 1)) ',' <> "#)"

renderTypeAtomWith :: [TyVarId] -> TcType -> String
renderTypeAtomWith scope ty =
  case ty of
    TcTyVar {} -> renderTypeWith scope ty
    TcMetaTv {} -> renderTypeWith scope ty
    TcTyCon _ [] -> renderTypeWith scope ty
    TcTyCon (TyCon "[]" _) [_] -> renderTypeWith scope ty
    _ -> "(" <> renderTypeWith scope ty <> ")"

renderTyVarBinders :: [TyVarId] -> [TyVarId] -> [String]
renderTyVarBinders _ [] = []
renderTyVarBinders scope (tyVar : tyVars) =
  renderTyVarBinderWith scope tyVar : renderTyVarBinders (tyVar : scope) tyVars

renderTyVarBinderWith :: [TyVarId] -> TyVarId -> String
renderTyVarBinderWith scope tyVar =
  "(" <> T.unpack (tvName tyVar) <> " : " <> renderKindWith scope (tvKind tyVar) <> ")"

renderTyVarOccurrence :: [TyVarId] -> TyVarId -> String
renderTyVarOccurrence scope tyVar
  | tyVar `elem` scope = T.unpack (tvName tyVar)
  | otherwise = renderTyVarBinderWith scope tyVar

renderPred :: [TyVarId] -> Pred -> String
renderPred scope predicate =
  case predicate of
    ClassPred name arguments -> unwords (T.unpack name : map (renderTypeAtomWith scope) arguments)
    EqPred left right -> renderTypeAtomWith scope left <> " ~ " <> renderTypeAtomWith scope right

collectForAlls :: TcType -> ([TyVarId], TcType)
collectForAlls (TcForAllTy tyVar body) =
  let (tyVars, inner) = collectForAlls body
   in (tyVar : tyVars, inner)
collectForAlls ty = ([], ty)

renderKindWith :: [TyVarId] -> Kind -> String
renderKindWith scope kind =
  case kind of
    KTYPE runtimeRep -> "TYPE " <> renderRuntimeRepWith scope runtimeRep
    KConstraint -> "Constraint"
    KRuntimeRep -> "RuntimeRep"
    KLevity -> "Levity"
    KVecCount -> "VecCount"
    KVecElem -> "VecElem"
    KFun argument result -> renderKindAtomWith scope argument <> " → " <> renderKindWith scope result
    KMeta (Unique unique) -> "?k" <> show unique

renderKindAtomWith :: [TyVarId] -> Kind -> String
renderKindAtomWith scope kind =
  case kind of
    KFun {} -> "(" <> renderKindWith scope kind <> ")"
    _ -> renderKindWith scope kind

renderRuntimeRep :: RuntimeRep -> String
renderRuntimeRep = renderRuntimeRepWith []

renderRuntimeRepWith :: [TyVarId] -> RuntimeRep -> String
renderRuntimeRepWith scope runtimeRep =
  case runtimeRep of
    VecRep count element -> "VecRep " <> show count <> " " <> show element
    TupleRep fields -> "TupleRep [" <> intercalate ", " (map (renderRuntimeRepWith scope) fields) <> "]"
    SumRep fields -> "SumRep [" <> intercalate ", " (map (renderRuntimeRepWith scope) fields) <> "]"
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
    RuntimeRepVar unique ->
      maybe
        ("RuntimeRepVar " <> showUnique unique)
        (T.unpack . tvName)
        (find (\tyVar -> tvUnique tyVar == unique && tvKind tyVar == KRuntimeRep) scope)
    RuntimeRepMeta (Unique unique) -> "RuntimeRepMeta " <> show unique
  where
    showUnique (Unique unique) = show unique

renderCoercionWith :: [TyVarId] -> Coercion -> String
renderCoercionWith tyScope coercion =
  case coercion of
    CoVar (EvVar (Unique unique)) -> "co#" <> show unique
    Refl ty -> "refl (" <> renderTypeWith tyScope ty <> ")"
    Sym inner -> "sym (" <> renderCoercionWith tyScope inner <> ")"
    Trans left right -> "trans (" <> renderCoercionWith tyScope left <> ") (" <> renderCoercionWith tyScope right <> ")"
    TyConAppCo tyCon arguments ->
      "tycon-co " <> T.unpack (tyConName tyCon) <> concatMap (\argument -> " (" <> renderCoercionWith tyScope argument <> ")") arguments
    AxiomInstCo name arguments ->
      "axiom-co " <> T.unpack name <> concatMap (\argument -> " @" <> renderTypeAtomWith tyScope argument) arguments

paren :: Bool -> String -> String
paren False value = value
paren True value = "(" <> value <> ")"

indent :: Int -> String
indent count = replicate count ' '
