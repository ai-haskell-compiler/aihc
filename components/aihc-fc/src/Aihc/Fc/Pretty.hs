{-# LANGUAGE OverloadedStrings #-}

-- | Human-readable, canonical System FC syntax.
module Aihc.Fc.Pretty
  ( renderProgram,
    renderExpr,
    renderType,
    renderTopBind,
  )
where

import Aihc.Fc.Subst (freeRigidTyVarsOf)
import Aihc.Fc.Syntax
import Aihc.Tc.Evidence (Coercion (..), EvVar (..))
import Aihc.Tc.Types
import Data.ByteString qualified as BS
import Data.Char (chr)
import Data.List (intercalate)
import Data.Text qualified as T

renderProgram :: FcProgram -> String
renderProgram = intercalate "\n\n" . map renderTopBind . fcTopBinds

renderTopBind :: FcTopBind -> String
renderTopBind topBind =
  case topBind of
    FcData name tyVars constructors ->
      "data "
        <> T.unpack name
        <> concatMap ((" " <>) . renderTyVarBinder) tyVars
        <> renderConstructors tyVars constructors
    FcAxiom declaration ->
      "axiom "
        <> T.unpack (fcAxiomName declaration)
        <> concatMap ((" " <>) . renderTyVarBinder) (fcAxiomTyVars declaration)
        <> " : "
        <> renderTypeWith (fcAxiomTyVars declaration) (fcAxiomLeft declaration)
        <> renderAxiomRole (fcAxiomRole declaration)
        <> renderTypeWith (fcAxiomTyVars declaration) (fcAxiomRight declaration)
    FcNewtype declaration ->
      "newtype "
        <> T.unpack (fcNewtypeName declaration)
        <> concatMap ((" " <>) . renderTyVarBinder) (fcNewtypeTyVars declaration)
        <> " : "
        <> renderTypeWith (fcNewtypeTyVars declaration) (fcNewtypeResult declaration)
        <> " = "
        <> T.unpack (fcNewtypeConstructor declaration)
        <> " "
        <> renderTypeAtomWith (fcNewtypeTyVars declaration) (fcNewtypeRepresentation declaration)
    FcPrimitive var arity ->
      "foreign prim " <> renderBinder var <> "/" <> show arity <> " : " <> renderType (varType var)
    FcForeignImport foreignCall -> renderForeignImport foreignCall
    FcTopBind bind -> renderBind 0 [] [] bind

renderConstructors :: [TyVarId] -> [(T.Text, [TcType])] -> String
renderConstructors _ [] = ""
renderConstructors tyVars constructors =
  "\n" <> intercalate "\n" (zipWith renderConstructor (" = " : repeat " | ") constructors)
  where
    renderConstructor prefix (name, fields) =
      prefix
        <> renderExistentials fields
        <> T.unpack name
        <> concatMap (\field -> " (" <> renderTypeWith (tyVars <> freeRigidTyVarsOf fields) field <> ")") fields
    renderExistentials fields =
      case filter (`notElem` tyVars) (freeRigidTyVarsOf fields) of
        [] -> ""
        existentialTyVars -> "∀ " <> unwords (map renderTyVarBinder existentialTyVars) <> ". "

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

renderBind :: Int -> TermScope -> [TyVarId] -> FcBind -> String
renderBind indentation scope tyScope bind =
  case bind of
    FcNonRec var rhs -> renderOne indentation scope tyScope var rhs
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
              <> renderExprIndented (indentation + 4) recursiveScope tyScope rhs
          | (var, rhs) <- bindings
          ]
        <> "\n"
        <> indent indentation
        <> "}"
      where
        recursiveScope = map (scopeEntry . fst) bindings <> scope

renderOne :: Int -> TermScope -> [TyVarId] -> Var -> FcExpr -> String
renderOne indentation scope tyScope var rhs =
  indent indentation
    <> renderBinder var
    <> " : "
    <> renderTypeWith tyScope (varType var)
    <> " =\n"
    <> renderExprIndented (indentation + 2) (scopeEntry var : scope) tyScope rhs

renderExpr :: FcExpr -> String
renderExpr = renderExprWith [] [] 0 False

renderExprIndented :: Int -> TermScope -> [TyVarId] -> FcExpr -> String
renderExprIndented indentation scope tyScope expression =
  indent indentation <> renderExprWith scope tyScope indentation False expression

renderExprWith :: TermScope -> [TyVarId] -> Int -> Bool -> FcExpr -> String
renderExprWith scope tyScope indentation parenthesize expression =
  case expression of
    FcVar var -> renderOccurrence scope tyScope var
    FcLit literal -> renderLiteral literal
    FcApp function argument ->
      paren parenthesize (renderExprWith scope tyScope indentation True function <> " " <> renderExprWith scope tyScope indentation True argument)
    FcTyApp function argument ->
      paren parenthesize (renderExprWith scope tyScope indentation True function <> " @" <> renderTypeAtomWith tyScope argument)
    FcLam var body ->
      paren parenthesize $
        "λ("
          <> renderBinder var
          <> " : "
          <> renderTypeWith tyScope (varType var)
          <> ").\n"
          <> renderExprIndented (indentation + 2) (scopeEntry var : scope) tyScope body
    FcTyLam tyVar body ->
      paren parenthesize $
        "Λ"
          <> renderTyVarBinder tyVar
          <> ".\n"
          <> renderExprIndented (indentation + 2) scope (tyVar : tyScope) body
    FcLet bind body ->
      paren parenthesize $
        "let {\n"
          <> renderBind (indentation + 2) scope tyScope bind
          <> "\n"
          <> indent indentation
          <> "} in\n"
          <> renderExprIndented (indentation + 2) (bindScope bind <> scope) tyScope body
    FcCase scrutinee binder alternatives ->
      paren parenthesize $
        "case "
          <> renderExprWith scope tyScope indentation False scrutinee
          <> " as ("
          <> renderBinder binder
          <> " : "
          <> renderTypeWith tyScope (varType binder)
          <> ") of "
          <> renderAlternatives binder alternatives
    FcCast body coercion ->
      paren parenthesize (renderExprWith scope tyScope indentation True body <> " ▷ " <> renderCoercionWith tyScope coercion)
    FcCallForeign foreignCall arguments ->
      paren parenthesize $
        "foreign-call "
          <> renderForeignCallHeader foreignCall
          <> concatMap ((" " <>) . renderExprWith scope tyScope indentation True) arguments
  where
    renderAlternatives _ [] = "{}"
    renderAlternatives binder' alternatives' =
      "{\n"
        <> intercalate ";\n" (map (renderAlt (indentation + 2) (scopeEntry binder' : scope) tyScope) alternatives')
        <> "\n"
        <> indent indentation
        <> "}"

bindScope :: FcBind -> TermScope
bindScope bind =
  case bind of
    FcNonRec var _ -> [scopeEntry var]
    FcRec bindings -> map (scopeEntry . fst) bindings

renderAlt :: Int -> TermScope -> [TyVarId] -> FcAlt -> String
renderAlt indentation scope tyScope alternative =
  indent indentation
    <> renderAltCon (altCon alternative)
    <> concatMap (\binder -> " (" <> renderBinder binder <> " : " <> renderTypeWith tyScope (varType binder) <> ")") binders
    <> " →\n"
    <> renderExprIndented (indentation + 2) (map scopeEntry binders <> scope) tyScope (altRhs alternative)
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

renderOccurrence :: TermScope -> [TyVarId] -> Var -> String
renderOccurrence scope tyScope var
  | scopeEntry var `elem` scope = renderBinder var
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
    TcTyCon tyCon [] -> T.unpack (tyConName tyCon)
    TcTyCon (TyCon "[]" _) [argument] -> "[" <> renderTypeWith scope argument <> "]"
    TcTyCon tyCon arguments ->
      unwords (T.unpack (tyConName tyCon) : map (renderTypeAtomWith scope) arguments)
    TcFunTy argument result -> renderTypeAtomWith scope argument <> " → " <> renderTypeWith scope result
    TcForAllTy tyVar body ->
      let (tyVars, inner) = collectForAlls body
          allTyVars = tyVar : tyVars
       in "∀ " <> unwords (map renderTyVarBinder allTyVars) <> ". " <> renderTypeWith (allTyVars <> scope) inner
    TcQualTy predicates body ->
      "(" <> intercalate ", " (map (renderPred scope) predicates) <> ") ⇒ " <> renderTypeWith scope body
    TcAppTy function argument ->
      renderTypeAtomWith scope function <> " · " <> renderTypeAtomWith scope argument

renderTypeAtomWith :: [TyVarId] -> TcType -> String
renderTypeAtomWith scope ty =
  case ty of
    TcTyVar {} -> renderTypeWith scope ty
    TcMetaTv {} -> renderTypeWith scope ty
    TcTyCon _ [] -> renderTypeWith scope ty
    TcTyCon (TyCon "[]" _) [_] -> renderTypeWith scope ty
    _ -> "(" <> renderTypeWith scope ty <> ")"

renderTyVarBinder :: TyVarId -> String
renderTyVarBinder tyVar = "(" <> T.unpack (tvName tyVar) <> " : " <> renderKind (tvKind tyVar) <> ")"

renderTyVarOccurrence :: [TyVarId] -> TyVarId -> String
renderTyVarOccurrence scope tyVar
  | tyVar `elem` scope = T.unpack (tvName tyVar)
  | otherwise = renderTyVarBinder tyVar

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

renderKind :: Kind -> String
renderKind kind =
  case kind of
    KTYPE runtimeRep -> "TYPE " <> renderRuntimeRep runtimeRep
    KConstraint -> "Constraint"
    KRuntimeRep -> "RuntimeRep"
    KLevity -> "Levity"
    KVecCount -> "VecCount"
    KVecElem -> "VecElem"
    KFun argument result -> renderKindAtom argument <> " → " <> renderKind result
    KMeta (Unique unique) -> "?k" <> show unique

renderKindAtom :: Kind -> String
renderKindAtom kind =
  case kind of
    KFun {} -> "(" <> renderKind kind <> ")"
    _ -> renderKind kind

renderRuntimeRep :: RuntimeRep -> String
renderRuntimeRep runtimeRep =
  case runtimeRep of
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
