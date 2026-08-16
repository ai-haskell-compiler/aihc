{-# LANGUAGE OverloadedStrings #-}

-- | Human-readable, canonical System FC syntax.
module Aihc.Fc.Pretty
  ( renderProgram,
    renderExpr,
    renderType,
    renderTopBind,
  )
where

import Aihc.Fc.Syntax
import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Evidence (Coercion (..), EvVar (..))
import Aihc.Tc.Types
import Data.ByteString qualified as BS
import Data.Char (chr)
import Data.List (intercalate)
import Data.Text qualified as T

renderProgram :: FcProgram -> String
renderProgram program =
  intercalate
    "\n\n"
    (renderModuleDeclaration (fcProgramModule program) : map renderTopBind topBinds)
  where
    topBinds = fcTopBinds program

renderModuleDeclaration :: FcModuleId -> String
renderModuleDeclaration moduleId =
  "module " <> renderModuleOrigin (fcModulePackageText moduleId) (fcModuleName moduleId) <> " where"

renderKindScheme :: TypeScheme -> String
renderKindScheme scheme@(ForAll tyVars _ _) =
  prefix <> renderKind (kindFromTypeScheme scheme)
  where
    prefix =
      case tyVars of
        [] -> ""
        _ -> "∀ " <> unwords (map renderTyVarBinder tyVars) <> ". "

renderTopBind :: FcTopBind -> String
renderTopBind topBind =
  case topBind of
    FcExternal origin ty ->
      "external " <> renderOrigin origin <> " : " <> renderType ty
    FcData declaration ->
      "data "
        <> renderDeclarationName (fcDataOrigin declaration)
        <> concatMap ((" " <>) . renderTyVarBinder) (fcDataTyVars declaration)
        <> renderDataResultKind declaration
        <> renderConstructors (fcDataTyVars declaration) (fcDataConstructors declaration)
    FcAxiom declaration ->
      "axiom "
        <> T.unpack (fcAxiomName declaration)
        <> concatMap ((" " <>) . renderTyVarBinder) (fcAxiomTyVars declaration)
        <> " : "
        <> renderType (fcAxiomLeft declaration)
        <> renderAxiomRole (fcAxiomRole declaration)
        <> renderType (fcAxiomRight declaration)
    FcNewtype declaration ->
      "newtype "
        <> renderDeclarationName (fcNewtypeOrigin declaration)
        <> concatMap ((" " <>) . renderTyVarBinder) (fcNewtypeTyVars declaration)
        <> " : "
        <> renderType (fcNewtypeResult declaration)
        <> " = "
        <> renderConstructorId (fcNewtypeConstructorOrigin declaration)
        <> " "
        <> renderTypeAtom (fcNewtypeRepresentation declaration)
    FcPrimitive var arity ->
      "foreign prim " <> renderBinder var <> "/" <> show arity <> " : " <> renderType (varType var)
    FcForeignImport foreignCall -> renderForeignImport foreignCall
    FcTopBind bind -> renderBind 0 bind

renderDataResultKind :: FcDataDecl -> String
renderDataResultKind declaration =
  " : " <> renderKind (fcDataResultKind declaration)

renderModuleOrigin :: T.Text -> T.Text -> String
renderModuleOrigin packageName moduleName =
  show (T.unpack packageName) <> " " <> T.unpack moduleName

renderConstructors :: [TyVarId] -> [FcDataConDecl] -> String
renderConstructors _ [] = ""
renderConstructors _ constructors =
  "\n" <> intercalate "\n" (zipWith renderConstructor (" = " : repeat " | ") constructors)
  where
    renderConstructor prefix declaration =
      prefix
        <> renderConstructorId (fcDataConOrigin declaration)
        <> concatMap (\field -> " (" <> renderType field <> ")") fields
      where
        fields = fcDataConFields declaration

renderDeclarationName :: FcSymbolOrigin -> String
renderDeclarationName = renderOrigin

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
    <> renderType (fcForeignCallType foreignCall)
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

renderBind :: Int -> FcBind -> String
renderBind indentation bind =
  case bind of
    FcNonRec var rhs -> renderOne indentation var rhs
    FcRec [] -> indent indentation <> "rec {}"
    FcRec bindings ->
      indent indentation
        <> "rec {\n"
        <> intercalate ";\n" [indent (indentation + 2) <> renderBinder var <> " : " <> renderType (varType var) | (var, _) <- bindings]
        <> ";\n"
        <> intercalate
          ";\n"
          [ indent (indentation + 2)
              <> renderBinder var
              <> " =\n"
              <> renderExprIndented (indentation + 4) rhs
          | (var, rhs) <- bindings
          ]
        <> "\n"
        <> indent indentation
        <> "}"

renderOne :: Int -> Var -> FcExpr -> String
renderOne indentation var rhs =
  indent indentation
    <> renderBinder var
    <> " : "
    <> renderType (varType var)
    <> " =\n"
    <> renderExprIndented (indentation + 2) rhs

renderExpr :: FcExpr -> String
renderExpr = renderExprWith 0 False

renderExprIndented :: Int -> FcExpr -> String
renderExprIndented indentation expression =
  indent indentation <> renderExprWith indentation False expression

renderExprWith :: Int -> Bool -> FcExpr -> String
renderExprWith indentation parenthesize expression =
  case expression of
    FcVar var -> renderOccurrence var
    FcLit literal ty -> "(" <> renderLiteral literal <> " : " <> renderType ty <> ")"
    FcApp function argument ->
      paren parenthesize (renderExprWith indentation True function <> " " <> renderExprWith indentation True argument)
    FcTyApp function argument ->
      paren parenthesize (renderExprWith indentation True function <> " @" <> renderTypeAtom argument)
    FcLam var body ->
      paren parenthesize $
        "λ("
          <> renderBinder var
          <> " : "
          <> renderType (varType var)
          <> ").\n"
          <> renderExprIndented (indentation + 2) body
    FcTyLam tyVar body ->
      paren parenthesize $
        "Λ"
          <> renderTyVarBinder tyVar
          <> ".\n"
          <> renderExprIndented (indentation + 2) body
    FcLet bind body ->
      paren parenthesize $
        "let {\n"
          <> renderBind (indentation + 2) bind
          <> "\n"
          <> indent indentation
          <> "} in\n"
          <> renderExprIndented (indentation + 2) body
    FcCase scrutinee binder alternatives ->
      paren parenthesize $
        "case "
          <> renderExprWith indentation False scrutinee
          <> " as ("
          <> renderBinder binder
          <> " : "
          <> renderType (varType binder)
          <> ") of "
          <> renderAlternatives binder alternatives
    FcCast body coercion ->
      paren parenthesize (renderExprWith indentation True body <> " ▷ " <> renderCoercion coercion)
    FcCallForeign foreignCall arguments ->
      paren parenthesize $
        "foreign-call "
          <> renderForeignCallHeader foreignCall
          <> concatMap ((" " <>) . renderExprWith indentation True) arguments
  where
    renderAlternatives _ [] = "{}"
    renderAlternatives _ alternatives' =
      "{\n"
        <> intercalate ";\n" (map (renderAlt (indentation + 2)) alternatives')
        <> "\n"
        <> indent indentation
        <> "}"

renderAlt :: Int -> FcAlt -> String
renderAlt indentation alternative =
  indent indentation
    <> renderAltCon (altCon alternative)
    <> concatMap (\binder -> " (" <> renderBinder binder <> " : " <> renderType (varType binder) <> ")") binders
    <> " →\n"
    <> renderExprIndented (indentation + 2) (altRhs alternative)
  where
    binders = altBinders alternative

renderAltCon :: FcAltCon -> String
renderAltCon alternative =
  case alternative of
    DataAlt constructor -> renderConstructorId constructor
    LitAlt literal ty -> "(" <> renderLiteral literal <> " : " <> renderType ty <> ")"
    DefaultAlt -> "_"

renderConstructorId :: FcConstructorId -> String
renderConstructorId constructor =
  show (T.unpack (packageIdText (fcConstructorPackage constructor)))
    <> " "
    <> T.unpack (fcConstructorModule constructor)
    <> "."
    <> T.unpack (fcConstructorName constructor)

renderBinder :: Var -> String
renderBinder = renderVarReference

renderVarReference :: Var -> String
renderVarReference var =
  T.unpack (varName var)
    <> "{unique "
    <> renderUnique (varUnique var)
    <> "}"
    <> maybe "" renderVarOrigin (varResolvedName var)

renderOccurrence :: Var -> String
renderOccurrence var =
  "(" <> renderVarReference var <> " : " <> renderType (varType var) <> ")"

renderVarOrigin :: FcSymbolOrigin -> String
renderVarOrigin origin = "{origin " <> renderOrigin origin <> "}"

renderOrigin :: FcSymbolOrigin -> String
renderOrigin origin =
  case origin of
    FcTopLevelOrigin packageName moduleName symbolName ->
      show (T.unpack packageName)
        <> " "
        <> T.unpack moduleName
        <> "."
        <> T.unpack symbolName
    FcBuiltinOrigin symbolName -> "builtin." <> T.unpack symbolName

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
renderType ty =
  case ty of
    TcTyVar tyVar -> renderTyVarBinder tyVar
    TcMetaTv (Unique unique) -> "?" <> show unique
    TcTyCon tyCon [] -> renderTyConHead tyCon
    TcTyCon tyCon arguments ->
      renderTyConHead tyCon <> "[" <> intercalate ", " (map renderType arguments) <> "]"
    TcFunTy argument result -> renderTypeAtom argument <> " → " <> renderType result
    TcForAllTy tyVar body ->
      "∀ " <> renderTyVarBinder tyVar <> ". " <> renderType body
    TcQualTy predicates body ->
      "(" <> intercalate ", " (map renderPred predicates) <> ") ⇒ " <> renderType body
    TcAppTy function argument ->
      renderTypeAtom function <> " · " <> renderTypeAtom argument
    TcBuiltinTyCon name arity arguments ->
      "builtin " <> T.unpack name <> "/" <> show arity <> "[" <> intercalate ", " (map renderType arguments) <> "]"

renderTyConHead :: TyCon -> String
renderTyConHead tyCon =
  "tycon "
    <> show (T.unpack (packageIdText (tyConPackageId tyCon)))
    <> " "
    <> show (T.unpack (tyConModuleName tyCon))
    <> " "
    <> T.unpack (tyConName tyCon)
    <> "/"
    <> show (tyConArity tyCon)
    <> " { :: "
    <> renderKindScheme (tyConKindScheme tyCon)
    <> " }"

renderTypeAtom :: TcType -> String
renderTypeAtom ty =
  case ty of
    TcTyVar {} -> renderType ty
    TcMetaTv {} -> renderType ty
    TcTyCon {} -> renderType ty
    TcBuiltinTyCon {} -> renderType ty
    _ -> "(" <> renderType ty <> ")"

renderTyVarBinder :: TyVarId -> String
renderTyVarBinder tyVar =
  "("
    <> T.unpack (tvName tyVar)
    <> "{unique "
    <> renderUnique (tvUnique tyVar)
    <> "} : "
    <> renderKind (tvKind tyVar)
    <> ")"

renderPred :: Pred -> String
renderPred predicate =
  case predicate of
    ClassPred classTyCon arguments -> renderTypeAtom (TcTyCon classTyCon arguments)
    EqPred left right -> renderTypeAtom left <> " ~ " <> renderTypeAtom right

renderKind :: Kind -> String
renderKind kind =
  case kind of
    KTYPE (BoxedRep Lifted) -> "Type"
    KTYPE (BoxedRep Unlifted) -> "TYPE UnliftedRep"
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
    RuntimeRepVar unique -> "RuntimeRepVar " <> showUnique unique
    RuntimeRepMeta (Unique unique) -> "RuntimeRepMeta " <> show unique
  where
    showUnique (Unique unique) = show unique

renderCoercion :: Coercion -> String
renderCoercion coercion =
  case coercion of
    CoVar (EvVar (Unique unique)) -> "co#" <> show unique
    Refl ty -> "refl (" <> renderType ty <> ")"
    Sym inner -> "sym (" <> renderCoercion inner <> ")"
    Trans left right -> "trans (" <> renderCoercion left <> ") (" <> renderCoercion right <> ")"
    TyConAppCo tyCon arguments ->
      "tycon-co " <> renderTyConHead tyCon <> concatMap (\argument -> " (" <> renderCoercion argument <> ")") arguments
    AxiomInstCo name arguments ->
      "axiom-co " <> T.unpack name <> concatMap (\argument -> " @" <> renderTypeAtom argument) arguments

paren :: Bool -> String -> String
paren False value = value
paren True value = "(" <> value <> ")"

indent :: Int -> String
indent count = replicate count ' '

renderUnique :: Unique -> String
renderUnique (Unique unique) = show unique
