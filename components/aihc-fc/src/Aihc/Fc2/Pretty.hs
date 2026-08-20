{-# LANGUAGE OverloadedStrings #-}

-- | Human-readable System FC 2 text.
module Aihc.Fc2.Pretty
  ( renderProgram,
    renderType,
    renderExpr,
  )
where

import Aihc.Fc2.Name
import Aihc.Fc2.Syntax
import Aihc.Fc2.TypeOf
import Aihc.Resolve (PackageId (..), packageIdText)
import Aihc.Tc.Types (Unique (..))
import Data.ByteString qualified as BS
import Data.Char (chr, isAscii, isPrint, ord)
import Data.List (intercalate)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import Numeric (showHex)

data Prec
  = PrecAtom
  | PrecApp
  | PrecFun
  | PrecEq
  | PrecForAll
  deriving (Eq, Ord)

renderProgram :: Program -> String
renderProgram program =
  intercalate "\n\n" (filter (not . null) (renderScopes scopes : map (renderDecl env scopes) (programDecls program)))
  where
    scopes = programScopes program
    env = typeEnvFromProgram program

renderScopes :: ScopeTable -> String
renderScopes table =
  intercalate "\n" [renderScopeEntry scopeId package moduleName | (scopeId, package, moduleName) <- scopeEntries table]

renderScopeEntry :: Int -> PackageId -> Text -> String
renderScopeEntry scopeId package moduleName =
  "scope " <> show scopeId <> " = " <> show (T.unpack (packageIdText package)) <> " " <> T.unpack moduleName

renderDecl :: TypeEnv -> ScopeTable -> Decl -> String
renderDecl env scopes decl =
  case decl of
    DeclType declaration -> renderTypeDecl env scopes declaration
    DeclSynonym declaration -> renderSynonymDecl env scopes declaration
    DeclAxiom declaration -> renderAxiomDecl env scopes declaration
    DeclVal declaration -> renderValDecl env scopes declaration
    DeclPrim declaration -> renderPrimDecl env scopes declaration

renderVis :: Vis -> String
renderVis Pub = "pub "
renderVis Private = ""

renderTypeDecl :: TypeEnv -> ScopeTable -> TypeDecl -> String
renderTypeDecl env scopes declaration =
  renderVis (typeVis declaration)
    <> "type "
    <> renderTopName scopes (typeName declaration)
    <> renderHeaderBinders env scopes (typeBinders declaration)
    <> " :: "
    <> renderTypeWith (headerBinderEnv env (typeBinders declaration)) scopes PrecForAll (typeResult declaration)
    <> renderRoleList (typeRoles declaration)
    <> renderConstructors env scopes (typeCons declaration)

renderHeaderBinders :: TypeEnv -> ScopeTable -> [Binder] -> String
renderHeaderBinders env scopes =
  concatMap ((" " <>) . renderPiBinder env scopes)

renderConstructors :: TypeEnv -> ScopeTable -> [ConDecl] -> String
renderConstructors _ _ [] = " {}"
renderConstructors env scopes constructors =
  " {\n"
    <> intercalate "\n" (map (renderConDecl env scopes) constructors)
    <> "\n}"

renderConDecl :: TypeEnv -> ScopeTable -> ConDecl -> String
renderConDecl env scopes declaration =
  "    "
    <> renderVis (conVis declaration)
    <> renderTopName scopes (conName declaration)
    <> " :: "
    <> renderTypeWith env scopes PrecForAll (conType declaration)

renderSynonymDecl :: TypeEnv -> ScopeTable -> SynonymDecl -> String
renderSynonymDecl env scopes declaration =
  renderVis (synVis declaration)
    <> "type "
    <> renderTopName scopes (synName declaration)
    <> renderHeaderBinders env scopes (synBinders declaration)
    <> " :: "
    <> renderTypeWith (headerBinderEnv env (synBinders declaration)) scopes PrecForAll (synResult declaration)
    <> " =\n "
    <> renderTypeWith (headerBinderEnv env (synBinders declaration)) scopes PrecForAll (synBody declaration)

renderAxiomDecl :: TypeEnv -> ScopeTable -> AxiomDecl -> String
renderAxiomDecl env scopes declaration =
  renderVis (axiomVis declaration)
    <> "axiom "
    <> renderTopName scopes (axiomName declaration)
    <> renderForAllBinders env scopes (axiomBinders declaration)
    <> " : "
    <> renderTypeWith binderEnv scopes PrecForAll (axiomLeft declaration)
    <> " "
    <> renderAxiomRole (axiomRole declaration)
    <> " "
    <> renderTypeWith binderEnv scopes PrecForAll (axiomRight declaration)
  where
    binderEnv = headerBinderEnv env (axiomBinders declaration)

renderForAllBinders :: TypeEnv -> ScopeTable -> [Binder] -> String
renderForAllBinders _ _ [] = ""
renderForAllBinders env scopes binders =
  " " <> unwords (map (renderPiBinder env scopes) binders)

renderAxiomRole :: Role -> String
renderAxiomRole Nominal = "~N"
renderAxiomRole Representational = "~R"
renderAxiomRole Phantom = "~P"

renderRoleList :: [Role] -> String
renderRoleList roles
  | all (== Representational) roles = ""
  | otherwise = concatMap ((" @" <>) . renderRoleTag) roles

renderRoleTag :: Role -> String
renderRoleTag Nominal = "N"
renderRoleTag Representational = "R"
renderRoleTag Phantom = "P"

renderValDecl :: TypeEnv -> ScopeTable -> ValDecl -> String
renderValDecl env scopes declaration =
  renderVis (valVis declaration)
    <> "val "
    <> renderTopName scopes (valName declaration)
    <> " :: "
    <> renderTypeWith env scopes PrecForAll (valType declaration)
    <> "\n = "
    <> renderExprWith env scopes 0 (valBody declaration)

renderPrimDecl :: TypeEnv -> ScopeTable -> PrimDecl -> String
renderPrimDecl env scopes declaration =
  "foreign import prim "
    <> renderTopName scopes (primName declaration)
    <> " :: "
    <> renderTypeWith env scopes PrecForAll (primType declaration)

renderType :: Program -> Type -> String
renderType program =
  renderTypeWith (typeEnvFromProgram program) (programScopes program) PrecForAll

renderTypeWith :: TypeEnv -> ScopeTable -> Prec -> Type -> String
renderTypeWith env scopes prec ty =
  case ty of
    TyVar name -> renderName scopes name
    TyCon name -> renderName scopes name
    TyApp function argument ->
      paren (prec < PrecApp) (renderTypeWith env scopes PrecApp function <> " " <> renderTypeWith env scopes PrecAtom argument)
    TyFun r1 r2 argument result
      | isLiftedRep env r1 && isLiftedRep env r2 ->
          paren (prec < PrecFun) (renderTypeWith env scopes PrecApp argument <> " → " <> renderTypeWith env scopes PrecFun result)
      | otherwise ->
          paren
            (prec < PrecApp)
            ( "FUN @"
                <> renderTypeWith env scopes PrecAtom r1
                <> " @"
                <> renderTypeWith env scopes PrecAtom r2
                <> " "
                <> renderTypeWith env scopes PrecAtom argument
                <> " "
                <> renderTypeWith env scopes PrecAtom result
            )
    TyForAll binder body ->
      let env' = extendPrettyEnv env binder
       in paren
            (prec < PrecForAll)
            ( "∀"
                <> renderPiBinder env scopes binder
                <> forallTail env' scopes body
            )
    TyEq left right ->
      paren (prec < PrecEq) (renderTypeWith env scopes PrecApp left <> " ~ " <> renderTypeWith env scopes PrecApp right)

forallTail :: TypeEnv -> ScopeTable -> Type -> String
forallTail env scopes ty =
  case ty of
    TyForAll binder body ->
      " " <> renderPiBinder env scopes binder <> forallTail (extendPrettyEnv env binder) scopes body
    _ -> ". " <> renderTypeWith env scopes PrecForAll ty

renderPiBinder :: TypeEnv -> ScopeTable -> Binder -> String
renderPiBinder env scopes binder =
  "("
    <> renderLocalBinder (binderName binder)
    <> " : "
    <> renderTypeWith env scopes PrecForAll (binderType binder)
    <> ")"

extendPrettyEnv :: TypeEnv -> Binder -> TypeEnv
extendPrettyEnv env binder =
  env {teBinders = Map.insert (binderName binder) (binderType binder) (teBinders env)}

headerBinderEnv :: TypeEnv -> [Binder] -> TypeEnv
headerBinderEnv = foldl (\env binder -> env {teBinders = Map.insert (binderName binder) (binderType binder) (teBinders env)})

renderExpr :: Program -> Expr -> String
renderExpr program =
  renderExprWith (typeEnvFromProgram program) (programScopes program) 0

renderExprWith :: TypeEnv -> ScopeTable -> Int -> Expr -> String
renderExprWith env scopes indent expr =
  case expr of
    ExVar name -> renderName scopes name
    ExLit literal -> renderLiteral scopes literal
    ExApp function argument ->
      renderApp env scopes indent function <> " " <> renderExprAtom env scopes indent argument
    ExTyApp function argument ->
      renderApp env scopes indent function <> " @" <> renderTypeWith env scopes PrecAtom argument
    ExLam binder body ->
      "λ" <> renderPiBinder env scopes binder <> ".\n" <> indentLine (indent + 1) (renderExprWith (extendPrettyEnv env binder) scopes (indent + 1) body)
    ExTyLam binder body ->
      "Λ" <> renderPiBinder env scopes binder <> ".\n" <> indentLine (indent + 1) (renderExprWith (extendPrettyEnv env binder) scopes (indent + 1) body)
    ExLet bind body ->
      "let {\n"
        <> indentLine (indent + 2) (renderBind env scopes (indent + 2) bind)
        <> "\n"
        <> indentLine indent "} in\n"
        <> indentLine (indent + 2) (renderExprWith (extendPrettyEnv env (bindBinder bind)) scopes (indent + 2) body)
    ExRec binds body ->
      "rec {\n"
        <> intercalate ";\n" (map (indentLine (indent + 2) . renderBind recEnv scopes (indent + 2)) binds)
        <> "\n"
        <> indentLine indent "} in\n"
        <> indentLine (indent + 2) (renderExprWith recEnv scopes (indent + 2) body)
      where
        recEnv = foldl extendPrettyEnv env (map bindBinder binds)
    ExCase scrutinee binder resultType alts ->
      "case "
        <> renderExprWith env scopes indent scrutinee
        <> " as "
        <> renderPiBinder env scopes binder
        <> " return ("
        <> renderTypeWith env scopes PrecForAll resultType
        <> ")"
        <> " of {\n"
        <> intercalate ";\n" (map (indentLine (indent + 2) . renderAlt (extendPrettyEnv env binder) scopes (indent + 2)) alts)
        <> "\n"
        <> indentLine indent "}"
    ExCast body coercion ->
      renderExprAtom env scopes indent body <> " ▷ " <> renderCoercion env scopes coercion

renderApp :: TypeEnv -> ScopeTable -> Int -> Expr -> String
renderApp env scopes indent expr =
  case expr of
    ExApp {} -> renderExprWith env scopes indent expr
    ExTyApp {} -> renderExprWith env scopes indent expr
    _ -> renderExprAtom env scopes indent expr

renderExprAtom :: TypeEnv -> ScopeTable -> Int -> Expr -> String
renderExprAtom env scopes indent expr =
  case expr of
    ExVar {} -> renderExprWith env scopes indent expr
    ExLit {} -> renderExprWith env scopes indent expr
    _ -> "(" <> renderExprWith env scopes indent expr <> ")"

renderBind :: TypeEnv -> ScopeTable -> Int -> Bind -> String
renderBind env scopes indent bind =
  renderLocalBinder (binderName (bindBinder bind))
    <> " : "
    <> renderTypeWith env scopes PrecForAll (binderType (bindBinder bind))
    <> " =\n"
    <> indentLine (indent + 2) (renderExprWith env scopes (indent + 2) (bindRhs bind))

renderAlt :: TypeEnv -> ScopeTable -> Int -> Alt -> String
renderAlt env scopes indent alternative =
  case altCon alternative of
    AltDefault -> "_ →\n" <> indentLine (indent + 2) (renderExprWith env scopes (indent + 2) (altRhs alternative))
    AltLit literal ->
      renderLiteral scopes literal <> altBinderText <> " →\n" <> indentLine (indent + 2) (renderExprWith env scopes (indent + 2) (altRhs alternative))
    AltData name ->
      renderName scopes name <> altBinderText <> " →\n" <> indentLine (indent + 2) (renderExprWith env scopes (indent + 2) (altRhs alternative))
  where
    altBinderText = concatMap ((" " <>) . renderPiBinder env scopes) (altBinders alternative)

renderCoercion :: TypeEnv -> ScopeTable -> Coercion -> String
renderCoercion env scopes coercion =
  case coercion of
    CoVar name -> renderName scopes name
    CoRefl ty -> "refl " <> renderTypeWith env scopes PrecAtom ty
    CoSym inner -> "sym (" <> renderCoercion env scopes inner <> ")"
    CoTrans left right -> "trans (" <> renderCoercion env scopes left <> ") (" <> renderCoercion env scopes right <> ")"
    CoTyConApp name arguments ->
      unwords ("tycon-co" : renderName scopes name : map (\argument -> "(" <> renderCoercion env scopes argument <> ")") arguments)
    CoAxiom name arguments ->
      unwords ("axiom-co" : renderName scopes name : map (\argument -> "@" <> renderTypeWith env scopes PrecAtom argument) arguments)

renderLiteral :: ScopeTable -> Literal -> String
renderLiteral scopes literal =
  case literal of
    LitInt representation value -> show value <> "#" <> renderName scopes (repName representation)
    LitChar representation value -> show value <> "#" <> renderName scopes (repName representation)
    LitString value -> "\"" <> concatMap encodeStringChar (T.unpack value) <> "\""
    LitAddr representation value -> "\"" <> concatMap encodeByte (BS.unpack value) <> "\"#" <> renderName scopes (repName representation)

encodeStringChar :: Char -> String
encodeStringChar character
  | character == '"' = "\\\""
  | character == '\\' = "\\\\"
  | character == '\n' = "\\n"
  | isAscii character && isPrint character = [character]
  | otherwise = encodeByte (fromIntegral (ord character))

encodeByte :: Word8 -> String
encodeByte byte
  | isAscii character && isPrint character && character `notElem` ("\"\\'" :: String) =
      [character]
  | otherwise = "\\x" <> padHex (showHex (fromIntegral byte :: Int) "")
  where
    character = chr (fromIntegral byte)
    padHex [digit] = ['0', digit]
    padHex digits = digits

repName :: Type -> Name
repName ty =
  case ty of
    TyCon name -> name
    TyVar name -> name
    _ -> Name "AddrRep" SortDataConstructor (OriginLocal (Unique 0))

renderName :: ScopeTable -> Name -> String
renderName scopes name =
  case nameOrigin name of
    OriginLocal {} -> renderLocalUse name
    OriginTop {} -> renderTopName scopes name

renderTopName :: ScopeTable -> Name -> String
renderTopName scopes name =
  case nameOrigin name of
    OriginTop package moduleName ->
      scopePrefix scopes package moduleName <> printedName name
    OriginLocal {} ->
      printedName name

scopePrefix :: ScopeTable -> PackageId -> Text -> String
scopePrefix scopes package moduleName =
  case lookupScopeId scopes package moduleName of
    Just scopeId -> show scopeId <> "."
    Nothing -> error ("missing System FC 2 scope for " <> show (packageIdText package, moduleName))

lookupScopeId :: ScopeTable -> PackageId -> Text -> Maybe Int
lookupScopeId table package moduleName =
  lookup (package, moduleName) [((entryPackage, entryModule), scopeId) | (scopeId, entryPackage, entryModule) <- scopeEntries table]

printedName :: Name -> String
printedName name =
  case nameClass (nameSort name) of
    NameClassType -> "t" <> rawPrinted (nameText name)
    NameClassValue -> "v" <> rawPrinted (nameText name)
    NameClassAxiom -> T.unpack (nameText name)
    NameClassTypeVar -> T.unpack (nameText name)

rawPrinted :: Text -> String
rawPrinted = T.unpack

renderLocalBinder :: Name -> String
renderLocalBinder name =
  T.unpack (nameText name) <> uniqueSuffix name

renderLocalUse :: Name -> String
renderLocalUse name =
  T.unpack (nameText name) <> uniqueSuffix name

uniqueSuffix :: Name -> String
uniqueSuffix name =
  case nameOrigin name of
    OriginLocal (Unique unique)
      | unique /= 0 -> "{" <> show unique <> "}"
      | otherwise -> ""
    OriginTop {} -> ""

paren :: Bool -> String -> String
paren False value = value
paren True value = "(" <> value <> ")"

indentLine :: Int -> String -> String
indentLine count value = replicate (count * 2) ' ' <> value
