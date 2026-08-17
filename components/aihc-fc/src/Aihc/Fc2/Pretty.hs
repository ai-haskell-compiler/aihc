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
import Data.Char (chr)
import Data.List (intercalate)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

data Prec
  = PrecAtom
  | PrecApp
  | PrecFun
  | PrecEq
  | PrecForAll
  deriving (Eq, Ord)

renderProgram :: Program -> String
renderProgram program =
  intercalate "\n\n" (filter (not . null) (renderScopes scopes : renderModule scopes (programModule program) : map (renderDecl env scopes Set.empty) (programDecls program)))
  where
    scopes = programScopes program
    env = typeEnvFromProgram program

renderScopes :: ScopeTable -> String
renderScopes table =
  intercalate "\n" [renderScopeEntry scopeId package moduleName | (scopeId, package, moduleName) <- scopeEntries table]

renderScopeEntry :: Int -> PackageId -> Text -> String
renderScopeEntry scopeId package moduleName =
  "scope " <> show scopeId <> " = " <> show (T.unpack (packageIdText package)) <> " " <> T.unpack moduleName

renderModule :: ScopeTable -> ModuleId -> String
renderModule scopes moduleId =
  "module " <> scopePrefix scopes (modulePackage moduleId) (moduleName moduleId) <> T.unpack (moduleName moduleId) <> " where"

renderDecl :: TypeEnv -> ScopeTable -> Set Text -> Decl -> String
renderDecl env scopes seen decl =
  case decl of
    DeclType declaration -> renderTypeDecl env scopes seen declaration
    DeclSynonym declaration -> renderSynonymDecl env scopes seen declaration
    DeclAxiom declaration -> renderAxiomDecl env scopes seen declaration
    DeclVal declaration -> renderValDecl env scopes seen declaration
    DeclPrim declaration -> renderPrimDecl env scopes seen declaration

renderVis :: Vis -> String
renderVis Pub = "pub "
renderVis Private = ""

renderTypeDecl :: TypeEnv -> ScopeTable -> Set Text -> TypeDecl -> String
renderTypeDecl env scopes seen declaration =
  renderVis (typeVis declaration)
    <> "type "
    <> renderTopName scopes (typeName declaration)
    <> renderHeaderBinders env scopes seen (typeBinders declaration)
    <> " :: "
    <> renderTypeWith (headerBinderEnv env (typeBinders declaration)) scopes seen PrecForAll (typeResult declaration)
    <> renderRoleList (typeRoles declaration)
    <> renderConstructors env scopes seen (typeCons declaration)

renderHeaderBinders :: TypeEnv -> ScopeTable -> Set Text -> [Binder] -> String
renderHeaderBinders env scopes seen =
  concatMap ((" " <>) . renderPiBinder env scopes seen)

renderConstructors :: TypeEnv -> ScopeTable -> Set Text -> [ConDecl] -> String
renderConstructors _ _ _ [] = " {}"
renderConstructors env scopes seen constructors =
  " {\n"
    <> intercalate "\n" (map (renderConDecl env scopes seen) constructors)
    <> "\n}"

renderConDecl :: TypeEnv -> ScopeTable -> Set Text -> ConDecl -> String
renderConDecl env scopes seen declaration =
  "    "
    <> renderVis (conVis declaration)
    <> renderTopName scopes (conName declaration)
    <> " :: "
    <> renderTypeWith env scopes seen PrecForAll (conType declaration)

renderSynonymDecl :: TypeEnv -> ScopeTable -> Set Text -> SynonymDecl -> String
renderSynonymDecl env scopes seen declaration =
  renderVis (synVis declaration)
    <> "type "
    <> renderTopName scopes (synName declaration)
    <> renderHeaderBinders env scopes seen (synBinders declaration)
    <> " :: "
    <> renderTypeWith (headerBinderEnv env (synBinders declaration)) scopes seen PrecForAll (synResult declaration)
    <> " =\n "
    <> renderTypeWith (headerBinderEnv env (synBinders declaration)) scopes seen PrecForAll (synBody declaration)

renderAxiomDecl :: TypeEnv -> ScopeTable -> Set Text -> AxiomDecl -> String
renderAxiomDecl env scopes seen declaration =
  renderVis (axiomVis declaration)
    <> "axiom "
    <> renderTopName scopes (axiomName declaration)
    <> renderForAllBinders env scopes seen (axiomBinders declaration)
    <> " : "
    <> renderTypeWith binderEnv scopes seen PrecForAll (axiomLeft declaration)
    <> " "
    <> renderAxiomRole (axiomRole declaration)
    <> " "
    <> renderTypeWith binderEnv scopes seen PrecForAll (axiomRight declaration)
  where
    binderEnv = headerBinderEnv env (axiomBinders declaration)

renderForAllBinders :: TypeEnv -> ScopeTable -> Set Text -> [Binder] -> String
renderForAllBinders _ _ _ [] = ""
renderForAllBinders env scopes seen binders =
  " " <> unwords (map (renderPiBinder env scopes seen) binders)

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

renderValDecl :: TypeEnv -> ScopeTable -> Set Text -> ValDecl -> String
renderValDecl env scopes seen declaration =
  renderVis (valVis declaration)
    <> "val "
    <> renderTopName scopes (valName declaration)
    <> " :: "
    <> renderTypeWith env scopes seen PrecForAll (valType declaration)
    <> "\n = "
    <> renderExprWith env scopes seen 0 (valBody declaration)

renderPrimDecl :: TypeEnv -> ScopeTable -> Set Text -> PrimDecl -> String
renderPrimDecl env scopes seen declaration =
  "foreign import prim "
    <> renderTopName scopes (primName declaration)
    <> " :: "
    <> renderTypeWith env scopes seen PrecForAll (primType declaration)

renderType :: Program -> Type -> String
renderType program =
  renderTypeWith (typeEnvFromProgram program) (programScopes program) Set.empty PrecForAll

renderTypeWith :: TypeEnv -> ScopeTable -> Set Text -> Prec -> Type -> String
renderTypeWith env scopes seen prec ty =
  case ty of
    TyVar name -> renderName scopes seen name
    TyCon name -> renderName scopes seen name
    TyApp function argument ->
      paren (prec < PrecApp) (renderTypeWith env scopes seen PrecApp function <> " " <> renderTypeWith env scopes seen PrecAtom argument)
    TyFun r1 r2 argument result
      | canHideFun env argument result r1 r2 ->
          paren (prec < PrecFun) (renderTypeWith env scopes seen PrecApp argument <> " → " <> renderTypeWith env scopes seen PrecFun result)
      | otherwise ->
          paren
            (prec < PrecApp)
            ( "FUN @"
                <> renderTypeWith env scopes seen PrecAtom r1
                <> " @"
                <> renderTypeWith env scopes seen PrecAtom r2
                <> " "
                <> renderTypeWith env scopes seen PrecAtom argument
                <> " "
                <> renderTypeWith env scopes seen PrecAtom result
            )
    TyForAll binder body ->
      let nextSeen = bindText seen binder
          env' = extendPrettyEnv env binder
       in paren
            (prec < PrecForAll)
            ( "∀"
                <> renderPiBinder env scopes seen binder
                <> forallTail env' scopes nextSeen body
            )
    TyEq left right ->
      paren (prec < PrecEq) (renderTypeWith env scopes seen PrecApp left <> " ~ " <> renderTypeWith env scopes seen PrecApp right)

forallTail :: TypeEnv -> ScopeTable -> Set Text -> Type -> String
forallTail env scopes seen ty =
  case ty of
    TyForAll binder body ->
      " " <> renderPiBinder env scopes seen binder <> forallTail (extendPrettyEnv env binder) scopes (bindText seen binder) body
    _ -> ". " <> renderTypeWith env scopes seen PrecForAll ty

renderPiBinder :: TypeEnv -> ScopeTable -> Set Text -> Binder -> String
renderPiBinder env scopes seen binder =
  "("
    <> renderLocalBinder seen (binderName binder)
    <> " : "
    <> renderTypeWith env scopes seen PrecForAll (binderType binder)
    <> ")"

canHideFun :: TypeEnv -> Type -> Type -> Type -> Type -> Bool
canHideFun env argument result r1 r2 =
  case (repOf env argument, repOf env result) of
    (Just left, Just right) -> left == r1 && right == r2
    _ -> False

extendPrettyEnv :: TypeEnv -> Binder -> TypeEnv
extendPrettyEnv env binder =
  env {teBinders = Map.insert (binderName binder) (binderType binder) (teBinders env)}

headerBinderEnv :: TypeEnv -> [Binder] -> TypeEnv
headerBinderEnv = foldl (\env binder -> env {teBinders = Map.insert (binderName binder) (binderType binder) (teBinders env)})

renderExpr :: Program -> Expr -> String
renderExpr program =
  renderExprWith (typeEnvFromProgram program) (programScopes program) Set.empty 0

renderExprWith :: TypeEnv -> ScopeTable -> Set Text -> Int -> Expr -> String
renderExprWith env scopes seen indent expr =
  case expr of
    ExVar name -> renderName scopes seen name
    ExLit literal -> renderLiteral scopes seen literal
    ExApp function argument ->
      renderApp env scopes seen indent function <> " " <> renderExprAtom env scopes seen indent argument
    ExTyApp function argument ->
      renderApp env scopes seen indent function <> " @" <> renderTypeWith env scopes seen PrecAtom argument
    ExLam binder body ->
      "λ" <> renderPiBinder env scopes seen binder <> ".\n" <> indentLine (indent + 1) (renderExprWith (extendPrettyEnv env binder) scopes (bindText seen binder) (indent + 1) body)
    ExTyLam binder body ->
      "Λ" <> renderPiBinder env scopes seen binder <> ".\n" <> indentLine (indent + 1) (renderExprWith (extendPrettyEnv env binder) scopes (bindText seen binder) (indent + 1) body)
    ExLet bind body ->
      "let {\n"
        <> indentLine (indent + 2) (renderBind env scopes seen (indent + 2) bind)
        <> "\n"
        <> indentLine indent "} in\n"
        <> indentLine (indent + 2) (renderExprWith (extendPrettyEnv env (bindBinder bind)) scopes (bindText seen (bindBinder bind)) (indent + 2) body)
    ExRec binds body ->
      "rec {\n"
        <> intercalate ";\n" (map (indentLine (indent + 2) . renderBind recEnv scopes recSeen (indent + 2)) binds)
        <> "\n"
        <> indentLine indent "} in\n"
        <> indentLine (indent + 2) (renderExprWith recEnv scopes recSeen (indent + 2) body)
      where
        recSeen = foldl bindText seen (map bindBinder binds)
        recEnv = foldl extendPrettyEnv env (map bindBinder binds)
    ExCase scrutinee binder alts ->
      "case "
        <> renderExprWith env scopes seen indent scrutinee
        <> " as "
        <> renderPiBinder env scopes seen binder
        <> " of {\n"
        <> intercalate ";\n" (map (indentLine (indent + 2) . renderAlt (extendPrettyEnv env binder) scopes (bindText seen binder) (indent + 2)) alts)
        <> "\n"
        <> indentLine indent "}"
    ExCast body coercion ->
      renderExprAtom env scopes seen indent body <> " ▷ " <> renderCoercion env scopes seen coercion

renderApp :: TypeEnv -> ScopeTable -> Set Text -> Int -> Expr -> String
renderApp env scopes seen indent expr =
  case expr of
    ExApp {} -> renderExprWith env scopes seen indent expr
    ExTyApp {} -> renderExprWith env scopes seen indent expr
    _ -> renderExprAtom env scopes seen indent expr

renderExprAtom :: TypeEnv -> ScopeTable -> Set Text -> Int -> Expr -> String
renderExprAtom env scopes seen indent expr =
  case expr of
    ExVar {} -> renderExprWith env scopes seen indent expr
    ExLit {} -> renderExprWith env scopes seen indent expr
    _ -> "(" <> renderExprWith env scopes seen indent expr <> ")"

renderBind :: TypeEnv -> ScopeTable -> Set Text -> Int -> Bind -> String
renderBind env scopes seen indent bind =
  renderLocalBinder seen (binderName (bindBinder bind))
    <> " : "
    <> renderTypeWith env scopes seen PrecForAll (binderType (bindBinder bind))
    <> " =\n"
    <> indentLine (indent + 2) (renderExprWith env scopes (bindText seen (bindBinder bind)) (indent + 2) (bindRhs bind))

renderAlt :: TypeEnv -> ScopeTable -> Set Text -> Int -> Alt -> String
renderAlt env scopes seen indent alternative =
  case altCon alternative of
    AltDefault -> "_ →\n" <> indentLine (indent + 2) (renderExprWith env scopes seen (indent + 2) (altRhs alternative))
    AltLit literal ->
      renderLiteral scopes seen literal <> altBinderText <> " →\n" <> indentLine (indent + 2) (renderExprWith env scopes nextSeen (indent + 2) (altRhs alternative))
    AltData name ->
      renderName scopes seen name <> altBinderText <> " →\n" <> indentLine (indent + 2) (renderExprWith env scopes nextSeen (indent + 2) (altRhs alternative))
  where
    altBinderText = concatMap ((" " <>) . renderPiBinder env scopes seen) (altBinders alternative)
    nextSeen = foldl bindText seen (altBinders alternative)

renderCoercion :: TypeEnv -> ScopeTable -> Set Text -> Coercion -> String
renderCoercion env scopes seen coercion =
  case coercion of
    CoVar name -> renderName scopes seen name
    CoRefl ty -> "refl " <> renderTypeWith env scopes seen PrecAtom ty
    CoSym inner -> "sym (" <> renderCoercion env scopes seen inner <> ")"
    CoTrans left right -> "trans (" <> renderCoercion env scopes seen left <> ") (" <> renderCoercion env scopes seen right <> ")"
    CoTyConApp name arguments ->
      unwords ("tycon-co" : renderName scopes seen name : map (\argument -> "(" <> renderCoercion env scopes seen argument <> ")") arguments)
    CoAxiom name arguments ->
      unwords ("axiom-co" : renderName scopes seen name : map (\argument -> "@" <> renderTypeWith env scopes seen PrecAtom argument) arguments)

renderLiteral :: ScopeTable -> Set Text -> Literal -> String
renderLiteral scopes seen literal =
  case literal of
    LitInt representation value -> show value <> "#" <> renderName scopes seen (repName representation)
    LitChar representation value -> show value <> "#" <> renderName scopes seen (repName representation)
    LitString value -> show (T.unpack value)
    LitAddr representation value -> show (map (chr . fromIntegral) (BS.unpack value)) <> "#" <> renderName scopes seen (repName representation)

repName :: Type -> Name
repName ty =
  case ty of
    TyCon name -> name
    TyVar name -> name
    _ -> Name "AddrRep" SortDataConstructor (OriginLocal (Unique 0))

renderName :: ScopeTable -> Set Text -> Name -> String
renderName scopes _seen name =
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

renderLocalBinder :: Set Text -> Name -> String
renderLocalBinder seen name =
  T.unpack (nameText name) <> uniqueSuffix True seen name

renderLocalUse :: Name -> String
renderLocalUse name =
  T.unpack (nameText name) <> uniqueSuffix False Set.empty name

uniqueSuffix :: Bool -> Set Text -> Name -> String
uniqueSuffix isBinder seen name =
  case nameOrigin name of
    OriginLocal (Unique unique)
      | unique /= 0 -> "{" <> show unique <> "}"
      | isBinder && nameText name `Set.member` seen -> "{0}"
      | otherwise -> ""
    OriginTop {} -> ""

bindText :: Set Text -> Binder -> Set Text
bindText seen binder = Set.insert (nameText (binderName binder)) seen

paren :: Bool -> String -> String
paren False value = value
paren True value = "(" <> value <> ")"

indentLine :: Int -> String -> String
indentLine count value = replicate (count * 2) ' ' <> value
