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
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import Numeric (showHex)
import Prettyprinter (Doc, defaultLayoutOptions, hardline, hsep, indent, layoutPretty, parens, pretty, punctuate, space, vsep, (<+>))
import Prettyprinter.Render.String (renderString)

data Prec
  = PrecAtom
  | PrecApp
  | PrecFun
  | PrecEq
  | PrecForAll
  deriving (Eq, Ord)

renderProgram :: Program -> String
renderProgram = renderDocument . prettyProgram

prettyProgram :: Program -> Doc ann
prettyProgram program =
  vsep (punctuate hardline documents)
  where
    scopes = programScopes program
    env = typeEnvFromProgram program
    scopeDocuments =
      case scopeEntries scopes of
        [] -> []
        entries -> [prettyScopes entries]
    importDocuments = prettyImports env scopes (programImports program)
    documents = scopeDocuments <> importDocuments <> map (prettyDecl env scopes) (programDecls program)

prettyImports :: TypeEnv -> ScopeTable -> Imports -> [Doc ann]
prettyImports env scopes imports =
  map (\(name, ty) -> "import header" <+> prettyTopName scopes name <+> "::" <+> prettyTypeWith env scopes PrecForAll ty) (Map.toAscList (importHeaders imports))
    <> map (\(name, ty) -> "import synonym" <+> prettyTopName scopes name <+> "=" <+> prettyTypeWith env scopes PrecForAll ty) (Map.toAscList (importSynonyms imports))
    <> map (\(name, axiom) -> "import axiom" <+> prettyTopName scopes name <> prettyForAllBinders env scopes (axiomBinders axiom) <+> ":" <+> prettyTypeWith env scopes PrecEq (axiomLeft axiom) <+> prettyAxiomRole (axiomRole axiom) <+> prettyTypeWith env scopes PrecEq (axiomRight axiom)) (Map.toAscList (importAxioms imports))
    <> map (\(name, ty) -> "import" <+> prettyBinderImportSort name <+> prettyName scopes name <+> "::" <+> prettyTypeWith env scopes PrecForAll ty) (Map.toAscList (importBinders imports))

prettyBinderImportSort :: Name -> Doc ann
prettyBinderImportSort name =
  case nameSort name of
    SortTypeVariable -> "type-binder"
    _ -> "value-binder"

prettyScopes :: [(Int, PackageId, Text)] -> Doc ann
prettyScopes = vsep . map prettyScopeEntry

prettyScopeEntry :: (Int, PackageId, Text) -> Doc ann
prettyScopeEntry (scopeId, package, moduleName) =
  "scope" <+> pretty scopeId <+> "=" <+> pretty (show (T.unpack (packageIdText package))) <+> pretty moduleName

prettyDecl :: TypeEnv -> ScopeTable -> Decl -> Doc ann
prettyDecl env scopes decl =
  case decl of
    DeclType declaration -> prettyTypeDecl env scopes declaration
    DeclSynonym declaration -> prettySynonymDecl env scopes declaration
    DeclAxiom declaration -> prettyAxiomDecl env scopes declaration
    DeclVal declaration -> prettyValDecl env scopes declaration
    DeclForeignImport declaration -> prettyForeignImportDecl env scopes declaration

prettyVis :: Vis -> Doc ann
prettyVis Pub = "pub "
prettyVis Private = mempty

prettyTypeDecl :: TypeEnv -> ScopeTable -> TypeDecl -> Doc ann
prettyTypeDecl env scopes declaration =
  prettyVis (typeVis declaration)
    <> "type "
    <> prettyTopName scopes (typeName declaration)
    <> prettyHeaderBinders env scopes (typeBinders declaration)
    <> " :: "
    <> prettyTypeWith (headerBinderEnv env (typeBinders declaration)) scopes PrecForAll (typeResult declaration)
    <> prettyRoleList (typeRoles declaration)
    <> prettyConstructors env scopes (typeCons declaration)

prettyHeaderBinders :: TypeEnv -> ScopeTable -> [Binder] -> Doc ann
prettyHeaderBinders env scopes =
  foldMap ((space <>) . prettyPiBinder env scopes)

prettyConstructors :: TypeEnv -> ScopeTable -> [ConDecl] -> Doc ann
prettyConstructors _ _ [] = " {}"
prettyConstructors env scopes constructors =
  " {"
    <> hardline
    <> indent 4 (vsep (punctuate ";" (map (prettyConDecl env scopes) constructors)))
    <> hardline
    <> "}"

prettyConDecl :: TypeEnv -> ScopeTable -> ConDecl -> Doc ann
prettyConDecl env scopes declaration =
  prettyVis (conVis declaration)
    <> prettyTopName scopes (conName declaration)
    <> " :: "
    <> prettyTypeWith env scopes PrecForAll (conType declaration)

prettySynonymDecl :: TypeEnv -> ScopeTable -> SynonymDecl -> Doc ann
prettySynonymDecl env scopes declaration =
  prettyVis (synVis declaration)
    <> "type "
    <> prettyTopName scopes (synName declaration)
    <> prettyHeaderBinders env scopes (synBinders declaration)
    <> " :: "
    <> prettyTypeWith binderEnv scopes PrecForAll (synResult declaration)
    <> " ="
    <> hardline
    <> indent 1 (prettyTypeWith binderEnv scopes PrecForAll (synBody declaration))
  where
    binderEnv = headerBinderEnv env (synBinders declaration)

prettyAxiomDecl :: TypeEnv -> ScopeTable -> AxiomDecl -> Doc ann
prettyAxiomDecl env scopes declaration =
  prettyVis (axiomVis declaration)
    <> "axiom "
    <> prettyTopName scopes (axiomName declaration)
    <> prettyForAllBinders env scopes (axiomBinders declaration)
    <> " : "
    <> prettyTypeWith binderEnv scopes PrecForAll (axiomLeft declaration)
    <+> prettyAxiomRole (axiomRole declaration)
    <+> prettyTypeWith binderEnv scopes PrecForAll (axiomRight declaration)
  where
    binderEnv = headerBinderEnv env (axiomBinders declaration)

prettyForAllBinders :: TypeEnv -> ScopeTable -> [Binder] -> Doc ann
prettyForAllBinders _ _ [] = mempty
prettyForAllBinders env scopes binders =
  space <> hsep (map (prettyPiBinder env scopes) binders)

prettyAxiomRole :: Role -> Doc ann
prettyAxiomRole Nominal = "~N"
prettyAxiomRole Representational = "~R"
prettyAxiomRole Phantom = "~P"

prettyRoleList :: [Role] -> Doc ann
prettyRoleList roles
  | all (== Representational) roles = mempty
  | otherwise = foldMap ((" @" <>) . prettyRoleTag) roles

prettyRoleTag :: Role -> Doc ann
prettyRoleTag Nominal = "N"
prettyRoleTag Representational = "R"
prettyRoleTag Phantom = "P"

prettyValDecl :: TypeEnv -> ScopeTable -> ValDecl -> Doc ann
prettyValDecl env scopes declaration =
  prettyVis (valVis declaration)
    <> "val "
    <> prettyTopName scopes (valName declaration)
    <> " :: "
    <> prettyTypeWith env scopes PrecForAll (valType declaration)
    <> hardline
    <> " = "
    <> prettyExprWith env scopes (valBody declaration)

prettyForeignImportDecl :: TypeEnv -> ScopeTable -> ForeignImportDecl -> Doc ann
prettyForeignImportDecl env scopes declaration =
  prettyVis (foreignImportVis declaration)
    <> "foreign import "
    <> prettyCallingConvention (foreignImportCallingConvention declaration)
    <> prettyTopName scopes (foreignImportName declaration)
    <> " :: "
    <> prettyTypeWith env scopes PrecForAll (foreignImportType declaration)

prettyCallingConvention :: CallingConvention -> Doc ann
prettyCallingConvention convention =
  case convention of
    Prim -> "prim "
    CCall specification ->
      "ccall unsafe "
        <> pretty (show (T.unpack (ccallSymbol specification)))
        <> " ["
        <> hsep (punctuate "," (map prettyCAbiType (ccallArgumentTypes specification)))
        <> " → "
        <> prettyCAbiType (ccallResultType specification)
        <> "; "
        <> prettyForeignEffect (ccallEffect specification)
        <> "] "

prettyCAbiType :: CAbiType -> Doc ann
prettyCAbiType abiType =
  case abiType of
    CAbiInt -> "Int"
    CAbiInt32 -> "Int32"
    CAbiWord64 -> "Word64"
    CAbiAddr -> "Addr"

prettyForeignEffect :: ForeignEffect -> Doc ann
prettyForeignEffect effect =
  case effect of
    ForeignPure -> "pure"
    ForeignRealWorld -> "real-world"

renderType :: Program -> Type -> String
renderType program =
  renderDocument . prettyTypeWith (typeEnvFromProgram program) (programScopes program) PrecForAll

prettyTypeWith :: TypeEnv -> ScopeTable -> Prec -> Type -> Doc ann
prettyTypeWith env scopes prec ty =
  case ty of
    TyVar name -> prettyName scopes name
    TyCon name -> prettyName scopes name
    TyApp function argument ->
      parenthesize (prec < PrecApp) (prettyTypeWith env scopes PrecApp function <+> prettyTypeWith env scopes PrecAtom argument)
    TyFun r1 r2 argument result
      | Just scopeId <- liftedArrowScope scopes r1 r2 ->
          parenthesize
            (prec < PrecFun)
            (prettyTypeWith env scopes PrecApp argument <+> (pretty scopeId <> ".→") <+> prettyTypeWith env scopes PrecFun result)
      | otherwise ->
          parenthesize
            (prec < PrecFun)
            ( "FUN @"
                <> prettyTypeWith env scopes PrecAtom r1
                <> " @"
                <> prettyTypeWith env scopes PrecAtom r2
                <> space
                <> prettyTypeWith env scopes PrecAtom argument
                <> space
                <> prettyTypeWith env scopes PrecAtom result
            )
    TyForAll binder body ->
      let env' = extendPrettyEnv env binder
       in parenthesize
            (prec < PrecForAll)
            ( "∀"
                <> prettyPiBinder env scopes binder
                <> prettyForallTail env' scopes body
            )
    TyEq left right ->
      parenthesize (prec < PrecEq) (prettyTypeWith env scopes PrecApp left <+> "~" <+> prettyTypeWith env scopes PrecApp right)

liftedArrowScope :: ScopeTable -> Type -> Type -> Maybe Int
liftedArrowScope scopes left right =
  case (left, right) of
    (TyCon leftName, TyCon rightName)
      | leftName == rightName,
        nameText leftName == "LiftedRep",
        OriginTop package moduleName <- nameOrigin leftName ->
          lookupScopeId scopes package moduleName
    _ -> Nothing

prettyForallTail :: TypeEnv -> ScopeTable -> Type -> Doc ann
prettyForallTail env scopes ty =
  case ty of
    TyForAll binder body ->
      space <> prettyPiBinder env scopes binder <> prettyForallTail (extendPrettyEnv env binder) scopes body
    _ -> ". " <> prettyTypeWith env scopes PrecForAll ty

prettyPiBinder :: TypeEnv -> ScopeTable -> Binder -> Doc ann
prettyPiBinder env scopes binder =
  parens
    ( prettyLocalBinder (binderName binder)
        <> " : "
        <> prettyTypeWith env scopes PrecForAll (binderType binder)
    )

extendPrettyEnv :: TypeEnv -> Binder -> TypeEnv
extendPrettyEnv env binder =
  env {teBinders = Map.insert (binderName binder) (binderType binder) (teBinders env)}

headerBinderEnv :: TypeEnv -> [Binder] -> TypeEnv
headerBinderEnv = foldl (\env binder -> env {teBinders = Map.insert (binderName binder) (binderType binder) (teBinders env)})

renderExpr :: Program -> Expr -> String
renderExpr program =
  renderDocument . prettyExprWith (typeEnvFromProgram program) (programScopes program)

prettyExprWith :: TypeEnv -> ScopeTable -> Expr -> Doc ann
prettyExprWith env scopes expr =
  case expr of
    ExVar name -> prettyName scopes name
    ExLit literal -> prettyLiteral scopes literal
    ExApp function argument ->
      prettyApp env scopes function <+> prettyExprAtom env scopes argument
    ExTyApp function argument ->
      prettyApp env scopes function <+> ("@" <> prettyTypeWith env scopes PrecAtom argument)
    ExLam binder body ->
      "λ" <> prettyPiBinder env scopes binder <> "." <> hardline <> indent 2 (prettyExprWith (extendPrettyEnv env binder) scopes body)
    ExTyLam binder body ->
      "Λ" <> prettyPiBinder env scopes binder <> "." <> hardline <> indent 2 (prettyExprWith (extendPrettyEnv env binder) scopes body)
    ExLet bind body ->
      "let {"
        <> hardline
        <> indent 4 (prettyBind env scopes bind)
        <> hardline
        <> "} in"
        <> hardline
        <> indent 4 (prettyExprWith (extendPrettyEnv env (bindBinder bind)) scopes body)
    ExRec binds body ->
      "rec {"
        <> hardline
        <> prettyIndentedItems 4 (map (prettyBind recEnv scopes) binds)
        <> hardline
        <> "} in"
        <> hardline
        <> indent 4 (prettyExprWith recEnv scopes body)
      where
        recEnv = foldl extendPrettyEnv env (map bindBinder binds)
    ExCase scrutinee binder resultType alts ->
      "case "
        <> prettyExprWith env scopes scrutinee
        <> " as "
        <> prettyPiBinder env scopes binder
        <> " return "
        <> parens (prettyTypeWith env scopes PrecForAll resultType)
        <> " of {"
        <> hardline
        <> prettyIndentedItems 4 (map (prettyAlt (extendPrettyEnv env binder) scopes) alts)
        <> hardline
        <> "}"
    ExCast body coercion ->
      prettyExprAtom env scopes body <+> "▷" <+> prettyCoercion env scopes coercion

prettyApp :: TypeEnv -> ScopeTable -> Expr -> Doc ann
prettyApp env scopes expr =
  case expr of
    ExApp {} -> prettyExprWith env scopes expr
    ExTyApp {} -> prettyExprWith env scopes expr
    _ -> prettyExprAtom env scopes expr

prettyExprAtom :: TypeEnv -> ScopeTable -> Expr -> Doc ann
prettyExprAtom env scopes expr =
  case expr of
    ExVar {} -> prettyExprWith env scopes expr
    ExLit {} -> prettyExprWith env scopes expr
    _ -> parens (prettyExprWith env scopes expr)

prettyBind :: TypeEnv -> ScopeTable -> Bind -> Doc ann
prettyBind env scopes bind =
  prettyLocalBinder (binderName (bindBinder bind))
    <> " : "
    <> prettyTypeWith env scopes PrecForAll (binderType (bindBinder bind))
    <> " ="
    <> hardline
    <> indent 4 (prettyExprWith env scopes (bindRhs bind))

prettyAlt :: TypeEnv -> ScopeTable -> Alt -> Doc ann
prettyAlt env scopes alternative =
  prettyAltHead env scopes alternative
    <> " →"
    <> hardline
    <> indent 4 (prettyExprWith rhsEnv scopes (altRhs alternative))
  where
    rhsEnv = foldl extendPrettyEnv env (altTypeBinders alternative <> altBinders alternative)

prettyAltHead :: TypeEnv -> ScopeTable -> Alt -> Doc ann
prettyAltHead env scopes alternative =
  case altCon alternative of
    AltDefault -> "_"
    AltLit literal -> prettyLiteral scopes literal <> prettyTypeBinders env (altTypeBinders alternative) <> prettyTermBinders typeEnv (altBinders alternative)
    AltData name -> prettyName scopes name <> prettyTypeBinders env (altTypeBinders alternative) <> prettyTermBinders typeEnv (altBinders alternative)
  where
    typeEnv = foldl extendPrettyEnv env (altTypeBinders alternative)
    prettyTypeBinders current binders =
      case binders of
        [] -> mempty
        binder : rest ->
          space
            <> "@"
            <> prettyPiBinder current scopes binder
            <> prettyTypeBinders (extendPrettyEnv current binder) rest
    prettyTermBinders current binders =
      case binders of
        [] -> mempty
        binder : rest ->
          space
            <> prettyPiBinder current scopes binder
            <> prettyTermBinders (extendPrettyEnv current binder) rest

prettyIndentedItems :: Int -> [Doc ann] -> Doc ann
prettyIndentedItems _ [] = mempty
prettyIndentedItems amount documents = indent amount (vsep (punctuate ";" documents))

prettyCoercion :: TypeEnv -> ScopeTable -> Coercion -> Doc ann
prettyCoercion env scopes coercion =
  case coercion of
    CoVar name -> prettyName scopes name
    CoRefl ty -> "refl " <> prettyTypeWith env scopes PrecAtom ty
    CoSym inner -> "sym " <> parens (prettyCoercion env scopes inner)
    CoTrans left right -> "trans " <> parens (prettyCoercion env scopes left) <+> parens (prettyCoercion env scopes right)
    CoTyConApp name arguments ->
      hsep ("tycon-co" : prettyName scopes name : map (parens . prettyCoercion env scopes) arguments)
    CoAxiom name arguments ->
      hsep ("axiom-co" : prettyName scopes name : map (("@" <>) . prettyTypeWith env scopes PrecAtom) arguments)

prettyLiteral :: ScopeTable -> Literal -> Doc ann
prettyLiteral scopes literal =
  case literal of
    LitInt representation value -> pretty value <> "#" <> prettyName scopes (repName representation)
    LitChar representation value -> "'" <> pretty (encodeCharLiteral value) <> "'#" <> prettyName scopes (repName representation)
    LitAddr representation value -> "\"" <> pretty (concatMap encodeByte (BS.unpack value)) <> "\"#" <> prettyName scopes (repName representation)

encodeCharLiteral :: Char -> String
encodeCharLiteral character
  | character == '\'' = "\\'"
  | character == '\\' = "\\\\"
  | character == '\n' = "\\n"
  | isPrint character = [character]
  | otherwise = "\\x{" <> showHex (ord character) "" <> "}"

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

prettyName :: ScopeTable -> Name -> Doc ann
prettyName scopes name =
  case nameOrigin name of
    OriginLocal {} -> prettyLocalUse name
    OriginTop {} -> prettyTopName scopes name

prettyTopName :: ScopeTable -> Name -> Doc ann
prettyTopName scopes name =
  case nameOrigin name of
    OriginTop package moduleName ->
      prettyScopePrefix scopes package moduleName <> prettyPrintedName name
    OriginLocal {} ->
      prettyPrintedName name

prettyScopePrefix :: ScopeTable -> PackageId -> Text -> Doc ann
prettyScopePrefix scopes package moduleName =
  case lookupScopeId scopes package moduleName of
    Just scopeId -> pretty scopeId <> "."
    Nothing -> error ("missing System FC 2 scope for " <> show (packageIdText package, moduleName))

lookupScopeId :: ScopeTable -> PackageId -> Text -> Maybe Int
lookupScopeId table package moduleName =
  lookup (package, moduleName) [((entryPackage, entryModule), scopeId) | (scopeId, entryPackage, entryModule) <- scopeEntries table]

prettyPrintedName :: Name -> Doc ann
prettyPrintedName name =
  case nameClass (nameSort name) of
    NameClassType -> "t" <> prettyRawPrinted (nameText name)
    NameClassValue -> "v" <> prettyRawPrinted (nameText name)
    NameClassAxiom -> pretty (nameText name)
    NameClassTypeVar -> pretty (nameText name)

prettyRawPrinted :: Text -> Doc ann
prettyRawPrinted = pretty

prettyLocalBinder :: Name -> Doc ann
prettyLocalBinder name =
  pretty (nameText name) <> prettyUniqueSuffix name

prettyLocalUse :: Name -> Doc ann
prettyLocalUse name =
  pretty (nameText name) <> prettyUniqueSuffix name

prettyUniqueSuffix :: Name -> Doc ann
prettyUniqueSuffix name =
  case nameOrigin name of
    OriginLocal (Unique unique)
      | unique /= 0 -> "{" <> pretty unique <> "}"
      | otherwise -> mempty
    OriginTop {} -> mempty

parenthesize :: Bool -> Doc ann -> Doc ann
parenthesize False value = value
parenthesize True value = parens value

renderDocument :: Doc ann -> String
renderDocument = renderString . layoutPretty defaultLayoutOptions
