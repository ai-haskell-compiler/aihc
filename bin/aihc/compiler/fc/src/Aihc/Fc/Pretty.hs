{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Human-readable System FC text.
--
-- The text leaves out each part of the program that "Aihc.Fc.TextInference"
-- can calculate again, and the parser puts these parts back:
--
-- * A function type is @a → b@ when its representations are the
--   representations of @a@ and @b@. Otherwise it is @FUN \@r1 \@r2 a b@.
-- * A literal is @0 :: t@ when its representation is the representation of
--   @t@. A case alternative is @0@ when the scrutinee type gives it.
-- * An application does not show the argument of an implicit binder when
--   the kind of a later argument gives it. When the kind does not give it,
--   the argument shows as @\@{t}@.
-- * A pattern or lambda binder that its scope does not use shows as @_@.
-- * A case binder that the alternatives do not use is not shown.
-- * The header shows only the scopes that the text uses.
module Aihc.Fc.Pretty
  ( renderProgram,
    renderProgramSections,
    reservedWords,
    globalReferences,
  )
where

import Aihc.Fc.Imports (declReferences, referencesFromImports)
import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Aihc.Fc.TextInference
import Aihc.Resolve (PackageId (..), packageIdText)
import Aihc.Tc.Types (Unique (..))
import Data.ByteString qualified as BS
import Data.Char (chr, isAscii, isPrint, ord)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import Numeric (showHex)
import Prettyprinter (Doc, LayoutOptions (..), PageWidth (..), SimpleDocStream (..), annotate, fillSep, group, hardline, hsep, indent, layoutPretty, line, nest, parens, pretty, punctuate, space, vsep, (<+>))
import Prettyprinter.Render.Text (renderStrict)

data Prec
  = PrecAtom
  | PrecApp
  | PrecFun
  | PrecEq
  | PrecForAll
  deriving (Eq, Ord)

-- | The annotation of a scope prefix in a document: the scope id that the
-- prefix uses. The header shows only the scopes that a prefix uses.
newtype ScopeUse = ScopeUse Int

type FcDoc = Doc ScopeUse

-- | Reverse lookup from a scope's identity to its printed scope id, built
-- once per program instead of re-scanning the scope table for every name.
-- It also holds the inference environment of the current position.
data ScopeIndex = ScopeIndex
  { scopeIds :: Map.Map (PackageId, Text) Int,
    shortNames :: Map.Map Text Name,
    primitiveNames :: Map.Map ForeignCall Name,
    inferEnv :: InferEnv,
    -- | The names that the current declaration uses. A binder that is not
    -- in this set has no use. A binder in this set can still have no use
    -- in its own scope, so the printer then examines the scope.
    usedNames :: Set.Set Name
  }

scopeIndexFromProgram :: Program -> ScopeIndex
scopeIndexFromProgram program =
  ScopeIndex
    { scopeIds = Map.fromList [((entryPackage, entryModule), scopeId) | (scopeId, entryPackage, entryModule) <- scopeEntries (programScopes program)],
      shortNames = globalReferences program,
      primitiveNames = primitiveAliases program,
      inferEnv = inferEnvFromProgram program,
      usedNames = mempty
    }

withTypeBinderScope :: Binder -> ScopeIndex -> ScopeIndex
withTypeBinderScope binder scopes = scopes {inferEnv = withTypeBinder binder (inferEnv scopes)}

withTypeBinderScopes :: [Binder] -> ScopeIndex -> ScopeIndex
withTypeBinderScopes binders scopes = foldl (flip withTypeBinderScope) scopes binders

withTermBinderScope :: Binder -> ScopeIndex -> ScopeIndex
withTermBinderScope binder scopes = scopes {inferEnv = withTermBinder binder (inferEnv scopes)}

withTermBinderScopes :: [Binder] -> ScopeIndex -> ScopeIndex
withTermBinderScopes binders scopes = foldl (flip withTermBinderScope) scopes binders

-- | Give each primitive signature a separate text alias.
--
-- The passes give the binders of a foreign type new names at each copy, so
-- the same primitive can have types that are equal except for the names of
-- their binders. These calls get one alias, and the import list shows the
-- first of them. Thus a primitive has one alias for each type that is
-- really different.
primitiveAliases :: Program -> Map.Map ForeignCall Name
primitiveAliases program = Map.fromList (concatMap aliases (Map.elems grouped))
  where
    calls = Set.toAscList (foldMap declarationCalls (programDecls program))
    grouped = Map.fromListWith (<>) [(nameText (foreignCallName call), [call]) | call <- calls]
    aliases variants =
      [ (call, Name (nameText (foreignCallName representative)) SortValue (OriginLocal (Unique unique)))
      | (unique, classCalls@(representative : _)) <- zip [0 ..] (alphaClasses variants),
        call <- classCalls
      ]
    -- The calls in groups that are equal up to the names of the binders,
    -- in the order of the first call of each group. Each group is sorted.
    alphaClasses variants =
      map (List.sort . snd) (List.sortOn (minimum . snd) (Map.toList (Map.fromListWith (<>) [(canonicalCall call, [call]) | call <- variants])))
    canonicalCall call = call {foreignCallType = canonicalType 0 Map.empty (foreignCallType call)}
    declarationCalls declaration = case declaration of
      DeclVal value -> expressionCalls (valBody value)
      DeclRule value -> expressionCalls (ruleLhs value) <> expressionCalls (ruleRhs value)
      _ -> mempty
    expressionCalls expression = case expression of
      ExApp function argument -> expressionCalls function <> expressionCalls argument
      ExTyApp function _ -> expressionCalls function
      ExLam _ body -> expressionCalls body
      ExTyLam _ body -> expressionCalls body
      ExLet binding body -> expressionCalls (bindRhs binding) <> expressionCalls body
      ExRec bindings body -> foldMap (expressionCalls . bindRhs) bindings <> expressionCalls body
      ExCase scrutinee _ (NE.toList -> alternatives) -> expressionCalls scrutinee <> foldMap (expressionCalls . altRhs) alternatives
      ExAbsurd scrutinee _ -> expressionCalls scrutinee
      ExCast body _ -> expressionCalls body
      ExForeignCall call _ arguments -> (if foreignCallConvention call == Prim then Set.singleton call else mempty) <> foldMap expressionCalls arguments
      _ -> mempty

-- | A type with each binder renamed to its depth. Two types are equal up
-- to the names of their binders when their canonical types are equal.
canonicalType :: Int -> Map.Map Name Name -> Type -> Type
canonicalType depth renaming ty =
  case ty of
    TyVar name -> TyVar (Map.findWithDefault name name renaming)
    TyCon {} -> ty
    TyLit {} -> ty
    TyApp function argument -> TyApp (canonical function) (canonical argument)
    TyFun r1 r2 argument result -> TyFun (canonical r1) (canonical r2) (canonical argument) (canonical result)
    TyEq left right -> TyEq (canonical left) (canonical right)
    TyForAll (Binder name kind) body ->
      -- The empty text is not the text of a source name, so a canonical
      -- binder cannot capture a free variable.
      let name' = Name "" (nameSort name) (OriginLocal (Unique depth))
       in TyForAll (Binder name' (canonical kind)) (canonicalType (depth + 1) (Map.insert name name' renaming) body)
  where
    canonical = canonicalType depth renaming

-- | Short references require one global origin and no local binder conflict.
globalReferences :: Program -> Map.Map Text Name
globalReferences program = Map.mapMaybe unique grouped
  where
    imports = programImports program
    declarations = programDecls program
    introduced = Set.fromList (Map.keys (importHeaders imports) <> Map.keys (importSynonyms imports) <> Map.keys (importAxioms imports) <> Map.keys (importBinders imports) <> concat (Map.elems (importDataCons imports)) <> concatMap declarationNames declarations)
    names = introduced <> referencesFromImports imports <> foldMap declReferences declarations
    grouped = Map.fromListWith Set.union [(printedNameText name, Set.singleton name) | name <- Set.toList names, OriginTop {} <- [nameOrigin name]]
    locals = Set.fromList [nameText name | name <- Map.keys (importBinders imports), OriginLocal {} <- [nameOrigin name]] <> foldMap declarationLocals declarations <> foldMap typeLocals (Map.elems (importHeaders imports) <> Map.elems (importSynonyms imports) <> Map.elems (importBinders imports)) <> foldMap axiomLocals (Map.elems (importAxioms imports))
    unique candidates = case Set.toList candidates of
      [name] | name `Set.member` introduced && printedNameText name `Set.notMember` locals && printedNameText name `notElem` reservedWords -> Just name
      _ -> Nothing
    declarationNames declaration = case declaration of
      DeclType value -> typeName value : map conName (typeCons value)
      DeclSynonym value -> [synName value]
      DeclAxiom value -> [axiomName value]
      DeclVal value -> [valName value]
      DeclRule {} -> []
    declarationLocals declaration = case declaration of
      DeclType value -> foldMap binderLocals (typeBinders value) <> typeLocals (typeResult value) <> foldMap (typeLocals . conType) (typeCons value)
      DeclSynonym value -> foldMap binderLocals (synBinders value) <> typeLocals (synResult value) <> typeLocals (synBody value)
      DeclAxiom value -> axiomLocals value
      DeclVal value -> typeLocals (valType value) <> expressionLocals (valBody value)
      DeclRule value -> foldMap binderLocals (ruleTypeBinders value <> ruleBinders value) <> typeLocals (ruleType value) <> expressionLocals (ruleLhs value) <> expressionLocals (ruleRhs value)

printedNameText :: Name -> Text
printedNameText name = prefix <> nameText name
  where
    prefix = case nameSort name of
      SortTypeConstructor -> "t"
      SortSynonym -> "s"
      SortValue -> "v"
      SortDataConstructor -> "c"
      _ -> ""

binderLocals :: Binder -> Set.Set Text
binderLocals binder = Set.singleton (nameText (binderName binder)) <> typeLocals (binderType binder)

axiomLocals :: AxiomDecl -> Set.Set Text
axiomLocals value = foldMap binderLocals (axiomBinders value) <> typeLocals (axiomLeft value) <> typeLocals (axiomRight value)

typeLocals :: Type -> Set.Set Text
typeLocals ty = case ty of
  TyApp function argument -> typeLocals function <> typeLocals argument
  TyFun r1 r2 argument result -> foldMap typeLocals [r1, r2, argument, result]
  TyForAll binder body -> binderLocals binder <> typeLocals body
  TyEq left right -> typeLocals left <> typeLocals right
  _ -> mempty

expressionLocals :: Expr -> Set.Set Text
expressionLocals expression = case expression of
  ExLit _ ty -> typeLocals ty
  ExApp function argument -> expressionLocals function <> expressionLocals argument
  ExTyApp function argument -> expressionLocals function <> typeLocals argument
  ExLam binder body -> binderLocals binder <> expressionLocals body
  ExTyLam binder body -> binderLocals binder <> expressionLocals body
  ExLet binding body -> bindingLocals binding <> expressionLocals body
  ExRec bindings body -> foldMap bindingLocals bindings <> expressionLocals body
  ExCase scrutinee binder (NE.toList -> alternatives) -> expressionLocals scrutinee <> foldMap binderLocals binder <> foldMap alternativeLocals alternatives
  ExCoercion proof -> coercionLocals proof
  ExAbsurd scrutinee resultType -> expressionLocals scrutinee <> typeLocals resultType
  ExCast body proof -> expressionLocals body <> coercionLocals proof
  ExForeignCall call types arguments -> typeLocals (foreignCallType call) <> foldMap typeLocals types <> foldMap expressionLocals arguments
  _ -> mempty
  where
    bindingLocals binding = binderLocals (bindBinder binding) <> expressionLocals (bindRhs binding)
    alternativeLocals alternative = foldMap binderLocals (altTypeBinders alternative <> altBinders alternative) <> expressionLocals (altRhs alternative)

coercionLocals :: Coercion -> Set.Set Text
coercionLocals proof = case proof of
  CoRefl ty -> typeLocals ty
  CoSym inner -> coercionLocals inner
  CoTrans left right -> coercionLocals left <> coercionLocals right
  CoApp function argument -> coercionLocals function <> coercionLocals argument
  CoNth _ inner -> coercionLocals inner
  CoFun argument result -> coercionLocals argument <> coercionLocals result
  CoForAll binder body -> binderLocals binder <> coercionLocals body
  CoTyConApp _ arguments -> foldMap coercionLocals arguments
  CoAxiom _ arguments -> foldMap typeLocals arguments
  _ -> mempty

renderProgram :: Program -> Text
renderProgram = renderDoc . prettyProgram

renderDoc :: FcDoc -> Text
renderDoc = renderStrict . layoutPretty (LayoutOptions (AvailablePerLine 120 1))

-- | The text of a program in sections: first the header with the scopes and
-- the imports, then one section for each declaration. A declaration section
-- has the name of the declaration, and a rule section has no name. The
-- sections, with a blank line between each two, are the text of
-- 'renderProgram'.
renderProgramSections :: Program -> [(Maybe Name, Text)]
renderProgramSections program =
  [(Nothing, renderDoc (vsep (punctuate hardline header))) | not (null header)]
    <> [(declName decl, renderDoc document) | (decl, document) <- declarations]
  where
    (header, declarations) = programDocuments program
    declName decl = case decl of
      DeclType declaration -> Just (typeName declaration)
      DeclSynonym declaration -> Just (synName declaration)
      DeclAxiom declaration -> Just (axiomName declaration)
      DeclVal declaration -> Just (valName declaration)
      DeclRule _ -> Nothing

prettyProgram :: Program -> FcDoc
prettyProgram program =
  vsep (punctuate hardline (header <> map snd declarations))
  where
    (header, declarations) = programDocuments program

-- | The header documents of a program, and the document of each
-- declaration.
programDocuments :: Program -> ([FcDoc], [(Decl, FcDoc)])
programDocuments program =
  (scopeDocuments <> primitiveDocuments <> importDocuments, declarations)
  where
    scopes = programScopes program
    scopeIndex = scopeIndexFromProgram program
    declarations = [(decl, prettyDecl scopeIndex decl) | decl <- programDecls program]
    usedScopes = foldMap documentScopeUses (primitiveDocuments <> importDocuments <> map snd declarations)
    scopeDocuments =
      case [entry | entry@(scopeId, _, _) <- scopeEntries scopes, scopeId `Set.member` usedScopes] of
        [] -> []
        entries -> [prettyScopes entries]
    importDocuments = prettyImports scopeIndex (programImports program)
    primitiveDocuments = prettyImportGroup "prims" [prettyPrimitiveAlias alias <+> "=" <+> prettyForeignImportDependencies scopeIndex (foreignCallDependencies call) <> prettyTopName scopeIndex (foreignCallName call) <+> "::" <+> prettyTypeWith scopeIndex PrecForAll (foreignCallType call) | (call, alias) <- primitiveImports (primitiveNames scopeIndex)]
    -- One import for each alias: the first call that has the alias.
    primitiveImports names = List.sort [(call, alias) | (alias, call) <- Map.toList (Map.fromListWith min [(alias, call) | (call, alias) <- Map.toList names])]

prettyImports :: ScopeIndex -> Imports -> [FcDoc]
prettyImports scopes imports =
  prettyImportGroup "headers" headerEntries
    <> prettyImportGroup "synonyms" synonymEntries
    <> prettyImportGroup "axioms" axiomEntries
    <> prettyImportGroup "type-binders" typeBinderEntries
    <> prettyImportGroup "value-binders" valueBinderEntries
  where
    headerEntries =
      map (\(name, ty) -> prettyDataCons scopes (Map.lookup name (importDataCons imports)) <> prettyConStrictFields (Map.findWithDefault [] name (importConStrictFields imports)) <> prettyConRepresentation (Map.findWithDefault HeapConstructor name (importConRepresentations imports)) <> prettyTopName scopes name <+> "::" <+> prettyTypeWith scopes PrecForAll ty) (importEntries (importHeaders imports))
    synonymEntries =
      map (\(name, ty) -> prettyTopName scopes name <+> "=" <+> prettyTypeWith scopes PrecForAll ty) (importEntries (importSynonyms imports))
    axiomEntries =
      map (\(name, axiom) -> let inner = withTypeBinderScopes (axiomBinders axiom) scopes in prettyTopName scopes name <> prettyForAllBinders scopes (axiomBinders axiom) <+> ":" <+> prettyTypeWith inner PrecEq (axiomLeft axiom) <+> prettyAxiomRole (axiomRole axiom) <+> prettyTypeWith inner PrecEq (axiomRight axiom)) (importEntries (importAxioms imports))
    typeBinderEntries = map prettyBinderEntry (filter ((== SortTypeVariable) . nameSort . fst) binderEntries)
    valueBinderEntries = map prettyBinderEntry (filter ((/= SortTypeVariable) . nameSort . fst) binderEntries)
    binderEntries = importEntries (importBinders imports)
    prettyBinderEntry (name, ty) = introduction name <+> "::" <+> prettyTypeWith scopes PrecForAll ty
    introduction name = case nameOrigin name of
      OriginTop {} -> prettyTopName scopes name
      OriginLocal {} -> prettyName scopes name

-- | The entries of an import group, with the type names before the value
-- names and each class in text order. The order is a presentation choice:
-- a map orders its keys by every field of the name, which would interleave
-- the classes.
importEntries :: Map.Map Name entry -> [(Name, entry)]
importEntries = List.sortOn (importEntryKey . fst) . Map.toAscList
  where
    importEntryKey name = (nameClass (nameSort name), nameText name, nameOrigin name)

-- | The constructors of an imported data type: @constructors [1.cA, 1.cB]@
-- in front of its header, or nothing when they are not known.
prettyDataCons :: ScopeIndex -> Maybe [Name] -> FcDoc
prettyDataCons scopes =
  maybe mempty (\constructors -> "constructors [" <> hsep (punctuate "," (map (prettyTopName scopes) constructors)) <> "] ")

prettyImportGroup :: FcDoc -> [FcDoc] -> [FcDoc]
prettyImportGroup _ [] = []
prettyImportGroup header entries =
  ["import" <+> header <> hardline <> indent 2 (vsep (punctuate ";" entries))]

prettyScopes :: [(Int, PackageId, Text)] -> FcDoc
prettyScopes = vsep . map prettyScopeEntry

prettyScopeEntry :: (Int, PackageId, Text) -> FcDoc
prettyScopeEntry (scopeId, package, moduleName) =
  "scope" <+> pretty scopeId <+> "=" <+> pretty (show (T.unpack (packageIdText package))) <+> pretty moduleName

-- | The scope ids that the prefixes of a document use. The compact layout
-- drops the annotations, so this uses an unbounded layout.
documentScopeUses :: FcDoc -> Set.Set Int
documentScopeUses = go . layoutPretty (LayoutOptions Unbounded)
  where
    go stream = case stream of
      SFail -> mempty
      SEmpty -> mempty
      SChar _ rest -> go rest
      SText _ _ rest -> go rest
      SLine _ rest -> go rest
      SAnnPush (ScopeUse scopeId) rest -> Set.insert scopeId (go rest)
      SAnnPop rest -> go rest

prettyDecl :: ScopeIndex -> Decl -> FcDoc
prettyDecl outer decl =
  case decl of
    DeclType declaration -> prettyTypeDecl scopes declaration
    DeclSynonym declaration -> prettySynonymDecl scopes declaration
    DeclAxiom declaration -> prettyAxiomDecl scopes declaration
    DeclVal declaration -> prettyValDecl scopes declaration
    DeclRule declaration -> prettyRuleDecl scopes declaration
  where
    scopes = outer {usedNames = declarationUses decl}

prettyVis :: Vis -> FcDoc
prettyVis Pub = "pub "
prettyVis Private = mempty

prettyTypeDecl :: ScopeIndex -> TypeDecl -> FcDoc
prettyTypeDecl scopes declaration =
  prettyVis (typeVis declaration)
    <> "type "
    <> prettyTopName scopes (typeName declaration)
    <> prettyHeaderBinders scopes (typeBinders declaration)
    <> " :: "
    <> prettyTypeWith inner PrecForAll (typeResult declaration)
    <> prettyRoleList (typeRoles declaration)
    <> prettyConstructors inner (typeCons declaration)
  where
    inner = withTypeBinderScopes (typeBinders declaration) scopes

prettyHeaderBinders :: ScopeIndex -> [Binder] -> FcDoc
prettyHeaderBinders scopes binders =
  foldMap (\(binder, before) -> space <> prettyPiBinder (withTypeBinderScopes before scopes) binder) (zip binders (List.inits binders))

prettyConstructors :: ScopeIndex -> [ConDecl] -> FcDoc
prettyConstructors _ [] = " {}"
prettyConstructors scopes constructors =
  " {"
    <> hardline
    <> indent 2 (vsep (punctuate ";" (map (prettyConDecl scopes) constructors)))
    <> hardline
    <> "}"

prettyConDecl :: ScopeIndex -> ConDecl -> FcDoc
prettyConDecl scopes declaration =
  prettyVis (conVis declaration)
    <> prettyConStrictFields (conStrictFields declaration)
    <> prettyConRepresentation (conRepresentation declaration)
    <> prettyTopName scopes (conName declaration)
    <> " :: "
    <> prettyTypeWith scopes PrecForAll (conType declaration)

prettyConStrictFields :: [Int] -> FcDoc
prettyConStrictFields fields
  | null fields = mempty
  | otherwise = "strict [" <> hsep (punctuate "," (map pretty fields)) <> "] "

prettyConRepresentation :: ConRepresentation -> FcDoc
prettyConRepresentation representation = case representation of
  HeapConstructor -> mempty
  UnboxedTupleConstructor -> "unboxed-tuple "
  UnboxedSumConstructor alternative arity -> "unboxed-sum" <+> pretty alternative <+> pretty arity <> space

prettySynonymDecl :: ScopeIndex -> SynonymDecl -> FcDoc
prettySynonymDecl scopes declaration =
  prettyVis (synVis declaration)
    <> "type "
    <> prettyTopName scopes (synName declaration)
    <> prettyHeaderBinders scopes (synBinders declaration)
    <> " :: "
    <> prettyTypeWith inner PrecForAll (synResult declaration)
    <> " ="
    <> hardline
    <> indent 2 (prettyTypeWith inner PrecForAll (synBody declaration))
  where
    inner = withTypeBinderScopes (synBinders declaration) scopes

prettyAxiomDecl :: ScopeIndex -> AxiomDecl -> FcDoc
prettyAxiomDecl scopes declaration =
  prettyVis (axiomVis declaration)
    <> "axiom "
    <> prettyTopName scopes (axiomName declaration)
    <> prettyForAllBinders scopes (axiomBinders declaration)
    <> " : "
    <> prettyTypeWith inner PrecForAll (axiomLeft declaration)
    <+> prettyAxiomRole (axiomRole declaration)
    <+> prettyTypeWith inner PrecForAll (axiomRight declaration)
  where
    inner = withTypeBinderScopes (axiomBinders declaration) scopes

prettyForAllBinders :: ScopeIndex -> [Binder] -> FcDoc
prettyForAllBinders _ [] = mempty
prettyForAllBinders scopes binders =
  space <> hsep [prettyPiBinder (withTypeBinderScopes before scopes) binder | (binder, before) <- zip binders (List.inits binders)]

prettyAxiomRole :: Role -> FcDoc
prettyAxiomRole Nominal = "~N"
prettyAxiomRole Representational = "~R"
prettyAxiomRole Phantom = "~P"

prettyRoleList :: [Role] -> FcDoc
prettyRoleList roles
  | all (== Representational) roles = mempty
  | otherwise = foldMap ((" @" <>) . prettyRoleTag) roles

prettyRoleTag :: Role -> FcDoc
prettyRoleTag Nominal = "N"
prettyRoleTag Representational = "R"
prettyRoleTag Phantom = "P"

prettyValDecl :: ScopeIndex -> ValDecl -> FcDoc
prettyValDecl scopes declaration =
  prettyVis (valVis declaration)
    <> "val "
    <> prettyTopName scopes (valName declaration)
    <> prettyInlineSpec (valInline declaration)
    <> " :: "
    <> prettyTypeWith scopes PrecForAll (valType declaration)
    <> hardline
    <> indent 2 ("= " <> prettyExprWith scopes (valBody declaration))

-- | A rule: @rule "name" [2] Λ(a : k). λ(x : t). lhs = rhs :: type@.
prettyRuleDecl :: ScopeIndex -> RuleDecl -> FcDoc
prettyRuleDecl scopes declaration =
  "rule "
    <> pretty (show (T.unpack (ruleName declaration)))
    <> prettyActivation (ruleActivation declaration)
    <> foldMap (\(binder, before) -> " Λ" <> prettyPiBinder (withTypeBinderScopes before scopes) binder <> ".") (zip typeBinders (List.inits typeBinders))
    <> foldMap (\binder -> " λ" <> prettyPiBinder typeScope binder <> ".") (ruleBinders declaration)
    <> hardline
    <> indent 2 (prettyExprWith inner (ruleLhs declaration))
    <> hardline
    <> " = "
    <> prettyExprWith inner (ruleRhs declaration)
    <> hardline
    <> " :: "
    <> prettyTypeWith inner PrecForAll (ruleType declaration)
  where
    typeBinders = ruleTypeBinders declaration
    typeScope = withTypeBinderScopes typeBinders scopes
    inner = withTermBinderScopes (ruleBinders declaration) typeScope

-- | The inline pragma of a value: @inline [2]@, @noinline@, @inlinable@.
prettyInlineSpec :: InlineSpec -> FcDoc
prettyInlineSpec spec =
  case spec of
    InlineDefault -> mempty
    InlineAlways activation -> " inline" <> prettyActivation activation
    InlineWhenUseful activation -> " inlinable" <> prettyActivation activation
    -- A plain @noinline@ allows no phase, so its activation goes unsaid.
    InlineNever NeverActive -> " noinline"
    InlineNever activation -> " noinline" <> prettyActivation activation

prettyActivation :: RuleActivation -> FcDoc
prettyActivation activation =
  case activation of
    AlwaysActive -> mempty
    ActiveAfter phase -> " [" <> pretty phase <> "]"
    ActiveBefore phase -> " [~" <> pretty phase <> "]"
    NeverActive -> " [~]"

prettyPrimitiveAlias :: Name -> FcDoc
prettyPrimitiveAlias name = pretty (nameText name) <> prettyUniqueSuffix name

-- | The head of a foreign call: @foreign {prim 1.vf :: type}@.
prettyForeignCall :: ScopeIndex -> ForeignCall -> FcDoc
prettyForeignCall scopes call
  | Just alias <- Map.lookup call (primitiveNames scopes) = "prim" <+> prettyPrimitiveAlias alias
  | otherwise =
      "foreign {"
        <> prettyCallingConvention (foreignCallConvention call)
        <> prettyForeignImportDependencies scopes (foreignCallDependencies call)
        <> prettyTopName scopes (foreignCallName call)
        <> " :: "
        <> prettyTypeWith scopes PrecForAll (foreignCallType call)
        <> "}"

prettyForeignImportDependencies :: ScopeIndex -> [ForeignImportDependency] -> FcDoc
prettyForeignImportDependencies _ [] = mempty
prettyForeignImportDependencies scopes dependencies =
  "using ["
    <> hsep (punctuate "," (map prettyDependency dependencies))
    <> "] "
  where
    prettyDependency dependency =
      case dependency of
        ForeignAxiom name -> "axiom" <+> prettyTopName scopes name
        ForeignConstructor name -> "constructor" <+> prettyTopName scopes name

prettyCallingConvention :: CallingConvention -> FcDoc
prettyCallingConvention convention =
  case convention of
    Prim -> "prim "
    CCall specification ->
      "ccall "
        <> prettyCCallTarget (ccallTarget specification)
        <> prettyForeignSafety (ccallSafety specification)
        <> " "
        <> pretty (show (T.unpack (ccallSymbol specification)))
        <> " ["
        <> hsep (punctuate "," (map prettyCAbiType (ccallArgumentTypes specification)))
        <> " → "
        <> prettyCAbiType (ccallResultType specification)
        <> "; "
        <> prettyForeignEffect (ccallEffect specification)
        <> "] "

prettyCCallTarget :: CCallTarget -> FcDoc
prettyCCallTarget target =
  case target of
    CCallFunction -> mempty
    CCallAddress -> "address "
    CCallDynamic -> "dynamic "
    CCallWrapper -> "wrapper "
    CCallFunctionAddress -> "function-address "

prettyCAbiType :: CAbiType -> FcDoc
prettyCAbiType abiType =
  case abiType of
    CAbiInt -> "Int"
    CAbiInt8 -> "Int8"
    CAbiInt16 -> "Int16"
    CAbiInt32 -> "Int32"
    CAbiInt64 -> "Int64"
    CAbiWord -> "Word"
    CAbiWord8 -> "Word8"
    CAbiWord16 -> "Word16"
    CAbiWord32 -> "Word32"
    CAbiWord64 -> "Word64"
    CAbiFloat -> "Float"
    CAbiDouble -> "Double"
    CAbiAddr -> "Addr"
    CAbiVoid -> "Void"

prettyForeignSafety :: ForeignSafety -> FcDoc
prettyForeignSafety safety =
  case safety of
    ForeignUnsafe -> "unsafe"
    ForeignSafe -> "safe"
    ForeignInterruptible -> "interruptible"

prettyForeignEffect :: ForeignEffect -> FcDoc
prettyForeignEffect effect =
  case effect of
    ForeignPure -> "pure"
    ForeignRealWorld -> "real-world"

prettyTypeWith :: ScopeIndex -> Prec -> Type -> FcDoc
prettyTypeWith scopes prec ty =
  case ty of
    TyVar name -> prettyName scopes name
    TyCon name -> prettyName scopes name
    TyLit kindName literal ->
      parenthesize (prec < PrecApp) ("lit" <+> prettyName scopes kindName <+> prettyTyLit literal)
    TyApp {} ->
      let (function, arguments) = typeSpine ty
          params = fromMaybe [] (typeTelescope (inferEnv scopes) function)
       in parenthesize (prec < PrecApp) (hsep (prettyTypeWith scopes PrecAtom function : prettyArguments scopes params arguments))
    TyFun r1 r2 argument result
      | typeRep env argument == Just r1,
        typeRep env result == Just r2 ->
          parenthesize
            (prec < PrecFun)
            (prettyTypeWith scopes PrecApp argument <+> "→" <+> prettyTypeWith scopes PrecFun result)
      | otherwise ->
          parenthesize
            (prec < PrecFun)
            ( "FUN @"
                <> prettyTypeWith scopes PrecAtom r1
                <> " @"
                <> prettyTypeWith scopes PrecAtom r2
                <> space
                <> prettyTypeWith scopes PrecAtom argument
                <> space
                <> prettyTypeWith scopes PrecAtom result
            )
    TyForAll binder body ->
      parenthesize
        (prec < PrecForAll)
        ( "∀"
            <> prettyPiBinder scopes binder
            <> prettyForallTail (withTypeBinderScope binder scopes) body
        )
    TyEq left right ->
      parenthesize (prec < PrecEq) (prettyTypeWith scopes PrecApp left <+> "~" <+> prettyTypeWith scopes PrecApp right)
  where
    env = inferEnv scopes

typeSpine :: Type -> (Type, [Type])
typeSpine = go []
  where
    go arguments ty = case ty of
      TyApp function argument -> go (argument : arguments) function
      _ -> (ty, arguments)

-- | The arguments of a type application. An argument that the inference
-- gives is not shown, and an implicit argument that the inference does not
-- give shows as @\@{t}@.
prettyArguments :: ScopeIndex -> [Param] -> [Type] -> [FcDoc]
prettyArguments scopes params arguments =
  [ case shown of
      ArgumentOmitted -> mempty
      ArgumentImplicit -> "@{" <> prettyTypeWith scopes PrecForAll argument <> "}"
      ArgumentExplicit -> prettyTypeWith scopes PrecAtom argument
  | (shown, argument) <- zip (argumentForms (inferEnv scopes) params arguments) arguments,
    shown /= ArgumentOmitted
  ]

-- | The type arguments of a term application, with their @\@@.
prettyTypeArguments :: ScopeIndex -> [Param] -> [Type] -> [FcDoc]
prettyTypeArguments scopes params arguments =
  [ case shown of
      ArgumentImplicit -> "@{" <> prettyTypeWith scopes PrecForAll argument <> "}"
      _ -> "@" <> prettyTypeWith scopes PrecAtom argument
  | (shown, argument) <- zip (argumentForms (inferEnv scopes) params arguments) arguments,
    shown /= ArgumentOmitted
  ]

data ArgumentForm
  = ArgumentOmitted
  | ArgumentImplicit
  | ArgumentExplicit
  deriving (Eq)

-- | The form of each argument of an application. The printer omits an
-- implicit argument only when the inference gives the same value from the
-- arguments that it shows.
argumentForms :: InferEnv -> [Param] -> [Type] -> [ArgumentForm]
argumentForms env params arguments = zipWith form [0 ..] flags
  where
    count = length arguments
    used = take count params
    flags = take count (implicitFlags params <> repeat False)
    candidates = [index | (index, True) <- zip [0 :: Int ..] flags, index < length used]
    settle omitted =
      let given = [if index `elem` omitted then Nothing else Just argument | (index, argument) <- zip [0 ..] arguments]
          inferred = inferImplicit env used (take (length used) given)
          kept = [index | index <- omitted, (inferred !! index) == Just (arguments !! index)]
       in if kept == omitted then omitted else settle kept
    -- The parser gives the @\@{t}@ arguments to the implicit parameters in
    -- order. Thus in a run of adjacent implicit parameters, all of the
    -- arguments show or none of them shows.
    runs = foldr addToRun [] candidates
    addToRun index groups = case groups of
      (group'@(next : _)) : rest | next == index + 1 -> (index : group') : rest
      _ -> [index] : groups
    whole omitted =
      let mixed = [index | run <- runs, any (`notElem` omitted) run, index <- run]
          reduced = [index | index <- omitted, index `notElem` mixed]
       in if reduced == omitted then omitted else whole (settle reduced)
    final = whole (settle candidates)
    form index implicit
      | index `elem` final = ArgumentOmitted
      | implicit = ArgumentImplicit
      | otherwise = ArgumentExplicit

prettyForallTail :: ScopeIndex -> Type -> FcDoc
prettyForallTail scopes ty =
  case ty of
    TyForAll binder body ->
      space <> prettyPiBinder scopes binder <> prettyForallTail (withTypeBinderScope binder scopes) body
    _ -> ". " <> prettyTypeWith scopes PrecForAll ty

prettyPiBinder :: ScopeIndex -> Binder -> FcDoc
prettyPiBinder scopes binder =
  parens
    ( prettyBinderName (binderName binder)
        <> " : "
        <> prettyTypeWith scopes PrecForAll (binderType binder)
    )

prettyBinderName :: Name -> FcDoc
prettyBinderName name = pretty (nameText name) <> prettyUniqueSuffix name

-- | A binder that its scope does not use shows as @_@, with its type.
prettyUnusedBinder :: ScopeIndex -> Set.Set Name -> Binder -> FcDoc
prettyUnusedBinder scopes scope binder
  | isUnused scopes scope binder = parens ("_ : " <> prettyTypeWith scopes PrecForAll (binderType binder))
  | otherwise = prettyPiBinder scopes binder

-- | True when the scope of the binder does not use it. The scope is the set
-- of free names of the code where the binder is visible. The
-- declaration-wide set is a fast test, and the scope set is calculated only
-- for a name that the declaration uses.
isUnused :: ScopeIndex -> Set.Set Name -> Binder -> Bool
isUnused scopes scope binder =
  name `Set.notMember` usedNames scopes || name `Set.notMember` scope
  where
    name = binderName binder

prettyExprWith :: ScopeIndex -> Expr -> FcDoc
prettyExprWith scopes expr =
  case expr of
    ExVar name -> prettyName scopes name
    ExLit literal ty
      | typeRep (inferEnv scopes) ty == Just (literalRep literal) -> prettyLiteralValue literal <+> "::" <+> prettyTypeWith scopes PrecAtom ty
      | otherwise -> "lit" <+> parens (prettyLiteral scopes literal) <+> "::" <+> prettyTypeWith scopes PrecAtom ty
    ExApp {} -> prettySpine scopes expr
    ExTyApp {} -> prettySpine scopes expr
    ExLam {} -> prettyLambda scopes expr
    ExTyLam {} -> prettyLambda scopes expr
    ExLet bind body ->
      "let " <> prettyBind scopes bind <> ";" <> hardline <> prettyExprWith (withTermBinderScope (bindBinder bind) scopes) body
    ExRec binds body ->
      let inner = withTermBinderScopes (map bindBinder binds) scopes
       in "rec {"
            <> hardline
            <> prettyIndentedItems 2 (map (prettyBind inner) binds)
            <> hardline
            <> "} in"
            <> hardline
            <> prettyExprWith inner body
    -- A case with one alternative prints as a strict pattern binding,
    -- so that its body keeps the indentation of the case.
    ExCase scrutinee binder (alternative NE.:| []) ->
      let inner = caseScope binder
       in "let! "
            <> prettyAltHead inner (scrutineeRep scrutinee) alternative
            <> prettyCaseBinder scopes (alternativeFree alternative) binder
            <> " ="
            <> group (nest 2 (line <> prettyExprWith scopes scrutinee))
            <> ";"
            <> hardline
            <> prettyExprWith (alternativeScope inner alternative) (altRhs alternative)
    ExCase scrutinee binder (NE.toList -> alts) ->
      let inner = caseScope binder
       in "case "
            <> prettyExprWith scopes scrutinee
            <> prettyCaseBinder scopes (foldMap alternativeFree alts) binder
            <> " of {"
            <> hardline
            <> prettyIndentedItems 2 (map (prettyAlt inner (scrutineeRep scrutinee)) alts)
            <> hardline
            <> "}"
    ExAbsurd scrutinee resultType -> "absurd " <> prettyExprWith scopes scrutinee <> " return " <> parens (prettyTypeWith scopes PrecForAll resultType)
    ExCoercion proof -> "coercion " <> parens (prettyCoercion scopes proof)
    ExCast body coercion ->
      prettyExprAtom scopes body <+> "▷" <+> prettyCoercion scopes coercion
    ExForeignCall call types arguments ->
      hsep
        ( prettyForeignCall scopes call
            : prettyTypeArguments scopes (termTelescope (foreignCallType call)) types
              <> map (prettyExprAtom scopes) arguments
        )
  where
    caseScope binder = maybe scopes (`withTermBinderScope` scopes) binder
    scrutineeRep scrutinee = exprType (inferEnv scopes) scrutinee >>= typeRep (inferEnv scopes)

-- | The scope of the right-hand side of an alternative.
alternativeScope :: ScopeIndex -> Alt -> ScopeIndex
alternativeScope scopes alternative =
  withTermBinderScopes (altBinders alternative) (withTypeBinderScopes (altTypeBinders alternative) scopes)

-- | The case binder. A binder that the alternatives do not use is not
-- shown.
prettyCaseBinder :: ScopeIndex -> Set.Set Name -> Maybe Binder -> FcDoc
prettyCaseBinder scopes scope binder = case binder of
  Just named
    | not (isUnused scopes scope named) -> " as " <> prettyPiBinder scopes named
  _ -> mempty

-- | An application: its head, then its arguments. The type arguments that
-- come first use the type of the head.
prettySpine :: ScopeIndex -> Expr -> FcDoc
prettySpine scopes expr =
  hsep (prettyExprAtom scopes function : prettyTypeArguments scopes params leading <> map prettyArgument rest)
  where
    (function, arguments) = expressionSpine expr
    leading = [ty | Left ty <- takeWhile isTypeArgument arguments]
    rest = dropWhile isTypeArgument arguments
    params = maybe [] termTelescope (exprType (inferEnv scopes) function)
    isTypeArgument argument = case argument of
      Left _ -> True
      Right _ -> False
    prettyArgument argument = case argument of
      Left ty -> "@" <> prettyTypeWith scopes PrecAtom ty
      Right value -> prettyExprAtom scopes value

expressionSpine :: Expr -> (Expr, [Either Type Expr])
expressionSpine = go []
  where
    go arguments expression = case expression of
      ExApp function argument -> go (Right argument : arguments) function
      ExTyApp function argument -> go (Left argument : arguments) function
      _ -> (expression, arguments)

-- | Put consecutive lambda binders on one line when they fit.
prettyLambda :: ScopeIndex -> Expr -> FcDoc
prettyLambda scopes expression =
  fillSep (concatMap prettyGroup groups) <> hardline <> indent 2 (prettyExprWith bodyScope body)
  where
    (binders, body) = collect expression
    -- Each binder with the scope that its type uses.
    scoped = zip binders (scanl (flip extend) scopes binders)
    bodyScope = foldl (flip extend) scopes binders
    extend (kind, binder) = if kind then withTypeBinderScope binder else withTermBinderScope binder
    lambdaFree = expressionFree body
    groups = List.groupBy (\((kind, _), _) ((other, _), _) -> kind == other) scoped
    collect expr = case expr of
      ExLam binder rest -> prepend False binder rest
      ExTyLam binder rest -> prepend True binder rest
      _ -> ([], expr)
    prepend kind binder rest =
      let (following, result) = collect rest
       in ((kind, binder) : following, result)
    prettyGroup [] = []
    prettyGroup (((kind, binder), scope) : rest) =
      finish ((if kind then "Λ" else "λ") <> prettyLambdaBinder kind scope binder : map (\((other, laterBinder), laterScope) -> prettyLambdaBinder other laterScope laterBinder) rest)
    prettyLambdaBinder kind scope binder
      | kind = prettyPiBinder scope binder
      | otherwise = prettyUnusedBinder scope lambdaFree binder
    finish [] = []
    finish [document] = [document <> "."]
    finish (document : rest) = document : finish rest

prettyExprAtom :: ScopeIndex -> Expr -> FcDoc
prettyExprAtom scopes expr =
  case expr of
    ExVar {} -> prettyExprWith scopes expr
    ExLit {} -> parens (prettyExprWith scopes expr)
    _ -> parens (prettyExprWith scopes expr)

prettyBind :: ScopeIndex -> Bind -> FcDoc
prettyBind scopes bind =
  prettyBinderName (binderName (bindBinder bind))
    <> " : "
    <> prettyTypeWith scopes PrecForAll (binderType (bindBinder bind))
    <> " ="
    <> hardline
    <> indent 2 (prettyExprWith scopes (bindRhs bind))

prettyAlt :: ScopeIndex -> Maybe Type -> Alt -> FcDoc
prettyAlt scopes scrutineeRep alternative =
  prettyAltHead scopes scrutineeRep alternative
    <> " →"
    <> hardline
    <> indent 2 (prettyExprWith (alternativeScope scopes alternative) (altRhs alternative))

-- | The pattern of an alternative. A literal has no representation when
-- the type of the scrutinee gives it.
prettyAltHead :: ScopeIndex -> Maybe Type -> Alt -> FcDoc
prettyAltHead scopes scrutineeRep alternative =
  case altCon alternative of
    AltDefault -> "_"
    AltLit literal
      | scrutineeRep == Just (literalRep literal) -> prettyLiteralValue literal <> binders
      | otherwise -> prettyLiteral scopes literal <> binders
    AltData name -> prettyName scopes name <> binders
  where
    binders = prettyTypeBinders scopes (altTypeBinders alternative) <> prettyTermBinders (withTypeBinderScopes (altTypeBinders alternative) scopes) (altBinders alternative)
    rhsFree = expressionFree (altRhs alternative)
    prettyTypeBinders scope typeBinders =
      case typeBinders of
        [] -> mempty
        binder : rest ->
          space
            <> "@"
            <> prettyPiBinder scope binder
            <> prettyTypeBinders (withTypeBinderScope binder scope) rest
    prettyTermBinders scope termBinders =
      case termBinders of
        [] -> mempty
        binder : rest ->
          space
            <> prettyUnusedBinder scope rhsFree binder
            <> prettyTermBinders scope rest

prettyIndentedItems :: Int -> [FcDoc] -> FcDoc
prettyIndentedItems _ [] = mempty
prettyIndentedItems amount documents = indent amount (vsep (punctuate ";" documents))

literalRep :: Literal -> Type
literalRep literal = case literal of
  LitInt representation _ -> representation
  LitChar representation _ -> representation
  LitAddr representation _ -> representation

-- | The value of a literal with no representation: @-3@, @'c'@ or
-- @"text"@.
prettyLiteralValue :: Literal -> FcDoc
prettyLiteralValue literal =
  case literal of
    LitInt _ value -> pretty value
    LitChar _ value -> "'" <> pretty (encodeCharLiteral value) <> "'"
    LitAddr _ value -> "\"" <> pretty (concatMap encodeByte (BS.unpack value)) <> "\""

-- | The names that the expressions, types and coercions of a declaration
-- use. The set can contain a name of a binder that has no use, because a
-- binder can have the same name as a binder in a different scope. It never
-- omits a name that has a use.
declarationUses :: Decl -> Set.Set Name
declarationUses declaration = case declaration of
  DeclVal value -> typeUses (valType value) <> expressionUses (valBody value)
  DeclRule value -> typeUses (ruleType value) <> expressionUses (ruleLhs value) <> expressionUses (ruleRhs value)
  _ -> mempty

expressionUses :: Expr -> Set.Set Name
expressionUses expression = case expression of
  ExVar name -> Set.singleton name
  ExLit _ ty -> typeUses ty
  ExApp function argument -> expressionUses function <> expressionUses argument
  ExTyApp function argument -> expressionUses function <> typeUses argument
  ExLam binder body -> binderUses binder <> expressionUses body
  ExTyLam binder body -> binderUses binder <> expressionUses body
  ExLet binding body -> bindingUses binding <> expressionUses body
  ExRec bindings body -> foldMap bindingUses bindings <> expressionUses body
  ExCase scrutinee binder (NE.toList -> alternatives) -> expressionUses scrutinee <> foldMap binderUses binder <> foldMap alternativeUses alternatives
  ExAbsurd scrutinee resultType -> expressionUses scrutinee <> typeUses resultType
  ExCast body proof -> expressionUses body <> coercionUses proof
  ExCoercion proof -> coercionUses proof
  ExForeignCall call types arguments -> typeUses (foreignCallType call) <> foldMap typeUses types <> foldMap expressionUses arguments
  where
    bindingUses binding = binderUses (bindBinder binding) <> expressionUses (bindRhs binding)
    alternativeUses alternative = foldMap binderUses (altTypeBinders alternative <> altBinders alternative) <> expressionUses (altRhs alternative)

binderUses :: Binder -> Set.Set Name
binderUses = typeUses . binderType

typeUses :: Type -> Set.Set Name
typeUses ty = case ty of
  TyVar name -> Set.singleton name
  TyApp function argument -> typeUses function <> typeUses argument
  TyFun r1 r2 argument result -> foldMap typeUses [r1, r2, argument, result]
  TyForAll binder body -> binderUses binder <> typeUses body
  TyEq left right -> typeUses left <> typeUses right
  _ -> mempty

coercionUses :: Coercion -> Set.Set Name
coercionUses proof = case proof of
  CoVar name -> Set.singleton name
  CoRefl ty -> typeUses ty
  CoSym inner -> coercionUses inner
  CoTrans left right -> coercionUses left <> coercionUses right
  CoApp function argument -> coercionUses function <> coercionUses argument
  CoNth _ inner -> coercionUses inner
  CoFun argument result -> coercionUses argument <> coercionUses result
  CoForAll binder body -> binderUses binder <> coercionUses body
  CoTyConApp _ arguments -> foldMap coercionUses arguments
  CoAxiom _ arguments -> foldMap typeUses arguments

-- | The free names of an expression: its free term, type and coercion
-- variables.
expressionFree :: Expr -> Set.Set Name
expressionFree expression = case expression of
  ExVar name -> Set.singleton name
  ExLit _ ty -> typeFree ty
  ExApp function argument -> expressionFree function <> expressionFree argument
  ExTyApp function argument -> expressionFree function <> typeFree argument
  ExLam binder body -> binderScope binder (expressionFree body)
  ExTyLam binder body -> binderScope binder (expressionFree body)
  ExLet binding body -> expressionFree (bindRhs binding) <> binderScope (bindBinder binding) (expressionFree body)
  ExRec bindings body ->
    foldr (binderScope . bindBinder) (foldMap (expressionFree . bindRhs) bindings <> expressionFree body) bindings
  ExCase scrutinee binder (NE.toList -> alternatives) ->
    expressionFree scrutinee <> foldr binderScope (foldMap alternativeFree alternatives) binder
  ExAbsurd scrutinee resultType -> expressionFree scrutinee <> typeFree resultType
  ExCast body proof -> expressionFree body <> coercionFree proof
  ExCoercion proof -> coercionFree proof
  ExForeignCall call types arguments -> typeFree (foreignCallType call) <> foldMap typeFree types <> foldMap expressionFree arguments

alternativeFree :: Alt -> Set.Set Name
alternativeFree alternative =
  foldr binderScope (expressionFree (altRhs alternative)) (altTypeBinders alternative <> altBinders alternative)

-- | The free names of a scope with a binder: the names of the scope other
-- than the binder, and the free names of the binder type.
binderScope :: Binder -> Set.Set Name -> Set.Set Name
binderScope binder free = Set.delete (binderName binder) free <> typeFree (binderType binder)

typeFree :: Type -> Set.Set Name
typeFree ty = case ty of
  TyVar name -> Set.singleton name
  TyApp function argument -> typeFree function <> typeFree argument
  TyFun r1 r2 argument result -> foldMap typeFree [r1, r2, argument, result]
  TyForAll binder body -> binderScope binder (typeFree body)
  TyEq left right -> typeFree left <> typeFree right
  _ -> mempty

coercionFree :: Coercion -> Set.Set Name
coercionFree proof = case proof of
  CoVar name -> Set.singleton name
  CoRefl ty -> typeFree ty
  CoSym inner -> coercionFree inner
  CoTrans left right -> coercionFree left <> coercionFree right
  CoApp function argument -> coercionFree function <> coercionFree argument
  CoNth _ inner -> coercionFree inner
  CoFun argument result -> coercionFree argument <> coercionFree result
  CoForAll binder body -> binderScope binder (coercionFree body)
  CoTyConApp _ arguments -> foldMap coercionFree arguments
  CoAxiom _ arguments -> foldMap typeFree arguments

prettyCoercion :: ScopeIndex -> Coercion -> FcDoc
prettyCoercion scopes coercion =
  case coercion of
    CoVar name -> prettyName scopes name
    CoRefl ty -> "refl " <> prettyTypeWith scopes PrecAtom ty
    CoSym inner -> "sym " <> parens (prettyCoercion scopes inner)
    CoTrans left right -> "trans " <> parens (prettyCoercion scopes left) <+> parens (prettyCoercion scopes right)
    CoApp function argument -> "app-co " <> parens (prettyCoercion scopes function) <+> parens (prettyCoercion scopes argument)
    CoNth index proof -> "nth-co " <> pretty index <+> parens (prettyCoercion scopes proof)
    CoFun domain range -> "fun-co " <> parens (prettyCoercion scopes domain) <+> parens (prettyCoercion scopes range)
    CoForAll binder body -> "forall-co " <> prettyPiBinder scopes binder <+> parens (prettyCoercion (withTypeBinderScope binder scopes) body)
    CoTyConApp name arguments ->
      hsep ("tycon-co" : prettyName scopes name : map (parens . prettyCoercion scopes) arguments)
    CoAxiom name arguments ->
      hsep ("axiom-co" : prettyName scopes name : map (("@" <>) . prettyTypeWith scopes PrecAtom) arguments)

-- | A type-level literal, in the same spellings the term-level literals
-- use so that one escaping rule covers both.
prettyTyLit :: TyLit -> FcDoc
prettyTyLit literal =
  case literal of
    TyLitNat value -> pretty value
    TyLitSymbol value -> "\"" <> pretty (concatMap encodeStringCharacter (T.unpack value)) <> "\""
    TyLitChar value -> "'" <> pretty (encodeCharLiteral value) <> "'"

-- | A character inside a symbol literal. A double quote is escaped there
-- and a single quote is not, the other way round from a character
-- literal.
encodeStringCharacter :: Char -> String
encodeStringCharacter character
  | character == '"' = "\\\""
  | character == '\\' = "\\\\"
  | character == '\n' = "\\n"
  | isPrint character = [character]
  | otherwise = "\\x{" <> showHex (ord character) "" <> "}"

prettyLiteral :: ScopeIndex -> Literal -> FcDoc
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

prettyName :: ScopeIndex -> Name -> FcDoc
prettyName scopes name =
  case nameOrigin name of
    OriginLocal (Unique 0)
      | Map.member (nameText name) (shortNames scopes) -> pretty (nameText name) <> "{0}"
    OriginLocal {} -> pretty (nameText name) <> prettyUniqueSuffix name
    OriginTop {}
      | Map.lookup (printedNameText name) (shortNames scopes) == Just name -> prettyPrintedName name
      | otherwise -> prettyTopName scopes name

prettyTopName :: ScopeIndex -> Name -> FcDoc
prettyTopName scopes name =
  case nameOrigin name of
    OriginTop package moduleName ->
      prettyScopePrefix scopes package moduleName <> prettyPrintedName name
    OriginLocal {} ->
      prettyPrintedName name

prettyScopePrefix :: ScopeIndex -> PackageId -> Text -> FcDoc
prettyScopePrefix scopes package moduleName =
  case lookupScopeId scopes package moduleName of
    Just scopeId -> annotate (ScopeUse scopeId) (pretty scopeId <> ".")
    Nothing -> error ("missing System FC scope for " <> show (packageIdText package, moduleName))

lookupScopeId :: ScopeIndex -> PackageId -> Text -> Maybe Int
lookupScopeId index package moduleName = Map.lookup (package, moduleName) (scopeIds index)

-- | A top name prints behind a letter that says its sort, so the parser
-- reads back the same name: @t@ for a type constructor, @s@ for a synonym,
-- @v@ for a value, and @c@ for a data constructor. An axiom name starts
-- with @$ax$@ and a type variable is local, so neither needs a letter.
prettyPrintedName :: Name -> FcDoc
prettyPrintedName name =
  case nameSort name of
    SortTypeConstructor -> "t" <> prettyRawPrinted (nameText name)
    SortSynonym -> "s" <> prettyRawPrinted (nameText name)
    SortValue -> "v" <> prettyRawPrinted (nameText name)
    SortDataConstructor -> "c" <> prettyRawPrinted (nameText name)
    SortAxiom -> pretty (nameText name)
    SortTypeVariable -> pretty (nameText name)

prettyRawPrinted :: Text -> FcDoc
prettyRawPrinted = pretty

-- | A local name prints its unique when it is not zero, and always when
-- the name is a reserved word: the suffix is what tells the parser that
-- @as{0}@ is a name and not a keyword.
prettyUniqueSuffix :: Name -> FcDoc
prettyUniqueSuffix name =
  case nameOrigin name of
    OriginLocal (Unique unique)
      | unique /= 0 || nameText name `elem` reservedWords -> "{" <> pretty unique <> "}"
      | otherwise -> mempty
    OriginTop {} -> mempty

-- | The words the printed form reserves. A top name prints behind its class
-- prefix and a local name prints with its unique, so both stay apart from
-- these.
reservedWords :: [Text]
reservedWords =
  [ "pub",
    "val",
    "rule",
    "inline",
    "inlinable",
    "noinline",
    "type",
    "axiom",
    "foreign",
    "import",
    "prim",
    "prims",
    "module",
    "where",
    "let",
    "rec",
    "in",
    "case",
    "absurd",
    "return",
    "as",
    "return",
    "of",
    "FUN",
    "lit",
    "refl",
    "sym",
    "trans",
    "tycon-co",
    "axiom-co"
  ]

parenthesize :: Bool -> FcDoc -> FcDoc
parenthesize False value = value
parenthesize True value = parens value
