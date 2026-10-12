{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Human-readable System FC text.
--
-- The printer uses only the forms that "Aihc.Fc.Parser" reads. When the
-- format has two forms for one thing, it uses the shorter form:
--
-- * The header has only the scopes that the text uses.
-- * A lambda or pattern binder that its scope does not use is @_@.
-- * A case binder that the alternatives do not use is not shown.
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
import Aihc.Resolve (PackageId (..), packageIdText)
import Aihc.Tc.Types (Unique (..))
import Data.ByteString qualified as BS
import Data.Char (chr, isAscii, isPrint, ord)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
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
data ScopeIndex = ScopeIndex
  { scopeIds :: Map.Map (PackageId, Text) Int,
    shortNames :: Map.Map Text Name,
    primitiveNames :: Map.Map ForeignCall Name
  }

scopeIndexFromProgram :: Program -> ScopeIndex
scopeIndexFromProgram program =
  ScopeIndex
    (Map.fromList [((entryPackage, entryModule), scopeId) | (scopeId, entryPackage, entryModule) <- scopeEntries (programScopes program)])
    (globalReferences program)
    (primitiveAliases program)

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
    -- A scope that no prefix of the text uses has no line.
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
      map (\(name, axiom) -> prettyTopName scopes name <> prettyForAllBinders scopes (axiomBinders axiom) <+> ":" <+> prettyTypeWith scopes PrecEq (axiomLeft axiom) <+> prettyAxiomRole (axiomRole axiom) <+> prettyTypeWith scopes PrecEq (axiomRight axiom)) (importEntries (importAxioms imports))
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
prettyDecl scopes decl =
  case decl of
    DeclType declaration -> prettyTypeDecl scopes declaration
    DeclSynonym declaration -> prettySynonymDecl scopes declaration
    DeclAxiom declaration -> prettyAxiomDecl scopes declaration
    DeclVal declaration -> prettyValDecl scopes declaration
    DeclRule declaration -> prettyRuleDecl scopes declaration

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
    <> prettyTypeWith scopes PrecForAll (typeResult declaration)
    <> prettyRoleList (typeRoles declaration)
    <> prettyConstructors scopes (typeCons declaration)

prettyHeaderBinders :: ScopeIndex -> [Binder] -> FcDoc
prettyHeaderBinders scopes =
  foldMap ((space <>) . prettyPiBinder scopes)

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
    <> prettyTypeWith scopes PrecForAll (synResult declaration)
    <> " ="
    <> hardline
    <> indent 2 (prettyTypeWith scopes PrecForAll (synBody declaration))

prettyAxiomDecl :: ScopeIndex -> AxiomDecl -> FcDoc
prettyAxiomDecl scopes declaration =
  prettyVis (axiomVis declaration)
    <> "axiom "
    <> prettyTopName scopes (axiomName declaration)
    <> prettyForAllBinders scopes (axiomBinders declaration)
    <> " : "
    <> prettyTypeWith scopes PrecForAll (axiomLeft declaration)
    <+> prettyAxiomRole (axiomRole declaration)
    <+> prettyTypeWith scopes PrecForAll (axiomRight declaration)

prettyForAllBinders :: ScopeIndex -> [Binder] -> FcDoc
prettyForAllBinders _ [] = mempty
prettyForAllBinders scopes binders =
  space <> hsep (map (prettyPiBinder scopes) binders)

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
    <> foldMap (\binder -> " Λ" <> prettyPiBinder scopes binder <> ".") (ruleTypeBinders declaration)
    <> foldMap (\binder -> " λ" <> prettyPiBinder scopes binder <> ".") (ruleBinders declaration)
    <> hardline
    <> indent 2 (prettyExprWith scopes (ruleLhs declaration))
    <> hardline
    <> " = "
    <> prettyExprWith scopes (ruleRhs declaration)
    <> hardline
    <> " :: "
    <> prettyTypeWith scopes PrecForAll (ruleType declaration)

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
    TyApp function argument ->
      parenthesize (prec < PrecApp) (prettyTypeWith scopes PrecApp function <+> prettyTypeWith scopes PrecAtom argument)
    TyFun r1 r2 argument result
      | Just prefix <- liftedArrowPrefix scopes r1 r2 ->
          parenthesize
            (prec < PrecFun)
            (prettyTypeWith scopes PrecApp argument <+> (prefix <> "→") <+> prettyTypeWith scopes PrecFun result)
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
            <> prettyForallTail scopes body
        )
    TyEq left right ->
      parenthesize (prec < PrecEq) (prettyTypeWith scopes PrecApp left <+> "~" <+> prettyTypeWith scopes PrecApp right)

-- | The scope prefix of an arrow whose two representations are the
-- @LiftedRep@ synonym. The arrow names the synonym, so it follows the rule
-- of the other names: it has no prefix when the short name of the synonym
-- is unambiguous.
liftedArrowPrefix :: ScopeIndex -> Type -> Type -> Maybe FcDoc
liftedArrowPrefix scopes left right =
  case (left, right) of
    (TyCon leftName, TyCon rightName)
      | leftName == rightName,
        nameText leftName == "LiftedRep",
        nameSort leftName == SortSynonym,
        OriginTop package moduleName <- nameOrigin leftName ->
          if Map.lookup (printedNameText leftName) (shortNames scopes) == Just leftName
            then Just mempty
            else (\scopeId -> annotate (ScopeUse scopeId) (pretty scopeId <> ".")) <$> lookupScopeId scopes package moduleName
    _ -> Nothing

prettyForallTail :: ScopeIndex -> Type -> FcDoc
prettyForallTail scopes ty =
  case ty of
    TyForAll binder body ->
      space <> prettyPiBinder scopes binder <> prettyForallTail scopes body
    _ -> ". " <> prettyTypeWith scopes PrecForAll ty

prettyPiBinder :: ScopeIndex -> Binder -> FcDoc
prettyPiBinder scopes binder =
  parens
    ( (pretty (nameText (binderName binder)) <> prettyUniqueSuffix (binderName binder))
        <> " : "
        <> prettyTypeWith scopes PrecForAll (binderType binder)
    )

prettyExprWith :: ScopeIndex -> Expr -> FcDoc
prettyExprWith scopes expr =
  case expr of
    ExVar name -> prettyName scopes name
    ExLit literal ty -> "lit" <+> parens (prettyLiteral scopes literal) <+> "::" <+> prettyTypeWith scopes PrecAtom ty
    ExApp function argument ->
      prettyApp scopes function <+> prettyExprAtom scopes argument
    ExTyApp function argument ->
      prettyApp scopes function <+> ("@" <> prettyTypeWith scopes PrecAtom argument)
    ExLam {} -> prettyLambda scopes expr
    ExTyLam {} -> prettyLambda scopes expr
    ExLet bind body ->
      "let " <> prettyBind scopes bind <> ";" <> hardline <> prettyExprWith scopes body
    ExRec binds body ->
      "rec {"
        <> hardline
        <> prettyIndentedItems 2 (map (prettyBind scopes) binds)
        <> hardline
        <> "} in"
        <> hardline
        <> prettyExprWith scopes body
    -- A case with one alternative prints as a strict pattern binding,
    -- so that its body keeps the indentation of the case.
    ExCase scrutinee binder (alternative NE.:| []) ->
      "let! "
        <> prettyAltHead scopes alternative
        <> prettyCaseBinder scopes (alternativeFree alternative) binder
        <> " ="
        <> group (nest 2 (line <> prettyExprWith scopes scrutinee))
        <> ";"
        <> hardline
        <> prettyExprWith scopes (altRhs alternative)
    ExCase scrutinee binder (NE.toList -> alts) ->
      "case "
        <> prettyExprWith scopes scrutinee
        <> prettyCaseBinder scopes (foldMap alternativeFree alts) binder
        <> " of {"
        <> hardline
        <> prettyIndentedItems 2 (map (prettyAlt scopes) alts)
        <> hardline
        <> "}"
    ExAbsurd scrutinee resultType -> "absurd " <> prettyExprWith scopes scrutinee <> " return " <> parens (prettyTypeWith scopes PrecForAll resultType)
    ExCoercion proof -> "coercion " <> parens (prettyCoercion scopes proof)
    ExCast body coercion ->
      prettyExprAtom scopes body <+> "▷" <+> prettyCoercion scopes coercion
    ExForeignCall call types arguments ->
      hsep
        ( prettyForeignCall scopes call
            : map (\ty -> "@" <> prettyTypeWith scopes PrecAtom ty) types
              <> map (prettyExprAtom scopes) arguments
        )

-- | Put consecutive lambda binders on one line when they fit.
prettyLambda :: ScopeIndex -> Expr -> FcDoc
prettyLambda scopes expression =
  fillSep (concatMap prettyGroup groups) <> hardline <> indent 2 (prettyExprWith scopes body)
  where
    (binders, body) = collect expression
    groups = List.groupBy (\(kind, _) (other, _) -> kind == other) binders
    collect expr = case expr of
      ExLam binder rest -> prepend False binder rest
      ExTyLam binder rest -> prepend True binder rest
      _ -> ([], expr)
    prepend kind binder rest =
      let (following, result) = collect rest
       in ((kind, binder) : following, result)
    prettyGroup [] = []
    prettyGroup ((kind, binder) : rest) =
      finish ((if kind then "Λ" else "λ") <> prettyLambdaBinder kind binder : map (uncurry prettyLambdaBinder) rest)
    bodyFree = expressionFree body
    prettyLambdaBinder kind binder
      | kind = prettyPiBinder scopes binder
      | otherwise = prettyUnusedBinder scopes bodyFree binder
    finish [] = []
    finish [document] = [document <> "."]
    finish (document : rest) = document : finish rest

prettyApp :: ScopeIndex -> Expr -> FcDoc
prettyApp scopes expr =
  case expr of
    ExApp {} -> prettyExprWith scopes expr
    ExTyApp {} -> prettyExprWith scopes expr
    _ -> prettyExprAtom scopes expr

prettyExprAtom :: ScopeIndex -> Expr -> FcDoc
prettyExprAtom scopes expr =
  case expr of
    ExVar {} -> prettyExprWith scopes expr
    ExLit {} -> prettyExprWith scopes expr
    _ -> parens (prettyExprWith scopes expr)

prettyBind :: ScopeIndex -> Bind -> FcDoc
prettyBind scopes bind =
  (pretty (nameText (binderName (bindBinder bind))) <> prettyUniqueSuffix (binderName (bindBinder bind)))
    <> " : "
    <> prettyTypeWith scopes PrecForAll (binderType (bindBinder bind))
    <> " ="
    <> hardline
    <> indent 2 (prettyExprWith scopes (bindRhs bind))

prettyAlt :: ScopeIndex -> Alt -> FcDoc
prettyAlt scopes alternative =
  prettyAltHead scopes alternative
    <> " →"
    <> hardline
    <> indent 2 (prettyExprWith scopes (altRhs alternative))

prettyAltHead :: ScopeIndex -> Alt -> FcDoc
prettyAltHead scopes alternative =
  case altCon alternative of
    AltDefault -> "_"
    AltLit literal -> prettyLiteral scopes literal <> prettyTypeBinders (altTypeBinders alternative) <> prettyTermBinders (altBinders alternative)
    AltData name -> prettyName scopes name <> prettyTypeBinders (altTypeBinders alternative) <> prettyTermBinders (altBinders alternative)
  where
    prettyTypeBinders binders =
      case binders of
        [] -> mempty
        binder : rest ->
          space
            <> "@"
            <> prettyPiBinder scopes binder
            <> prettyTypeBinders rest
    prettyTermBinders binders =
      case binders of
        [] -> mempty
        binder : rest ->
          space
            <> prettyUnusedBinder scopes rhsFree binder
            <> prettyTermBinders rest
    rhsFree = expressionFree (altRhs alternative)

-- | The case binder. A binder that the alternatives do not use is not
-- shown, because the grammar makes the case binder optional.
prettyCaseBinder :: ScopeIndex -> Set.Set Name -> Maybe Binder -> FcDoc
prettyCaseBinder scopes scope binder = case binder of
  Just named
    | binderName named `Set.member` scope -> " as " <> prettyPiBinder scopes named
  _ -> mempty

-- | A term binder. A binder that its scope does not use shows as @_@, with
-- its type. The scope is the set of free names of the code where the binder
-- is visible.
prettyUnusedBinder :: ScopeIndex -> Set.Set Name -> Binder -> FcDoc
prettyUnusedBinder scopes scope binder
  | binderName binder `Set.member` scope = prettyPiBinder scopes binder
  | otherwise = parens ("_ : " <> prettyTypeWith scopes PrecForAll (binderType binder))

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

prettyIndentedItems :: Int -> [FcDoc] -> FcDoc
prettyIndentedItems _ [] = mempty
prettyIndentedItems amount documents = indent amount (vsep (punctuate ";" documents))

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
    CoForAll binder body -> "forall-co " <> prettyPiBinder scopes binder <+> parens (prettyCoercion scopes body)
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
