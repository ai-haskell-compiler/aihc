{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Human-readable System FC text.
--
-- The text has two styles. The canonical style is the format that
-- "Aihc.Fc.Parser" reads, and the parser gives back the same program. The
-- readable style is for people. It removes information that the parser
-- needs but that a reader can find from the types:
--
-- * It shows only the scopes that the text uses.
-- * It shows primitive literals in the GHC form, for example @0#@, @0##@,
--   @3#Word8@ and @"abc"#@, and a string literal as a Haskell string.
-- * It shows unboxed tuples as @(# a, b #)@, with no representation
--   arguments.
-- * It shows each function type as @a → b@, with no representations.
-- * It shows the representation and kind synonyms (@LiftedRep@, @Type@ and
--   the unlifted forms) as the synonym.
-- * It shows a promoted list as @'[a, b]@.
-- * It shows a pattern binder that the declaration does not use as @_@.
module Aihc.Fc.Pretty
  ( RenderStyle (..),
    renderProgram,
    renderProgramWith,
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

-- | The style of the text.
data RenderStyle
  = -- | The format that the parser reads back.
    RenderCanonical
  | -- | A shorter format for people. The parser does not read it.
    RenderReadable
  deriving (Eq, Show)

-- | The annotation of a scope prefix in a document: the scope id that the
-- prefix uses. The readable style keeps only the scopes that a prefix uses.
newtype ScopeUse = ScopeUse Int

type FcDoc = Doc ScopeUse

-- | Reverse lookup from a scope's identity to its printed scope id, built
-- once per program instead of re-scanning the scope table for every name.
-- It also holds the facts that the readable style uses.
data ScopeIndex = ScopeIndex
  { scopeIds :: Map.Map (PackageId, Text) Int,
    shortNames :: Map.Map Text Name,
    primitiveNames :: Map.Map ForeignCall Name,
    renderStyle :: RenderStyle,
    -- | The number of fields of each unboxed tuple type and constructor.
    unboxedTupleArities :: Map.Map Name Int,
    -- | The nullary synonyms that the readable style shows, by their
    -- expanded bodies.
    synonymFolds :: Map.Map Type Name,
    -- | The bodies of the synonyms in 'synonymFolds'.
    synonymBodies :: Map.Map Name Type,
    -- | False in the body of a synonym declaration, so that the body does
    -- not show as the synonym itself.
    foldSynonyms :: Bool,
    -- | The names that the current declaration uses. A pattern binder that
    -- is not in this set shows as @_@ in the readable style.
    usedNames :: Set.Set Name
  }

scopeIndexFromProgram :: RenderStyle -> Program -> ScopeIndex
scopeIndexFromProgram style program =
  ScopeIndex
    { scopeIds = Map.fromList [((entryPackage, entryModule), scopeId) | (scopeId, entryPackage, entryModule) <- scopeEntries (programScopes program)],
      shortNames = globalReferences program,
      primitiveNames = primitiveAliases program,
      renderStyle = style,
      unboxedTupleArities = tupleArities program,
      synonymFolds = Map.fromList [(expandSynonyms bodies body, name) | (name, body) <- Map.toList bodies],
      synonymBodies = bodies,
      foldSynonyms = True,
      usedNames = mempty
    }
  where
    bodies = foldedSynonyms program

isReadable :: ScopeIndex -> Bool
isReadable scopes = renderStyle scopes == RenderReadable

-- | The number of fields of each unboxed tuple type and constructor of a
-- program. The type of an unboxed tuple with @n@ fields has @2n@ arguments:
-- first the @n@ representations, then the @n@ field types.
tupleArities :: Program -> Map.Map Name Int
tupleArities program =
  Map.fromList (concatMap declaredTuples (programDecls program) <> importedTuples)
  where
    imports = programImports program
    declaredTuples declaration = case declaration of
      DeclType value
        | [constructor] <- typeCons value,
          conRepresentation constructor == UnboxedTupleConstructor,
          even (length (typeBinders value)) ->
            let arity = length (typeBinders value) `div` 2
             in [(typeName value, arity), (conName constructor, arity)]
      _ -> []
    importedTuples =
      [ entry
      | (typeName', [constructor]) <- Map.toList (importDataCons imports),
        Map.lookup constructor (importConRepresentations imports) == Just UnboxedTupleConstructor,
        Just header <- [Map.lookup typeName' (importHeaders imports)],
        let arity = length (fst (splitKindArrows header)) `div` 2,
        entry <- [(typeName', arity), (constructor, arity)]
      ]

-- | The arguments of a kind: the binders of the foralls and the arguments of
-- the arrows.
splitKindArrows :: Type -> ([Type], Type)
splitKindArrows ty = case ty of
  TyForAll binder body -> let (arguments, result) = splitKindArrows body in (binderType binder : arguments, result)
  TyFun _ _ argument body -> let (arguments, result) = splitKindArrows body in (argument : arguments, result)
  _ -> ([], ty)

-- | The nullary synonyms that the readable style shows in place of their
-- bodies. Only the representation and kind synonyms are in this set: a
-- general synonym such as @FilePath@ would show at sites that do not use it.
foldedSynonyms :: Program -> Map.Map Name Type
foldedSynonyms program =
  Map.fromList
    ( [ (synName value, synBody value)
      | DeclSynonym value <- programDecls program,
        null (synBinders value),
        isFolded (synName value)
      ]
        <> [(name, body) | (name, body) <- Map.toList (importSynonyms (programImports program)), isFolded name]
    )
  where
    isFolded name = nameText name `elem` ["Type", "UnliftedType", "LiftedRep", "UnliftedRep"]

-- | Replace each synonym of the map with its expanded body.
expandSynonyms :: Map.Map Name Type -> Type -> Type
expandSynonyms bodies = go (Map.size bodies)
  where
    go fuel ty = case ty of
      TyCon name
        | fuel > 0,
          Just body <- Map.lookup name bodies ->
            go (fuel - 1) body
      TyApp function argument -> TyApp (go fuel function) (go fuel argument)
      _ -> ty

-- | The names that the expressions, types and coercions of a declaration
-- use. A binder can have the same name as a binder in a different scope, so
-- the set can contain a name of a binder that has no use. It never omits a
-- name that has a use.
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

-- | Give each primitive signature a separate text alias.
primitiveAliases :: Program -> Map.Map ForeignCall Name
primitiveAliases program = Map.fromList (concatMap aliases (Map.elems grouped))
  where
    calls = Set.toAscList (foldMap declarationCalls (programDecls program))
    grouped = Map.fromListWith (<>) [(nameText (foreignCallName call), [call]) | call <- calls]
    aliases variants =
      [(call, Name (nameText (foreignCallName call)) SortValue (OriginLocal (Unique unique))) | (unique, call) <- zip [0 ..] (List.sort variants)]
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

-- | The canonical text of a program. The parser reads it back.
renderProgram :: Program -> Text
renderProgram = renderProgramWith RenderCanonical

renderProgramWith :: RenderStyle -> Program -> Text
renderProgramWith style = renderDoc . prettyProgram style

renderDoc :: FcDoc -> Text
renderDoc = renderStrict . layoutPretty (LayoutOptions (AvailablePerLine 120 1))

-- | The text of a program in sections: first the header with the scopes and
-- the imports, then one section for each declaration. A declaration section
-- has the name of the declaration, and a rule section has no name. The
-- sections, with a blank line between each two, are the text of
-- 'renderProgramWith'.
renderProgramSections :: RenderStyle -> Program -> [(Maybe Name, Text)]
renderProgramSections style program =
  [(Nothing, renderDoc (vsep (punctuate hardline header))) | not (null header)]
    <> [(declName decl, renderDoc document) | (decl, document) <- declarations]
  where
    (header, declarations) = programDocuments style program
    declName decl = case decl of
      DeclType declaration -> Just (typeName declaration)
      DeclSynonym declaration -> Just (synName declaration)
      DeclAxiom declaration -> Just (axiomName declaration)
      DeclVal declaration -> Just (valName declaration)
      DeclRule _ -> Nothing

prettyProgram :: RenderStyle -> Program -> FcDoc
prettyProgram style program =
  vsep (punctuate hardline (header <> map snd declarations))
  where
    (header, declarations) = programDocuments style program

-- | The header documents of a program, and the document of each
-- declaration.
programDocuments :: RenderStyle -> Program -> ([FcDoc], [(Decl, FcDoc)])
programDocuments style program =
  (scopeDocuments <> primitiveDocuments <> importDocuments, declarations)
  where
    scopes = programScopes program
    scopeIndex = scopeIndexFromProgram style program
    declarations = [(decl, prettyDecl scopeIndex decl) | decl <- programDecls program]
    shownEntries = case style of
      RenderCanonical -> scopeEntries scopes
      RenderReadable -> [entry | entry@(scopeId, _, _) <- scopeEntries scopes, scopeId `Set.member` usedScopes]
    usedScopes = foldMap documentScopeUses (primitiveDocuments <> importDocuments <> map snd declarations)
    scopeDocuments =
      case shownEntries of
        [] -> []
        entries -> [prettyScopes entries]
    importDocuments = prettyImports scopeIndex (programImports program)
    primitiveDocuments = prettyImportGroup "prims" [prettyPrimitiveAlias alias <+> "=" <+> prettyForeignImportDependencies scopeIndex (foreignCallDependencies call) <> prettyTopName scopeIndex (foreignCallName call) <+> "::" <+> prettyTypeWith scopeIndex PrecForAll (foreignCallType call) | (call, alias) <- Map.toAscList (primitiveNames scopeIndex)]

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
prettyDecl outer decl =
  case decl of
    DeclType declaration -> prettyTypeDecl scopes declaration
    DeclSynonym declaration -> prettySynonymDecl scopes declaration
    DeclAxiom declaration -> prettyAxiomDecl scopes declaration
    DeclVal declaration -> prettyValDecl scopes declaration
    DeclRule declaration -> prettyRuleDecl scopes declaration
  where
    scopes
      | isReadable outer = outer {usedNames = declarationUses decl}
      | otherwise = outer

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
    <> indent 2 (prettyTypeWith scopes {foldSynonyms = False} PrecForAll (synBody declaration))

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
prettyTypeWith scopes prec ty
  | isReadable scopes = prettyReadableType scopes prec ty
  | otherwise = prettyCanonicalType scopes prec ty

-- | A type in the readable style. A type that has no short form shows as in
-- the canonical style, and its parts show in the readable style.
prettyReadableType :: ScopeIndex -> Prec -> Type -> FcDoc
prettyReadableType scopes prec ty
  | foldSynonyms scopes,
    TyApp {} <- ty,
    Just synonym <- Map.lookup (expandSynonyms (synonymBodies scopes) ty) (synonymFolds scopes) =
      prettyName scopes synonym
  | Just fields <- unboxedTupleType scopes ty =
      prettyUnboxedTuple (map (prettyTypeWith scopes PrecForAll) fields)
  | Just elements <- promotedList ty =
      "'[" <> hsep (punctuate "," (map (prettyTypeWith scopes PrecForAll) elements)) <> "]"
  | TyFun _ _ argument result <- ty =
      parenthesize
        (prec < PrecFun)
        (prettyTypeWith scopes PrecApp argument <+> "→" <+> prettyTypeWith scopes PrecFun result)
  | otherwise = prettyCanonicalType scopes prec ty

-- | The field types of a saturated unboxed tuple type.
unboxedTupleType :: ScopeIndex -> Type -> Maybe [Type]
unboxedTupleType scopes ty = do
  let (function, arguments) = typeSpine ty
  TyCon name <- Just function
  arity <- Map.lookup name (unboxedTupleArities scopes)
  if length arguments == 2 * arity then Just (drop arity arguments) else Nothing

typeSpine :: Type -> (Type, [Type])
typeSpine = go []
  where
    go arguments ty = case ty of
      TyApp function argument -> go (argument : arguments) function
      _ -> (ty, arguments)

-- | The elements of a promoted list: @': k x xs@ ends in @'[] k@.
promotedList :: Type -> Maybe [Type]
promotedList ty = case typeSpine ty of
  (TyCon name, [_, element, rest])
    | isListConstructor ":" name -> (element :) <$> promotedList rest
  (TyCon name, [_])
    | isListConstructor "[]" name -> Just []
  _ -> Nothing
  where
    isListConstructor text name = nameText name == text && nameSort name == SortDataConstructor

prettyUnboxedTuple :: [FcDoc] -> FcDoc
prettyUnboxedTuple [] = "(# #)"
prettyUnboxedTuple fields = "(#" <+> hsep (punctuate "," fields) <+> "#)"

prettyCanonicalType :: ScopeIndex -> Prec -> Type -> FcDoc
prettyCanonicalType scopes prec ty =
  case ty of
    TyVar name -> prettyName scopes name
    TyCon name -> prettyName scopes name
    TyLit kindName literal ->
      parenthesize (prec < PrecApp) ("lit" <+> prettyName scopes kindName <+> prettyTyLit literal)
    TyApp function argument ->
      parenthesize (prec < PrecApp) (prettyTypeWith scopes PrecApp function <+> prettyTypeWith scopes PrecAtom argument)
    TyFun r1 r2 argument result
      | Just scopeId <- liftedArrowScope scopes r1 r2 ->
          parenthesize
            (prec < PrecFun)
            (prettyTypeWith scopes PrecApp argument <+> (pretty scopeId <> ".→") <+> prettyTypeWith scopes PrecFun result)
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

liftedArrowScope :: ScopeIndex -> Type -> Type -> Maybe Int
liftedArrowScope scopes left right =
  case (left, right) of
    (TyCon leftName, TyCon rightName)
      | leftName == rightName,
        nameText leftName == "LiftedRep",
        OriginTop package moduleName <- nameOrigin leftName ->
          lookupScopeId scopes package moduleName
    _ -> Nothing

prettyForallTail :: ScopeIndex -> Type -> FcDoc
prettyForallTail scopes ty =
  case ty of
    TyForAll binder body ->
      space <> prettyPiBinder scopes binder <> prettyForallTail scopes body
    _ -> ". " <> prettyTypeWith scopes PrecForAll ty

prettyBinderName :: Name -> FcDoc
prettyBinderName name = pretty (nameText name) <> prettyUniqueSuffix name

prettyPiBinder :: ScopeIndex -> Binder -> FcDoc
prettyPiBinder scopes binder =
  parens
    ( prettyBinderName (binderName binder)
        <> " : "
        <> prettyTypeWith scopes PrecForAll (binderType binder)
    )

-- | An expression that has a short atomic form in the readable style.
prettyReadableAtom :: ScopeIndex -> Expr -> Maybe FcDoc
prettyReadableAtom scopes expr
  | not (isReadable scopes) = Nothing
  | otherwise = case expr of
      ExLit literal ty -> prettyShortLiteral literal ty
      ExApp (ExApp (ExVar function) (ExLit (LitAddr _ bytes) _)) (ExLit (LitInt _ 0) _)
        | isStringUnpacker function ->
            Just ("\"" <> pretty (concatMap (encodeStringCharacter . chr . fromIntegral) (BS.unpack bytes)) <> "\"")
      _ -> unboxedTupleExpr
  where
    unboxedTupleExpr = do
      let (function, types, arguments) = expressionSpine expr
      ExVar name <- Just function
      arity <- Map.lookup name (unboxedTupleArities scopes)
      if length types == 2 * arity && length arguments == arity
        then Just (prettyUnboxedTuple (map (prettyExprWith scopes) arguments))
        else Nothing

-- | A negative literal needs parentheses as an argument, so that it does
-- not look like a subtraction.
isNegativeLiteral :: Expr -> Bool
isNegativeLiteral expr = case expr of
  ExLit (LitInt _ value) _ -> value < 0
  _ -> False

-- | The function that unpacks a string literal: a Haskell string literal
-- becomes a call of it on the bytes of the literal and the offset 0. The
-- bytes are Latin-1, so each byte is one character of the string.
isStringUnpacker :: Name -> Bool
isStringUnpacker name =
  nameText name == "unpackLatin1From"
    && nameSort name == SortValue
    && case nameOrigin name of
      OriginTop _ moduleName -> moduleName == "GHC.CString"
      OriginLocal {} -> False

-- | The head of an application, its type arguments and its value
-- arguments, when the type arguments come first.
expressionSpine :: Expr -> (Expr, [Type], [Expr])
expressionSpine = go []
  where
    go arguments expression = case expression of
      ExApp function argument -> go (argument : arguments) function
      _ ->
        let (function, types) = typeArguments [] expression
         in (function, types, arguments)
    typeArguments types expression = case expression of
      ExTyApp function argument -> typeArguments (argument : types) function
      _ -> (expression, types)

-- | A primitive literal in the GHC form, when its type is the type that its
-- representation gives: @1#@ for @Int#@, @1##@ for @Word#@, @1#Word8@ for
-- @Word8#@, @'c'#@ for @Char#@ and @"text"#@ for @Addr#@.
prettyShortLiteral :: Literal -> Type -> Maybe FcDoc
prettyShortLiteral literal ty = do
  TyCon typeName' <- Just ty
  let typeText = nameText typeName'
  case literal of
    LitInt (TyCon representation) _
      | Just (expected, _) <- lookup (nameText representation) integerRepresentations,
        typeText == expected ->
          prettyShortAltLiteral literal
    LitChar _ _
      | typeText == "Char#" -> prettyShortAltLiteral literal
    LitAddr _ _
      | typeText == "Addr#" -> prettyShortAltLiteral literal
    _ -> Nothing

-- | A primitive literal in the GHC form, with the suffix that its
-- representation gives. A case alternative has no literal type.
prettyShortAltLiteral :: Literal -> Maybe FcDoc
prettyShortAltLiteral literal = case literal of
  LitInt (TyCon representation) value
    | Just (_, suffix) <- lookup (nameText representation) integerRepresentations ->
        Just (pretty value <> suffix)
  LitInt {} -> Nothing
  LitChar _ value -> Just ("'" <> pretty (encodeCharLiteral value) <> "'#")
  LitAddr _ value -> Just ("\"" <> pretty (concatMap encodeByte (BS.unpack value)) <> "\"#")

-- | The representations of the integer literals, with the type of the
-- literal and the suffix of its GHC form.
integerRepresentations :: [(Text, (Text, FcDoc))]
integerRepresentations =
  [ ("IntRep", ("Int#", "#")),
    ("WordRep", ("Word#", "##")),
    ("Int8Rep", ("Int8#", "#Int8")),
    ("Int16Rep", ("Int16#", "#Int16")),
    ("Int32Rep", ("Int32#", "#Int32")),
    ("Int64Rep", ("Int64#", "#Int64")),
    ("Word8Rep", ("Word8#", "#Word8")),
    ("Word16Rep", ("Word16#", "#Word16")),
    ("Word32Rep", ("Word32#", "#Word32")),
    ("Word64Rep", ("Word64#", "#Word64")),
    ("AddrRep", ("Addr#", "#Addr"))
  ]

prettyExprWith :: ScopeIndex -> Expr -> FcDoc
prettyExprWith scopes expr
  | Just document <- prettyReadableAtom scopes expr = document
  | otherwise = prettyExprBody scopes expr

prettyExprBody :: ScopeIndex -> Expr -> FcDoc
prettyExprBody scopes expr =
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
        <> prettyCaseBinder scopes (expressionFree (altRhs alternative)) binder
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
    lambdaFree = expressionFree body <> foldMap (typeFree . binderType . snd) binders
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
      finish ((if kind then "Λ" else "λ") <> prettyLambdaBinder binder : map (prettyLambdaBinder . snd) rest)
    -- A lambda binder keeps its type, because the type of the lambda
    -- needs it.
    prettyLambdaBinder binder
      | isUnused scopes lambdaFree binder = parens ("_ : " <> prettyTypeWith scopes PrecForAll (binderType binder))
      | otherwise = prettyPiBinder scopes binder
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
    _ | Just document <- prettyReadableAtom scopes expr -> parenthesize (isNegativeLiteral expr) document
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
    AltLit literal -> prettyAltLiteral literal <> prettyTypeBinders (altTypeBinders alternative) <> prettyTermBinders (altBinders alternative)
    AltData name
      | isReadable scopes,
        Just arity <- Map.lookup name (unboxedTupleArities scopes),
        null (altTypeBinders alternative),
        length (altBinders alternative) == arity ->
          prettyUnboxedTuple (map prettyTupleField (altBinders alternative))
      | otherwise -> prettyName scopes name <> prettyTypeBinders (altTypeBinders alternative) <> prettyTermBinders (altBinders alternative)
  where
    alternativeScope = expressionFree (altRhs alternative) <> foldMap (typeFree . binderType) (altBinders alternative)
    prettyTupleField binder
      | isUnused scopes alternativeScope binder = "_"
      | otherwise = prettyBinderName (binderName binder) <> " : " <> prettyTypeWith scopes PrecForAll (binderType binder)
    prettyAltLiteral literal
      | isReadable scopes, Just document <- prettyShortAltLiteral literal = document
      | otherwise = prettyLiteral scopes literal
    prettyTypeBinders binders =
      case binders of
        [] -> mempty
        binder : rest ->
          space
            <> "@"
            <> prettyPatternBinder scopes alternativeScope binder
            <> prettyTypeBinders rest
    prettyTermBinders binders =
      case binders of
        [] -> mempty
        binder : rest ->
          space
            <> prettyPatternBinder scopes alternativeScope binder
            <> prettyTermBinders rest

-- | The @as@ binder of a case. The readable style omits a binder that the
-- declaration does not use.
prettyCaseBinder :: ScopeIndex -> Set.Set Name -> Maybe Binder -> FcDoc
prettyCaseBinder scopes scope binder = case binder of
  Just named
    | not (isUnused scopes scope named) -> " as " <> prettyPiBinder scopes named
  Just _ -> mempty
  Nothing -> mempty

-- | A binder of a pattern. The readable style shows a binder that the
-- declaration does not use as @_@, with no type.
prettyPatternBinder :: ScopeIndex -> Set.Set Name -> Binder -> FcDoc
prettyPatternBinder scopes scope binder
  | isUnused scopes scope binder = "_"
  | otherwise = prettyPiBinder scopes binder

-- | True when the style is readable and the scope of the binder does not
-- use it. The scope is the set of free names of the code where the binder
-- is visible. The declaration-wide set is a fast test, and the scope set is
-- calculated only for a name that the declaration uses.
isUnused :: ScopeIndex -> Set.Set Name -> Binder -> Bool
isUnused scopes scope binder =
  isReadable scopes && (name `Set.notMember` usedNames scopes || name `Set.notMember` scope)
  where
    name = binderName binder

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
