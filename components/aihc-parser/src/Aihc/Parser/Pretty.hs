{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Aihc.Parser.Pretty
-- Description : Pretty-printing of AST to Haskell source code
--
-- This module provides pretty-printing of parsed AST back to valid Haskell
-- source code. It is used for round-trip testing and code generation.
--
-- The 'Pretty' instances from 'Prettyprinter' are provided for the main
-- AST types, allowing direct use of 'pretty' from @Prettyprinter@.
--
-- __Parenthesization__ is handled by the 'Aihc.Parser.Parens' module, which
-- inserts 'EParen', 'PParen', 'TParen', and 'CmdPar' nodes at all required
-- positions. The Pretty instances call the paren-insertion pass before
-- formatting, so the formatting code here does not need to worry about
-- operator precedence or context-sensitive parenthesization.
--
-- This module has an empty export list because it only provides typeclass
-- instances. Import it to bring the 'Pretty' instances into scope.
--
-- __Provided instances:__ 'Pretty' for 'Module', 'Expr', 'Pattern', 'Type'.
module Aihc.Parser.Pretty
  (
  )
where

import Aihc.Parser.Parens (addDeclParens, addExprParens, addModuleParens, addPatternParens, addTypeParens)
import Aihc.Parser.Syntax
import Data.Maybe (catMaybes, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    align,
    braces,
    brackets,
    comma,
    hang,
    hardline,
    hsep,
    indent,
    parens,
    punctuate,
    semi,
    vsep,
    (<+>),
  )

-- | Wrap a document in braces with interior spaces: @{ content }@.
-- Unlike 'braces' which produces @{content}@, this version avoids the
-- @{-@ block-comment ambiguity that occurs when the content starts with a
-- minus sign (e.g. a negated literal pattern in a let binding).
spacedBraces :: Doc ann -> Doc ann
spacedBraces d = "{" <+> d <+> "}"

-- | Pretty instance for Module - renders to valid Haskell source code.
instance Pretty Module where
  pretty = prettyModuleDoc . addModuleParens

-- | Pretty instance for Expr - renders to valid Haskell source code.
instance Pretty Expr where
  pretty = prettyExpr . addExprParens

-- | Pretty instance for Pattern - renders to valid Haskell source code.
instance Pretty Pattern where
  pretty = prettyPattern . addPatternParens

-- | Pretty instance for Decl - renders to valid Haskell source code.
instance Pretty Decl where
  pretty decl = vsep (prettyDeclLines (addDeclParens decl))

-- | Pretty instance for Type - renders to valid Haskell source code.
instance Pretty Type where
  pretty = prettyType . addTypeParens

instance Pretty Name where
  pretty = pretty . renderName

instance Pretty UnqualifiedName where
  pretty = pretty . renderUnqualifiedName

prettyModuleDoc :: Module -> Doc ann
prettyModuleDoc modu =
  vsep (pragmaLines <> headerLines <> importLines <> declLines)
  where
    pragmaLines =
      map
        (\ext -> "{-# LANGUAGE" <+> pretty (extensionSettingName ext) <+> "#-}")
        (moduleLanguagePragmas modu)
    headerLines =
      case moduleName modu of
        Just name ->
          [ hsep
              ( ["module", pretty name]
                  <> maybe [] (\p -> [prettyPragma p]) (moduleWarningPragma modu)
                  <> maybe [] (\specs -> [prettyExportSpecList specs]) (moduleExports modu)
                  <> ["where"]
              )
          ]
        Nothing -> []
    importLines = map prettyImportDecl (moduleImports modu)
    declLines = concatMap prettyDeclLines (moduleDecls modu)

prettyExportSpecList :: [ExportSpec] -> Doc ann
prettyExportSpecList specs =
  parens (hsep (punctuate comma (map prettyExportSpec specs)))

prettyExportSpec :: ExportSpec -> Doc ann
prettyExportSpec spec =
  case spec of
    ExportAnn _ sub -> prettyExportSpec sub
    ExportModule mWarning modName -> prettyExportWarning mWarning ("module" <+> pretty modName)
    ExportVar mWarning namespace name ->
      prettyExportWarning mWarning (prettyNamespacePrefix namespace <> prettyName name)
    ExportAbs mWarning namespace name ->
      prettyExportWarning mWarning (prettyNamespacePrefix namespace <> prettyName name)
    ExportAll mWarning namespace name ->
      prettyExportWarning mWarning (prettyNamespacePrefix namespace <> prettyName name <> "(..)")
    ExportWith mWarning namespace name members ->
      prettyExportWarning
        mWarning
        (prettyNamespacePrefix namespace <> prettyName name <> parens (hsep (punctuate comma (map prettyExportMember members))))
    ExportWithAll mWarning namespace name wildcardIndex members ->
      prettyExportWarning
        mWarning
        (prettyNamespacePrefix namespace <> prettyName name <> parens (hsep (punctuate comma (insertWildcard wildcardIndex (map prettyExportMember members)))))

prettyExportWarning :: Maybe Pragma -> Doc ann -> Doc ann
prettyExportWarning mWarning doc =
  case mWarning of
    Nothing -> doc
    Just p -> hsep [prettyPragma p, doc]

prettyImportDecl :: ImportDecl -> Doc ann
prettyImportDecl decl =
  let renderPostQualified =
        importDeclQualifiedPost decl
          && importDeclQualified decl
   in hsep
        ( ["import"]
            <> ["safe" | importDeclSafe decl]
            <> maybe [] (\p -> [prettyPragma p]) (importDeclSourcePragma decl)
            <> ["qualified" | importDeclQualified decl && not renderPostQualified]
            <> maybe [] (\level -> [prettyImportLevel level]) (importDeclLevel decl)
            <> maybe [] (\pkg -> [prettyQuotedText pkg]) (importDeclPackage decl)
            <> [pretty (importDeclModule decl)]
            <> ["qualified" | importDeclQualified decl && renderPostQualified]
            <> maybe [] (\alias -> ["as", pretty alias]) (importDeclAs decl)
            <> maybe [] (\spec -> [prettyImportSpec spec]) (importDeclSpec decl)
        )

prettyImportLevel :: ImportLevel -> Doc ann
prettyImportLevel level =
  case level of
    ImportLevelQuote -> "quote"
    ImportLevelSplice -> "splice"

-- | Pretty-print a top-level declaration splice.
-- The expression is printed as-is; explicit TH splices appear as @$expr@ or
-- @$(expr)@ through the normal 'ETHSplice' pretty-printer.
prettyDeclSpliceExpr :: Expr -> Doc ann
prettyDeclSpliceExpr = prettyExpr

prettyQuotedText :: Text -> Doc ann
prettyQuotedText txt = "\"" <> pretty txt <> "\""

prettyImportSpec :: ImportSpec -> Doc ann
prettyImportSpec spec =
  hsep
    ( ["hiding" | importSpecHiding spec]
        <> [parens (hsep (punctuate comma (map prettyImportItem (importSpecItems spec))))]
    )

prettyImportItem :: ImportItem -> Doc ann
prettyImportItem item =
  case item of
    ImportAnn _ sub -> prettyImportItem sub
    ImportItemVar namespace name -> prettyNamespacePrefix namespace <> prettyBinderUName name
    ImportItemAbs namespace name -> prettyNamespacePrefix namespace <> prettyConstructorUName name
    ImportItemAll namespace name -> prettyNamespacePrefix namespace <> prettyConstructorUName name <> "(..)"
    ImportItemWith namespace name members ->
      prettyNamespacePrefix namespace <> prettyConstructorUName name <> parens (hsep (punctuate comma (map prettyExportMember members)))
    ImportItemAllWith namespace name wildcardIndex members ->
      prettyNamespacePrefix namespace <> prettyConstructorUName name <> parens (hsep (punctuate comma (insertWildcard wildcardIndex (map prettyExportMember members))))

insertWildcard :: Int -> [Doc ann] -> [Doc ann]
insertWildcard wildcardIndex members =
  let (before, after) = splitAt wildcardIndex members
   in before <> [".."] <> after

prettyExportMember :: IEBundledMember -> Doc ann
prettyExportMember (IEBundledMember namespace name) =
  prettyMemberNamespacePrefix namespace <> prettyBundledMemberName name

prettyNamespacePrefix :: Maybe IEEntityNamespace -> Doc ann
prettyNamespacePrefix namespace =
  case namespace of
    Just ns -> prettyNamespace ns <> " "
    Nothing -> mempty

prettyNamespace :: IEEntityNamespace -> Doc ann
prettyNamespace namespace =
  case namespace of
    IEEntityNamespaceType -> "type"
    IEEntityNamespacePattern -> "pattern"
    IEEntityNamespaceData -> "data"

prettyMemberNamespacePrefix :: Maybe IEBundledNamespace -> Doc ann
prettyMemberNamespacePrefix namespace =
  case namespace of
    Just ns -> prettyMemberNamespace ns <> " "
    Nothing -> mempty

prettyMemberNamespace :: IEBundledNamespace -> Doc ann
prettyMemberNamespace namespace =
  case namespace of
    IEBundledNamespaceType -> "type"
    IEBundledNamespaceData -> "data"

prettyDeclLines :: Decl -> [Doc ann]
prettyDeclLines decl =
  case decl of
    DeclAnn _ sub -> prettyDeclLines sub
    DeclValue valueDecl -> prettyValueDeclLines valueDecl
    DeclTypeSig names ty -> [hsep [hsep (punctuate comma (map prettyBinderName names)), "::", prettyType ty]]
    DeclPatSyn patSynDecl -> [prettyPatSynDecl patSynDecl]
    DeclPatSynSig names ty -> [hsep ["pattern", hsep (punctuate comma (map prettyConstructorUName names)), "::", prettyType ty]]
    DeclStandaloneKindSig name kind -> [hsep ["type", prettyConstructorUName name, "::", prettyType kind]]
    DeclFixity assoc mNamespace prec ops ->
      [ hsep
          ( [prettyFixityAssoc assoc]
              <> maybe [] (pure . pretty . show) prec
              <> maybe [] (pure . prettyNamespace) mNamespace
              <> punctuate comma (map prettyInfixOp ops)
          )
      ]
    DeclRoleAnnotation ann -> [prettyRoleAnnotation ann]
    DeclTypeSyn synDecl ->
      [hsep (["type"] <> prettyDeclBinderHead [] (typeSynHead synDecl) <> ["=", prettyType (typeSynBody synDecl)])]
    DeclData dataDecl -> [prettyDataDecl dataDecl]
    DeclTypeData dataDecl -> [prettyTypeDataDecl dataDecl]
    DeclNewtype newtypeDecl -> [prettyNewtypeDecl newtypeDecl]
    DeclClass classDecl -> [prettyClassDecl classDecl]
    DeclInstance instanceDecl -> [prettyInstanceDecl instanceDecl]
    DeclStandaloneDeriving derivingDecl -> [prettyStandaloneDeriving derivingDecl]
    DeclDefault tys -> ["default" <+> parens (hsep (punctuate comma (map prettyType tys)))]
    DeclForeign foreignDecl -> [prettyForeignDecl foreignDecl]
    DeclSplice body -> [prettyDeclSpliceExpr body]
    DeclTypeFamilyDecl tf -> [prettyTypeFamilyDecl tf]
    DeclDataFamilyDecl df -> [prettyDataFamilyDecl df]
    DeclTypeFamilyInst tfi -> [prettyTopTypeFamilyInst tfi]
    DeclDataFamilyInst dfi -> [prettyTopDataFamilyInst dfi]
    DeclPragma pragma -> [prettyPragma pragma]

prettyRoleAnnotation :: RoleAnnotation -> Doc ann
prettyRoleAnnotation ann =
  hsep
    ( [ "type",
        "role",
        prettyConstructorUName (roleAnnotationName ann)
      ]
        <> map prettyRole (roleAnnotationRoles ann)
    )

prettyRole :: Role -> Doc ann
prettyRole role =
  case role of
    RoleNominal -> "nominal"
    RoleRepresentational -> "representational"
    RolePhantom -> "phantom"
    RoleInfer -> "_"

prettyValueDeclLines :: ValueDecl -> [Doc ann]
prettyValueDeclLines valueDecl =
  case valueDecl of
    PatternBind multTag pat rhs -> [prettyMultiplicityTag multTag <> prettyPattern pat <+> prettyRhs rhs]
    FunctionBind name matches ->
      concatMap (prettyFunctionMatchLines name) matches

-- | Pretty-print a value declaration on a single line.
prettyValueDeclSingleLine :: ValueDecl -> Doc ann
prettyValueDeclSingleLine valueDecl =
  case valueDecl of
    PatternBind multTag pat rhs -> prettyMultiplicityTag multTag <> prettyPattern pat <+> prettyRhs rhs
    FunctionBind name matches ->
      hsep (punctuate semi (map (prettyFunctionMatch name) matches))

prettyMultiplicityTag :: MultiplicityTag -> Doc ann
prettyMultiplicityTag NoMultiplicityTag = mempty
prettyMultiplicityTag LinearMultiplicityTag = "%1" <+> mempty
prettyMultiplicityTag (ExplicitMultiplicityTag ty) = "%" <> prettyType ty <+> mempty

-- | Pretty-print a pattern synonym declaration.
prettyPatSynDecl :: PatSynDecl -> Doc ann
prettyPatSynDecl ps =
  hsep
    ( ["pattern"]
        <> prettyPatSynLhs (patSynDeclName ps) (patSynDeclArgs ps)
        <> [dirArrow (patSynDeclDir ps)]
        <> [prettyPattern (patSynDeclPat ps)]
        <> prettyPatSynWhere (patSynDeclName ps) (patSynDeclDir ps)
    )
  where
    dirArrow PatSynBidirectional = "="
    dirArrow PatSynUnidirectional = "<-"
    dirArrow (PatSynExplicitBidirectional _) = "<-"

prettyPatSynLhs :: UnqualifiedName -> PatSynArgs -> [Doc ann]
prettyPatSynLhs name args =
  case args of
    PatSynPrefixArgs vars ->
      prettyConstructorUName name : map pretty vars
    PatSynInfixArgs lhs rhs ->
      [pretty lhs, prettyInfixOp name, pretty rhs]
    PatSynRecordArgs fields ->
      [prettyConstructorUName name <+> braces (hsep (punctuate comma (map pretty fields)))]

prettyPatSynWhere :: UnqualifiedName -> PatSynDir -> [Doc ann]
prettyPatSynWhere _ PatSynBidirectional = []
prettyPatSynWhere _ PatSynUnidirectional = []
prettyPatSynWhere _ (PatSynExplicitBidirectional []) = ["where", spacedBraces mempty]
prettyPatSynWhere name (PatSynExplicitBidirectional matches) =
  ["where" <> hardline <> indent 2 (vsep (map (prettyFunctionMatch name) matches))]

prettyFunctionMatchLines :: UnqualifiedName -> Match -> [Doc ann]
prettyFunctionMatchLines name match =
  case matchRhs match of
    UnguardedRhs {} -> [prettyFunctionMatch name match]
    GuardedRhss _ grhss mWhereDecls ->
      prettyFunctionHead name (matchHeadForm match) (matchPats match)
        : map
          (indent 2)
          ( map prettyGuardedRhsBlock grhss
              <> [prettyWhereClauseBare mWhereDecls | isJust mWhereDecls]
          )

prettyFunctionMatch :: UnqualifiedName -> Match -> Doc ann
prettyFunctionMatch name match =
  prettyFunctionHead name (matchHeadForm match) (matchPats match) <+> prettyRhs (matchRhs match)

prettyFunctionHead :: UnqualifiedName -> MatchHeadForm -> [Pattern] -> Doc ann
prettyFunctionHead name headForm pats =
  case headForm of
    MatchHeadPrefix ->
      hsep (prettyBinderUName name : map prettyPattern pats)
    MatchHeadInfix ->
      case pats of
        lhs : rhsPat : tailPats ->
          let infixHead = prettyPattern lhs <+> prettyInfixOp name <+> prettyPattern rhsPat
           in case tailPats of
                [] -> infixHead
                _ -> hsep (parens infixHead : map prettyPattern tailPats)
        _ ->
          hsep (prettyBinderUName name : map prettyPattern pats)

prettyRhs :: Rhs Expr -> Doc ann
prettyRhs rhs =
  case rhs of
    UnguardedRhs _ body whereDecls
      | isJust whereDecls && exprEndsWithLayoutCase body ->
          "="
            <> hardline
            <> indent 2 (prettyExpr body)
            <> hardline
            <> indent 2 (prettyWhereClauseBare whereDecls)
    UnguardedRhs _ body whereDecls ->
      "=" <+> prettyExpr body <> prettyWhereClause whereDecls
    GuardedRhss _ guards whereDecls
      | guardedRhssNeedLayoutBlock guards whereDecls ->
          hardline <> indent 2 (prettyGuardedRhssBlock guards whereDecls)
    GuardedRhss _ guards whereDecls ->
      hsep
        (map (prettyGuardedRhs "=") guards)
        <> prettyWhereClause whereDecls

prettyWhereClause :: Maybe [Decl] -> Doc ann
prettyWhereClause Nothing = mempty
prettyWhereClause (Just []) = " where" <+> spacedBraces mempty
prettyWhereClause (Just decls)
  | any declContainsMultilineString decls = " where" <+> spacedBraces (prettyInlineDecls decls)
prettyWhereClause (Just decls) =
  hardline <> indent 2 ("where" <> hardline <> indent 2 (vsep (concatMap prettyDeclLines decls)))

prettyWhereClauseBare :: Maybe [Decl] -> Doc ann
prettyWhereClauseBare Nothing = mempty
prettyWhereClauseBare (Just []) = "where" <+> spacedBraces mempty
prettyWhereClauseBare (Just decls)
  | any declContainsMultilineString decls = "where" <+> spacedBraces (prettyInlineDecls decls)
prettyWhereClauseBare (Just decls) =
  "where" <> hardline <> indent 2 (vsep (concatMap prettyDeclLines decls))

-- | Infix type-family heads use @l \`Op\` r@ with a 'NameConId' operator (e.g.
-- @\`And\`@), so 'isSymbolicTypeName' is false; 'TypeHeadInfix' already marks
-- this shape as infix.
typeFamilyInfixAppView :: Type -> Maybe (Name, TypePromotion, Type, Type)
typeFamilyInfixAppView ty =
  case peelTypeAnn ty of
    TInfix lhs op promoted rhs -> Just (op, promoted, lhs, rhs)
    _ -> Nothing

-- | Pretty-print a type. The AST is assumed to already have TParen nodes
-- in the correct positions (inserted by 'addTypeParens').
-- | Check if a type, when pretty-printed, would start with a
-- promotion tick.  This is used to decide whether a promoted list or
-- tuple needs a space after the opening tick to avoid lexer confusion
-- (e.g.  ''[ '[' ... ]'' is invalid, whereas ''[ '[ ... ]'' is valid).
startsWithTick :: Type -> Bool
startsWithTick (TAnn _ sub) = startsWithTick sub
startsWithTick (TList Promoted _) = True
startsWithTick (TCon _ Promoted) = True
startsWithTick (TTuple _ Promoted _) = True
startsWithTick (TApp f _) = startsWithTick f
startsWithTick _ = False

prettyType :: Type -> Doc ann
prettyType ty =
  case ty of
    TAnn _ sub -> prettyType sub
    TVar name -> pretty name
    TCon name promoted ->
      let rendered = renderName name
          base
            | isSymbolicTypeName name = parens (pretty rendered)
            | otherwise = pretty rendered
          promoteTick
            | T.any (== '\'') rendered = "' "
            | otherwise = "'"
       in if promoted == Promoted then promoteTick <> base else base
    TImplicitParam name inner -> pretty name <+> "::" <+> prettyType inner
    TTypeLit lit -> prettyTypeLiteral lit
    TStar -> "*"
    TQuasiQuote quoter body -> prettyQuasiQuote quoter body
    TForall telescope inner ->
      prettyForallTelescope telescope <+> prettyType inner
    -- Before infix detection: required grouping from the parser ('TParen',
    -- @(a :+: b) -> c@, constraints, nested @(c => t)@).
    TParen inner -> parens (prettyType inner)
    TInfix lhs op promoted rhs ->
      prettyType lhs
        <+> (if promoted == Promoted then "'" else mempty)
        <> prettyNameInfixOp op
        <+> prettyType rhs
    TApp f x ->
      prettyType f <+> prettyType x
    TTypeApp f x ->
      prettyType f <+> "@" <> prettyType x
    TFun arrowKind a b ->
      prettyType a <+> prettyArrowKind arrowKind <+> prettyType b
    TTuple tupleFlavor promoted elems ->
      let elemsDoc = hsep (punctuate comma (map prettyType elems))
          tupleDoc = prettyTupleBody tupleFlavor elemsDoc
       in case (promoted, elems) of
            (Promoted, h : _) | startsWithTick h -> "' " <> tupleDoc
            (Promoted, _) -> "'" <> tupleDoc
            _ -> tupleDoc
    TUnboxedSum elems ->
      hsep ["(#", hsep (punctuate " |" (map prettyType elems)), "#)"]
    TList promoted elems ->
      let elemsDoc = hsep (punctuate comma (map prettyType elems))
          listDoc = brackets elemsDoc
       in case (promoted, elems) of
            (Promoted, h : _) | startsWithTick h -> "' " <> listDoc
            (Promoted, _) -> "'" <> listDoc
            _ -> listDoc
    TKindSig ty' kind ->
      prettyType ty' <+> "::" <+> prettyType kind
    TContext constraints inner ->
      case constraints of
        [] -> prettyType inner
        cs -> prettyContext cs <+> "=>" <+> prettyType inner
    TSplice body -> "$" <> prettyExpr body
    TWildcard -> "_"

prettyArrowKind :: ArrowKind -> Doc ann
prettyArrowKind ArrowUnrestricted = "->"
prettyArrowKind ArrowLinear = "%1" <+> "->"
prettyArrowKind (ArrowExplicit ty) = "%" <> prettyType ty <+> "->"

prettyContext :: [Type] -> Doc ann
prettyContext constraints =
  case constraints of
    [] -> mempty
    [single] -> prettyType single
    _ -> parens (hsep (punctuate comma (map prettyType constraints)))

prettyTypeLiteral :: TypeLiteral -> Doc ann
prettyTypeLiteral lit =
  case lit of
    TypeLitInteger _ repr -> pretty repr
    TypeLitSymbol _ repr -> pretty repr
    TypeLitChar _ repr -> pretty repr

-- | Pretty-print a pattern. The AST is assumed to already have PParen nodes
-- in the correct positions (inserted by 'addPatternParens').
prettyPattern :: Pattern -> Doc ann
prettyPattern pat =
  case pat of
    PAnn _ sub -> prettyPattern sub
    PVar name -> prettyBinderUName name
    PTypeBinder binder -> prettyTyVarBinder binder
    PTypeSyntax TypeSyntaxExplicitNamespace ty -> "type" <+> prettyType ty
    PTypeSyntax TypeSyntaxInTerm ty -> prettyType ty
    PWildcard -> "_"
    PLit lit -> prettyLiteral lit
    PQuasiQuote quoter body -> prettyQuasiQuote quoter body
    PTuple Unboxed [] -> "(# #)"
    PTuple tupleFlavor elems -> prettyTupleBody tupleFlavor (hsep (punctuate comma (map prettyPattern elems)))
    PUnboxedSum altIdx arity inner ->
      let slots = [if i == altIdx then prettyPattern inner else mempty | i <- [0 .. arity - 1]]
       in hsep ["(#", hsep (punctuate " |" slots), "#)"]
    PList elems -> brackets (hsep (punctuate comma (map prettyPattern elems)))
    PCon con typeArgs args -> hsep ([prettyPrefixName con] <> map prettyInvisibleTypeArg typeArgs <> map prettyPattern args)
    PInfix lhs op rhs -> prettyPattern lhs <+> prettyNameInfixOp op <+> prettyPattern rhs
    PView viewExpr inner
      | exprEndsWithLayoutCase viewExpr ->
          prettyExpr viewExpr <> hardline <> indent 1 ("->" <+> prettyPattern inner)
    PView viewExpr inner ->
      prettyExpr viewExpr <+> "->" <+> prettyPattern inner
    PAs name inner -> prettyBinderUName name <> "@" <> prettyPattern inner
    PStrict inner -> "!" <> prettyPattern inner
    PIrrefutable inner -> "~" <> prettyPattern inner
    PNegLit lit -> "-" <> prettyLiteral lit
    PParen inner -> parens (prettyPattern inner)
    PRecord con fields hasWildcard ->
      prettyPrefixName con
        <+> braces
          ( hsep
              ( punctuate
                  comma
                  ( [prettyPatternFieldBinding field | field <- fields]
                      ++ [".." | hasWildcard]
                  )
              )
          )
    PTypeSig inner ty -> prettyPattern inner <+> "::" <+> prettyType ty
    PSplice body -> "$" <> prettyExpr body

-- | Pretty print a pattern field binding.
prettyPatternFieldBinding :: RecordField Pattern -> Doc ann
prettyPatternFieldBinding field =
  if recordFieldPun field
    then pretty (recordFieldName field)
    else pretty (recordFieldName field) <+> "=" <+> prettyPattern (recordFieldValue field)

prettyLiteral :: Literal -> Doc ann
prettyLiteral lit =
  case peelLiteralAnn lit of
    LitInt _ _ repr -> pretty repr
    LitFloat _ _ repr -> pretty repr
    LitChar _ repr -> pretty repr
    LitCharHash _ repr -> pretty repr
    LitString _ repr -> pretty repr
    LitStringHash _ repr -> pretty repr
    LitAnn {} -> error "unreachable"

prettyDataDecl :: DataDecl -> Doc ann
prettyDataDecl decl =
  case dataDeclConstructors decl of
    ctors
      | any isGadtCon ctors ->
          headDoc <+> prettyGadtConBlock ctors (dataDeclDeriving decl)
    _ ->
      hsep (headParts <> ctorPart <> derivingParts (dataDeclDeriving decl))
  where
    headParts =
      ["data"]
        <> maybe [] (pure . prettyPragma) (dataDeclCTypePragma decl)
        <> prettyDeclBinderHead (dataDeclContext decl) (dataDeclHead decl)
        <> kindPart
    headDoc = hsep headParts
    kindPart = maybe [] (\k -> ["::", prettyType k]) (dataDeclKind decl)
    ctorPart =
      case dataDeclConstructors decl of
        [] -> []
        ctors -> ["=", hsep (punctuate " |" (map prettyDataCon ctors))]

prettyTypeDataDecl :: DataDecl -> Doc ann
prettyTypeDataDecl decl =
  case dataDeclConstructors decl of
    ctors
      | any isGadtCon ctors -> headDoc <+> prettyGadtConBlock ctors []
    _ -> hsep (headParts <> ctorPart)
  where
    headParts =
      ["type data"]
        <> prettyDeclBinderHead (dataDeclContext decl) (dataDeclHead decl)
        <> kindPart
    headDoc = hsep headParts
    kindPart = maybe [] (\k -> ["::", prettyType k]) (dataDeclKind decl)
    ctorPart =
      case dataDeclConstructors decl of
        [] -> []
        ctors -> ["=", hsep (punctuate " |" (map prettyDataCon ctors))]

isGadtCon :: DataConDecl -> Bool
isGadtCon (DataConAnn _ inner) = isGadtCon inner
isGadtCon (GadtCon {}) = True
isGadtCon _ = False

prettyNewtypeDecl :: NewtypeDecl -> Doc ann
prettyNewtypeDecl decl =
  case newtypeDeclConstructor decl of
    Just ctor
      | isGadtCon ctor ->
          headDoc <+> prettyGadtConBlock [ctor] (newtypeDeclDeriving decl)
    _ ->
      hsep (headParts <> ctorPart <> derivingParts (newtypeDeclDeriving decl))
  where
    headParts =
      ["newtype"]
        <> maybe [] (pure . prettyPragma) (newtypeDeclCTypePragma decl)
        <> prettyDeclBinderHead (newtypeDeclContext decl) (newtypeDeclHead decl)
        <> kindPart
    headDoc = hsep headParts
    kindPart = maybe [] (\k -> ["::", prettyType k]) (newtypeDeclKind decl)
    ctorPart =
      case newtypeDeclConstructor decl of
        Nothing -> []
        Just ctor -> ["=", prettyDataCon ctor]

derivingParts :: [DerivingClause] -> [Doc ann]
derivingParts = concatMap derivingPart

prettyGadtConBlock :: [DataConDecl] -> [DerivingClause] -> Doc ann
prettyGadtConBlock ctors derivingClauses =
  "where"
    <> hardline
    <> indent 2 (vsep (map prettyDataCon ctors <> derivingParts derivingClauses))

derivingPart :: DerivingClause -> [Doc ann]
derivingPart (DerivingClause strategy classes) =
  ["deriving"] <> strategyPart strategy <> classesPart classes <> viaPart strategy
  where
    strategyPart Nothing = []
    strategyPart (Just (DerivingVia _)) = []
    strategyPart (Just strategy') = [prettyDerivingStrategy strategy']

    classesPart (Left name) = [prettyName name]
    classesPart (Right []) = ["()"]
    classesPart (Right classTypes) = [parens (hsep (punctuate comma (map prettyType classTypes)))]

    viaPart (Just (DerivingVia ty)) = ["via", prettyType ty]
    viaPart _ = []

prettyDeclBinderHead :: [Type] -> BinderHead UnqualifiedName -> [Doc ann]
prettyDeclBinderHead constraints head' =
  contextPrefix constraints
    <> prettyDeclHeadNameAndParams head'
  where
    prettyDeclHeadNameAndParams binderHead =
      case binderHead of
        PrefixBinderHead name params ->
          [prettyConstructorUName name] <> map prettyTyVarBinder params
        InfixBinderHead lhs name rhs tailPrms ->
          let infixHead = prettyTyVarBinder lhs <+> prettyInfixOp name <+> prettyTyVarBinder rhs
           in case tailPrms of
                [] -> [infixHead]
                _ -> parens infixHead : map prettyTyVarBinder tailPrms

prettyTyVarBinder :: TyVarBinder -> Doc ann
prettyTyVarBinder binder =
  visibleDoc
  where
    coreDoc =
      case (tyVarBinderSpecificity binder, tyVarBinderKind binder) of
        (TyVarBInferred, Nothing) -> braces (pretty (tyVarBinderName binder))
        (TyVarBInferred, Just kind) -> braces (pretty (tyVarBinderName binder) <+> "::" <+> prettyType kind)
        (TyVarBSpecified, Nothing) -> pretty (tyVarBinderName binder)
        (TyVarBSpecified, Just kind) -> parens (pretty (tyVarBinderName binder) <+> "::" <+> prettyType kind)
    visibleDoc =
      case tyVarBinderVisibility binder of
        TyVarBVisible -> coreDoc
        TyVarBInvisible -> "@" <> coreDoc

contextPrefix :: [Type] -> [Doc ann]
contextPrefix constraints =
  case constraints of
    [] -> []
    _ -> [prettyContext constraints, "=>"]

forallTyVarBinderPrefix :: [TyVarBinder] -> [Doc ann]
forallTyVarBinderPrefix [] = []
forallTyVarBinderPrefix binders = ["forall", hsep (map prettyTyVarBinder binders) <> "."]

prettyForallTelescope :: ForallTelescope -> Doc ann
prettyForallTelescope telescope =
  "forall"
    <+> hsep (map prettyTyVarBinder (forallTelescopeBinders telescope))
    <> case forallTelescopeVisibility telescope of
      ForallInvisible -> "."
      ForallVisible -> " ->"

prettyInvisibleTypeArg :: Type -> Doc ann
prettyInvisibleTypeArg ty = "@" <> prettyType ty

prettyDataCon :: DataConDecl -> Doc ann
prettyDataCon ctor =
  case ctor of
    DataConAnn _ inner -> prettyDataCon inner
    PrefixCon forallVars constraints name fields ->
      hsep (dataConQualifierPrefix forallVars constraints <> [prettyConstructorUName name] <> map prettyBangType fields)
    InfixCon forallVars constraints lhs op rhs ->
      hsep
        ( dataConQualifierPrefix forallVars constraints
            <> [prettyBangType lhs, prettyInfixOp op, prettyBangType rhs]
        )
    RecordCon forallVars constraints name fields ->
      hsep (dataConQualifierPrefix forallVars constraints <> [prettyConstructorUName name])
        <+> braces (prettyRecordFields fields)
    GadtCon foralls constraints names body ->
      prettyGadtCon foralls constraints names body
    TupleCon forallVars constraints Boxed fields ->
      hsep (dataConQualifierPrefix forallVars constraints)
        <+> parens (hsep (punctuate comma (map prettyBangType fields)))
    TupleCon forallVars constraints Unboxed fields ->
      hsep (dataConQualifierPrefix forallVars constraints)
        <+> "(#"
        <+> hsep (punctuate comma (map prettyBangType fields))
        <+> "#)"
    UnboxedSumCon forallVars constraints pos arity field ->
      hsep (dataConQualifierPrefix forallVars constraints)
        <+> "(#"
        <+> hsep (replicate (pos - 1) "|" <> [prettyBangType field] <> replicate (arity - pos) "|")
        <+> "#)"
    ListCon forallVars constraints ->
      hsep (dataConQualifierPrefix forallVars constraints <> ["[]"])

prettyGadtCon :: [ForallTelescope] -> [Type] -> [UnqualifiedName] -> GadtBody -> Doc ann
prettyGadtCon forallBinders constraints names body =
  hsep
    ( [hsep (punctuate comma (map prettyConstructorUName names)), "::"]
        <> forallPart
        <> contextPart
        <> [prettyGadtBody body]
    )
  where
    forallPart = map prettyForallTelescope forallBinders
    contextPart
      | null constraints = []
      | otherwise = [prettyContext constraints, "=>"]

prettyGadtBody :: GadtBody -> Doc ann
prettyGadtBody body =
  case body of
    GadtPrefixBody args resultTy ->
      case args of
        [] -> prettyType resultTy
        _ ->
          hsep $
            concatMap (\(bt, ak) -> [prettyBangType bt, prettyArrowKind ak]) args
              ++ [prettyType resultTy]
    GadtRecordBody fields resultTy ->
      braces (prettyRecordFields fields) <+> "->" <+> prettyType resultTy

prettyRecordFields :: [FieldDecl] -> Doc ann
prettyRecordFields fields =
  hsep
    ( punctuate
        comma
        [ hsep $
            [hsep (punctuate comma (map prettyFieldName (fieldNames fld)))]
              <> maybe [] (\mult -> ["%" <> prettyType mult]) (fieldMultiplicity fld)
              <> [ "::",
                   prettyRecordFieldBangType (fieldType fld)
                 ]
        | fld <- fields
        ]
    )
  where
    prettyFieldName :: UnqualifiedName -> Doc ann
    prettyFieldName = prettyFunctionBinder

dataConQualifierPrefix :: [TyVarBinder] -> [Type] -> [Doc ann]
dataConQualifierPrefix forallVars constraints = forallTyVarBinderPrefix forallVars <> contextPrefix constraints

-- | Pretty print a BangType. The type already has TParen nodes where needed.
prettyBangType :: BangType -> Doc ann
prettyBangType bt =
  hsep (map prettyPragma (bangPragmas bt) <> [strictOrLazyDoc])
  where
    strictOrLazyDoc
      | bangStrict bt = "!" <> prettyType (bangType bt)
      | bangLazy bt = "~" <> prettyType (bangType bt)
      | otherwise = prettyType (bangType bt)

prettyRecordFieldBangType :: BangType -> Doc ann
prettyRecordFieldBangType bt =
  hsep (map prettyPragma (bangPragmas bt) <> [strictOrLazyDoc])
  where
    strictOrLazyDoc
      | bangStrict bt = "!" <> prettyType (bangType bt)
      | bangLazy bt = "~" <> prettyType (bangType bt)
      | otherwise = prettyType (bangType bt)

prettyClassDecl :: ClassDecl -> Doc ann
prettyClassDecl decl =
  let headDoc =
        hsep
          ( ["class"]
              <> maybeContextPrefix (classDeclContext decl)
              <> prettyNamedBinderHead (classDeclHead decl)
              <> prettyClassFundeps (classDeclFundeps decl)
          )
   in case classDeclItems decl of
        [] -> headDoc
        items ->
          headDoc
            <+> "where"
            <> hardline
            <> indent 2 (vsep (concatMap prettyClassItemLines items))

prettyClassFundeps :: [FunctionalDependency] -> [Doc ann]
prettyClassFundeps deps =
  case deps of
    [] -> []
    _ -> ["|", hsep (punctuate comma (map prettyFunctionalDependency deps))]

prettyFunctionalDependency :: FunctionalDependency -> Doc ann
prettyFunctionalDependency dep =
  hsep
    [ hsep (map pretty (functionalDependencyDeterminers dep)),
      "->",
      hsep (map pretty (functionalDependencyDetermined dep))
    ]

prettyTypeFamilyInjectivity :: TypeFamilyInjectivity -> Doc ann
prettyTypeFamilyInjectivity injectivity =
  hsep
    [ pretty (typeFamilyInjectivityResult injectivity),
      "->",
      hsep (map pretty (typeFamilyInjectivityDetermined injectivity))
    ]

maybeContextPrefix :: Maybe [Type] -> [Doc ann]
maybeContextPrefix maybeConstraints =
  case maybeConstraints of
    Nothing -> []
    Just constraints -> [prettyContext constraints, "=>"]

prettyClassItem :: ClassDeclItem -> Doc ann
prettyClassItem item =
  case item of
    ClassItemAnn _ sub -> prettyClassItem sub
    ClassItemTypeSig names ty -> hsep [hsep (punctuate comma (map prettyBinderName names)), "::", prettyType ty]
    ClassItemDefaultSig name ty -> hsep ["default", prettyBinderName name, "::", prettyType ty]
    ClassItemFixity assoc mNamespace prec ops ->
      hsep
        ( [prettyFixityAssoc assoc]
            <> maybe [] (pure . pretty . show) prec
            <> maybe [] (pure . prettyNamespace) mNamespace
            <> punctuate comma (map prettyInfixOp ops)
        )
    ClassItemDefault valueDecl -> prettyValueDeclSingleLine valueDecl
    ClassItemTypeFamilyDecl tf -> prettyAssocTypeFamilyDecl tf
    ClassItemDataFamilyDecl df -> prettyAssocDataFamilyDecl df
    ClassItemDefaultTypeInst tfi -> prettyDefaultTypeInst tfi
    ClassItemPragma pragma -> prettyPragma pragma

prettyClassItemLines :: ClassDeclItem -> [Doc ann]
prettyClassItemLines item =
  case item of
    ClassItemAnn _ inner -> prettyClassItemLines inner
    ClassItemDefault valueDecl -> prettyValueDeclLines valueDecl
    _ -> [prettyClassItem item]

prettyInstanceDecl :: InstanceDecl -> Doc ann
prettyInstanceDecl decl =
  let headDoc =
        hsep
          ( ["instance"]
              <> map prettyPragma (instanceDeclPragmas decl)
              <> maybe [] (\w -> [prettyPragma w]) (instanceDeclWarning decl)
              <> forallTyVarBinderPrefix (instanceDeclForall decl)
              <> contextPrefix (instanceDeclContext decl)
              <> [prettyType (instanceDeclHead decl)]
          )
   in case instanceDeclItems decl of
        [] -> headDoc
        items ->
          headDoc
            <+> "where"
            <> hardline
            <> indent 2 (vsep (concatMap prettyInstanceItemLines items))

prettyStandaloneDeriving :: StandaloneDerivingDecl -> Doc ann
prettyStandaloneDeriving decl =
  hsep
    ( ["deriving"]
        <> maybe [] (\s -> [prettyDerivingStrategy s]) (standaloneDerivingStrategy decl)
        <> ["instance"]
        <> map prettyPragma (standaloneDerivingPragmas decl)
        <> maybe [] (\w -> [prettyPragma w]) (standaloneDerivingWarning decl)
        <> forallTyVarBinderPrefix (standaloneDerivingForall decl)
        <> contextPrefix (standaloneDerivingContext decl)
        <> [prettyType (standaloneDerivingHead decl)]
    )

prettyPragma :: Pragma -> Doc ann
prettyPragma pragma
  | not (T.null (pragmaRawText pragma)) = pretty (pragmaRawText pragma)
  | otherwise = prettyPragmaType (pragmaType pragma)

prettyPragmaType :: PragmaType -> Doc ann
prettyPragmaType pt =
  case pt of
    PragmaLanguage settings -> "{-# LANGUAGE " <> hsep (punctuate comma (map (pretty . extensionSettingName) settings)) <> " #-}"
    PragmaInstanceOverlap overlapPragma ->
      case overlapPragma of
        Overlapping -> "{-# OVERLAPPING #-}"
        Overlappable -> "{-# OVERLAPPABLE #-}"
        Overlaps -> "{-# OVERLAPS #-}"
        Incoherent -> "{-# INCOHERENT #-}"
    PragmaWarning msg -> "{-# WARNING " <> pretty (show msg) <> " #-}"
    PragmaDeprecated msg -> "{-# DEPRECATED " <> pretty (show msg) <> " #-}"
    PragmaInline kind body -> "{-# " <> pretty kind <> " " <> pretty body <> " #-}"
    PragmaUnpack unpackKind ->
      case unpackKind of
        UnpackPragma -> "{-# UNPACK #-}"
        NoUnpackPragma -> "{-# NOUNPACK #-}"
    PragmaSource sourceText _ -> "{-# SOURCE " <> pretty sourceText <> " #-}"
    PragmaSCC label -> "{-# SCC " <> pretty label <> " #-}"
    PragmaUnknown text -> pretty text

prettyDerivingStrategy :: DerivingStrategy -> Doc ann
prettyDerivingStrategy strategy =
  case strategy of
    DerivingStock -> "stock"
    DerivingNewtype -> "newtype"
    DerivingAnyclass -> "anyclass"
    DerivingVia ty -> "via" <+> prettyType ty

prettyInstanceItem :: InstanceDeclItem -> Doc ann
prettyInstanceItem item =
  case item of
    InstanceItemAnn _ inner -> prettyInstanceItem inner
    InstanceItemBind valueDecl -> prettyValueDeclSingleLine valueDecl
    InstanceItemTypeSig names ty -> hsep [hsep (punctuate comma (map prettyBinderName names)), "::", prettyType ty]
    InstanceItemFixity assoc mNamespace prec ops ->
      hsep
        ( [prettyFixityAssoc assoc]
            <> maybe [] (pure . pretty . show) prec
            <> maybe [] (pure . prettyNamespace) mNamespace
            <> punctuate comma (map prettyInfixOp ops)
        )
    InstanceItemTypeFamilyInst tfi -> prettyInstTypeFamilyInst tfi
    InstanceItemDataFamilyInst dfi -> prettyInstDataFamilyInst dfi
    InstanceItemPragma pragma -> prettyPragma pragma

prettyInstanceItemLines :: InstanceDeclItem -> [Doc ann]
prettyInstanceItemLines item =
  case item of
    InstanceItemAnn _ inner -> prettyInstanceItemLines inner
    InstanceItemBind valueDecl -> prettyValueDeclLines valueDecl
    _ -> [prettyInstanceItem item]

prettyFixityAssoc :: FixityAssoc -> Doc ann
prettyFixityAssoc assoc =
  case assoc of
    Infix -> "infix"
    InfixL -> "infixl"
    InfixR -> "infixr"

prettyForeignDecl :: ForeignDecl -> Doc ann
prettyForeignDecl decl =
  hsep . catMaybes $
    [ Just "foreign",
      Just (prettyDirection (foreignDirection decl)),
      Just (prettyCallConv (foreignCallConv decl)),
      prettySafety <$> foreignSafety decl,
      prettyForeignEntity (foreignEntity decl),
      Just (prettyBinderName (foreignName decl)),
      Just "::",
      Just (prettyType (foreignType decl))
    ]

prettyDirection :: ForeignDirection -> Doc ann
prettyDirection direction =
  case direction of
    ForeignImport -> "import"
    ForeignExport -> "export"

prettyCallConv :: CallConv -> Doc ann
prettyCallConv cc =
  case cc of
    CCall -> "ccall"
    StdCall -> "stdcall"
    CApi -> "capi"
    CPrim -> "prim"

prettySafety :: ForeignSafety -> Doc ann
prettySafety safety =
  case safety of
    Safe -> "safe"
    Unsafe -> "unsafe"
    Interruptible -> "interruptible"

prettyForeignEntity :: ForeignEntitySpec -> Maybe (Doc ann)
prettyForeignEntity spec =
  case spec of
    ForeignEntityOmitted -> Nothing
    ForeignEntityDynamic -> Just (quoted "dynamic")
    ForeignEntityWrapper -> Just (quoted "wrapper")
    ForeignEntityStatic Nothing -> Just (quoted "static")
    ForeignEntityStatic (Just name) -> Just (quoted ("static " <> name))
    ForeignEntityAddress Nothing -> Just (quoted "&")
    ForeignEntityAddress (Just name) -> Just (quoted ("&" <> name))
    ForeignEntityNamed name -> Just (quoted name)

prettyInfixOp :: UnqualifiedName -> Doc ann
prettyInfixOp name
  | isSymbolicUName name = pretty (renderUnqualifiedName name)
  | otherwise = "`" <> pretty (renderUnqualifiedName name) <> "`"

prettyNameInfixOp :: Name -> Doc ann
prettyNameInfixOp name
  | isSymbolicName name = pretty (renderName name)
  | otherwise = "`" <> pretty (renderName name) <> "`"

prettyPrefixName :: Name -> Doc ann
prettyPrefixName name
  | isSymbolicName name = parens (pretty rendered)
  | otherwise = pretty rendered
  where
    rendered = renderName name

isSymbolicUName :: UnqualifiedName -> Bool
isSymbolicUName name =
  case unqualifiedNameType name of
    NameVarSym -> True
    NameConSym -> True
    _ -> False

isSymbolicName :: Name -> Bool
isSymbolicName name =
  case nameType name of
    NameVarSym -> True
    NameConSym -> True
    _ -> False

isSymbolicTypeName :: Name -> Bool
isSymbolicTypeName = isSymbolicName

prettyFunctionBinder :: UnqualifiedName -> Doc ann
prettyFunctionBinder name
  | unqualifiedNameType name == NameVarSym || unqualifiedNameType name == NameConSym =
      let rendered = renderUnqualifiedName name
       in if startsWithHash rendered
            then parens (" " <> pretty rendered <> " ")
            else parens (pretty rendered)
  | otherwise = pretty (renderUnqualifiedName name)
  where
    startsWithHash t =
      case T.uncons t of
        Just ('#', _) -> True
        _ -> False

prettyBinderName :: UnqualifiedName -> Doc ann
prettyBinderName = prettyFunctionBinder

prettyBinderUName :: UnqualifiedName -> Doc ann
prettyBinderUName = prettyFunctionBinder

prettyName :: Name -> Doc ann
prettyName name
  | nameType name == NameVarSym || nameType name == NameConSym =
      let rendered = renderName name
       in if startsWithHash rendered
            then parens (" " <> pretty rendered <> " ")
            else parens (pretty rendered)
  | otherwise = pretty (renderName name)
  where
    startsWithHash t =
      case T.uncons t of
        Just ('#', _) -> True
        _ -> False

prettyBundledMemberName :: Name -> Doc ann
prettyBundledMemberName name
  | isHashLeadingSymbolicName name = parens (" " <> pretty (renderName name) <> " ")
  | otherwise = prettyName name

isHashLeadingSymbolicName :: Name -> Bool
isHashLeadingSymbolicName name =
  isSymbolicName name
    && case nameQualifier name of
      Nothing -> case T.uncons (nameText name) of
        Just ('#', _) -> True
        _ -> False
      Just _ -> False

prettyConstructorUName :: UnqualifiedName -> Doc ann
prettyConstructorUName name
  | isSymbolicUName name = parens (pretty (renderUnqualifiedName name))
  | otherwise = pretty (renderUnqualifiedName name)

-- | Pretty-print an expression. The AST is assumed to already have EParen
-- nodes in the correct positions (inserted by 'addExprParens').
prettyExpr :: Expr -> Doc ann
prettyExpr expr =
  case expr of
    EApp {} -> prettyApp expr
    ETypeApp fn ty ->
      if exprEndsWithLayoutCase fn
        then prettyExpr fn <> hardline <> " " <> "@" <> prettyType ty
        else prettyExpr fn <+> "@" <> prettyType ty
    EVar name -> prettyName name
    ETypeSyntax TypeSyntaxExplicitNamespace ty -> "type" <+> prettyType ty
    ETypeSyntax TypeSyntaxInTerm ty -> prettyType ty
    EInt _ _ repr -> pretty repr
    EFloat _ _ repr -> pretty repr
    EChar _ repr -> pretty repr
    ECharHash _ repr -> pretty repr
    EString _ repr -> pretty repr
    EStringHash _ repr -> pretty repr
    EOverloadedLabel _ raw -> pretty (" " <> raw)
    EQuasiQuote quoter body -> prettyQuasiQuote quoter body
    ETHExpQuote body -> "[|" <+> prettyExpr body <+> "|]"
    ETHTypedQuote body -> "[||" <+> prettyExpr body <+> "||]"
    ETHDeclQuote decls
      | any declContainsLayoutCase decls ->
          "[d|"
            <> hardline
            <> indent 2 (vsep (concatMap prettyDeclLines decls))
            <> hardline
            <> indent 2 "|]"
      | otherwise -> "[d|" <+> prettyInlineDecls decls <+> "|]"
    ETHTypeQuote ty -> "[t|" <+> prettyType ty <+> "|]"
    ETHPatQuote pat -> "[p|" <+> prettyPattern pat <+> "|]"
    ETHNameQuote body -> "' " <> prettyExpr body
    ETHTypeNameQuote ty -> "'' " <> prettyType ty
    ETHSplice body -> "$" <> prettyExpr body
    ETHTypedSplice body -> "$$" <> prettyExpr body
    EIf cond yes no ->
      "if" <+> prettyExpr cond <+> "then" <+> prettyExpr yes <+> "else" <+> prettyExpr no
    EMultiWayIf rhss ->
      "if"
        <+> "{"
        <+> prettyMultiWayIfRhss rhss
        <+> "}"
    ELambdaPats pats body ->
      "\\" <+> hsep (map prettyPattern pats) <+> "->" <+> prettyExpr body
    ELambdaCase alts ->
      "\\" <> "case" <> prettyCaseLayout (map prettyCaseAlt alts)
    ELambdaCases alts ->
      "\\" <> "cases" <> prettyCaseLayout (map prettyLambdaCaseAlt alts)
    EInfix lhs op rhs ->
      if exprEndsWithLayoutCase lhs
        then prettyExpr lhs <> hardline <> " " <> prettyNameInfixOp op <+> prettyExpr rhs
        else prettyExpr lhs <+> prettyNameInfixOp op <+> prettyExpr rhs
    ENegate inner -> "-" <> prettyExpr inner
    ESectionL lhs op ->
      if exprEndsWithLayoutCase lhs
        then prettyExpr lhs <> hardline <> " " <> prettyNameInfixOp op
        else prettyExpr lhs <+> prettyNameInfixOp op
    ESectionR op rhs -> prettyNameInfixOp op <+> prettyExpr rhs
    ELetDecls decls body ->
      case decls of
        [] -> prettyLetDecls decls <+> "in" <+> prettyExpr body
        _ -> align (prettyLetDecls decls <> hardline <> "in" <+> prettyExpr body)
    ECase scrutinee alts ->
      prettyCaseExpr prettyCaseLayout scrutinee alts
    EDo stmts flavor ->
      prettyDoFlavor flavor <> prettyDoLayout prettyDoStmt doStmtContainsLayoutCase stmts
    EListComp body quals ->
      brackets
        ( let qualifiers = prettyCommaSeparated prettyCompStmt compStmtContainsLayoutCase quals
           in if exprEndsWithLayoutCase body
                then prettyExpr body <> hardline <> " |" <+> qualifiers
                else prettyExpr body <+> "|" <+> qualifiers
        )
    EListCompParallel body qualifierGroups ->
      brackets
        ( let qualifiers =
                prettyBarSeparated
                  [ (prettyCommaSeparated prettyCompStmt compStmtContainsLayoutCase group, any compStmtContainsLayoutCase group)
                  | group <- qualifierGroups
                  ]
           in if exprEndsWithLayoutCase body
                then prettyExpr body <> hardline <> " |" <+> qualifiers
                else prettyExpr body <+> "|" <+> qualifiers
        )
    EArithSeq seqInfo -> prettyArithSeq seqInfo
    ERecordCon name fields hasWildcard ->
      prettyPrefixName name <+> braces (hsep (punctuate comma (map prettyBinding fields ++ [".." | hasWildcard])))
    ERecordUpd base fields ->
      prettyExpr base <+> braces (hsep (punctuate comma (map prettyBinding fields)))
    EGetField base field ->
      prettyExpr base <> "." <> prettyName field
    EGetFieldProjection fields ->
      "." <> mconcat (punctuate "." (map prettyName fields))
    ETypeSig inner ty ->
      if exprEndsWithLayoutCase inner
        then prettyExpr inner <> hardline <> " " <> "::" <+> prettyType ty
        else prettyExpr inner <+> "::" <+> prettyType ty
    EParen inner -> case inner of
      ECase scrutinee alts ->
        parens (prettyCaseExpr prettyCaseLayoutAligned scrutinee alts)
      -- ESectionR with a '#'-starting op renders as "(# ...)" which the lexer
      -- reads as TkSpecialUnboxedLParen when UnboxedTuples/UnboxedSums is on.
      -- A leading space produces "( # ...)" which is unambiguous.
      ESectionR op _ | T.isPrefixOf "#" (renderName op) -> parens (" " <> prettyExpr inner)
      -- ESectionL with a '#'-ending op renders as "(x #)" which the lexer
      -- reads as TkSpecialUnboxedRParen when UnboxedTuples/UnboxedSums is on.
      -- A trailing space produces "(x # )" which is unambiguous.
      ESectionL _ op | T.isSuffixOf "#" (renderName op) -> parens (prettyExpr inner <> " ")
      _ -> parens (prettyExpr inner)
    EList values -> brackets (prettyCommaSeparated prettyExpr exprEndsWithLayoutCase values)
    ETuple Unboxed [] -> "(# #)"
    ETuple tupleFlavor values ->
      prettyTupleBody tupleFlavor (prettyCommaSeparated (maybe mempty prettyExpr) (maybe False exprEndsWithLayoutCase) values)
    EUnboxedSum altIdx arity inner ->
      let slots = [(if i == altIdx then prettyExpr inner else mempty, i == altIdx && exprEndsWithLayoutCase inner) | i <- [0 .. arity - 1]]
       in hsep ["(#", prettyBarSeparated slots, "#)"]
    EProc pat body ->
      "proc" <+> prettyPattern pat <+> "->" <+> prettyCmd body
    EPragma pragma inner ->
      prettyPragma pragma <+> prettyExpr inner
    EAnn _ sub -> prettyExpr sub

prettyApp :: Expr -> Doc ann
prettyApp expr =
  let (fn, args) = flattenApps expr
      docs = prettyExpr fn : map prettyExpr args
   in if any exprEndsWithLayoutCase (init args)
        then vsep (prettyExpr fn : map (indent 2 . prettyExpr) args)
        else hsep docs
  where
    flattenApps = go []
    go args (EAnn _ sub) = go args sub
    go args (EApp fn arg) = go (arg : args) fn
    go args root = (root, args)

prettySemiBlock :: (a -> Doc ann) -> (a -> Bool) -> [a] -> Doc ann
prettySemiBlock render endsWithLayout items =
  "{" <+> prettySemiSeparated render endsWithLayout items <+> "}"

prettySemiSeparated :: (a -> Doc ann) -> (a -> Bool) -> [a] -> Doc ann
prettySemiSeparated render endsWithLayout items =
  case map render items of
    [] -> mempty
    rendered@(firstItem : restItems)
      | any endsWithLayout (init items) -> vsep (firstItem : map (semi <+>) restItems)
      | otherwise -> hsep (punctuate semi rendered)

prettyCommaSeparated :: (a -> Doc ann) -> (a -> Bool) -> [a] -> Doc ann
prettyCommaSeparated render endsWithLayout items =
  case map render items of
    [] -> mempty
    rendered@(firstItem : restItems)
      | any endsWithLayout (init items) -> vsep (firstItem : map ((" " <> comma) <+>) restItems)
      | otherwise -> hsep (punctuate comma rendered)

prettyBarSeparated :: [(Doc ann, Bool)] -> Doc ann
prettyBarSeparated items =
  case items of
    [] -> mempty
    (firstItem, _) : restItems
      | any snd (init items) ->
          vsep (firstItem : map (((" " <> "|") <+>) . fst) restItems)
      | otherwise -> hsep (punctuate " |" (map fst items))

prettyTupleBody :: TupleFlavor -> Doc ann -> Doc ann
prettyTupleBody tupleFlavor inner =
  case tupleFlavor of
    Boxed -> parens inner
    Unboxed -> hsep ["(#", inner, "#)"]

prettyBinding :: RecordField Expr -> Doc ann
prettyBinding field =
  if recordFieldPun field
    then prettyName (recordFieldName field)
    else prettyName (recordFieldName field) <+> "=" <+> prettyExpr (recordFieldValue field)

prettyCaseAltWith :: (body -> Doc ann) -> (body -> Bool) -> CaseAlt body -> Doc ann
prettyCaseAltWith prettyBody bodyEndsWithLayoutCase (CaseAlt _ pat rhs) =
  case rhs of
    UnguardedRhs _ body whereDecls
      | isJust whereDecls && bodyEndsWithLayoutCase body ->
          prettyPattern pat
            <+> "->"
            <> hardline
            <> indent 2 (prettyBody body)
            <> hardline
            <> indent 2 (prettyWhereClauseBare whereDecls)
    UnguardedRhs _ body whereDecls ->
      prettyPattern pat
        <+> "->"
        <+> prettyBody body
        <> prettyWhereClause whereDecls
    GuardedRhss _ grhss whereDecls ->
      if guardedBodiesNeedLayoutBlock bodyEndsWithLayoutCase grhss whereDecls
        then
          prettyPattern pat
            <> hardline
            <> indent 2 (vsep (map (prettyCaseGuardedRhsBlock prettyBody bodyEndsWithLayoutCase) grhss))
            <> case whereDecls of
              Nothing -> mempty
              Just _ -> hardline <> indent 2 (prettyWhereClauseBare whereDecls)
        else
          hsep
            [ prettyPattern pat,
              hsep (map (prettyCaseGuardedRhs prettyBody) grhss)
            ]
            <> prettyWhereClause whereDecls

prettyCaseAlt :: CaseAlt Expr -> Doc ann
prettyCaseAlt = prettyCaseAltWith prettyExpr exprEndsWithLayoutCase

prettyGuardedRhs :: Text -> GuardedRhs Expr -> Doc ann
prettyGuardedRhs arrow grhs =
  let guards = guardedRhsGuards grhs
      body = guardedRhsBody grhs
   in if any guardQualifierEndsWithLayoutCase guards
        then
          "|"
            <+> prettyGuardQualifiersLayout guards
            <> hardline
            <> " "
            <> pretty arrow
            <+> prettyExpr body
        else
          "|"
            <+> prettyGuardQualifiers guards
            <+> pretty arrow
            <+> prettyExpr body

prettyGuardedRhsBodyBlock :: Text -> GuardedRhs Expr -> Doc ann
prettyGuardedRhsBodyBlock arrow grhs =
  let guards = guardedRhsGuards grhs
      body = guardedRhsBody grhs
   in if any guardQualifierEndsWithLayoutCase guards
        then
          "|"
            <+> prettyGuardQualifiersLayout guards
            <> hardline
            <> " "
            <> pretty arrow
            <+> prettyExpr body
        else
          if exprEndsWithLayoutCase body
            then
              "|"
                <+> prettyGuardQualifiers guards
                <+> pretty arrow
                <> hardline
                <> indent 2 (prettyExpr body)
            else prettyGuardedRhs arrow grhs

prettyCaseGuardedRhs :: (body -> Doc ann) -> GuardedRhs body -> Doc ann
prettyCaseGuardedRhs prettyBody grhs =
  "|"
    <+> hsep (punctuate comma (map prettyGuardQualifier (guardedRhsGuards grhs)))
    <+> "->"
    <+> prettyBody (guardedRhsBody grhs)

prettyCaseGuardedRhsBlock :: (body -> Doc ann) -> (body -> Bool) -> GuardedRhs body -> Doc ann
prettyCaseGuardedRhsBlock prettyBody bodyEndsWithLayoutCase grhs =
  let guards = guardedRhsGuards grhs
      body = guardedRhsBody grhs
   in if any guardQualifierEndsWithLayoutCase guards
        then
          "|"
            <+> prettyGuardQualifiersLayout guards
            <> hardline
            <> " "
            <> "->"
            <+> prettyBody body
        else
          if bodyEndsWithLayoutCase body
            then
              "|"
                <+> hsep (punctuate comma (map prettyGuardQualifier guards))
                <+> "->"
                <> hardline
                <> indent 2 (prettyBody body)
            else prettyCaseGuardedRhs prettyBody grhs

prettyMultiWayIfRhss :: [GuardedRhs Expr] -> Doc ann
prettyMultiWayIfRhss rhss =
  if guardedBodiesNeedLayoutBlock exprEndsWithLayoutCase rhss Nothing
    then vsep (map (prettyGuardedRhsBodyBlock "->") rhss)
    else hsep (map (prettyGuardedRhs "->") rhss)

prettyGuardQualifiers :: [GuardQualifier] -> Doc ann
prettyGuardQualifiers qualifiers =
  hsep (punctuate comma (map prettyGuardQualifier qualifiers))

prettyGuardQualifiersLayout :: [GuardQualifier] -> Doc ann
prettyGuardQualifiersLayout qualifiers =
  case map prettyGuardQualifier qualifiers of
    [] -> mempty
    firstQualifier : restQualifiers ->
      vsep (firstQualifier : map (indent 2 . (comma <+>)) restQualifiers)

prettyCaseExpr :: ([Doc ann] -> Doc ann) -> Expr -> [CaseAlt Expr] -> Doc ann
prettyCaseExpr layout scrutinee alts =
  "case"
    <+> prettyExpr scrutinee
    <+> "of"
    <> layout (map prettyCaseAlt alts)

prettyCaseLayout :: [Doc ann] -> Doc ann
prettyCaseLayout [] = " " <> spacedBraces mempty
prettyCaseLayout alts = hardline <> indent 2 (vsep alts)

prettyCaseLayoutAligned :: [Doc ann] -> Doc ann
prettyCaseLayoutAligned [] = " " <> spacedBraces mempty
prettyCaseLayoutAligned alts = hang 2 (hardline <> vsep alts)

prettyLambdaCaseAlt :: LambdaCaseAlt -> Doc ann
prettyLambdaCaseAlt (LambdaCaseAlt _ pats rhs) =
  case rhs of
    UnguardedRhs _ body whereDecls
      | isJust whereDecls && exprEndsWithLayoutCase body ->
          hsep (map prettyPattern pats)
            <+> "->"
            <> hardline
            <> indent 2 (prettyExpr body)
            <> hardline
            <> indent 2 (prettyWhereClauseBare whereDecls)
    UnguardedRhs _ body whereDecls ->
      hsep (map prettyPattern pats)
        <+> "->"
        <+> prettyExpr body
        <> prettyWhereClause whereDecls
    GuardedRhss _ grhss whereDecls ->
      if guardedBodiesNeedLayoutBlock exprEndsWithLayoutCase grhss whereDecls
        then
          hsep (map prettyPattern pats)
            <> hardline
            <> indent 2 (vsep (map (prettyCaseGuardedRhsBlock prettyExpr exprEndsWithLayoutCase) grhss))
            <> case whereDecls of
              Nothing -> mempty
              Just _ -> hardline <> indent 2 (prettyWhereClauseBare whereDecls)
        else
          hsep
            [ hsep (map prettyPattern pats),
              hsep (map (prettyCaseGuardedRhs prettyExpr) grhss)
            ]
            <> prettyWhereClause whereDecls

prettyGuardQualifier :: GuardQualifier -> Doc ann
prettyGuardQualifier qualifier =
  case qualifier of
    GuardAnn _ inner -> prettyGuardQualifier inner
    GuardExpr expr -> prettyExpr expr
    GuardPat pat expr -> prettyPattern pat <+> "<-" <+> prettyExpr expr
    GuardLet decls -> prettyLetDecls decls

prettyLetDecls :: [Decl] -> Doc ann
prettyLetDecls decls
  | null decls = "let" <+> spacedBraces mempty
  | otherwise =
      "let" <> hardline <> indent 2 (vsep (concatMap prettyDeclLines decls))

prettyDoFlavor :: DoFlavor -> Doc ann
prettyDoFlavor DoPlain = "do"
prettyDoFlavor DoMdo = "mdo"
prettyDoFlavor (DoQualified m) = pretty m <> ".do"
prettyDoFlavor (DoQualifiedMdo m) = pretty m <> ".mdo"

prettyDoLayout :: (a -> Doc ann) -> (a -> Bool) -> [a] -> Doc ann
prettyDoLayout prettyStmt stmtContainsLayout stmts
  | any stmtContainsLayout stmts =
      " {"
        <> hardline
        <> prettySemiSeparated prettyStmt stmtContainsLayout stmts
        <> hardline
        <> "}"
  | otherwise = " " <> prettySemiBlock prettyStmt (const False) stmts

prettyDoStmt :: DoStmt Expr -> Doc ann
prettyDoStmt stmt =
  case stmt of
    DoAnn _ inner -> prettyDoStmt inner
    DoBind pat expr -> prettyPattern pat <+> "<-" <+> prettyExpr expr
    DoLetDecls decls -> prettyLetDecls decls
    DoExpr expr@ELetDecls {} -> parens (prettyExpr expr)
    DoExpr expr -> prettyExpr expr
    DoRecStmt stmts -> "rec" <> prettyDoLayout prettyDoStmt doStmtContainsLayoutCase stmts

-- | Pretty-print an arrow command.
prettyCmd :: Cmd -> Doc ann
prettyCmd cmd =
  case cmd of
    CmdAnn _ inner -> prettyCmd inner
    CmdArrApp lhs HsFirstOrderApp rhs ->
      prettyExpr lhs <+> "-<" <+> prettyExpr rhs
    CmdArrApp lhs HsHigherOrderApp rhs ->
      prettyExpr lhs <+> "-<<" <+> prettyExpr rhs
    CmdInfix l op r ->
      prettyCmd l <+> prettyNameInfixOp op <+> prettyCmd r
    CmdDo stmts ->
      "do" <> prettyDoLayout prettyCmdStmt cmdStmtContainsLayoutCase stmts
    CmdIf cond yes no ->
      "if" <+> prettyExpr cond <+> "then" <+> prettyCmd yes <+> "else" <+> prettyCmd no
    CmdCase scrut alts ->
      "case" <+> prettyExpr scrut <+> "of" <> prettyCaseLayout (map prettyCmdCaseAlt alts)
    CmdLet decls body ->
      case decls of
        [] -> prettyLetDecls decls <+> "in" <+> prettyCmd body
        _ -> align (prettyLetDecls decls <> hardline <> "in" <+> prettyCmd body)
    CmdLam pats body ->
      "\\" <+> hsep (map prettyPattern pats) <+> "->" <+> prettyCmd body
    CmdApp c e ->
      prettyCmd c <+> prettyExpr e
    CmdPar c ->
      parens (prettyCmd c)

prettyCmdStmt :: DoStmt Cmd -> Doc ann
prettyCmdStmt stmt =
  case stmt of
    DoAnn _ inner -> prettyCmdStmt inner
    DoBind pat cmd' -> prettyPattern pat <+> "<-" <+> prettyCmd cmd'
    DoLetDecls decls -> prettyLetDecls decls
    DoExpr cmd' -> prettyCmd cmd'
    DoRecStmt stmts -> "rec" <> prettyDoLayout prettyCmdStmt cmdStmtContainsLayoutCase stmts

prettyCmdCaseAlt :: CaseAlt Cmd -> Doc ann
prettyCmdCaseAlt = prettyCaseAltWith prettyCmd cmdEndsWithLayoutCase

prettyCompStmt :: CompStmt -> Doc ann
prettyCompStmt stmt =
  case stmt of
    CompAnn _ inner -> prettyCompStmt inner
    CompGen pat expr -> prettyPattern pat <+> "<-" <+> prettyExpr expr
    CompGuard expr -> prettyExpr expr
    CompLetDecls decls -> prettyLetDecls decls
    CompThen f -> "then" <+> prettyExpr f
    CompThenBy f e -> "then" <+> prettyExpr f <+> "by" <+> prettyExpr e
    CompGroupUsing f -> "then" <+> "group" <+> "using" <+> prettyExpr f
    CompGroupByUsing e f -> "then" <+> "group" <+> "by" <+> prettyExpr e <+> "using" <+> prettyExpr f

prettyInlineDecls :: [Decl] -> Doc ann
prettyInlineDecls decls =
  case map prettyInlineDecl decls of
    [] -> mempty
    renderedDecls@(firstDecl : restDecls)
      | any inlineDeclEndsWithLayoutCase (init decls) ->
          vsep (firstDecl : map (semi <+>) restDecls)
      | otherwise -> hsep (punctuate semi renderedDecls)
  where
    prettyInlineDecl decl = case decl of
      DeclValue valueDecl -> prettyInlineValueDecl valueDecl
      _ -> hsep (prettyDeclLines decl)

    prettyInlineValueDecl valueDecl = case valueDecl of
      PatternBind multTag pat rhs -> prettyMultiplicityTag multTag <> prettyPattern pat <+> prettyInlineRhs rhs
      FunctionBind name matches ->
        hsep (punctuate semi (map (prettyInlineFunctionMatch name) matches))

    prettyInlineFunctionMatch name match =
      prettyFunctionHead name (matchHeadForm match) (matchPats match) <+> prettyInlineRhs (matchRhs match)

    prettyInlineRhs rhs = case rhs of
      UnguardedRhs _ body whereDecls
        | isJust whereDecls && exprEndsWithLayoutCase body ->
            "="
              <> hardline
              <> indent 2 (prettyExpr body)
              <> hardline
              <> indent 2 (prettyWhereClauseBare whereDecls)
      UnguardedRhs _ body whereDecls ->
        "="
          <+> prettyInlineExpr body
          <> prettyWhereClause whereDecls
      GuardedRhss _ guards whereDecls
        | guardedRhssNeedLayoutBlock guards whereDecls ->
            hardline <> indent 2 (prettyGuardedRhssBlock guards whereDecls)
      GuardedRhss _ guards whereDecls ->
        hsep
          (map (prettyGuardedRhs "=") guards)
          <> prettyWhereClause whereDecls

    prettyInlineExpr = prettyExpr

    inlineDeclEndsWithLayoutCase decl = case decl of
      DeclAnn _ sub -> inlineDeclEndsWithLayoutCase sub
      DeclValue valueDecl -> valueDeclContainsLayoutCase valueDecl
      _ -> False

exprEndsWithLayoutCase :: Expr -> Bool
exprEndsWithLayoutCase expr =
  case expr of
    EAnn _ sub -> exprEndsWithLayoutCase sub
    ECase _ alts -> not (null alts)
    ELambdaCase alts -> not (null alts)
    ELambdaCases alts -> not (null alts)
    EIf _ _ no -> exprEndsWithLayoutCase no
    ELambdaPats _ body -> exprEndsWithLayoutCase body
    ELetDecls _ body -> exprEndsWithLayoutCase body
    EInfix _ _ rhs -> exprEndsWithLayoutCase rhs
    EApp _ arg -> exprEndsWithLayoutCase arg
    ETypeApp fn _ -> exprEndsWithLayoutCase fn
    ENegate inner -> exprEndsWithLayoutCase inner
    EDo stmts _ -> any doStmtContainsLayoutCase stmts
    ERecordUpd base _ -> exprEndsWithLayoutCase base
    EGetField base _ -> exprEndsWithLayoutCase base
    EProc _ cmd -> cmdEndsWithLayoutCase cmd
    EPragma _ inner -> exprEndsWithLayoutCase inner
    EParen inner -> exprEndsWithLayoutCase inner
    _ -> False

declContainsLayoutCase :: Decl -> Bool
declContainsLayoutCase decl =
  case decl of
    DeclAnn _ sub -> declContainsLayoutCase sub
    DeclValue valueDecl -> valueDeclContainsLayoutCase valueDecl
    DeclPatSyn patSynDecl -> patSynDeclContainsLayout patSynDecl
    DeclData dataDecl -> dataDeclContainsLayout dataDecl
    DeclTypeData dataDecl -> dataDeclContainsLayout dataDecl
    DeclNewtype newtypeDecl -> maybe False isGadtCon (newtypeDeclConstructor newtypeDecl)
    DeclTypeFamilyDecl tf -> maybe False (not . null) (typeFamilyDeclEquations tf)
    DeclDataFamilyInst dfi -> any isGadtCon (dataFamilyInstConstructors dfi)
    _ -> False
  where
    patSynDeclContainsLayout ps =
      case patSynDeclDir ps of
        PatSynExplicitBidirectional matches -> not (null matches)
        _ -> False

    dataDeclContainsLayout dataDecl = any isGadtCon (dataDeclConstructors dataDecl)

valueDeclContainsLayoutCase :: ValueDecl -> Bool
valueDeclContainsLayoutCase valueDecl =
  case valueDecl of
    PatternBind _ pat rhs -> patternContainsLayoutCase pat || rhsContainsLayoutCase rhs
    FunctionBind _ matches -> any matchContainsLayoutCase matches

matchContainsLayoutCase :: Match -> Bool
matchContainsLayoutCase match =
  any patternContainsLayoutCase (matchPats match) || rhsContainsLayoutCase (matchRhs match)

rhsEndsWithLayoutCase :: Rhs Expr -> Bool
rhsEndsWithLayoutCase rhs =
  case rhs of
    UnguardedRhs _ body _ -> exprEndsWithLayoutCase body
    GuardedRhss _ guards _ -> any (exprEndsWithLayoutCase . guardedRhsBody) guards

rhsContainsLayoutCase :: Rhs Expr -> Bool
rhsContainsLayoutCase rhs =
  case rhs of
    UnguardedRhs _ body whereDecls ->
      exprContainsLayoutCase body || maybe False whereDeclsContainLayout whereDecls
    GuardedRhss _ guards whereDecls ->
      any guardedRhsContainsLayoutCase guards || maybe False whereDeclsContainLayout whereDecls
  where
    whereDeclsContainLayout decls = not (null decls) || any declContainsLayoutCase decls

declContainsMultilineString :: Decl -> Bool
declContainsMultilineString decl =
  case decl of
    DeclAnn _ sub -> declContainsMultilineString sub
    DeclValue valueDecl -> valueDeclContainsMultilineString valueDecl
    _ -> False

valueDeclContainsMultilineString :: ValueDecl -> Bool
valueDeclContainsMultilineString valueDecl =
  case valueDecl of
    PatternBind _ pat rhs -> patternContainsMultilineString pat || rhsContainsMultilineString rhs
    FunctionBind _ matches -> any matchContainsMultilineString matches

matchContainsMultilineString :: Match -> Bool
matchContainsMultilineString match =
  any patternContainsMultilineString (matchPats match) || rhsContainsMultilineString (matchRhs match)

rhsContainsMultilineString :: Rhs Expr -> Bool
rhsContainsMultilineString rhs =
  case rhs of
    UnguardedRhs _ body whereDecls ->
      exprContainsMultilineString body || maybe False (any declContainsMultilineString) whereDecls
    GuardedRhss _ guards whereDecls ->
      any guardedRhsContainsMultilineString guards || maybe False (any declContainsMultilineString) whereDecls

guardedRhsContainsMultilineString :: GuardedRhs Expr -> Bool
guardedRhsContainsMultilineString grhs =
  any guardQualifierContainsMultilineString (guardedRhsGuards grhs)
    || exprContainsMultilineString (guardedRhsBody grhs)

exprContainsMultilineString :: Expr -> Bool
exprContainsMultilineString expr =
  case expr of
    EAnn _ sub -> exprContainsMultilineString sub
    EString _ raw -> T.isInfixOf "\n" raw
    EStringHash _ raw -> T.isInfixOf "\n" raw
    EApp fn arg -> exprContainsMultilineString fn || exprContainsMultilineString arg
    ETypeApp fn _ -> exprContainsMultilineString fn
    EIf cond yes no -> any exprContainsMultilineString [cond, yes, no]
    EMultiWayIf rhss -> any guardedRhsContainsMultilineString rhss
    ELambdaPats _ body -> exprContainsMultilineString body
    ELambdaCase alts -> any caseAltContainsMultilineString alts
    ELambdaCases alts -> any lambdaCaseAltContainsMultilineString alts
    EInfix lhs _ rhs -> exprContainsMultilineString lhs || exprContainsMultilineString rhs
    ENegate inner -> exprContainsMultilineString inner
    ESectionL lhs _ -> exprContainsMultilineString lhs
    ESectionR _ rhs -> exprContainsMultilineString rhs
    ELetDecls decls body -> any declContainsMultilineString decls || exprContainsMultilineString body
    ECase scrut alts -> exprContainsMultilineString scrut || any caseAltContainsMultilineString alts
    EDo stmts _ -> any doStmtContainsMultilineString stmts
    EListComp body quals -> exprContainsMultilineString body || any compStmtContainsMultilineString quals
    EListCompParallel body qualifierGroups -> exprContainsMultilineString body || any (any compStmtContainsMultilineString) qualifierGroups
    ERecordCon _ fields _ -> any (exprContainsMultilineString . recordFieldValue) fields
    ERecordUpd base fields -> exprContainsMultilineString base || any (exprContainsMultilineString . recordFieldValue) fields
    EGetField base _ -> exprContainsMultilineString base
    ETypeSig inner _ -> exprContainsMultilineString inner
    EParen inner -> exprContainsMultilineString inner
    EList values -> any exprContainsMultilineString values
    ETuple _ values -> any (maybe False exprContainsMultilineString) values
    EUnboxedSum _ _ inner -> exprContainsMultilineString inner
    EProc _ cmd -> cmdContainsMultilineString cmd
    EPragma _ inner -> exprContainsMultilineString inner
    ETHExpQuote body -> exprContainsMultilineString body
    ETHTypedQuote body -> exprContainsMultilineString body
    ETHDeclQuote decls -> any declContainsMultilineString decls
    ETHPatQuote pat -> patternContainsMultilineString pat
    ETHNameQuote body -> exprContainsMultilineString body
    ETHSplice body -> exprContainsMultilineString body
    ETHTypedSplice body -> exprContainsMultilineString body
    _ -> False

cmdContainsMultilineString :: Cmd -> Bool
cmdContainsMultilineString cmd =
  case cmd of
    CmdAnn _ inner -> cmdContainsMultilineString inner
    CmdArrApp lhs _ rhs -> exprContainsMultilineString lhs || exprContainsMultilineString rhs
    CmdInfix lhs _ rhs -> cmdContainsMultilineString lhs || cmdContainsMultilineString rhs
    CmdDo stmts -> any cmdStmtContainsMultilineString stmts
    CmdIf cond yes no -> exprContainsMultilineString cond || cmdContainsMultilineString yes || cmdContainsMultilineString no
    CmdCase scrut alts -> exprContainsMultilineString scrut || any cmdCaseAltContainsMultilineString alts
    CmdLet decls body -> any declContainsMultilineString decls || cmdContainsMultilineString body
    CmdLam _ body -> cmdContainsMultilineString body
    CmdApp c e -> cmdContainsMultilineString c || exprContainsMultilineString e
    CmdPar c -> cmdContainsMultilineString c

patternContainsMultilineString :: Pattern -> Bool
patternContainsMultilineString pat =
  case pat of
    PAnn _ sub -> patternContainsMultilineString sub
    PLit lit -> literalContainsMultilineString lit
    PNegLit lit -> literalContainsMultilineString lit
    PView expr sub -> exprContainsMultilineString expr || patternContainsMultilineString sub
    PSplice expr -> exprContainsMultilineString expr
    PInfix lhs _ rhs -> patternContainsMultilineString lhs || patternContainsMultilineString rhs
    PCon _ _ args -> any patternContainsMultilineString args
    PRecord _ fields _ -> any (patternContainsMultilineString . recordFieldValue) fields
    PList pats -> any patternContainsMultilineString pats
    PTuple _ pats -> any patternContainsMultilineString pats
    PUnboxedSum _ _ inner -> patternContainsMultilineString inner
    PAs _ sub -> patternContainsMultilineString sub
    PStrict sub -> patternContainsMultilineString sub
    PIrrefutable sub -> patternContainsMultilineString sub
    PTypeSig sub _ -> patternContainsMultilineString sub
    PParen sub -> patternContainsMultilineString sub
    _ -> False

literalContainsMultilineString :: Literal -> Bool
literalContainsMultilineString lit =
  case lit of
    LitAnn _ sub -> literalContainsMultilineString sub
    LitString _ raw -> T.isInfixOf "\n" raw
    LitStringHash _ raw -> T.isInfixOf "\n" raw
    _ -> False

caseAltContainsMultilineString :: CaseAlt Expr -> Bool
caseAltContainsMultilineString (CaseAlt _ pat rhs) =
  patternContainsMultilineString pat || rhsContainsMultilineString rhs

cmdCaseAltContainsMultilineString :: CaseAlt Cmd -> Bool
cmdCaseAltContainsMultilineString (CaseAlt _ pat rhs) =
  patternContainsMultilineString pat || cmdRhsContainsMultilineString rhs

lambdaCaseAltContainsMultilineString :: LambdaCaseAlt -> Bool
lambdaCaseAltContainsMultilineString (LambdaCaseAlt _ pats rhs) =
  any patternContainsMultilineString pats || rhsContainsMultilineString rhs

cmdRhsContainsMultilineString :: Rhs Cmd -> Bool
cmdRhsContainsMultilineString rhs =
  case rhs of
    UnguardedRhs _ body whereDecls ->
      cmdContainsMultilineString body || maybe False (any declContainsMultilineString) whereDecls
    GuardedRhss _ guards whereDecls ->
      any cmdGuardedRhsContainsMultilineString guards || maybe False (any declContainsMultilineString) whereDecls

cmdGuardedRhsContainsMultilineString :: GuardedRhs Cmd -> Bool
cmdGuardedRhsContainsMultilineString grhs =
  any guardQualifierContainsMultilineString (guardedRhsGuards grhs)
    || cmdContainsMultilineString (guardedRhsBody grhs)

guardQualifierContainsMultilineString :: GuardQualifier -> Bool
guardQualifierContainsMultilineString qualifier =
  case qualifier of
    GuardAnn _ inner -> guardQualifierContainsMultilineString inner
    GuardExpr expr -> exprContainsMultilineString expr
    GuardPat pat expr -> patternContainsMultilineString pat || exprContainsMultilineString expr
    GuardLet decls -> any declContainsMultilineString decls

doStmtContainsMultilineString :: DoStmt Expr -> Bool
doStmtContainsMultilineString stmt =
  case stmt of
    DoAnn _ inner -> doStmtContainsMultilineString inner
    DoBind pat expr -> patternContainsMultilineString pat || exprContainsMultilineString expr
    DoExpr expr -> exprContainsMultilineString expr
    DoLetDecls decls -> any declContainsMultilineString decls
    DoRecStmt stmts -> any doStmtContainsMultilineString stmts

cmdStmtContainsMultilineString :: DoStmt Cmd -> Bool
cmdStmtContainsMultilineString stmt =
  case stmt of
    DoAnn _ inner -> cmdStmtContainsMultilineString inner
    DoBind pat cmd -> patternContainsMultilineString pat || cmdContainsMultilineString cmd
    DoExpr cmd -> cmdContainsMultilineString cmd
    DoLetDecls decls -> any declContainsMultilineString decls
    DoRecStmt stmts -> any cmdStmtContainsMultilineString stmts

compStmtContainsMultilineString :: CompStmt -> Bool
compStmtContainsMultilineString stmt =
  case stmt of
    CompAnn _ inner -> compStmtContainsMultilineString inner
    CompGen pat expr -> patternContainsMultilineString pat || exprContainsMultilineString expr
    CompGuard expr -> exprContainsMultilineString expr
    CompLetDecls decls -> any declContainsMultilineString decls
    CompThen expr -> exprContainsMultilineString expr
    CompThenBy lhs rhs -> exprContainsMultilineString lhs || exprContainsMultilineString rhs
    CompGroupUsing expr -> exprContainsMultilineString expr
    CompGroupByUsing lhs rhs -> exprContainsMultilineString lhs || exprContainsMultilineString rhs

guardedRhsContainsLayoutCase :: GuardedRhs Expr -> Bool
guardedRhsContainsLayoutCase grhs =
  any guardQualifierContainsLayoutCase (guardedRhsGuards grhs)
    || exprContainsLayoutCase (guardedRhsBody grhs)

exprContainsLayoutCase :: Expr -> Bool
exprContainsLayoutCase expr =
  case expr of
    EAnn _ sub -> exprContainsLayoutCase sub
    ECase scrut alts -> exprContainsLayoutCase scrut || not (null alts) || any caseAltEndsWithLayoutCase alts
    EApp fn arg -> exprContainsLayoutCase fn || exprContainsLayoutCase arg
    ETypeApp fn _ -> exprContainsLayoutCase fn
    EIf cond yes no -> any exprContainsLayoutCase [cond, yes, no]
    EMultiWayIf rhss -> any guardedRhsContainsLayoutCase rhss
    ELambdaPats _ body -> exprContainsLayoutCase body
    ELambdaCase alts -> not (null alts) || any caseAltEndsWithLayoutCase alts
    ELambdaCases alts -> not (null alts) || any lambdaCaseAltEndsWithLayoutCase alts
    EInfix lhs _ rhs -> exprContainsLayoutCase lhs || exprContainsLayoutCase rhs
    ENegate inner -> exprContainsLayoutCase inner
    ESectionL lhs _ -> exprContainsLayoutCase lhs
    ESectionR _ rhs -> exprContainsLayoutCase rhs
    ELetDecls decls body -> any declContainsLayoutCase decls || exprContainsLayoutCase body
    EDo stmts _ -> any doStmtContainsLayoutCase stmts
    EListComp body quals -> exprContainsLayoutCase body || any compStmtContainsLayoutCase quals
    EListCompParallel body qualifierGroups -> exprContainsLayoutCase body || any (any compStmtContainsLayoutCase) qualifierGroups
    ERecordCon _ fields _ -> any (exprContainsLayoutCase . recordFieldValue) fields
    ERecordUpd base fields -> exprContainsLayoutCase base || any (exprContainsLayoutCase . recordFieldValue) fields
    EGetField base _ -> exprContainsLayoutCase base
    ETypeSig inner _ -> exprContainsLayoutCase inner
    EParen inner -> exprContainsLayoutCase inner
    EList values -> any exprContainsLayoutCase values
    ETuple _ values -> any (maybe False exprContainsLayoutCase) values
    EUnboxedSum _ _ inner -> exprContainsLayoutCase inner
    EProc _ cmd -> cmdContainsLayoutCase cmd
    EPragma _ inner -> exprContainsLayoutCase inner
    ETHExpQuote body -> exprContainsLayoutCase body
    ETHTypedQuote body -> exprContainsLayoutCase body
    ETHDeclQuote decls -> any declContainsLayoutCase decls
    ETHPatQuote pat -> patternContainsLayoutCase pat
    ETHNameQuote body -> exprContainsLayoutCase body
    ETHSplice body -> exprContainsLayoutCase body
    ETHTypedSplice body -> exprContainsLayoutCase body
    _ -> False

cmdContainsLayoutCase :: Cmd -> Bool
cmdContainsLayoutCase cmd =
  case cmd of
    CmdAnn _ inner -> cmdContainsLayoutCase inner
    CmdArrApp lhs _ rhs -> exprContainsLayoutCase lhs || exprContainsLayoutCase rhs
    CmdInfix lhs _ rhs -> cmdContainsLayoutCase lhs || cmdContainsLayoutCase rhs
    CmdDo stmts -> any cmdStmtContainsLayoutCase stmts
    CmdIf cond yes no -> exprContainsLayoutCase cond || cmdContainsLayoutCase yes || cmdContainsLayoutCase no
    CmdCase _ alts -> not (null alts)
    CmdLet decls body -> any declContainsLayoutCase decls || cmdContainsLayoutCase body
    CmdLam _ body -> cmdContainsLayoutCase body
    CmdApp c e -> cmdContainsLayoutCase c || exprContainsLayoutCase e
    CmdPar c -> cmdContainsLayoutCase c

cmdEndsWithLayoutCase :: Cmd -> Bool
cmdEndsWithLayoutCase cmd =
  case cmd of
    CmdAnn _ inner -> cmdEndsWithLayoutCase inner
    CmdArrApp _ _ rhs -> exprEndsWithLayoutCase rhs
    CmdInfix _ _ rhs -> cmdEndsWithLayoutCase rhs
    CmdIf _ _ no -> cmdEndsWithLayoutCase no
    CmdCase _ alts -> not (null alts)
    CmdLet _ body -> cmdEndsWithLayoutCase body
    CmdLam _ body -> cmdEndsWithLayoutCase body
    CmdApp _ arg -> exprEndsWithLayoutCase arg
    CmdPar inner -> cmdEndsWithLayoutCase inner
    _ -> False

cmdStmtContainsLayoutCase :: DoStmt Cmd -> Bool
cmdStmtContainsLayoutCase stmt =
  case stmt of
    DoAnn _ inner -> cmdStmtContainsLayoutCase inner
    DoBind _ cmd -> cmdContainsLayoutCase cmd
    DoExpr cmd -> cmdContainsLayoutCase cmd
    DoLetDecls decls -> not (null decls) || any declContainsLayoutCase decls
    DoRecStmt stmts -> any cmdStmtContainsLayoutCase stmts

compStmtContainsLayoutCase :: CompStmt -> Bool
compStmtContainsLayoutCase stmt =
  case stmt of
    CompAnn _ inner -> compStmtContainsLayoutCase inner
    CompGen _ expr -> exprContainsLayoutCase expr
    CompGuard expr -> exprContainsLayoutCase expr
    CompLetDecls decls -> not (null decls) || any declContainsLayoutCase decls
    CompThen expr -> exprContainsLayoutCase expr
    CompThenBy lhs rhs -> exprContainsLayoutCase lhs || exprContainsLayoutCase rhs
    CompGroupUsing expr -> exprContainsLayoutCase expr
    CompGroupByUsing lhs rhs -> exprContainsLayoutCase lhs || exprContainsLayoutCase rhs

patternContainsLayoutCase :: Pattern -> Bool
patternContainsLayoutCase pat =
  case pat of
    PAnn _ sub -> patternContainsLayoutCase sub
    PView expr sub -> exprContainsLayoutCase expr || patternContainsLayoutCase sub
    PSplice expr -> exprContainsLayoutCase expr
    PInfix lhs _ rhs -> patternContainsLayoutCase lhs || patternContainsLayoutCase rhs
    PCon _ _ args -> any patternContainsLayoutCase args
    PRecord _ fields _ -> any (patternContainsLayoutCase . recordFieldValue) fields
    PList pats -> any patternContainsLayoutCase pats
    PTuple _ pats -> any patternContainsLayoutCase pats
    PUnboxedSum _ _ inner -> patternContainsLayoutCase inner
    PAs _ sub -> patternContainsLayoutCase sub
    PStrict sub -> patternContainsLayoutCase sub
    PIrrefutable sub -> patternContainsLayoutCase sub
    PTypeSig sub _ -> patternContainsLayoutCase sub
    PParen sub -> patternContainsLayoutCase sub
    _ -> False

caseAltEndsWithLayoutCase :: CaseAlt Expr -> Bool
caseAltEndsWithLayoutCase (CaseAlt _ _ rhs) = rhsEndsWithLayoutCase rhs

lambdaCaseAltEndsWithLayoutCase :: LambdaCaseAlt -> Bool
lambdaCaseAltEndsWithLayoutCase (LambdaCaseAlt _ _ rhs) = rhsEndsWithLayoutCase rhs

guardQualifierEndsWithLayoutCase :: GuardQualifier -> Bool
guardQualifierEndsWithLayoutCase qualifier =
  case qualifier of
    GuardAnn _ inner -> guardQualifierEndsWithLayoutCase inner
    GuardExpr expr -> exprEndsWithLayoutCase expr
    GuardPat _ expr -> exprEndsWithLayoutCase expr
    GuardLet decls -> not (null decls) || any declContainsLayoutCase decls

guardQualifierContainsLayoutCase :: GuardQualifier -> Bool
guardQualifierContainsLayoutCase qualifier =
  case qualifier of
    GuardAnn _ inner -> guardQualifierContainsLayoutCase inner
    GuardExpr expr -> exprContainsLayoutCase expr
    GuardPat pat expr -> patternContainsLayoutCase pat || exprContainsLayoutCase expr
    GuardLet decls -> not (null decls) || any declContainsLayoutCase decls

doStmtContainsLayoutCase :: DoStmt Expr -> Bool
doStmtContainsLayoutCase stmt =
  case stmt of
    DoAnn _ inner -> doStmtContainsLayoutCase inner
    DoBind _ expr -> exprContainsLayoutCase expr
    DoExpr expr -> exprContainsLayoutCase expr
    DoLetDecls decls -> not (null decls) || any declContainsLayoutCase decls
    DoRecStmt stmts -> any doStmtContainsLayoutCase stmts

guardedRhssNeedLayoutBlock :: [GuardedRhs Expr] -> Maybe [Decl] -> Bool
guardedRhssNeedLayoutBlock = guardedBodiesNeedLayoutBlock exprEndsWithLayoutCase

guardedBodiesNeedLayoutBlock :: (body -> Bool) -> [GuardedRhs body] -> Maybe [Decl] -> Bool
guardedBodiesNeedLayoutBlock bodyEndsWithLayoutCase guards whereDecls =
  any guardedRhsHasLayoutGuard guards
    || any (bodyEndsWithLayoutCase . guardedRhsBody) (dropLast guards)
    || (isJust whereDecls && any (bodyEndsWithLayoutCase . guardedRhsBody) guards)
  where
    guardedRhsHasLayoutGuard = any guardQualifierEndsWithLayoutCase . guardedRhsGuards

    dropLast [] = []
    dropLast [_] = []
    dropLast (x : xs) = x : dropLast xs

prettyGuardedRhssBlock :: [GuardedRhs Expr] -> Maybe [Decl] -> Doc ann
prettyGuardedRhssBlock guards whereDecls =
  vsep (map prettyGuardedRhsBlock guards)
    <> case whereDecls of
      Nothing -> mempty
      Just _ -> hardline <> prettyWhereClauseBare whereDecls

prettyGuardedRhsBlock :: GuardedRhs Expr -> Doc ann
prettyGuardedRhsBlock grhs =
  let guards = guardedRhsGuards grhs
      body = guardedRhsBody grhs
   in if exprEndsWithLayoutCase body
        then
          "|"
            <+> prettyGuardQualifiers guards
            <+> "="
            <> hardline
            <> indent 2 (prettyExpr body)
        else prettyGuardedRhs "=" grhs

prettyArithSeq :: ArithSeq -> Doc ann
prettyArithSeq seqInfo =
  case seqInfo of
    ArithSeqAnn _ inner -> prettyArithSeq inner
    ArithSeqFrom fromExpr -> brackets (prettyExpr fromExpr <> " ..")
    ArithSeqFromThen fromExpr thenExpr -> brackets (prettyExpr fromExpr <> ", " <> prettyExpr thenExpr <> " ..")
    ArithSeqFromTo fromExpr toExpr -> brackets (prettyExpr fromExpr <> " .. " <> prettyExpr toExpr)
    ArithSeqFromThenTo fromExpr thenExpr toExpr ->
      brackets (prettyExpr fromExpr <> ", " <> prettyExpr thenExpr <> " .. " <> prettyExpr toExpr)

quoted :: Text -> Doc ann
quoted txt = pretty (show (T.unpack txt))

prettyQuasiQuote :: Text -> Text -> Doc ann
prettyQuasiQuote quoter body = "[" <> pretty quoter <> "|" <> pretty body <> "|]"

-- ---------------------------------------------------------------------------
-- TypeFamilies pretty-printing helpers

prettyTypeFamilyDecl :: TypeFamilyDecl -> Doc ann
prettyTypeFamilyDecl tf =
  hsep $
    ["type"]
      <> familyKeywordPart (typeFamilyDeclExplicitFamilyKeyword tf)
      <> prettyTypeFamilyHead (typeFamilyDeclHeadForm tf) (typeFamilyDeclHead tf) (typeFamilyDeclParams tf)
      <> resultSigPart (typeFamilyDeclResultSig tf)
      <> eqsPart (typeFamilyDeclEquations tf)
  where
    familyKeywordPart True = ["family"]
    familyKeywordPart False = []
    resultSigPart Nothing = []
    resultSigPart (Just (TypeFamilyKindSig k)) = ["::", prettyType k]
    resultSigPart (Just (TypeFamilyTyVarSig result)) = ["=", prettyTyVarBinder result]
    resultSigPart (Just (TypeFamilyInjectiveSig result injectivity)) =
      ["=", prettyTyVarBinder result, "|", prettyTypeFamilyInjectivity injectivity]
    eqsPart Nothing = []
    eqsPart (Just []) = ["where", spacedBraces mempty]
    eqsPart (Just eqs) = ["where" <> hardline <> indent 2 (vsep (map prettyTypeFamilyEq eqs))]

prettyTypeFamilyEq :: TypeFamilyEq -> Doc ann
prettyTypeFamilyEq eq =
  hsep $
    forallPart (typeFamilyEqForall eq)
      <> prettyTypeFamilyLhs (typeFamilyEqHeadForm eq) (typeFamilyEqLhs eq)
      <> ["=", prettyType (typeFamilyEqRhs eq)]
  where
    forallPart [] = []
    forallPart binders = ["forall", hsep (map prettyTyVarBinder binders) <> "."]

prettyDataFamilyDecl :: DataFamilyDecl -> Doc ann
prettyDataFamilyDecl df =
  hsep $
    ["data", "family"]
      <> prettyNamedBinderHead (dataFamilyDeclHead df)
      <> kindPart (dataFamilyDeclKind df)
  where
    kindPart Nothing = []
    kindPart (Just k) = ["::", prettyType k]

prettyTopTypeFamilyInst :: TypeFamilyInst -> Doc ann
prettyTopTypeFamilyInst tfi =
  hsep $
    ["type", "instance"]
      <> forallPart (typeFamilyInstForall tfi)
      <> prettyTypeFamilyLhs (typeFamilyInstHeadForm tfi) (typeFamilyInstLhs tfi)
      <> ["=", prettyType (typeFamilyInstRhs tfi)]
  where
    forallPart [] = []
    forallPart binders = ["forall", hsep (map prettyTyVarBinder binders) <> "."]

prettyTopDataFamilyInst :: DataFamilyInst -> Doc ann
prettyTopDataFamilyInst dfi =
  case dataFamilyInstConstructors dfi of
    ctors
      | any isGadtCon ctors ->
          headDoc <+> prettyGadtConBlock ctors (dataFamilyInstDeriving dfi)
    _ ->
      hsep $
        headParts
          <> ctorPart (dataFamilyInstConstructors dfi)
          <> derivingParts (dataFamilyInstDeriving dfi)
  where
    keyword = if dataFamilyInstIsNewtype dfi then "newtype" else "data"
    headParts =
      [keyword, "instance"]
        <> forallPart (dataFamilyInstForall dfi)
        <> [prettyType (dataFamilyInstHead dfi)]
        <> kindPart (dataFamilyInstKind dfi)
    headDoc = hsep headParts
    forallPart [] = []
    forallPart binders = ["forall", hsep (map prettyTyVarBinder binders) <> "."]
    kindPart Nothing = []
    kindPart (Just k) = ["::", prettyType k]
    ctorPart [] = []
    ctorPart ctors@(c : _)
      | dataFamilyInstIsNewtype dfi = ["=", prettyDataCon c]
      | otherwise = ["=", hsep (punctuate " |" (map prettyDataCon ctors))]

prettyAssocTypeFamilyDecl :: TypeFamilyDecl -> Doc ann
prettyAssocTypeFamilyDecl tf =
  hsep $
    ["type"]
      <> familyKeywordPart (typeFamilyDeclExplicitFamilyKeyword tf)
      <> prettyTypeFamilyHead (typeFamilyDeclHeadForm tf) (typeFamilyDeclHead tf) (typeFamilyDeclParams tf)
      <> resultSigPart (typeFamilyDeclResultSig tf)
  where
    familyKeywordPart True = ["family"]
    familyKeywordPart False = []
    resultSigPart Nothing = []
    resultSigPart (Just (TypeFamilyKindSig k)) = ["::", prettyType k]
    resultSigPart (Just (TypeFamilyTyVarSig result)) = ["=", prettyTyVarBinder result]
    resultSigPart (Just (TypeFamilyInjectiveSig result injectivity)) =
      ["=", prettyTyVarBinder result, "|", prettyTypeFamilyInjectivity injectivity]

prettyTypeFamilyHead :: TypeHeadForm -> Type -> [TyVarBinder] -> [Doc ann]
prettyTypeFamilyHead headForm headType params =
  case headForm of
    TypeHeadPrefix -> [prettyType headType] <> map prettyTyVarBinder params
    TypeHeadInfix ->
      case (params, typeFamilyInfixAppView headType) of
        ([lhs, rhs], Just (op, promoted, _, _)) ->
          [ prettyTyVarBinder lhs,
            (if promoted == Promoted then "'" else mempty) <> prettyNameInfixOp op,
            prettyTyVarBinder rhs
          ]
        _ -> [prettyTypeFamilyInfix headType]

prettyTypeFamilyLhs :: TypeHeadForm -> Type -> [Doc ann]
prettyTypeFamilyLhs headForm lhs =
  case headForm of
    TypeHeadPrefix -> [prettyType lhs]
    TypeHeadInfix -> [prettyTypeFamilyInfix lhs]

prettyAssocDataFamilyDecl :: DataFamilyDecl -> Doc ann
prettyAssocDataFamilyDecl df =
  hsep $
    ["data"]
      <> prettyNamedBinderHead (dataFamilyDeclHead df)
      <> kindPart (dataFamilyDeclKind df)
  where
    kindPart Nothing = []
    kindPart (Just k) = ["::", prettyType k]

prettyDefaultTypeInst :: TypeFamilyInst -> Doc ann
prettyDefaultTypeInst tfi =
  hsep $
    ["type", "instance"]
      <> forallPart (typeFamilyInstForall tfi)
      <> prettyTypeFamilyLhs (typeFamilyInstHeadForm tfi) (typeFamilyInstLhs tfi)
      <> ["=", prettyType (typeFamilyInstRhs tfi)]
  where
    forallPart [] = []
    forallPart binders = ["forall", hsep (map prettyTyVarBinder binders) <> "."]

prettyInstTypeFamilyInst :: TypeFamilyInst -> Doc ann
prettyInstTypeFamilyInst tfi =
  hsep $
    ["type"]
      <> forallPart (typeFamilyInstForall tfi)
      <> prettyTypeFamilyLhs (typeFamilyInstHeadForm tfi) (typeFamilyInstLhs tfi)
      <> ["=", prettyType (typeFamilyInstRhs tfi)]
  where
    forallPart [] = []
    forallPart binders = ["forall", hsep (map prettyTyVarBinder binders) <> "."]

prettyNamedBinderHead :: BinderHead UnqualifiedName -> [Doc ann]
prettyNamedBinderHead head' =
  case head' of
    PrefixBinderHead name params ->
      [prettyConstructorUName name] <> map prettyTyVarBinder params
    InfixBinderHead lhs name rhs tailPrms ->
      let infixHead = prettyTyVarBinder lhs <+> prettyTypeHeadInfixName name <+> prettyTyVarBinder rhs
       in case tailPrms of
            [] -> [infixHead]
            _ -> parens infixHead : map prettyTyVarBinder tailPrms

prettyTypeHeadInfixName :: UnqualifiedName -> Doc ann
prettyTypeHeadInfixName = prettyInfixOp

prettyTypeFamilyInfix :: Type -> Doc ann
prettyTypeFamilyInfix ty =
  case peelTypeAnn ty of
    TParen inner -> parens (prettyType inner)
    peeled ->
      case typeFamilyInfixAppView peeled of
        Just (op, promoted, lhs, rhs) ->
          prettyType lhs
            <+> (if promoted == Promoted then "'" else mempty)
            <> prettyNameInfixOp op
            <+> prettyType rhs
        _ -> prettyType ty

prettyInstDataFamilyInst :: DataFamilyInst -> Doc ann
prettyInstDataFamilyInst dfi =
  case dataFamilyInstConstructors dfi of
    ctors
      | any isGadtCon ctors ->
          headDoc <+> prettyGadtConBlock ctors (dataFamilyInstDeriving dfi)
    _ ->
      hsep $
        headParts
          <> ctorPart (dataFamilyInstConstructors dfi)
          <> derivingParts (dataFamilyInstDeriving dfi)
  where
    keyword = if dataFamilyInstIsNewtype dfi then "newtype" else "data"
    headParts =
      [keyword, prettyType (dataFamilyInstHead dfi)]
        <> kindPart (dataFamilyInstKind dfi)
    headDoc = hsep headParts
    kindPart Nothing = []
    kindPart (Just k) = ["::", prettyType k]
    ctorPart [] = []
    ctorPart ctors@(c : _)
      | dataFamilyInstIsNewtype dfi = ["=", prettyDataCon c]
      | otherwise = ["=", hsep (punctuate " |" (map prettyDataCon ctors))]
