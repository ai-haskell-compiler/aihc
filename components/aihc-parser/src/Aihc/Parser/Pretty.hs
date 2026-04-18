{-# LANGUAGE LambdaCase #-}
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
    braces,
    brackets,
    comma,
    hsep,
    nest,
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
                  <> maybe [] prettyWarningText (moduleWarningText modu)
                  <> maybe [] (\specs -> [prettyExportSpecList specs]) (moduleExports modu)
                  <> ["where"]
              )
          ]
        Nothing -> []
    prettyWarningText (DeprText msg) = ["{-# DEPRECATED", pretty (show msg), "#-}"]
    prettyWarningText (WarnText msg) = ["{-# WARNING", pretty (show msg), "#-}"]
    prettyWarningText (WarningTextAnn _ sub) = prettyWarningText sub
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
    ExportWithAll mWarning namespace name members ->
      prettyExportWarning
        mWarning
        (prettyNamespacePrefix namespace <> prettyName name <> parens (hsep (punctuate comma (map prettyExportMember members <> [".."]))))

prettyExportWarning :: Maybe WarningText -> Doc ann -> Doc ann
prettyExportWarning mWarning doc =
  case mWarning of
    Nothing -> doc
    Just (DeprText msg) -> hsep ["{-# DEPRECATED", pretty (show msg), "#-}", doc]
    Just (WarnText msg) -> hsep ["{-# WARNING", pretty (show msg), "#-}", doc]
    Just (WarningTextAnn _ sub) -> prettyExportWarning (Just sub) doc

prettyImportDecl :: ImportDecl -> Doc ann
prettyImportDecl decl =
  let renderPostQualified =
        importDeclQualifiedPost decl
          && importDeclQualified decl
   in hsep
        ( ["import"]
            <> ["safe" | importDeclSafe decl]
            <> ["qualified" | importDeclQualified decl && not renderPostQualified]
            <> maybe [] (\level -> [prettyImportLevel level]) (importDeclLevel decl)
            <> maybe [] (\pkg -> [prettyQuotedText pkg]) (importDeclPackage decl)
            <> ["{-# SOURCE #-}" | importDeclSource decl]
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
-- EVar and EParen get a @$@ prefix; other expressions are bare.
prettyDeclSpliceExpr :: Expr -> Doc ann
prettyDeclSpliceExpr body =
  case peelExprAnn body of
    EVar {} -> "$" <> prettyExpr body
    EParen {} -> "$" <> prettyExpr body
    _ -> prettyExpr body

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
    ImportItemAllWith namespace name members ->
      prettyNamespacePrefix namespace <> prettyConstructorUName name <> parens (hsep (punctuate comma (map prettyExportMember members <> [".."])))

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
      let headDocs = case (typeSynHeadForm synDecl, typeSynParams synDecl) of
            (TypeHeadInfix, [lhs, rhs]) ->
              let name = typeSynName synDecl
               in [pretty (tyVarBinderName lhs), prettyInfixOp name, pretty (tyVarBinderName rhs)]
            _ -> [prettyDeclHead TypeHeadPrefix [] (typeSynName synDecl) (typeSynParams synDecl)]
       in [hsep (["type"] <> headDocs <> ["=", prettyType (typeSynBody synDecl)])]
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
    PatternBind pat rhs -> [prettyPattern pat <+> prettyRhs rhs]
    FunctionBind name matches ->
      concatMap (prettyFunctionMatchLines name) matches

-- | Pretty-print a value declaration on a single line.
prettyValueDeclSingleLine :: ValueDecl -> Doc ann
prettyValueDeclSingleLine valueDecl =
  case valueDecl of
    PatternBind pat rhs -> prettyPattern pat <+> prettyRhs rhs
    FunctionBind name matches ->
      hsep (punctuate semi (map (prettyFunctionMatch name) matches))

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
prettyPatSynWhere name (PatSynExplicitBidirectional matches) =
  ["where", braces (hsep (punctuate semi (map (prettyFunctionMatch name) matches)))]

prettyFunctionMatchLines :: UnqualifiedName -> Match -> [Doc ann]
prettyFunctionMatchLines name match =
  case matchRhs match of
    UnguardedRhs {} -> [prettyFunctionMatch name match]
    GuardedRhss _ grhss mWhereDecls ->
      prettyFunctionHead name (matchHeadForm match) (matchPats match)
        : [ "  |"
              <+> hsep (punctuate comma (map prettyGuardQualifier (guardedRhsGuards grhs)))
              <+> "="
              <+> prettyExpr (guardedRhsBody grhs)
          | grhs <- grhss
          ]
          <> [prettyWhereClause mWhereDecls | isJust mWhereDecls]

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

prettyRhs :: Rhs -> Doc ann
prettyRhs rhs =
  case rhs of
    UnguardedRhs _ expr whereDecls ->
      "="
        <+> prettyExpr expr
        <> prettyWhereClause whereDecls
    GuardedRhss _ guards whereDecls ->
      hsep
        [ "|"
            <+> hsep (punctuate comma (map prettyGuardQualifier (guardedRhsGuards grhs)))
            <+> "="
            <+> prettyExpr (guardedRhsBody grhs)
        | grhs <- guards
        ]
        <> prettyWhereClause whereDecls

prettyWhereClause :: Maybe [Decl] -> Doc ann
prettyWhereClause Nothing = mempty
prettyWhereClause (Just []) = " where" <+> spacedBraces mempty
prettyWhereClause (Just decls) = " where" <+> spacedBraces (prettyInlineDecls decls)

-- FIXME: If we strip the annotations before pretty-printing, we can get rid of these two functions.

-- | Recognize @lhs \`op\` rhs@ / @lhs op rhs@ after peeling span-only 'TAnn'
-- on each 'TApp' spine step. Do not peel 'TParen' here: explicit parentheses
-- must be handled by the 'TParen' case in 'prettyType' first (see
-- 'typeAnnSpan', 'addTypeParens').
typeInfixAppView :: Type -> Maybe (Name, TypePromotion, Type, Type)
typeInfixAppView ty =
  case peelTypeAnn ty of
    TApp l r ->
      case peelTypeAnn l of
        TApp opRaw lhs ->
          case peelTypeAnn opRaw of
            TCon op promoted
              | isSymbolicTypeName op,
                renderName op /= "->" ->
                  Just (op, promoted, lhs, r)
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

-- | Infix type-family heads use @l \`Op\` r@ with a 'NameConId' operator (e.g.
-- @\`And\`@), so 'isSymbolicTypeName' is false; 'TypeHeadInfix' already marks
-- this shape as infix.
typeFamilyInfixAppView :: Type -> Maybe (Name, TypePromotion, Type, Type)
typeFamilyInfixAppView ty =
  case peelTypeAnn ty of
    TApp l r ->
      case peelTypeAnn l of
        TApp opRaw lhs ->
          case peelTypeAnn opRaw of
            TCon op promoted
              | renderName op /= "->" ->
                  Just (op, promoted, lhs, r)
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

-- | Pretty-print a type. The AST is assumed to already have TParen nodes
-- in the correct positions (inserted by 'addTypeParens').
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
       in if promoted == Promoted then "'" <> base else base
    TImplicitParam name inner -> pretty name <+> "::" <+> prettyType inner
    TTypeLit lit -> prettyTypeLiteral lit
    TStar -> "*"
    TQuasiQuote quoter body -> prettyQuasiQuote quoter body
    TForall telescope inner ->
      prettyForallTelescope telescope <+> prettyType inner
    -- Before infix detection: required grouping from the parser ('TParen',
    -- @(a :+: b) -> c@, constraints, nested @(c => t)@).
    TParen inner -> parens (prettyType inner)
    other
      | Just (op, promoted, lhs, rhs) <- typeInfixAppView other ->
          prettyType lhs
            <+> (if promoted == Promoted then "'" else mempty)
            <> prettyNameInfixOp op
            <+> prettyType rhs
    TApp f x ->
      prettyType f <+> prettyType x
    TFun a b ->
      prettyType a <+> "->" <+> prettyType b
    TTuple tupleFlavor promoted elems ->
      let tupleDoc = prettyTupleBody tupleFlavor (hsep (punctuate comma (map prettyType elems)))
       in if promoted == Promoted then "'" <> tupleDoc else tupleDoc
    TUnboxedSum elems ->
      hsep ["(#", hsep (punctuate " |" (map prettyType elems)), "#)"]
    TList promoted elems ->
      let listDoc = brackets (hsep (punctuate comma (map prettyType elems)))
       in if promoted == Promoted then "'" <> listDoc else listDoc
    TKindSig ty' kind ->
      prettyType ty' <+> "::" <+> prettyType kind
    TContext constraints inner ->
      case constraints of
        [] -> prettyType inner
        cs -> prettyContext cs <+> "=>" <+> prettyType inner
    TSplice body -> "$" <> prettyExpr body
    TWildcard -> "_"

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
    PVar name -> pretty name
    PTypeBinder binder -> prettyTyVarBinder binder
    PTypeSyntax TypeSyntaxExplicitNamespace ty -> "type" <+> prettyType ty
    PTypeSyntax TypeSyntaxInTerm ty -> prettyType ty
    PWildcard -> "_"
    PLit lit -> prettyLiteral lit
    PQuasiQuote quoter body -> prettyQuasiQuote quoter body
    PTuple tupleFlavor elems -> prettyTupleBody tupleFlavor (hsep (punctuate comma (map prettyPattern elems)))
    PUnboxedSum altIdx arity inner ->
      let slots = [if i == altIdx then prettyPattern inner else mempty | i <- [0 .. arity - 1]]
       in hsep ["(#", hsep (punctuate " |" slots), "#)"]
    PList elems -> brackets (hsep (punctuate comma (map prettyPattern elems)))
    PCon con typeArgs args -> hsep ([prettyPrefixName con] <> map prettyInvisibleTypeArg typeArgs <> map prettyPattern args)
    PInfix lhs op rhs -> prettyPattern lhs <+> prettyNameInfixOp op <+> prettyPattern rhs
    PView viewExpr inner ->
      prettyExpr viewExpr <+> "->" <+> prettyPattern inner
    PAs name inner -> pretty name <> "@" <> prettyPattern inner
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
                  ( [prettyPatternFieldBinding fieldName fieldPat | (fieldName, fieldPat) <- fields]
                      ++ [".." | hasWildcard]
                  )
              )
          )
    PTypeSig inner ty -> prettyPattern inner <+> "::" <+> prettyType ty
    PSplice body -> "$" <> prettyExpr body

-- | Pretty print a pattern field binding.
prettyPatternFieldBinding :: Name -> Pattern -> Doc ann
prettyPatternFieldBinding fieldName fieldPat =
  case peelPatternAnn fieldPat of
    PVar varName | renderUnqualifiedName varName == renderName fieldName -> pretty fieldName
    _ -> pretty fieldName <+> "=" <+> prettyPattern fieldPat

prettyLiteral :: Literal -> Doc ann
prettyLiteral lit =
  case peelLiteralAnn lit of
    LitInt _ repr -> pretty repr
    LitIntHash _ repr -> pretty repr
    LitIntBase _ repr -> pretty repr
    LitIntBaseHash _ repr -> pretty repr
    LitFloat _ repr -> pretty repr
    LitFloatHash _ repr -> pretty repr
    LitChar _ repr -> pretty repr
    LitCharHash _ repr -> pretty repr
    LitString _ repr -> pretty repr
    LitStringHash _ repr -> pretty repr
    LitAnn {} -> error "unreachable"

prettyDataDecl :: DataDecl -> Doc ann
prettyDataDecl decl =
  hsep
    ( [ "data",
        prettyDeclHead (dataDeclHeadForm decl) (dataDeclContext decl) (dataDeclName decl) (dataDeclParams decl)
      ]
        <> kindPart
        <> ctorPart
        <> derivingParts (dataDeclDeriving decl)
    )
  where
    kindPart = maybe [] (\k -> ["::", prettyType k]) (dataDeclKind decl)
    ctorPart =
      case dataDeclConstructors decl of
        [] -> []
        ctors
          | any isGadtCon ctors -> ["where", braces (hsep (punctuate semi (map prettyDataCon ctors)))]
          | otherwise -> ["=", hsep (punctuate " |" (map prettyDataCon ctors))]

prettyTypeDataDecl :: DataDecl -> Doc ann
prettyTypeDataDecl decl =
  hsep
    ( [ "type data",
        prettyDeclHead (dataDeclHeadForm decl) (dataDeclContext decl) (dataDeclName decl) (dataDeclParams decl)
      ]
        <> kindPart
        <> ctorPart
    )
  where
    kindPart = maybe [] (\k -> ["::", prettyType k]) (dataDeclKind decl)
    ctorPart =
      case dataDeclConstructors decl of
        [] -> []
        ctors
          | any isGadtCon ctors -> ["where", braces (hsep (punctuate semi (map prettyDataCon ctors)))]
          | otherwise -> ["=", hsep (punctuate " |" (map prettyDataCon ctors))]

isGadtCon :: DataConDecl -> Bool
isGadtCon (DataConAnn _ inner) = isGadtCon inner
isGadtCon (GadtCon {}) = True
isGadtCon _ = False

prettyNewtypeDecl :: NewtypeDecl -> Doc ann
prettyNewtypeDecl decl =
  hsep
    ( [ "newtype",
        prettyDeclHead (newtypeDeclHeadForm decl) (newtypeDeclContext decl) (newtypeDeclName decl) (newtypeDeclParams decl)
      ]
        <> kindPart
        <> ctorPart
        <> derivingParts (newtypeDeclDeriving decl)
    )
  where
    kindPart = maybe [] (\k -> ["::", prettyType k]) (newtypeDeclKind decl)
    ctorPart =
      case newtypeDeclConstructor decl of
        Nothing -> []
        Just ctor
          | isGadtCon ctor -> ["where", braces (prettyDataCon ctor)]
          | otherwise -> ["=", prettyDataCon ctor]

derivingParts :: [DerivingClause] -> [Doc ann]
derivingParts = concatMap derivingPart

derivingPart :: DerivingClause -> [Doc ann]
derivingPart (DerivingClause strategy classes viaTy parenthesized) =
  ["deriving"] <> strategyPart strategy <> classesPart classes <> viaPart viaTy
  where
    strategyPart Nothing = []
    strategyPart (Just DerivingStock) = ["stock"]
    strategyPart (Just DerivingNewtype) = ["newtype"]
    strategyPart (Just DerivingAnyclass) = ["anyclass"]

    classesPart [] = ["()"]
    classesPart [single]
      | parenthesized = [parens (prettyType single)]
      | otherwise = [prettyType single]
    classesPart _ = [parens (hsep (punctuate comma (map prettyType classes)))]

    viaPart Nothing = []
    viaPart (Just ty) = ["via", prettyType ty]

prettyDeclHead :: TypeHeadForm -> [Type] -> UnqualifiedName -> [TyVarBinder] -> Doc ann
prettyDeclHead headForm constraints name params =
  hsep
    ( contextPrefix constraints
        <> prettyDeclHeadNameAndParams name params
    )
  where
    prettyDeclHeadNameAndParams nm prms = case (headForm, prms) of
      (TypeHeadInfix, lhs : rhs : tailPrms) ->
        let infixHead = pretty (tyVarBinderName lhs) <+> prettyInfixOp nm <+> pretty (tyVarBinderName rhs)
         in case tailPrms of
              [] -> [infixHead]
              _ -> parens infixHead : map prettyTyVarBinder tailPrms
      _ ->
        [prettyConstructorUName nm] <> map prettyTyVarBinder prms

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
        <+> braces
          ( hsep
              ( punctuate
                  comma
                  [ hsep
                      [ hsep (punctuate comma (map prettyFieldName (fieldNames fld))),
                        "::",
                        prettyRecordFieldBangType (fieldType fld)
                      ]
                  | fld <- fields
                  ]
              )
          )
      where
        prettyFieldName :: UnqualifiedName -> Doc ann
        prettyFieldName fieldName
          | isSymbolicUName fieldName = parens (pretty fieldName)
          | otherwise = pretty fieldName
    GadtCon foralls constraints names body ->
      prettyGadtCon foralls constraints names body

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
        _ -> hsep (punctuate " ->" (map prettyBangType args ++ [prettyType resultTy]))
    GadtRecordBody fields resultTy ->
      braces (prettyRecordFields fields) <+> "->" <+> prettyType resultTy

prettyRecordFields :: [FieldDecl] -> Doc ann
prettyRecordFields fields =
  hsep
    ( punctuate
        comma
        [ hsep
            [ hsep (punctuate comma (map prettyFieldName (fieldNames fld))),
              "::",
              prettyRecordFieldBangType (fieldType fld)
            ]
        | fld <- fields
        ]
    )
  where
    prettyFieldName :: UnqualifiedName -> Doc ann
    prettyFieldName name
      | isSymbolicUName name = parens (pretty name)
      | otherwise = pretty name

dataConQualifierPrefix :: [Text] -> [Type] -> [Doc ann]
dataConQualifierPrefix forallVars constraints = forallPrefix forallVars <> contextPrefix constraints
  where
    forallPrefix [] = []
    forallPrefix binders = ["forall", hsep (map pretty binders) <> "."]

-- | Pretty print a BangType. The type already has TParen nodes where needed.
prettyBangType :: BangType -> Doc ann
prettyBangType bt =
  hsep (prettySourceUnpackedness (bangSourceUnpackedness bt) <> [strictOrLazyDoc])
  where
    strictOrLazyDoc
      | bangStrict bt = "!" <> prettyType (bangType bt)
      | bangLazy bt = "~" <> prettyType (bangType bt)
      | otherwise = prettyType (bangType bt)

prettyRecordFieldBangType :: BangType -> Doc ann
prettyRecordFieldBangType bt =
  hsep (prettySourceUnpackedness (bangSourceUnpackedness bt) <> [strictOrLazyDoc])
  where
    strictOrLazyDoc
      | bangStrict bt = "!" <> prettyType (bangType bt)
      | bangLazy bt = "~" <> prettyType (bangType bt)
      | otherwise = prettyType (bangType bt)

prettySourceUnpackedness :: SourceUnpackedness -> [Doc ann]
prettySourceUnpackedness unpackedness =
  case unpackedness of
    NoSourceUnpackedness -> []
    SourceUnpack -> ["{-# UNPACK #-}"]
    SourceNoUnpack -> ["{-# NOUNPACK #-}"]

prettyClassDecl :: ClassDecl -> Doc ann
prettyClassDecl decl =
  let headDoc =
        hsep
          ( ["class"]
              <> maybeContextPrefix (classDeclContext decl)
              <> prettyNamedTypeHead (classDeclHeadForm decl) (classDeclName decl) (classDeclParams decl)
              <> prettyClassFundeps (classDeclFundeps decl)
          )
   in case classDeclItems decl of
        [] -> headDoc
        items -> headDoc <+> "where" <+> braces (hsep (punctuate semi (map prettyClassItem items)))

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
            <> map prettyInfixOp ops
        )
    ClassItemDefault valueDecl -> prettyValueDeclSingleLine valueDecl
    ClassItemTypeFamilyDecl tf -> prettyAssocTypeFamilyDecl tf
    ClassItemDataFamilyDecl df -> prettyAssocDataFamilyDecl df
    ClassItemDefaultTypeInst tfi -> prettyDefaultTypeInst tfi
    ClassItemPragma pragma -> prettyPragma pragma

prettyInstanceDecl :: InstanceDecl -> Doc ann
prettyInstanceDecl decl =
  let headDoc =
        hsep
          ( ["instance"]
              <> maybe [] (\pragma' -> [prettyInstanceOverlapPragma pragma']) (instanceDeclOverlapPragma decl)
              <> maybe [] (\w -> [prettyInstanceWarning w]) (instanceDeclWarning decl)
              <> forallTyVarBinderPrefix (instanceDeclForall decl)
              <> contextPrefix (instanceDeclContext decl)
              <> [instanceHeadDoc decl]
          )
   in case instanceDeclItems decl of
        [] -> headDoc
        items -> vsep [headDoc <+> "where", nest 2 (braces (vsep (punctuate semi (map prettyInstanceItem items))))]

prettyInstanceWarning :: WarningText -> Doc ann
prettyInstanceWarning (DeprText msg) = "{-# DEPRECATED " <> pretty (show msg) <> " #-}"
prettyInstanceWarning (WarnText msg) = "{-# WARNING " <> pretty (show msg) <> " #-}"
prettyInstanceWarning (WarningTextAnn _ sub) = prettyInstanceWarning sub

prettyStandaloneDeriving :: StandaloneDerivingDecl -> Doc ann
prettyStandaloneDeriving decl =
  hsep
    ( ["deriving"]
        <> maybe [] (\s -> [prettyDerivingStrategy s]) (standaloneDerivingStrategy decl)
        <> maybe [] (\ty -> ["via", prettyType ty]) (standaloneDerivingViaType decl)
        <> ["instance"]
        <> maybe [] (\pragma' -> [prettyInstanceOverlapPragma pragma']) (standaloneDerivingOverlapPragma decl)
        <> maybe [] (\w -> [prettyInstanceWarning w]) (standaloneDerivingWarning decl)
        <> forallTyVarBinderPrefix (standaloneDerivingForall decl)
        <> contextPrefix (standaloneDerivingContext decl)
        <> [standaloneDerivingHeadDoc decl]
    )

instanceHeadDoc :: InstanceDecl -> Doc ann
instanceHeadDoc decl =
  maybeParenthesize (instanceDeclParenthesizedHead decl) $
    prettyInstanceLikeHead (instanceDeclHeadForm decl) (instanceDeclClassName decl) (instanceDeclTypes decl)

standaloneDerivingHeadDoc :: StandaloneDerivingDecl -> Doc ann
standaloneDerivingHeadDoc decl =
  maybeParenthesize (standaloneDerivingParenthesizedHead decl) $
    prettyStandaloneDerivingHead
      (standaloneDerivingHeadForm decl)
      (standaloneDerivingClassName decl)
      (standaloneDerivingTypes decl)

prettyInstanceLikeHead :: TypeHeadForm -> UnqualifiedName -> [Type] -> Doc ann
prettyInstanceLikeHead headForm className tys =
  case (headForm, tys) of
    (TypeHeadInfix, [lhs, rhs]) -> prettyType lhs <+> prettyInfixOp className <+> prettyType rhs
    _ -> hsep (prettyConstructorUName className : map prettyType tys)

prettyStandaloneDerivingHead :: TypeHeadForm -> Name -> [Type] -> Doc ann
prettyStandaloneDerivingHead headForm className tys =
  case (headForm, tys) of
    (TypeHeadInfix, [lhs, rhs]) -> prettyType lhs <+> prettyNameInfixOp className <+> prettyType rhs
    _ -> hsep (prettyPrefixName className : map prettyType tys)

maybeParenthesize :: Bool -> Doc ann -> Doc ann
maybeParenthesize shouldParen doc
  | shouldParen = parens doc
  | otherwise = doc

prettyInstanceOverlapPragma :: InstanceOverlapPragma -> Doc ann
prettyInstanceOverlapPragma pragma' =
  case pragma' of
    Overlapping -> "{-# OVERLAPPING #-}"
    Overlappable -> "{-# OVERLAPPABLE #-}"
    Overlaps -> "{-# OVERLAPS #-}"
    Incoherent -> "{-# INCOHERENT #-}"

prettyPragma :: Pragma -> Doc ann
prettyPragma pragma =
  case pragma of
    PragmaLanguage settings -> "{-# LANGUAGE " <> hsep (punctuate comma (map (pretty . extensionSettingName) settings)) <> " #-}"
    PragmaInstanceOverlap overlapPragma -> prettyInstanceOverlapPragma overlapPragma
    PragmaWarning msg -> "{-# WARNING " <> pretty msg <> " #-}"
    PragmaDeprecated msg -> "{-# DEPRECATED " <> pretty msg <> " #-}"
    PragmaInline kind body -> "{-# " <> pretty kind <> " " <> pretty body <> " #-}"
    PragmaUnpack unpackKind ->
      case unpackKind of
        UnpackPragma -> "{-# UNPACK #-}"
        NoUnpackPragma -> "{-# NOUNPACK #-}"
    PragmaSource sourceText _ -> "{-# SOURCE " <> pretty sourceText <> " #-}"
    PragmaUnknown text -> pretty text

prettyDerivingStrategy :: DerivingStrategy -> Doc ann
prettyDerivingStrategy strategy =
  case strategy of
    DerivingStock -> "stock"
    DerivingNewtype -> "newtype"
    DerivingAnyclass -> "anyclass"

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
            <> map prettyInfixOp ops
        )
    InstanceItemTypeFamilyInst tfi -> prettyInstTypeFamilyInst tfi
    InstanceItemDataFamilyInst dfi -> prettyInstDataFamilyInst dfi
    InstanceItemPragma pragma -> prettyPragma pragma

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
      Just (pretty (foreignName decl)),
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
    EApp fn arg -> prettyExpr fn <+> prettyExpr arg
    ETypeApp fn ty -> prettyExpr fn <+> "@" <> prettyType ty
    EVar name
      | isSymbolicName name -> parens (pretty (renderName name))
      | otherwise -> pretty name
    ETypeSyntax TypeSyntaxExplicitNamespace ty -> "type" <+> prettyType ty
    ETypeSyntax TypeSyntaxInTerm ty -> prettyType ty
    EInt _ repr -> pretty repr
    EIntHash _ repr -> pretty repr
    EIntBase _ repr -> pretty repr
    EIntBaseHash _ repr -> pretty repr
    EFloat _ repr -> pretty repr
    EFloatHash _ repr -> pretty repr
    EChar _ repr -> pretty repr
    ECharHash _ repr -> pretty repr
    EString _ repr -> pretty repr
    EStringHash _ repr -> pretty repr
    EOverloadedLabel _ raw -> pretty (" " <> raw)
    EQuasiQuote quoter body -> prettyQuasiQuote quoter body
    ETHExpQuote body -> "[|" <+> prettyExpr body <+> "|]"
    ETHTypedQuote body -> "[||" <+> prettyExpr body <+> "||]"
    ETHDeclQuote decls -> "[d|" <+> prettyInlineDecls decls <+> "|]"
    ETHTypeQuote ty -> "[t|" <+> prettyType ty <+> "|]"
    ETHPatQuote pat -> "[p|" <+> prettyPattern pat <+> "|]"
    ETHNameQuote name
      | thNameQuoteNeedsParens name -> "'" <> parens (pretty name)
      | otherwise -> "'" <> pretty name
    ETHTypeNameQuote name
      | isOperatorName name -> "''" <> parens (pretty name)
      | otherwise -> "''" <> pretty name
    ETHSplice body -> "$" <> prettyExpr body
    ETHTypedSplice body -> "$$" <> prettyExpr body
    EIf cond yes no ->
      "if" <+> prettyExpr cond <+> "then" <+> prettyExpr yes <+> "else" <+> prettyExpr no
    EMultiWayIf rhss ->
      "if"
        <+> "{"
        <+> hsep
          [ "|"
              <+> hsep (punctuate comma (map prettyGuardQualifier (guardedRhsGuards grhs)))
              <+> "->"
              <+> prettyExpr (guardedRhsBody grhs)
          | grhs <- rhss
          ]
        <+> "}"
    ELambdaPats pats body ->
      "\\" <+> hsep (map prettyPattern pats) <+> "->" <+> prettyExpr body
    ELambdaCase alts ->
      "\\" <> "case" <+> "{" <+> hsep (punctuate semi (map prettyCaseAlt alts)) <+> "}"
    ELambdaCases alts ->
      "\\" <> "cases" <+> "{" <+> hsep (punctuate semi (map prettyLambdaCaseAlt alts)) <+> "}"
    EInfix lhs op rhs ->
      prettyExpr lhs <+> prettyNameInfixOp op <+> prettyExpr rhs
    ENegate inner -> "-" <> prettyExpr inner
    ESectionL lhs op ->
      parens (prettyExpr lhs <+> prettyNameInfixOp op)
    ESectionR op rhs -> parens (prettyNameInfixOp op <+> prettyExpr rhs)
    ELetDecls decls body ->
      "let"
        <+> spacedBraces (prettyInlineDecls decls)
        <+> "in"
        <+> prettyExpr body
    ECase scrutinee alts ->
      "case"
        <+> prettyExpr scrutinee
        <+> "of"
        <+> "{"
        <+> hsep (punctuate semi (map prettyCaseAlt alts))
        <+> "}"
    EDo stmts isMdo ->
      (if isMdo then "mdo" else "do")
        <+> "{"
        <+> hsep (punctuate semi (map prettyDoStmt stmts))
        <+> "}"
    EListComp body quals ->
      brackets
        ( prettyExpr body
            <+> "|"
            <+> hsep (punctuate comma (map prettyCompStmt quals))
        )
    EListCompParallel body qualifierGroups ->
      brackets
        ( prettyExpr body
            <+> "|"
            <+> hsep
              ( punctuate
                  " |"
                  (map (hsep . punctuate comma . map prettyCompStmt) qualifierGroups)
              )
        )
    EArithSeq seqInfo -> prettyArithSeq seqInfo
    ERecordCon name fields hasWildcard ->
      pretty name <+> braces (hsep (punctuate comma (map prettyBinding fields ++ [".." | hasWildcard])))
    ERecordUpd base fields ->
      prettyExpr base <+> braces (hsep (punctuate comma (map prettyBinding fields)))
    ETypeSig inner ty -> prettyExpr inner <+> "::" <+> prettyType ty
    EParen inner ->
      case peelExprAnn inner of
        ESectionL {} -> prettyExpr inner
        ESectionR {} -> prettyExpr inner
        _ -> parens (prettyExpr inner)
    EList values -> brackets (hsep (punctuate comma (map prettyExpr values)))
    ETuple tupleFlavor values ->
      prettyTupleBody
        tupleFlavor
        ( hsep
            ( punctuate
                comma
                ( map
                    ( \case
                        Just val -> prettyExpr val
                        Nothing -> mempty
                    )
                    values
                )
            )
        )
    EUnboxedSum altIdx arity inner ->
      let slots = [if i == altIdx then prettyExpr inner else mempty | i <- [0 .. arity - 1]]
       in hsep ["(#", hsep (punctuate " |" slots), "#)"]
    EProc pat body ->
      "proc" <+> prettyPattern pat <+> "->" <+> prettyCmd body
    EAnn _ sub -> prettyExpr sub

prettyTupleBody :: TupleFlavor -> Doc ann -> Doc ann
prettyTupleBody tupleFlavor inner =
  case tupleFlavor of
    Boxed -> parens inner
    Unboxed -> hsep ["(#", inner, "#)"]

prettyBinding :: (Text, Expr) -> Doc ann
prettyBinding (name, value) =
  case peelExprAnn value of
    EVar varName | renderName varName == name -> pretty name
    _ -> pretty name <+> "=" <+> prettyExpr value

prettyCaseAlt :: CaseAlt -> Doc ann
prettyCaseAlt (CaseAlt _ pat rhs) =
  case rhs of
    UnguardedRhs _ body whereDecls ->
      prettyPattern pat
        <+> "->"
        <+> prettyExpr body
        <> prettyWhereClause whereDecls
    GuardedRhss _ grhss whereDecls ->
      hsep
        [ prettyPattern pat,
          hsep
            [ "|"
                <+> hsep (punctuate comma (map prettyGuardQualifier (guardedRhsGuards grhs)))
                <+> "->"
                <+> prettyExpr (guardedRhsBody grhs)
            | grhs <- grhss
            ]
        ]
        <> prettyWhereClause whereDecls

prettyLambdaCaseAlt :: LambdaCaseAlt -> Doc ann
prettyLambdaCaseAlt (LambdaCaseAlt _ pats rhs) =
  case rhs of
    UnguardedRhs _ body whereDecls ->
      hsep (map prettyPattern pats)
        <+> "->"
        <+> prettyExpr body
        <> prettyWhereClause whereDecls
    GuardedRhss _ grhss whereDecls ->
      hsep
        [ hsep (map prettyPattern pats),
          hsep
            [ "|"
                <+> hsep (punctuate comma (map prettyGuardQualifier (guardedRhsGuards grhs)))
                <+> "->"
                <+> prettyExpr (guardedRhsBody grhs)
            | grhs <- grhss
            ]
        ]
        <> prettyWhereClause whereDecls

prettyGuardQualifier :: GuardQualifier -> Doc ann
prettyGuardQualifier qualifier =
  case qualifier of
    GuardAnn _ inner -> prettyGuardQualifier inner
    GuardExpr expr -> prettyExpr expr
    GuardPat pat expr -> prettyPattern pat <+> "<-" <+> prettyExpr expr
    GuardLet decls -> "let" <+> spacedBraces (prettyInlineDecls decls)

prettyDoStmt :: DoStmt Expr -> Doc ann
prettyDoStmt stmt =
  case stmt of
    DoAnn _ inner -> prettyDoStmt inner
    DoBind pat expr -> prettyPattern pat <+> "<-" <+> prettyExpr expr
    DoLetDecls decls -> "let" <+> spacedBraces (prettyInlineDecls decls)
    DoExpr expr -> prettyExpr expr
    DoRecStmt stmts -> "rec" <+> "{" <+> hsep (punctuate semi (map prettyDoStmt stmts)) <+> "}"

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
      "do" <+> "{" <+> hsep (punctuate semi (map prettyCmdStmt stmts)) <+> "}"
    CmdIf cond yes no ->
      "if" <+> prettyExpr cond <+> "then" <+> prettyCmd yes <+> "else" <+> prettyCmd no
    CmdCase scrut alts ->
      "case" <+> prettyExpr scrut <+> "of" <+> "{" <+> hsep (punctuate semi (map prettyCmdCaseAlt alts)) <+> "}"
    CmdLet decls body ->
      "let" <+> spacedBraces (prettyInlineDecls decls) <+> "in" <+> prettyCmd body
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
    DoLetDecls decls -> "let" <+> spacedBraces (prettyInlineDecls decls)
    DoExpr cmd' -> prettyCmd cmd'
    DoRecStmt stmts -> "rec" <+> "{" <+> hsep (punctuate semi (map prettyCmdStmt stmts)) <+> "}"

prettyCmdCaseAlt :: CmdCaseAlt -> Doc ann
prettyCmdCaseAlt alt =
  prettyPattern (cmdCaseAltPat alt) <+> "->" <+> prettyCmd (cmdCaseAltBody alt)

prettyCompStmt :: CompStmt -> Doc ann
prettyCompStmt stmt =
  case stmt of
    CompAnn _ inner -> prettyCompStmt inner
    CompGen pat expr -> prettyPattern pat <+> "<-" <+> prettyExpr expr
    CompGuard expr -> prettyExpr expr
    CompLetDecls decls -> "let" <+> spacedBraces (prettyInlineDecls decls)

prettyInlineDecls :: [Decl] -> Doc ann
prettyInlineDecls decls =
  hsep (punctuate semi (map prettyInlineDecl decls))
  where
    prettyInlineDecl decl = case decl of
      DeclValue valueDecl -> prettyValueDeclSingleLine valueDecl
      _ -> hsep (prettyDeclLines decl)

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

isOperatorName :: Name -> Bool
isOperatorName name =
  let ty = nameType name
   in ty == NameVarSym || ty == NameConSym

-- | Whether a TH value name quote @'...@ must wrap its payload in parentheses.
--
-- Unqualified operators need @'(+), ...@. Qualified operators such as @P.+@
-- must be written @'(P.+), ...@ because @'P.+@ is not a single lexeme.
thNameQuoteNeedsParens :: Name -> Bool
thNameQuoteNeedsParens = isOperatorName

-- ---------------------------------------------------------------------------
-- TypeFamilies pretty-printing helpers

prettyTypeFamilyDecl :: TypeFamilyDecl -> Doc ann
prettyTypeFamilyDecl tf =
  hsep $
    ["type", "family"]
      <> prettyTypeFamilyHead (typeFamilyDeclHeadForm tf) (typeFamilyDeclHead tf) (typeFamilyDeclParams tf)
      <> kindPart (typeFamilyDeclKind tf)
      <> eqsPart (typeFamilyDeclEquations tf)
  where
    kindPart Nothing = []
    kindPart (Just k) = ["::", prettyType k]
    eqsPart Nothing = []
    eqsPart (Just eqs) = ["where", braces (hsep (punctuate semi (map prettyTypeFamilyEq eqs)))]

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
    ["data", "family", pretty (dataFamilyDeclName df)]
      <> map prettyTyVarBinder (dataFamilyDeclParams df)
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
  hsep $
    [keyword, "instance"]
      <> forallPart (dataFamilyInstForall dfi)
      <> [prettyType (dataFamilyInstHead dfi)]
      <> kindPart (dataFamilyInstKind dfi)
      <> ctorPart (dataFamilyInstConstructors dfi)
      <> derivingParts (dataFamilyInstDeriving dfi)
  where
    keyword = if dataFamilyInstIsNewtype dfi then "newtype" else "data"
    forallPart [] = []
    forallPart binders = ["forall", hsep (map prettyTyVarBinder binders) <> "."]
    kindPart Nothing = []
    kindPart (Just k) = ["::", prettyType k]
    ctorPart [] = []
    ctorPart ctors@(c : _)
      | any isGadtCon ctors = ["where", braces (hsep (punctuate semi (map prettyDataCon ctors)))]
      | dataFamilyInstIsNewtype dfi = ["=", prettyDataCon c]
      | otherwise = ["=", hsep (punctuate " |" (map prettyDataCon ctors))]

prettyAssocTypeFamilyDecl :: TypeFamilyDecl -> Doc ann
prettyAssocTypeFamilyDecl tf =
  hsep $
    ["type"]
      <> prettyTypeFamilyHead (typeFamilyDeclHeadForm tf) (typeFamilyDeclHead tf) (typeFamilyDeclParams tf)
      <> kindPart (typeFamilyDeclKind tf)
  where
    kindPart Nothing = []
    kindPart (Just k) = ["::", prettyType k]

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
    ["data", pretty (dataFamilyDeclName df)]
      <> map prettyTyVarBinder (dataFamilyDeclParams df)
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

prettyNamedTypeHead :: TypeHeadForm -> UnqualifiedName -> [TyVarBinder] -> [Doc ann]
prettyNamedTypeHead headForm name params =
  case (headForm, params) of
    (TypeHeadInfix, [lhs, rhs]) ->
      [ prettyTyVarBinder lhs,
        prettyTypeHeadInfixName name,
        prettyTyVarBinder rhs
      ]
    _ -> [prettyConstructorUName name] <> map prettyTyVarBinder params

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
  hsep $
    [keyword, prettyType (dataFamilyInstHead dfi)]
      <> kindPart (dataFamilyInstKind dfi)
      <> ctorPart (dataFamilyInstConstructors dfi)
      <> derivingParts (dataFamilyInstDeriving dfi)
  where
    keyword = if dataFamilyInstIsNewtype dfi then "newtype" else "data"
    kindPart Nothing = []
    kindPart (Just k) = ["::", prettyType k]
    ctorPart [] = []
    ctorPart ctors@(c : _)
      | any isGadtCon ctors = ["where", braces (hsep (punctuate semi (map prettyDataCon ctors)))]
      | dataFamilyInstIsNewtype dfi = ["=", prettyDataCon c]
      | otherwise = ["=", hsep (punctuate " |" (map prettyDataCon ctors))]
