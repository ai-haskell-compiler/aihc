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
import Data.Char (GeneralCategory (..), generalCategory, isAscii)
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
    prettyWarningText (DeprText _ msg) = ["{-# DEPRECATED", pretty (show msg), "#-}"]
    prettyWarningText (WarnText _ msg) = ["{-# WARNING", pretty (show msg), "#-}"]
    importLines = map prettyImportDecl (moduleImports modu)
    declLines = concatMap prettyDeclLines (moduleDecls modu)

prettyExportSpecList :: [ExportSpec] -> Doc ann
prettyExportSpecList specs =
  parens (hsep (punctuate comma (map prettyExportSpec specs)))

prettyExportSpec :: ExportSpec -> Doc ann
prettyExportSpec spec =
  case spec of
    ExportModule _ mWarning modName -> prettyExportWarning mWarning ("module" <+> pretty modName)
    ExportVar _ mWarning namespace name ->
      prettyExportWarning mWarning (prettyNamespacePrefix namespace <> prettyName name)
    ExportAbs _ mWarning namespace name ->
      prettyExportWarning mWarning (prettyNamespacePrefix namespace <> prettyName name)
    ExportAll _ mWarning namespace name ->
      prettyExportWarning mWarning (prettyNamespacePrefix namespace <> prettyName name <> "(..)")
    ExportWith _ mWarning namespace name members ->
      prettyExportWarning
        mWarning
        (prettyNamespacePrefix namespace <> prettyName name <> parens (hsep (punctuate comma (map prettyExportMember members))))
    ExportWithAll _ mWarning namespace name members ->
      prettyExportWarning
        mWarning
        (prettyNamespacePrefix namespace <> prettyName name <> parens (hsep (punctuate comma (map prettyExportMember members <> [".."]))))

prettyExportWarning :: Maybe WarningText -> Doc ann -> Doc ann
prettyExportWarning mWarning doc =
  case mWarning of
    Nothing -> doc
    Just (DeprText _ msg) -> hsep ["{-# DEPRECATED", pretty (show msg), "#-}", doc]
    Just (WarnText _ msg) -> hsep ["{-# WARNING", pretty (show msg), "#-}", doc]

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
  case body of
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
    ImportItemVar _ namespace name -> prettyNamespacePrefix namespace <> prettyBinderUName name
    ImportItemAbs _ namespace name -> prettyNamespacePrefix namespace <> prettyConstructorUName name
    ImportItemAll _ namespace name -> prettyNamespacePrefix namespace <> prettyConstructorUName name <> "(..)"
    ImportItemWith _ namespace name members ->
      prettyNamespacePrefix namespace <> prettyConstructorUName name <> parens (hsep (punctuate comma (map prettyExportMember members)))
    ImportItemAllWith _ namespace name members ->
      prettyNamespacePrefix namespace <> prettyConstructorUName name <> parens (hsep (punctuate comma (map prettyExportMember members <> [".."])))

prettyExportMember :: IEBundledMember -> Doc ann
prettyExportMember (IEBundledMember namespace name) =
  prettyMemberNamespacePrefix namespace <> prettyName name

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
    DeclValue _ valueDecl -> prettyValueDeclLines valueDecl
    DeclTypeSig _ names ty -> [hsep [hsep (punctuate comma (map prettyBinderName names)), "::", prettyType ty]]
    DeclPatSyn _ patSynDecl -> [prettyPatSynDecl patSynDecl]
    DeclPatSynSig _ names ty -> [hsep ["pattern", hsep (punctuate comma (map prettyConstructorUName names)), "::", prettyType ty]]
    DeclStandaloneKindSig _ name kind -> [hsep ["type", prettyConstructorUName name, "::", prettyType kind]]
    DeclFixity _ assoc mNamespace prec ops ->
      [ hsep
          ( [prettyFixityAssoc assoc]
              <> maybe [] (pure . pretty . show) prec
              <> maybe [] (pure . prettyNamespace) mNamespace
              <> punctuate comma (map (prettyInfixOp . renderUnqualifiedName) ops)
          )
      ]
    DeclRoleAnnotation _ ann -> [prettyRoleAnnotation ann]
    DeclTypeSyn _ synDecl ->
      let headDocs = case (typeSynHeadForm synDecl, typeSynParams synDecl) of
            (TypeHeadInfix, [lhs, rhs]) ->
              let name = typeSynName synDecl
               in if isOperatorToken name
                    then [pretty (tyVarBinderName lhs), pretty name, pretty (tyVarBinderName rhs)]
                    else [pretty (tyVarBinderName lhs), "`" <> pretty name <> "`", pretty (tyVarBinderName rhs)]
            _ -> [prettyDeclHead TypeHeadPrefix [] (unqualifiedNameFromText (typeSynName synDecl)) (typeSynParams synDecl)]
       in [hsep (["type"] <> headDocs <> ["=", prettyType (typeSynBody synDecl)])]
    DeclData _ dataDecl -> [prettyDataDecl dataDecl]
    DeclTypeData _ dataDecl -> [prettyTypeDataDecl dataDecl]
    DeclNewtype _ newtypeDecl -> [prettyNewtypeDecl newtypeDecl]
    DeclClass _ classDecl -> [prettyClassDecl classDecl]
    DeclInstance _ instanceDecl -> [prettyInstanceDecl instanceDecl]
    DeclStandaloneDeriving _ derivingDecl -> [prettyStandaloneDeriving derivingDecl]
    DeclDefault _ tys -> ["default" <+> parens (hsep (punctuate comma (map prettyType tys)))]
    DeclForeign _ foreignDecl -> [prettyForeignDecl foreignDecl]
    DeclSplice _ body -> [prettyDeclSpliceExpr body]
    DeclTypeFamilyDecl _ tf -> [prettyTypeFamilyDecl tf]
    DeclDataFamilyDecl _ df -> [prettyDataFamilyDecl df]
    DeclTypeFamilyInst _ tfi -> [prettyTopTypeFamilyInst tfi]
    DeclDataFamilyInst _ dfi -> [prettyTopDataFamilyInst dfi]
    DeclPragma _ pragma -> [prettyPragma pragma]

prettyRoleAnnotation :: RoleAnnotation -> Doc ann
prettyRoleAnnotation ann =
  hsep
    ( [ "type",
        "role",
        prettyConstructorName (roleAnnotationName ann)
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
    PatternBind _ pat rhs -> [prettyPattern pat <+> prettyRhs rhs]
    FunctionBind _ name matches ->
      concatMap (prettyFunctionMatchLines name) matches

-- | Pretty-print a value declaration on a single line.
prettyValueDeclSingleLine :: ValueDecl -> Doc ann
prettyValueDeclSingleLine valueDecl =
  case valueDecl of
    PatternBind _ pat rhs -> prettyPattern pat <+> prettyRhs rhs
    FunctionBind _ name matches ->
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
      [pretty lhs, prettyInfixOp (renderUnqualifiedName name), pretty rhs]
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
          let infixHead = prettyPattern lhs <+> prettyInfixOp (renderUnqualifiedName name) <+> prettyPattern rhsPat
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
prettyWhereClause (Just []) = " where" <+> braces mempty
prettyWhereClause (Just decls) = " where" <+> braces (prettyInlineDecls decls)

-- | Pretty-print a type. The AST is assumed to already have TParen nodes
-- in the correct positions (inserted by 'addTypeParens').
prettyType :: Type -> Doc ann
prettyType ty =
  case ty of
    TAnn _ sub -> prettyType sub
    TVar _ name -> pretty name
    TCon _ name promoted ->
      let rendered = renderName name
          base
            | isSymbolicTypeName name = parens (pretty rendered)
            | otherwise = pretty rendered
       in if promoted == Promoted then "'" <> base else base
    TImplicitParam _ name inner -> pretty name <+> "::" <+> prettyType inner
    TTypeLit _ lit -> prettyTypeLiteral lit
    TStar _ -> "*"
    TQuasiQuote _ quoter body -> prettyQuasiQuote quoter body
    TForall _ binders inner ->
      "forall" <+> hsep (map prettyTyVarBinder binders) <> "." <+> prettyType inner
    TApp _ (TApp _ (TCon _ op promoted) lhs) rhs
      | isSymbolicTypeName op && renderName op /= "->" ->
          prettyType lhs
            <+> (if promoted == Promoted then "'" else mempty)
            <> prettyNameInfixOp op
            <+> prettyType rhs
    TApp _ f x ->
      prettyType f <+> prettyType x
    TFun _ a b ->
      prettyType a <+> "->" <+> prettyType b
    TTuple _ tupleFlavor promoted elems ->
      let tupleDoc = prettyTupleBody tupleFlavor (hsep (punctuate comma (map prettyType elems)))
       in if promoted == Promoted then "'" <> tupleDoc else tupleDoc
    TUnboxedSum _ elems ->
      hsep ["(#", hsep (punctuate " |" (map prettyType elems)), "#)"]
    TList _ promoted elems ->
      let listDoc = brackets (hsep (punctuate comma (map prettyType elems)))
       in if promoted == Promoted then "'" <> listDoc else listDoc
    TParen _ inner -> parens (prettyType inner)
    TKindSig _ ty' kind ->
      prettyType ty' <+> "::" <+> prettyType kind
    TContext _ constraints inner ->
      prettyContext constraints <+> "=>" <+> prettyType inner
    TSplice _ body -> "$" <> prettyExpr body
    TWildcard _ -> "_"

prettyContext :: [Type] -> Doc ann
prettyContext constraints =
  case constraints of
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
    PVar _ name -> pretty name
    PWildcard _ -> "_"
    PLit _ lit -> prettyLiteral lit
    PQuasiQuote _ quoter body -> prettyQuasiQuote quoter body
    PTuple _ tupleFlavor elems -> prettyTupleBody tupleFlavor (hsep (punctuate comma (map prettyPattern elems)))
    PUnboxedSum _ altIdx arity inner ->
      let slots = [if i == altIdx then prettyPattern inner else mempty | i <- [0 .. arity - 1]]
       in hsep ["(#", hsep (punctuate " |" slots), "#)"]
    PList _ elems -> brackets (hsep (punctuate comma (map prettyPattern elems)))
    PCon _ con args -> hsep (prettyPrefixName con : map prettyPattern args)
    PInfix _ lhs op rhs -> prettyPattern lhs <+> prettyNameInfixOp op <+> prettyPattern rhs
    PView _ viewExpr inner ->
      prettyExpr viewExpr <+> "->" <+> prettyPattern inner
    PAs _ name inner -> pretty name <> "@" <> prettyPattern inner
    PStrict _ inner -> "!" <> prettyPattern inner
    PIrrefutable _ inner -> "~" <> prettyPattern inner
    PNegLit _ lit -> "-" <> prettyLiteral lit
    PParen _ inner -> parens (prettyPattern inner)
    PRecord _ con fields hasWildcard ->
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
    PTypeSig _ inner ty -> prettyPattern inner <+> "::" <+> prettyType ty
    PSplice _ body -> "$" <> prettyExpr body

-- | Pretty print a pattern field binding.
prettyPatternFieldBinding :: Name -> Pattern -> Doc ann
prettyPatternFieldBinding fieldName fieldPat =
  case fieldPat of
    PVar _ varName | renderUnqualifiedName varName == renderName fieldName -> pretty fieldName
    _ -> pretty fieldName <+> "=" <+> prettyPattern fieldPat

prettyLiteral :: Literal -> Doc ann
prettyLiteral lit =
  case lit of
    LitInt _ _ repr -> pretty repr
    LitIntHash _ _ repr -> pretty repr
    LitIntBase _ _ repr -> pretty repr
    LitIntBaseHash _ _ repr -> pretty repr
    LitFloat _ _ repr -> pretty repr
    LitFloatHash _ _ repr -> pretty repr
    LitChar _ _ repr -> pretty repr
    LitCharHash _ _ repr -> pretty repr
    LitString _ _ repr -> pretty repr
    LitStringHash _ _ repr -> pretty repr

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
      (TypeHeadInfix, [lhs, rhs]) ->
        [pretty (tyVarBinderName lhs), pretty nm, pretty (tyVarBinderName rhs)]
      _ ->
        [prettyConstructorUName nm] <> map prettyTyVarBinder prms

prettyTyVarBinder :: TyVarBinder -> Doc ann
prettyTyVarBinder binder =
  case (tyVarBinderSpecificity binder, tyVarBinderKind binder) of
    (TyVarBInferred, Nothing) -> braces (pretty (tyVarBinderName binder))
    (TyVarBInferred, Just kind) -> braces (pretty (tyVarBinderName binder) <+> "::" <+> prettyType kind)
    (TyVarBSpecified, Nothing) -> pretty (tyVarBinderName binder)
    (TyVarBSpecified, Just kind) -> parens (pretty (tyVarBinderName binder) <+> "::" <+> prettyType kind)

contextPrefix :: [Type] -> [Doc ann]
contextPrefix constraints =
  case constraints of
    [] -> []
    _ -> [prettyContext constraints, "=>"]

forallTyVarBinderPrefix :: [TyVarBinder] -> [Doc ann]
forallTyVarBinderPrefix [] = []
forallTyVarBinderPrefix binders = ["forall", hsep (map prettyTyVarBinder binders) <> "."]

prettyDataCon :: DataConDecl -> Doc ann
prettyDataCon ctor =
  case ctor of
    PrefixCon _ forallVars constraints name fields ->
      hsep (dataConQualifierPrefix forallVars constraints <> [prettyConstructorUName name] <> map prettyBangType fields)
    InfixCon _ forallVars constraints lhs op rhs ->
      hsep
        ( dataConQualifierPrefix forallVars constraints
            <> [prettyBangType lhs, prettyInfixOp (renderUnqualifiedName op), prettyBangType rhs]
        )
    RecordCon _ forallVars constraints name fields ->
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
          | isOperatorToken (renderUnqualifiedName fieldName) = parens (pretty fieldName)
          | otherwise = pretty fieldName
    GadtCon _ forallBinders constraints names body ->
      prettyGadtCon forallBinders constraints names body

prettyGadtCon :: [TyVarBinder] -> [Type] -> [UnqualifiedName] -> GadtBody -> Doc ann
prettyGadtCon forallBinders constraints names body =
  hsep
    ( [hsep (punctuate comma (map prettyConstructorUName names)), "::"]
        <> forallPart
        <> contextPart
        <> [prettyGadtBody body]
    )
  where
    forallPart
      | null forallBinders = []
      | otherwise = ["forall", hsep (map prettyTyVarBinder forallBinders) <> "."]
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
      | isOperatorToken (renderUnqualifiedName name) = parens (pretty name)
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
    ClassItemTypeSig _ names ty -> hsep [hsep (punctuate comma (map prettyBinderName names)), "::", prettyType ty]
    ClassItemDefaultSig _ name ty -> hsep ["default", prettyBinderName name, "::", prettyType ty]
    ClassItemFixity _ assoc mNamespace prec ops ->
      hsep
        ( [prettyFixityAssoc assoc]
            <> maybe [] (pure . pretty . show) prec
            <> maybe [] (pure . prettyNamespace) mNamespace
            <> map (prettyInfixOp . renderUnqualifiedName) ops
        )
    ClassItemDefault _ valueDecl -> prettyValueDeclSingleLine valueDecl
    ClassItemTypeFamilyDecl _ tf -> prettyAssocTypeFamilyDecl tf
    ClassItemDataFamilyDecl _ df -> prettyAssocDataFamilyDecl df
    ClassItemDefaultTypeInst _ tfi -> prettyDefaultTypeInst tfi
    ClassItemPragma _ pragma -> prettyPragma pragma

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
prettyInstanceWarning (DeprText _ msg) = "{-# DEPRECATED " <> pretty (show msg) <> " #-}"
prettyInstanceWarning (WarnText _ msg) = "{-# WARNING " <> pretty (show msg) <> " #-}"

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
    hsep ([pretty (instanceDeclClassName decl)] <> map prettyType (instanceDeclTypes decl))

standaloneDerivingHeadDoc :: StandaloneDerivingDecl -> Doc ann
standaloneDerivingHeadDoc decl =
  maybeParenthesize (standaloneDerivingParenthesizedHead decl) $
    hsep ([pretty (standaloneDerivingClassName decl)] <> map prettyType (standaloneDerivingTypes decl))

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
    InstanceItemBind _ valueDecl -> prettyValueDeclSingleLine valueDecl
    InstanceItemTypeSig _ names ty -> hsep [hsep (punctuate comma (map prettyBinderName names)), "::", prettyType ty]
    InstanceItemFixity _ assoc mNamespace prec ops ->
      hsep
        ( [prettyFixityAssoc assoc]
            <> maybe [] (pure . pretty . show) prec
            <> maybe [] (pure . prettyNamespace) mNamespace
            <> map (prettyInfixOp . renderUnqualifiedName) ops
        )
    InstanceItemTypeFamilyInst _ tfi -> prettyInstTypeFamilyInst tfi
    InstanceItemDataFamilyInst _ dfi -> prettyInstDataFamilyInst dfi
    InstanceItemPragma _ pragma -> prettyPragma pragma

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

prettyInfixOp :: Text -> Doc ann
prettyInfixOp op
  | isOperatorToken op = pretty op
  | otherwise = "`" <> pretty op <> "`"

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
  | unqualifiedNameType name == NameVarSym || unqualifiedNameType name == NameConSym = parens (pretty (renderUnqualifiedName name))
  | otherwise = pretty (renderUnqualifiedName name)

prettyBinderName :: UnqualifiedName -> Doc ann
prettyBinderName = prettyFunctionBinder

prettyBinderUName :: UnqualifiedName -> Doc ann
prettyBinderUName = prettyFunctionBinder

prettyName :: Name -> Doc ann
prettyName name
  | nameType name == NameVarSym || nameType name == NameConSym = parens (pretty (renderName name))
  | otherwise = pretty (renderName name)

prettyConstructorName :: Text -> Doc ann
prettyConstructorName name
  | isOperatorToken name = parens (pretty name)
  | otherwise = pretty name

prettyConstructorUName :: UnqualifiedName -> Doc ann
prettyConstructorUName = prettyConstructorName . renderUnqualifiedName

-- | Pretty-print an expression. The AST is assumed to already have EParen
-- nodes in the correct positions (inserted by 'addExprParens').
prettyExpr :: Expr -> Doc ann
prettyExpr expr =
  case expr of
    EApp _ fn arg -> prettyExpr fn <+> prettyExpr arg
    ETypeApp _ fn ty -> prettyExpr fn <+> "@" <> prettyType ty
    EVar _ name
      | isSymbolicName name -> parens (pretty (renderName name))
      | otherwise -> pretty name
    EInt _ _ repr -> pretty repr
    EIntHash _ _ repr -> pretty repr
    EIntBase _ _ repr -> pretty repr
    EIntBaseHash _ _ repr -> pretty repr
    EFloat _ _ repr -> pretty repr
    EFloatHash _ _ repr -> pretty repr
    EChar _ _ repr -> pretty repr
    ECharHash _ _ repr -> pretty repr
    EString _ _ repr -> pretty repr
    EStringHash _ _ repr -> pretty repr
    EOverloadedLabel _ _ raw -> pretty (" " <> raw)
    EQuasiQuote _ quoter body -> prettyQuasiQuote quoter body
    ETHExpQuote _ body -> "[|" <+> prettyExpr body <+> "|]"
    ETHTypedQuote _ body -> "[||" <+> prettyExpr body <+> "||]"
    ETHDeclQuote _ decls -> "[d|" <+> prettyInlineDecls decls <+> "|]"
    ETHTypeQuote _ ty -> "[t|" <+> prettyType ty <+> "|]"
    ETHPatQuote _ pat -> "[p|" <+> prettyPattern pat <+> "|]"
    ETHNameQuote _ name
      | isOperatorToken name -> "'" <> parens (pretty name)
      | otherwise -> "'" <> pretty name
    ETHTypeNameQuote _ name
      | isOperatorName name -> "''" <> parens (pretty name)
      | otherwise -> "''" <> pretty name
    ETHSplice _ body -> "$" <> prettyExpr body
    ETHTypedSplice _ body -> "$$" <> prettyExpr body
    EIf _ cond yes no ->
      "if" <+> prettyExpr cond <+> "then" <+> prettyExpr yes <+> "else" <+> prettyExpr no
    EMultiWayIf _ rhss ->
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
    ELambdaPats _ pats body ->
      "\\" <+> hsep (map prettyPattern pats) <+> "->" <+> prettyExpr body
    ELambdaCase _ alts ->
      "\\" <> "case" <+> "{" <+> hsep (punctuate semi (map prettyCaseAlt alts)) <+> "}"
    EInfix _ lhs op rhs ->
      prettyExpr lhs <+> prettyNameInfixOp op <+> prettyExpr rhs
    ENegate _ inner -> "-" <> prettyExpr inner
    ESectionL _ lhs op ->
      parens (prettyExpr lhs <+> prettyNameInfixOp op)
    ESectionR _ op rhs -> parens (prettyNameInfixOp op <+> prettyExpr rhs)
    ELetDecls _ decls body ->
      "let"
        <+> braces (prettyInlineDecls decls)
        <+> "in"
        <+> prettyExpr body
    ECase _ scrutinee alts ->
      "case"
        <+> prettyExpr scrutinee
        <+> "of"
        <+> "{"
        <+> hsep (punctuate semi (map prettyCaseAlt alts))
        <+> "}"
    EDo _ stmts isMdo ->
      (if isMdo then "mdo" else "do")
        <+> "{"
        <+> hsep (punctuate semi (map prettyDoStmt stmts))
        <+> "}"
    EListComp _ body quals ->
      brackets
        ( prettyExpr body
            <+> "|"
            <+> hsep (punctuate comma (map prettyCompStmt quals))
        )
    EListCompParallel _ body qualifierGroups ->
      brackets
        ( prettyExpr body
            <+> "|"
            <+> hsep
              ( punctuate
                  " |"
                  (map (hsep . punctuate comma . map prettyCompStmt) qualifierGroups)
              )
        )
    EArithSeq _ seqInfo -> prettyArithSeq seqInfo
    ERecordCon _ name fields hasWildcard ->
      pretty name <+> braces (hsep (punctuate comma (map prettyBinding fields ++ [".." | hasWildcard])))
    ERecordUpd _ base fields ->
      prettyExpr base <+> braces (hsep (punctuate comma (map prettyBinding fields)))
    ETypeSig _ inner ty -> prettyExpr inner <+> "::" <+> prettyType ty
    EParen _ inner ->
      case inner of
        ESectionL {} -> prettyExpr inner
        ESectionR {} -> prettyExpr inner
        _ -> parens (prettyExpr inner)
    EList _ values -> brackets (hsep (punctuate comma (map prettyExpr values)))
    ETuple _ tupleFlavor values ->
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
    EUnboxedSum _ altIdx arity inner ->
      let slots = [if i == altIdx then prettyExpr inner else mempty | i <- [0 .. arity - 1]]
       in hsep ["(#", hsep (punctuate " |" slots), "#)"]
    EProc _ pat body ->
      "proc" <+> prettyPattern pat <+> "->" <+> prettyCmd body
    EAnn _ sub -> prettyExpr sub

prettyTupleBody :: TupleFlavor -> Doc ann -> Doc ann
prettyTupleBody tupleFlavor inner =
  case tupleFlavor of
    Boxed -> parens inner
    Unboxed -> hsep ["(#", inner, "#)"]

prettyBinding :: (Text, Expr) -> Doc ann
prettyBinding (name, value) =
  case value of
    EVar _ varName | renderName varName == name -> pretty name
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

prettyGuardQualifier :: GuardQualifier -> Doc ann
prettyGuardQualifier qualifier =
  case qualifier of
    GuardExpr _ expr -> prettyExpr expr
    GuardPat _ pat expr -> prettyPattern pat <+> "<-" <+> prettyExpr expr
    GuardLet _ decls -> "let" <+> braces (prettyInlineDecls decls)

prettyDoStmt :: DoStmt Expr -> Doc ann
prettyDoStmt stmt =
  case stmt of
    DoBind _ pat expr -> prettyPattern pat <+> "<-" <+> prettyExpr expr
    DoLetDecls _ decls -> "let" <+> braces (prettyInlineDecls decls)
    DoExpr _ expr -> prettyExpr expr
    DoRecStmt _ stmts -> "rec" <+> "{" <+> hsep (punctuate semi (map prettyDoStmt stmts)) <+> "}"

-- | Pretty-print an arrow command.
prettyCmd :: Cmd -> Doc ann
prettyCmd cmd =
  case cmd of
    CmdArrApp _ lhs HsFirstOrderApp rhs ->
      prettyExpr lhs <+> "-<" <+> prettyExpr rhs
    CmdArrApp _ lhs HsHigherOrderApp rhs ->
      prettyExpr lhs <+> "-<<" <+> prettyExpr rhs
    CmdInfix _ l op r ->
      prettyCmd l <+> prettyInfixOp op <+> prettyCmd r
    CmdDo _ stmts ->
      "do" <+> "{" <+> hsep (punctuate semi (map prettyCmdStmt stmts)) <+> "}"
    CmdIf _ cond yes no ->
      "if" <+> prettyExpr cond <+> "then" <+> prettyCmd yes <+> "else" <+> prettyCmd no
    CmdCase _ scrut alts ->
      "case" <+> prettyExpr scrut <+> "of" <+> "{" <+> hsep (punctuate semi (map prettyCmdCaseAlt alts)) <+> "}"
    CmdLet _ decls body ->
      "let" <+> braces (prettyInlineDecls decls) <+> "in" <+> prettyCmd body
    CmdLam _ pats body ->
      "\\" <+> hsep (map prettyPattern pats) <+> "->" <+> prettyCmd body
    CmdApp _ c e ->
      prettyCmd c <+> prettyExpr e
    CmdPar _ c ->
      parens (prettyCmd c)

prettyCmdStmt :: DoStmt Cmd -> Doc ann
prettyCmdStmt stmt =
  case stmt of
    DoBind _ pat cmd' -> prettyPattern pat <+> "<-" <+> prettyCmd cmd'
    DoLetDecls _ decls -> "let" <+> braces (prettyInlineDecls decls)
    DoExpr _ cmd' -> prettyCmd cmd'
    DoRecStmt _ stmts -> "rec" <+> "{" <+> hsep (punctuate semi (map prettyCmdStmt stmts)) <+> "}"

prettyCmdCaseAlt :: CmdCaseAlt -> Doc ann
prettyCmdCaseAlt alt =
  prettyPattern (cmdCaseAltPat alt) <+> "->" <+> prettyCmd (cmdCaseAltBody alt)

prettyCompStmt :: CompStmt -> Doc ann
prettyCompStmt stmt =
  case stmt of
    CompGen _ pat expr -> prettyPattern pat <+> "<-" <+> prettyExpr expr
    CompGuard _ expr -> prettyExpr expr
    CompLet _ bindings -> "let" <+> hsep (punctuate semi (map prettyBinding bindings))
    CompLetDecls _ decls -> "let" <+> braces (prettyInlineDecls decls)

prettyInlineDecls :: [Decl] -> Doc ann
prettyInlineDecls decls =
  hsep (punctuate semi (map prettyInlineDecl decls))
  where
    prettyInlineDecl decl = case decl of
      DeclValue _ valueDecl -> prettyValueDeclSingleLine valueDecl
      _ -> hsep (prettyDeclLines decl)

prettyArithSeq :: ArithSeq -> Doc ann
prettyArithSeq seqInfo =
  case seqInfo of
    ArithSeqFrom _ fromExpr -> brackets (prettyExpr fromExpr <> " ..")
    ArithSeqFromThen _ fromExpr thenExpr -> brackets (prettyExpr fromExpr <> ", " <> prettyExpr thenExpr <> " ..")
    ArithSeqFromTo _ fromExpr toExpr -> brackets (prettyExpr fromExpr <> " .. " <> prettyExpr toExpr)
    ArithSeqFromThenTo _ fromExpr thenExpr toExpr ->
      brackets (prettyExpr fromExpr <> ", " <> prettyExpr thenExpr <> " .. " <> prettyExpr toExpr)

quoted :: Text -> Doc ann
quoted txt = pretty (show (T.unpack txt))

prettyQuasiQuote :: Text -> Text -> Doc ann
prettyQuasiQuote quoter body = "[" <> pretty quoter <> "|" <> pretty body <> "|]"

isOperatorName :: Name -> Bool
isOperatorName name =
  let ty = nameType name
   in ty == NameVarSym || ty == NameConSym

isOperatorToken :: Text -> Bool
isOperatorToken tok =
  not (T.null tok) && T.all isSymbolicOpChar tok

isSymbolicOpChar :: Char -> Bool
isSymbolicOpChar c =
  c `elem` (":!#$%&*+./<=>?@\\^|-~" :: String) || isUnicodeSymbolCategory c

isUnicodeSymbolCategory :: Char -> Bool
isUnicodeSymbolCategory c = case generalCategory c of
  MathSymbol -> True
  CurrencySymbol -> True
  ModifierSymbol -> True
  OtherSymbol -> True
  OtherPunctuation -> not (isAscii c)
  _ -> False

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
      <> ctorPart (dataFamilyInstConstructors dfi)
      <> derivingParts (dataFamilyInstDeriving dfi)
  where
    keyword = if dataFamilyInstIsNewtype dfi then "newtype" else "data"
    forallPart [] = []
    forallPart binders = ["forall", hsep (map prettyTyVarBinder binders) <> "."]
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
    TypeHeadInfix -> [prettyTypeFamilyInfix headType]

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

prettyNamedTypeHead :: TypeHeadForm -> Text -> [TyVarBinder] -> [Doc ann]
prettyNamedTypeHead headForm name params =
  case (headForm, params) of
    (TypeHeadInfix, [lhs, rhs]) -> [pretty (tyVarBinderName lhs), pretty name, pretty (tyVarBinderName rhs)]
    _ -> [prettyConstructorName name] <> map prettyTyVarBinder params

prettyTypeFamilyInfix :: Type -> Doc ann
prettyTypeFamilyInfix ty =
  case ty of
    TApp _ (TApp _ (TCon _ op promoted) lhs) rhs ->
      prettyType lhs
        <+> (if promoted == Promoted then "'" else mempty)
        <> prettyNameInfixOp op
        <+> prettyType rhs
    _ -> prettyType ty

prettyInstDataFamilyInst :: DataFamilyInst -> Doc ann
prettyInstDataFamilyInst dfi =
  hsep $
    [keyword, prettyType (dataFamilyInstHead dfi)]
      <> ctorPart (dataFamilyInstConstructors dfi)
      <> derivingParts (dataFamilyInstDeriving dfi)
  where
    keyword = if dataFamilyInstIsNewtype dfi then "newtype" else "data"
    ctorPart [] = []
    ctorPart ctors@(c : _)
      | any isGadtCon ctors = ["where", braces (hsep (punctuate semi (map prettyDataCon ctors)))]
      | dataFamilyInstIsNewtype dfi = ["=", prettyDataCon c]
      | otherwise = ["=", hsep (punctuate " |" (map prettyDataCon ctors))]
