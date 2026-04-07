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
-- This module has an empty export list because it only provides typeclass
-- instances. Import it to bring the 'Pretty' instances into scope.
--
-- __Provided instances:__ 'Pretty' for 'Module', 'Expr', 'Pattern', 'Type'.
module Aihc.Parser.Pretty
  (
  )
where

import Aihc.Parser.Syntax
import Data.Char (GeneralCategory (..), generalCategory)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    braces,
    brackets,
    comma,
    hsep,
    parens,
    punctuate,
    semi,
    vsep,
    (<+>),
  )

-- | Pretty instance for Module - renders to valid Haskell source code.
instance Pretty Module where
  pretty = prettyModuleDoc

-- | Pretty instance for Expr - renders to valid Haskell source code.
instance Pretty Expr where
  pretty = prettyExprPrec 0

-- | Pretty instance for Pattern - renders to valid Haskell source code.
instance Pretty Pattern where
  pretty = prettyPattern

-- | Pretty instance for Type - renders to valid Haskell source code.
instance Pretty Type where
  pretty = prettyType

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
    ExportAnn _ sub -> prettyExportSpec sub
    ExportModule _ modName -> "module" <+> pretty modName
    ExportVar _ namespace name -> prettyNamespacePrefix namespace <> prettyBinderName name
    ExportAbs _ namespace name -> prettyNamespacePrefix namespace <> prettyConstructorName name
    ExportAll _ namespace name -> prettyNamespacePrefix namespace <> prettyConstructorName name <> "(..)"
    ExportWith _ namespace name members ->
      prettyNamespacePrefix namespace <> prettyConstructorName name <> parens (hsep (punctuate comma (map prettyBinderName members)))

prettyImportDecl :: ImportDecl -> Doc ann
prettyImportDecl decl =
  let renderPostQualified =
        importDeclQualifiedPost decl
          && importDeclQualified decl
   in hsep
        ( ["import"]
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

prettyDeclSplice :: Expr -> Doc ann
prettyDeclSplice body =
  case body of
    EVar {} -> prettySplice "$" body
    EParen {} -> prettySplice "$" body
    _ -> prettyExprPrec 0 body

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
    ImportItemVar _ namespace name -> prettyNamespacePrefix namespace <> prettyBinderName name
    ImportItemAbs _ namespace name -> prettyNamespacePrefix namespace <> prettyConstructorName name
    ImportItemAll _ namespace name -> prettyNamespacePrefix namespace <> prettyConstructorName name <> "(..)"
    ImportItemWith _ namespace name members ->
      prettyNamespacePrefix namespace <> prettyConstructorName name <> parens (hsep (punctuate comma (map prettyBinderName members)))

prettyNamespacePrefix :: Maybe Text -> Doc ann
prettyNamespacePrefix namespace =
  case namespace of
    Just ns -> pretty ns <> " "
    Nothing -> mempty

prettyDeclLines :: Decl -> [Doc ann]
prettyDeclLines decl =
  case decl of
    DeclValue _ valueDecl -> prettyValueDeclLines valueDecl
    DeclTypeSig _ names ty -> [hsep [hsep (punctuate comma (map prettyBinderName names)), "::", prettyType ty]]
    DeclPatSyn _ patSynDecl -> [prettyPatSynDecl patSynDecl]
    DeclPatSynSig _ names ty -> [hsep ["pattern", hsep (punctuate comma (map prettyConstructorName names)), "::", prettyType ty]]
    DeclStandaloneKindSig _ name kind -> [hsep ["type", prettyConstructorName name, "::", prettyType kind]]
    DeclFixity _ assoc prec ops ->
      [ hsep
          ( [prettyFixityAssoc assoc]
              <> maybe [] (pure . pretty . show) prec
              <> map prettyInfixOp ops
          )
      ]
    DeclRoleAnnotation _ ann -> [prettyRoleAnnotation ann]
    DeclTypeSyn _ synDecl ->
      [ hsep
          [ "type",
            pretty (typeSynName synDecl),
            hsep (map prettyTyVarBinder (typeSynParams synDecl)),
            "=",
            prettyType (typeSynBody synDecl)
          ]
      ]
    DeclData _ dataDecl -> [prettyDataDecl dataDecl]
    DeclTypeData _ dataDecl -> [prettyTypeDataDecl dataDecl]
    DeclNewtype _ newtypeDecl -> [prettyNewtypeDecl newtypeDecl]
    DeclClass _ classDecl -> [prettyClassDecl classDecl]
    DeclInstance _ instanceDecl -> [prettyInstanceDecl instanceDecl]
    DeclStandaloneDeriving _ derivingDecl -> [prettyStandaloneDeriving derivingDecl]
    DeclDefault _ tys -> ["default" <+> parens (hsep (punctuate comma (map prettyType tys)))]
    DeclForeign _ foreignDecl -> [prettyForeignDecl foreignDecl]
    DeclSplice _ body -> [prettyDeclSplice body]
    DeclTypeFamilyDecl _ tf -> [prettyTypeFamilyDecl tf]
    DeclDataFamilyDecl _ df -> [prettyDataFamilyDecl df]
    DeclTypeFamilyInst _ tfi -> [prettyTopTypeFamilyInst tfi]
    DeclDataFamilyInst _ dfi -> [prettyTopDataFamilyInst dfi]
    DeclAnn _ sub -> prettyDeclLines sub

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
-- For function binds with multiple matches, each match becomes a semicolon-separated item.
-- For function binds with guards, the guards are space-separated.
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

-- | Pretty-print the LHS of a pattern synonym declaration (after @pattern@).
prettyPatSynLhs :: Text -> PatSynArgs -> [Doc ann]
prettyPatSynLhs name args =
  case args of
    PatSynPrefixArgs vars ->
      prettyConstructorName name : map pretty vars
    PatSynInfixArgs lhs rhs ->
      [pretty lhs, prettyInfixOp name, pretty rhs]
    PatSynRecordArgs fields ->
      [prettyConstructorName name <+> braces (hsep (punctuate comma (map pretty fields)))]

-- | Pretty-print the where clause of an explicitly bidirectional pattern synonym.
prettyPatSynWhere :: Text -> PatSynDir -> [Doc ann]
prettyPatSynWhere _ PatSynBidirectional = []
prettyPatSynWhere _ PatSynUnidirectional = []
prettyPatSynWhere name (PatSynExplicitBidirectional matches) =
  ["where", braces (hsep (punctuate semi (map (prettyFunctionMatch name) matches)))]

prettyFunctionMatchLines :: Text -> Match -> [Doc ann]
prettyFunctionMatchLines name match =
  case matchRhs match of
    UnguardedRhs _ _ -> [prettyFunctionMatch name match]
    GuardedRhss _ grhss ->
      prettyFunctionHead name (matchHeadForm match) (matchPats match)
        : [ "  |"
              <+> hsep (punctuate comma (map prettyGuardQualifier (guardedRhsGuards grhs)))
              <+> "="
              <+> prettyExprPrec 0 (guardedRhsBody grhs)
          | grhs <- grhss
          ]

prettyFunctionMatch :: Text -> Match -> Doc ann
prettyFunctionMatch name match =
  prettyFunctionHead name (matchHeadForm match) (matchPats match) <+> prettyRhs (matchRhs match)

prettyFunctionHead :: Text -> MatchHeadForm -> [Pattern] -> Doc ann
prettyFunctionHead name headForm pats =
  case headForm of
    MatchHeadPrefix ->
      hsep (prettyFunctionBinder name : map prettyPattern pats)
    MatchHeadInfix ->
      case pats of
        lhs : rhsPat : tailPats ->
          let infixHead = prettyPattern lhs <+> prettyInfixOp name <+> prettyPattern rhsPat
           in case tailPats of
                [] -> infixHead
                _ -> hsep (parens infixHead : map prettyPattern tailPats)
        _ ->
          hsep (prettyFunctionBinder name : map prettyPattern pats)

prettyRhs :: Rhs -> Doc ann
prettyRhs rhs =
  case rhs of
    -- For UnguardedRhs, nothing follows the expression, so no parens needed
    UnguardedRhs _ expr -> "=" <+> prettyExprPrec 0 expr
    -- For GuardedRhss, multiple guards can follow, but brace-terminated
    -- expressions (do, case, \case) are safe. Open-ended expressions
    -- (if, lambda, let, where) could capture trailing guards with layout,
    -- but our pretty-printer doesn't use layout for guards.
    GuardedRhss _ guards ->
      hsep
        [ "|"
            <+> hsep (punctuate comma (map prettyGuardQualifier (guardedRhsGuards grhs)))
            <+> "="
            <+> prettyExprPrec 0 (guardedRhsBody grhs)
        | grhs <- guards
        ]

prettyType :: Type -> Doc ann
prettyType = prettyTypePrec 0

-- | Type context for parenthesization decisions.
-- CtxTypeFunArg: LHS of -> or function position of type application (same rules).
-- CtxTypeAppArg: argument position of type application.
-- CtxTypeAtom: must be syntactically atomic (e.g., constraint args, instance heads).
data TypeCtx
  = CtxTypeFunArg
  | CtxTypeAppArg
  | CtxTypeAtom

prettyTypeIn :: TypeCtx -> Type -> Doc ann
prettyTypeIn ctx ty =
  parenthesize (needsTypeParens ctx ty) (prettyTypePrec 0 ty)

needsTypeParens :: TypeCtx -> Type -> Bool
needsTypeParens ctx ty =
  case ctx of
    CtxTypeFunArg ->
      case ty of
        TForall {} -> True
        TFun {} -> True
        TContext {} -> True
        _ -> False
    CtxTypeAppArg ->
      case ty of
        TApp _ (TApp _ (TCon _ op _) _) _
          | isSymbolicTypeOperator op && op /= "->" -> False
        TQuasiQuote {} -> False
        TApp {} -> True
        TForall {} -> True
        TFun {} -> True
        TContext {} -> True
        _ -> False
    CtxTypeAtom ->
      case ty of
        TVar {} -> False
        TCon {} -> False
        TTypeLit {} -> False
        TStar {} -> False
        TQuasiQuote {} -> False
        TList {} -> False
        TTuple {} -> False
        TUnboxedSum {} -> False
        TParen {} -> False
        TKindSig {} -> False
        TWildcard {} -> False
        _ -> True

prettyTypePrec :: Int -> Type -> Doc ann
prettyTypePrec prec ty =
  case ty of
    TVar _ name -> pretty name
    TCon _ name promoted ->
      let base
            | isSymbolicTypeOperator name = parens (pretty name)
            | otherwise = pretty name
       in if promoted == Promoted then "'" <> base else base
    TTypeLit _ lit -> prettyTypeLiteral lit
    TStar _ -> "*"
    TQuasiQuote _ quoter body -> prettyQuasiQuote quoter body
    TForall _ binders inner ->
      parenthesize
        (prec > 0)
        ("forall" <+> hsep (map pretty binders) <> "." <+> prettyTypePrec 0 inner)
    TApp _ (TApp _ (TCon _ op promoted) lhs) rhs
      | isSymbolicTypeOperator op && op /= "->" ->
          prettyTypePrec 0 lhs
            <+> (if promoted == Promoted then "'" else mempty)
            <> pretty op
            <+> prettyTypePrec 0 rhs
    TApp _ f x ->
      parenthesize
        (prec > 2)
        (prettyTypeIn CtxTypeFunArg f <+> prettyTypeIn CtxTypeAppArg x)
    TFun _ a b ->
      parenthesize
        (prec > 0)
        (prettyTypeIn CtxTypeFunArg a <+> "->" <+> prettyTypePrec 0 b)
    TTuple _ tupleFlavor promoted elems ->
      let tupleDoc = prettyTupleBody tupleFlavor (hsep (punctuate comma (map (prettyTypePrec 0) elems)))
       in if promoted == Promoted then "'" <> tupleDoc else tupleDoc
    TUnboxedSum _ elems ->
      hsep ["(#", hsep (punctuate " |" (map (prettyTypePrec 0) elems)), "#)"]
    TList _ promoted inner ->
      let listDoc = brackets (prettyTypePrec 0 inner)
       in if promoted == Promoted then "'" <> listDoc else listDoc
    TParen _ inner -> parens (prettyTypePrec 0 inner)
    TKindSig _ ty' kind -> parens (prettyTypePrec 0 ty' <+> "::" <+> prettyTypePrec 0 kind)
    TContext _ constraints inner ->
      parenthesize
        (prec > 0)
        (prettyContext constraints <+> "=>" <+> prettyTypePrec 0 inner)
    TSplice _ body -> prettySplice "$" body
    TWildcard _ -> "_"
    TAnn _ sub -> prettyTypePrec prec sub

prettyContext :: [Constraint] -> Doc ann
prettyContext constraints =
  case constraints of
    [single] -> prettyConstraint single
    _ -> parens (hsep (punctuate comma (map prettyConstraint constraints)))

prettyConstraint :: Constraint -> Doc ann
prettyConstraint constraint =
  case constraint of
    Constraint _ cls [ty]
      | "?" `T.isPrefixOf` cls ->
          pretty cls <+> "::" <+> prettyTypePrec 0 ty
    Constraint _ cls [lhs, rhs]
      | cls == "~" ->
          prettyTypeIn CtxTypeFunArg lhs <+> pretty cls <+> prettyTypeIn CtxTypeFunArg rhs
    Constraint _ cls args ->
      hsep (pretty cls : map (prettyTypeIn CtxTypeAtom) args)
    CParen _ inner ->
      parens (prettyConstraint inner)
    CWildcard _ ->
      "_"

isSymbolicTypeOperator :: Text -> Bool
isSymbolicTypeOperator op =
  case T.uncons op of
    Nothing -> False
    Just _ -> T.all (`elem` (":!#$%&*+./<=>?\\^|-~" :: String)) op

prettyTypeLiteral :: TypeLiteral -> Doc ann
prettyTypeLiteral lit =
  case lit of
    TypeLitInteger _ repr -> pretty repr
    TypeLitSymbol _ repr -> pretty repr
    TypeLitChar _ repr -> pretty repr

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
    PCon _ con args -> hsep (pretty con : map prettyPatternAtom args)
    PInfix _ lhs op rhs -> prettyPatternAtom lhs <+> prettyInfixOp op <+> prettyPatternAtom rhs
    PView _ viewExpr inner -> parens (prettyExprPrec 0 viewExpr <+> "->" <+> prettyPattern inner)
    PAs _ name inner -> pretty name <> "@" <> prettyPatternAtomStrict inner
    PStrict _ inner -> "!" <> prettyPatternAtomStrict inner
    PIrrefutable _ inner -> "~" <> prettyPatternAtomStrict inner
    PNegLit _ lit -> "-" <> prettyLiteral lit
    PParen _ inner -> parens (prettyPattern inner)
    PRecord _ con fields hasWildcard ->
      pretty con
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
    PSplice _ body -> prettySplice "$" body

-- | Pretty print a pattern field binding.
-- Supports NamedFieldPuns: if pattern is a variable with the same name as the field,
-- print just the field name (punned form).
-- Pattern fields are comma-separated, so greedy patterns don't need parens.
prettyPatternFieldBinding :: Text -> Pattern -> Doc ann
prettyPatternFieldBinding fieldName fieldPat =
  case fieldPat of
    PVar _ varName | varName == fieldName -> pretty fieldName -- NamedFieldPuns: punned form
    _ -> pretty fieldName <+> "=" <+> prettyPattern fieldPat

prettyPatternAtom :: Pattern -> Doc ann
prettyPatternAtom pat =
  case pat of
    PVar _ _ -> prettyPattern pat
    PWildcard _ -> prettyPattern pat
    PLit _ _ -> prettyPattern pat
    PQuasiQuote {} -> prettyPattern pat
    PNegLit _ _ -> prettyPattern pat
    PList _ _ -> prettyPattern pat
    PTuple {} -> prettyPattern pat
    PUnboxedSum {} -> prettyPattern pat
    PParen _ _ -> prettyPattern pat
    PStrict _ _ -> prettyPattern pat
    PView {} -> prettyPattern pat
    PAs {} -> prettyPattern pat
    PSplice {} -> prettyPattern pat
    PCon _ _ [] -> prettyPattern pat
    _ -> parens (prettyPattern pat)

-- | Pretty print a pattern atom after @ or as the operand of ! or ~.
-- Negative literals and nested strictness/irrefutability need parens.
prettyPatternAtomStrict :: Pattern -> Doc ann
prettyPatternAtomStrict pat =
  case pat of
    PNegLit {} -> parens (prettyPattern pat)
    PStrict {} -> parens (prettyPattern pat)
    PIrrefutable {} -> parens (prettyPattern pat)
    _ -> prettyPatternAtom pat

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
        prettyDeclHead (dataDeclContext decl) (dataDeclName decl) (dataDeclParams decl)
      ]
        <> ctorPart
        <> derivingParts (dataDeclDeriving decl)
    )
  where
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
        prettyDeclHead (dataDeclContext decl) (dataDeclName decl) (dataDeclParams decl)
      ]
        <> ctorPart
    )
  where
    ctorPart =
      case dataDeclConstructors decl of
        [] -> []
        ctors
          | any isGadtCon ctors -> ["where", braces (hsep (punctuate semi (map prettyDataCon ctors)))]
          | otherwise -> ["=", hsep (punctuate " |" (map prettyDataCon ctors))]

-- | Check if a constructor uses GADT syntax
isGadtCon :: DataConDecl -> Bool
isGadtCon (GadtCon {}) = True
isGadtCon _ = False

prettyNewtypeDecl :: NewtypeDecl -> Doc ann
prettyNewtypeDecl decl =
  hsep
    ( [ "newtype",
        prettyDeclHead (newtypeDeclContext decl) (newtypeDeclName decl) (newtypeDeclParams decl)
      ]
        <> ctorPart
        <> derivingParts (newtypeDeclDeriving decl)
    )
  where
    ctorPart =
      case newtypeDeclConstructor decl of
        Nothing -> []
        Just ctor
          | isGadtCon ctor -> ["where", braces (prettyDataCon ctor)]
          | otherwise -> ["=", prettyDataCon ctor]

derivingParts :: [DerivingClause] -> [Doc ann]
derivingParts = concatMap derivingPart

derivingPart :: DerivingClause -> [Doc ann]
derivingPart (DerivingClause strategy classes viaTy) =
  ["deriving"] <> strategyPart strategy <> classesPart classes <> viaPart viaTy
  where
    strategyPart Nothing = []
    strategyPart (Just DerivingStock) = ["stock"]
    strategyPart (Just DerivingNewtype) = ["newtype"]
    strategyPart (Just DerivingAnyclass) = ["anyclass"]

    classesPart [] = ["()"]
    classesPart [single]
      | Just DerivingStock <- strategy = [parens (prettyConstraint single)]
      | otherwise = [prettyConstraint single]
    classesPart _ = [parens (hsep (punctuate comma (map prettyConstraint classes)))]

    viaPart Nothing = []
    viaPart (Just ty) = ["via", prettyType ty]

prettyDeclHead :: [Constraint] -> Text -> [TyVarBinder] -> Doc ann
prettyDeclHead constraints name params =
  hsep
    ( contextPrefix constraints
        <> [pretty name]
        <> map prettyTyVarBinder params
    )

prettyTyVarBinder :: TyVarBinder -> Doc ann
prettyTyVarBinder binder =
  case tyVarBinderKind binder of
    Nothing -> pretty (tyVarBinderName binder)
    Just kind -> parens (pretty (tyVarBinderName binder) <+> "::" <+> prettyType kind)

contextPrefix :: [Constraint] -> [Doc ann]
contextPrefix constraints =
  case constraints of
    [] -> []
    _ -> [prettyContext constraints, "=>"]

-- | Render a forall prefix for [TyVarBinder]
forallTyVarBinderPrefix :: [TyVarBinder] -> [Doc ann]
forallTyVarBinderPrefix [] = []
forallTyVarBinderPrefix binders = ["forall", hsep (map prettyTyVarBinder binders) <> "."]

prettyDataCon :: DataConDecl -> Doc ann
prettyDataCon ctor =
  case ctor of
    PrefixCon _ forallVars constraints name fields ->
      hsep (dataConQualifierPrefix forallVars constraints <> [prettyConstructorName name] <> map prettyBangType fields)
    InfixCon _ forallVars constraints lhs op rhs ->
      hsep
        ( dataConQualifierPrefix forallVars constraints
            <> [prettyBangTypeAtom lhs, prettyInfixOp op, prettyBangTypeAtom rhs]
        )
    RecordCon _ forallVars constraints name fields ->
      hsep (dataConQualifierPrefix forallVars constraints <> [prettyConstructorName name])
        <+> braces
          ( hsep
              ( punctuate
                  comma
                  [ hsep
                      [ hsep (punctuate comma (map pretty (fieldNames fld))),
                        "::",
                        prettyRecordFieldBangType (fieldType fld)
                      ]
                  | fld <- fields
                  ]
              )
          )
    GadtCon _ forallBinders constraints names body ->
      prettyGadtCon forallBinders constraints names body

-- | Pretty print a GADT constructor in GADT syntax: @Con :: forall a. Ctx => Type@
prettyGadtCon :: [TyVarBinder] -> [Constraint] -> [Text] -> GadtBody -> Doc ann
prettyGadtCon forallBinders constraints names body =
  hsep
    ( [hsep (punctuate comma (map prettyConstructorName names)), "::"]
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

-- | Pretty print the body of a GADT constructor
prettyGadtBody :: GadtBody -> Doc ann
prettyGadtBody body =
  case body of
    GadtPrefixBody args resultTy ->
      case args of
        [] -> prettyType resultTy
        _ -> hsep (punctuate " ->" (map prettyBangType args ++ [prettyType resultTy]))
    GadtRecordBody fields resultTy ->
      braces (prettyRecordFields fields) <+> "->" <+> prettyType resultTy

-- | Pretty print record fields for GADT body
prettyRecordFields :: [FieldDecl] -> Doc ann
prettyRecordFields fields =
  hsep
    ( punctuate
        comma
        [ hsep
            [ hsep (punctuate comma (map pretty (fieldNames fld))),
              "::",
              prettyRecordFieldBangType (fieldType fld)
            ]
        | fld <- fields
        ]
    )

dataConQualifierPrefix :: [Text] -> [Constraint] -> [Doc ann]
dataConQualifierPrefix forallVars constraints = forallPrefix forallVars <> contextPrefix constraints
  where
    forallPrefix [] = []
    forallPrefix binders = ["forall", hsep (map pretty binders) <> "."]

-- | Pretty print a BangType in GADT prefix body context.
-- For strict types (!Type), we use atomic type rendering to ensure the type is atomic
-- (e.g., !Int or !(Term a), not !Term a which would be parsed as (!Term) a).
-- For non-strict types, we use function-LHS context rendering since only function types,
-- foralls, and contexts need parentheses before -> in GADT syntax.
prettyBangType :: BangType -> Doc ann
prettyBangType bt
  | bangStrict bt = "!" <> prettyTypeIn CtxTypeAtom (bangType bt)
  | otherwise = prettyTypeIn CtxTypeFunArg (bangType bt)

prettyRecordFieldBangType :: BangType -> Doc ann
prettyRecordFieldBangType bt
  | bangStrict bt = "!" <> prettyType (bangType bt)
  | otherwise = prettyType (bangType bt)

-- | Pretty print a BangType as an atom (e.g., for infix data constructors).
-- Wraps the entire bang type in parens if the underlying type needs it.
prettyBangTypeAtom :: BangType -> Doc ann
prettyBangTypeAtom bt =
  parenthesize (needsTypeParens CtxTypeFunArg (bangType bt)) (prettyBangType bt)

prettyClassDecl :: ClassDecl -> Doc ann
prettyClassDecl decl =
  let headDoc =
        hsep
          ( ["class"]
              <> maybeContextPrefix (classDeclContext decl)
              <> [pretty (classDeclName decl)]
              <> map prettyTyVarBinder (classDeclParams decl)
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

maybeContextPrefix :: Maybe [Constraint] -> [Doc ann]
maybeContextPrefix maybeConstraints =
  case maybeConstraints of
    Nothing -> []
    Just constraints -> [prettyContext constraints, "=>"]

prettyClassItem :: ClassDeclItem -> Doc ann
prettyClassItem item =
  case item of
    ClassItemTypeSig _ names ty -> hsep [hsep (punctuate comma (map prettyBinderName names)), "::", prettyType ty]
    ClassItemDefaultSig _ name ty -> hsep ["default", prettyBinderName name, "::", prettyType ty]
    ClassItemFixity _ assoc prec ops ->
      hsep
        ( [prettyFixityAssoc assoc]
            <> maybe [] (pure . pretty . show) prec
            <> map prettyInfixOp ops
        )
    ClassItemDefault _ valueDecl -> prettyValueDeclSingleLine valueDecl
    ClassItemTypeFamilyDecl _ tf -> prettyAssocTypeFamilyDecl tf
    ClassItemDataFamilyDecl _ df -> prettyAssocDataFamilyDecl df
    ClassItemDefaultTypeInst _ tfi -> prettyDefaultTypeInst tfi

prettyInstanceDecl :: InstanceDecl -> Doc ann
prettyInstanceDecl decl =
  let headDoc =
        hsep (["instance"] <> maybe [] (\pragma' -> [prettyInstanceOverlapPragma pragma']) (instanceDeclOverlapPragma decl) <> forallTyVarBinderPrefix (instanceDeclForall decl) <> contextPrefix (instanceDeclContext decl) <> [instanceHeadDoc decl])
   in case instanceDeclItems decl of
        [] -> headDoc
        items -> headDoc <+> "where" <+> braces (hsep (punctuate semi (map prettyInstanceItem items)))

prettyStandaloneDeriving :: StandaloneDerivingDecl -> Doc ann
prettyStandaloneDeriving decl =
  hsep
    ( ["deriving"]
        <> maybe [] (\s -> [prettyDerivingStrategy s]) (standaloneDerivingStrategy decl)
        <> maybe [] (\ty -> ["via", prettyType ty]) (standaloneDerivingViaType decl)
        <> ["instance"]
        <> maybe [] (\pragma' -> [prettyInstanceOverlapPragma pragma']) (standaloneDerivingOverlapPragma decl)
        <> forallTyVarBinderPrefix (standaloneDerivingForall decl)
        <> contextPrefix (standaloneDerivingContext decl)
        <> [standaloneDerivingHeadDoc decl]
    )

instanceHeadDoc :: InstanceDecl -> Doc ann
instanceHeadDoc decl =
  maybeParenthesize (instanceDeclParenthesizedHead decl) $
    hsep ([pretty (instanceDeclClassName decl)] <> map (prettyTypeIn CtxTypeAtom) (instanceDeclTypes decl))

standaloneDerivingHeadDoc :: StandaloneDerivingDecl -> Doc ann
standaloneDerivingHeadDoc decl =
  maybeParenthesize (standaloneDerivingParenthesizedHead decl) $
    hsep ([pretty (standaloneDerivingClassName decl)] <> map (prettyTypeIn CtxTypeAtom) (standaloneDerivingTypes decl))

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
    InstanceItemFixity _ assoc prec ops ->
      hsep
        ( [prettyFixityAssoc assoc]
            <> maybe [] (pure . pretty . show) prec
            <> map prettyInfixOp ops
        )
    InstanceItemTypeFamilyInst _ tfi -> prettyInstTypeFamilyInst tfi
    InstanceItemDataFamilyInst _ dfi -> prettyInstDataFamilyInst dfi

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

-- | Check whether an operator is an arrow tail operator (@-<@ or @-<<@).
-- These are special-cased in the pretty-printer to have the lowest precedence.
isArrowTailOp :: Text -> Bool
isArrowTailOp "-<" = True
isArrowTailOp "-<<" = True
isArrowTailOp _ = False

prettyFunctionBinder :: Text -> Doc ann
prettyFunctionBinder name
  | isOperatorToken name = parens (pretty name)
  | otherwise = pretty name

prettyBinderName :: Text -> Doc ann
prettyBinderName = prettyFunctionBinder

prettyConstructorName :: Text -> Doc ann
prettyConstructorName name
  | isOperatorToken name = parens (pretty name)
  | otherwise = pretty name

-- | Print an expression in a context-sensitive slot.
-- Nested infix expressions need context-sensitive parenthesization, not just
-- operator precedence. We model these slots explicitly.
data ExprCtx
  = CtxInfixRhs Bool
  | CtxInfixLhs
  | CtxWhereBody
  | CtxAppFun
  | CtxTypeSigBody
  | CtxGuarded

prettyExprIn :: ExprCtx -> Expr -> Doc ann
prettyExprIn ctx expr =
  parenthesize (needsExprParens ctx expr) (prettyExprPrec (exprCtxPrec ctx expr) expr)

exprCtxPrec :: ExprCtx -> Expr -> Int
exprCtxPrec ctx expr =
  case ctx of
    CtxInfixRhs _
      | isGreedyExpr expr -> 0
      | otherwise -> 1
    CtxInfixLhs -> 1
    CtxWhereBody -> 0
    CtxAppFun -> 2
    CtxTypeSigBody -> 1
    CtxGuarded -> 0

needsExprParens :: ExprCtx -> Expr -> Bool
needsExprParens ctx expr =
  case ctx of
    CtxInfixRhs protectOpenEnded ->
      case expr of
        EInfix {} -> True
        ETypeSig {} -> True
        ENegate {} -> True
        EWhereDecls {} -> True
        _ | protectOpenEnded && isOpenEnded expr -> True
        _ -> False
    CtxInfixLhs ->
      case expr of
        ETypeSig {} -> True
        ENegate {} -> True
        _ -> False
    CtxWhereBody ->
      case expr of
        ENegate {} -> True
        _ -> isOpenEnded expr
    CtxAppFun ->
      case expr of
        ENegate {} -> True
        _ -> False
    CtxTypeSigBody ->
      case expr of
        ENegate {} -> True
        ETypeSig {} -> True
        ELambdaPats {} -> True
        _ -> False
    CtxGuarded -> isGreedyExpr expr

-- | Check if an expression is "greedy" - i.e., it could consume trailing syntax.
-- These expressions may need special handling in certain contexts.
isGreedyExpr :: Expr -> Bool
isGreedyExpr = \case
  ECase {} -> True
  EIf {} -> True
  ELambdaPats {} -> True
  ELambdaCase {} -> True
  ELetDecls {} -> True
  EWhereDecls {} -> True
  EDo {} -> True
  EProc {} -> True
  _ -> False

-- | Print an expression in a "guarded" context where greedy expressions
-- need parentheses to prevent them from consuming trailing syntax.
prettyExprGuarded :: Expr -> Doc ann
prettyExprGuarded = prettyExprIn CtxGuarded

-- | Check if an expression is "open-ended" - its rightmost component can
-- capture a trailing where clause. This includes:
-- - Directly open-ended expressions (if, lambda, let)
-- - Infix expressions whose RHS is open-ended (recursively)
-- Brace-terminated expressions (do, case, \case) are NOT open-ended because
-- their explicit braces delimit them.
isOpenEnded :: Expr -> Bool
isOpenEnded = \case
  EIf {} -> True
  ELambdaPats {} -> True
  ELetDecls {} -> True
  EWhereDecls {} -> True
  EProc {} -> True
  EInfix _ _ _ rhs -> isOpenEnded rhs
  _ -> False

-- | Print the body of a where expression.
prettyWhereBody :: Expr -> Doc ann
prettyWhereBody = prettyExprIn CtxWhereBody

-- | Print an expression used as the function in an application.
prettyExprApp :: Expr -> Doc ann
prettyExprApp = prettyExprIn CtxAppFun

-- | Check whether an expression's pretty-printed form starts with '$'.
-- This matters for negation: -$x lexes as the operator -$ rather than
-- negate applied to a splice.
startsWithDollar :: Expr -> Bool
startsWithDollar (ETHSplice {}) = True
startsWithDollar (ETHTypedSplice {}) = True
startsWithDollar (ERecordUpd _ base _) = startsWithDollar base
startsWithDollar (EApp _ fn _) = startsWithDollar fn
startsWithDollar _ = False

-- | Print a negation expression.
prettyNegate :: Expr -> Doc ann
prettyNegate inner =
  -- Splices start with $ which is a symbolic operator character.
  -- Without parens, -$x lexes as the operator -$ followed by x,
  -- and - $x lexes as a right section. The same applies when a splice
  -- is the leading subexpression of a record update or application.
  if startsWithDollar inner
    then "-" <> parens (prettyExprPrec 0 inner)
    else "-" <> prettyExprPrec 3 inner

-- | Print the body of a type signature expression.
prettyTypeSigBody :: Expr -> Doc ann
prettyTypeSigBody = prettyExprIn CtxTypeSigBody

prettyExprPrec :: Int -> Expr -> Doc ann
prettyExprPrec prec expr =
  case expr of
    EApp _ fn arg ->
      parenthesize (prec > 2) (prettyExprApp fn <+> prettyExprPrec 3 arg)
    ETypeApp _ fn ty ->
      parenthesize (prec > 2) (prettyExprApp fn <+> "@" <> prettyTypeIn CtxTypeAtom ty)
    EVar _ name
      | isOperatorToken name -> parens (pretty name)
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
    EQuasiQuote _ quoter body -> prettyQuasiQuote quoter body
    ETHExpQuote _ body -> "[|" <+> prettyExprPrec 0 body <+> "|]"
    ETHTypedQuote _ body -> "[||" <+> prettyExprPrec 0 body <+> "||]"
    ETHDeclQuote _ decls -> "[d|" <+> prettyInlineDecls decls <+> "|]"
    ETHTypeQuote _ ty -> "[t|" <+> prettyType ty <+> "|]"
    ETHPatQuote _ pat -> "[p|" <+> prettyPattern pat <+> "|]"
    ETHNameQuote _ name
      | isOperatorToken name -> "'" <> parens (pretty name)
      | otherwise -> "'" <> pretty name
    ETHTypeNameQuote _ name
      | isOperatorToken name -> "''" <> parens (pretty name)
      | otherwise -> "''" <> pretty name
    ETHSplice _ body -> prettySplice "$" body
    ETHTypedSplice _ body -> prettySplice "$$" body
    EIf _ cond yes no ->
      -- The 'then' keyword delimits the condition, and 'else' delimits the then-branch,
      -- so greedy expressions in those positions don't need parentheses.
      parenthesize
        (prec > 0)
        ("if" <+> prettyExprPrec 0 cond <+> "then" <+> prettyExprPrec 0 yes <+> "else" <+> prettyExprPrec 0 no)
    EMultiWayIf _ rhss ->
      parenthesize
        (prec > 0)
        ( "if"
            <+> "{"
            <+> hsep
              [ "|"
                  <+> hsep (punctuate comma (map prettyGuardQualifier (guardedRhsGuards grhs)))
                  <+> "->"
                  <+> prettyExprPrec 0 (guardedRhsBody grhs)
              | grhs <- rhss
              ]
            <+> "}"
        )
    ELambdaPats _ pats body ->
      parenthesize (prec > 0) ("\\" <+> hsep (map prettyPattern pats) <+> "->" <+> prettyExprPrec 0 body)
    ELambdaCase _ alts ->
      parenthesize
        (prec > 0)
        ("\\" <> "case" <+> "{" <+> hsep (punctuate semi (map prettyCaseAlt alts)) <+> "}")
    EInfix _ lhs op rhs
      | isArrowTailOp op ->
          -- Arrow application operators (-<, -<<) are command-level syntax
          -- in GHC.  The LHS is a command (which may be a greedy do/if/case)
          -- and the RHS is a full expression.
          -- Always parenthesize the whole expression to avoid ambiguity.
          -- Also parenthesize the LHS to prevent constructs like `where` from
          -- capturing the `-<` operator.
          parenthesize
            True
            (parens (prettyExprPrec 0 lhs) <+> prettyInfixOp op <+> prettyExprPrec 0 rhs)
      | otherwise ->
          parenthesize
            (prec > 1)
            (prettyExprIn CtxInfixLhs lhs <+> prettyInfixOp op <+> prettyExprIn (CtxInfixRhs (prec == 1)) rhs)
    ENegate _ inner -> parenthesize (prec > 2) (prettyNegate inner)
    ESectionL _ lhs op -> parens (prettyExprPrec 3 lhs <+> prettyInfixOp op)
    ESectionR _ op rhs -> parens (prettyInfixOp op <+> prettyExprPrec 0 rhs)
    ELetDecls _ decls body ->
      parenthesize
        (prec > 0)
        ( "let"
            <+> braces (prettyInlineDecls decls)
            <+> "in"
            <+> prettyExprPrec 0 body
        )
    ECase _ scrutinee alts ->
      -- The 'of' keyword delimits the scrutinee, so greedy expressions don't need parens
      -- Use "{ " instead of braces to avoid {- being lexed as block comment start
      parenthesize
        (prec > 0)
        ( "case"
            <+> prettyExprPrec 0 scrutinee
            <+> "of"
            <+> "{"
            <+> hsep (punctuate semi (map prettyCaseAlt alts))
            <+> "}"
        )
    EDo _ stmts isMdo ->
      parenthesize
        (prec > 0)
        ( (if isMdo then "mdo" else "do")
            <+> "{"
            <+> hsep (punctuate semi (map prettyDoStmt stmts))
            <+> "}"
        )
    EListComp _ body quals ->
      -- Brace-terminated expressions in the body don't capture the |
      brackets
        ( prettyExprPrec 0 body
            <+> "|"
            <+> hsep (punctuate comma (map prettyCompStmt quals))
        )
    EListCompParallel _ body qualifierGroups ->
      -- Brace-terminated expressions in the body don't capture the |
      brackets
        ( prettyExprPrec 0 body
            <+> "|"
            <+> hsep
              ( punctuate
                  "|"
                  (map (hsep . punctuate comma . map prettyCompStmt) qualifierGroups)
              )
        )
    EArithSeq _ seqInfo -> prettyArithSeq seqInfo
    ERecordCon _ name fields hasWildcard ->
      pretty name <+> braces (hsep (punctuate comma (map prettyBinding fields ++ [".." | hasWildcard])))
    ERecordUpd _ base fields ->
      prettyExprPrec 3 base <+> braces (hsep (punctuate comma (map prettyBinding fields)))
    ETypeSig _ inner ty -> parenthesize (prec > 1) (prettyTypeSigBody inner <+> "::" <+> prettyType ty)
    EParen _ inner ->
      case inner of
        ESectionL {} -> prettyExprPrec 0 inner
        ESectionR {} -> prettyExprPrec 0 inner
        _ -> parens (prettyExprPrec 0 inner)
    EWhereDecls _ body decls ->
      parenthesize
        (prec > 0)
        (prettyWhereBody body <+> "where" <+> braces (prettyInlineDecls decls))
    EList _ values -> brackets (hsep (punctuate comma (map (prettyExprPrec 0) values)))
    ETuple _ tupleFlavor values ->
      prettyTupleBody
        tupleFlavor
        ( hsep
            ( punctuate
                comma
                ( map
                    ( \case
                        Just val -> prettyExprPrec 0 val
                        Nothing -> mempty
                    )
                    values
                )
            )
        )
    EUnboxedSum _ altIdx arity inner ->
      let slots = [if i == altIdx then prettyExprPrec 0 inner else mempty | i <- [0 .. arity - 1]]
       in hsep ["(#", hsep (punctuate " |" slots), "#)"]
    EProc _ pat body ->
      parenthesize (prec > 0) ("proc" <+> prettyPattern pat <+> "->" <+> prettyCmd body)
    EAnn _ sub -> prettyExprPrec prec sub

prettyTupleBody :: TupleFlavor -> Doc ann -> Doc ann
prettyTupleBody tupleFlavor inner =
  case tupleFlavor of
    Boxed -> parens inner
    Unboxed -> hsep ["(#", inner, "#)"]

-- | Pretty print a record field binding.
-- Supports NamedFieldPuns: if value is a variable with the same name as the field,
-- print just the field name (punned form).
-- Supports RecordWildCards: if name is "..", print just "..".
-- Record fields are comma-separated, so greedy expressions don't need parens.
prettyBinding :: (Text, Expr) -> Doc ann
prettyBinding (name, value) =
  case value of
    EVar _ varName | varName == name -> pretty name -- NamedFieldPuns: punned form
    _ -> pretty name <+> "=" <+> prettyExprPrec 0 value

-- | Pretty print a case alternative.
-- Since case alternatives are separated by semicolons (in explicit brace syntax),
-- greedy expressions in the body don't need parentheses.
prettyCaseAlt :: CaseAlt -> Doc ann
prettyCaseAlt (CaseAlt _ pat rhs) =
  case rhs of
    UnguardedRhs _ expr -> prettyPattern pat <+> "->" <+> prettyExprPrec 0 expr
    GuardedRhss _ grhss ->
      hsep
        [ prettyPattern pat,
          hsep
            [ "|"
                <+> hsep (punctuate comma (map prettyGuardQualifier (guardedRhsGuards grhs)))
                <+> "->"
                <+> prettyExprPrec 0 (guardedRhsBody grhs)
            | grhs <- grhss
            ]
        ]

prettyGuardQualifier :: GuardQualifier -> Doc ann
prettyGuardQualifier qualifier =
  case qualifier of
    GuardExpr _ expr -> prettyExprPrec 0 expr
    GuardPat _ pat expr -> prettyPattern pat <+> "<-" <+> prettyExprPrec 0 expr
    GuardLet _ decls -> "let" <+> braces (prettyInlineDecls decls)

-- | Pretty print a do statement.
-- Since do blocks are always rendered with explicit braces and semicolons,
-- statement boundaries are clear and greedy expressions don't need parens.
prettyDoStmt :: DoStmt Expr -> Doc ann
prettyDoStmt stmt =
  case stmt of
    DoBind _ pat expr -> prettyPattern pat <+> "<-" <+> prettyExprPrec 0 expr
    DoLet _ bindings -> "let" <+> braces (hsep (punctuate semi (map prettyBinding bindings)))
    DoLetDecls _ decls -> "let" <+> braces (prettyInlineDecls decls)
    DoExpr _ expr -> prettyExprPrec 0 expr
    DoRecStmt _ stmts -> "rec" <+> "{" <+> hsep (punctuate semi (map prettyDoStmt stmts)) <+> "}"

-- | Pretty-print an arrow command.
prettyCmd :: Cmd -> Doc ann
prettyCmd cmd =
  case cmd of
    CmdArrApp _ lhs HsFirstOrderApp rhs ->
      prettyExprPrec 1 lhs <+> "-<" <+> prettyExprPrec 0 rhs
    CmdArrApp _ lhs HsHigherOrderApp rhs ->
      prettyExprPrec 1 lhs <+> "-<<" <+> prettyExprPrec 0 rhs
    CmdInfix _ l op r ->
      prettyCmd l <+> prettyInfixOp op <+> prettyCmd r
    CmdDo _ stmts ->
      "do" <+> "{" <+> hsep (punctuate semi (map prettyCmdStmt stmts)) <+> "}"
    CmdIf _ cond yes no ->
      "if" <+> prettyExprPrec 0 cond <+> "then" <+> prettyCmd yes <+> "else" <+> prettyCmd no
    CmdCase _ scrut alts ->
      "case" <+> prettyExprPrec 0 scrut <+> "of" <+> "{" <+> hsep (punctuate semi (map prettyCmdCaseAlt alts)) <+> "}"
    CmdLet _ decls body ->
      "let" <+> braces (prettyInlineDecls decls) <+> "in" <+> prettyCmd body
    CmdLam _ pats body ->
      "\\" <+> hsep (map prettyPatternAtom pats) <+> "->" <+> prettyCmd body
    CmdApp _ c e ->
      prettyCmd c <+> prettyExprPrec 3 e
    CmdPar _ c ->
      parens (prettyCmd c)

-- | Pretty-print a do-statement in command context.
prettyCmdStmt :: DoStmt Cmd -> Doc ann
prettyCmdStmt stmt =
  case stmt of
    DoBind _ pat cmd' -> prettyPattern pat <+> "<-" <+> prettyCmd cmd'
    DoLet _ bindings -> "let" <+> braces (hsep (punctuate semi (map prettyBinding bindings)))
    DoLetDecls _ decls -> "let" <+> braces (prettyInlineDecls decls)
    DoExpr _ cmd' -> prettyCmd cmd'
    DoRecStmt _ stmts -> "rec" <+> "{" <+> hsep (punctuate semi (map prettyCmdStmt stmts)) <+> "}"

-- | Pretty-print a command case alternative.
prettyCmdCaseAlt :: CmdCaseAlt -> Doc ann
prettyCmdCaseAlt alt =
  prettyPattern (cmdCaseAltPat alt) <+> "->" <+> prettyCmd (cmdCaseAltBody alt)

prettyCompStmt :: CompStmt -> Doc ann
prettyCompStmt stmt =
  case stmt of
    CompGen _ pat expr -> prettyPattern pat <+> "<-" <+> prettyExprPrec 0 expr
    CompGuard _ expr -> prettyExprPrec 0 expr
    CompLet _ bindings -> "let" <+> hsep (punctuate semi (map prettyBinding bindings))
    CompLetDecls _ decls -> "let" <+> braces (prettyInlineDecls decls)

prettyInlineDecls :: [Decl] -> Doc ann
prettyInlineDecls decls =
  hsep (punctuate semi (map prettyInlineDecl decls))
  where
    -- For value declarations, use single-line form to keep guarded bindings together.
    -- For other declarations, join their lines with spaces.
    prettyInlineDecl decl = case decl of
      DeclValue _ valueDecl -> prettyValueDeclSingleLine valueDecl
      _ -> hsep (prettyDeclLines decl)

prettyArithSeq :: ArithSeq -> Doc ann
prettyArithSeq seqInfo =
  case seqInfo of
    ArithSeqFrom _ fromExpr -> brackets (prettyExprGuarded fromExpr <> " ..")
    ArithSeqFromThen _ fromExpr thenExpr -> brackets (prettyExprGuarded fromExpr <> ", " <> prettyExprGuarded thenExpr <> " ..")
    ArithSeqFromTo _ fromExpr toExpr -> brackets (prettyExprGuarded fromExpr <> " .. " <> prettyExprPrec 0 toExpr)
    ArithSeqFromThenTo _ fromExpr thenExpr toExpr ->
      brackets (prettyExprGuarded fromExpr <> ", " <> prettyExprGuarded thenExpr <> " .. " <> prettyExprPrec 0 toExpr)

parenthesize :: Bool -> Doc ann -> Doc ann
parenthesize shouldWrap doc
  | shouldWrap = parens doc
  | otherwise = doc

quoted :: Text -> Doc ann
quoted txt = pretty (show (T.unpack txt))

prettyQuasiQuote :: Text -> Text -> Doc ann
prettyQuasiQuote quoter body = "[" <> pretty quoter <> "|" <> pretty body <> "|]"

isOperatorToken :: Text -> Bool
isOperatorToken tok =
  not (T.null tok) && T.all isSymbolicOpChar tok

-- | Matches operator characters per Haskell 2010 §2.2: ASCII symbol chars
-- plus Unicode characters with general category Sm, Sc, Sk, or So.
isSymbolicOpChar :: Char -> Bool
isSymbolicOpChar c =
  c `elem` (":!#$%&*+./<=>?@\\^|-~" :: String) || isUnicodeSymbolCategory c

isUnicodeSymbolCategory :: Char -> Bool
isUnicodeSymbolCategory c = case generalCategory c of
  MathSymbol -> True
  CurrencySymbol -> True
  ModifierSymbol -> True
  OtherSymbol -> True
  _ -> False

-- | Pretty-print a TH splice with the given prefix ("$" or "$$").
-- If the body is a parenthesized expression, print as $(expr) or $$(expr).
-- If the body is a bare variable, print as $name or $$name.
prettySplice :: Doc ann -> Expr -> Doc ann
prettySplice prefix body =
  case body of
    EParen _ inner -> prefix <> parens (prettyExprPrec 0 inner)
    EVar {} -> prefix <> prettyExprPrec 11 body
    _ -> prefix <> parens (prettyExprPrec 0 body)

-- ---------------------------------------------------------------------------
-- TypeFamilies pretty-printing helpers

-- | @type family Name params [:: Kind] [where { equations }]@
prettyTypeFamilyDecl :: TypeFamilyDecl -> Doc ann
prettyTypeFamilyDecl tf =
  hsep $
    ["type", "family", pretty (typeFamilyDeclName tf)]
      <> map prettyTyVarBinder (typeFamilyDeclParams tf)
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
      <> [prettyType (typeFamilyEqLhs eq), "=", prettyType (typeFamilyEqRhs eq)]
  where
    forallPart [] = []
    forallPart binders = ["forall", hsep (map prettyTyVarBinder binders) <> "."]

-- | @data family Name params [:: Kind]@
prettyDataFamilyDecl :: DataFamilyDecl -> Doc ann
prettyDataFamilyDecl df =
  hsep $
    ["data", "family", pretty (dataFamilyDeclName df)]
      <> map prettyTyVarBinder (dataFamilyDeclParams df)
      <> kindPart (dataFamilyDeclKind df)
  where
    kindPart Nothing = []
    kindPart (Just k) = ["::", prettyType k]

-- | @type instance [forall.] LhsType = RhsType@ (top-level)
prettyTopTypeFamilyInst :: TypeFamilyInst -> Doc ann
prettyTopTypeFamilyInst tfi =
  hsep $
    ["type", "instance"]
      <> forallPart (typeFamilyInstForall tfi)
      <> [prettyType (typeFamilyInstLhs tfi), "=", prettyType (typeFamilyInstRhs tfi)]
  where
    forallPart [] = []
    forallPart binders = ["forall", hsep (map prettyTyVarBinder binders) <> "."]

-- | @(data|newtype) instance [forall.] HeadType = Cons@ (top-level)
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

-- | @type Name params [:: Kind]@ (associated type family inside a class, no @family@ keyword)
prettyAssocTypeFamilyDecl :: TypeFamilyDecl -> Doc ann
prettyAssocTypeFamilyDecl tf =
  hsep $
    ["type", pretty (typeFamilyDeclName tf)]
      <> map prettyTyVarBinder (typeFamilyDeclParams tf)
      <> kindPart (typeFamilyDeclKind tf)
  where
    kindPart Nothing = []
    kindPart (Just k) = ["::", prettyType k]

-- | @data Name params [:: Kind]@ (associated data family inside a class, no @family@ keyword)
prettyAssocDataFamilyDecl :: DataFamilyDecl -> Doc ann
prettyAssocDataFamilyDecl df =
  hsep $
    ["data", pretty (dataFamilyDeclName df)]
      <> map prettyTyVarBinder (dataFamilyDeclParams df)
      <> kindPart (dataFamilyDeclKind df)
  where
    kindPart Nothing = []
    kindPart (Just k) = ["::", prettyType k]

-- | @type instance LhsType = RhsType@ (default type instance inside a class)
prettyDefaultTypeInst :: TypeFamilyInst -> Doc ann
prettyDefaultTypeInst tfi =
  hsep $
    ["type", "instance"]
      <> forallPart (typeFamilyInstForall tfi)
      <> [prettyType (typeFamilyInstLhs tfi), "=", prettyType (typeFamilyInstRhs tfi)]
  where
    forallPart [] = []
    forallPart binders = ["forall", hsep (map prettyTyVarBinder binders) <> "."]

-- | @type LhsType = RhsType@ inside an instance body (no @instance@ keyword)
prettyInstTypeFamilyInst :: TypeFamilyInst -> Doc ann
prettyInstTypeFamilyInst tfi =
  hsep $
    ["type"]
      <> forallPart (typeFamilyInstForall tfi)
      <> [prettyType (typeFamilyInstLhs tfi), "=", prettyType (typeFamilyInstRhs tfi)]
  where
    forallPart [] = []
    forallPart binders = ["forall", hsep (map prettyTyVarBinder binders) <> "."]

-- | @(data|newtype) HeadType = Cons@ inside an instance body
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
