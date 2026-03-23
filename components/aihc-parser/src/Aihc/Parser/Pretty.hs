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

import Aihc.Parser.Ast
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
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
    DeclStandaloneKindSig _ name kind -> [hsep ["type", prettyConstructorName name, "::", prettyType kind]]
    DeclFixity _ assoc prec ops ->
      [ hsep
          ( [prettyFixityAssoc assoc]
              <> maybe [] (pure . pretty . show) prec
              <> map prettyOperatorName ops
          )
      ]
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
    DeclNewtype _ newtypeDecl -> [prettyNewtypeDecl newtypeDecl]
    DeclClass _ classDecl -> [prettyClassDecl classDecl]
    DeclInstance _ instanceDecl -> [prettyInstanceDecl instanceDecl]
    DeclStandaloneDeriving _ derivingDecl -> [prettyStandaloneDeriving derivingDecl]
    DeclDefault _ tys -> ["default" <+> parens (hsep (punctuate comma (map prettyType tys)))]
    DeclForeign _ foreignDecl -> [prettyForeignDecl foreignDecl]

prettyValueDeclLines :: ValueDecl -> [Doc ann]
prettyValueDeclLines valueDecl =
  case valueDecl of
    PatternBind _ pat rhs -> [prettyPattern pat <+> prettyRhs rhs]
    FunctionBind _ name matches ->
      concatMap (prettyFunctionMatchLines name) matches

prettyFunctionMatchLines :: Text -> Match -> [Doc ann]
prettyFunctionMatchLines name match =
  case matchRhs match of
    UnguardedRhs _ _ -> [prettyFunctionMatch name match]
    GuardedRhss _ grhss ->
      prettyFunctionHead name (matchPats match)
        : [ "  |"
              <+> hsep (punctuate comma (map prettyGuardQualifier (guardedRhsGuards grhs)))
              <+> "="
              <+> prettyExprPrec 0 (guardedRhsBody grhs)
          | grhs <- grhss
          ]

prettyFunctionMatch :: Text -> Match -> Doc ann
prettyFunctionMatch name match =
  prettyFunctionHead name (matchPats match) <+> prettyRhs (matchRhs match)

prettyFunctionHead :: Text -> [Pattern] -> Doc ann
prettyFunctionHead name pats =
  case pats of
    [lhs, rhsPat]
      | isOperatorToken name ->
          prettyPattern lhs <+> pretty name <+> prettyPattern rhsPat
    _ ->
      hsep (prettyFunctionBinder name : map prettyPattern pats)

prettyRhs :: Rhs -> Doc ann
prettyRhs rhs =
  case rhs of
    UnguardedRhs _ expr -> "=" <+> prettyExprPrec 0 expr
    GuardedRhss _ guards ->
      hsep
        [ "|"
            <+> hsep (punctuate comma (map prettyGuardQualifier (guardedRhsGuards grhs)))
            <+> "="
            <+> prettyExprPrec 0 (guardedRhsBody grhs)
        | grhs <- guards
        ]

prettyType :: Type -> Doc ann
prettyType ty =
  case ty of
    TVar _ name -> pretty name
    TCon _ name
      | isSymbolicTypeOperator name -> parens (pretty name)
      | otherwise -> pretty name
    TStar _ -> "*"
    TQuasiQuote _ quoter body -> prettyQuasiQuote quoter body
    TForall _ binders inner ->
      "forall" <+> hsep (map pretty binders) <> "." <+> prettyType inner
    TApp _ (TApp _ (TCon _ op) lhs) rhs
      | isSymbolicTypeOperator op && op /= "->" ->
          parens (prettyType lhs <+> pretty op <+> prettyType rhs)
    TApp _ f x -> parenthesizeTypeApp f <+> parenthesizeTypeArg x
    TFun _ a b -> parenthesizeTypeFunLeft a <+> "->" <+> prettyType b
    TTuple _ elems -> parens (hsep (punctuate comma (map prettyType elems)))
    TList _ inner -> brackets (prettyType inner)
    TParen _ inner
      | isInfixTypeApp inner -> prettyType inner
      | otherwise -> parens (prettyType inner)
    TContext _ constraints inner ->
      prettyContext constraints <+> "=>" <+> prettyType inner

parenthesizeTypeFunLeft :: Type -> Doc ann
parenthesizeTypeFunLeft ty =
  case ty of
    TForall {} -> parens (prettyType ty)
    TFun {} -> parens (prettyType ty)
    TContext {} -> parens (prettyType ty)
    _ -> prettyType ty

parenthesizeTypeApp :: Type -> Doc ann
parenthesizeTypeApp ty =
  case ty of
    TQuasiQuote {} -> prettyType ty
    TForall {} -> parens (prettyType ty)
    TFun {} -> parens (prettyType ty)
    TContext {} -> parens (prettyType ty)
    _ -> prettyType ty

parenthesizeTypeArg :: Type -> Doc ann
parenthesizeTypeArg ty =
  case ty of
    TApp _ (TApp _ (TCon _ op) _) _
      | isSymbolicTypeOperator op && op /= "->" -> prettyType ty
    TQuasiQuote {} -> prettyType ty
    TApp {} -> parens (prettyType ty)
    TForall {} -> parens (prettyType ty)
    TFun {} -> parens (prettyType ty)
    TContext {} -> parens (prettyType ty)
    _ -> prettyType ty

prettyContext :: [Constraint] -> Doc ann
prettyContext constraints =
  case constraints of
    [single] -> prettyConstraint single
    _ -> parens (hsep (punctuate comma (map prettyConstraint constraints)))

prettyConstraint :: Constraint -> Doc ann
prettyConstraint constraint =
  let base =
        if constraintClass constraint == "()" && null (constraintArgs constraint)
          then "()"
          else hsep (pretty (constraintClass constraint) : map prettyTypeAtom (constraintArgs constraint))
   in if constraintParen constraint
        then parens base
        else base

prettyTypeAtom :: Type -> Doc ann
prettyTypeAtom ty =
  case ty of
    TVar _ _ -> prettyType ty
    TCon _ _ -> prettyType ty
    TStar _ -> prettyType ty
    TQuasiQuote {} -> prettyType ty
    TList _ _ -> prettyType ty
    TTuple _ _ -> prettyType ty
    TParen _ _ -> prettyType ty
    _ -> parens (prettyType ty)

isSymbolicTypeOperator :: Text -> Bool
isSymbolicTypeOperator op =
  case T.uncons op of
    Nothing -> False
    Just _ -> T.all (`elem` (":!#$%&*+./<=>?\\^|-~" :: String)) op

isInfixTypeApp :: Type -> Bool
isInfixTypeApp ty =
  case ty of
    TApp _ (TApp _ (TCon _ op) _) _ -> isSymbolicTypeOperator op && op /= "->"
    _ -> False

prettyPattern :: Pattern -> Doc ann
prettyPattern pat =
  case pat of
    PVar _ name -> pretty name
    PWildcard _ -> "_"
    PLit _ lit -> prettyLiteral lit
    PQuasiQuote _ quoter body -> prettyQuasiQuote quoter body
    PTuple _ elems -> parens (hsep (punctuate comma (map prettyPattern elems)))
    PList _ elems -> brackets (hsep (punctuate comma (map prettyPattern elems)))
    PCon _ con args -> hsep (pretty con : map prettyPatternAtom args)
    PInfix _ lhs op rhs -> prettyPatternAtom lhs <+> pretty op <+> prettyPatternAtom rhs
    PView _ viewExpr inner -> parens (prettyExprPrec 0 viewExpr <+> "->" <+> prettyPattern inner)
    PAs _ name inner -> pretty name <+> "@" <+> prettyPatternAtom inner
    PStrict _ inner -> "!" <> prettyUnaryPattern inner
    PIrrefutable _ inner -> "~" <> prettyUnaryPattern inner
    PNegLit _ lit -> "-" <> prettyLiteral lit
    PParen _ inner -> parens (prettyPattern inner)
    PRecord _ con fields ->
      pretty con
        <+> braces
          ( hsep
              ( punctuate
                  comma
                  [prettyPatternFieldBinding fieldName fieldPat | (fieldName, fieldPat) <- fields]
              )
          )

-- | Pretty print a pattern field binding.
-- Supports NamedFieldPuns: if pattern is a variable with the same name as the field,
-- print just the field name (punned form).
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
    PTuple _ _ -> prettyPattern pat
    PParen _ _ -> prettyPattern pat
    PStrict _ _ -> prettyPattern pat
    PView {} -> prettyPattern pat
    _ -> parens (prettyPattern pat)

prettyUnaryPattern :: Pattern -> Doc ann
prettyUnaryPattern pat =
  case pat of
    PNegLit {} -> parens (prettyPattern pat)
    PStrict {} -> parens (prettyPattern pat)
    PIrrefutable {} -> parens (prettyPattern pat)
    _ -> prettyPatternAtom pat

prettyLiteral :: Literal -> Doc ann
prettyLiteral lit =
  case lit of
    LitInt _ _ repr -> pretty repr
    LitIntBase _ _ repr -> pretty repr
    LitFloat _ _ repr -> pretty repr
    LitChar _ _ repr -> pretty repr
    LitString _ _ repr -> pretty repr

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
        ctors -> ["=", hsep (punctuate " |" (map prettyDataCon ctors))]

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
        Just ctor -> ["=", prettyDataCon ctor]

derivingParts :: [DerivingClause] -> [Doc ann]
derivingParts = concatMap derivingPart

derivingPart :: DerivingClause -> [Doc ann]
derivingPart (DerivingClause strategy classes) =
  ["deriving"] <> strategyPart strategy <> classesPart classes
  where
    strategyPart Nothing = []
    strategyPart (Just DerivingStock) = ["stock"]
    strategyPart (Just DerivingNewtype) = ["newtype"]
    strategyPart (Just DerivingAnyclass) = ["anyclass"]

    classesPart [] = ["()"]
    classesPart [single]
      | Just DerivingStock <- strategy = [parens (pretty single)]
      | otherwise = [pretty single]
    classesPart _ = [parens (hsep (punctuate comma (map pretty classes)))]

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

prettyDataCon :: DataConDecl -> Doc ann
prettyDataCon ctor =
  case ctor of
    PrefixCon _ forallVars constraints name fields ->
      hsep (dataConQualifierPrefix forallVars constraints <> [prettyConstructorName name] <> map prettyBangType fields)
    InfixCon _ forallVars constraints lhs op rhs ->
      hsep
        ( dataConQualifierPrefix forallVars constraints
            <> [prettyBangTypeAtom lhs, pretty op, prettyBangTypeAtom rhs]
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

dataConQualifierPrefix :: [Text] -> [Constraint] -> [Doc ann]
dataConQualifierPrefix forallVars constraints = forallPrefix forallVars <> contextPrefix constraints
  where
    forallPrefix [] = []
    forallPrefix binders = ["forall", hsep (map pretty binders) <> "."]

prettyBangType :: BangType -> Doc ann
prettyBangType bt
  | bangStrict bt = "!" <> prettyTypeAtom (bangType bt)
  | otherwise = prettyTypeAtom (bangType bt)

prettyRecordFieldBangType :: BangType -> Doc ann
prettyRecordFieldBangType bt
  | bangStrict bt = "!" <> prettyType (bangType bt)
  | otherwise = prettyType (bangType bt)

prettyBangTypeAtom :: BangType -> Doc ann
prettyBangTypeAtom bt =
  case bangType bt of
    TForall {} -> parens (prettyBangType bt)
    TFun {} -> parens (prettyBangType bt)
    TContext {} -> parens (prettyBangType bt)
    _ -> prettyBangType bt

prettyClassDecl :: ClassDecl -> Doc ann
prettyClassDecl decl =
  let headDoc =
        hsep
          ( ["class"]
              <> contextPrefix (classDeclContext decl)
              <> [pretty (classDeclName decl)]
              <> map prettyTyVarBinder (classDeclParams decl)
          )
   in case classDeclItems decl of
        [] -> headDoc
        items -> headDoc <+> "where" <+> braces (hsep (punctuate semi (map prettyClassItem items)))

prettyClassItem :: ClassDeclItem -> Doc ann
prettyClassItem item =
  case item of
    ClassItemTypeSig _ names ty -> hsep [hsep (punctuate comma (map prettyBinderName names)), "::", prettyType ty]
    ClassItemFixity _ assoc prec ops ->
      hsep
        ( [prettyFixityAssoc assoc]
            <> maybe [] (pure . pretty . show) prec
            <> map prettyOperatorName ops
        )
    ClassItemDefault _ valueDecl ->
      case prettyValueDeclLines valueDecl of
        [] -> ""
        (line : _) -> line

prettyInstanceDecl :: InstanceDecl -> Doc ann
prettyInstanceDecl decl =
  let headDoc =
        hsep
          ( ["instance"]
              <> contextPrefix (instanceDeclContext decl)
              <> [pretty (instanceDeclClassName decl)]
              <> map prettyTypeAtom (instanceDeclTypes decl)
          )
   in case instanceDeclItems decl of
        [] -> headDoc
        items -> headDoc <+> "where" <+> braces (hsep (punctuate semi (map prettyInstanceItem items)))

prettyStandaloneDeriving :: StandaloneDerivingDecl -> Doc ann
prettyStandaloneDeriving decl =
  hsep
    ( ["deriving"]
        <> maybe [] (\s -> [prettyDerivingStrategy s]) (standaloneDerivingStrategy decl)
        <> ["instance"]
        <> contextPrefix (standaloneDerivingContext decl)
        <> [pretty (standaloneDerivingClassName decl)]
        <> map prettyTypeAtom (standaloneDerivingTypes decl)
    )

prettyDerivingStrategy :: DerivingStrategy -> Doc ann
prettyDerivingStrategy strategy =
  case strategy of
    DerivingStock -> "stock"
    DerivingNewtype -> "newtype"
    DerivingAnyclass -> "anyclass"

prettyInstanceItem :: InstanceDeclItem -> Doc ann
prettyInstanceItem item =
  case item of
    InstanceItemBind _ valueDecl ->
      case prettyValueDeclLines valueDecl of
        [] -> ""
        (line : _) -> line
    InstanceItemTypeSig _ names ty -> hsep [hsep (punctuate comma (map prettyBinderName names)), "::", prettyType ty]
    InstanceItemFixity _ assoc prec ops ->
      hsep
        ( [prettyFixityAssoc assoc]
            <> maybe [] (pure . pretty . show) prec
            <> map prettyOperatorName ops
        )

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

prettyOperatorName :: Text -> Doc ann
prettyOperatorName name
  | isOperatorToken name = pretty name
  | otherwise = parens (pretty name)

prettyFunctionBinder :: Text -> Doc ann
prettyFunctionBinder name
  | isOperatorToken name = parens (pretty name)
  | otherwise = pretty name

prettyBinderName :: Text -> Doc ann
prettyBinderName = prettyFunctionBinder

prettyExprOperator :: Text -> Doc ann
prettyExprOperator op
  | isOperatorToken op = pretty op
  | otherwise = "`" <> pretty op <> "`"

prettyConstructorName :: Text -> Doc ann
prettyConstructorName name
  | isOperatorToken name = parens (pretty name)
  | otherwise = pretty name

-- | Print an expression used as the RHS of an infix operator.
-- Self-delimiting expressions (do, if, case, let, lambda) don't need parentheses.
prettyExprInfixRhs :: Expr -> Doc ann
prettyExprInfixRhs expr =
  case expr of
    EDo {} -> prettyExprPrec 0 expr
    EIf {} -> prettyExprPrec 0 expr
    ECase {} -> prettyExprPrec 0 expr
    ELetDecls {} -> prettyExprPrec 0 expr
    ELambdaPats {} -> prettyExprPrec 0 expr
    ELambdaCase {} -> prettyExprPrec 0 expr
    _ -> prettyExprPrec 1 expr

prettyExprPrec :: Int -> Expr -> Doc ann
prettyExprPrec prec expr =
  case expr of
    EApp _ fn arg ->
      parenthesize (prec > 2) (prettyExprPrec 2 fn <+> prettyExprPrec 3 arg)
    ETypeApp _ fn ty ->
      parenthesize (prec > 2) (prettyExprPrec 2 fn <+> "@" <> prettyTypeAtom ty)
    EVar _ name
      | isOperatorToken name -> parens (pretty name)
      | otherwise -> pretty name
    EInt _ _ repr -> pretty repr
    EIntBase _ _ repr -> pretty repr
    EFloat _ _ repr -> pretty repr
    EChar _ _ repr -> pretty repr
    EString _ _ repr -> pretty repr
    EQuasiQuote _ quoter body -> prettyQuasiQuote quoter body
    EIf _ cond yes no ->
      parenthesize
        (prec > 0)
        ("if" <+> prettyExprPrec 0 cond <+> "then" <+> prettyExprPrec 0 yes <+> "else" <+> prettyExprPrec 0 no)
    ELambdaPats _ pats body ->
      parenthesize (prec > 0) ("\\" <+> hsep (map prettyPattern pats) <+> "->" <+> prettyExprPrec 0 body)
    ELambdaCase _ alts ->
      parenthesize
        (prec > 0)
        ("\\" <> "case" <+> braces (hsep (punctuate semi (map prettyCaseAlt alts))))
    EInfix _ lhs op rhs -> parenthesize (prec > 1) (prettyExprPrec 1 lhs <+> prettyExprOperator op <+> prettyExprInfixRhs rhs)
    ENegate _ inner -> parenthesize (prec > 2) ("-" <> prettyExprPrec 3 inner)
    ESectionL _ lhs op -> parens (prettyExprPrec 0 lhs <+> prettyExprOperator op)
    ESectionR _ op rhs -> parens (prettyExprOperator op <+> prettyExprPrec 0 rhs)
    ELetDecls _ decls body ->
      parenthesize
        (prec > 0)
        ( "let"
            <+> braces (prettyInlineDecls decls)
            <+> "in"
            <+> prettyExprPrec 0 body
        )
    ECase _ scrutinee alts ->
      parenthesize
        (prec > 0)
        ( "case"
            <+> prettyExprPrec 0 scrutinee
            <+> "of"
            <+> case alts of
              [] -> braces mempty
              _ -> hsep (punctuate semi (map prettyCaseAlt alts))
        )
    EDo _ stmts ->
      parenthesize
        (prec > 0)
        ("do" <+> braces (hsep (punctuate semi (map prettyDoStmt stmts))))
    EListComp _ body quals ->
      brackets
        ( prettyExprPrec 0 body
            <+> "|"
            <+> hsep (punctuate comma (map prettyCompStmt quals))
        )
    EListCompParallel _ body qualifierGroups ->
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
    ERecordCon _ name fields ->
      pretty name <+> braces (hsep (punctuate comma (map prettyBinding fields)))
    ERecordUpd _ base fields ->
      prettyExprPrec 3 base <+> braces (hsep (punctuate comma (map prettyBinding fields)))
    ETypeSig _ inner ty -> parenthesize (prec > 1) (prettyExprPrec 1 inner <+> "::" <+> prettyType ty)
    EParen _ inner ->
      case inner of
        ESectionL {} -> prettyExprPrec 0 inner
        ESectionR {} -> prettyExprPrec 0 inner
        _ -> parens (prettyExprPrec 0 inner)
    EWhereDecls _ body decls ->
      parenthesize
        (prec > 0)
        (prettyExprPrec 0 body <+> "where" <+> braces (prettyInlineDecls decls))
    EList _ values -> brackets (hsep (punctuate comma (map (prettyExprPrec 0) values)))
    ETuple _ values -> parens (hsep (punctuate comma (map (prettyExprPrec 0) values)))
    ETupleSection _ values ->
      parens
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
    ETupleCon _ arity -> parens (pretty (T.replicate (max 1 (arity - 1)) ","))

-- | Pretty print a record field binding.
-- Supports NamedFieldPuns: if value is a variable with the same name as the field,
-- print just the field name (punned form).
prettyBinding :: (Text, Expr) -> Doc ann
prettyBinding (name, value) =
  case value of
    EVar _ varName | varName == name -> pretty name -- NamedFieldPuns: punned form
    _ -> pretty name <+> "=" <+> prettyExprPrec 0 value

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

prettyDoStmt :: DoStmt -> Doc ann
prettyDoStmt stmt =
  case stmt of
    DoBind _ pat expr -> prettyPattern pat <+> "<-" <+> prettyExprPrec 0 expr
    DoLet _ bindings -> "let" <+> braces (hsep (punctuate semi (map prettyBinding bindings)))
    DoLetDecls _ decls -> "let" <+> braces (prettyInlineDecls decls)
    DoExpr _ expr -> prettyExprPrec 0 expr

prettyCompStmt :: CompStmt -> Doc ann
prettyCompStmt stmt =
  case stmt of
    CompGen _ pat expr -> prettyPattern pat <+> "<-" <+> prettyExprPrec 0 expr
    CompGuard _ expr -> prettyExprPrec 0 expr
    CompLet _ bindings -> "let" <+> hsep (punctuate semi (map prettyBinding bindings))
    CompLetDecls _ decls -> "let" <+> braces (prettyInlineDecls decls)

prettyInlineDecls :: [Decl] -> Doc ann
prettyInlineDecls decls =
  hsep (punctuate semi (concatMap prettyDeclLines decls))

prettyArithSeq :: ArithSeq -> Doc ann
prettyArithSeq seqInfo =
  case seqInfo of
    ArithSeqFrom _ fromExpr -> brackets (prettyExprPrec 0 fromExpr <> " ..")
    ArithSeqFromThen _ fromExpr thenExpr -> brackets (prettyExprPrec 0 fromExpr <> ", " <> prettyExprPrec 0 thenExpr <> " ..")
    ArithSeqFromTo _ fromExpr toExpr -> brackets (prettyExprPrec 0 fromExpr <> " .. " <> prettyExprPrec 0 toExpr)
    ArithSeqFromThenTo _ fromExpr thenExpr toExpr ->
      brackets (prettyExprPrec 0 fromExpr <> ", " <> prettyExprPrec 0 thenExpr <> " .. " <> prettyExprPrec 0 toExpr)

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
  not (T.null tok) && T.all (`elem` (":!#$%&*+./<=>?\\^|-~" :: String)) tok
