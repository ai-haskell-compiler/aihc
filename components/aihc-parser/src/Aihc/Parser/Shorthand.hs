{-# LANGUAGE OverloadedStrings #-}

-- |
--
-- Module      : Aihc.Parser.Shorthand
-- Description : Compact pretty-printing for debugging/inspection
--
-- This module provides a compact, human-readable representation of parsed
-- AST structures via the 'Shorthand' typeclass. Key features:
--
-- * Source spans are omitted to reduce noise
-- * Empty fields (Nothing, [], False, etc.) are omitted
-- * Output is on a single line by default
-- * Uses the prettyprinter library for consistent formatting
--
-- Example:
--
-- >>> shorthand $ parseModule defaultConfig "module Demo where x = 1"
-- ParseOk (Module {name = "Demo", decls = [DeclValue (FunctionBind "x" [Match {headForm = Prefix, rhs = UnguardedRhs (EInt 1)}])]})
module Aihc.Parser.Shorthand
  ( Shorthand (..),
  )
where

import Aihc.Parser.Lex (LexToken (..), LexTokenKind (..))
import Aihc.Parser.Syntax
import Aihc.Parser.Types (ParseResult (..))
import Data.Text (Text)
import Prettyprinter
  ( Doc,
    Pretty (..),
    braces,
    brackets,
    comma,
    dquotes,
    hsep,
    parens,
    punctuate,
    (<+>),
  )

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Aihc.Parser

-- | Typeclass for compact, human-readable AST representations.
--
-- The 'shorthand' method produces a 'Doc' that can be rendered to text
-- or shown as a string. This is useful for debugging and golden tests.
--
-- Use 'show' on the result of 'shorthand' to get a 'String':
--
-- @
-- show (shorthand expr) :: String
-- @
class Shorthand a where
  shorthand :: a -> Doc ()

-- ParseResult

instance (Shorthand a) => Shorthand (ParseResult a) where
  shorthand (ParseOk a) = "ParseOk" <+> parens (shorthand a)
  shorthand (ParseErr _) = "ParseErr"

-- Module

instance Shorthand Module where
  shorthand modu =
    "Module" <+> braces (hsep (punctuate comma fields))
    where
      fields =
        optionalField "name" docText (moduleName modu)
          <> listField "languagePragmas" docExtensionSetting (moduleLanguagePragmas modu)
          <> optionalField "warningText" docWarningText (moduleWarningText modu)
          <> optionalField "exports" (brackets . hsep . punctuate comma . map docExportSpec) (moduleExports modu)
          <> listField "imports" docImportDecl (moduleImports modu)
          <> listField "decls" docDecl (moduleDecls modu)

instance Shorthand Expr where
  shorthand = docExpr

instance Shorthand Pattern where
  shorthand = docPattern

instance Shorthand Type where
  shorthand = docType

instance Shorthand LexToken where
  shorthand = docToken

instance Shorthand LexTokenKind where
  shorthand = docTokenKind

docWarningText :: WarningText -> Doc ann
docWarningText wt =
  case wt of
    DeprText _ msg -> "DeprText" <+> docText msg
    WarnText _ msg -> "WarnText" <+> docText msg

docExtensionSetting :: ExtensionSetting -> Doc ann
docExtensionSetting setting =
  case setting of
    EnableExtension ext -> "EnableExtension" <+> pretty (extensionName ext)
    DisableExtension ext -> "DisableExtension" <+> pretty (extensionName ext)

docExportSpec :: ExportSpec -> Doc ann
docExportSpec spec =
  case spec of
    ExportModule _ name -> "ExportModule" <+> docText name
    ExportVar _ mNamespace name ->
      "ExportVar" <> braces (hsep (punctuate comma (optionalField "namespace" docText mNamespace <> [field "name" (docText name)])))
    ExportAbs _ mNamespace name ->
      "ExportAbs" <> braces (hsep (punctuate comma (optionalField "namespace" docText mNamespace <> [field "name" (docText name)])))
    ExportAll _ mNamespace name ->
      "ExportAll" <> braces (hsep (punctuate comma (optionalField "namespace" docText mNamespace <> [field "name" (docText name)])))
    ExportWith _ mNamespace name members ->
      "ExportWith" <> braces (hsep (punctuate comma (optionalField "namespace" docText mNamespace <> [field "name" (docText name), field "members" (docTextList members)])))

docImportDecl :: ImportDecl -> Doc ann
docImportDecl decl =
  "ImportDecl" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      [field "module" (docText (importDeclModule decl))]
        <> boolField "qualified" (importDeclQualified decl)
        <> boolField "qualifiedPost" (importDeclQualifiedPost decl)
        <> optionalField "level" docImportLevel (importDeclLevel decl)
        <> optionalField "package" docText (importDeclPackage decl)
        <> optionalField "as" docText (importDeclAs decl)
        <> optionalField "spec" docImportSpec (importDeclSpec decl)

docImportLevel :: ImportLevel -> Doc ann
docImportLevel level =
  case level of
    ImportLevelQuote -> "ImportLevelQuote"
    ImportLevelSplice -> "ImportLevelSplice"

docImportSpec :: ImportSpec -> Doc ann
docImportSpec spec =
  "ImportSpec" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      boolField "hiding" (importSpecHiding spec)
        <> [field "items" (brackets (hsep (punctuate comma (map docImportItem (importSpecItems spec)))))]

docImportItem :: ImportItem -> Doc ann
docImportItem item =
  case item of
    ImportItemVar _ mNamespace name ->
      "ImportItemVar" <> braces (hsep (punctuate comma (optionalField "namespace" docText mNamespace <> [field "name" (docText name)])))
    ImportItemAbs _ mNamespace name ->
      "ImportItemAbs" <> braces (hsep (punctuate comma (optionalField "namespace" docText mNamespace <> [field "name" (docText name)])))
    ImportItemAll _ mNamespace name ->
      "ImportItemAll" <> braces (hsep (punctuate comma (optionalField "namespace" docText mNamespace <> [field "name" (docText name)])))
    ImportItemWith _ mNamespace name members ->
      "ImportItemWith" <> braces (hsep (punctuate comma (optionalField "namespace" docText mNamespace <> [field "name" (docText name), field "members" (docTextList members)])))

-- Declarations

docDecl :: Decl -> Doc ann
docDecl decl =
  case decl of
    DeclValue _ vdecl -> "DeclValue" <+> parens (docValueDecl vdecl)
    DeclTypeSig _ names ty -> "DeclTypeSig" <+> braces (hsep (punctuate comma [field "names" (docTextList names), field "type" (docType ty)]))
    DeclStandaloneKindSig _ name kind -> "DeclStandaloneKindSig" <+> braces (hsep (punctuate comma [field "name" (docText name), field "kind" (docType kind)]))
    DeclFixity _ assoc mPrec ops -> "DeclFixity" <+> braces (hsep (punctuate comma ([field "assoc" (docFixityAssoc assoc)] <> optionalField "prec" pretty mPrec <> [field "ops" (docTextList ops)])))
    DeclTypeSyn _ syn -> "DeclTypeSyn" <+> parens (docTypeSynDecl syn)
    DeclData _ dd -> "DeclData" <+> parens (docDataDecl dd)
    DeclNewtype _ nd -> "DeclNewtype" <+> parens (docNewtypeDecl nd)
    DeclClass _ cd -> "DeclClass" <+> parens (docClassDecl cd)
    DeclInstance _ inst -> "DeclInstance" <+> parens (docInstanceDecl inst)
    DeclStandaloneDeriving _ sd -> "DeclStandaloneDeriving" <+> parens (docStandaloneDerivingDecl sd)
    DeclDefault _ tys -> "DeclDefault" <+> brackets (hsep (punctuate comma (map docType tys)))
    DeclForeign _ fd -> "DeclForeign" <+> parens (docForeignDecl fd)

docValueDecl :: ValueDecl -> Doc ann
docValueDecl vdecl =
  case vdecl of
    FunctionBind _ name matches -> "FunctionBind" <+> docText name <+> brackets (hsep (punctuate comma (map docMatch matches)))
    PatternBind _ pat rhs -> "PatternBind" <+> docPattern pat <+> docRhs rhs

docMatch :: Match -> Doc ann
docMatch m =
  "Match" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      [ field "headForm" (docMatchHeadForm (matchHeadForm m))
      ]
        <> listField "pats" docPattern (matchPats m)
        <> [field "rhs" (docRhs (matchRhs m))]

docMatchHeadForm :: MatchHeadForm -> Doc ann
docMatchHeadForm headForm =
  case headForm of
    MatchHeadPrefix -> "Prefix"
    MatchHeadInfix -> "Infix"

docRhs :: Rhs -> Doc ann
docRhs rhs =
  case rhs of
    UnguardedRhs _ expr -> "UnguardedRhs" <+> parens (docExpr expr)
    GuardedRhss _ grhss -> "GuardedRhss" <+> brackets (hsep (punctuate comma (map docGuardedRhs grhss)))

docGuardedRhs :: GuardedRhs -> Doc ann
docGuardedRhs grhs =
  "GuardedRhs" <+> braces (hsep (punctuate comma [field "guards" (brackets (hsep (punctuate comma (map docGuardQualifier (guardedRhsGuards grhs))))), field "body" (docExpr (guardedRhsBody grhs))]))

docGuardQualifier :: GuardQualifier -> Doc ann
docGuardQualifier gq =
  case gq of
    GuardExpr _ expr -> "GuardExpr" <+> parens (docExpr expr)
    GuardPat _ pat expr -> "GuardPat" <+> parens (docPattern pat) <+> parens (docExpr expr)
    GuardLet _ decls -> "GuardLet" <+> brackets (hsep (punctuate comma (map docDecl decls)))

docTypeSynDecl :: TypeSynDecl -> Doc ann
docTypeSynDecl syn =
  "TypeSynDecl" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      [field "name" (docText (typeSynName syn))]
        <> listField "params" docTyVarBinder (typeSynParams syn)
        <> [field "body" (docType (typeSynBody syn))]

docDataDecl :: DataDecl -> Doc ann
docDataDecl dd =
  "DataDecl" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      [field "name" (docText (dataDeclName dd))]
        <> listField "context" docConstraint (dataDeclContext dd)
        <> listField "params" docTyVarBinder (dataDeclParams dd)
        <> listField "constructors" docDataConDecl (dataDeclConstructors dd)
        <> listField "deriving" docDerivingClause (dataDeclDeriving dd)

docNewtypeDecl :: NewtypeDecl -> Doc ann
docNewtypeDecl nd =
  "NewtypeDecl" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      [field "name" (docText (newtypeDeclName nd))]
        <> listField "context" docConstraint (newtypeDeclContext nd)
        <> listField "params" docTyVarBinder (newtypeDeclParams nd)
        <> optionalField "constructor" docDataConDecl (newtypeDeclConstructor nd)
        <> listField "deriving" docDerivingClause (newtypeDeclDeriving nd)

docDataConDecl :: DataConDecl -> Doc ann
docDataConDecl dcd =
  case dcd of
    PrefixCon _ forallVars constraints name fields' ->
      "PrefixCon" <+> braces (hsep (punctuate comma ([field "name" (docText name)] <> listField "forallVars" docText forallVars <> listField "constraints" docConstraint constraints <> listField "fields" docBangType fields')))
    InfixCon _ forallVars constraints lhs op rhs ->
      "InfixCon" <+> braces (hsep (punctuate comma ([field "op" (docText op), field "lhs" (docBangType lhs), field "rhs" (docBangType rhs)] <> listField "forallVars" docText forallVars <> listField "constraints" docConstraint constraints)))
    RecordCon _ forallVars constraints name fields' ->
      "RecordCon" <+> braces (hsep (punctuate comma ([field "name" (docText name)] <> listField "forallVars" docText forallVars <> listField "constraints" docConstraint constraints <> listField "fields" docFieldDecl fields')))
    GadtCon _ forallBinders constraints names body ->
      "GadtCon" <+> braces (hsep (punctuate comma (listField "names" docText names <> listField "forallBinders" docTyVarBinder forallBinders <> listField "constraints" docConstraint constraints <> [field "body" (docGadtBody body)])))

-- | Document a GADT body
docGadtBody :: GadtBody -> Doc ann
docGadtBody body =
  case body of
    GadtPrefixBody args resultTy ->
      "GadtPrefixBody" <+> braces (hsep (punctuate comma (listField "args" docBangType args <> [field "result" (docType resultTy)])))
    GadtRecordBody fields' resultTy ->
      "GadtRecordBody" <+> braces (hsep (punctuate comma (listField "fields" docFieldDecl fields' <> [field "result" (docType resultTy)])))

docBangType :: BangType -> Doc ann
docBangType bt =
  "BangType" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      boolField "strict" (bangStrict bt)
        <> [field "type" (docType (bangType bt))]

docFieldDecl :: FieldDecl -> Doc ann
docFieldDecl fd =
  "FieldDecl" <+> braces (hsep (punctuate comma [field "names" (docTextList (fieldNames fd)), field "type" (docBangType (fieldType fd))]))

docDerivingClause :: DerivingClause -> Doc ann
docDerivingClause dc =
  "DerivingClause" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      optionalField "strategy" docDerivingStrategy (derivingStrategy dc)
        <> listField "classes" docConstraint (derivingClasses dc)

docDerivingStrategy :: DerivingStrategy -> Doc ann
docDerivingStrategy ds =
  case ds of
    DerivingStock -> "DerivingStock"
    DerivingNewtype -> "DerivingNewtype"
    DerivingAnyclass -> "DerivingAnyclass"

docClassDecl :: ClassDecl -> Doc ann
docClassDecl cd =
  "ClassDecl" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      [field "name" (docText (classDeclName cd))]
        <> optionalField "context" (brackets . hsep . punctuate comma . map docConstraint) (classDeclContext cd)
        <> listField "params" docTyVarBinder (classDeclParams cd)
        <> listField "fundeps" docFunctionalDependency (classDeclFundeps cd)
        <> listField "items" docClassDeclItem (classDeclItems cd)

docFunctionalDependency :: FunctionalDependency -> Doc ann
docFunctionalDependency dep =
  "FunctionalDependency" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      listField "determiners" docText (functionalDependencyDeterminers dep)
        <> listField "determined" docText (functionalDependencyDetermined dep)

docClassDeclItem :: ClassDeclItem -> Doc ann
docClassDeclItem item =
  case item of
    ClassItemTypeSig _ names ty -> "ClassItemTypeSig" <+> braces (hsep (punctuate comma [field "names" (docTextList names), field "type" (docType ty)]))
    ClassItemFixity _ assoc mPrec ops -> "ClassItemFixity" <+> braces (hsep (punctuate comma ([field "assoc" (docFixityAssoc assoc)] <> optionalField "prec" pretty mPrec <> [field "ops" (docTextList ops)])))
    ClassItemDefault _ vdecl -> "ClassItemDefault" <+> parens (docValueDecl vdecl)

docInstanceDecl :: InstanceDecl -> Doc ann
docInstanceDecl inst =
  "InstanceDecl" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      [field "className" (docText (instanceDeclClassName inst))]
        <> listField "context" docConstraint (instanceDeclContext inst)
        <> [field "types" (brackets (hsep (punctuate comma (map docType (instanceDeclTypes inst)))))]
        <> listField "items" docInstanceDeclItem (instanceDeclItems inst)

docInstanceDeclItem :: InstanceDeclItem -> Doc ann
docInstanceDeclItem item =
  case item of
    InstanceItemBind _ vdecl -> "InstanceItemBind" <+> parens (docValueDecl vdecl)
    InstanceItemTypeSig _ names ty -> "InstanceItemTypeSig" <+> braces (hsep (punctuate comma [field "names" (docTextList names), field "type" (docType ty)]))
    InstanceItemFixity _ assoc mPrec ops -> "InstanceItemFixity" <+> braces (hsep (punctuate comma ([field "assoc" (docFixityAssoc assoc)] <> optionalField "prec" pretty mPrec <> [field "ops" (docTextList ops)])))

docStandaloneDerivingDecl :: StandaloneDerivingDecl -> Doc ann
docStandaloneDerivingDecl sd =
  "StandaloneDerivingDecl" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      [field "className" (docText (standaloneDerivingClassName sd))]
        <> optionalField "strategy" docDerivingStrategy (standaloneDerivingStrategy sd)
        <> listField "context" docConstraint (standaloneDerivingContext sd)
        <> [field "types" (brackets (hsep (punctuate comma (map docType (standaloneDerivingTypes sd)))))]

docForeignDecl :: ForeignDecl -> Doc ann
docForeignDecl fd =
  "ForeignDecl" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      [field "direction" (docForeignDirection (foreignDirection fd))]
        <> [field "callConv" (docCallConv (foreignCallConv fd))]
        <> optionalField "safety" docForeignSafety (foreignSafety fd)
        <> [field "entity" (docForeignEntitySpec (foreignEntity fd))]
        <> [field "name" (docText (foreignName fd))]
        <> [field "type" (docType (foreignType fd))]

docForeignDirection :: ForeignDirection -> Doc ann
docForeignDirection fd =
  case fd of
    ForeignImport -> "ForeignImport"
    ForeignExport -> "ForeignExport"

docCallConv :: CallConv -> Doc ann
docCallConv cc =
  case cc of
    CCall -> "CCall"
    StdCall -> "StdCall"

docForeignSafety :: ForeignSafety -> Doc ann
docForeignSafety fs =
  case fs of
    Safe -> "Safe"
    Unsafe -> "Unsafe"

docForeignEntitySpec :: ForeignEntitySpec -> Doc ann
docForeignEntitySpec spec =
  case spec of
    ForeignEntityDynamic -> "ForeignEntityDynamic"
    ForeignEntityWrapper -> "ForeignEntityWrapper"
    ForeignEntityStatic mName -> "ForeignEntityStatic" <> optionalField' docText mName
    ForeignEntityAddress mName -> "ForeignEntityAddress" <> optionalField' docText mName
    ForeignEntityNamed name -> "ForeignEntityNamed" <+> docText name
    ForeignEntityOmitted -> "ForeignEntityOmitted"

docFixityAssoc :: FixityAssoc -> Doc ann
docFixityAssoc fa =
  case fa of
    Infix -> "Infix"
    InfixL -> "InfixL"
    InfixR -> "InfixR"

-- Types

docType :: Type -> Doc ann
docType ty =
  case ty of
    TVar _ name -> "TVar" <+> docText name
    TCon _ name promoted ->
      if promoted == Promoted
        then "TConPromoted" <+> docText name
        else "TCon" <+> docText name
    TTypeLit _ lit -> "TTypeLit" <+> docTypeLiteral lit
    TStar _ -> "TStar"
    TQuasiQuote _ quoter body -> "TQuasiQuote" <+> docText quoter <+> docText body
    TForall _ binders inner -> "TForall" <+> brackets (hsep (punctuate comma (map docText binders))) <+> parens (docType inner)
    TApp _ f x -> "TApp" <+> parens (docType f) <+> parens (docType x)
    TFun _ a b -> "TFun" <+> parens (docType a) <+> parens (docType b)
    TTuple _ tupleFlavor promoted elems ->
      ( case (tupleFlavor, promoted) of
          (Boxed, Promoted) -> "TTuplePromoted"
          (Boxed, Unpromoted) -> "TTuple"
          (Unboxed, Promoted) -> "TTuplePromotedUnboxed"
          (Unboxed, Unpromoted) -> "TTupleUnboxed"
      )
        <+> brackets (hsep (punctuate comma (map docType elems)))
    TList _ promoted inner ->
      (if promoted == Promoted then "TListPromoted" else "TList")
        <+> parens (docType inner)
    TParen _ inner -> "TParen" <+> parens (docType inner)
    TContext _ constraints inner -> "TContext" <+> brackets (hsep (punctuate comma (map docConstraint constraints))) <+> parens (docType inner)

docTypeLiteral :: TypeLiteral -> Doc ann
docTypeLiteral lit =
  case lit of
    TypeLitInteger n _ -> "TypeLitInteger" <+> pretty n
    TypeLitSymbol s _ -> "TypeLitSymbol" <+> docText s
    TypeLitChar c _ -> "TypeLitChar" <+> pretty (show c)

docConstraint :: Constraint -> Doc ann
docConstraint c =
  case c of
    Constraint _ cls args ->
      "Constraint" <+> braces (hsep (punctuate comma ([field "class" (docText cls)] <> listField "args" docType args)))
    CParen _ inner ->
      "CParen" <+> parens (docConstraint inner)

docTyVarBinder :: TyVarBinder -> Doc ann
docTyVarBinder tvb =
  "TyVarBinder" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      [field "name" (docText (tyVarBinderName tvb))]
        <> optionalField "kind" docType (tyVarBinderKind tvb)

-- Patterns

docPattern :: Pattern -> Doc ann
docPattern pat =
  case pat of
    PVar _ name -> "PVar" <+> docText name
    PWildcard _ -> "PWildcard"
    PLit _ lit -> "PLit" <+> parens (docLiteral lit)
    PQuasiQuote _ quoter body -> "PQuasiQuote" <+> docText quoter <+> docText body
    PTuple _ tupleFlavor elems ->
      (if tupleFlavor == Boxed then "PTuple" else "PTupleUnboxed")
        <+> brackets (hsep (punctuate comma (map docPattern elems)))
    PList _ elems -> "PList" <+> brackets (hsep (punctuate comma (map docPattern elems)))
    PCon _ name args -> "PCon" <+> docText name <+> brackets (hsep (punctuate comma (map docPattern args)))
    PInfix _ lhs op rhs -> "PInfix" <+> parens (docPattern lhs) <+> docText op <+> parens (docPattern rhs)
    PView _ expr inner -> "PView" <+> parens (docExpr expr) <+> parens (docPattern inner)
    PAs _ name inner -> "PAs" <+> docText name <+> parens (docPattern inner)
    PStrict _ inner -> "PStrict" <+> parens (docPattern inner)
    PIrrefutable _ inner -> "PIrrefutable" <+> parens (docPattern inner)
    PNegLit _ lit -> "PNegLit" <+> parens (docLiteral lit)
    PParen _ inner -> "PParen" <+> parens (docPattern inner)
    PRecord _ name fields' -> "PRecord" <+> docText name <+> braces (hsep (punctuate comma [docText fn <+> "=" <+> docPattern fp | (fn, fp) <- fields']))

docLiteral :: Literal -> Doc ann
docLiteral lit =
  case lit of
    LitInt _ n _ -> "LitInt" <+> pretty n
    LitIntHash _ n repr -> "LitIntHash" <+> pretty n <+> docText repr
    LitIntBase _ n repr -> "LitIntBase" <+> pretty n <+> docText repr
    LitIntBaseHash _ n repr -> "LitIntBaseHash" <+> pretty n <+> docText repr
    LitFloat _ n _ -> "LitFloat" <+> pretty n
    LitFloatHash _ n repr -> "LitFloatHash" <+> pretty n <+> docText repr
    LitChar _ c _ -> "LitChar" <+> pretty (show c)
    LitCharHash _ c repr -> "LitCharHash" <+> pretty (show c) <+> docText repr
    LitString _ s _ -> "LitString" <+> docText s
    LitStringHash _ s repr -> "LitStringHash" <+> docText s <+> docText repr

-- Expressions

docExpr :: Expr -> Doc ann
docExpr expr =
  case expr of
    EVar _ name -> "EVar" <+> docText name
    EInt _ n _ -> "EInt" <+> pretty n
    EIntHash _ n repr -> "EIntHash" <+> pretty n <+> docText repr
    EIntBase _ n repr -> "EIntBase" <+> pretty n <+> docText repr
    EIntBaseHash _ n repr -> "EIntBaseHash" <+> pretty n <+> docText repr
    EFloat _ n _ -> "EFloat" <+> pretty n
    EFloatHash _ n repr -> "EFloatHash" <+> pretty n <+> docText repr
    EChar _ c _ -> "EChar" <+> pretty (show c)
    ECharHash _ c repr -> "ECharHash" <+> pretty (show c) <+> docText repr
    EString _ s _ -> "EString" <+> docText s
    EStringHash _ s repr -> "EStringHash" <+> docText s <+> docText repr
    EQuasiQuote _ quoter body -> "EQuasiQuote" <+> docText quoter <+> docText body
    EIf _ cond yes no -> "EIf" <+> parens (docExpr cond) <+> parens (docExpr yes) <+> parens (docExpr no)
    EMultiWayIf _ rhss -> "EMultiWayIf" <+> brackets (hsep (punctuate comma (map docGuardedRhs rhss)))
    ELambdaPats _ pats body -> "ELambdaPats" <+> brackets (hsep (punctuate comma (map docPattern pats))) <+> parens (docExpr body)
    ELambdaCase _ alts -> "ELambdaCase" <+> brackets (hsep (punctuate comma (map docCaseAlt alts)))
    EInfix _ lhs op rhs -> "EInfix" <+> parens (docExpr lhs) <+> docText op <+> parens (docExpr rhs)
    ENegate _ inner -> "ENegate" <+> parens (docExpr inner)
    ESectionL _ lhs op -> "ESectionL" <+> parens (docExpr lhs) <+> docText op
    ESectionR _ op rhs -> "ESectionR" <+> docText op <+> parens (docExpr rhs)
    ELetDecls _ decls body -> "ELetDecls" <+> brackets (hsep (punctuate comma (map docDecl decls))) <+> parens (docExpr body)
    ECase _ scrutinee alts -> "ECase" <+> parens (docExpr scrutinee) <+> brackets (hsep (punctuate comma (map docCaseAlt alts)))
    EDo _ stmts -> "EDo" <+> brackets (hsep (punctuate comma (map docDoStmt stmts)))
    EListComp _ body quals -> "EListComp" <+> parens (docExpr body) <+> brackets (hsep (punctuate comma (map docCompStmt quals)))
    EListCompParallel _ body qualGroups -> "EListCompParallel" <+> parens (docExpr body) <+> brackets (hsep (punctuate "|" [brackets (hsep (punctuate comma (map docCompStmt qs))) | qs <- qualGroups]))
    EArithSeq _ seqInfo -> "EArithSeq" <+> parens (docArithSeq seqInfo)
    ERecordCon _ name fields' -> "ERecordCon" <+> docText name <+> braces (hsep (punctuate comma [docText fn <+> "=" <+> docExpr fv | (fn, fv) <- fields']))
    ERecordUpd _ base fields' -> "ERecordUpd" <+> parens (docExpr base) <+> braces (hsep (punctuate comma [docText fn <+> "=" <+> docExpr fv | (fn, fv) <- fields']))
    ETypeSig _ inner ty -> "ETypeSig" <+> parens (docExpr inner) <+> parens (docType ty)
    EParen _ inner -> "EParen" <+> parens (docExpr inner)
    EWhereDecls _ body decls -> "EWhereDecls" <+> parens (docExpr body) <+> brackets (hsep (punctuate comma (map docDecl decls)))
    EList _ elems -> "EList" <+> brackets (hsep (punctuate comma (map docExpr elems)))
    ETuple _ tupleFlavor elems ->
      (if tupleFlavor == Boxed then "ETuple" else "ETupleUnboxed")
        <+> brackets (hsep (punctuate comma (map docExpr elems)))
    ETupleSection _ tupleFlavor elems ->
      (if tupleFlavor == Boxed then "ETupleSection" else "ETupleSectionUnboxed")
        <+> brackets (hsep (punctuate comma (map (maybe "_" docExpr) elems)))
    ETupleCon _ tupleFlavor arity ->
      (if tupleFlavor == Boxed then "ETupleCon" else "ETupleConUnboxed")
        <+> pretty arity
    ETypeApp _ inner ty -> "ETypeApp" <+> parens (docExpr inner) <+> parens (docType ty)
    EApp _ f x -> "EApp" <+> parens (docExpr f) <+> parens (docExpr x)

docCaseAlt :: CaseAlt -> Doc ann
docCaseAlt (CaseAlt _ pat rhs) =
  "CaseAlt" <+> parens (docPattern pat) <+> parens (docRhs rhs)

docDoStmt :: DoStmt -> Doc ann
docDoStmt stmt =
  case stmt of
    DoBind _ pat expr -> "DoBind" <+> parens (docPattern pat) <+> parens (docExpr expr)
    DoLet _ bindings -> "DoLet" <+> braces (hsep (punctuate comma [docText name <+> "=" <+> docExpr e | (name, e) <- bindings]))
    DoLetDecls _ decls -> "DoLetDecls" <+> brackets (hsep (punctuate comma (map docDecl decls)))
    DoExpr _ expr -> "DoExpr" <+> parens (docExpr expr)

docCompStmt :: CompStmt -> Doc ann
docCompStmt stmt =
  case stmt of
    CompGen _ pat expr -> "CompGen" <+> parens (docPattern pat) <+> parens (docExpr expr)
    CompGuard _ expr -> "CompGuard" <+> parens (docExpr expr)
    CompLet _ bindings -> "CompLet" <+> braces (hsep (punctuate comma [docText name <+> "=" <+> docExpr e | (name, e) <- bindings]))
    CompLetDecls _ decls -> "CompLetDecls" <+> brackets (hsep (punctuate comma (map docDecl decls)))

docArithSeq :: ArithSeq -> Doc ann
docArithSeq seqInfo =
  case seqInfo of
    ArithSeqFrom _ from -> "ArithSeqFrom" <+> parens (docExpr from)
    ArithSeqFromThen _ from thn -> "ArithSeqFromThen" <+> parens (docExpr from) <+> parens (docExpr thn)
    ArithSeqFromTo _ from to -> "ArithSeqFromTo" <+> parens (docExpr from) <+> parens (docExpr to)
    ArithSeqFromThenTo _ from thn to -> "ArithSeqFromThenTo" <+> parens (docExpr from) <+> parens (docExpr thn) <+> parens (docExpr to)

-- Token pretty printing

docToken :: LexToken -> Doc ann
docToken tok = docTokenKind (lexTokenKind tok)

docTokenKind :: LexTokenKind -> Doc ann
docTokenKind kind =
  case kind of
    TkKeywordCase -> "TkKeywordCase"
    TkKeywordClass -> "TkKeywordClass"
    TkKeywordData -> "TkKeywordData"
    TkKeywordDefault -> "TkKeywordDefault"
    TkKeywordDeriving -> "TkKeywordDeriving"
    TkKeywordDo -> "TkKeywordDo"
    TkKeywordElse -> "TkKeywordElse"
    TkKeywordForeign -> "TkKeywordForeign"
    TkKeywordIf -> "TkKeywordIf"
    TkKeywordImport -> "TkKeywordImport"
    TkKeywordIn -> "TkKeywordIn"
    TkKeywordInfix -> "TkKeywordInfix"
    TkKeywordInfixl -> "TkKeywordInfixl"
    TkKeywordInfixr -> "TkKeywordInfixr"
    TkKeywordInstance -> "TkKeywordInstance"
    TkKeywordLet -> "TkKeywordLet"
    TkKeywordModule -> "TkKeywordModule"
    TkKeywordNewtype -> "TkKeywordNewtype"
    TkKeywordOf -> "TkKeywordOf"
    TkKeywordThen -> "TkKeywordThen"
    TkKeywordType -> "TkKeywordType"
    TkKeywordWhere -> "TkKeywordWhere"
    TkKeywordUnderscore -> "TkKeywordUnderscore"
    TkKeywordQualified -> "TkKeywordQualified"
    TkKeywordAs -> "TkKeywordAs"
    TkKeywordHiding -> "TkKeywordHiding"
    TkReservedDotDot -> "TkReservedDotDot"
    TkReservedColon -> "TkReservedColon"
    TkReservedDoubleColon -> "TkReservedDoubleColon"
    TkReservedEquals -> "TkReservedEquals"
    TkReservedBackslash -> "TkReservedBackslash"
    TkReservedPipe -> "TkReservedPipe"
    TkReservedLeftArrow -> "TkReservedLeftArrow"
    TkReservedRightArrow -> "TkReservedRightArrow"
    TkReservedAt -> "TkReservedAt"
    TkReservedDoubleArrow -> "TkReservedDoubleArrow"
    TkVarId name -> "TkVarId" <+> docText name
    TkConId name -> "TkConId" <+> docText name
    TkQVarId name -> "TkQVarId" <+> docText name
    TkQConId name -> "TkQConId" <+> docText name
    TkVarSym name -> "TkVarSym" <+> docText name
    TkConSym name -> "TkConSym" <+> docText name
    TkQVarSym name -> "TkQVarSym" <+> docText name
    TkQConSym name -> "TkQConSym" <+> docText name
    TkInteger n -> "TkInteger" <+> pretty n
    TkIntegerHash n repr -> "TkIntegerHash" <+> pretty n <+> docText repr
    TkIntegerBase n repr -> "TkIntegerBase" <+> pretty n <+> docText repr
    TkIntegerBaseHash n repr -> "TkIntegerBaseHash" <+> pretty n <+> docText repr
    TkFloat n repr -> "TkFloat" <+> pretty n <+> docText repr
    TkFloatHash n repr -> "TkFloatHash" <+> pretty n <+> docText repr
    TkChar c -> "TkChar" <+> pretty (show c)
    TkCharHash c repr -> "TkCharHash" <+> pretty (show c) <+> docText repr
    TkString s -> "TkString" <+> docText s
    TkStringHash s repr -> "TkStringHash" <+> docText s <+> docText repr
    TkSpecialLParen -> "TkSpecialLParen"
    TkSpecialRParen -> "TkSpecialRParen"
    TkSpecialUnboxedLParen -> "TkSpecialUnboxedLParen"
    TkSpecialUnboxedRParen -> "TkSpecialUnboxedRParen"
    TkSpecialComma -> "TkSpecialComma"
    TkSpecialSemicolon -> "TkSpecialSemicolon"
    TkSpecialLBracket -> "TkSpecialLBracket"
    TkSpecialRBracket -> "TkSpecialRBracket"
    TkSpecialBacktick -> "TkSpecialBacktick"
    TkSpecialLBrace -> "TkSpecialLBrace"
    TkSpecialRBrace -> "TkSpecialRBrace"
    TkMinusOperator -> "TkMinusOperator"
    TkPrefixMinus -> "TkPrefixMinus"
    TkPrefixBang -> "TkPrefixBang"
    TkPrefixTilde -> "TkPrefixTilde"
    TkPragmaLanguage settings -> "TkPragmaLanguage" <+> brackets (hsep (punctuate comma (map docExtensionSetting settings)))
    TkPragmaWarning msg -> "TkPragmaWarning" <+> docText msg
    TkPragmaDeprecated msg -> "TkPragmaDeprecated" <+> docText msg
    TkQuasiQuote quoter body -> "TkQuasiQuote" <+> docText quoter <+> docText body
    TkError msg -> "TkError" <+> docText msg
    TkEOF -> "TkEOF"

-- Helpers

field :: Text -> Doc ann -> Doc ann
field name val = pretty name <+> "=" <+> val

optionalField :: Text -> (a -> Doc ann) -> Maybe a -> [Doc ann]
optionalField name f mVal =
  case mVal of
    Just val -> [field name (f val)]
    Nothing -> []

optionalField' :: (a -> Doc ann) -> Maybe a -> Doc ann
optionalField' f mVal =
  case mVal of
    Just val -> " " <> f val
    Nothing -> ""

listField :: Text -> (a -> Doc ann) -> [a] -> [Doc ann]
listField _ _ [] = []
listField name f xs = [field name (brackets (hsep (punctuate comma (map f xs))))]

boolField :: Text -> Bool -> [Doc ann]
boolField _ False = []
boolField name True = [field name "True"]

docText :: Text -> Doc ann
docText t = dquotes (pretty t)

docTextList :: [Text] -> Doc ann
docTextList ts = brackets (hsep (punctuate comma (map docText ts)))
