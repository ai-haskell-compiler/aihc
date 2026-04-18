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
-- >>> shorthand $ snd $ parseModule defaultConfig "module Demo where x = 1"
-- Module {name = "Demo", decls = [DeclValue (FunctionBind "x" [Match {headForm = Prefix, rhs = UnguardedRhs (EInt 1)}])]}
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

instance Shorthand Decl where
  shorthand = docDecl

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
    DeprText msg -> "DeprText" <+> docText msg
    WarnText msg -> "WarnText" <+> docText msg
    WarningTextAnn _ sub -> docWarningText sub

docExtensionSetting :: ExtensionSetting -> Doc ann
docExtensionSetting setting =
  case setting of
    EnableExtension ext -> "EnableExtension" <+> pretty (extensionName ext)
    DisableExtension ext -> "DisableExtension" <+> pretty (extensionName ext)

docExportSpec :: ExportSpec -> Doc ann
docExportSpec spec =
  case spec of
    ExportAnn _ sub -> docExportSpec sub
    ExportModule mWarning name ->
      "ExportModule" <> braces (hsep (punctuate comma (optionalField "warningText" docWarningText mWarning <> [field "name" (docText name)])))
    ExportVar mWarning mNamespace name ->
      "ExportVar" <> braces (hsep (punctuate comma (optionalField "warningText" docWarningText mWarning <> optionalField "namespace" docIENamespace mNamespace <> [field "name" (docName name)])))
    ExportAbs mWarning mNamespace name ->
      "ExportAbs" <> braces (hsep (punctuate comma (optionalField "warningText" docWarningText mWarning <> optionalField "namespace" docIENamespace mNamespace <> [field "name" (docName name)])))
    ExportAll mWarning mNamespace name ->
      "ExportAll" <> braces (hsep (punctuate comma (optionalField "warningText" docWarningText mWarning <> optionalField "namespace" docIENamespace mNamespace <> [field "name" (docName name)])))
    ExportWith mWarning mNamespace name members ->
      "ExportWith" <> braces (hsep (punctuate comma fields))
      where
        fields =
          optionalField "warningText" docWarningText mWarning
            <> optionalField "namespace" docIENamespace mNamespace
            <> [field "name" (docName name), field "members" (brackets (hsep (punctuate comma (map docExportMember members))))]
    ExportWithAll mWarning mNamespace name wildcardIndex members ->
      "ExportWithAll" <> braces (hsep (punctuate comma fields))
      where
        fields =
          optionalField "warningText" docWarningText mWarning
            <> optionalField "namespace" docIENamespace mNamespace
            <> [field "name" (docName name), field "wildcardIndex" (pretty wildcardIndex), field "members" (brackets (hsep (punctuate comma (map docExportMember members))))]

docImportDecl :: ImportDecl -> Doc ann
docImportDecl decl =
  "ImportDecl" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      [field "module" (docText (importDeclModule decl))]
        <> boolField "source" (importDeclSource decl)
        <> boolField "safe" (importDeclSafe decl)
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
    ImportAnn _ sub -> docImportItem sub
    ImportItemVar mNamespace name ->
      "ImportItemVar" <> braces (hsep (punctuate comma (optionalField "namespace" docIENamespace mNamespace <> [field "name" (docUnqualifiedName name)])))
    ImportItemAbs mNamespace name ->
      "ImportItemAbs" <> braces (hsep (punctuate comma (optionalField "namespace" docIENamespace mNamespace <> [field "name" (docUnqualifiedName name)])))
    ImportItemAll mNamespace name ->
      "ImportItemAll" <> braces (hsep (punctuate comma (optionalField "namespace" docIENamespace mNamespace <> [field "name" (docUnqualifiedName name)])))
    ImportItemWith mNamespace name members ->
      "ImportItemWith" <> braces (hsep (punctuate comma fields))
      where
        fields =
          optionalField "namespace" docIENamespace mNamespace
            <> [field "name" (docUnqualifiedName name), field "members" (brackets (hsep (punctuate comma (map docExportMember members))))]
    ImportItemAllWith mNamespace name wildcardIndex members ->
      "ImportItemAllWith" <> braces (hsep (punctuate comma fields))
      where
        fields =
          optionalField "namespace" docIENamespace mNamespace
            <> [field "name" (docUnqualifiedName name), field "wildcardIndex" (pretty wildcardIndex), field "members" (brackets (hsep (punctuate comma (map docExportMember members))))]

docIENamespace :: IEEntityNamespace -> Doc ann
docIENamespace namespace =
  case namespace of
    IEEntityNamespaceType -> docText "type"
    IEEntityNamespacePattern -> docText "pattern"
    IEEntityNamespaceData -> docText "data"

docIEBundledNamespace :: IEBundledNamespace -> Doc ann
docIEBundledNamespace namespace =
  case namespace of
    IEBundledNamespaceData -> docText "data"

docExportMember :: IEBundledMember -> Doc ann
docExportMember (IEBundledMember mNamespace name) =
  "ExportMember" <> braces (hsep (punctuate comma (optionalField "namespace" docIEBundledNamespace mNamespace <> [field "name" (docName name)])))

-- Declarations

docDecl :: Decl -> Doc ann
docDecl decl =
  case decl of
    DeclAnn _ sub -> docDecl sub
    DeclValue vdecl -> "DeclValue" <+> parens (docValueDecl vdecl)
    DeclTypeSig names ty -> "DeclTypeSig" <+> braces (hsep (punctuate comma [field "names" (brackets (hsep (punctuate comma (map docUnqualifiedName names)))), field "type" (docType ty)]))
    DeclPatSyn ps -> "DeclPatSyn" <+> parens (docPatSynDecl ps)
    DeclPatSynSig names ty -> "DeclPatSynSig" <+> braces (hsep (punctuate comma [field "names" (brackets (hsep (punctuate comma (map docUnqualifiedName names)))), field "type" (docType ty)]))
    DeclStandaloneKindSig name kind -> "DeclStandaloneKindSig" <+> braces (hsep (punctuate comma [field "name" (docUnqualifiedName name), field "kind" (docType kind)]))
    DeclFixity assoc mNamespace mPrec ops -> "DeclFixity" <+> braces (hsep (punctuate comma ([field "assoc" (docFixityAssoc assoc)] <> optionalField "namespace" docIENamespace mNamespace <> optionalField "prec" pretty mPrec <> [field "ops" (brackets (hsep (punctuate comma (map docUnqualifiedName ops))))])))
    DeclRoleAnnotation ann -> "DeclRoleAnnotation" <+> parens (docRoleAnnotation ann)
    DeclTypeSyn syn -> "DeclTypeSyn" <+> parens (docTypeSynDecl syn)
    DeclData dd -> "DeclData" <+> parens (docDataDecl dd)
    DeclTypeData dd -> "DeclTypeData" <+> parens (docDataDecl dd)
    DeclNewtype nd -> "DeclNewtype" <+> parens (docNewtypeDecl nd)
    DeclClass cd -> "DeclClass" <+> parens (docClassDecl cd)
    DeclInstance inst -> "DeclInstance" <+> parens (docInstanceDecl inst)
    DeclStandaloneDeriving sd -> "DeclStandaloneDeriving" <+> parens (docStandaloneDerivingDecl sd)
    DeclDefault tys -> "DeclDefault" <+> brackets (hsep (punctuate comma (map docType tys)))
    DeclForeign fd -> "DeclForeign" <+> parens (docForeignDecl fd)
    DeclSplice body -> "DeclSplice" <+> parens (docExpr body)
    DeclTypeFamilyDecl tf -> "DeclTypeFamilyDecl" <+> parens (docTypeFamilyDecl tf)
    DeclDataFamilyDecl df -> "DeclDataFamilyDecl" <+> parens (docDataFamilyDecl df)
    DeclTypeFamilyInst tfi -> "DeclTypeFamilyInst" <+> parens (docTypeFamilyInst tfi)
    DeclDataFamilyInst dfi -> "DeclDataFamilyInst" <+> parens (docDataFamilyInst dfi)
    DeclPragma pragma -> "DeclPragma" <+> docPragma pragma

docValueDecl :: ValueDecl -> Doc ann
docValueDecl vdecl =
  case vdecl of
    FunctionBind name matches -> "FunctionBind" <+> docUnqualifiedName name <+> brackets (hsep (punctuate comma (map docMatch matches)))
    PatternBind pat rhs -> "PatternBind" <+> docPattern pat <+> docRhs rhs

docPatSynDecl :: PatSynDecl -> Doc ann
docPatSynDecl ps =
  "PatSynDecl" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      [field "name" (docUnqualifiedName (patSynDeclName ps))]
        <> [field "args" (docPatSynArgs (patSynDeclArgs ps))]
        <> [field "dir" (docPatSynDir (patSynDeclDir ps))]
        <> [field "pat" (docPattern (patSynDeclPat ps))]

docPatSynDir :: PatSynDir -> Doc ann
docPatSynDir dir =
  case dir of
    PatSynUnidirectional -> "Unidirectional"
    PatSynBidirectional -> "Bidirectional"
    PatSynExplicitBidirectional matches ->
      "ExplicitBidirectional" <+> brackets (hsep (punctuate comma (map docMatch matches)))

docPatSynArgs :: PatSynArgs -> Doc ann
docPatSynArgs args =
  case args of
    PatSynPrefixArgs vars -> "PrefixArgs" <+> docTextList vars
    PatSynInfixArgs lhs rhs -> "InfixArgs" <+> docText lhs <+> docText rhs
    PatSynRecordArgs fields' -> "RecordArgs" <+> docTextList fields'

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
    UnguardedRhs _ expr Nothing -> "UnguardedRhs" <+> parens (docExpr expr)
    UnguardedRhs _ expr (Just decls) -> "UnguardedRhs" <+> parens (docExpr expr) <+> "where" <+> brackets (hsep (punctuate comma (map docDecl decls)))
    GuardedRhss _ grhss Nothing -> "GuardedRhss" <+> brackets (hsep (punctuate comma (map docGuardedRhs grhss)))
    GuardedRhss _ grhss (Just decls) -> "GuardedRhss" <+> brackets (hsep (punctuate comma (map docGuardedRhs grhss))) <+> "where" <+> brackets (hsep (punctuate comma (map docDecl decls)))

docGuardedRhs :: GuardedRhs -> Doc ann
docGuardedRhs grhs =
  "GuardedRhs" <+> braces (hsep (punctuate comma [field "guards" (brackets (hsep (punctuate comma (map docGuardQualifier (guardedRhsGuards grhs))))), field "body" (docExpr (guardedRhsBody grhs))]))

docGuardQualifier :: GuardQualifier -> Doc ann
docGuardQualifier gq =
  case gq of
    GuardAnn _ inner -> docGuardQualifier inner
    GuardExpr expr -> "GuardExpr" <+> parens (docExpr expr)
    GuardPat pat expr -> "GuardPat" <+> parens (docPattern pat) <+> parens (docExpr expr)
    GuardLet decls -> "GuardLet" <+> brackets (hsep (punctuate comma (map docDecl decls)))

docTypeSynDecl :: TypeSynDecl -> Doc ann
docTypeSynDecl syn =
  "TypeSynDecl" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      [field "name" (docUnqualifiedName (typeSynName syn))]
        <> listField "params" docTyVarBinder (typeSynParams syn)
        <> [field "body" (docType (typeSynBody syn))]

docRoleAnnotation :: RoleAnnotation -> Doc ann
docRoleAnnotation ann =
  "RoleAnnotation" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      [field "name" (docUnqualifiedName (roleAnnotationName ann))]
        <> [field "roles" (brackets (hsep (punctuate comma (map docRole (roleAnnotationRoles ann)))))]

docRole :: Role -> Doc ann
docRole role =
  case role of
    RoleNominal -> "RoleNominal"
    RoleRepresentational -> "RoleRepresentational"
    RolePhantom -> "RolePhantom"
    RoleInfer -> "RoleInfer"

docDataDecl :: DataDecl -> Doc ann
docDataDecl dd =
  "DataDecl" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      [field "name" (docUnqualifiedName (dataDeclName dd))]
        <> listField "context" docType (dataDeclContext dd)
        <> listField "params" docTyVarBinder (dataDeclParams dd)
        <> optionalField "kind" docType (dataDeclKind dd)
        <> listField "constructors" docDataConDecl (dataDeclConstructors dd)
        <> listField "deriving" docDerivingClause (dataDeclDeriving dd)

docNewtypeDecl :: NewtypeDecl -> Doc ann
docNewtypeDecl nd =
  "NewtypeDecl" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      [field "name" (docUnqualifiedName (newtypeDeclName nd))]
        <> listField "context" docType (newtypeDeclContext nd)
        <> listField "params" docTyVarBinder (newtypeDeclParams nd)
        <> optionalField "kind" docType (newtypeDeclKind nd)
        <> optionalField "constructor" docDataConDecl (newtypeDeclConstructor nd)
        <> listField "deriving" docDerivingClause (newtypeDeclDeriving nd)

docDataConDecl :: DataConDecl -> Doc ann
docDataConDecl dcd =
  case dcd of
    DataConAnn _ inner -> docDataConDecl inner
    PrefixCon forallVars constraints name fields' ->
      "PrefixCon" <+> braces (hsep (punctuate comma ([field "name" (docUnqualifiedName name)] <> listField "forallVars" docText forallVars <> listField "constraints" docType constraints <> listField "fields" docBangType fields')))
    InfixCon forallVars constraints lhs op rhs ->
      "InfixCon" <+> braces (hsep (punctuate comma ([field "op" (docUnqualifiedName op), field "lhs" (docBangType lhs), field "rhs" (docBangType rhs)] <> listField "forallVars" docText forallVars <> listField "constraints" docType constraints)))
    RecordCon forallVars constraints name fields' ->
      "RecordCon" <+> braces (hsep (punctuate comma ([field "name" (docUnqualifiedName name)] <> listField "forallVars" docText forallVars <> listField "constraints" docType constraints <> listField "fields" docFieldDecl fields')))
    GadtCon forallBinders constraints names body ->
      "GadtCon" <+> braces (hsep (punctuate comma (listField "names" docUnqualifiedName names <> listField "forallBinders" docForallTelescope forallBinders <> listField "constraints" docType constraints <> [field "body" (docGadtBody body)])))

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
      sourceUnpackednessField (bangSourceUnpackedness bt)
        <> boolField "strict" (bangStrict bt)
        <> boolField "lazy" (bangLazy bt)
        <> [field "type" (docType (bangType bt))]

sourceUnpackednessField :: SourceUnpackedness -> [Doc ann]
sourceUnpackednessField unpackedness =
  case unpackedness of
    NoSourceUnpackedness -> []
    SourceUnpack -> [field "unpackedness" "\"UNPACK\""]
    SourceNoUnpack -> [field "unpackedness" "\"NOUNPACK\""]

docFieldDecl :: FieldDecl -> Doc ann
docFieldDecl fd =
  "FieldDecl" <+> braces (hsep (punctuate comma [field "names" (brackets (hsep (punctuate comma (map docUnqualifiedName (fieldNames fd))))), field "type" (docBangType (fieldType fd))]))

docDerivingClause :: DerivingClause -> Doc ann
docDerivingClause dc =
  "DerivingClause" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      optionalField "strategy" docDerivingStrategy (derivingStrategy dc)
        <> listField "classes" docType (derivingClasses dc)
        <> optionalField "viaType" docType (derivingViaType dc)
        <> boolField "parenthesized" (derivingParenthesized dc)

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
      [field "headForm" (docTypeHeadForm (classDeclHeadForm cd)), field "name" (docUnqualifiedName (classDeclName cd))]
        <> optionalField "context" (brackets . hsep . punctuate comma . map docType) (classDeclContext cd)
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

docTypeFamilyInjectivity :: TypeFamilyInjectivity -> Doc ann
docTypeFamilyInjectivity injectivity =
  "TypeFamilyInjectivity" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      [ field "result" (docText (typeFamilyInjectivityResult injectivity))
      ]
        <> listField "determined" docText (typeFamilyInjectivityDetermined injectivity)

docTypeFamilyResultSig :: TypeFamilyResultSig -> Doc ann
docTypeFamilyResultSig sig =
  case sig of
    TypeFamilyKindSig kind -> "TypeFamilyKindSig" <+> parens (docType kind)
    TypeFamilyInjectiveSig result injectivity ->
      "TypeFamilyInjectiveSig" <+> braces (hsep (punctuate comma [field "result" (docTyVarBinder result), field "injectivity" (docTypeFamilyInjectivity injectivity)]))

docClassDeclItem :: ClassDeclItem -> Doc ann
docClassDeclItem item =
  case item of
    ClassItemAnn _ sub -> docClassDeclItem sub
    ClassItemTypeSig names ty -> "ClassItemTypeSig" <+> braces (hsep (punctuate comma [field "names" (brackets (hsep (punctuate comma (map docUnqualifiedName names)))), field "type" (docType ty)]))
    ClassItemDefaultSig name ty -> "ClassItemDefaultSig" <+> braces (hsep (punctuate comma [field "name" (docUnqualifiedName name), field "type" (docType ty)]))
    ClassItemFixity assoc mNamespace mPrec ops -> "ClassItemFixity" <+> braces (hsep (punctuate comma ([field "assoc" (docFixityAssoc assoc)] <> optionalField "namespace" docIENamespace mNamespace <> optionalField "prec" pretty mPrec <> [field "ops" (brackets (hsep (punctuate comma (map docUnqualifiedName ops))))])))
    ClassItemDefault vdecl -> "ClassItemDefault" <+> parens (docValueDecl vdecl)
    ClassItemTypeFamilyDecl tf -> "ClassItemTypeFamilyDecl" <+> parens (docTypeFamilyDecl tf)
    ClassItemDataFamilyDecl df -> "ClassItemDataFamilyDecl" <+> parens (docDataFamilyDecl df)
    ClassItemDefaultTypeInst tfi -> "ClassItemDefaultTypeInst" <+> parens (docTypeFamilyInst tfi)
    ClassItemPragma pragma -> "ClassItemPragma" <+> docPragma pragma

docInstanceDecl :: InstanceDecl -> Doc ann
docInstanceDecl inst =
  "InstanceDecl" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      optionalField "overlapPragma" docInstanceOverlapPragma (instanceDeclOverlapPragma inst)
        <> optionalField "warning" docWarningText (instanceDeclWarning inst)
        <> listField "forall" docTyVarBinder (instanceDeclForall inst)
        <> boolField "parenthesizedHead" (instanceDeclParenthesizedHead inst)
        <> [field "headForm" (docTypeHeadForm (instanceDeclHeadForm inst))]
        <> [field "className" (docUnqualifiedName (instanceDeclClassName inst))]
        <> listField "context" docType (instanceDeclContext inst)
        <> [field "types" (brackets (hsep (punctuate comma (map docType (instanceDeclTypes inst)))))]
        <> listField "items" docInstanceDeclItem (instanceDeclItems inst)

docInstanceDeclItem :: InstanceDeclItem -> Doc ann
docInstanceDeclItem item =
  case item of
    InstanceItemAnn _ inner -> docInstanceDeclItem inner
    InstanceItemBind vdecl -> "InstanceItemBind" <+> parens (docValueDecl vdecl)
    InstanceItemTypeSig names ty -> "InstanceItemTypeSig" <+> braces (hsep (punctuate comma [field "names" (brackets (hsep (punctuate comma (map docUnqualifiedName names)))), field "type" (docType ty)]))
    InstanceItemFixity assoc mNamespace mPrec ops -> "InstanceItemFixity" <+> braces (hsep (punctuate comma ([field "assoc" (docFixityAssoc assoc)] <> optionalField "namespace" docIENamespace mNamespace <> optionalField "prec" pretty mPrec <> [field "ops" (brackets (hsep (punctuate comma (map docUnqualifiedName ops))))])))
    InstanceItemTypeFamilyInst tfi -> "InstanceItemTypeFamilyInst" <+> parens (docTypeFamilyInst tfi)
    InstanceItemDataFamilyInst dfi -> "InstanceItemDataFamilyInst" <+> parens (docDataFamilyInst dfi)
    InstanceItemPragma pragma -> "InstanceItemPragma" <+> docPragma pragma

docStandaloneDerivingDecl :: StandaloneDerivingDecl -> Doc ann
docStandaloneDerivingDecl sd =
  "StandaloneDerivingDecl" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      optionalField "overlapPragma" docInstanceOverlapPragma (standaloneDerivingOverlapPragma sd)
        <> optionalField "warning" docWarningText (standaloneDerivingWarning sd)
        <> boolField "parenthesizedHead" (standaloneDerivingParenthesizedHead sd)
        <> [field "headForm" (docTypeHeadForm (standaloneDerivingHeadForm sd))]
        <> [field "className" (docName (standaloneDerivingClassName sd))]
        <> optionalField "strategy" docDerivingStrategy (standaloneDerivingStrategy sd)
        <> listField "context" docType (standaloneDerivingContext sd)
        <> [field "types" (brackets (hsep (punctuate comma (map docType (standaloneDerivingTypes sd)))))]
        <> optionalField "viaType" docType (standaloneDerivingViaType sd)

docInstanceOverlapPragma :: InstanceOverlapPragma -> Doc ann
docInstanceOverlapPragma pragma' =
  case pragma' of
    Overlapping -> "Overlapping"
    Overlappable -> "Overlappable"
    Overlaps -> "Overlaps"
    Incoherent -> "Incoherent"

docPragmaUnpackKind :: PragmaUnpackKind -> Doc ann
docPragmaUnpackKind kind =
  case kind of
    UnpackPragma -> "UnpackPragma"
    NoUnpackPragma -> "NoUnpackPragma"

docPragma :: Pragma -> Doc ann
docPragma pragma =
  case pragma of
    PragmaLanguage settings -> "PragmaLanguage" <+> brackets (hsep (punctuate comma (map docExtensionSetting settings)))
    PragmaInstanceOverlap overlapPragma -> "PragmaInstanceOverlap" <+> docInstanceOverlapPragma overlapPragma
    PragmaWarning msg -> "PragmaWarning" <+> docText msg
    PragmaDeprecated msg -> "PragmaDeprecated" <+> docText msg
    PragmaInline kind body -> "PragmaInline" <+> docText kind <+> docText body
    PragmaUnpack unpackKind -> "PragmaUnpack" <+> docPragmaUnpackKind unpackKind
    PragmaSource sourceText _ -> "PragmaSource" <+> docText sourceText
    PragmaUnknown text -> "PragmaUnknown" <+> docText text

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
    CApi -> "CApi"

docForeignSafety :: ForeignSafety -> Doc ann
docForeignSafety fs =
  case fs of
    Safe -> "Safe"
    Unsafe -> "Unsafe"
    Interruptible -> "Interruptible"

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
    TVar name -> "TVar" <+> docUnqualifiedName name
    TCon name promoted ->
      if promoted == Promoted
        then "TConPromoted" <+> docName name
        else "TCon" <+> docName name
    TImplicitParam name inner -> "TImplicitParam" <+> docText name <+> parens (docType inner)
    TTypeLit lit -> "TTypeLit" <+> docTypeLiteral lit
    TStar -> "TStar"
    TQuasiQuote quoter body -> "TQuasiQuote" <+> docText quoter <+> docText body
    TForall telescope inner ->
      case forallTelescopeVisibility telescope of
        ForallInvisible ->
          "TForall"
            <+> brackets (hsep (punctuate comma (map docTyVarBinder (forallTelescopeBinders telescope))))
            <+> parens (docType inner)
        ForallVisible -> "TForall" <+> parens (docForallTelescope telescope) <+> parens (docType inner)
    TApp f x -> "TApp" <+> parens (docType f) <+> parens (docType x)
    TInfix lhs op promoted rhs ->
      "TInfix"
        <+> parens (docType lhs)
        <+> docName op
        <> (if promoted == Promoted then "'" else "")
        <+> parens (docType rhs)
    TFun a b -> "TFun" <+> parens (docType a) <+> parens (docType b)
    TTuple tupleFlavor promoted elems ->
      ( case (tupleFlavor, promoted) of
          (Boxed, Promoted) -> "TTuplePromoted"
          (Boxed, Unpromoted) -> "TTuple"
          (Unboxed, Promoted) -> "TTuplePromotedUnboxed"
          (Unboxed, Unpromoted) -> "TTupleUnboxed"
      )
        <+> brackets (hsep (punctuate comma (map docType elems)))
    TUnboxedSum elems ->
      "TUnboxedSum"
        <+> brackets (hsep (punctuate comma (map docType elems)))
    TList promoted elems ->
      (if promoted == Promoted then "TListPromoted" else "TList")
        <+> brackets (hsep (punctuate comma (map docType elems)))
    TParen inner -> "TParen" <+> parens (docType inner)
    TKindSig ty' kind -> "TKindSig" <+> parens (docType ty') <+> parens (docType kind)
    TContext constraints inner -> "TContext" <+> brackets (hsep (punctuate comma (map docType constraints))) <+> parens (docType inner)
    TSplice body -> "TSplice" <+> parens (docExpr body)
    TWildcard -> "TWildcard"
    TAnn _ sub -> docType sub

docTypeLiteral :: TypeLiteral -> Doc ann
docTypeLiteral lit =
  case lit of
    TypeLitInteger n _ -> "TypeLitInteger" <+> pretty n
    TypeLitSymbol s _ -> "TypeLitSymbol" <+> docText s
    TypeLitChar c _ -> "TypeLitChar" <+> pretty (show c)

docTyVarBinder :: TyVarBinder -> Doc ann
docTyVarBinder tvb =
  "TyVarBinder" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      [field "name" (docText (tyVarBinderName tvb))]
        <> optionalField "specificity" docTyVarBSpecificity (specificityField tvb)
        <> optionalField "visibility" docTyVarBVisibility (visibilityField tvb)
        <> optionalField "kind" docType (tyVarBinderKind tvb)

    specificityField binder =
      case tyVarBinderSpecificity binder of
        TyVarBSpecified -> Nothing
        specificity -> Just specificity

    visibilityField binder =
      case tyVarBinderVisibility binder of
        TyVarBVisible -> Nothing
        visibility -> Just visibility

docTyVarBSpecificity :: TyVarBSpecificity -> Doc ann
docTyVarBSpecificity specificity =
  case specificity of
    TyVarBInferred -> "TyVarBInferred"
    TyVarBSpecified -> "TyVarBSpecified"

docTyVarBVisibility :: TyVarBVisibility -> Doc ann
docTyVarBVisibility visibility =
  case visibility of
    TyVarBVisible -> "TyVarBVisible"
    TyVarBInvisible -> "TyVarBInvisible"

docForallVis :: ForallVis -> Doc ann
docForallVis visibility =
  case visibility of
    ForallInvisible -> "ForallInvisible"
    ForallVisible -> "ForallVisible"

docForallTelescope :: ForallTelescope -> Doc ann
docForallTelescope telescope =
  "ForallTelescope"
    <+> braces
      ( hsep
          ( punctuate
              comma
              ( [field "visibility" (docForallVis (forallTelescopeVisibility telescope))]
                  <> listField "binders" docTyVarBinder (forallTelescopeBinders telescope)
              )
          )
      )

-- Patterns

docPattern :: Pattern -> Doc ann
docPattern pat =
  case pat of
    PAnn _ sub -> docPattern sub
    PVar name -> "PVar" <+> docUnqualifiedName name
    PTypeBinder binder -> "PTypeBinder" <+> parens (docTyVarBinder binder)
    PTypeSyntax form ty -> "PTypeSyntax" <+> docTypeSyntaxForm form <+> parens (docType ty)
    PWildcard -> "PWildcard"
    PLit lit -> "PLit" <+> parens (docLiteral lit)
    PQuasiQuote quoter body -> "PQuasiQuote" <+> docText quoter <+> docText body
    PTuple tupleFlavor elems ->
      (if tupleFlavor == Boxed then "PTuple" else "PTupleUnboxed")
        <+> brackets (hsep (punctuate comma (map docPattern elems)))
    PUnboxedSum altIdx arity inner ->
      "PUnboxedSum" <+> pretty altIdx <+> pretty arity <+> docPattern inner
    PList elems -> "PList" <+> brackets (hsep (punctuate comma (map docPattern elems)))
    PCon name typeArgs args ->
      case typeArgs of
        [] -> "PCon" <+> docName name <+> brackets (hsep (punctuate comma (map docPattern args)))
        _ ->
          "PCon"
            <+> docName name
            <+> braces
              ( hsep
                  ( punctuate
                      comma
                      ( listField "typeArgs" docType typeArgs
                          <> listField "args" docPattern args
                      )
                  )
              )
    PInfix lhs op rhs -> "PInfix" <+> parens (docPattern lhs) <+> docName op <+> parens (docPattern rhs)
    PView expr inner -> "PView" <+> parens (docExpr expr) <+> parens (docPattern inner)
    PAs name inner -> "PAs" <+> docText name <+> parens (docPattern inner)
    PStrict inner -> "PStrict" <+> parens (docPattern inner)
    PIrrefutable inner -> "PIrrefutable" <+> parens (docPattern inner)
    PNegLit lit -> "PNegLit" <+> parens (docLiteral lit)
    PParen inner -> "PParen" <+> parens (docPattern inner)
    PRecord name fields' hasWildcard -> "PRecord" <+> docName name <+> braces (hsep (punctuate comma ([docName fn <+> "=" <+> docPattern fp | (fn, fp) <- fields'] ++ [".." | hasWildcard])))
    PTypeSig inner ty -> "PTypeSig" <+> parens (docPattern inner) <+> parens (docType ty)
    PSplice body -> "PSplice" <+> parens (docExpr body)

docLiteral :: Literal -> Doc ann
docLiteral lit =
  case peelLiteralAnn lit of
    LitInt n _ -> "LitInt" <+> pretty n
    LitIntHash n repr -> "LitIntHash" <+> pretty n <+> docText repr
    LitIntBase n repr -> "LitIntBase" <+> pretty n <+> docText repr
    LitIntBaseHash n repr -> "LitIntBaseHash" <+> pretty n <+> docText repr
    LitFloat n _ -> "LitFloat" <+> pretty n
    LitFloatHash n repr -> "LitFloatHash" <+> pretty n <+> docText repr
    LitChar c _ -> "LitChar" <+> pretty (show c)
    LitCharHash c repr -> "LitCharHash" <+> pretty (show c) <+> docText repr
    LitString s _ -> "LitString" <+> docText s
    LitStringHash s repr -> "LitStringHash" <+> docText s <+> docText repr
    LitAnn {} -> error "unreachable"

-- Expressions

docExpr :: Expr -> Doc ann
docExpr expr =
  case expr of
    EVar name -> "EVar" <+> docName name
    ETypeSyntax form ty -> "ETypeSyntax" <+> docTypeSyntaxForm form <+> parens (docType ty)
    EInt n _ -> "EInt" <+> pretty n
    EIntHash n repr -> "EIntHash" <+> pretty n <+> docText repr
    EIntBase n repr -> "EIntBase" <+> pretty n <+> docText repr
    EIntBaseHash n repr -> "EIntBaseHash" <+> pretty n <+> docText repr
    EFloat n _ -> "EFloat" <+> pretty n
    EFloatHash n repr -> "EFloatHash" <+> pretty n <+> docText repr
    EChar c _ -> "EChar" <+> pretty (show c)
    ECharHash c repr -> "ECharHash" <+> pretty (show c) <+> docText repr
    EString s _ -> "EString" <+> docText s
    EStringHash s repr -> "EStringHash" <+> docText s <+> docText repr
    EOverloadedLabel label raw -> "EOverloadedLabel" <+> docText label <+> docText raw
    EQuasiQuote quoter body -> "EQuasiQuote" <+> docText quoter <+> docText body
    ETHExpQuote body -> "ETHExpQuote" <+> parens (docExpr body)
    ETHTypedQuote body -> "ETHTypedQuote" <+> parens (docExpr body)
    ETHDeclQuote decls -> "ETHDeclQuote" <+> brackets (hsep (punctuate comma (map docDecl decls)))
    ETHTypeQuote ty -> "ETHTypeQuote" <+> parens (docType ty)
    ETHPatQuote pat -> "ETHPatQuote" <+> parens (docPattern pat)
    ETHNameQuote body -> "ETHNameQuote" <+> parens (docExpr body)
    ETHTypeNameQuote ty -> "ETHTypeNameQuote" <+> parens (docType ty)
    ETHSplice body -> "ETHSplice" <+> parens (docExpr body)
    ETHTypedSplice body -> "ETHTypedSplice" <+> parens (docExpr body)
    EIf cond yes no -> "EIf" <+> parens (docExpr cond) <+> parens (docExpr yes) <+> parens (docExpr no)
    EMultiWayIf rhss -> "EMultiWayIf" <+> brackets (hsep (punctuate comma (map docGuardedRhs rhss)))
    ELambdaPats pats body -> "ELambdaPats" <+> brackets (hsep (punctuate comma (map docPattern pats))) <+> parens (docExpr body)
    ELambdaCase alts -> "ELambdaCase" <+> brackets (hsep (punctuate comma (map docCaseAlt alts)))
    ELambdaCases alts -> "ELambdaCases" <+> brackets (hsep (punctuate comma (map docLambdaCaseAlt alts)))
    EInfix lhs op rhs -> "EInfix" <+> parens (docExpr lhs) <+> docName op <+> parens (docExpr rhs)
    ENegate inner -> "ENegate" <+> parens (docExpr inner)
    ESectionL lhs op -> "ESectionL" <+> parens (docExpr lhs) <+> docName op
    ESectionR op rhs -> "ESectionR" <+> docName op <+> parens (docExpr rhs)
    ELetDecls decls body -> "ELetDecls" <+> brackets (hsep (punctuate comma (map docDecl decls))) <+> parens (docExpr body)
    ECase scrutinee alts -> "ECase" <+> parens (docExpr scrutinee) <+> brackets (hsep (punctuate comma (map docCaseAlt alts)))
    EDo stmts isMdo -> (if isMdo then "EMdo" else "EDo") <+> brackets (hsep (punctuate comma (map docDoStmt stmts)))
    EListComp body quals -> "EListComp" <+> parens (docExpr body) <+> brackets (hsep (punctuate comma (map docCompStmt quals)))
    EListCompParallel body qualGroups -> "EListCompParallel" <+> parens (docExpr body) <+> brackets (hsep (punctuate "|" [brackets (hsep (punctuate comma (map docCompStmt qs))) | qs <- qualGroups]))
    EArithSeq seqInfo -> "EArithSeq" <+> parens (docArithSeq seqInfo)
    ERecordCon name fields' hasWildcard -> "ERecordCon" <+> docText name <+> braces (hsep (punctuate comma ([docText fn <+> "=" <+> docExpr fv | (fn, fv) <- fields'] ++ [".." | hasWildcard])))
    ERecordUpd base fields' -> "ERecordUpd" <+> parens (docExpr base) <+> braces (hsep (punctuate comma [docText fn <+> "=" <+> docExpr fv | (fn, fv) <- fields']))
    ETypeSig inner ty -> "ETypeSig" <+> parens (docExpr inner) <+> parens (docType ty)
    EParen inner -> "EParen" <+> parens (docExpr inner)
    EList elems -> "EList" <+> brackets (hsep (punctuate comma (map docExpr elems)))
    ETuple tupleFlavor elems ->
      (if tupleFlavor == Boxed then "ETuple" else "ETupleUnboxed")
        <+> brackets (hsep (punctuate comma (map (maybe "_" docExpr) elems)))
    EUnboxedSum altIdx arity inner ->
      "EUnboxedSum" <+> pretty altIdx <+> pretty arity <+> docExpr inner
    ETypeApp inner ty -> "ETypeApp" <+> parens (docExpr inner) <+> parens (docType ty)
    EApp f x -> "EApp" <+> parens (docExpr f) <+> parens (docExpr x)
    EProc pat body -> "EProc" <+> parens (docPattern pat) <+> parens (docCmd body)
    EAnn _ sub -> docExpr sub

docCaseAlt :: CaseAlt -> Doc ann
docCaseAlt (CaseAlt _ pat rhs) =
  "CaseAlt" <+> parens (docPattern pat) <+> parens (docRhs rhs)

docLambdaCaseAlt :: LambdaCaseAlt -> Doc ann
docLambdaCaseAlt (LambdaCaseAlt _ pats rhs) =
  "LambdaCaseAlt" <+> brackets (hsep (punctuate comma (map docPattern pats))) <+> parens (docRhs rhs)

docDoStmt :: DoStmt Expr -> Doc ann
docDoStmt stmt =
  case stmt of
    DoAnn _ inner -> docDoStmt inner
    DoBind pat expr -> "DoBind" <+> parens (docPattern pat) <+> parens (docExpr expr)
    DoLetDecls decls -> "DoLetDecls" <+> brackets (hsep (punctuate comma (map docDecl decls)))
    DoExpr expr -> "DoExpr" <+> parens (docExpr expr)
    DoRecStmt stmts -> "DoRecStmt" <+> brackets (hsep (punctuate comma (map docDoStmt stmts)))

docCompStmt :: CompStmt -> Doc ann
docCompStmt stmt =
  case stmt of
    CompAnn _ inner -> docCompStmt inner
    CompGen pat expr -> "CompGen" <+> parens (docPattern pat) <+> parens (docExpr expr)
    CompGuard expr -> "CompGuard" <+> parens (docExpr expr)
    CompLetDecls decls -> "CompLetDecls" <+> brackets (hsep (punctuate comma (map docDecl decls)))

docCmd :: Cmd -> Doc ann
docCmd cmd =
  case cmd of
    CmdAnn _ inner -> docCmd inner
    CmdArrApp lhs HsFirstOrderApp rhs -> "CmdArrApp" <+> parens (docExpr lhs) <+> "HsFirstOrderApp" <+> parens (docExpr rhs)
    CmdArrApp lhs HsHigherOrderApp rhs -> "CmdArrApp" <+> parens (docExpr lhs) <+> "HsHigherOrderApp" <+> parens (docExpr rhs)
    CmdInfix l op r -> "CmdInfix" <+> parens (docCmd l) <+> docName op <+> parens (docCmd r)
    CmdDo stmts -> "CmdDo" <+> brackets (hsep (punctuate comma (map docCmdStmt stmts)))
    CmdIf cond yes no -> "CmdIf" <+> parens (docExpr cond) <+> parens (docCmd yes) <+> parens (docCmd no)
    CmdCase scrut alts -> "CmdCase" <+> parens (docExpr scrut) <+> brackets (hsep (punctuate comma (map docCmdCaseAlt alts)))
    CmdLet decls body -> "CmdLet" <+> brackets (hsep (punctuate comma (map docDecl decls))) <+> parens (docCmd body)
    CmdLam pats body -> "CmdLam" <+> brackets (hsep (punctuate comma (map docPattern pats))) <+> parens (docCmd body)
    CmdApp c e -> "CmdApp" <+> parens (docCmd c) <+> parens (docExpr e)
    CmdPar c -> "CmdPar" <+> parens (docCmd c)

docCmdStmt :: DoStmt Cmd -> Doc ann
docCmdStmt stmt =
  case stmt of
    DoAnn _ inner -> docCmdStmt inner
    DoBind pat cmd' -> "DoBind" <+> parens (docPattern pat) <+> parens (docCmd cmd')
    DoLetDecls decls -> "DoLetDecls" <+> brackets (hsep (punctuate comma (map docDecl decls)))
    DoExpr cmd' -> "DoExpr" <+> parens (docCmd cmd')
    DoRecStmt stmts -> "DoRecStmt" <+> brackets (hsep (punctuate comma (map docCmdStmt stmts)))

docCmdCaseAlt :: CmdCaseAlt -> Doc ann
docCmdCaseAlt alt =
  "CmdCaseAlt" <+> parens (docPattern (cmdCaseAltPat alt)) <+> parens (docCmd (cmdCaseAltBody alt))

docArithSeq :: ArithSeq -> Doc ann
docArithSeq seqInfo =
  case seqInfo of
    ArithSeqAnn _ inner -> docArithSeq inner
    ArithSeqFrom from -> "ArithSeqFrom" <+> parens (docExpr from)
    ArithSeqFromThen from thn -> "ArithSeqFromThen" <+> parens (docExpr from) <+> parens (docExpr thn)
    ArithSeqFromTo from to -> "ArithSeqFromTo" <+> parens (docExpr from) <+> parens (docExpr to)
    ArithSeqFromThenTo from thn to -> "ArithSeqFromThenTo" <+> parens (docExpr from) <+> parens (docExpr thn) <+> parens (docExpr to)

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
    TkKeywordForall -> "TkKeywordForall"
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
    TkKeywordProc -> "TkKeywordProc"
    TkKeywordPattern -> "TkKeywordPattern"
    TkKeywordRec -> "TkKeywordRec"
    TkKeywordMdo -> "TkKeywordMdo"
    TkArrowTail -> "TkArrowTail"
    TkArrowTailReverse -> "TkArrowTailReverse"
    TkDoubleArrowTail -> "TkDoubleArrowTail"
    TkDoubleArrowTailReverse -> "TkDoubleArrowTailReverse"
    TkBananaOpen -> "TkBananaOpen"
    TkBananaClose -> "TkBananaClose"
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
    TkTypeApp -> "TkTypeApp"
    TkVarId name -> "TkVarId" <+> docText name
    TkConId name -> "TkConId" <+> docText name
    TkQVarId modName name -> "TkQVarId" <+> docText modName <+> docText name
    TkQConId modName name -> "TkQConId" <+> docText modName <+> docText name
    TkImplicitParam name -> "TkImplicitParam" <+> docText name
    TkVarSym name -> "TkVarSym" <+> docText name
    TkConSym name -> "TkConSym" <+> docText name
    TkQVarSym modName name -> "TkQVarSym" <+> docText modName <+> docText name
    TkQConSym modName name -> "TkQConSym" <+> docText modName <+> docText name
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
    TkOverloadedLabel label raw -> "TkOverloadedLabel" <+> docText label <+> docText raw
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
    TkPragma pragma' ->
      case pragma' of
        PragmaLanguage settings -> "TkPragma" <+> ("PragmaLanguage" <+> brackets (hsep (punctuate comma (map docExtensionSetting settings))))
        PragmaInstanceOverlap overlapPragma -> "TkPragma" <+> ("PragmaInstanceOverlap" <+> docInstanceOverlapPragma overlapPragma)
        PragmaWarning msg -> "TkPragma" <+> ("PragmaWarning" <+> docText msg)
        PragmaDeprecated msg -> "TkPragma" <+> ("PragmaDeprecated" <+> docText msg)
        PragmaInline inlineKind body -> "TkPragma" <+> ("PragmaInline" <+> docText inlineKind <+> docText body)
        PragmaUnpack unpackKind -> "TkPragma" <+> ("PragmaUnpack" <+> docPragmaUnpackKind unpackKind)
        PragmaSource sourceText _ -> "TkPragma" <+> ("PragmaSource" <+> docText sourceText)
        PragmaUnknown text -> "TkPragma" <+> ("PragmaUnknown" <+> docText text)
    TkQuasiQuote quoter body -> "TkQuasiQuote" <+> docText quoter <+> docText body
    TkTHExpQuoteOpen -> "TkTHExpQuoteOpen"
    TkTHExpQuoteClose -> "TkTHExpQuoteClose"
    TkTHTypedQuoteOpen -> "TkTHTypedQuoteOpen"
    TkTHTypedQuoteClose -> "TkTHTypedQuoteClose"
    TkTHDeclQuoteOpen -> "TkTHDeclQuoteOpen"
    TkTHTypeQuoteOpen -> "TkTHTypeQuoteOpen"
    TkTHPatQuoteOpen -> "TkTHPatQuoteOpen"
    TkTHQuoteTick -> "TkTHQuoteTick"
    TkTHTypeQuoteTick -> "TkTHTypeQuoteTick"
    TkTHSplice -> "TkTHSplice"
    TkTHTypedSplice -> "TkTHTypedSplice"
    TkError msg -> "TkError" <+> docText msg
    TkEOF -> "TkEOF"

docTypeSyntaxForm :: TypeSyntaxForm -> Doc ann
docTypeSyntaxForm form =
  case form of
    TypeSyntaxExplicitNamespace -> "TypeSyntaxExplicitNamespace"
    TypeSyntaxInTerm -> "TypeSyntaxInTerm"

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

docName :: Name -> Doc ann
docName = docText . renderName

docUnqualifiedName :: UnqualifiedName -> Doc ann
docUnqualifiedName = docText . renderUnqualifiedName

docTextList :: [Text] -> Doc ann
docTextList ts = brackets (hsep (punctuate comma (map docText ts)))

docTypeFamilyDecl :: TypeFamilyDecl -> Doc ann
docTypeFamilyDecl tf =
  "TypeFamilyDecl" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      [field "headForm" (docTypeHeadForm (typeFamilyDeclHeadForm tf)), field "head" (docType (typeFamilyDeclHead tf))]
        <> listField "params" docTyVarBinder (typeFamilyDeclParams tf)
        <> optionalField "resultSig" docTypeFamilyResultSig (typeFamilyDeclResultSig tf)
        <> case typeFamilyDeclEquations tf of
          Nothing -> []
          Just eqs -> [field "equations" (brackets (hsep (punctuate comma (map docTypeFamilyEq eqs))))]

docTypeFamilyEq :: TypeFamilyEq -> Doc ann
docTypeFamilyEq eq =
  "TypeFamilyEq" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      listField "forall" docTyVarBinder (typeFamilyEqForall eq)
        <> [field "headForm" (docTypeHeadForm (typeFamilyEqHeadForm eq)), field "lhs" (docType (typeFamilyEqLhs eq)), field "rhs" (docType (typeFamilyEqRhs eq))]

docDataFamilyDecl :: DataFamilyDecl -> Doc ann
docDataFamilyDecl df =
  "DataFamilyDecl" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      [field "name" (docUnqualifiedName (dataFamilyDeclName df))]
        <> listField "params" docTyVarBinder (dataFamilyDeclParams df)
        <> optionalField "kind" docType (dataFamilyDeclKind df)

docTypeFamilyInst :: TypeFamilyInst -> Doc ann
docTypeFamilyInst tfi =
  "TypeFamilyInst" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      listField "forall" docTyVarBinder (typeFamilyInstForall tfi)
        <> [field "headForm" (docTypeHeadForm (typeFamilyInstHeadForm tfi)), field "lhs" (docType (typeFamilyInstLhs tfi)), field "rhs" (docType (typeFamilyInstRhs tfi))]

docTypeHeadForm :: TypeHeadForm -> Doc ann
docTypeHeadForm headForm =
  case headForm of
    TypeHeadPrefix -> "TypeHeadPrefix"
    TypeHeadInfix -> "TypeHeadInfix"

docDataFamilyInst :: DataFamilyInst -> Doc ann
docDataFamilyInst dfi =
  "DataFamilyInst" <+> braces (hsep (punctuate comma fields))
  where
    fields =
      boolField "isNewtype" (dataFamilyInstIsNewtype dfi)
        <> listField "forall" docTyVarBinder (dataFamilyInstForall dfi)
        <> [field "head" (docType (dataFamilyInstHead dfi))]
        <> optionalField "kind" docType (dataFamilyInstKind dfi)
        <> listField "constructors" docDataConDecl (dataFamilyInstConstructors dfi)
        <> listField "deriving" docDerivingClause (dataFamilyInstDeriving dfi)
