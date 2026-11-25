{-# LANGUAGE OverloadedStrings #-}

-- | Copy a parser syntax tree into the resolved syntax tree with no
-- resolution.
--
-- The resolver uses this copy for syntax that it does not resolve, such as
-- a fixity declaration or the body of a quasi-quote. The copy keeps each
-- annotation of the parser tree, and each name gets no resolution.
--
-- The body of a declaration or pattern quote stays unresolved, but each
-- splice in the body is an expression at the level of the quote. The
-- 'SpliceHooks' tell the copy what to do with such a splice. The copy does
-- not go into a nested quote with the hooks, because the splices of a
-- nested quote are at a different level.
module Aihc.Resolve.Syntax.FromParser
  ( -- * Splice hooks
    SpliceHooks (..),
    plainHooks,

    -- * Copies with hooks
    convertExpr,
    convertDecl,
    convertPattern,
    convertType,

    -- * Plain copies
    fromParserName,
    fromParserUnqualifiedName,
    fromParserModuleHead,
    fromParserImportDecl,
    fromParserImportItem,
    fromParserExportSpec,
    fromParserIEBundledMember,
    fromParserDecl,
    fromParserExpr,
    fromParserPattern,
    fromParserType,
    fromParserTyVarBinder,
    fromParserTypeFamilyInjectivity,
    fromParserDataConDecl,
    fromParserBangType,
    fromParserFieldDecl,
    fromParserClassDeclItem,
    fromParserInstanceDeclItem,
    fromParserRecordField,
    fromParserArrowKind,
    fromParserBinderHead,
    fromParserMultiplicityTag,
    fromParserGuardQualifier,
    fromParserPragmaDecl,
  )
where

import Aihc.Parser.Syntax qualified as P
import Aihc.Resolve.Syntax
import Data.Functor.Identity (Identity (..))
import Data.Text (Text)
import Data.Text qualified as T

-- | What the copy does with the body of a splice.
data SpliceHooks f = SpliceHooks
  { -- | The body of an expression splice @$e@.
    hookExprSplice :: P.Expr -> f Expr,
    -- | The body of a type splice.
    hookTypeSplice :: P.Expr -> f Expr,
    -- | The body of a pattern splice.
    hookPatternSplice :: P.Expr -> f Expr,
    -- | The body of a declaration splice.
    hookDeclSplice :: P.Expr -> f Expr
  }

-- | The hooks that copy a splice body as other syntax.
plainHooks :: SpliceHooks Identity
plainHooks =
  SpliceHooks
    { hookExprSplice = convertExpr plainHooks,
      hookTypeSplice = convertExpr plainHooks,
      hookPatternSplice = convertExpr plainHooks,
      hookDeclSplice = convertExpr plainHooks
    }

plain :: (SpliceHooks Identity -> a -> Identity b) -> a -> b
plain convert = runIdentity . convert plainHooks

fromParserName :: P.Name -> Name
fromParserName (P.Name qualifier ty txt anns) = Name qualifier ty txt anns Nothing

fromParserUnqualifiedName :: P.UnqualifiedName -> UnqualifiedName
fromParserUnqualifiedName (P.UnqualifiedName ty txt anns) = UnqualifiedName ty txt anns Nothing

fromParserModuleHead :: P.ModuleHead -> ModuleHead
fromParserModuleHead head' =
  ModuleHead
    { moduleHeadAnns = P.moduleHeadAnns head',
      moduleHeadName = P.moduleHeadName head',
      moduleHeadWarningPragma = P.moduleHeadWarningPragma head',
      moduleHeadExports = map fromParserExportSpec <$> P.moduleHeadExports head'
    }

fromParserIEBundledMember :: P.IEBundledMember -> IEBundledMember
fromParserIEBundledMember (P.IEBundledMember namespace name) =
  IEBundledMember namespace (fromParserName name)

fromParserExportSpec :: P.ExportSpec -> ExportSpec
fromParserExportSpec spec =
  case spec of
    P.ExportModule pragma name -> ExportModule pragma name
    P.ExportVar pragma namespace name -> ExportVar pragma namespace (fromParserName name)
    P.ExportAbs pragma namespace name -> ExportAbs pragma namespace (fromParserName name)
    P.ExportAll pragma namespace name -> ExportAll pragma namespace (fromParserName name)
    P.ExportWith pragma namespace name members ->
      ExportWith pragma namespace (fromParserName name) (map fromParserIEBundledMember members)
    P.ExportWithAll pragma namespace name index members ->
      ExportWithAll pragma namespace (fromParserName name) index (map fromParserIEBundledMember members)
    P.ExportAnn ann inner -> ExportAnn ann (fromParserExportSpec inner)

fromParserImportDecl :: P.ImportDecl -> ImportDecl
fromParserImportDecl decl =
  ImportDecl
    { importDeclAnns = P.importDeclAnns decl,
      importDeclError = Nothing,
      importDeclLevel = P.importDeclLevel decl,
      importDeclPackage = P.importDeclPackage decl,
      importDeclSourcePragma = P.importDeclSourcePragma decl,
      importDeclSafe = P.importDeclSafe decl,
      importDeclQualified = P.importDeclQualified decl,
      importDeclQualifiedPost = P.importDeclQualifiedPost decl,
      importDeclModule = P.importDeclModule decl,
      importDeclAs = P.importDeclAs decl,
      importDeclSpec = fromParserImportSpec <$> P.importDeclSpec decl
    }

fromParserImportSpec :: P.ImportSpec -> ImportSpec
fromParserImportSpec spec =
  ImportSpec
    { importSpecAnns = P.importSpecAnns spec,
      importSpecHiding = P.importSpecHiding spec,
      importSpecItems = map fromParserImportItem (P.importSpecItems spec)
    }

fromParserImportItem :: P.ImportItem -> ImportItem
fromParserImportItem item =
  case item of
    P.ImportItemVar namespace name -> ImportItemVar namespace (fromParserUnqualifiedName name)
    P.ImportItemAbs namespace name -> ImportItemAbs namespace (fromParserUnqualifiedName name)
    P.ImportItemAll namespace name -> ImportItemAll namespace (fromParserUnqualifiedName name)
    P.ImportItemWith namespace name members ->
      ImportItemWith namespace (fromParserUnqualifiedName name) (map fromParserIEBundledMember members)
    P.ImportItemAllWith namespace name index members ->
      ImportItemAllWith namespace (fromParserUnqualifiedName name) index (map fromParserIEBundledMember members)
    P.ImportAnn ann inner -> ImportAnn ann (fromParserImportItem inner)

fromParserDecl :: P.Decl -> Decl
fromParserDecl = plain convertDecl

fromParserExpr :: P.Expr -> Expr
fromParserExpr = plain convertExpr

fromParserPattern :: P.Pattern -> Pattern
fromParserPattern = plain convertPattern

fromParserType :: P.Type -> Type
fromParserType = plain convertType

fromParserTyVarBinder :: P.TyVarBinder -> TyVarBinder
fromParserTyVarBinder = plain convertTyVarBinder

fromParserDataConDecl :: P.DataConDecl -> DataConDecl
fromParserDataConDecl = plain convertDataConDecl

fromParserBangType :: P.BangType -> BangType
fromParserBangType = plain convertBangType

fromParserFieldDecl :: P.FieldDecl -> FieldDecl
fromParserFieldDecl = plain convertFieldDecl

fromParserClassDeclItem :: P.ClassDeclItem -> ClassDeclItem
fromParserClassDeclItem = plain convertClassDeclItem

fromParserInstanceDeclItem :: P.InstanceDeclItem -> InstanceDeclItem
fromParserInstanceDeclItem = plain convertInstanceDeclItem

fromParserArrowKind :: P.ArrowKind -> ArrowKind
fromParserArrowKind = plain convertArrowKind

fromParserTypeFamilyInjectivity :: P.TypeFamilyInjectivity -> TypeFamilyInjectivity
fromParserTypeFamilyInjectivity = id

fromParserRecordField :: (a -> b) -> P.RecordField a -> RecordField b
fromParserRecordField convertValue (P.RecordField name value pun) =
  RecordField (fromParserName name) (convertValue value) pun

fromParserBinderHead :: P.BinderHead P.UnqualifiedName -> BinderHead UnqualifiedName
fromParserBinderHead = plain convertBinderHead

fromParserMultiplicityTag :: P.MultiplicityTag -> MultiplicityTag
fromParserMultiplicityTag = plain convertMultiplicityTag

fromParserGuardQualifier :: P.GuardQualifier -> GuardQualifier
fromParserGuardQualifier = plain convertGuardQualifier

-- | A pragma declaration. It has no syntax to copy.
fromParserPragmaDecl :: P.Pragma -> Decl
fromParserPragmaDecl = DeclPragma

convertDecl :: (Applicative f) => SpliceHooks f -> P.Decl -> f Decl
convertDecl hooks decl =
  case decl of
    P.DeclAnn ann inner -> DeclAnn ann <$> convertDecl hooks inner
    P.DeclValue valueDecl -> DeclValue <$> convertValueDecl hooks valueDecl
    P.DeclImplicitParam name expr mDecls ->
      DeclImplicitParam name <$> convertExpr hooks expr <*> traverse (traverse (convertDecl hooks)) mDecls
    P.DeclTypeSig names ty -> DeclTypeSig (map fromParserUnqualifiedName names) <$> convertType hooks ty
    P.DeclPatSyn patSyn -> DeclPatSyn <$> convertPatSynDecl hooks patSyn
    P.DeclPatSynSig names ty -> DeclPatSynSig (map fromParserUnqualifiedName names) <$> convertType hooks ty
    P.DeclStandaloneKindSig name ty -> DeclStandaloneKindSig (fromParserUnqualifiedName name) <$> convertType hooks ty
    P.DeclFixity assoc namespace precedence ops ->
      pure (DeclFixity assoc namespace precedence (map fromParserUnqualifiedName ops))
    P.DeclRoleAnnotation (P.RoleAnnotation name roles) ->
      pure (DeclRoleAnnotation (RoleAnnotation (fromParserUnqualifiedName name) roles))
    P.DeclTypeSyn typeSyn -> DeclTypeSyn <$> convertTypeSynDecl hooks typeSyn
    P.DeclTypeData dataDecl -> DeclTypeData <$> convertDataDecl hooks dataDecl
    P.DeclData dataDecl -> DeclData <$> convertDataDecl hooks dataDecl
    P.DeclNewtype newtypeDecl -> DeclNewtype <$> convertNewtypeDecl hooks newtypeDecl
    P.DeclClass classDecl -> DeclClass <$> convertClassDecl hooks classDecl
    P.DeclInstance instanceDecl -> DeclInstance <$> convertInstanceDecl hooks instanceDecl
    P.DeclStandaloneDeriving derivingDecl -> DeclStandaloneDeriving <$> convertStandaloneDerivingDecl hooks derivingDecl
    P.DeclDefault tys -> DeclDefault <$> traverse (convertType hooks) tys
    P.DeclSplice expr -> DeclSplice <$> hookDeclSplice hooks expr
    P.DeclForeign foreignDecl -> DeclForeign <$> convertForeignDecl hooks foreignDecl
    P.DeclTypeFamilyDecl familyDecl -> DeclTypeFamilyDecl <$> convertTypeFamilyDecl hooks familyDecl
    P.DeclDataFamilyDecl familyDecl -> DeclDataFamilyDecl <$> convertDataFamilyDecl hooks familyDecl
    P.DeclTypeFamilyInst familyInst -> DeclTypeFamilyInst <$> convertTypeFamilyInst hooks familyInst
    P.DeclDataFamilyInst familyInst -> DeclDataFamilyInst <$> convertDataFamilyInst hooks familyInst
    P.DeclPragma pragma -> pure (DeclPragma pragma)
    P.DeclRules rules -> DeclRules <$> traverse (convertRuleDecl hooks) rules

convertRuleDecl :: (Applicative f) => SpliceHooks f -> P.RuleDecl -> f RuleDecl
convertRuleDecl hooks rule =
  RuleDecl (P.ruleAnns rule) Nothing (P.ruleName rule) (P.ruleActivation rule)
    <$> traverse (convertTyVarBinder hooks) (P.ruleTypeBinders rule)
    <*> traverse (convertRuleBinder hooks) (P.ruleBinders rule)
    <*> convertExpr hooks (P.ruleLhs rule)
    <*> convertExpr hooks (P.ruleRhs rule)

convertRuleBinder :: (Applicative f) => SpliceHooks f -> P.RuleBinder -> f RuleBinder
convertRuleBinder hooks binder =
  RuleBinder (P.ruleBinderAnns binder) (fromParserUnqualifiedName (P.ruleBinderName binder))
    <$> traverse (convertType hooks) (P.ruleBinderType binder)

convertMultiplicityTag :: (Applicative f) => SpliceHooks f -> P.MultiplicityTag -> f MultiplicityTag
convertMultiplicityTag hooks tag =
  case tag of
    P.NoMultiplicityTag -> pure NoMultiplicityTag
    P.LinearMultiplicityTag -> pure LinearMultiplicityTag
    P.ExplicitMultiplicityTag ty -> ExplicitMultiplicityTag <$> convertType hooks ty

convertValueDecl :: (Applicative f) => SpliceHooks f -> P.ValueDecl -> f ValueDecl
convertValueDecl hooks valueDecl =
  case valueDecl of
    P.FunctionBind name matches -> FunctionBind (fromParserUnqualifiedName name) <$> traverse (convertMatch hooks) matches
    P.PatternBind tag pat rhs ->
      PatternBind <$> convertMultiplicityTag hooks tag <*> convertPattern hooks pat <*> convertRhs hooks (convertExpr hooks) rhs

convertMatch :: (Applicative f) => SpliceHooks f -> P.Match -> f Match
convertMatch hooks match =
  Match (P.matchAnns match) (P.matchHeadForm match)
    <$> traverse (convertPattern hooks) (P.matchPats match)
    <*> convertRhs hooks (convertExpr hooks) (P.matchRhs match)

convertPatSynDecl :: (Applicative f) => SpliceHooks f -> P.PatSynDecl -> f PatSynDecl
convertPatSynDecl hooks patSyn =
  PatSynDecl (fromParserUnqualifiedName (P.patSynDeclName patSyn)) (P.patSynDeclArgs patSyn)
    <$> convertPattern hooks (P.patSynDeclPat patSyn)
    <*> convertPatSynDir hooks (P.patSynDeclDir patSyn)

convertPatSynDir :: (Applicative f) => SpliceHooks f -> P.PatSynDir -> f PatSynDir
convertPatSynDir hooks dir =
  case dir of
    P.PatSynUnidirectional -> pure PatSynUnidirectional
    P.PatSynBidirectional -> pure PatSynBidirectional
    P.PatSynExplicitBidirectional matches -> PatSynExplicitBidirectional <$> traverse (convertMatch hooks) matches

convertRhs :: (Applicative f) => SpliceHooks f -> (a -> f b) -> P.Rhs a -> f (Rhs b)
convertRhs hooks convertBody rhs =
  case rhs of
    P.UnguardedRhs anns body mDecls ->
      UnguardedRhs anns <$> convertBody body <*> traverse (traverse (convertDecl hooks)) mDecls
    P.GuardedRhss anns guarded mDecls ->
      GuardedRhss anns <$> traverse (convertGuardedRhs hooks convertBody) guarded <*> traverse (traverse (convertDecl hooks)) mDecls

convertGuardedRhs :: (Applicative f) => SpliceHooks f -> (a -> f b) -> P.GuardedRhs a -> f (GuardedRhs b)
convertGuardedRhs hooks convertBody guarded =
  GuardedRhs (P.guardedRhsAnns guarded)
    <$> traverse (convertGuardQualifier hooks) (P.guardedRhsGuards guarded)
    <*> convertBody (P.guardedRhsBody guarded)

convertGuardQualifier :: (Applicative f) => SpliceHooks f -> P.GuardQualifier -> f GuardQualifier
convertGuardQualifier hooks qualifier =
  case qualifier of
    P.GuardAnn ann inner -> GuardAnn ann <$> convertGuardQualifier hooks inner
    P.GuardExpr expr -> GuardExpr <$> convertExpr hooks expr
    P.GuardPat pat expr -> GuardPat <$> convertPattern hooks pat <*> convertExpr hooks expr
    P.GuardLet decls -> GuardLet <$> traverse (convertDecl hooks) decls

convertRecordField :: (Applicative f) => (a -> f b) -> P.RecordField a -> f (RecordField b)
convertRecordField convertValue (P.RecordField name value pun) =
  (\value' -> RecordField (fromParserName name) value' pun) <$> convertValue value

convertPattern :: (Applicative f) => SpliceHooks f -> P.Pattern -> f Pattern
convertPattern hooks pat =
  case pat of
    P.PAnn ann inner -> PAnn ann <$> convertPattern hooks inner
    P.PVar name -> pure (PVar (fromParserUnqualifiedName name))
    P.PTypeBinder binder -> PTypeBinder <$> convertTyVarBinder hooks binder
    P.PTypeSyntax form ty -> PTypeSyntax form <$> convertType hooks ty
    P.PWildcard -> pure PWildcard
    P.PLit lit -> pure (PLit (copiedLiteralMatch False lit) lit)
    P.PQuasiQuote quoter body -> pure (PQuasiQuote quoter body)
    P.PTuple flavor pats -> PTuple flavor <$> traverse (convertPattern hooks) pats
    P.PUnboxedSum alt arity inner -> PUnboxedSum alt arity <$> convertPattern hooks inner
    P.PList pats -> PList Nothing <$> traverse (convertPattern hooks) pats
    P.PCon name tys pats -> PCon (fromParserName name) <$> traverse (convertType hooks) tys <*> traverse (convertPattern hooks) pats
    P.PBuiltinCon builtin tys pats -> PBuiltinCon builtin <$> traverse (convertType hooks) tys <*> traverse (convertPattern hooks) pats
    P.PInfix left op right -> (\left' right' -> PInfix left' (fromParserName op) right') <$> convertPattern hooks left <*> convertPattern hooks right
    P.PView expr inner -> PView <$> convertExpr hooks expr <*> convertPattern hooks inner
    P.PAs name inner -> PAs (fromParserUnqualifiedName name) <$> convertPattern hooks inner
    P.PStrict inner -> PStrict <$> convertPattern hooks inner
    P.PIrrefutable inner -> PIrrefutable <$> convertPattern hooks inner
    P.PNegLit lit -> pure (PNegLit (copiedLiteralMatch True lit) lit)
    P.PNPlusK name lit -> pure (PNPlusK copiedNPlusKMatch (fromParserUnqualifiedName name) lit)
    P.PParen inner -> PParen <$> convertPattern hooks inner
    P.PRecord name fields wildcard ->
      (\fields' -> PRecord (fromParserName name) fields' wildcard) <$> traverse (convertRecordField (convertPattern hooks)) fields
    P.PTypeSig inner ty -> PTypeSig <$> convertPattern hooks inner <*> convertType hooks ty
    P.PSplice expr -> PSplice <$> hookPatternSplice hooks expr

-- | The match names of a copied literal pattern. The names get no
-- resolution. The copy does not know the extensions of the module. Thus, a
-- string literal gets the match that it has without @OverloadedStrings@.
copiedLiteralMatch :: Bool -> Literal -> LiteralMatch
copiedLiteralMatch negated lit =
  case primitiveLiteralTypeName lit of
    Just typeName -> LiteralMatchPrimitive (copiedName NameConId typeName)
    Nothing ->
      case peelLiteralAnn lit of
        LitInt _ TInteger _ -> overloaded (Just (copiedName NameConId "Integer")) "fromInteger"
        LitFloat _ TFractional _ -> overloaded Nothing "fromRational"
        _ -> LiteralMatchFixed
  where
    overloaded literalType conversion =
      LiteralMatchOverloaded
        OverloadedLiteralMatch
          { overloadedLiteralType = literalType,
            overloadedLiteralConversion = copiedName NameVarId conversion,
            overloadedLiteralNegate = if negated then Just (copiedName NameVarId "negate") else Nothing,
            overloadedLiteralEquality = copiedName NameVarSym "=="
          }

-- | The names of a copied numeric literal expression, with no resolution.
-- A literal with a primitive type names that type. Else the literal is
-- overloaded.
copiedLiteralSyntax :: Maybe Text -> Maybe Name -> Text -> LiteralSyntax
copiedLiteralSyntax primitiveType literalType conversion =
  case primitiveType of
    Just typeName -> LiteralSyntaxPrimitive (copiedName NameConId typeName)
    Nothing -> LiteralSyntaxOverloaded literalType (copiedName NameVarId conversion)

-- | The text of the tuple syntax name, such as @(,)@ or @(#,#)@.
tupleSyntaxText :: TupleFlavor -> Int -> Text
tupleSyntaxText flavor arity =
  case flavor of
    Boxed -> "(" <> commas <> ")"
    Unboxed -> "(#" <> commas <> "#)"
  where
    commas = T.replicate (max 0 (arity - 1)) ","

-- | The match names of a copied n+k pattern, with no resolution.
copiedNPlusKMatch :: NPlusKMatch
copiedNPlusKMatch =
  NPlusKMatch
    { nPlusKType = Just (copiedName NameConId "Integer"),
      nPlusKFromInteger = copiedName NameVarId "fromInteger",
      nPlusKGreaterEqual = copiedName NameVarSym ">=",
      nPlusKMinus = copiedName NameVarSym "-"
    }

copiedName :: NameType -> Text -> Name
copiedName kind text = Name Nothing kind text [] Nothing

convertForallTelescope :: (Applicative f) => SpliceHooks f -> P.ForallTelescope -> f ForallTelescope
convertForallTelescope hooks (P.ForallTelescope visibility binders) =
  ForallTelescope visibility <$> traverse (convertTyVarBinder hooks) binders

convertArrowKind :: (Applicative f) => SpliceHooks f -> P.ArrowKind -> f ArrowKind
convertArrowKind hooks arrowKind =
  case arrowKind of
    P.ArrowUnrestricted -> pure ArrowUnrestricted
    P.ArrowLinear -> pure ArrowLinear
    P.ArrowExplicit ty -> ArrowExplicit <$> convertType hooks ty

convertType :: (Applicative f) => SpliceHooks f -> P.Type -> f Type
convertType hooks ty =
  case ty of
    P.TAnn ann inner -> TAnn ann <$> convertType hooks inner
    P.TVar name -> pure (TVar (fromParserUnqualifiedName name))
    P.TCon name promotion -> pure (TCon (fromParserName name) promotion)
    P.TBuiltinCon builtin promotion -> pure (TBuiltinCon builtin promotion)
    P.TImplicitParam name inner -> TImplicitParam name <$> convertType hooks inner
    P.TTypeLit lit -> pure (TTypeLit lit)
    P.TStar text -> pure (TStar text)
    P.TQuasiQuote quoter body -> pure (TQuasiQuote quoter body)
    P.TForall telescope inner -> TForall <$> convertForallTelescope hooks telescope <*> convertType hooks inner
    P.TApp left right -> TApp <$> convertType hooks left <*> convertType hooks right
    P.TTypeApp left right -> TTypeApp <$> convertType hooks left <*> convertType hooks right
    P.TInfix left name promotion right ->
      (\left' right' -> TInfix left' (fromParserName name) promotion right') <$> convertType hooks left <*> convertType hooks right
    P.TFun arrowKind left right -> TFun <$> convertArrowKind hooks arrowKind <*> convertType hooks left <*> convertType hooks right
    P.TTuple flavor promotion items -> TTuple flavor promotion <$> traverse (convertType hooks) items
    P.TUnboxedSum items -> TUnboxedSum <$> traverse (convertType hooks) items
    P.TList promotion items -> TList promotion <$> traverse (convertType hooks) items
    P.TParen inner -> TParen <$> convertType hooks inner
    P.TKindSig inner kind -> TKindSig <$> convertType hooks inner <*> convertType hooks kind
    P.TContext constraints inner -> TContext <$> traverse (convertType hooks) constraints <*> convertType hooks inner
    P.TSplice expr -> TSplice <$> hookTypeSplice hooks expr
    P.TWildcard -> pure TWildcard

convertTyVarBinder :: (Applicative f) => SpliceHooks f -> P.TyVarBinder -> f TyVarBinder
convertTyVarBinder hooks binder =
  (\kind -> TyVarBinder (P.tyVarBinderAnns binder) (P.tyVarBinderName binder) kind (P.tyVarBinderSpecificity binder) (P.tyVarBinderVisibility binder))
    <$> traverse (convertType hooks) (P.tyVarBinderKind binder)

convertBinderHead :: (Applicative f) => SpliceHooks f -> P.BinderHead P.UnqualifiedName -> f (BinderHead UnqualifiedName)
convertBinderHead hooks head' =
  case head' of
    P.PrefixBinderHead name params -> PrefixBinderHead (fromParserUnqualifiedName name) <$> traverse (convertTyVarBinder hooks) params
    P.InfixBinderHead left name right params ->
      (\left' right' params' -> InfixBinderHead left' (fromParserUnqualifiedName name) right' params')
        <$> convertTyVarBinder hooks left
        <*> convertTyVarBinder hooks right
        <*> traverse (convertTyVarBinder hooks) params

convertTypeSynDecl :: (Applicative f) => SpliceHooks f -> P.TypeSynDecl -> f TypeSynDecl
convertTypeSynDecl hooks (P.TypeSynDecl head' body) =
  TypeSynDecl <$> convertBinderHead hooks head' <*> convertType hooks body

convertTypeFamilyDecl :: (Applicative f) => SpliceHooks f -> P.TypeFamilyDecl -> f TypeFamilyDecl
convertTypeFamilyDecl hooks familyDecl =
  TypeFamilyDecl (P.typeFamilyDeclHeadForm familyDecl) (P.typeFamilyDeclExplicitFamilyKeyword familyDecl)
    <$> convertType hooks (P.typeFamilyDeclHead familyDecl)
    <*> traverse (convertTyVarBinder hooks) (P.typeFamilyDeclParams familyDecl)
    <*> traverse (convertTypeFamilyResultSig hooks) (P.typeFamilyDeclResultSig familyDecl)
    <*> traverse (traverse (convertTypeFamilyEq hooks)) (P.typeFamilyDeclEquations familyDecl)

convertTypeFamilyResultSig :: (Applicative f) => SpliceHooks f -> P.TypeFamilyResultSig -> f TypeFamilyResultSig
convertTypeFamilyResultSig hooks resultSig =
  case resultSig of
    P.TypeFamilyKindSig ty -> TypeFamilyKindSig <$> convertType hooks ty
    P.TypeFamilyTyVarSig binder -> TypeFamilyTyVarSig <$> convertTyVarBinder hooks binder
    P.TypeFamilyInjectiveSig binder injectivity -> (`TypeFamilyInjectiveSig` injectivity) <$> convertTyVarBinder hooks binder

convertTypeFamilyEq :: (Applicative f) => SpliceHooks f -> P.TypeFamilyEq -> f TypeFamilyEq
convertTypeFamilyEq hooks equation =
  (\forall' lhs rhs -> TypeFamilyEq (P.typeFamilyEqAnns equation) forall' (P.typeFamilyEqHeadForm equation) lhs rhs)
    <$> traverse (convertTyVarBinder hooks) (P.typeFamilyEqForall equation)
    <*> convertType hooks (P.typeFamilyEqLhs equation)
    <*> convertType hooks (P.typeFamilyEqRhs equation)

convertDataFamilyDecl :: (Applicative f) => SpliceHooks f -> P.DataFamilyDecl -> f DataFamilyDecl
convertDataFamilyDecl hooks (P.DataFamilyDecl head' kind) =
  DataFamilyDecl <$> convertBinderHead hooks head' <*> traverse (convertType hooks) kind

convertTypeFamilyInst :: (Applicative f) => SpliceHooks f -> P.TypeFamilyInst -> f TypeFamilyInst
convertTypeFamilyInst hooks familyInst =
  (\forall' lhs rhs -> TypeFamilyInst forall' (P.typeFamilyInstHeadForm familyInst) lhs rhs)
    <$> traverse (convertTyVarBinder hooks) (P.typeFamilyInstForall familyInst)
    <*> convertType hooks (P.typeFamilyInstLhs familyInst)
    <*> convertType hooks (P.typeFamilyInstRhs familyInst)

convertDataFamilyInst :: (Applicative f) => SpliceHooks f -> P.DataFamilyInst -> f DataFamilyInst
convertDataFamilyInst hooks familyInst =
  DataFamilyInst (P.dataFamilyInstIsNewtype familyInst)
    <$> traverse (convertTyVarBinder hooks) (P.dataFamilyInstForall familyInst)
    <*> convertType hooks (P.dataFamilyInstHead familyInst)
    <*> traverse (convertType hooks) (P.dataFamilyInstKind familyInst)
    <*> traverse (convertDataConDecl hooks) (P.dataFamilyInstConstructors familyInst)
    <*> traverse (convertDerivingClause hooks) (P.dataFamilyInstDeriving familyInst)

convertDataDecl :: (Applicative f) => SpliceHooks f -> P.DataDecl -> f DataDecl
convertDataDecl hooks dataDecl =
  DataDecl (P.dataDeclCTypePragma dataDecl)
    <$> convertBinderHead hooks (P.dataDeclHead dataDecl)
    <*> traverse (convertType hooks) (P.dataDeclContext dataDecl)
    <*> traverse (convertType hooks) (P.dataDeclKind dataDecl)
    <*> traverse (convertDataConDecl hooks) (P.dataDeclConstructors dataDecl)
    <*> traverse (convertDerivingClause hooks) (P.dataDeclDeriving dataDecl)

convertNewtypeDecl :: (Applicative f) => SpliceHooks f -> P.NewtypeDecl -> f NewtypeDecl
convertNewtypeDecl hooks newtypeDecl =
  NewtypeDecl (P.newtypeDeclCTypePragma newtypeDecl)
    <$> convertBinderHead hooks (P.newtypeDeclHead newtypeDecl)
    <*> traverse (convertType hooks) (P.newtypeDeclContext newtypeDecl)
    <*> traverse (convertType hooks) (P.newtypeDeclKind newtypeDecl)
    <*> traverse (convertDataConDecl hooks) (P.newtypeDeclConstructor newtypeDecl)
    <*> traverse (convertDerivingClause hooks) (P.newtypeDeclDeriving newtypeDecl)

convertDataConDecl :: (Applicative f) => SpliceHooks f -> P.DataConDecl -> f DataConDecl
convertDataConDecl hooks dataConDecl =
  case dataConDecl of
    P.DataConAnn ann inner -> DataConAnn ann <$> convertDataConDecl hooks inner
    P.PrefixCon forallVars context name bangTypes ->
      (\forallVars' context' bangTypes' -> PrefixCon forallVars' context' (fromParserUnqualifiedName name) bangTypes')
        <$> binders forallVars
        <*> types context
        <*> traverse (convertBangType hooks) bangTypes
    P.InfixCon forallVars context left name right ->
      (\forallVars' context' left' right' -> InfixCon forallVars' context' left' (fromParserUnqualifiedName name) right')
        <$> binders forallVars
        <*> types context
        <*> convertBangType hooks left
        <*> convertBangType hooks right
    P.RecordCon forallVars context name fields ->
      (\forallVars' context' fields' -> RecordCon forallVars' context' (fromParserUnqualifiedName name) fields')
        <$> binders forallVars
        <*> types context
        <*> traverse (convertFieldDecl hooks) fields
    P.GadtCon telescopes context names body ->
      (\telescopes' context' body' -> GadtCon telescopes' context' (map fromParserUnqualifiedName names) body')
        <$> traverse (convertForallTelescope hooks) telescopes
        <*> types context
        <*> convertGadtBody hooks body
    P.TupleCon forallVars context flavor fields ->
      (\forallVars' context' fields' -> TupleCon forallVars' context' flavor fields')
        <$> binders forallVars
        <*> types context
        <*> traverse (convertBangType hooks) fields
    P.UnboxedSumCon forallVars context position arity field ->
      (\forallVars' context' field' -> UnboxedSumCon forallVars' context' position arity field')
        <$> binders forallVars
        <*> types context
        <*> convertBangType hooks field
    P.ListCon forallVars context -> ListCon <$> binders forallVars <*> types context
  where
    binders = traverse (convertTyVarBinder hooks)
    types = traverse (convertType hooks)

convertGadtBody :: (Applicative f) => SpliceHooks f -> P.GadtBody -> f GadtBody
convertGadtBody hooks body =
  case body of
    P.GadtPrefixBody args result ->
      GadtPrefixBody
        <$> traverse (\(bangType', arrowKind) -> (,) <$> convertBangType hooks bangType' <*> convertArrowKind hooks arrowKind) args
        <*> convertType hooks result
    P.GadtRecordBody fields result -> GadtRecordBody <$> traverse (convertFieldDecl hooks) fields <*> convertType hooks result

convertBangType :: (Applicative f) => SpliceHooks f -> P.BangType -> f BangType
convertBangType hooks bangType' =
  BangType (P.bangAnns bangType') (P.bangPragmas bangType') (P.bangStrict bangType') (P.bangLazy bangType')
    <$> convertType hooks (P.bangType bangType')

convertFieldDecl :: (Applicative f) => SpliceHooks f -> P.FieldDecl -> f FieldDecl
convertFieldDecl hooks field =
  FieldDecl (P.fieldAnns field) (map fromParserUnqualifiedName (P.fieldNames field))
    <$> traverse (convertType hooks) (P.fieldMultiplicity field)
    <*> convertBangType hooks (P.fieldType field)

convertDerivingClause :: (Applicative f) => SpliceHooks f -> P.DerivingClause -> f DerivingClause
convertDerivingClause hooks clause =
  DerivingClause
    <$> traverse (convertDerivingStrategy hooks) (P.derivingStrategy clause)
    <*> either (pure . Left . fromParserName) (fmap Right . traverse (convertType hooks)) (P.derivingClasses clause)

convertDerivingStrategy :: (Applicative f) => SpliceHooks f -> P.DerivingStrategy -> f DerivingStrategy
convertDerivingStrategy hooks strategy =
  case strategy of
    P.DerivingStock -> pure DerivingStock
    P.DerivingNewtype -> pure DerivingNewtype
    P.DerivingAnyclass -> pure DerivingAnyclass
    P.DerivingVia ty -> DerivingVia <$> convertType hooks ty

convertStandaloneDerivingDecl :: (Applicative f) => SpliceHooks f -> P.StandaloneDerivingDecl -> f StandaloneDerivingDecl
convertStandaloneDerivingDecl hooks decl =
  (\strategy forall' context head' -> StandaloneDerivingDecl strategy (P.standaloneDerivingPragmas decl) (P.standaloneDerivingWarning decl) forall' context head')
    <$> traverse (convertDerivingStrategy hooks) (P.standaloneDerivingStrategy decl)
    <*> traverse (convertTyVarBinder hooks) (P.standaloneDerivingForall decl)
    <*> traverse (convertType hooks) (P.standaloneDerivingContext decl)
    <*> convertType hooks (P.standaloneDerivingHead decl)

convertClassDecl :: (Applicative f) => SpliceHooks f -> P.ClassDecl -> f ClassDecl
convertClassDecl hooks classDecl =
  (\context head' items -> ClassDecl context head' (P.classDeclFundeps classDecl) items)
    <$> traverse (traverse (convertType hooks)) (P.classDeclContext classDecl)
    <*> convertBinderHead hooks (P.classDeclHead classDecl)
    <*> traverse (convertClassDeclItem hooks) (P.classDeclItems classDecl)

convertClassDeclItem :: (Applicative f) => SpliceHooks f -> P.ClassDeclItem -> f ClassDeclItem
convertClassDeclItem hooks item =
  case item of
    P.ClassItemAnn ann inner -> ClassItemAnn ann <$> convertClassDeclItem hooks inner
    P.ClassItemTypeSig names ty -> ClassItemTypeSig (map fromParserUnqualifiedName names) <$> convertType hooks ty
    P.ClassItemDefaultSig name ty -> ClassItemDefaultSig (fromParserUnqualifiedName name) <$> convertType hooks ty
    P.ClassItemFixity assoc namespace precedence ops ->
      pure (ClassItemFixity assoc namespace precedence (map fromParserUnqualifiedName ops))
    P.ClassItemDefault valueDecl -> ClassItemDefault <$> convertValueDecl hooks valueDecl
    P.ClassItemTypeFamilyDecl familyDecl -> ClassItemTypeFamilyDecl <$> convertTypeFamilyDecl hooks familyDecl
    P.ClassItemDataFamilyDecl familyDecl -> ClassItemDataFamilyDecl <$> convertDataFamilyDecl hooks familyDecl
    P.ClassItemDefaultTypeInst familyInst -> ClassItemDefaultTypeInst <$> convertTypeFamilyInst hooks familyInst
    P.ClassItemPragma pragma -> pure (ClassItemPragma pragma)

convertInstanceDecl :: (Applicative f) => SpliceHooks f -> P.InstanceDecl -> f InstanceDecl
convertInstanceDecl hooks decl =
  InstanceDecl (P.instanceDeclPragmas decl) (P.instanceDeclWarning decl)
    <$> traverse (convertTyVarBinder hooks) (P.instanceDeclForall decl)
    <*> traverse (convertType hooks) (P.instanceDeclContext decl)
    <*> convertType hooks (P.instanceDeclHead decl)
    <*> traverse (convertInstanceDeclItem hooks) (P.instanceDeclItems decl)

convertInstanceDeclItem :: (Applicative f) => SpliceHooks f -> P.InstanceDeclItem -> f InstanceDeclItem
convertInstanceDeclItem hooks item =
  case item of
    P.InstanceItemAnn ann inner -> InstanceItemAnn ann <$> convertInstanceDeclItem hooks inner
    P.InstanceItemBind valueDecl -> InstanceItemBind <$> convertValueDecl hooks valueDecl
    P.InstanceItemTypeSig names ty -> InstanceItemTypeSig (map fromParserUnqualifiedName names) <$> convertType hooks ty
    P.InstanceItemFixity assoc namespace precedence ops ->
      pure (InstanceItemFixity assoc namespace precedence (map fromParserUnqualifiedName ops))
    P.InstanceItemTypeFamilyInst familyInst -> InstanceItemTypeFamilyInst <$> convertTypeFamilyInst hooks familyInst
    P.InstanceItemDataFamilyInst familyInst -> InstanceItemDataFamilyInst <$> convertDataFamilyInst hooks familyInst
    P.InstanceItemPragma pragma -> pure (InstanceItemPragma pragma)

convertForeignDecl :: (Applicative f) => SpliceHooks f -> P.ForeignDecl -> f ForeignDecl
convertForeignDecl hooks decl =
  ForeignDecl (P.foreignDirection decl) (P.foreignCallConv decl) (P.foreignSafety decl) (P.foreignEntity decl) (fromParserUnqualifiedName (P.foreignName decl))
    <$> convertType hooks (P.foreignType decl)

convertExpr :: (Applicative f) => SpliceHooks f -> P.Expr -> f Expr
convertExpr hooks expr =
  case expr of
    P.EAnn ann inner -> EAnn ann <$> go inner
    P.EVar name -> pure (EVar (fromParserName name))
    P.EImplicitParam name -> pure (EImplicitParam name)
    P.ETypeSyntax form ty -> ETypeSyntax form <$> convertType hooks ty
    P.EInt value numericType text ->
      pure (EInt (copiedLiteralSyntax (primitiveNumericTypeName numericType) (Just (copiedName NameConId "Integer")) "fromInteger") value numericType text)
    P.EFloat value floatType text ->
      pure (EFloat (copiedLiteralSyntax (primitiveFloatTypeName floatType) Nothing "fromRational") value floatType text)
    P.EChar value text -> pure (EChar value text)
    P.ECharHash value text -> pure (ECharHash (copiedName NameConId "Char#") value text)
    P.EString value text -> pure (EString Nothing value text)
    P.EStringHash value text -> pure (EStringHash (copiedName NameConId "Addr#") value text)
    P.EOverloadedLabel label text -> pure (EOverloadedLabel label text)
    P.EQuasiQuote quoter body -> pure (EQuasiQuote quoter body)
    P.EIf cond trueBranch falseBranch -> EIf Nothing <$> go cond <*> go trueBranch <*> go falseBranch
    P.EMultiWayIf guarded -> EMultiWayIf <$> traverse (convertGuardedRhs hooks go) guarded
    P.ELambdaPats pats body -> ELambdaPats <$> traverse (convertPattern hooks) pats <*> go body
    P.ELambdaCase alts -> ELambdaCase <$> traverse (convertCaseAlt hooks go) alts
    P.ELambdaCases alts -> ELambdaCases <$> traverse (convertLambdaCaseAlt hooks) alts
    P.EInfix left op right -> (\left' right' -> EInfix left' (fromParserName op) right') <$> go left <*> go right
    P.EViewPat left right -> EViewPat <$> go left <*> go right
    P.ENegate inner -> ENegate (copiedName NameVarId "negate") <$> go inner
    P.ESectionL inner op -> (`ESectionL` fromParserName op) <$> go inner
    P.ESectionR op inner -> ESectionR (fromParserName op) <$> go inner
    P.ELetDecls decls body -> ELetDecls <$> traverse (convertDecl hooks) decls <*> go body
    P.ECase scrutinee alts -> ECase <$> go scrutinee <*> traverse (convertCaseAlt hooks go) alts
    P.EDo stmts flavor -> (`EDo` flavor) <$> traverse (convertDoStmt hooks go) stmts
    P.EListComp body stmts -> EListComp <$> go body <*> traverse (convertCompStmt hooks) stmts
    P.EListCompParallel body branches -> EListCompParallel <$> go body <*> traverse (traverse (convertCompStmt hooks)) branches
    P.EArithSeq arithSeq -> EArithSeq Nothing <$> convertArithSeq hooks arithSeq
    P.ERecordCon name fields wildcard ->
      (\fields' -> ERecordCon (fromParserName name) fields' wildcard) <$> traverse (convertRecordField go) fields
    P.ERecordUpd record fields -> ERecordUpd <$> go record <*> traverse (convertRecordField go) fields
    P.EGetField record name -> (\record' -> EGetField (copiedName NameVarId "getField") record' (fromParserName name)) <$> go record
    P.EGetFieldProjection names -> pure (EGetFieldProjection (copiedName NameVarId "getField") (map fromParserName names))
    P.ETypeSig inner ty -> ETypeSig <$> go inner <*> convertType hooks ty
    P.EParen inner -> EParen <$> go inner
    P.EList items -> EList (copiedName NameConSym "[]") Nothing <$> traverse go items
    P.ETuple flavor items -> ETuple (copiedName NameConSym (tupleSyntaxText flavor (length items))) flavor <$> traverse (traverse go) items
    P.EUnboxedSum alt arity inner -> EUnboxedSum alt arity <$> go inner
    P.ETypeApp fun ty -> ETypeApp <$> go fun <*> convertType hooks ty
    P.EApp fun arg -> EApp <$> go fun <*> go arg
    P.ETHExpQuote inner -> pure (ETHExpQuote (fromParserExpr inner))
    P.ETHTypedQuote inner -> pure (ETHTypedQuote (fromParserExpr inner))
    P.ETHDeclQuote decls -> pure (ETHDeclQuote (map fromParserDecl decls))
    P.ETHTypeQuote ty -> pure (ETHTypeQuote (fromParserType ty))
    P.ETHPatQuote pat -> pure (ETHPatQuote (fromParserPattern pat))
    P.ETHNameQuote inner -> ETHNameQuote <$> go inner
    P.ETHTypeNameQuote ty -> ETHTypeNameQuote <$> convertType hooks ty
    P.ETHSplice inner -> ETHSplice <$> hookExprSplice hooks inner
    P.ETHTypedSplice inner -> ETHTypedSplice <$> go inner
    P.EProc pat cmd -> EProc <$> convertPattern hooks pat <*> convertCmd hooks cmd
    P.EPragma pragma inner -> EPragma pragma <$> go inner
  where
    go = convertExpr hooks

convertCaseAlt :: (Applicative f) => SpliceHooks f -> (a -> f b) -> P.CaseAlt a -> f (CaseAlt b)
convertCaseAlt hooks convertBody alt =
  CaseAlt (P.caseAltAnns alt)
    <$> convertPattern hooks (P.caseAltPattern alt)
    <*> convertRhs hooks convertBody (P.caseAltRhs alt)

convertLambdaCaseAlt :: (Applicative f) => SpliceHooks f -> P.LambdaCaseAlt -> f LambdaCaseAlt
convertLambdaCaseAlt hooks alt =
  LambdaCaseAlt (P.lambdaCaseAltAnns alt)
    <$> traverse (convertPattern hooks) (P.lambdaCaseAltPats alt)
    <*> convertRhs hooks (convertExpr hooks) (P.lambdaCaseAltRhs alt)

convertDoStmt :: (Applicative f) => SpliceHooks f -> (a -> f b) -> P.DoStmt a -> f (DoStmt b)
convertDoStmt hooks convertBody stmt =
  case stmt of
    P.DoAnn ann inner -> DoAnn ann <$> convertDoStmt hooks convertBody inner
    P.DoBind pat body -> DoBind <$> convertPattern hooks pat <*> convertBody body
    P.DoLetDecls decls -> DoLetDecls <$> traverse (convertDecl hooks) decls
    P.DoExpr body -> DoExpr <$> convertBody body
    P.DoRecStmt stmts -> DoRecStmt <$> traverse (convertDoStmt hooks convertBody) stmts

convertCmd :: (Applicative f) => SpliceHooks f -> P.Cmd -> f Cmd
convertCmd hooks cmd =
  case cmd of
    P.CmdAnn ann inner -> CmdAnn ann <$> go inner
    P.CmdArrApp left appType right -> (`CmdArrApp` appType) <$> expr left <*> expr right
    P.CmdInfix left op right -> (\left' right' -> CmdInfix left' (fromParserName op) right') <$> go left <*> go right
    P.CmdDo stmts -> CmdDo <$> traverse (convertDoStmt hooks go) stmts
    P.CmdIf cond trueBranch falseBranch -> CmdIf <$> expr cond <*> go trueBranch <*> go falseBranch
    P.CmdCase scrutinee alts -> CmdCase <$> expr scrutinee <*> traverse (convertCaseAlt hooks go) alts
    P.CmdLet decls body -> CmdLet <$> traverse (convertDecl hooks) decls <*> go body
    P.CmdLam pats body -> CmdLam <$> traverse (convertPattern hooks) pats <*> go body
    P.CmdApp inner arg -> CmdApp <$> go inner <*> expr arg
    P.CmdPar inner -> CmdPar <$> go inner
  where
    go = convertCmd hooks
    expr = convertExpr hooks

convertCompStmt :: (Applicative f) => SpliceHooks f -> P.CompStmt -> f CompStmt
convertCompStmt hooks stmt =
  case stmt of
    P.CompAnn ann inner -> CompAnn ann <$> convertCompStmt hooks inner
    P.CompGen pat src -> CompGen <$> convertPattern hooks pat <*> expr src
    P.CompGuard guard -> CompGuard <$> expr guard
    P.CompLetDecls decls -> CompLetDecls <$> traverse (convertDecl hooks) decls
    P.CompThen fun -> CompThen <$> expr fun
    P.CompThenBy fun by -> CompThenBy <$> expr fun <*> expr by
    P.CompGroupUsing fun -> CompGroupUsing <$> expr fun
    P.CompGroupByUsing by fun -> CompGroupByUsing <$> expr by <*> expr fun
  where
    expr = convertExpr hooks

convertArithSeq :: (Applicative f) => SpliceHooks f -> P.ArithSeq -> f ArithSeq
convertArithSeq hooks arithSeq =
  case arithSeq of
    P.ArithSeqAnn ann inner -> ArithSeqAnn ann <$> convertArithSeq hooks inner
    P.ArithSeqFrom from -> ArithSeqFrom <$> expr from
    P.ArithSeqFromThen from then' -> ArithSeqFromThen <$> expr from <*> expr then'
    P.ArithSeqFromTo from to -> ArithSeqFromTo <$> expr from <*> expr to
    P.ArithSeqFromThenTo from then' to -> ArithSeqFromThenTo <$> expr from <*> expr then' <*> expr to
  where
    expr = convertExpr hooks
