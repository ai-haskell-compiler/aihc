-- | Change a resolved syntax tree back into a parser syntax tree.
--
-- The type checker and the later phases read the parser syntax tree. They
-- find the results of name resolution in its 'P.Annotation' values. This
-- module puts each explicit result of the resolved tree into such an
-- annotation, in the position where the resolver put it before the
-- resolved tree existed:
--
-- * The resolution of a name is the first annotation of the name.
-- * Each wrapper constructor, such as 'EResolved', becomes the annotation
--   wrapper of its syntax, such as 'P.EAnn'.
-- * A resolve error of an import declaration or a rule is the first
--   annotation of that declaration.
--
-- This change is temporary. When the type checker reads the resolved tree,
-- this module goes away.
module Aihc.Resolve.Syntax.ToParser
  ( toParserModule,
    toParserExpr,
    toParserMatch,
    resolutionResultParserAnnotation,
  )
where

import Aihc.Parser.Syntax qualified as P
import Aihc.Resolve.Syntax

-- | The parser annotation that carries one resolution result.
resolutionResultParserAnnotation :: ResolutionResult -> P.Annotation
resolutionResultParserAnnotation result =
  case result of
    ResolutionOk annotation -> P.mkAnnotation annotation
    ResolutionError resolveError -> P.mkAnnotation resolveError

withResolution :: Maybe ResolutionResult -> [P.Annotation] -> [P.Annotation]
withResolution resolution anns =
  case resolution of
    Nothing -> anns
    Just result -> resolutionResultParserAnnotation result : anns

withError :: Maybe ResolveError -> [P.Annotation] -> [P.Annotation]
withError resolveError anns =
  case resolveError of
    Nothing -> anns
    Just err -> P.mkAnnotation err : anns

-- | Change a module.
toParserModule :: Module -> P.Module
toParserModule modu =
  P.Module
    { P.moduleAnns = moduleAnns modu,
      P.moduleHead = toParserModuleHead <$> moduleHead modu,
      P.moduleLanguagePragmas = moduleLanguagePragmas modu,
      P.moduleImports = map toParserImportDecl (moduleImports modu),
      P.moduleDecls = map toParserDecl (moduleDecls modu)
    }

toParserName :: Name -> P.Name
toParserName (Name qualifier ty txt anns resolution) =
  P.Name qualifier ty txt (withResolution resolution anns)

toParserUnqualifiedName :: UnqualifiedName -> P.UnqualifiedName
toParserUnqualifiedName (UnqualifiedName ty txt anns resolution) =
  P.UnqualifiedName ty txt (withResolution resolution anns)

toParserModuleHead :: ModuleHead -> P.ModuleHead
toParserModuleHead head' =
  P.ModuleHead
    { P.moduleHeadAnns = moduleHeadAnns head',
      P.moduleHeadName = moduleHeadName head',
      P.moduleHeadWarningPragma = moduleHeadWarningPragma head',
      P.moduleHeadExports = map toParserExportSpec <$> moduleHeadExports head'
    }

toParserIEBundledMember :: IEBundledMember -> P.IEBundledMember
toParserIEBundledMember (IEBundledMember namespace name) =
  P.IEBundledMember namespace (toParserName name)

toParserExportSpec :: ExportSpec -> P.ExportSpec
toParserExportSpec spec =
  case spec of
    ExportModule pragma name -> P.ExportModule pragma name
    ExportVar pragma namespace name -> P.ExportVar pragma namespace (toParserName name)
    ExportAbs pragma namespace name -> P.ExportAbs pragma namespace (toParserName name)
    ExportAll pragma namespace name -> P.ExportAll pragma namespace (toParserName name)
    ExportWith pragma namespace name members ->
      P.ExportWith pragma namespace (toParserName name) (map toParserIEBundledMember members)
    ExportWithAll pragma namespace name index members ->
      P.ExportWithAll pragma namespace (toParserName name) index (map toParserIEBundledMember members)
    ExportAnn ann inner -> P.ExportAnn ann (toParserExportSpec inner)

toParserImportDecl :: ImportDecl -> P.ImportDecl
toParserImportDecl decl =
  P.ImportDecl
    { P.importDeclAnns = withError (importDeclError decl) (importDeclAnns decl),
      P.importDeclLevel = importDeclLevel decl,
      P.importDeclPackage = importDeclPackage decl,
      P.importDeclSourcePragma = importDeclSourcePragma decl,
      P.importDeclSafe = importDeclSafe decl,
      P.importDeclQualified = importDeclQualified decl,
      P.importDeclQualifiedPost = importDeclQualifiedPost decl,
      P.importDeclModule = importDeclModule decl,
      P.importDeclAs = importDeclAs decl,
      P.importDeclSpec = toParserImportSpec <$> importDeclSpec decl
    }

toParserImportSpec :: ImportSpec -> P.ImportSpec
toParserImportSpec spec =
  P.ImportSpec
    { P.importSpecAnns = importSpecAnns spec,
      P.importSpecHiding = importSpecHiding spec,
      P.importSpecItems = map toParserImportItem (importSpecItems spec)
    }

toParserImportItem :: ImportItem -> P.ImportItem
toParserImportItem item =
  case item of
    ImportItemVar namespace name -> P.ImportItemVar namespace (toParserUnqualifiedName name)
    ImportItemAbs namespace name -> P.ImportItemAbs namespace (toParserUnqualifiedName name)
    ImportItemAll namespace name -> P.ImportItemAll namespace (toParserUnqualifiedName name)
    ImportItemWith namespace name members ->
      P.ImportItemWith namespace (toParserUnqualifiedName name) (map toParserIEBundledMember members)
    ImportItemAllWith namespace name index members ->
      P.ImportItemAllWith namespace (toParserUnqualifiedName name) index (map toParserIEBundledMember members)
    ImportAnn ann inner -> P.ImportAnn ann (toParserImportItem inner)
    ImportItemError err inner -> P.ImportAnn (P.mkAnnotation err) (toParserImportItem inner)

toParserDecl :: Decl -> P.Decl
toParserDecl decl =
  case decl of
    DeclAnn ann inner -> P.DeclAnn ann (toParserDecl inner)
    DeclError err inner -> P.DeclAnn (P.mkAnnotation err) (toParserDecl inner)
    DeclValue valueDecl -> P.DeclValue (toParserValueDecl valueDecl)
    DeclImplicitParam name expr mDecls ->
      P.DeclImplicitParam name (toParserExpr expr) (map toParserDecl <$> mDecls)
    DeclTypeSig names ty -> P.DeclTypeSig (map toParserUnqualifiedName names) (toParserType ty)
    DeclPatSyn patSyn -> P.DeclPatSyn (toParserPatSynDecl patSyn)
    DeclPatSynSig names ty -> P.DeclPatSynSig (map toParserUnqualifiedName names) (toParserType ty)
    DeclStandaloneKindSig name ty -> P.DeclStandaloneKindSig (toParserUnqualifiedName name) (toParserType ty)
    DeclFixity assoc namespace precedence ops ->
      P.DeclFixity assoc namespace precedence (map toParserUnqualifiedName ops)
    DeclRoleAnnotation (RoleAnnotation name roles) ->
      P.DeclRoleAnnotation (P.RoleAnnotation (toParserUnqualifiedName name) roles)
    DeclTypeSyn typeSyn -> P.DeclTypeSyn (toParserTypeSynDecl typeSyn)
    DeclTypeData dataDecl -> P.DeclTypeData (toParserDataDecl dataDecl)
    DeclData dataDecl -> P.DeclData (toParserDataDecl dataDecl)
    DeclNewtype newtypeDecl -> P.DeclNewtype (toParserNewtypeDecl newtypeDecl)
    DeclClass classDecl -> P.DeclClass (toParserClassDecl classDecl)
    DeclInstance instanceDecl -> P.DeclInstance (toParserInstanceDecl instanceDecl)
    DeclStandaloneDeriving derivingDecl -> P.DeclStandaloneDeriving (toParserStandaloneDerivingDecl derivingDecl)
    DeclDefault tys -> P.DeclDefault (map toParserType tys)
    DeclSplice expr -> P.DeclSplice (toParserExpr expr)
    DeclForeign foreignDecl -> P.DeclForeign (toParserForeignDecl foreignDecl)
    DeclTypeFamilyDecl familyDecl -> P.DeclTypeFamilyDecl (toParserTypeFamilyDecl familyDecl)
    DeclDataFamilyDecl familyDecl -> P.DeclDataFamilyDecl (toParserDataFamilyDecl familyDecl)
    DeclTypeFamilyInst familyInst -> P.DeclTypeFamilyInst (toParserTypeFamilyInst familyInst)
    DeclDataFamilyInst familyInst -> P.DeclDataFamilyInst (toParserDataFamilyInst familyInst)
    DeclPragma pragma -> P.DeclPragma pragma
    DeclRules rules -> P.DeclRules (map toParserRuleDecl rules)

toParserRuleDecl :: RuleDecl -> P.RuleDecl
toParserRuleDecl rule =
  P.RuleDecl
    { P.ruleAnns = withError (ruleError rule) (ruleAnns rule),
      P.ruleName = ruleName rule,
      P.ruleActivation = ruleActivation rule,
      P.ruleTypeBinders = map toParserTyVarBinder (ruleTypeBinders rule),
      P.ruleBinders = map toParserRuleBinder (ruleBinders rule),
      P.ruleLhs = toParserExpr (ruleLhs rule),
      P.ruleRhs = toParserExpr (ruleRhs rule)
    }

toParserRuleBinder :: RuleBinder -> P.RuleBinder
toParserRuleBinder binder =
  P.RuleBinder
    { P.ruleBinderAnns = ruleBinderAnns binder,
      P.ruleBinderName = toParserUnqualifiedName (ruleBinderName binder),
      P.ruleBinderType = toParserType <$> ruleBinderType binder
    }

toParserMultiplicityTag :: MultiplicityTag -> P.MultiplicityTag
toParserMultiplicityTag tag =
  case tag of
    NoMultiplicityTag -> P.NoMultiplicityTag
    LinearMultiplicityTag -> P.LinearMultiplicityTag
    ExplicitMultiplicityTag ty -> P.ExplicitMultiplicityTag (toParserType ty)

toParserValueDecl :: ValueDecl -> P.ValueDecl
toParserValueDecl valueDecl =
  case valueDecl of
    FunctionBind name matches -> P.FunctionBind (toParserUnqualifiedName name) (map toParserMatch matches)
    PatternBind tag pat rhs -> P.PatternBind (toParserMultiplicityTag tag) (toParserPattern pat) (toParserRhs toParserExpr rhs)

toParserMatch :: Match -> P.Match
toParserMatch match =
  P.Match
    { P.matchAnns = matchAnns match,
      P.matchHeadForm = matchHeadForm match,
      P.matchPats = map toParserPattern (matchPats match),
      P.matchRhs = toParserRhs toParserExpr (matchRhs match)
    }

toParserPatSynDecl :: PatSynDecl -> P.PatSynDecl
toParserPatSynDecl patSyn =
  P.PatSynDecl
    { P.patSynDeclName = toParserUnqualifiedName (patSynDeclName patSyn),
      P.patSynDeclArgs = patSynDeclArgs patSyn,
      P.patSynDeclPat = toParserPattern (patSynDeclPat patSyn),
      P.patSynDeclDir = toParserPatSynDir (patSynDeclDir patSyn)
    }

toParserPatSynDir :: PatSynDir -> P.PatSynDir
toParserPatSynDir dir =
  case dir of
    PatSynUnidirectional -> P.PatSynUnidirectional
    PatSynBidirectional -> P.PatSynBidirectional
    PatSynExplicitBidirectional matches -> P.PatSynExplicitBidirectional (map toParserMatch matches)

toParserRhs :: (a -> b) -> Rhs a -> P.Rhs b
toParserRhs body rhs =
  case rhs of
    UnguardedRhs anns value mDecls -> P.UnguardedRhs anns (body value) (map toParserDecl <$> mDecls)
    GuardedRhss anns guarded mDecls -> P.GuardedRhss anns (map (toParserGuardedRhs body) guarded) (map toParserDecl <$> mDecls)

toParserGuardedRhs :: (a -> b) -> GuardedRhs a -> P.GuardedRhs b
toParserGuardedRhs body guarded =
  P.GuardedRhs
    { P.guardedRhsAnns = guardedRhsAnns guarded,
      P.guardedRhsGuards = map toParserGuardQualifier (guardedRhsGuards guarded),
      P.guardedRhsBody = body (guardedRhsBody guarded)
    }

toParserGuardQualifier :: GuardQualifier -> P.GuardQualifier
toParserGuardQualifier qualifier =
  case qualifier of
    GuardAnn ann inner -> P.GuardAnn ann (toParserGuardQualifier inner)
    GuardExpr expr -> P.GuardExpr (toParserExpr expr)
    GuardPat pat expr -> P.GuardPat (toParserPattern pat) (toParserExpr expr)
    GuardLet decls -> P.GuardLet (map toParserDecl decls)

toParserRecordField :: (a -> b) -> RecordField a -> P.RecordField b
toParserRecordField value (RecordField name fieldValue pun) =
  P.RecordField (toParserName name) (value fieldValue) pun

toParserPattern :: Pattern -> P.Pattern
toParserPattern pat =
  case pat of
    PAnn ann inner -> P.PAnn ann (toParserPattern inner)
    PResolved result inner -> P.PAnn (resolutionResultParserAnnotation result) (toParserPattern inner)
    PVar name -> P.PVar (toParserUnqualifiedName name)
    PTypeBinder binder -> P.PTypeBinder (toParserTyVarBinder binder)
    PTypeSyntax form ty -> P.PTypeSyntax form (toParserType ty)
    PWildcard -> P.PWildcard
    PLit lit -> P.PLit lit
    PQuasiQuote quoter body -> P.PQuasiQuote quoter body
    PTuple flavor pats -> P.PTuple flavor (map toParserPattern pats)
    PUnboxedSum alt arity inner -> P.PUnboxedSum alt arity (toParserPattern inner)
    PList pats -> P.PList (map toParserPattern pats)
    PCon name tys pats -> P.PCon (toParserName name) (map toParserType tys) (map toParserPattern pats)
    PBuiltinCon builtin tys pats -> P.PBuiltinCon builtin (map toParserType tys) (map toParserPattern pats)
    PInfix left op right -> P.PInfix (toParserPattern left) (toParserName op) (toParserPattern right)
    PView expr inner -> P.PView (toParserExpr expr) (toParserPattern inner)
    PAs name inner -> P.PAs (toParserUnqualifiedName name) (toParserPattern inner)
    PStrict inner -> P.PStrict (toParserPattern inner)
    PIrrefutable inner -> P.PIrrefutable (toParserPattern inner)
    PNegLit lit -> P.PNegLit lit
    PNPlusK name lit -> P.PNPlusK (toParserUnqualifiedName name) lit
    PParen inner -> P.PParen (toParserPattern inner)
    PRecord name fields wildcard -> P.PRecord (toParserName name) (map (toParserRecordField toParserPattern) fields) wildcard
    PTypeSig inner ty -> P.PTypeSig (toParserPattern inner) (toParserType ty)
    PSplice expr -> P.PSplice (toParserExpr expr)

toParserForallTelescope :: ForallTelescope -> P.ForallTelescope
toParserForallTelescope (ForallTelescope visibility binders) =
  P.ForallTelescope visibility (map toParserTyVarBinder binders)

toParserArrowKind :: ArrowKind -> P.ArrowKind
toParserArrowKind arrowKind =
  case arrowKind of
    ArrowUnrestricted -> P.ArrowUnrestricted
    ArrowLinear -> P.ArrowLinear
    ArrowExplicit ty -> P.ArrowExplicit (toParserType ty)

toParserType :: Type -> P.Type
toParserType ty =
  case ty of
    TAnn ann inner -> P.TAnn ann (toParserType inner)
    TResolved result inner -> P.TAnn (resolutionResultParserAnnotation result) (toParserType inner)
    TVar name -> P.TVar (toParserUnqualifiedName name)
    TCon name promotion -> P.TCon (toParserName name) promotion
    TBuiltinCon builtin promotion -> P.TBuiltinCon builtin promotion
    TImplicitParam name inner -> P.TImplicitParam name (toParserType inner)
    TTypeLit lit -> P.TTypeLit lit
    TStar text -> P.TStar text
    TQuasiQuote quoter body -> P.TQuasiQuote quoter body
    TForall telescope inner -> P.TForall (toParserForallTelescope telescope) (toParserType inner)
    TApp left right -> P.TApp (toParserType left) (toParserType right)
    TTypeApp left right -> P.TTypeApp (toParserType left) (toParserType right)
    TInfix left name promotion right -> P.TInfix (toParserType left) (toParserName name) promotion (toParserType right)
    TFun arrowKind left right -> P.TFun (toParserArrowKind arrowKind) (toParserType left) (toParserType right)
    TTuple flavor promotion items -> P.TTuple flavor promotion (map toParserType items)
    TUnboxedSum items -> P.TUnboxedSum (map toParserType items)
    TList promotion items -> P.TList promotion (map toParserType items)
    TParen inner -> P.TParen (toParserType inner)
    TKindSig inner kind -> P.TKindSig (toParserType inner) (toParserType kind)
    TContext constraints inner -> P.TContext (map toParserType constraints) (toParserType inner)
    TSplice expr -> P.TSplice (toParserExpr expr)
    TWildcard -> P.TWildcard

toParserTyVarBinder :: TyVarBinder -> P.TyVarBinder
toParserTyVarBinder binder =
  P.TyVarBinder
    { P.tyVarBinderAnns = tyVarBinderAnns binder,
      P.tyVarBinderName = tyVarBinderName binder,
      P.tyVarBinderKind = toParserType <$> tyVarBinderKind binder,
      P.tyVarBinderSpecificity = tyVarBinderSpecificity binder,
      P.tyVarBinderVisibility = tyVarBinderVisibility binder
    }

toParserBinderHead :: BinderHead UnqualifiedName -> P.BinderHead P.UnqualifiedName
toParserBinderHead head' =
  case head' of
    PrefixBinderHead name params -> P.PrefixBinderHead (toParserUnqualifiedName name) (map toParserTyVarBinder params)
    InfixBinderHead left name right params ->
      P.InfixBinderHead (toParserTyVarBinder left) (toParserUnqualifiedName name) (toParserTyVarBinder right) (map toParserTyVarBinder params)

toParserTypeSynDecl :: TypeSynDecl -> P.TypeSynDecl
toParserTypeSynDecl (TypeSynDecl head' body) =
  P.TypeSynDecl (toParserBinderHead head') (toParserType body)

toParserTypeFamilyDecl :: TypeFamilyDecl -> P.TypeFamilyDecl
toParserTypeFamilyDecl familyDecl =
  P.TypeFamilyDecl
    { P.typeFamilyDeclHeadForm = typeFamilyDeclHeadForm familyDecl,
      P.typeFamilyDeclExplicitFamilyKeyword = typeFamilyDeclExplicitFamilyKeyword familyDecl,
      P.typeFamilyDeclHead = toParserType (typeFamilyDeclHead familyDecl),
      P.typeFamilyDeclParams = map toParserTyVarBinder (typeFamilyDeclParams familyDecl),
      P.typeFamilyDeclResultSig = toParserTypeFamilyResultSig <$> typeFamilyDeclResultSig familyDecl,
      P.typeFamilyDeclEquations = map toParserTypeFamilyEq <$> typeFamilyDeclEquations familyDecl
    }

toParserTypeFamilyResultSig :: TypeFamilyResultSig -> P.TypeFamilyResultSig
toParserTypeFamilyResultSig resultSig =
  case resultSig of
    TypeFamilyKindSig ty -> P.TypeFamilyKindSig (toParserType ty)
    TypeFamilyTyVarSig binder -> P.TypeFamilyTyVarSig (toParserTyVarBinder binder)
    TypeFamilyInjectiveSig binder injectivity -> P.TypeFamilyInjectiveSig (toParserTyVarBinder binder) injectivity

toParserTypeFamilyEq :: TypeFamilyEq -> P.TypeFamilyEq
toParserTypeFamilyEq equation =
  P.TypeFamilyEq
    { P.typeFamilyEqAnns = typeFamilyEqAnns equation,
      P.typeFamilyEqForall = map toParserTyVarBinder (typeFamilyEqForall equation),
      P.typeFamilyEqHeadForm = typeFamilyEqHeadForm equation,
      P.typeFamilyEqLhs = toParserType (typeFamilyEqLhs equation),
      P.typeFamilyEqRhs = toParserType (typeFamilyEqRhs equation)
    }

toParserDataFamilyDecl :: DataFamilyDecl -> P.DataFamilyDecl
toParserDataFamilyDecl (DataFamilyDecl head' kind) =
  P.DataFamilyDecl (toParserBinderHead head') (toParserType <$> kind)

toParserTypeFamilyInst :: TypeFamilyInst -> P.TypeFamilyInst
toParserTypeFamilyInst familyInst =
  P.TypeFamilyInst
    { P.typeFamilyInstForall = map toParserTyVarBinder (typeFamilyInstForall familyInst),
      P.typeFamilyInstHeadForm = typeFamilyInstHeadForm familyInst,
      P.typeFamilyInstLhs = toParserType (typeFamilyInstLhs familyInst),
      P.typeFamilyInstRhs = toParserType (typeFamilyInstRhs familyInst)
    }

toParserDataFamilyInst :: DataFamilyInst -> P.DataFamilyInst
toParserDataFamilyInst familyInst =
  P.DataFamilyInst
    { P.dataFamilyInstIsNewtype = dataFamilyInstIsNewtype familyInst,
      P.dataFamilyInstForall = map toParserTyVarBinder (dataFamilyInstForall familyInst),
      P.dataFamilyInstHead = toParserType (dataFamilyInstHead familyInst),
      P.dataFamilyInstKind = toParserType <$> dataFamilyInstKind familyInst,
      P.dataFamilyInstConstructors = map toParserDataConDecl (dataFamilyInstConstructors familyInst),
      P.dataFamilyInstDeriving = map toParserDerivingClause (dataFamilyInstDeriving familyInst)
    }

toParserDataDecl :: DataDecl -> P.DataDecl
toParserDataDecl dataDecl =
  P.DataDecl
    { P.dataDeclCTypePragma = dataDeclCTypePragma dataDecl,
      P.dataDeclHead = toParserBinderHead (dataDeclHead dataDecl),
      P.dataDeclContext = map toParserType (dataDeclContext dataDecl),
      P.dataDeclKind = toParserType <$> dataDeclKind dataDecl,
      P.dataDeclConstructors = map toParserDataConDecl (dataDeclConstructors dataDecl),
      P.dataDeclDeriving = map toParserDerivingClause (dataDeclDeriving dataDecl)
    }

toParserNewtypeDecl :: NewtypeDecl -> P.NewtypeDecl
toParserNewtypeDecl newtypeDecl =
  P.NewtypeDecl
    { P.newtypeDeclCTypePragma = newtypeDeclCTypePragma newtypeDecl,
      P.newtypeDeclHead = toParserBinderHead (newtypeDeclHead newtypeDecl),
      P.newtypeDeclContext = map toParserType (newtypeDeclContext newtypeDecl),
      P.newtypeDeclKind = toParserType <$> newtypeDeclKind newtypeDecl,
      P.newtypeDeclConstructor = toParserDataConDecl <$> newtypeDeclConstructor newtypeDecl,
      P.newtypeDeclDeriving = map toParserDerivingClause (newtypeDeclDeriving newtypeDecl)
    }

toParserDataConDecl :: DataConDecl -> P.DataConDecl
toParserDataConDecl dataConDecl =
  case dataConDecl of
    DataConAnn ann inner -> P.DataConAnn ann (toParserDataConDecl inner)
    DataConResolved result inner -> P.DataConAnn (resolutionResultParserAnnotation result) (toParserDataConDecl inner)
    PrefixCon forallVars context name bangTypes ->
      P.PrefixCon (binders forallVars) (types context) (toParserUnqualifiedName name) (map toParserBangType bangTypes)
    InfixCon forallVars context left name right ->
      P.InfixCon (binders forallVars) (types context) (toParserBangType left) (toParserUnqualifiedName name) (toParserBangType right)
    RecordCon forallVars context name fields ->
      P.RecordCon (binders forallVars) (types context) (toParserUnqualifiedName name) (map toParserFieldDecl fields)
    GadtCon telescopes context names body ->
      P.GadtCon (map toParserForallTelescope telescopes) (types context) (map toParserUnqualifiedName names) (toParserGadtBody body)
    TupleCon forallVars context flavor fields ->
      P.TupleCon (binders forallVars) (types context) flavor (map toParserBangType fields)
    UnboxedSumCon forallVars context position arity field ->
      P.UnboxedSumCon (binders forallVars) (types context) position arity (toParserBangType field)
    ListCon forallVars context -> P.ListCon (binders forallVars) (types context)
  where
    binders = map toParserTyVarBinder
    types = map toParserType

toParserGadtBody :: GadtBody -> P.GadtBody
toParserGadtBody body =
  case body of
    GadtPrefixBody args result ->
      P.GadtPrefixBody [(toParserBangType bangType', toParserArrowKind arrowKind) | (bangType', arrowKind) <- args] (toParserType result)
    GadtRecordBody fields result -> P.GadtRecordBody (map toParserFieldDecl fields) (toParserType result)

toParserBangType :: BangType -> P.BangType
toParserBangType bangType' =
  P.BangType
    { P.bangAnns = bangAnns bangType',
      P.bangPragmas = bangPragmas bangType',
      P.bangStrict = bangStrict bangType',
      P.bangLazy = bangLazy bangType',
      P.bangType = toParserType (bangType bangType')
    }

toParserFieldDecl :: FieldDecl -> P.FieldDecl
toParserFieldDecl field =
  P.FieldDecl
    { P.fieldAnns = fieldAnns field,
      P.fieldNames = map toParserUnqualifiedName (fieldNames field),
      P.fieldMultiplicity = toParserType <$> fieldMultiplicity field,
      P.fieldType = toParserBangType (fieldType field)
    }

toParserDerivingClause :: DerivingClause -> P.DerivingClause
toParserDerivingClause clause =
  P.DerivingClause
    { P.derivingStrategy = toParserDerivingStrategy <$> derivingStrategy clause,
      P.derivingClasses = either (Left . toParserName) (Right . map toParserType) (derivingClasses clause)
    }

toParserDerivingStrategy :: DerivingStrategy -> P.DerivingStrategy
toParserDerivingStrategy strategy =
  case strategy of
    DerivingStock -> P.DerivingStock
    DerivingNewtype -> P.DerivingNewtype
    DerivingAnyclass -> P.DerivingAnyclass
    DerivingVia ty -> P.DerivingVia (toParserType ty)

toParserStandaloneDerivingDecl :: StandaloneDerivingDecl -> P.StandaloneDerivingDecl
toParserStandaloneDerivingDecl decl =
  P.StandaloneDerivingDecl
    { P.standaloneDerivingStrategy = toParserDerivingStrategy <$> standaloneDerivingStrategy decl,
      P.standaloneDerivingPragmas = standaloneDerivingPragmas decl,
      P.standaloneDerivingWarning = standaloneDerivingWarning decl,
      P.standaloneDerivingForall = map toParserTyVarBinder (standaloneDerivingForall decl),
      P.standaloneDerivingContext = map toParserType (standaloneDerivingContext decl),
      P.standaloneDerivingHead = toParserType (standaloneDerivingHead decl)
    }

toParserClassDecl :: ClassDecl -> P.ClassDecl
toParserClassDecl classDecl =
  P.ClassDecl
    { P.classDeclContext = map toParserType <$> classDeclContext classDecl,
      P.classDeclHead = toParserBinderHead (classDeclHead classDecl),
      P.classDeclFundeps = classDeclFundeps classDecl,
      P.classDeclItems = map toParserClassDeclItem (classDeclItems classDecl)
    }

toParserClassDeclItem :: ClassDeclItem -> P.ClassDeclItem
toParserClassDeclItem item =
  case item of
    ClassItemAnn ann inner -> P.ClassItemAnn ann (toParserClassDeclItem inner)
    ClassItemError err inner -> P.ClassItemAnn (P.mkAnnotation err) (toParserClassDeclItem inner)
    ClassItemTypeSig names ty -> P.ClassItemTypeSig (map toParserUnqualifiedName names) (toParserType ty)
    ClassItemDefaultSig name ty -> P.ClassItemDefaultSig (toParserUnqualifiedName name) (toParserType ty)
    ClassItemFixity assoc namespace precedence ops ->
      P.ClassItemFixity assoc namespace precedence (map toParserUnqualifiedName ops)
    ClassItemDefault valueDecl -> P.ClassItemDefault (toParserValueDecl valueDecl)
    ClassItemTypeFamilyDecl familyDecl -> P.ClassItemTypeFamilyDecl (toParserTypeFamilyDecl familyDecl)
    ClassItemDataFamilyDecl familyDecl -> P.ClassItemDataFamilyDecl (toParserDataFamilyDecl familyDecl)
    ClassItemDefaultTypeInst familyInst -> P.ClassItemDefaultTypeInst (toParserTypeFamilyInst familyInst)
    ClassItemPragma pragma -> P.ClassItemPragma pragma

toParserInstanceDecl :: InstanceDecl -> P.InstanceDecl
toParserInstanceDecl decl =
  P.InstanceDecl
    { P.instanceDeclPragmas = instanceDeclPragmas decl,
      P.instanceDeclWarning = instanceDeclWarning decl,
      P.instanceDeclForall = map toParserTyVarBinder (instanceDeclForall decl),
      P.instanceDeclContext = map toParserType (instanceDeclContext decl),
      P.instanceDeclHead = toParserType (instanceDeclHead decl),
      P.instanceDeclItems = map toParserInstanceDeclItem (instanceDeclItems decl)
    }

toParserInstanceDeclItem :: InstanceDeclItem -> P.InstanceDeclItem
toParserInstanceDeclItem item =
  case item of
    InstanceItemAnn ann inner -> P.InstanceItemAnn ann (toParserInstanceDeclItem inner)
    InstanceItemError err inner -> P.InstanceItemAnn (P.mkAnnotation err) (toParserInstanceDeclItem inner)
    InstanceItemBind valueDecl -> P.InstanceItemBind (toParserValueDecl valueDecl)
    InstanceItemTypeSig names ty -> P.InstanceItemTypeSig (map toParserUnqualifiedName names) (toParserType ty)
    InstanceItemFixity assoc namespace precedence ops ->
      P.InstanceItemFixity assoc namespace precedence (map toParserUnqualifiedName ops)
    InstanceItemTypeFamilyInst familyInst -> P.InstanceItemTypeFamilyInst (toParserTypeFamilyInst familyInst)
    InstanceItemDataFamilyInst familyInst -> P.InstanceItemDataFamilyInst (toParserDataFamilyInst familyInst)
    InstanceItemPragma pragma -> P.InstanceItemPragma pragma

toParserForeignDecl :: ForeignDecl -> P.ForeignDecl
toParserForeignDecl decl =
  P.ForeignDecl
    { P.foreignDirection = foreignDirection decl,
      P.foreignCallConv = foreignCallConv decl,
      P.foreignSafety = foreignSafety decl,
      P.foreignEntity = foreignEntity decl,
      P.foreignName = toParserUnqualifiedName (foreignName decl),
      P.foreignType = toParserType (foreignType decl)
    }

toParserExpr :: Expr -> P.Expr
toParserExpr expr =
  case expr of
    EAnn ann inner -> P.EAnn ann (toParserExpr inner)
    EResolved result inner -> P.EAnn (resolutionResultParserAnnotation result) (toParserExpr inner)
    EVar name -> P.EVar (toParserName name)
    EImplicitParam name -> P.EImplicitParam name
    ETypeSyntax form ty -> P.ETypeSyntax form (toParserType ty)
    EInt value numericType text -> P.EInt value numericType text
    EFloat value floatType text -> P.EFloat value floatType text
    EChar value text -> P.EChar value text
    ECharHash value text -> P.ECharHash value text
    EString value text -> P.EString value text
    EStringHash value text -> P.EStringHash value text
    EOverloadedLabel label text -> P.EOverloadedLabel label text
    EQuasiQuote quoter body -> P.EQuasiQuote quoter body
    EIf cond trueBranch falseBranch -> P.EIf (toParserExpr cond) (toParserExpr trueBranch) (toParserExpr falseBranch)
    EMultiWayIf guarded -> P.EMultiWayIf (map (toParserGuardedRhs toParserExpr) guarded)
    ELambdaPats pats body -> P.ELambdaPats (map toParserPattern pats) (toParserExpr body)
    ELambdaCase alts -> P.ELambdaCase (map (toParserCaseAlt toParserExpr) alts)
    ELambdaCases alts -> P.ELambdaCases (map toParserLambdaCaseAlt alts)
    EInfix left op right -> P.EInfix (toParserExpr left) (toParserName op) (toParserExpr right)
    EViewPat left right -> P.EViewPat (toParserExpr left) (toParserExpr right)
    ENegate inner -> P.ENegate (toParserExpr inner)
    ESectionL inner op -> P.ESectionL (toParserExpr inner) (toParserName op)
    ESectionR op inner -> P.ESectionR (toParserName op) (toParserExpr inner)
    ELetDecls decls body -> P.ELetDecls (map toParserDecl decls) (toParserExpr body)
    ECase scrutinee alts -> P.ECase (toParserExpr scrutinee) (map (toParserCaseAlt toParserExpr) alts)
    EDo stmts flavor -> P.EDo (map (toParserDoStmt toParserExpr) stmts) flavor
    EListComp body stmts -> P.EListComp (toParserExpr body) (map toParserCompStmt stmts)
    EListCompParallel body branches -> P.EListCompParallel (toParserExpr body) (map (map toParserCompStmt) branches)
    EArithSeq arithSeq -> P.EArithSeq (toParserArithSeq arithSeq)
    ERecordCon name fields wildcard -> P.ERecordCon (toParserName name) (map (toParserRecordField toParserExpr) fields) wildcard
    ERecordUpd record fields -> P.ERecordUpd (toParserExpr record) (map (toParserRecordField toParserExpr) fields)
    EGetField record name -> P.EGetField (toParserExpr record) (toParserName name)
    EGetFieldProjection names -> P.EGetFieldProjection (map toParserName names)
    ETypeSig inner ty -> P.ETypeSig (toParserExpr inner) (toParserType ty)
    EParen inner -> P.EParen (toParserExpr inner)
    EList items -> P.EList (map toParserExpr items)
    ETuple flavor items -> P.ETuple flavor (map (fmap toParserExpr) items)
    EUnboxedSum alt arity inner -> P.EUnboxedSum alt arity (toParserExpr inner)
    ETypeApp fun ty -> P.ETypeApp (toParserExpr fun) (toParserType ty)
    EApp fun arg -> P.EApp (toParserExpr fun) (toParserExpr arg)
    ETHExpQuote inner -> P.ETHExpQuote (toParserExpr inner)
    ETHTypedQuote inner -> P.ETHTypedQuote (toParserExpr inner)
    ETHDeclQuote decls -> P.ETHDeclQuote (map toParserDecl decls)
    ETHTypeQuote ty -> P.ETHTypeQuote (toParserType ty)
    ETHPatQuote pat -> P.ETHPatQuote (toParserPattern pat)
    ETHNameQuote inner -> P.ETHNameQuote (toParserExpr inner)
    ETHTypeNameQuote ty -> P.ETHTypeNameQuote (toParserType ty)
    ETHSplice inner -> P.ETHSplice (toParserExpr inner)
    ETHTypedSplice inner -> P.ETHTypedSplice (toParserExpr inner)
    EProc pat cmd -> P.EProc (toParserPattern pat) (toParserCmd cmd)
    EPragma pragma inner -> P.EPragma pragma (toParserExpr inner)

toParserCaseAlt :: (a -> b) -> CaseAlt a -> P.CaseAlt b
toParserCaseAlt body alt =
  P.CaseAlt
    { P.caseAltAnns = caseAltAnns alt,
      P.caseAltPattern = toParserPattern (caseAltPattern alt),
      P.caseAltRhs = toParserRhs body (caseAltRhs alt)
    }

toParserLambdaCaseAlt :: LambdaCaseAlt -> P.LambdaCaseAlt
toParserLambdaCaseAlt alt =
  P.LambdaCaseAlt
    { P.lambdaCaseAltAnns = lambdaCaseAltAnns alt,
      P.lambdaCaseAltPats = map toParserPattern (lambdaCaseAltPats alt),
      P.lambdaCaseAltRhs = toParserRhs toParserExpr (lambdaCaseAltRhs alt)
    }

toParserDoStmt :: (a -> b) -> DoStmt a -> P.DoStmt b
toParserDoStmt body stmt =
  case stmt of
    DoAnn ann inner -> P.DoAnn ann (toParserDoStmt body inner)
    DoResolved result inner -> P.DoAnn (resolutionResultParserAnnotation result) (toParserDoStmt body inner)
    DoRecKnot knot inner -> P.DoAnn (P.mkAnnotation knot) (toParserDoStmt body inner)
    DoBind pat value -> P.DoBind (toParserPattern pat) (body value)
    DoLetDecls decls -> P.DoLetDecls (map toParserDecl decls)
    DoExpr value -> P.DoExpr (body value)
    DoRecStmt stmts -> P.DoRecStmt (map (toParserDoStmt body) stmts)

toParserCmd :: Cmd -> P.Cmd
toParserCmd cmd =
  case cmd of
    CmdAnn ann inner -> P.CmdAnn ann (toParserCmd inner)
    CmdArrApp left appType right -> P.CmdArrApp (toParserExpr left) appType (toParserExpr right)
    CmdInfix left op right -> P.CmdInfix (toParserCmd left) (toParserName op) (toParserCmd right)
    CmdDo stmts -> P.CmdDo (map (toParserDoStmt toParserCmd) stmts)
    CmdIf cond trueBranch falseBranch -> P.CmdIf (toParserExpr cond) (toParserCmd trueBranch) (toParserCmd falseBranch)
    CmdCase scrutinee alts -> P.CmdCase (toParserExpr scrutinee) (map (toParserCaseAlt toParserCmd) alts)
    CmdLet decls body -> P.CmdLet (map toParserDecl decls) (toParserCmd body)
    CmdLam pats body -> P.CmdLam (map toParserPattern pats) (toParserCmd body)
    CmdApp inner arg -> P.CmdApp (toParserCmd inner) (toParserExpr arg)
    CmdPar inner -> P.CmdPar (toParserCmd inner)

toParserCompStmt :: CompStmt -> P.CompStmt
toParserCompStmt stmt =
  case stmt of
    CompAnn ann inner -> P.CompAnn ann (toParserCompStmt inner)
    CompGen pat src -> P.CompGen (toParserPattern pat) (toParserExpr src)
    CompGuard guard -> P.CompGuard (toParserExpr guard)
    CompLetDecls decls -> P.CompLetDecls (map toParserDecl decls)
    CompThen fun -> P.CompThen (toParserExpr fun)
    CompThenBy fun by -> P.CompThenBy (toParserExpr fun) (toParserExpr by)
    CompGroupUsing fun -> P.CompGroupUsing (toParserExpr fun)
    CompGroupByUsing by fun -> P.CompGroupByUsing (toParserExpr by) (toParserExpr fun)

toParserArithSeq :: ArithSeq -> P.ArithSeq
toParserArithSeq arithSeq =
  case arithSeq of
    ArithSeqAnn ann inner -> P.ArithSeqAnn ann (toParserArithSeq inner)
    ArithSeqResolved result inner -> P.ArithSeqAnn (resolutionResultParserAnnotation result) (toParserArithSeq inner)
    ArithSeqFrom from -> P.ArithSeqFrom (toParserExpr from)
    ArithSeqFromThen from then' -> P.ArithSeqFromThen (toParserExpr from) (toParserExpr then')
    ArithSeqFromTo from to -> P.ArithSeqFromTo (toParserExpr from) (toParserExpr to)
    ArithSeqFromThenTo from then' to -> P.ArithSeqFromThenTo (toParserExpr from) (toParserExpr then') (toParserExpr to)
