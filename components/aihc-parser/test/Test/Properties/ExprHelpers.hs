-- | Shared normalization helpers used by multiple round-trip test modules.
module Test.Properties.ExprHelpers
  ( normalizeExpr,
    normalizeDecl,
    span0,
  )
where

import Aihc.Parser.Syntax
import Data.Text qualified as T

-- | Canonical empty source span for normalization.
span0 :: SourceSpan
span0 = noSourceSpan

-- | Normalize an expression to canonical form for comparison.
-- Removes source spans and simplifies parentheses.
normalizeExpr :: Expr -> Expr
normalizeExpr expr =
  case expr of
    EVar _ name -> EVar span0 name
    EInt _ value _ -> EInt span0 value (T.pack (show value))
    EIntHash _ value repr -> EIntHash span0 value repr
    EIntBase _ value repr -> EIntBase span0 value repr
    EIntBaseHash _ value repr -> EIntBaseHash span0 value repr
    EFloat _ value repr -> EFloat span0 value repr
    EFloatHash _ value repr -> EFloatHash span0 value repr
    EChar _ value repr -> EChar span0 value repr
    ECharHash _ value repr -> ECharHash span0 value repr
    EString _ value repr -> EString span0 value repr
    EStringHash _ value repr -> EStringHash span0 value repr
    EOverloadedLabel _ value repr -> EOverloadedLabel span0 value repr
    EQuasiQuote _ quoter body -> EQuasiQuote span0 quoter body
    EApp _ fn arg -> EApp span0 (normalizeExpr fn) (normalizeExpr arg)
    EInfix _ lhs op rhs -> EInfix span0 (normalizeExpr lhs) op (normalizeExpr rhs)
    ENegate _ inner -> ENegate span0 (normalizeExpr inner)
    ESectionL _ inner op -> ESectionL span0 (normalizeExpr inner) op
    ESectionR _ op inner -> ESectionR span0 op (normalizeExpr inner)
    EIf _ cond thenE elseE -> EIf span0 (normalizeExpr cond) (normalizeExpr thenE) (normalizeExpr elseE)
    EMultiWayIf _ rhss -> EMultiWayIf span0 (map normalizeGuardedRhs rhss)
    ECase _ scrutinee alts -> ECase span0 (normalizeExpr scrutinee) (map normalizeCaseAlt alts)
    ELambdaPats _ pats body -> ELambdaPats span0 (map normalizeLambdaPat pats) (normalizeExpr body)
    ELambdaCase _ alts -> ELambdaCase span0 (map normalizeCaseAlt alts)
    ELetDecls _ decls body -> ELetDecls span0 (map normalizeDecl decls) (normalizeExpr body)
    EDo _ stmts isMdo -> EDo span0 (map normalizeDoStmt stmts) isMdo
    EListComp _ body stmts -> EListComp span0 (normalizeExpr body) (map normalizeCompStmt stmts)
    EListCompParallel _ body stmtss -> EListCompParallel span0 (normalizeExpr body) (map (map normalizeCompStmt) stmtss)
    EList _ elems -> EList span0 (map normalizeExpr elems)
    ETuple _ tupleFlavor elems -> ETuple span0 tupleFlavor (map (fmap normalizeExpr) elems)
    EArithSeq _ seq' -> EArithSeq span0 (normalizeArithSeq seq')
    ERecordCon _ con fields rwc -> ERecordCon span0 con [(name, normalizeExpr e) | (name, e) <- fields] rwc
    ERecordUpd _ target fields -> ERecordUpd span0 (normalizeExpr target) [(name, normalizeExpr e) | (name, e) <- fields]
    ETypeSig _ inner ty -> ETypeSig span0 (normalizeExpr inner) (normalizeType ty)
    ETypeApp _ inner ty -> ETypeApp span0 (normalizeExpr inner) (normalizeType ty)
    EUnboxedSum _ altIdx arity inner -> EUnboxedSum span0 altIdx arity (normalizeExpr inner)
    EParen _ inner -> normalizeExpr inner
    ETHExpQuote _ body -> ETHExpQuote span0 (normalizeExpr body)
    ETHTypedQuote _ body -> ETHTypedQuote span0 (normalizeExpr body)
    ETHDeclQuote _ decls -> ETHDeclQuote span0 (map normalizeDecl decls)
    ETHTypeQuote _ ty -> ETHTypeQuote span0 (normalizeType ty)
    ETHPatQuote _ pat -> ETHPatQuote span0 (normalizePattern pat)
    ETHNameQuote _ name -> ETHNameQuote span0 name
    ETHTypeNameQuote _ name -> ETHTypeNameQuote span0 name
    ETHSplice _ body -> ETHSplice span0 (normalizeExpr body)
    ETHTypedSplice _ body -> ETHTypedSplice span0 (normalizeExpr body)
    EProc _ pat body -> EProc span0 (normalizePattern pat) body
    EAnn ann sub -> EAnn ann (normalizeExpr sub)

normalizeCaseAlt :: CaseAlt -> CaseAlt
normalizeCaseAlt alt =
  CaseAlt
    { caseAltSpan = span0,
      caseAltPattern = normalizePattern (caseAltPattern alt),
      caseAltRhs = normalizeRhs (caseAltRhs alt)
    }

normalizeRhs :: Rhs -> Rhs
normalizeRhs rhs =
  case rhs of
    UnguardedRhs _ body mDecls -> UnguardedRhs span0 (normalizeExpr body) (fmap (map normalizeDecl) mDecls)
    GuardedRhss _ guards mDecls -> GuardedRhss span0 (map normalizeGuardedRhs guards) (fmap (map normalizeDecl) mDecls)

normalizeGuardedRhs :: GuardedRhs -> GuardedRhs
normalizeGuardedRhs grhs =
  GuardedRhs
    { guardedRhsSpan = span0,
      guardedRhsGuards = map normalizeGuardQualifier (guardedRhsGuards grhs),
      guardedRhsBody = normalizeExpr (guardedRhsBody grhs)
    }

normalizeGuardQualifier :: GuardQualifier -> GuardQualifier
normalizeGuardQualifier qual =
  case qual of
    GuardExpr _ e -> GuardExpr span0 (normalizeExpr e)
    GuardPat _ pat e -> GuardPat span0 (normalizePattern pat) (normalizeExpr e)
    GuardLet _ decls -> GuardLet span0 (map normalizeDecl decls)

normalizePattern :: Pattern -> Pattern
normalizePattern pat =
  case pat of
    PAnn _ sub -> normalizePattern sub
    PVar _ name -> PVar span0 name
    PWildcard _ -> PWildcard span0
    PLit _ lit -> PLit span0 (normalizeLiteral lit)
    PQuasiQuote _ quoter body -> PQuasiQuote span0 quoter body
    PTuple _ tupleFlavor elems -> PTuple span0 tupleFlavor (map normalizePattern elems)
    PList _ elems -> PList span0 (map normalizePattern elems)
    PCon _ con args -> PCon span0 con (map normalizePattern args)
    PInfix _ lhs op rhs -> PInfix span0 (normalizePattern lhs) op (normalizePattern rhs)
    PView _ e inner -> PView span0 (normalizeExpr e) (normalizePattern inner)
    PAs _ name inner -> PAs span0 name (normalizeUnaryPatInner inner)
    PStrict _ inner -> PStrict span0 (normalizeUnaryPatInner inner)
    PIrrefutable _ inner -> PIrrefutable span0 (normalizeUnaryPatInner inner)
    PNegLit _ lit -> PNegLit span0 (normalizeLiteral lit)
    PParen _ inner -> PParen span0 (normalizePattern inner)
    PUnboxedSum _ altIdx arity inner -> PUnboxedSum span0 altIdx arity (normalizePattern inner)
    PRecord _ con fields rwc -> PRecord span0 con [(name, normalizePattern p) | (name, p) <- fields] rwc
    PTypeSig _ inner ty -> PTypeSig span0 (normalizePattern inner) (normalizeType ty)
    PSplice _ body -> PSplice span0 (normalizeExpr body)

-- | Normalize a pattern in lambda argument position.
-- The pretty-printer uses prettyLambdaPatternAtom for lambda patterns, which
-- wraps certain patterns in PParen beyond what prettyPatternAtom does:
-- PNegLit (ambiguous with subtraction) and nullary PCon (greedily absorbs
-- next argument). It also inherits prettyPatternAtom's parenthesization of
-- non-atomic patterns like PCon with args, PInfix, PTypeSig, PRecord.
-- Strip that added PParen so generated and parsed forms match.
normalizeLambdaPat :: Pattern -> Pattern
normalizeLambdaPat pat =
  case normalizePattern pat of
    PParen _ inner@(PNegLit {}) -> inner
    PParen _ inner@(PCon {}) -> inner
    PParen _ inner@(PInfix {}) -> inner
    PParen _ inner@(PTypeSig {}) -> inner
    PParen _ inner@(PRecord {}) -> inner
    other -> other

-- | Normalize the inner pattern of a strict (!) or irrefutable (~) pattern.
-- The pretty-printer's prettyPatternAtomStrict adds PParen around PNegLit,
-- PStrict, and PIrrefutable to avoid ambiguity. Strip those added parens.
normalizeUnaryPatInner :: Pattern -> Pattern
normalizeUnaryPatInner pat =
  case normalizePattern pat of
    PParen _ inner@(PCon {}) -> inner
    PParen _ inner@(PNegLit {}) -> inner
    PParen _ inner@(PStrict {}) -> inner
    PParen _ inner@(PIrrefutable {}) -> inner
    other -> other

normalizeLiteral :: Literal -> Literal
normalizeLiteral lit =
  case lit of
    LitInt _ value repr -> LitInt span0 value repr
    LitIntHash _ value repr -> LitIntHash span0 value repr
    LitIntBase _ value repr -> LitIntBase span0 value repr
    LitIntBaseHash _ value repr -> LitIntBaseHash span0 value repr
    LitFloat _ value repr -> LitFloat span0 value repr
    LitFloatHash _ value repr -> LitFloatHash span0 value repr
    LitChar _ value repr -> LitChar span0 value repr
    LitCharHash _ value repr -> LitCharHash span0 value repr
    LitString _ value repr -> LitString span0 value repr
    LitStringHash _ value repr -> LitStringHash span0 value repr

normalizeDecl :: Decl -> Decl
normalizeDecl decl =
  case decl of
    DeclAnn ann sub -> DeclAnn ann (normalizeDecl sub)
    DeclValue _ vdecl -> DeclValue span0 (normalizeValueDecl vdecl)
    DeclTypeSig _ names ty -> DeclTypeSig span0 names (normalizeType ty)
    DeclPatSyn _ ps -> DeclPatSyn span0 (normalizePatSynDecl ps)
    DeclPatSynSig _ names ty -> DeclPatSynSig span0 names (normalizeType ty)
    DeclStandaloneKindSig _ name kind -> DeclStandaloneKindSig span0 name (normalizeType kind)
    DeclFixity _ assoc mNs prec ops -> DeclFixity span0 assoc mNs prec ops
    DeclRoleAnnotation _ ann -> DeclRoleAnnotation span0 (normalizeRoleAnnotation ann)
    DeclTypeSyn _ synDecl -> DeclTypeSyn span0 (normalizeTypeSynDecl synDecl)
    DeclTypeData _ dataDecl -> DeclTypeData span0 (normalizeDataDecl dataDecl)
    DeclData _ dataDecl -> DeclData span0 (normalizeDataDecl dataDecl)
    DeclNewtype _ newtypeDecl -> DeclNewtype span0 (normalizeNewtypeDecl newtypeDecl)
    DeclClass _ classDecl -> DeclClass span0 (normalizeClassDecl classDecl)
    DeclInstance _ instanceDecl -> DeclInstance span0 (normalizeInstanceDecl instanceDecl)
    DeclStandaloneDeriving _ derivingDecl -> DeclStandaloneDeriving span0 (normalizeStandaloneDerivingDecl derivingDecl)
    DeclDefault _ tys -> DeclDefault span0 (map normalizeType tys)
    DeclForeign _ foreignDecl -> DeclForeign span0 (normalizeForeignDecl foreignDecl)
    DeclSplice _ body -> DeclSplice span0 (normalizeExpr body)
    DeclTypeFamilyDecl _ tf -> DeclTypeFamilyDecl span0 (normalizeTypeFamilyDecl tf)
    DeclDataFamilyDecl _ df -> DeclDataFamilyDecl span0 (normalizeDataFamilyDecl df)
    DeclTypeFamilyInst _ tfi -> DeclTypeFamilyInst span0 (normalizeTypeFamilyInst tfi)
    DeclDataFamilyInst _ dfi -> DeclDataFamilyInst span0 (normalizeDataFamilyInst dfi)
    DeclPragma _ pragma -> DeclPragma span0 pragma

normalizeValueDecl :: ValueDecl -> ValueDecl
normalizeValueDecl vdecl =
  case vdecl of
    PatternBind _ pat rhs -> PatternBind span0 (normalizePattern pat) (normalizeRhs rhs)
    FunctionBind _ name [Match {matchHeadForm = MatchHeadPrefix, matchPats = [], matchRhs = rhs}] ->
      PatternBind span0 (PVar span0 name) (normalizeRhs rhs)
    FunctionBind _ name matches -> FunctionBind span0 name (map normalizeMatch matches)

normalizeMatch :: Match -> Match
normalizeMatch m =
  Match
    { matchSpan = span0,
      matchHeadForm = matchHeadForm m,
      matchPats = map normalizeFunctionHeadPat (matchPats m),
      matchRhs = normalizeRhs (matchRhs m)
    }

-- | Normalize a pattern in function-head argument position.
-- The pretty-printer wraps constructor applications, infix patterns, type
-- signatures, records, and negative literals in parens when they appear as
-- head arguments so the parser does not split them into multiple patterns.
normalizeFunctionHeadPat :: Pattern -> Pattern
normalizeFunctionHeadPat pat =
  case normalizePattern pat of
    PParen _ inner@(PNegLit {}) -> inner
    PParen _ inner@(PCon {}) -> inner
    PParen _ inner@(PInfix {}) -> inner
    PParen _ inner@(PTypeSig {}) -> inner
    PParen _ inner@(PRecord {}) -> inner
    other -> other

normalizeDoStmt :: DoStmt Expr -> DoStmt Expr
normalizeDoStmt stmt =
  case stmt of
    DoBind _ pat e -> DoBind span0 (normalizePattern pat) (normalizeExpr e)
    DoLetDecls _ decls -> DoLetDecls span0 (map normalizeDecl decls)
    DoExpr _ e -> DoExpr span0 (normalizeExpr e)
    DoRecStmt _ stmts -> DoRecStmt span0 (map normalizeDoStmt stmts)

normalizeCompStmt :: CompStmt -> CompStmt
normalizeCompStmt stmt =
  case stmt of
    CompGen _ pat e -> CompGen span0 (normalizePattern pat) (normalizeExpr e)
    CompGuard _ e -> CompGuard span0 (normalizeExpr e)
    CompLet _ bindings -> CompLet span0 [(name, normalizeExpr e) | (name, e) <- bindings]
    CompLetDecls _ decls -> CompLetDecls span0 (map normalizeDecl decls)

normalizeArithSeq :: ArithSeq -> ArithSeq
normalizeArithSeq seq' =
  case seq' of
    ArithSeqFrom _ from -> ArithSeqFrom span0 (normalizeExpr from)
    ArithSeqFromThen _ from thenE -> ArithSeqFromThen span0 (normalizeExpr from) (normalizeExpr thenE)
    ArithSeqFromTo _ from to -> ArithSeqFromTo span0 (normalizeExpr from) (normalizeExpr to)
    ArithSeqFromThenTo _ from thenE to -> ArithSeqFromThenTo span0 (normalizeExpr from) (normalizeExpr thenE) (normalizeExpr to)

normalizeType :: Type -> Type
normalizeType ty =
  case ty of
    TVar _ name -> TVar span0 name
    TCon _ name promoted -> TCon span0 name promoted
    TImplicitParam _ name inner -> TImplicitParam span0 name (normalizeType inner)
    TTypeLit _ lit -> TTypeLit span0 lit
    TStar _ -> TStar span0
    TQuasiQuote _ quoter body -> TQuasiQuote span0 quoter body
    TForall _ binders inner -> TForall span0 (map normalizeTyVarBinder binders) (normalizeType inner)
    TApp _ fn arg -> TApp span0 (normalizeType fn) (normalizeType arg)
    TFun _ lhs rhs -> TFun span0 (normalizeType lhs) (normalizeType rhs)
    TTuple _ tupleFlavor promoted elems -> TTuple span0 tupleFlavor promoted (map normalizeType elems)
    TList _ promoted elems -> TList span0 promoted (map normalizeType elems)
    -- Remove redundant parentheses from types
    TParen _ inner -> normalizeType inner
    TKindSig _ inner kind -> TKindSig span0 (normalizeType inner) (normalizeType kind)
    TUnboxedSum _ elems -> TUnboxedSum span0 (map normalizeType elems)
    TContext _ constraints inner -> TContext span0 (map normalizeType constraints) (normalizeType inner)
    TSplice _ body -> TSplice span0 (normalizeExpr body)
    TWildcard _ -> TWildcard span0
    TAnn ann sub -> TAnn ann (normalizeType sub)

normalizeTyVarBinder :: TyVarBinder -> TyVarBinder
normalizeTyVarBinder tvb =
  tvb
    { tyVarBinderSpan = span0,
      tyVarBinderKind = fmap normalizeType (tyVarBinderKind tvb)
    }

normalizeWarningText :: WarningText -> WarningText
normalizeWarningText wt =
  case wt of
    DeprText _ msg -> DeprText span0 msg
    WarnText _ msg -> WarnText span0 msg

normalizePatSynDecl :: PatSynDecl -> PatSynDecl
normalizePatSynDecl ps =
  PatSynDecl
    { patSynDeclSpan = span0,
      patSynDeclName = patSynDeclName ps,
      patSynDeclArgs = patSynDeclArgs ps,
      patSynDeclPat = normalizePattern (patSynDeclPat ps),
      patSynDeclDir = normalizePatSynDir (patSynDeclDir ps)
    }

normalizePatSynDir :: PatSynDir -> PatSynDir
normalizePatSynDir dir =
  case dir of
    PatSynUnidirectional -> PatSynUnidirectional
    PatSynBidirectional -> PatSynBidirectional
    PatSynExplicitBidirectional matches -> PatSynExplicitBidirectional (map normalizeMatch matches)

normalizeRoleAnnotation :: RoleAnnotation -> RoleAnnotation
normalizeRoleAnnotation ann = ann {roleAnnotationSpan = span0}

normalizeTypeSynDecl :: TypeSynDecl -> TypeSynDecl
normalizeTypeSynDecl decl =
  TypeSynDecl
    { typeSynSpan = span0,
      typeSynHeadForm = typeSynHeadForm decl,
      typeSynName = typeSynName decl,
      typeSynParams = map normalizeTyVarBinder (typeSynParams decl),
      typeSynBody = normalizeType (typeSynBody decl)
    }

normalizeDataDecl :: DataDecl -> DataDecl
normalizeDataDecl decl =
  DataDecl
    { dataDeclSpan = span0,
      dataDeclHeadForm = dataDeclHeadForm decl,
      dataDeclContext = map normalizeType (dataDeclContext decl),
      dataDeclName = dataDeclName decl,
      dataDeclParams = map normalizeTyVarBinder (dataDeclParams decl),
      dataDeclKind = fmap normalizeType (dataDeclKind decl),
      dataDeclConstructors = map normalizeDataConDecl (dataDeclConstructors decl),
      dataDeclDeriving = map normalizeDerivingClause (dataDeclDeriving decl)
    }

normalizeNewtypeDecl :: NewtypeDecl -> NewtypeDecl
normalizeNewtypeDecl decl =
  NewtypeDecl
    { newtypeDeclSpan = span0,
      newtypeDeclHeadForm = newtypeDeclHeadForm decl,
      newtypeDeclContext = map normalizeType (newtypeDeclContext decl),
      newtypeDeclName = newtypeDeclName decl,
      newtypeDeclParams = map normalizeTyVarBinder (newtypeDeclParams decl),
      newtypeDeclKind = fmap normalizeType (newtypeDeclKind decl),
      newtypeDeclConstructor = fmap normalizeDataConDecl (newtypeDeclConstructor decl),
      newtypeDeclDeriving = map normalizeDerivingClause (newtypeDeclDeriving decl)
    }

normalizeDataConDecl :: DataConDecl -> DataConDecl
normalizeDataConDecl con =
  case con of
    PrefixCon _ forallVars constraints name fields ->
      PrefixCon span0 forallVars (map normalizeType constraints) name (map normalizeBangType fields)
    InfixCon _ forallVars constraints lhs op rhs ->
      InfixCon span0 forallVars (map normalizeType constraints) (normalizeBangType lhs) op (normalizeBangType rhs)
    RecordCon _ forallVars constraints name fields ->
      RecordCon span0 forallVars (map normalizeType constraints) name (map normalizeFieldDecl fields)
    GadtCon _ forallBinders constraints names body ->
      GadtCon span0 (map normalizeTyVarBinder forallBinders) (map normalizeType constraints) names (normalizeGadtBody body)

normalizeBangType :: BangType -> BangType
normalizeBangType bt =
  BangType
    { bangSpan = span0,
      bangSourceUnpackedness = bangSourceUnpackedness bt,
      bangStrict = bangStrict bt,
      bangLazy = bangLazy bt,
      bangType = normalizeType (bangType bt)
    }

normalizeFieldDecl :: FieldDecl -> FieldDecl
normalizeFieldDecl fd =
  FieldDecl
    { fieldSpan = span0,
      fieldNames = fieldNames fd,
      fieldType = normalizeBangType (fieldType fd)
    }

normalizeGadtBody :: GadtBody -> GadtBody
normalizeGadtBody body =
  case body of
    GadtPrefixBody fields resultTy -> GadtPrefixBody (map normalizeBangType fields) (normalizeType resultTy)
    GadtRecordBody fields resultTy -> GadtRecordBody (map normalizeFieldDecl fields) (normalizeType resultTy)

normalizeDerivingClause :: DerivingClause -> DerivingClause
normalizeDerivingClause dc =
  DerivingClause
    { derivingStrategy = derivingStrategy dc,
      derivingClasses = map normalizeType (derivingClasses dc),
      derivingViaType = fmap normalizeType (derivingViaType dc),
      derivingParenthesized = derivingParenthesized dc
    }

normalizeClassDecl :: ClassDecl -> ClassDecl
normalizeClassDecl decl =
  ClassDecl
    { classDeclSpan = span0,
      classDeclContext = fmap (map normalizeType) (classDeclContext decl),
      classDeclHeadForm = classDeclHeadForm decl,
      classDeclName = classDeclName decl,
      classDeclParams = map normalizeTyVarBinder (classDeclParams decl),
      classDeclFundeps = map normalizeFunctionalDependency (classDeclFundeps decl),
      classDeclItems = map normalizeClassDeclItem (classDeclItems decl)
    }

normalizeFunctionalDependency :: FunctionalDependency -> FunctionalDependency
normalizeFunctionalDependency dep = dep {functionalDependencySpan = span0}

normalizeClassDeclItem :: ClassDeclItem -> ClassDeclItem
normalizeClassDeclItem item =
  case item of
    ClassItemTypeSig _ names ty -> ClassItemTypeSig span0 names (normalizeType ty)
    ClassItemDefaultSig _ name ty -> ClassItemDefaultSig span0 name (normalizeType ty)
    ClassItemFixity _ assoc mNs prec ops -> ClassItemFixity span0 assoc mNs prec ops
    ClassItemDefault _ vdecl -> ClassItemDefault span0 (normalizeValueDecl vdecl)
    ClassItemTypeFamilyDecl _ tf -> ClassItemTypeFamilyDecl span0 (normalizeTypeFamilyDecl tf)
    ClassItemDataFamilyDecl _ df -> ClassItemDataFamilyDecl span0 (normalizeDataFamilyDecl df)
    ClassItemDefaultTypeInst _ tfi -> ClassItemDefaultTypeInst span0 (normalizeTypeFamilyInst tfi)
    ClassItemPragma _ pragma -> ClassItemPragma span0 pragma

normalizeInstanceDecl :: InstanceDecl -> InstanceDecl
normalizeInstanceDecl decl =
  InstanceDecl
    { instanceDeclSpan = span0,
      instanceDeclOverlapPragma = instanceDeclOverlapPragma decl,
      instanceDeclWarning = fmap normalizeWarningText (instanceDeclWarning decl),
      instanceDeclForall = map normalizeTyVarBinder (instanceDeclForall decl),
      instanceDeclContext = map normalizeType (instanceDeclContext decl),
      instanceDeclParenthesizedHead = instanceDeclParenthesizedHead decl,
      instanceDeclHeadForm = instanceDeclHeadForm decl,
      instanceDeclClassName = instanceDeclClassName decl,
      instanceDeclTypes = map normalizeType (instanceDeclTypes decl),
      instanceDeclItems = map normalizeInstanceDeclItem (instanceDeclItems decl)
    }

normalizeInstanceDeclItem :: InstanceDeclItem -> InstanceDeclItem
normalizeInstanceDeclItem item =
  case item of
    InstanceItemBind _ vdecl -> InstanceItemBind span0 (normalizeValueDecl vdecl)
    InstanceItemTypeSig _ names ty -> InstanceItemTypeSig span0 names (normalizeType ty)
    InstanceItemFixity _ assoc mNs prec ops -> InstanceItemFixity span0 assoc mNs prec ops
    InstanceItemTypeFamilyInst _ tfi -> InstanceItemTypeFamilyInst span0 (normalizeTypeFamilyInst tfi)
    InstanceItemDataFamilyInst _ dfi -> InstanceItemDataFamilyInst span0 (normalizeDataFamilyInst dfi)
    InstanceItemPragma _ pragma -> InstanceItemPragma span0 pragma

normalizeStandaloneDerivingDecl :: StandaloneDerivingDecl -> StandaloneDerivingDecl
normalizeStandaloneDerivingDecl decl =
  StandaloneDerivingDecl
    { standaloneDerivingSpan = span0,
      standaloneDerivingStrategy = standaloneDerivingStrategy decl,
      standaloneDerivingViaType = fmap normalizeType (standaloneDerivingViaType decl),
      standaloneDerivingOverlapPragma = standaloneDerivingOverlapPragma decl,
      standaloneDerivingWarning = fmap normalizeWarningText (standaloneDerivingWarning decl),
      standaloneDerivingForall = map normalizeTyVarBinder (standaloneDerivingForall decl),
      standaloneDerivingContext = map normalizeType (standaloneDerivingContext decl),
      standaloneDerivingParenthesizedHead = standaloneDerivingParenthesizedHead decl,
      standaloneDerivingHeadForm = standaloneDerivingHeadForm decl,
      standaloneDerivingClassName = standaloneDerivingClassName decl,
      standaloneDerivingTypes = map normalizeType (standaloneDerivingTypes decl)
    }

normalizeForeignDecl :: ForeignDecl -> ForeignDecl
normalizeForeignDecl decl =
  ForeignDecl
    { foreignDeclSpan = span0,
      foreignDirection = foreignDirection decl,
      foreignCallConv = foreignCallConv decl,
      foreignSafety = foreignSafety decl,
      foreignEntity = foreignEntity decl,
      foreignName = foreignName decl,
      foreignType = normalizeType (foreignType decl)
    }

normalizeTypeFamilyDecl :: TypeFamilyDecl -> TypeFamilyDecl
normalizeTypeFamilyDecl tf =
  TypeFamilyDecl
    { typeFamilyDeclSpan = span0,
      typeFamilyDeclHeadForm = typeFamilyDeclHeadForm tf,
      typeFamilyDeclHead = normalizeType (typeFamilyDeclHead tf),
      typeFamilyDeclParams = map normalizeTyVarBinder (typeFamilyDeclParams tf),
      typeFamilyDeclKind = fmap normalizeType (typeFamilyDeclKind tf),
      typeFamilyDeclEquations = fmap (map normalizeTypeFamilyEq) (typeFamilyDeclEquations tf)
    }

normalizeTypeFamilyEq :: TypeFamilyEq -> TypeFamilyEq
normalizeTypeFamilyEq eq =
  TypeFamilyEq
    { typeFamilyEqSpan = span0,
      typeFamilyEqForall = map normalizeTyVarBinder (typeFamilyEqForall eq),
      typeFamilyEqHeadForm = typeFamilyEqHeadForm eq,
      typeFamilyEqLhs = normalizeType (typeFamilyEqLhs eq),
      typeFamilyEqRhs = normalizeType (typeFamilyEqRhs eq)
    }

normalizeDataFamilyDecl :: DataFamilyDecl -> DataFamilyDecl
normalizeDataFamilyDecl df =
  DataFamilyDecl
    { dataFamilyDeclSpan = span0,
      dataFamilyDeclName = dataFamilyDeclName df,
      dataFamilyDeclParams = map normalizeTyVarBinder (dataFamilyDeclParams df),
      dataFamilyDeclKind = fmap normalizeType (dataFamilyDeclKind df)
    }

normalizeTypeFamilyInst :: TypeFamilyInst -> TypeFamilyInst
normalizeTypeFamilyInst tfi =
  TypeFamilyInst
    { typeFamilyInstSpan = span0,
      typeFamilyInstForall = map normalizeTyVarBinder (typeFamilyInstForall tfi),
      typeFamilyInstHeadForm = typeFamilyInstHeadForm tfi,
      typeFamilyInstLhs = normalizeType (typeFamilyInstLhs tfi),
      typeFamilyInstRhs = normalizeType (typeFamilyInstRhs tfi)
    }

normalizeDataFamilyInst :: DataFamilyInst -> DataFamilyInst
normalizeDataFamilyInst dfi =
  DataFamilyInst
    { dataFamilyInstSpan = span0,
      dataFamilyInstIsNewtype = dataFamilyInstIsNewtype dfi,
      dataFamilyInstForall = map normalizeTyVarBinder (dataFamilyInstForall dfi),
      dataFamilyInstHead = normalizeType (dataFamilyInstHead dfi),
      dataFamilyInstConstructors = map normalizeDataConDecl (dataFamilyInstConstructors dfi),
      dataFamilyInstDeriving = map normalizeDerivingClause (dataFamilyInstDeriving dfi)
    }
