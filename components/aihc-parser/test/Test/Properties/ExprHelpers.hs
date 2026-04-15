{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

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
    EVar name -> exprAnnSpan span0 (EVar name)
    EInt value _ -> exprAnnSpan span0 (EInt value (T.pack (show value)))
    EIntHash value repr -> exprAnnSpan span0 (EIntHash value repr)
    EIntBase value repr -> exprAnnSpan span0 (EIntBase value repr)
    EIntBaseHash value repr -> exprAnnSpan span0 (EIntBaseHash value repr)
    EFloat value repr -> exprAnnSpan span0 (EFloat value repr)
    EFloatHash value repr -> exprAnnSpan span0 (EFloatHash value repr)
    EChar value repr -> exprAnnSpan span0 (EChar value repr)
    ECharHash value repr -> exprAnnSpan span0 (ECharHash value repr)
    EString value repr -> exprAnnSpan span0 (EString value repr)
    EStringHash value repr -> exprAnnSpan span0 (EStringHash value repr)
    EOverloadedLabel value repr -> exprAnnSpan span0 (EOverloadedLabel value repr)
    EQuasiQuote quoter body -> exprAnnSpan span0 (EQuasiQuote quoter body)
    EApp fn arg -> exprAnnSpan span0 (EApp (normalizeExpr fn) (normalizeExpr arg))
    EInfix lhs op rhs -> exprAnnSpan span0 (EInfix (normalizeExpr lhs) op (normalizeExpr rhs))
    ENegate inner -> exprAnnSpan span0 (ENegate (normalizeExpr inner))
    ESectionL inner op -> exprAnnSpan span0 (ESectionL (normalizeExpr inner) op)
    ESectionR op inner -> exprAnnSpan span0 (ESectionR op (normalizeExpr inner))
    EIf cond thenE elseE -> exprAnnSpan span0 (EIf (normalizeExpr cond) (normalizeExpr thenE) (normalizeExpr elseE))
    EMultiWayIf rhss -> exprAnnSpan span0 (EMultiWayIf (map normalizeGuardedRhs rhss))
    ECase scrutinee alts -> exprAnnSpan span0 (ECase (normalizeExpr scrutinee) (map normalizeCaseAlt alts))
    ELambdaPats pats body -> exprAnnSpan span0 (ELambdaPats (map normalizeLambdaPat pats) (normalizeExpr body))
    ELambdaCase alts -> exprAnnSpan span0 (ELambdaCase (map normalizeCaseAlt alts))
    ELetDecls decls body -> exprAnnSpan span0 (ELetDecls (map normalizeDecl decls) (normalizeExpr body))
    EDo stmts isMdo -> exprAnnSpan span0 (EDo (map normalizeDoStmt stmts) isMdo)
    EListComp body stmts -> exprAnnSpan span0 (EListComp (normalizeExpr body) (map normalizeCompStmt stmts))
    EListCompParallel body stmtss -> exprAnnSpan span0 (EListCompParallel (normalizeExpr body) (map (map normalizeCompStmt) stmtss))
    EList elems -> exprAnnSpan span0 (EList (map normalizeExpr elems))
    ETuple tupleFlavor elems -> exprAnnSpan span0 (ETuple tupleFlavor (map (fmap normalizeExpr) elems))
    EArithSeq seq' -> exprAnnSpan span0 (EArithSeq (normalizeArithSeq seq'))
    ERecordCon con fields rwc -> exprAnnSpan span0 (ERecordCon con [(name, normalizeExpr e) | (name, e) <- fields] rwc)
    ERecordUpd target fields -> exprAnnSpan span0 (ERecordUpd (normalizeExpr target) [(name, normalizeExpr e) | (name, e) <- fields])
    ETypeSig inner ty -> exprAnnSpan span0 (ETypeSig (normalizeExpr inner) (normalizeType ty))
    ETypeApp inner ty -> exprAnnSpan span0 (ETypeApp (normalizeExpr inner) (normalizeType ty))
    EUnboxedSum altIdx arity inner -> exprAnnSpan span0 (EUnboxedSum altIdx arity (normalizeExpr inner))
    EParen inner -> normalizeExpr inner
    ETHExpQuote body -> exprAnnSpan span0 (ETHExpQuote (normalizeExpr body))
    ETHTypedQuote body -> exprAnnSpan span0 (ETHTypedQuote (normalizeExpr body))
    ETHDeclQuote decls -> exprAnnSpan span0 (ETHDeclQuote (map normalizeDecl decls))
    ETHTypeQuote ty -> exprAnnSpan span0 (ETHTypeQuote (normalizeType ty))
    ETHPatQuote pat -> exprAnnSpan span0 (ETHPatQuote (normalizePattern pat))
    ETHNameQuote name -> exprAnnSpan span0 (ETHNameQuote name)
    ETHTypeNameQuote name -> exprAnnSpan span0 (ETHTypeNameQuote name)
    ETHSplice body -> exprAnnSpan span0 (ETHSplice (normalizeExpr body))
    ETHTypedSplice body -> exprAnnSpan span0 (ETHTypedSplice (normalizeExpr body))
    EProc pat body -> exprAnnSpan span0 (EProc (normalizePattern pat) body)
    EAnn ann sub
      | Just _ <- fromAnnotation @SourceSpan ann -> normalizeExpr sub
      | otherwise -> EAnn ann (normalizeExpr sub)

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
    PVar name -> patternAnnSpan span0 (PVar name)
    PWildcard -> patternAnnSpan span0 PWildcard
    PLit lit -> patternAnnSpan span0 (PLit (normalizeLiteral lit))
    PQuasiQuote quoter body -> patternAnnSpan span0 (PQuasiQuote quoter body)
    PTuple tupleFlavor elems -> patternAnnSpan span0 (PTuple tupleFlavor (map normalizePattern elems))
    PList elems -> patternAnnSpan span0 (PList (map normalizePattern elems))
    PCon con args -> patternAnnSpan span0 (PCon con (map normalizePattern args))
    PInfix lhs op rhs -> patternAnnSpan span0 (PInfix (normalizePattern lhs) op (normalizePattern rhs))
    PView e inner -> patternAnnSpan span0 (PView (normalizeExpr e) (normalizePattern inner))
    PAs name inner -> patternAnnSpan span0 (PAs name (normalizeUnaryPatInner inner))
    PStrict inner -> patternAnnSpan span0 (PStrict (normalizeUnaryPatInner inner))
    PIrrefutable inner -> patternAnnSpan span0 (PIrrefutable (normalizeUnaryPatInner inner))
    PNegLit lit -> patternAnnSpan span0 (PNegLit (normalizeLiteral lit))
    PParen inner -> patternAnnSpan span0 (PParen (normalizePattern inner))
    PUnboxedSum altIdx arity inner -> patternAnnSpan span0 (PUnboxedSum altIdx arity (normalizePattern inner))
    PRecord con fields rwc -> patternAnnSpan span0 (PRecord con [(name, normalizePattern p) | (name, p) <- fields] rwc)
    PTypeSig inner ty -> patternAnnSpan span0 (PTypeSig (normalizePattern inner) (normalizeType ty))
    PSplice body -> patternAnnSpan span0 (PSplice (normalizeExpr body))

-- | Normalize a pattern in lambda argument position.
-- The pretty-printer uses prettyLambdaPatternAtom for lambda patterns, which
-- wraps certain patterns in PParen beyond what prettyPatternAtom does:
-- PNegLit (ambiguous with subtraction) and nullary PCon (greedily absorbs
-- next argument). It also inherits prettyPatternAtom's parenthesization of
-- non-atomic patterns like PCon with args, PInfix, PTypeSig, PRecord.
-- Strip that added PParen so generated and parsed forms match.
normalizeLambdaPat :: Pattern -> Pattern
normalizeLambdaPat = stripWrappedParenIf isLambdaWrappedPattern

-- | Normalize the inner pattern of a strict (!) or irrefutable (~) pattern.
-- The pretty-printer's prettyPatternAtomStrict adds PParen around PNegLit,
-- PStrict, and PIrrefutable to avoid ambiguity. Strip those added parens.
normalizeUnaryPatInner :: Pattern -> Pattern
normalizeUnaryPatInner = stripWrappedParenIf isUnaryWrappedPattern

normalizeLiteral :: Literal -> Literal
normalizeLiteral lit =
  case peelLiteralAnn lit of
    LitInt value repr -> literalAnnSpan span0 (LitInt value repr)
    LitIntHash value repr -> literalAnnSpan span0 (LitIntHash value repr)
    LitIntBase value repr -> literalAnnSpan span0 (LitIntBase value repr)
    LitIntBaseHash value repr -> literalAnnSpan span0 (LitIntBaseHash value repr)
    LitFloat value repr -> literalAnnSpan span0 (LitFloat value repr)
    LitFloatHash value repr -> literalAnnSpan span0 (LitFloatHash value repr)
    LitChar value repr -> literalAnnSpan span0 (LitChar value repr)
    LitCharHash value repr -> literalAnnSpan span0 (LitCharHash value repr)
    LitString value repr -> literalAnnSpan span0 (LitString value repr)
    LitStringHash value repr -> literalAnnSpan span0 (LitStringHash value repr)
    LitAnn {} -> error "unreachable"

normalizeDecl :: Decl -> Decl
normalizeDecl decl =
  case decl of
    DeclAnn _ sub -> normalizeDecl sub
    DeclValue vdecl -> DeclValue (normalizeValueDecl vdecl)
    DeclTypeSig names ty -> DeclTypeSig names (normalizeType ty)
    DeclPatSyn ps -> DeclPatSyn (normalizePatSynDecl ps)
    DeclPatSynSig names ty -> DeclPatSynSig names (normalizeType ty)
    DeclStandaloneKindSig name kind -> DeclStandaloneKindSig name (normalizeType kind)
    DeclFixity assoc mNs prec ops -> DeclFixity assoc mNs prec ops
    DeclRoleAnnotation ann -> DeclRoleAnnotation (normalizeRoleAnnotation ann)
    DeclTypeSyn synDecl -> DeclTypeSyn (normalizeTypeSynDecl synDecl)
    DeclTypeData dataDecl -> DeclTypeData (normalizeDataDecl dataDecl)
    DeclData dataDecl -> DeclData (normalizeDataDecl dataDecl)
    DeclNewtype newtypeDecl -> DeclNewtype (normalizeNewtypeDecl newtypeDecl)
    DeclClass classDecl -> DeclClass (normalizeClassDecl classDecl)
    DeclInstance instanceDecl -> DeclInstance (normalizeInstanceDecl instanceDecl)
    DeclStandaloneDeriving derivingDecl -> DeclStandaloneDeriving (normalizeStandaloneDerivingDecl derivingDecl)
    DeclDefault tys -> DeclDefault (map normalizeType tys)
    DeclForeign foreignDecl -> DeclForeign (normalizeForeignDecl foreignDecl)
    DeclSplice body -> DeclSplice (normalizeExpr body)
    DeclTypeFamilyDecl tf -> DeclTypeFamilyDecl (normalizeTypeFamilyDecl tf)
    DeclDataFamilyDecl df -> DeclDataFamilyDecl (normalizeDataFamilyDecl df)
    DeclTypeFamilyInst tfi -> DeclTypeFamilyInst (normalizeTypeFamilyInst tfi)
    DeclDataFamilyInst dfi -> DeclDataFamilyInst (normalizeDataFamilyInst dfi)
    DeclPragma pragma -> DeclPragma pragma

normalizeValueDecl :: ValueDecl -> ValueDecl
normalizeValueDecl vdecl =
  case vdecl of
    PatternBind _ pat rhs -> PatternBind span0 (normalizePattern pat) (normalizeRhs rhs)
    FunctionBind _ name [Match {matchHeadForm = MatchHeadPrefix, matchPats = [], matchRhs = rhs}] ->
      PatternBind span0 (patternAnnSpan span0 (PVar name)) (normalizeRhs rhs)
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
normalizeFunctionHeadPat = stripWrappedParenIf isFunctionHeadWrappedPattern

stripWrappedParenIf :: (Pattern -> Bool) -> Pattern -> Pattern
stripWrappedParenIf predicate pat =
  let normalized = normalizePattern pat
   in case peelPatternAnn normalized of
        PParen inner
          | predicate (peelPatternAnn inner) -> inner
        _ -> normalized

isLambdaWrappedPattern :: Pattern -> Bool
isLambdaWrappedPattern = \case
  PNegLit {} -> True
  PCon {} -> True
  PInfix {} -> True
  PTypeSig {} -> True
  PRecord {} -> True
  PView {} -> True
  _ -> False

isUnaryWrappedPattern :: Pattern -> Bool
isUnaryWrappedPattern = \case
  PCon {} -> True
  PNegLit {} -> True
  PStrict {} -> True
  PIrrefutable {} -> True
  PView {} -> True
  PAs {} -> True
  _ -> False

isFunctionHeadWrappedPattern :: Pattern -> Bool
isFunctionHeadWrappedPattern = \case
  PNegLit {} -> True
  PCon {} -> True
  PInfix {} -> True
  PTypeSig {} -> True
  PRecord {} -> True
  PView {} -> True
  _ -> False

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
    TAnn ann sub
      | Just _ <- fromAnnotation @SourceSpan ann -> normalizeType sub
      | otherwise -> TAnn ann (normalizeType sub)

normalizeTyVarBinder :: TyVarBinder -> TyVarBinder
normalizeTyVarBinder tvb =
  tvb
    { tyVarBinderSpan = span0,
      tyVarBinderKind = fmap normalizeType (tyVarBinderKind tvb)
    }

normalizeWarningText :: WarningText -> WarningText
normalizeWarningText wt =
  case wt of
    DeprText msg -> DeprText msg
    WarnText msg -> WarnText msg
    WarningTextAnn _ sub -> normalizeWarningText sub

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
    ClassItemAnn _ sub -> normalizeClassDeclItem sub
    ClassItemTypeSig names ty -> classItemAnnSpan span0 (ClassItemTypeSig names (normalizeType ty))
    ClassItemDefaultSig name ty -> classItemAnnSpan span0 (ClassItemDefaultSig name (normalizeType ty))
    ClassItemFixity assoc mNs prec ops -> classItemAnnSpan span0 (ClassItemFixity assoc mNs prec ops)
    ClassItemDefault vdecl -> classItemAnnSpan span0 (ClassItemDefault (normalizeValueDecl vdecl))
    ClassItemTypeFamilyDecl tf -> classItemAnnSpan span0 (ClassItemTypeFamilyDecl (normalizeTypeFamilyDecl tf))
    ClassItemDataFamilyDecl df -> classItemAnnSpan span0 (ClassItemDataFamilyDecl (normalizeDataFamilyDecl df))
    ClassItemDefaultTypeInst tfi -> classItemAnnSpan span0 (ClassItemDefaultTypeInst (normalizeTypeFamilyInst tfi))
    ClassItemPragma pragma -> classItemAnnSpan span0 (ClassItemPragma pragma)

normalizeInstanceDecl :: InstanceDecl -> InstanceDecl
normalizeInstanceDecl decl =
  InstanceDecl
    { instanceDeclSpan = span0,
      instanceDeclOverlapPragma = instanceDeclOverlapPragma decl,
      instanceDeclWarning = fmap normalizeWarningText (instanceDeclWarning decl),
      instanceDeclForall = map normalizeTyVarBinder (instanceDeclForall decl),
      instanceDeclContext = map normalizeType (instanceDeclContext decl),
      instanceDeclParenthesizedHead = instanceDeclParenthesizedHead decl,
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
