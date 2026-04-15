{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- | Shared normalization helpers used by multiple round-trip test modules.
module Test.Properties.ExprHelpers
  ( normalizeExpr,
    normalizeDecl,
    span0,
    stripTypeSourceSpanAnnotations,
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
    EVar name -> EAnn (mkAnnotation span0) (EVar name)
    EInt value _ -> EAnn (mkAnnotation span0) (EInt value (T.pack (show value)))
    EIntHash value repr -> EAnn (mkAnnotation span0) (EIntHash value repr)
    EIntBase value repr -> EAnn (mkAnnotation span0) (EIntBase value repr)
    EIntBaseHash value repr -> EAnn (mkAnnotation span0) (EIntBaseHash value repr)
    EFloat value repr -> EAnn (mkAnnotation span0) (EFloat value repr)
    EFloatHash value repr -> EAnn (mkAnnotation span0) (EFloatHash value repr)
    EChar value repr -> EAnn (mkAnnotation span0) (EChar value repr)
    ECharHash value repr -> EAnn (mkAnnotation span0) (ECharHash value repr)
    EString value repr -> EAnn (mkAnnotation span0) (EString value repr)
    EStringHash value repr -> EAnn (mkAnnotation span0) (EStringHash value repr)
    EOverloadedLabel value repr -> EAnn (mkAnnotation span0) (EOverloadedLabel value repr)
    EQuasiQuote quoter body -> EAnn (mkAnnotation span0) (EQuasiQuote quoter body)
    EApp fn arg -> EAnn (mkAnnotation span0) (EApp (normalizeExpr fn) (normalizeExpr arg))
    EInfix lhs op rhs -> EAnn (mkAnnotation span0) (EInfix (normalizeExpr lhs) op (normalizeExpr rhs))
    ENegate inner -> EAnn (mkAnnotation span0) (ENegate (normalizeExpr inner))
    ESectionL inner op -> EAnn (mkAnnotation span0) (ESectionL (normalizeExpr inner) op)
    ESectionR op inner -> EAnn (mkAnnotation span0) (ESectionR op (normalizeExpr inner))
    EIf cond thenE elseE -> EAnn (mkAnnotation span0) (EIf (normalizeExpr cond) (normalizeExpr thenE) (normalizeExpr elseE))
    EMultiWayIf rhss -> EAnn (mkAnnotation span0) (EMultiWayIf (map normalizeGuardedRhs rhss))
    ECase scrutinee alts -> EAnn (mkAnnotation span0) (ECase (normalizeExpr scrutinee) (map normalizeCaseAlt alts))
    ELambdaPats pats body -> EAnn (mkAnnotation span0) (ELambdaPats (map normalizeLambdaPat pats) (normalizeExpr body))
    ELambdaCase alts -> EAnn (mkAnnotation span0) (ELambdaCase (map normalizeCaseAlt alts))
    ELetDecls decls body -> EAnn (mkAnnotation span0) (ELetDecls (map normalizeDecl decls) (normalizeExpr body))
    EDo stmts isMdo -> EAnn (mkAnnotation span0) (EDo (map normalizeDoStmt stmts) isMdo)
    EListComp body stmts -> EAnn (mkAnnotation span0) (EListComp (normalizeExpr body) (map normalizeCompStmt stmts))
    EListCompParallel body stmtss -> EAnn (mkAnnotation span0) (EListCompParallel (normalizeExpr body) (map (map normalizeCompStmt) stmtss))
    EList elems -> EAnn (mkAnnotation span0) (EList (map normalizeExpr elems))
    ETuple tupleFlavor elems -> EAnn (mkAnnotation span0) (ETuple tupleFlavor (map (fmap normalizeExpr) elems))
    EArithSeq seq' -> EAnn (mkAnnotation span0) (EArithSeq (normalizeArithSeq seq'))
    ERecordCon con fields rwc -> EAnn (mkAnnotation span0) (ERecordCon con [(name, normalizeExpr e) | (name, e) <- fields] rwc)
    ERecordUpd target fields -> EAnn (mkAnnotation span0) (ERecordUpd (normalizeExpr target) [(name, normalizeExpr e) | (name, e) <- fields])
    ETypeSig inner ty -> EAnn (mkAnnotation span0) (ETypeSig (normalizeExpr inner) (normalizeType ty))
    ETypeApp inner ty -> EAnn (mkAnnotation span0) (ETypeApp (normalizeExpr inner) (normalizeType ty))
    EUnboxedSum altIdx arity inner -> EAnn (mkAnnotation span0) (EUnboxedSum altIdx arity (normalizeExpr inner))
    EParen inner -> normalizeExpr inner
    ETHExpQuote body -> EAnn (mkAnnotation span0) (ETHExpQuote (normalizeExpr body))
    ETHTypedQuote body -> EAnn (mkAnnotation span0) (ETHTypedQuote (normalizeExpr body))
    ETHDeclQuote decls -> EAnn (mkAnnotation span0) (ETHDeclQuote (map normalizeDecl decls))
    ETHTypeQuote ty -> EAnn (mkAnnotation span0) (ETHTypeQuote (normalizeType ty))
    ETHPatQuote pat -> EAnn (mkAnnotation span0) (ETHPatQuote (normalizePattern pat))
    ETHNameQuote name -> EAnn (mkAnnotation span0) (ETHNameQuote name)
    ETHTypeNameQuote name -> EAnn (mkAnnotation span0) (ETHTypeNameQuote name)
    ETHSplice body -> EAnn (mkAnnotation span0) (ETHSplice (normalizeExpr body))
    ETHTypedSplice body -> EAnn (mkAnnotation span0) (ETHTypedSplice (normalizeExpr body))
    EProc pat body -> EAnn (mkAnnotation span0) (EProc (normalizePattern pat) (normalizeCmd body))
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
  GuardAnn (mkAnnotation span0) (normalizeGuardQualifierInner qual)

normalizeGuardQualifierInner :: GuardQualifier -> GuardQualifier
normalizeGuardQualifierInner (GuardAnn _ inner) = normalizeGuardQualifierInner inner
normalizeGuardQualifierInner (GuardExpr e) = GuardExpr (normalizeExpr e)
normalizeGuardQualifierInner (GuardPat pat e) = GuardPat (normalizePattern pat) (normalizeExpr e)
normalizeGuardQualifierInner (GuardLet decls) = GuardLet (map normalizeDecl decls)

normalizePattern :: Pattern -> Pattern
normalizePattern pat =
  case pat of
    PAnn _ sub -> normalizePattern sub
    PVar name -> PVar name
    PWildcard -> PWildcard
    PLit lit -> PLit (normalizeLiteral lit)
    PQuasiQuote quoter body -> PQuasiQuote quoter body
    PTuple tupleFlavor elems -> PTuple tupleFlavor (map normalizePattern elems)
    PList elems -> PList (map normalizePattern elems)
    PCon con args -> PCon con (map normalizePattern args)
    PInfix lhs op rhs -> PInfix (normalizePattern lhs) op (normalizePattern rhs)
    PView e inner -> PView (normalizeExpr e) (normalizePattern inner)
    PAs name inner -> PAs name (normalizeUnaryPatInner inner)
    PStrict inner -> PStrict (normalizeUnaryPatInner inner)
    PIrrefutable inner -> PIrrefutable (normalizeUnaryPatInner inner)
    PNegLit lit -> PNegLit (normalizeLiteral lit)
    PParen inner -> PParen (normalizePattern inner)
    PUnboxedSum altIdx arity inner -> PUnboxedSum altIdx arity (normalizePattern inner)
    PRecord con fields rwc -> PRecord con [(name, normalizePattern p) | (name, p) <- fields] rwc
    PTypeSig inner ty -> PTypeSig (normalizePattern inner) (normalizeType ty)
    PSplice body -> PSplice (normalizeExpr body)

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
    LitInt value repr -> LitInt value repr
    LitIntHash value repr -> LitIntHash value repr
    LitIntBase value repr -> LitIntBase value repr
    LitIntBaseHash value repr -> LitIntBaseHash value repr
    LitFloat value repr -> LitFloat value repr
    LitFloatHash value repr -> LitFloatHash value repr
    LitChar value repr -> LitChar value repr
    LitCharHash value repr -> LitCharHash value repr
    LitString value repr -> LitString value repr
    LitStringHash value repr -> LitStringHash value repr
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
      PatternBind span0 (PVar name) (normalizeRhs rhs)
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
  DoAnn (mkAnnotation span0) (normalizeDoStmtInner stmt)

normalizeDoStmtInner :: DoStmt Expr -> DoStmt Expr
normalizeDoStmtInner (DoAnn _ inner) = normalizeDoStmtInner inner
normalizeDoStmtInner (DoBind pat e) = DoBind (normalizePattern pat) (normalizeExpr e)
normalizeDoStmtInner (DoLetDecls decls) = DoLetDecls (map normalizeDecl decls)
normalizeDoStmtInner (DoExpr e) = DoExpr (normalizeExpr e)
normalizeDoStmtInner (DoRecStmt stmts) = DoRecStmt (map normalizeDoStmt stmts)

normalizeCmd :: Cmd -> Cmd
normalizeCmd cmd =
  CmdAnn (mkAnnotation span0) (normalizeCmdInner cmd)

normalizeCmdInner :: Cmd -> Cmd
normalizeCmdInner (CmdAnn _ inner) = normalizeCmdInner inner
normalizeCmdInner (CmdArrApp lhs appTy rhs) = CmdArrApp (normalizeExpr lhs) appTy (normalizeExpr rhs)
normalizeCmdInner (CmdInfix l op r) = CmdInfix (normalizeCmd l) op (normalizeCmd r)
normalizeCmdInner (CmdDo stmts) = CmdDo (map normalizeDoCmdStmt stmts)
normalizeCmdInner (CmdIf cond yes no) = CmdIf (normalizeExpr cond) (normalizeCmd yes) (normalizeCmd no)
normalizeCmdInner (CmdCase scrut alts) = CmdCase (normalizeExpr scrut) (map normalizeCmdCaseAlt alts)
normalizeCmdInner (CmdLet decls body) = CmdLet (map normalizeDecl decls) (normalizeCmd body)
normalizeCmdInner (CmdLam pats body) = CmdLam (map normalizePattern pats) (normalizeCmd body)
normalizeCmdInner (CmdApp c e) = CmdApp (normalizeCmd c) (normalizeExpr e)
normalizeCmdInner (CmdPar c) = CmdPar (normalizeCmd c)

normalizeDoCmdStmt :: DoStmt Cmd -> DoStmt Cmd
normalizeDoCmdStmt stmt =
  DoAnn (mkAnnotation span0) (normalizeDoCmdStmtInner stmt)

normalizeDoCmdStmtInner :: DoStmt Cmd -> DoStmt Cmd
normalizeDoCmdStmtInner (DoAnn _ inner) = normalizeDoCmdStmtInner inner
normalizeDoCmdStmtInner (DoBind pat c) = DoBind (normalizePattern pat) (normalizeCmd c)
normalizeDoCmdStmtInner (DoLetDecls decls) = DoLetDecls (map normalizeDecl decls)
normalizeDoCmdStmtInner (DoExpr c) = DoExpr (normalizeCmd c)
normalizeDoCmdStmtInner (DoRecStmt stmts) = DoRecStmt (map normalizeDoCmdStmt stmts)

normalizeCmdCaseAlt :: CmdCaseAlt -> CmdCaseAlt
normalizeCmdCaseAlt alt =
  alt
    { cmdCaseAltSpan = span0,
      cmdCaseAltPat = normalizePattern (cmdCaseAltPat alt),
      cmdCaseAltBody = normalizeCmd (cmdCaseAltBody alt)
    }

normalizeCompStmt :: CompStmt -> CompStmt
normalizeCompStmt stmt =
  CompAnn (mkAnnotation span0) (normalizeCompStmtInner stmt)

normalizeCompStmtInner :: CompStmt -> CompStmt
normalizeCompStmtInner (CompAnn _ inner) = normalizeCompStmtInner inner
normalizeCompStmtInner (CompGen pat e) = CompGen (normalizePattern pat) (normalizeExpr e)
normalizeCompStmtInner (CompGuard e) = CompGuard (normalizeExpr e)
normalizeCompStmtInner (CompLetDecls decls) = CompLetDecls (map normalizeDecl decls)

normalizeArithSeq :: ArithSeq -> ArithSeq
normalizeArithSeq seq' =
  ArithSeqAnn (mkAnnotation span0) (normalizeArithSeqInner seq')

normalizeArithSeqInner :: ArithSeq -> ArithSeq
normalizeArithSeqInner (ArithSeqAnn _ inner) = normalizeArithSeqInner inner
normalizeArithSeqInner (ArithSeqFrom from) = ArithSeqFrom (normalizeExpr from)
normalizeArithSeqInner (ArithSeqFromThen from thenE) = ArithSeqFromThen (normalizeExpr from) (normalizeExpr thenE)
normalizeArithSeqInner (ArithSeqFromTo from to) = ArithSeqFromTo (normalizeExpr from) (normalizeExpr to)
normalizeArithSeqInner (ArithSeqFromThenTo from thenE to) = ArithSeqFromThenTo (normalizeExpr from) (normalizeExpr thenE) (normalizeExpr to)

-- | Recursively remove span-only 'TAnn' wrappers (see 'typeAnnSpan') so tests
-- can pattern-match on structural 'Type' shape.
stripTypeSourceSpanAnnotations :: Type -> Type
stripTypeSourceSpanAnnotations ty =
  case ty of
    TAnn ann sub
      | Just _ <- fromAnnotation @SourceSpan ann -> stripTypeSourceSpanAnnotations sub
      | otherwise -> TAnn ann (stripTypeSourceSpanAnnotations sub)
    TVar x -> TVar x
    TCon n p -> TCon n p
    TImplicitParam nm t -> TImplicitParam nm (stripTypeSourceSpanAnnotations t)
    TTypeLit l -> TTypeLit l
    TStar -> TStar
    TQuasiQuote q b -> TQuasiQuote q b
    TForall bs t -> TForall bs (stripTypeSourceSpanAnnotations t)
    TApp a b -> TApp (stripTypeSourceSpanAnnotations a) (stripTypeSourceSpanAnnotations b)
    TFun a b -> TFun (stripTypeSourceSpanAnnotations a) (stripTypeSourceSpanAnnotations b)
    TTuple fl pr es -> TTuple fl pr (map stripTypeSourceSpanAnnotations es)
    TUnboxedSum es -> TUnboxedSum (map stripTypeSourceSpanAnnotations es)
    TList pr es -> TList pr (map stripTypeSourceSpanAnnotations es)
    TParen t -> TParen (stripTypeSourceSpanAnnotations t)
    TKindSig a b -> TKindSig (stripTypeSourceSpanAnnotations a) (stripTypeSourceSpanAnnotations b)
    TContext cs t -> TContext (map stripTypeSourceSpanAnnotations cs) (stripTypeSourceSpanAnnotations t)
    TSplice e -> TSplice e
    TWildcard -> TWildcard

normalizeType :: Type -> Type
normalizeType ty =
  case ty of
    TVar name -> TVar name
    TCon name promoted -> TCon name promoted
    TImplicitParam name inner -> TImplicitParam name (normalizeType inner)
    TTypeLit lit -> TTypeLit lit
    TStar -> TStar
    TQuasiQuote quoter body -> TQuasiQuote quoter body
    TForall binders inner -> TForall (map normalizeTyVarBinder binders) (normalizeType inner)
    TApp fn arg -> TApp (normalizeType fn) (normalizeType arg)
    TFun lhs rhs -> TFun (normalizeType lhs) (normalizeType rhs)
    TTuple tupleFlavor promoted elems -> TTuple tupleFlavor promoted (map normalizeType elems)
    TList promoted elems -> TList promoted (map normalizeType elems)
    -- Remove redundant parentheses from types
    TParen inner -> normalizeType inner
    TKindSig inner kind -> TKindSig (normalizeType inner) (normalizeType kind)
    TUnboxedSum elems -> TUnboxedSum (map normalizeType elems)
    TContext constraints inner -> TContext (map normalizeType constraints) (normalizeType inner)
    TSplice body -> TSplice (normalizeExpr body)
    TWildcard -> TWildcard
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
  DataConAnn (mkAnnotation span0) (normalizeDataConInner con)

-- | Normalize constructor shape; peels nested 'DataConAnn'.
normalizeDataConInner :: DataConDecl -> DataConDecl
normalizeDataConInner (DataConAnn _ inner) = normalizeDataConInner inner
normalizeDataConInner (PrefixCon forallVars constraints name fields) =
  PrefixCon forallVars (map normalizeType constraints) name (map normalizeBangType fields)
normalizeDataConInner (InfixCon forallVars constraints lhs op rhs) =
  InfixCon forallVars (map normalizeType constraints) (normalizeBangType lhs) op (normalizeBangType rhs)
normalizeDataConInner (RecordCon forallVars constraints name fields) =
  RecordCon forallVars (map normalizeType constraints) name (map normalizeFieldDecl fields)
normalizeDataConInner (GadtCon forallBinders constraints names body) =
  GadtCon (map normalizeTyVarBinder forallBinders) (map normalizeType constraints) names (normalizeGadtBody body)

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
    ClassItemTypeSig names ty -> ClassItemTypeSig names (normalizeType ty)
    ClassItemDefaultSig name ty -> ClassItemDefaultSig name (normalizeType ty)
    ClassItemFixity assoc mNs prec ops -> ClassItemFixity assoc mNs prec ops
    ClassItemDefault vdecl -> ClassItemDefault (normalizeValueDecl vdecl)
    ClassItemTypeFamilyDecl tf -> ClassItemTypeFamilyDecl (normalizeTypeFamilyDecl tf)
    ClassItemDataFamilyDecl df -> ClassItemDataFamilyDecl (normalizeDataFamilyDecl df)
    ClassItemDefaultTypeInst tfi -> ClassItemDefaultTypeInst (normalizeTypeFamilyInst tfi)
    ClassItemPragma pragma -> ClassItemPragma pragma

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
  InstanceItemAnn (mkAnnotation span0) (normalizeInstanceDeclItemInner item)

normalizeInstanceDeclItemInner :: InstanceDeclItem -> InstanceDeclItem
normalizeInstanceDeclItemInner (InstanceItemAnn _ inner) = normalizeInstanceDeclItemInner inner
normalizeInstanceDeclItemInner (InstanceItemBind vdecl) = InstanceItemBind (normalizeValueDecl vdecl)
normalizeInstanceDeclItemInner (InstanceItemTypeSig names ty) = InstanceItemTypeSig names (normalizeType ty)
normalizeInstanceDeclItemInner (InstanceItemFixity assoc mNs prec ops) = InstanceItemFixity assoc mNs prec ops
normalizeInstanceDeclItemInner (InstanceItemTypeFamilyInst tfi) = InstanceItemTypeFamilyInst (normalizeTypeFamilyInst tfi)
normalizeInstanceDeclItemInner (InstanceItemDataFamilyInst dfi) = InstanceItemDataFamilyInst (normalizeDataFamilyInst dfi)
normalizeInstanceDeclItemInner (InstanceItemPragma pragma) = InstanceItemPragma pragma

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
