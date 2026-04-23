{-# LANGUAGE LambdaCase #-}

-- | Shared normalization helpers used by multiple round-trip test modules.
module Test.Properties.ExprHelpers
  ( normalizeExpr,
    normalizeDecl,
    span0,
    stripTypeAnnotations,
  )
where

import Aihc.Parser.Syntax
import Data.Text qualified as T

renderFloat :: Rational -> T.Text
renderFloat value = T.pack (show (fromRational value :: Double))

-- | Canonical empty source span for normalization.
span0 :: SourceSpan
span0 = noSourceSpan

-- | Normalize an expression to canonical form for comparison.
-- Removes annotations.
normalizeExpr :: Expr -> Expr
normalizeExpr expr =
  case expr of
    EVar name -> EVar name
    EInt value _ _ -> EInt value TInteger (T.pack (show value))
    EFloat value _ _ -> EFloat value TFractional (renderFloat value)
    EChar value repr -> EChar value repr
    ECharHash value repr -> ECharHash value repr
    EString value repr -> EString value repr
    EStringHash value repr -> EStringHash value repr
    EOverloadedLabel value repr -> EOverloadedLabel value repr
    ETypeSyntax form ty -> ETypeSyntax form (normalizeType ty)
    EQuasiQuote quoter body -> EQuasiQuote quoter body
    EApp fn arg -> EApp (normalizeExpr fn) (normalizeExpr arg)
    EInfix lhs op rhs -> EInfix (normalizeExpr lhs) op (normalizeExpr rhs)
    ENegate inner ->
      let normInner = normalizeExpr inner
          -- Strip parentheses added by the parenthesization pass around
          -- primitive-literal-starting compound expressions.
          stripped = case normInner of
            EParen e -> e
            _ -> normInner
       in case stripped of
            -- The lexer merges minus into primitive (MagicHash) numeric
            -- literals, so ENegate around a numeric literal normalizes
            -- to a single negative literal.
            EInt n _ _ -> EInt (negate n) TInteger (T.pack (show (negate n)))
            EFloat r _ _ -> EFloat (negate r) TFractional (renderFloat (negate r))
            _ -> ENegate stripped
    ESectionL inner op -> ESectionL (normalizeExpr inner) op
    ESectionR op inner -> ESectionR op (normalizeExpr inner)
    EIf cond thenE elseE -> EIf (normalizeExpr cond) (normalizeExpr thenE) (normalizeExpr elseE)
    EMultiWayIf rhss -> EMultiWayIf (map normalizeGuardedRhs rhss)
    ECase scrutinee alts -> ECase (normalizeExpr scrutinee) (map normalizeCaseAlt alts)
    ELambdaPats pats body -> ELambdaPats (map normalizeLambdaPat pats) (normalizeExpr body)
    ELambdaCase alts -> ELambdaCase (map normalizeCaseAlt alts)
    ELambdaCases alts -> ELambdaCases (map normalizeLambdaCaseAlt alts)
    ELetDecls decls body -> ELetDecls (map normalizeDecl decls) (normalizeExpr body)
    EDo stmts isMdo -> EDo (map normalizeDoStmt stmts) isMdo
    EListComp body stmts -> EListComp (normalizeExpr body) (map normalizeCompStmt stmts)
    EListCompParallel body stmtss -> EListCompParallel (normalizeExpr body) (map (map normalizeCompStmt) stmtss)
    EList elems -> EList (map normalizeExpr elems)
    ETuple tupleFlavor elems -> ETuple tupleFlavor (map (fmap normalizeExpr) elems)
    EArithSeq seq' -> EArithSeq (normalizeArithSeq seq')
    ERecordCon con fields rwc -> ERecordCon con [(name, normalizeExpr e) | (name, e) <- fields] rwc
    ERecordUpd target fields -> ERecordUpd (normalizeExpr target) [(name, normalizeExpr e) | (name, e) <- fields]
    ETypeSig inner ty -> ETypeSig (normalizeExpr inner) (normalizeType ty)
    ETypeApp inner ty -> ETypeApp (normalizeExpr inner) (normalizeType ty)
    EUnboxedSum altIdx arity inner -> EUnboxedSum altIdx arity (normalizeExpr inner)
    EParen inner -> EParen (normalizeExpr inner)
    ETHExpQuote body -> ETHExpQuote (normalizeExpr body)
    ETHTypedQuote body -> ETHTypedQuote (normalizeExpr body)
    ETHDeclQuote decls -> ETHDeclQuote (map normalizeDecl decls)
    ETHTypeQuote ty -> ETHTypeQuote (normalizeType ty)
    ETHPatQuote pat -> ETHPatQuote (normalizePattern pat)
    ETHNameQuote body -> ETHNameQuote (normalizeExpr body)
    ETHTypeNameQuote ty -> ETHTypeNameQuote (normalizeType ty)
    ETHSplice body -> ETHSplice (normalizeExpr body)
    ETHTypedSplice body -> ETHTypedSplice (normalizeExpr body)
    EProc pat body -> EProc (normalizePattern pat) (normalizeCmd body)
    EPragma pragma body -> EPragma pragma (normalizeExpr body)
    EAnn _ sub -> normalizeExpr sub

normalizeCaseAlt :: CaseAlt -> CaseAlt
normalizeCaseAlt alt =
  CaseAlt
    { caseAltAnns = [],
      caseAltPattern = normalizePattern (caseAltPattern alt),
      caseAltRhs = normalizeRhs (caseAltRhs alt)
    }

normalizeLambdaCaseAlt :: LambdaCaseAlt -> LambdaCaseAlt
normalizeLambdaCaseAlt alt =
  LambdaCaseAlt
    { lambdaCaseAltAnns = [],
      lambdaCaseAltPats = map normalizePattern (lambdaCaseAltPats alt),
      lambdaCaseAltRhs = normalizeRhs (lambdaCaseAltRhs alt)
    }

normalizeRhs :: Rhs -> Rhs
normalizeRhs rhs =
  case rhs of
    UnguardedRhs _ body mDecls -> UnguardedRhs [] (normalizeExpr body) (fmap (map normalizeDecl) mDecls)
    GuardedRhss _ guards mDecls -> GuardedRhss [] (map normalizeGuardedRhs guards) (fmap (map normalizeDecl) mDecls)

normalizeGuardedRhs :: GuardedRhs -> GuardedRhs
normalizeGuardedRhs grhs =
  GuardedRhs
    { guardedRhsAnns = [],
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
    PTypeBinder binder -> PTypeBinder (normalizeTyVarBinder binder)
    PTypeSyntax form ty -> PTypeSyntax form (normalizeType ty)
    PWildcard -> PWildcard
    PLit lit -> PLit (normalizeLiteral lit)
    PQuasiQuote quoter body -> PQuasiQuote quoter body
    PTuple tupleFlavor elems -> PTuple tupleFlavor (map normalizePattern elems)
    PList elems -> PList (map normalizePattern elems)
    PCon con typeArgs args -> PCon con (map normalizeType typeArgs) (map normalizePattern args)
    PInfix lhs op rhs -> PInfix (normalizePattern lhs) op (normalizePattern rhs)
    PView e inner -> PView (normalizeExpr e) (normalizePattern inner)
    PAs name inner -> PAs name (normalizeUnaryPatInner inner)
    PStrict inner -> PStrict (normalizeUnaryPatInner inner)
    PIrrefutable inner -> PIrrefutable (normalizeUnaryPatInner inner)
    PNegLit lit -> PNegLit (normalizeLiteral lit)
    PParen inner ->
      let normInner = normalizePattern inner
       in case normInner of
            PCon con [] [] | nameType con == NameConSym -> normInner
            _ -> PParen normInner
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
  case lit of
    LitAnn _ sub -> normalizeLiteral sub
    LitInt value _ _ -> LitInt value TInteger (T.pack (show value))
    LitFloat value _ _ -> LitFloat value TFractional (renderFloat value)
    LitChar value repr -> LitChar value repr
    LitCharHash value repr -> LitCharHash value repr
    LitString value repr -> LitString value repr
    LitStringHash value repr -> LitStringHash value repr

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
    DeclRoleAnnotation ann -> DeclRoleAnnotation ann
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
    PatternBind pat rhs -> PatternBind (normalizePattern pat) (normalizeRhs rhs)
    FunctionBind name [Match {matchHeadForm = MatchHeadPrefix, matchPats = [], matchRhs = rhs}] ->
      PatternBind (PVar name) (normalizeRhs rhs)
    FunctionBind name matches -> FunctionBind name (map normalizeMatch matches)

normalizeMatch :: Match -> Match
normalizeMatch m =
  Match
    { matchAnns = [],
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
    { cmdCaseAltAnns = [],
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
normalizeCompStmtInner (CompThen f) = CompThen (normalizeExpr f)
normalizeCompStmtInner (CompThenBy f e) = CompThenBy (normalizeExpr f) (normalizeExpr e)
normalizeCompStmtInner (CompGroupUsing f) = CompGroupUsing (normalizeExpr f)
normalizeCompStmtInner (CompGroupByUsing e f) = CompGroupByUsing (normalizeExpr e) (normalizeExpr f)

normalizeArithSeq :: ArithSeq -> ArithSeq
normalizeArithSeq seq' =
  ArithSeqAnn (mkAnnotation span0) (normalizeArithSeqInner seq')

normalizeArithSeqInner :: ArithSeq -> ArithSeq
normalizeArithSeqInner (ArithSeqAnn _ inner) = normalizeArithSeqInner inner
normalizeArithSeqInner (ArithSeqFrom from) = ArithSeqFrom (normalizeExpr from)
normalizeArithSeqInner (ArithSeqFromThen from thenE) = ArithSeqFromThen (normalizeExpr from) (normalizeExpr thenE)
normalizeArithSeqInner (ArithSeqFromTo from to) = ArithSeqFromTo (normalizeExpr from) (normalizeExpr to)
normalizeArithSeqInner (ArithSeqFromThenTo from thenE to) = ArithSeqFromThenTo (normalizeExpr from) (normalizeExpr thenE) (normalizeExpr to)

-- | Recursively remove annotations.
stripTypeAnnotations :: Type -> Type
stripTypeAnnotations ty =
  case ty of
    TAnn _ sub -> stripTypeAnnotations sub
    TVar x -> TVar x
    TCon n p -> TCon n p
    TImplicitParam nm t -> TImplicitParam nm (stripTypeAnnotations t)
    TTypeLit l -> TTypeLit l
    TStar -> TStar
    TQuasiQuote q b -> TQuasiQuote q b
    TForall telescope t -> TForall (telescope {forallTelescopeBinders = map normalizeTyVarBinder (forallTelescopeBinders telescope)}) (stripTypeAnnotations t)
    TApp a b -> TApp (stripTypeAnnotations a) (stripTypeAnnotations b)
    TTypeApp a b -> TTypeApp (stripTypeAnnotations a) (stripTypeAnnotations b)
    TInfix lhs op promoted rhs -> TInfix (stripTypeAnnotations lhs) op promoted (stripTypeAnnotations rhs)
    TFun a b -> TFun (stripTypeAnnotations a) (stripTypeAnnotations b)
    TTuple fl pr es -> TTuple fl pr (map stripTypeAnnotations es)
    TUnboxedSum es -> TUnboxedSum (map stripTypeAnnotations es)
    TList pr es -> TList pr (map stripTypeAnnotations es)
    TParen t -> TParen (stripTypeAnnotations t)
    TKindSig a b -> TKindSig (stripTypeAnnotations a) (stripTypeAnnotations b)
    TContext cs t -> TContext (map stripTypeAnnotations cs) (stripTypeAnnotations t)
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
    TForall telescope inner ->
      TForall
        (telescope {forallTelescopeBinders = map normalizeTyVarBinder (forallTelescopeBinders telescope)})
        (normalizeType inner)
    TApp fn arg -> TApp (normalizeType fn) (normalizeType arg)
    TTypeApp fn arg -> TTypeApp (normalizeType fn) (normalizeType arg)
    TInfix lhs op promoted rhs -> TInfix (normalizeType lhs) op promoted (normalizeType rhs)
    TFun lhs rhs -> TFun (normalizeType lhs) (normalizeType rhs)
    TTuple tupleFlavor promoted elems -> TTuple tupleFlavor promoted (map normalizeType elems)
    TList promoted elems -> TList promoted (map normalizeType elems)
    TParen inner -> TParen (normalizeType inner)
    TKindSig inner kind -> TKindSig (normalizeType inner) (normalizeType kind)
    TUnboxedSum elems -> TUnboxedSum (map normalizeType elems)
    TContext constraints inner -> TContext (map normalizeType constraints) (normalizeType inner)
    TSplice body -> TSplice (normalizeExpr body)
    TWildcard -> TWildcard
    TAnn _ sub -> normalizeType sub

normalizeTyVarBinder :: TyVarBinder -> TyVarBinder
normalizeTyVarBinder tvb =
  tvb
    { tyVarBinderAnns = [],
      tyVarBinderKind = fmap normalizeType (tyVarBinderKind tvb)
    }

normalizeForallTelescope :: ForallTelescope -> ForallTelescope
normalizeForallTelescope telescope =
  telescope
    { forallTelescopeBinders = map normalizeTyVarBinder (forallTelescopeBinders telescope)
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
    { patSynDeclName = patSynDeclName ps,
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

normalizeTypeSynDecl :: TypeSynDecl -> TypeSynDecl
normalizeTypeSynDecl decl =
  TypeSynDecl
    { typeSynHead = normalizeBinderHead (typeSynHead decl),
      typeSynBody = normalizeType (typeSynBody decl)
    }

normalizeDataDecl :: DataDecl -> DataDecl
normalizeDataDecl decl =
  DataDecl
    { dataDeclHead = normalizeBinderHead (dataDeclHead decl),
      dataDeclContext = map normalizeType (dataDeclContext decl),
      dataDeclKind = fmap normalizeType (dataDeclKind decl),
      dataDeclConstructors = map normalizeDataConDecl (dataDeclConstructors decl),
      dataDeclDeriving = map normalizeDerivingClause (dataDeclDeriving decl)
    }

normalizeNewtypeDecl :: NewtypeDecl -> NewtypeDecl
normalizeNewtypeDecl decl =
  NewtypeDecl
    { newtypeDeclHead = normalizeBinderHead (newtypeDeclHead decl),
      newtypeDeclContext = map normalizeType (newtypeDeclContext decl),
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
  GadtCon (map normalizeForallTelescope forallBinders) (map normalizeType constraints) names (normalizeGadtBody body)

normalizeBangType :: BangType -> BangType
normalizeBangType bt =
  BangType
    { bangAnns = [],
      bangSourceUnpackedness = bangSourceUnpackedness bt,
      bangStrict = bangStrict bt,
      bangLazy = bangLazy bt,
      bangType = normalizeType (bangType bt)
    }

normalizeFieldDecl :: FieldDecl -> FieldDecl
normalizeFieldDecl fd =
  FieldDecl
    { fieldAnns = [],
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
    { classDeclContext = fmap (map normalizeType) (classDeclContext decl),
      classDeclHead = normalizeBinderHead (classDeclHead decl),
      classDeclFundeps = classDeclFundeps decl,
      classDeclItems = map normalizeClassDeclItem (classDeclItems decl)
    }

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
    { instanceDeclOverlapPragma = instanceDeclOverlapPragma decl,
      instanceDeclWarning = fmap normalizeWarningText (instanceDeclWarning decl),
      instanceDeclForall = map normalizeTyVarBinder (instanceDeclForall decl),
      instanceDeclContext = map normalizeType (instanceDeclContext decl),
      instanceDeclParenthesizedHead = instanceDeclParenthesizedHead decl,
      instanceDeclHead = normalizeInstanceHead (instanceDeclHead decl),
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
    { standaloneDerivingStrategy = standaloneDerivingStrategy decl,
      standaloneDerivingViaType = fmap normalizeType (standaloneDerivingViaType decl),
      standaloneDerivingOverlapPragma = standaloneDerivingOverlapPragma decl,
      standaloneDerivingWarning = fmap normalizeWarningText (standaloneDerivingWarning decl),
      standaloneDerivingForall = map normalizeTyVarBinder (standaloneDerivingForall decl),
      standaloneDerivingContext = map normalizeType (standaloneDerivingContext decl),
      standaloneDerivingParenthesizedHead = standaloneDerivingParenthesizedHead decl,
      standaloneDerivingHead = normalizeInstanceHead (standaloneDerivingHead decl)
    }

normalizeForeignDecl :: ForeignDecl -> ForeignDecl
normalizeForeignDecl decl =
  ForeignDecl
    { foreignDirection = foreignDirection decl,
      foreignCallConv = foreignCallConv decl,
      foreignSafety = foreignSafety decl,
      foreignEntity = foreignEntity decl,
      foreignName = foreignName decl,
      foreignType = normalizeType (foreignType decl)
    }

normalizeTypeFamilyDecl :: TypeFamilyDecl -> TypeFamilyDecl
normalizeTypeFamilyDecl tf =
  TypeFamilyDecl
    { typeFamilyDeclHeadForm = typeFamilyDeclHeadForm tf,
      typeFamilyDeclExplicitFamilyKeyword = typeFamilyDeclExplicitFamilyKeyword tf,
      typeFamilyDeclHead = normalizeType (typeFamilyDeclHead tf),
      typeFamilyDeclParams = map normalizeTyVarBinder (typeFamilyDeclParams tf),
      typeFamilyDeclResultSig = fmap normalizeTypeFamilyResultSig (typeFamilyDeclResultSig tf),
      typeFamilyDeclEquations = fmap (map normalizeTypeFamilyEq) (typeFamilyDeclEquations tf)
    }

normalizeTypeFamilyResultSig :: TypeFamilyResultSig -> TypeFamilyResultSig
normalizeTypeFamilyResultSig sig =
  case sig of
    TypeFamilyKindSig kind -> TypeFamilyKindSig (normalizeType kind)
    TypeFamilyTyVarSig result -> TypeFamilyTyVarSig (normalizeTyVarBinder result)
    TypeFamilyInjectiveSig result injectivity ->
      TypeFamilyInjectiveSig (normalizeTyVarBinder result) injectivity

normalizeTypeFamilyEq :: TypeFamilyEq -> TypeFamilyEq
normalizeTypeFamilyEq eq =
  TypeFamilyEq
    { typeFamilyEqAnns = typeFamilyEqAnns eq,
      typeFamilyEqForall = map normalizeTyVarBinder (typeFamilyEqForall eq),
      typeFamilyEqHeadForm = typeFamilyEqHeadForm eq,
      typeFamilyEqLhs = normalizeType (typeFamilyEqLhs eq),
      typeFamilyEqRhs = normalizeType (typeFamilyEqRhs eq)
    }

normalizeDataFamilyDecl :: DataFamilyDecl -> DataFamilyDecl
normalizeDataFamilyDecl df =
  DataFamilyDecl
    { dataFamilyDeclHead = normalizeBinderHead (dataFamilyDeclHead df),
      dataFamilyDeclKind = fmap normalizeType (dataFamilyDeclKind df)
    }

normalizeBinderHead :: BinderHead name -> BinderHead name
normalizeBinderHead head' =
  case head' of
    PrefixBinderHead name params -> PrefixBinderHead name (map normalizeTyVarBinder params)
    InfixBinderHead lhs name rhs tailParams ->
      InfixBinderHead
        (normalizeTyVarBinder lhs)
        name
        (normalizeTyVarBinder rhs)
        (map normalizeTyVarBinder tailParams)

normalizeInstanceHead :: InstanceHead name -> InstanceHead name
normalizeInstanceHead head' =
  case head' of
    PrefixInstanceHead name tys -> PrefixInstanceHead name (map normalizeType tys)
    InfixInstanceHead lhs name rhs tailTypes -> InfixInstanceHead (normalizeType lhs) name (normalizeType rhs) (map normalizeType tailTypes)

normalizeTypeFamilyInst :: TypeFamilyInst -> TypeFamilyInst
normalizeTypeFamilyInst tfi =
  TypeFamilyInst
    { typeFamilyInstForall = map normalizeTyVarBinder (typeFamilyInstForall tfi),
      typeFamilyInstHeadForm = typeFamilyInstHeadForm tfi,
      typeFamilyInstLhs = normalizeType (typeFamilyInstLhs tfi),
      typeFamilyInstRhs = normalizeType (typeFamilyInstRhs tfi)
    }

normalizeDataFamilyInst :: DataFamilyInst -> DataFamilyInst
normalizeDataFamilyInst dfi =
  DataFamilyInst
    { dataFamilyInstIsNewtype = dataFamilyInstIsNewtype dfi,
      dataFamilyInstForall = map normalizeTyVarBinder (dataFamilyInstForall dfi),
      dataFamilyInstHead = normalizeType (dataFamilyInstHead dfi),
      dataFamilyInstKind = fmap normalizeType (dataFamilyInstKind dfi),
      dataFamilyInstConstructors = map normalizeDataConDecl (dataFamilyInstConstructors dfi),
      dataFamilyInstDeriving = map normalizeDerivingClause (dataFamilyInstDeriving dfi)
    }
