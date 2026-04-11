{-# LANGUAGE OverloadedStrings #-}

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
    ELambdaPats _ pats body -> ELambdaPats span0 (map normalizePattern pats) (normalizeExpr body)
    ELambdaCase _ alts -> ELambdaCase span0 (map normalizeCaseAlt alts)
    ELetDecls _ decls body -> ELetDecls span0 (map normalizeDecl decls) (normalizeExpr body)
    EWhereDecls _ body decls -> EWhereDecls span0 (normalizeExpr body) (map normalizeDecl decls)
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
    UnguardedRhs _ body -> UnguardedRhs span0 (normalizeExpr body)
    GuardedRhss _ guards -> GuardedRhss span0 (map normalizeGuardedRhs guards)

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
    PAs _ name inner -> PAs span0 name (normalizePattern inner)
    PStrict _ inner -> PStrict span0 (normalizePattern inner)
    PIrrefutable _ inner -> PIrrefutable span0 (normalizePattern inner)
    PNegLit _ lit -> PNegLit span0 (normalizeLiteral lit)
    PParen _ inner -> PParen span0 (normalizePattern inner)
    PUnboxedSum _ altIdx arity inner -> PUnboxedSum span0 altIdx arity (normalizePattern inner)
    PRecord _ con fields rwc -> PRecord span0 con [(name, normalizePattern p) | (name, p) <- fields] rwc
    PTypeSig _ inner ty -> PTypeSig span0 (normalizePattern inner) (normalizeType ty)
    PSplice _ body -> PSplice span0 (normalizeExpr body)

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
    DeclValue _ vdecl -> DeclValue span0 (normalizeValueDecl vdecl)
    DeclTypeSig _ names ty -> DeclTypeSig span0 names (normalizeType ty)
    _ -> decl

normalizeValueDecl :: ValueDecl -> ValueDecl
normalizeValueDecl vdecl =
  case vdecl of
    PatternBind _ pat rhs -> PatternBind span0 (normalizePattern pat) (normalizeRhs rhs)
    FunctionBind _ name matches -> FunctionBind span0 name (map normalizeMatch matches)

normalizeMatch :: Match -> Match
normalizeMatch m =
  Match
    { matchSpan = span0,
      matchHeadForm = matchHeadForm m,
      matchPats = map normalizePattern (matchPats m),
      matchRhs = normalizeRhs (matchRhs m)
    }

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
