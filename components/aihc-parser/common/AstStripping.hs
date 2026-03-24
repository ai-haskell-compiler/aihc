-- | Strip source spans from AST nodes for comparison purposes.
--
-- This module provides functions to recursively remove source location
-- information from AST nodes, making them suitable for structural comparison
-- where source positions should be ignored.
module AstStripping
  ( -- * Top-level stripping
    stripExpr,
    stripDecl,
    stripPattern,
    stripType,

    -- * Supporting types
    stripValueDecl,
    stripMatch,
    stripRhs,
    stripGuardedRhs,
    stripGuardQualifier,
    stripLiteral,
    stripConstraint,
    stripTypeSynDecl,
    stripDataDecl,
    stripNewtypeDecl,
    stripTyVarBinder,
    stripDataConDecl,
    stripBangType,
    stripFieldDecl,
    stripClassDecl,
    stripClassDeclItem,
    stripInstanceDecl,
    stripInstanceDeclItem,
    stripStandaloneDerivingDecl,
    stripForeignDecl,
    stripCaseAlt,
    stripDoStmt,
    stripCompStmt,
    stripArithSeq,
  )
where

import Aihc.Parser.Ast hiding (noSourceSpan)
import qualified Aihc.Parser.Ast as Ast

noSourceSpan :: SourceSpan
noSourceSpan = Ast.noSourceSpan

stripExpr :: Expr -> Expr
stripExpr expr =
  case expr of
    EVar _ t -> EVar noSourceSpan t
    EInt _ n repr -> EInt noSourceSpan n repr
    EIntBase _ n txt -> EIntBase noSourceSpan n txt
    EFloat _ d repr -> EFloat noSourceSpan d repr
    EChar _ c repr -> EChar noSourceSpan c repr
    EString _ s repr -> EString noSourceSpan s repr
    EQuasiQuote _ q body -> EQuasiQuote noSourceSpan q body
    EIf _ a b c -> EIf noSourceSpan (stripExpr a) (stripExpr b) (stripExpr c)
    ELambdaPats _ pats e -> ELambdaPats noSourceSpan (map stripPattern pats) (stripExpr e)
    ELambdaCase _ alts -> ELambdaCase noSourceSpan (map stripCaseAlt alts)
    EInfix _ a op b -> EInfix noSourceSpan (stripExpr a) op (stripExpr b)
    ENegate _ e -> ENegate noSourceSpan (stripExpr e)
    ESectionL _ e op -> ESectionL noSourceSpan (stripExpr e) op
    ESectionR _ op e -> ESectionR noSourceSpan op (stripExpr e)
    ELetDecls _ decls e -> ELetDecls noSourceSpan (map stripDecl decls) (stripExpr e)
    ECase _ e alts -> ECase noSourceSpan (stripExpr e) (map stripCaseAlt alts)
    EDo _ stmts -> EDo noSourceSpan (map stripDoStmt stmts)
    EListComp _ e stmts -> EListComp noSourceSpan (stripExpr e) (map stripCompStmt stmts)
    EListCompParallel _ e groups -> EListCompParallel noSourceSpan (stripExpr e) (map (map stripCompStmt) groups)
    EArithSeq _ a -> EArithSeq noSourceSpan (stripArithSeq a)
    ERecordCon _ name fields -> ERecordCon noSourceSpan name [(field, stripExpr val) | (field, val) <- fields]
    ERecordUpd _ base fields -> ERecordUpd noSourceSpan (stripExpr base) [(field, stripExpr val) | (field, val) <- fields]
    ETypeSig _ e t -> ETypeSig noSourceSpan (stripExpr e) (stripType t)
    EParen _ e -> EParen noSourceSpan (stripExpr e)
    EWhereDecls _ e decls -> EWhereDecls noSourceSpan (stripExpr e) (map stripDecl decls)
    EList _ es -> EList noSourceSpan (map stripExpr es)
    ETuple _ es -> ETuple noSourceSpan (map stripExpr es)
    ETupleSection _ es -> ETupleSection noSourceSpan (map (fmap stripExpr) es)
    ETupleCon _ n -> ETupleCon noSourceSpan n
    ETypeApp _ e t -> ETypeApp noSourceSpan (stripExpr e) (stripType t)
    EApp _ f x -> EApp noSourceSpan (stripExpr f) (stripExpr x)

stripDecl :: Decl -> Decl
stripDecl decl =
  case decl of
    DeclValue _ value -> DeclValue noSourceSpan (stripValueDecl value)
    DeclTypeSig _ names t -> DeclTypeSig noSourceSpan names (stripType t)
    DeclStandaloneKindSig _ name kind -> DeclStandaloneKindSig noSourceSpan name (stripType kind)
    DeclFixity _ assoc prec ops -> DeclFixity noSourceSpan assoc prec ops
    DeclTypeSyn _ syn -> DeclTypeSyn noSourceSpan (stripTypeSynDecl syn)
    DeclData _ dat -> DeclData noSourceSpan (stripDataDecl dat)
    DeclNewtype _ nt -> DeclNewtype noSourceSpan (stripNewtypeDecl nt)
    DeclClass _ cls -> DeclClass noSourceSpan (stripClassDecl cls)
    DeclInstance _ inst -> DeclInstance noSourceSpan (stripInstanceDecl inst)
    DeclStandaloneDeriving _ sd -> DeclStandaloneDeriving noSourceSpan (stripStandaloneDerivingDecl sd)
    DeclDefault _ tys -> DeclDefault noSourceSpan (map stripType tys)
    DeclForeign _ foreignDecl -> DeclForeign noSourceSpan (stripForeignDecl foreignDecl)

stripValueDecl :: ValueDecl -> ValueDecl
stripValueDecl valueDecl =
  case valueDecl of
    FunctionBind _ name matches -> FunctionBind noSourceSpan name (map stripMatch matches)
    PatternBind _ pat rhs -> PatternBind noSourceSpan (stripPattern pat) (stripRhs rhs)

stripMatch :: Match -> Match
stripMatch match =
  Match
    { matchSpan = noSourceSpan,
      matchPats = map stripPattern (matchPats match),
      matchRhs = stripRhs (matchRhs match)
    }

stripRhs :: Rhs -> Rhs
stripRhs rhs =
  case rhs of
    UnguardedRhs _ expr -> UnguardedRhs noSourceSpan (stripExpr expr)
    GuardedRhss _ guarded -> GuardedRhss noSourceSpan (map stripGuardedRhs guarded)

stripGuardedRhs :: GuardedRhs -> GuardedRhs
stripGuardedRhs guarded =
  GuardedRhs
    { guardedRhsSpan = noSourceSpan,
      guardedRhsGuards = map stripGuardQualifier (guardedRhsGuards guarded),
      guardedRhsBody = stripExpr (guardedRhsBody guarded)
    }

stripGuardQualifier :: GuardQualifier -> GuardQualifier
stripGuardQualifier qualifier =
  case qualifier of
    GuardExpr _ expr -> GuardExpr noSourceSpan (stripExpr expr)
    GuardPat _ pat expr -> GuardPat noSourceSpan (stripPattern pat) (stripExpr expr)
    GuardLet _ decls -> GuardLet noSourceSpan (map stripDecl decls)

stripPattern :: Pattern -> Pattern
stripPattern pat =
  case pat of
    PVar _ n -> PVar noSourceSpan n
    PWildcard _ -> PWildcard noSourceSpan
    PLit _ lit -> PLit noSourceSpan (stripLiteral lit)
    PQuasiQuote _ q body -> PQuasiQuote noSourceSpan q body
    PTuple _ pats -> PTuple noSourceSpan (map stripPattern pats)
    PList _ pats -> PList noSourceSpan (map stripPattern pats)
    PCon _ name pats -> PCon noSourceSpan name (map stripPattern pats)
    PInfix _ a op b -> PInfix noSourceSpan (stripPattern a) op (stripPattern b)
    PView _ expr inner -> PView noSourceSpan (stripExpr expr) (stripPattern inner)
    PAs _ n inner -> PAs noSourceSpan n (stripPattern inner)
    PStrict _ inner -> PStrict noSourceSpan (stripPattern inner)
    PIrrefutable _ inner -> PIrrefutable noSourceSpan (stripPattern inner)
    PNegLit _ lit -> PNegLit noSourceSpan (stripLiteral lit)
    PParen _ inner -> PParen noSourceSpan (stripPattern inner)
    PRecord _ name fields -> PRecord noSourceSpan name [(field, stripPattern value) | (field, value) <- fields]

stripLiteral :: Literal -> Literal
stripLiteral lit =
  case lit of
    LitInt _ i repr -> LitInt noSourceSpan i repr
    LitIntBase _ i txt -> LitIntBase noSourceSpan i txt
    LitFloat _ d repr -> LitFloat noSourceSpan d repr
    LitChar _ c repr -> LitChar noSourceSpan c repr
    LitString _ s repr -> LitString noSourceSpan s repr

stripType :: Type -> Type
stripType ty =
  case ty of
    TVar _ n -> TVar noSourceSpan n
    TCon _ n -> TCon noSourceSpan n
    TStar _ -> TStar noSourceSpan
    TQuasiQuote _ q body -> TQuasiQuote noSourceSpan q body
    TForall _ binders inner -> TForall noSourceSpan binders (stripType inner)
    TApp _ a b -> TApp noSourceSpan (stripType a) (stripType b)
    TFun _ a b -> TFun noSourceSpan (stripType a) (stripType b)
    TTuple _ tys -> TTuple noSourceSpan (map stripType tys)
    TList _ t -> TList noSourceSpan (stripType t)
    TParen _ t -> TParen noSourceSpan (stripType t)
    TContext _ constraints t -> TContext noSourceSpan (map stripConstraint constraints) (stripType t)

stripConstraint :: Constraint -> Constraint
stripConstraint c =
  Constraint
    { constraintSpan = noSourceSpan,
      constraintClass = constraintClass c,
      constraintArgs = map stripType (constraintArgs c),
      constraintParen = constraintParen c
    }

stripTypeSynDecl :: TypeSynDecl -> TypeSynDecl
stripTypeSynDecl d =
  TypeSynDecl
    { typeSynSpan = noSourceSpan,
      typeSynName = typeSynName d,
      typeSynParams = map stripTyVarBinder (typeSynParams d),
      typeSynBody = stripType (typeSynBody d)
    }

stripDataDecl :: DataDecl -> DataDecl
stripDataDecl d =
  DataDecl
    { dataDeclSpan = noSourceSpan,
      dataDeclContext = map stripConstraint (dataDeclContext d),
      dataDeclName = dataDeclName d,
      dataDeclParams = map stripTyVarBinder (dataDeclParams d),
      dataDeclConstructors = map stripDataConDecl (dataDeclConstructors d),
      dataDeclDeriving = dataDeclDeriving d
    }

stripNewtypeDecl :: NewtypeDecl -> NewtypeDecl
stripNewtypeDecl d =
  NewtypeDecl
    { newtypeDeclSpan = noSourceSpan,
      newtypeDeclContext = map stripConstraint (newtypeDeclContext d),
      newtypeDeclName = newtypeDeclName d,
      newtypeDeclParams = map stripTyVarBinder (newtypeDeclParams d),
      newtypeDeclConstructor = fmap stripDataConDecl (newtypeDeclConstructor d),
      newtypeDeclDeriving = newtypeDeclDeriving d
    }

stripTyVarBinder :: TyVarBinder -> TyVarBinder
stripTyVarBinder b =
  TyVarBinder
    { tyVarBinderSpan = noSourceSpan,
      tyVarBinderName = tyVarBinderName b,
      tyVarBinderKind = fmap stripType (tyVarBinderKind b)
    }

stripDataConDecl :: DataConDecl -> DataConDecl
stripDataConDecl con =
  case con of
    PrefixCon _ forallVars context name args -> PrefixCon noSourceSpan forallVars (map stripConstraint context) name (map stripBangType args)
    InfixCon _ forallVars context left op right -> InfixCon noSourceSpan forallVars (map stripConstraint context) (stripBangType left) op (stripBangType right)
    RecordCon _ forallVars context name fields -> RecordCon noSourceSpan forallVars (map stripConstraint context) name (map stripFieldDecl fields)

stripBangType :: BangType -> BangType
stripBangType b =
  BangType
    { bangSpan = noSourceSpan,
      bangStrict = bangStrict b,
      bangType = stripType (bangType b)
    }

stripFieldDecl :: FieldDecl -> FieldDecl
stripFieldDecl f =
  FieldDecl
    { fieldSpan = noSourceSpan,
      fieldNames = fieldNames f,
      fieldType = stripBangType (fieldType f)
    }

stripClassDecl :: ClassDecl -> ClassDecl
stripClassDecl d =
  ClassDecl
    { classDeclSpan = noSourceSpan,
      classDeclContext = map stripConstraint (classDeclContext d),
      classDeclName = classDeclName d,
      classDeclParams = map stripTyVarBinder (classDeclParams d),
      classDeclItems = map stripClassDeclItem (classDeclItems d)
    }

stripClassDeclItem :: ClassDeclItem -> ClassDeclItem
stripClassDeclItem item =
  case item of
    ClassItemTypeSig _ names t -> ClassItemTypeSig noSourceSpan names (stripType t)
    ClassItemFixity _ assoc prec ops -> ClassItemFixity noSourceSpan assoc prec ops
    ClassItemDefault _ value -> ClassItemDefault noSourceSpan (stripValueDecl value)

stripInstanceDecl :: InstanceDecl -> InstanceDecl
stripInstanceDecl d =
  InstanceDecl
    { instanceDeclSpan = noSourceSpan,
      instanceDeclContext = map stripConstraint (instanceDeclContext d),
      instanceDeclClassName = instanceDeclClassName d,
      instanceDeclTypes = map stripType (instanceDeclTypes d),
      instanceDeclItems = map stripInstanceDeclItem (instanceDeclItems d)
    }

stripStandaloneDerivingDecl :: StandaloneDerivingDecl -> StandaloneDerivingDecl
stripStandaloneDerivingDecl d =
  StandaloneDerivingDecl
    { standaloneDerivingSpan = noSourceSpan,
      standaloneDerivingStrategy = standaloneDerivingStrategy d,
      standaloneDerivingContext = map stripConstraint (standaloneDerivingContext d),
      standaloneDerivingClassName = standaloneDerivingClassName d,
      standaloneDerivingTypes = map stripType (standaloneDerivingTypes d)
    }

stripInstanceDeclItem :: InstanceDeclItem -> InstanceDeclItem
stripInstanceDeclItem item =
  case item of
    InstanceItemBind _ value -> InstanceItemBind noSourceSpan (stripValueDecl value)
    InstanceItemTypeSig _ names t -> InstanceItemTypeSig noSourceSpan names (stripType t)
    InstanceItemFixity _ assoc prec ops -> InstanceItemFixity noSourceSpan assoc prec ops

stripForeignDecl :: ForeignDecl -> ForeignDecl
stripForeignDecl d =
  ForeignDecl
    { foreignDeclSpan = noSourceSpan,
      foreignDirection = foreignDirection d,
      foreignCallConv = foreignCallConv d,
      foreignSafety = foreignSafety d,
      foreignEntity = foreignEntity d,
      foreignName = foreignName d,
      foreignType = stripType (foreignType d)
    }

stripCaseAlt :: CaseAlt -> CaseAlt
stripCaseAlt alt =
  CaseAlt
    { caseAltSpan = noSourceSpan,
      caseAltPattern = stripPattern (caseAltPattern alt),
      caseAltRhs = stripRhs (caseAltRhs alt)
    }

stripDoStmt :: DoStmt -> DoStmt
stripDoStmt stmt =
  case stmt of
    DoBind _ pat expr -> DoBind noSourceSpan (stripPattern pat) (stripExpr expr)
    DoLet _ binds -> DoLet noSourceSpan [(name, stripExpr e) | (name, e) <- binds]
    DoLetDecls _ decls -> DoLetDecls noSourceSpan (map stripDecl decls)
    DoExpr _ expr -> DoExpr noSourceSpan (stripExpr expr)

stripCompStmt :: CompStmt -> CompStmt
stripCompStmt stmt =
  case stmt of
    CompGen _ pat expr -> CompGen noSourceSpan (stripPattern pat) (stripExpr expr)
    CompGuard _ expr -> CompGuard noSourceSpan (stripExpr expr)
    CompLet _ binds -> CompLet noSourceSpan [(name, stripExpr e) | (name, e) <- binds]
    CompLetDecls _ decls -> CompLetDecls noSourceSpan (map stripDecl decls)

stripArithSeq :: ArithSeq -> ArithSeq
stripArithSeq seqExpr =
  case seqExpr of
    ArithSeqFrom _ a -> ArithSeqFrom noSourceSpan (stripExpr a)
    ArithSeqFromThen _ a b -> ArithSeqFromThen noSourceSpan (stripExpr a) (stripExpr b)
    ArithSeqFromTo _ a b -> ArithSeqFromTo noSourceSpan (stripExpr a) (stripExpr b)
    ArithSeqFromThenTo _ a b c -> ArithSeqFromThenTo noSourceSpan (stripExpr a) (stripExpr b) (stripExpr c)
