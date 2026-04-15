{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Aihc.Parser.Parens
-- Description : Parenthesization pass for AST
--
-- This module provides a parenthesization pass that inserts 'EParen', 'PParen',
-- 'TParen', and 'CmdPar' nodes at all required positions in the AST. After this
-- pass, the pretty-printer can format code without worrying about parentheses.
--
-- The pass is __idempotent__: calling it twice produces the same result as
-- calling it once. It never adds unnecessary parentheses, so parsed code
-- (which already has explicit paren nodes from the source) is not modified.
--
-- __Entry points:__
--
-- * 'addModuleParens'  — parenthesise an entire module
-- * 'addDeclParens'    — parenthesise a declaration
-- * 'addExprParens'    — parenthesise an expression
-- * 'addPatternParens' — parenthesise a pattern
-- * 'addTypeParens'    — parenthesise a type
module Aihc.Parser.Parens
  ( addModuleParens,
    addDeclParens,
    addExprParens,
    addPatternParens,
    addTypeParens,
  )
where

import Aihc.Parser.Syntax
import Data.Text (Text)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Wrap an expression in 'EParen' if the predicate holds, unless it is
-- already parenthesised.
wrapExpr :: Bool -> Expr -> Expr
wrapExpr True e@(EParen {}) = e
wrapExpr True e = EParen noSourceSpan e
wrapExpr False e = e

-- | Wrap a pattern in 'PParen' if the predicate holds, unless already wrapped.
wrapPat :: Bool -> Pattern -> Pattern
wrapPat True p@(PParen {}) = p
wrapPat True p = PParen noSourceSpan p
wrapPat False p = p

-- | Wrap a type in 'TParen' if the predicate holds, unless already wrapped.
wrapTy :: Bool -> Type -> Type
wrapTy True t@(TParen {}) = t
wrapTy True t = TParen noSourceSpan t
wrapTy False t = t

-- ---------------------------------------------------------------------------
-- Token classification helpers (mirrored from Pretty.hs)
-- ---------------------------------------------------------------------------

isSymbolicName :: Name -> Bool
isSymbolicName name =
  case nameType name of
    NameVarSym -> True
    NameConSym -> True
    _ -> False

isArrowTailOp :: Text -> Bool
isArrowTailOp "-<" = True
isArrowTailOp "-<<" = True
isArrowTailOp _ = False

-- ---------------------------------------------------------------------------
-- Expression classification helpers (mirrored from Pretty.hs)
-- ---------------------------------------------------------------------------

-- | Check if an expression is a "block expression" that can appear without
-- parentheses as a function argument when BlockArguments is enabled.
isBlockExpr :: Expr -> Bool
isBlockExpr = \case
  EIf {} -> True
  EMultiWayIf {} -> True
  ECase {} -> True
  EDo {} -> True
  ELambdaPats {} -> True
  ELambdaCase {} -> True
  ELetDecls {} -> True
  _ -> False

-- | Check if an expression is "greedy" - i.e., it could consume trailing syntax.
isGreedyExpr :: Expr -> Bool
isGreedyExpr = \case
  ECase {} -> True
  EIf {} -> True
  ELambdaPats {} -> True
  ELambdaCase {} -> True
  ELetDecls {} -> True
  EDo {} -> True
  EProc {} -> True
  EApp _ _ arg | isBlockExpr arg -> isOpenEnded arg
  _ -> False

-- | Check if an expression is "braced" - i.e., the pretty-printer always
-- wraps it in explicit @{ }@ braces, making it self-delimiting, AND the parser
-- can parse the resulting @expr { ... } op rhs@ without parentheses.
-- Self-delimiting expressions do not need parentheses on the left-hand side of
-- an infix operator, because the closing @}@ unambiguously ends the expression.
isBracedExpr :: Expr -> Bool
isBracedExpr = \case
  ECase {} -> True
  EDo {} -> True
  ELambdaCase {} -> True
  _ -> False

-- | Check if an expression is "open-ended" - its rightmost component can
-- capture a trailing where clause.
isOpenEnded :: Expr -> Bool
isOpenEnded = \case
  EIf {} -> True
  ELambdaPats {} -> True
  ELetDecls {} -> True
  EProc {} -> True
  EInfix _ _ _ rhs -> isOpenEnded rhs
  EApp _ _ arg | isBlockExpr arg -> isOpenEnded arg
  _ -> False

-- | Does the pretty-printed form of an expression end with @:: Type@?
endsWithTypeSig :: Expr -> Bool
endsWithTypeSig = \case
  ETypeSig {} -> True
  ELetDecls _ _ body -> endsWithTypeSig body
  ELambdaPats _ _ body -> endsWithTypeSig body
  EInfix _ _ _ rhs -> endsWithTypeSig rhs
  _ -> False

-- | Check whether an expression's pretty-printed form starts with '$'.
startsWithDollar :: Expr -> Bool
startsWithDollar (ETHSplice {}) = True
startsWithDollar (ETHTypedSplice {}) = True
startsWithDollar (ERecordUpd _ base _) = startsWithDollar base
startsWithDollar (EApp _ fn _) = startsWithDollar fn
startsWithDollar _ = False

startsWithOverloadedLabel :: Expr -> Bool
startsWithOverloadedLabel = \case
  EOverloadedLabel {} -> True
  EAnn _ sub -> startsWithOverloadedLabel sub
  EApp _ fn _ -> startsWithOverloadedLabel fn
  EInfix _ lhs _ _ -> startsWithOverloadedLabel lhs
  ERecordUpd _ base _ -> startsWithOverloadedLabel base
  ETypeSig _ inner _ -> startsWithOverloadedLabel inner
  ETypeApp _ fn _ -> startsWithOverloadedLabel fn
  _ -> False

-- ---------------------------------------------------------------------------
-- Expression contexts
-- ---------------------------------------------------------------------------

data ExprCtx
  = CtxInfixRhs Bool
  | CtxInfixLhs
  | CtxAppFun
  | CtxAppArg
  | CtxAppArgNoParens
  | CtxTypeSigBody
  | CtxGuarded

needsExprParens :: ExprCtx -> Expr -> Bool
needsExprParens ctx expr =
  case ctx of
    CtxInfixRhs protectOpenEnded ->
      case expr of
        EInfix {} -> True
        ETypeSig {} -> True
        ENegate {} -> True
        _ | protectOpenEnded && isOpenEnded expr -> True
        _ -> False
    CtxInfixLhs ->
      case expr of
        ETypeSig {} -> True
        ENegate {} -> True
        _ -> isOpenEnded expr
    CtxAppFun ->
      case expr of
        ENegate {} -> True
        _ -> False
    CtxAppArg ->
      case expr of
        _ | isBlockExpr expr -> False
        _ -> False
    CtxAppArgNoParens ->
      False
    CtxTypeSigBody ->
      case expr of
        ENegate {} -> True
        ETypeSig {} -> True
        ELambdaPats {} -> True
        _ -> isOpenEnded expr
    CtxGuarded -> isGreedyExpr expr

exprCtxPrec :: ExprCtx -> Expr -> Int
exprCtxPrec ctx expr =
  case ctx of
    CtxInfixRhs _
      | isGreedyExpr expr -> 0
      | otherwise -> 1
    CtxInfixLhs
      | isBracedExpr expr -> 0
      | otherwise -> 1
    CtxAppFun -> 2
    CtxAppArg -> 3
    CtxAppArgNoParens -> 0
    CtxTypeSigBody -> 1
    CtxGuarded -> 0

-- ---------------------------------------------------------------------------
-- Type contexts
-- ---------------------------------------------------------------------------

data TypeCtx
  = CtxTypeFunArg
  | CtxTypeAppArg
  | CtxTypeAtom
  | CtxKindSig

needsTypeParens :: TypeCtx -> Type -> Bool
needsTypeParens ctx ty =
  case ctx of
    CtxTypeFunArg ->
      case ty of
        TForall {} -> True
        TFun {} -> True
        TContext {} -> True
        _ -> False
    CtxTypeAppArg ->
      case ty of
        TQuasiQuote {} -> False
        TApp {} -> True
        TForall {} -> True
        TFun {} -> True
        TContext {} -> True
        _ -> False
    CtxTypeAtom ->
      case ty of
        TVar {} -> False
        TCon {} -> False
        TImplicitParam {} -> False
        TTypeLit {} -> False
        TStar {} -> False
        TQuasiQuote {} -> False
        TList {} -> False
        TTuple {} -> False
        TUnboxedSum {} -> False
        TParen {} -> False
        TKindSig {} -> False
        TWildcard {} -> False
        _ -> True
    CtxKindSig ->
      case ty of
        TKindSig {} -> False
        _ -> True

-- ---------------------------------------------------------------------------
-- Guard contexts
-- ---------------------------------------------------------------------------

data GuardArrow = GuardArrow | GuardEquals

guardExprNeedsParens :: GuardArrow -> Expr -> Bool
guardExprNeedsParens arrow = \case
  ELambdaPats {} -> True
  EProc {} -> True
  EApp _ _ arg | isBlockExpr arg -> guardExprNeedsParens arrow arg
  expr -> case arrow of
    GuardArrow -> endsWithTypeSig expr
    GuardEquals -> False

-- ---------------------------------------------------------------------------
-- Module
-- ---------------------------------------------------------------------------

addModuleParens :: Module -> Module
addModuleParens modu =
  modu
    { moduleDecls = map addDeclParens (moduleDecls modu)
    }

-- ---------------------------------------------------------------------------
-- Declarations
-- ---------------------------------------------------------------------------

addDeclParens :: Decl -> Decl
addDeclParens decl =
  case decl of
    DeclAnn ann sub -> DeclAnn ann (addDeclParens sub)
    DeclValue sp vdecl -> DeclValue sp (addValueDeclParens vdecl)
    DeclTypeSig sp names ty -> DeclTypeSig sp names (addTypeParens ty)
    DeclPatSyn sp ps -> DeclPatSyn sp (addPatSynDeclParens ps)
    DeclPatSynSig sp names ty -> DeclPatSynSig sp names (addTypeParens ty)
    DeclStandaloneKindSig sp name kind -> DeclStandaloneKindSig sp name (addTypeParens kind)
    DeclFixity {} -> decl
    DeclRoleAnnotation {} -> decl
    DeclTypeSyn sp synDecl ->
      DeclTypeSyn sp synDecl {typeSynBody = addTypeParens (typeSynBody synDecl)}
    DeclData sp dataDecl -> DeclData sp (addDataDeclParens dataDecl)
    DeclTypeData sp dataDecl -> DeclTypeData sp (addDataDeclParens dataDecl)
    DeclNewtype sp newtypeDecl -> DeclNewtype sp (addNewtypeDeclParens newtypeDecl)
    DeclClass sp classDecl -> DeclClass sp (addClassDeclParens classDecl)
    DeclInstance sp instanceDecl -> DeclInstance sp (addInstanceDeclParens instanceDecl)
    DeclStandaloneDeriving sp derivingDecl -> DeclStandaloneDeriving sp (addStandaloneDerivingParens derivingDecl)
    DeclDefault sp tys -> DeclDefault sp (map addTypeParens tys)
    DeclForeign sp foreignDecl -> DeclForeign sp (addForeignDeclParens foreignDecl)
    DeclSplice sp body -> DeclSplice sp (addDeclSpliceParens body)
    DeclTypeFamilyDecl sp tf -> DeclTypeFamilyDecl sp (addTypeFamilyDeclParens tf)
    DeclDataFamilyDecl sp df -> DeclDataFamilyDecl sp (addDataFamilyDeclParens df)
    DeclTypeFamilyInst sp tfi -> DeclTypeFamilyInst sp (addTypeFamilyInstParens tfi)
    DeclDataFamilyInst sp dfi -> DeclDataFamilyInst sp (addDataFamilyInstParens dfi)
    DeclPragma {} -> decl

addDeclSpliceParens :: Expr -> Expr
addDeclSpliceParens body =
  case body of
    EVar {} -> body
    EParen sp inner -> EParen sp (addExprParens inner)
    _ -> addExprParens body

addValueDeclParens :: ValueDecl -> ValueDecl
addValueDeclParens vdecl =
  case vdecl of
    PatternBind sp pat rhs -> PatternBind sp (addPatternParens pat) (addRhsParens rhs)
    FunctionBind sp name matches -> FunctionBind sp name (map (addMatchParens name) matches)

addMatchParens :: UnqualifiedName -> Match -> Match
addMatchParens name match =
  match
    { matchPats = addFunctionHeadPats name (matchHeadForm match) (matchPats match),
      matchRhs = addRhsParens (matchRhs match)
    }

addFunctionHeadPats :: UnqualifiedName -> MatchHeadForm -> [Pattern] -> [Pattern]
addFunctionHeadPats _name headForm pats =
  case headForm of
    MatchHeadPrefix -> map addFunctionHeadPatternAtomParens pats
    MatchHeadInfix ->
      case pats of
        lhs : rhs : tailPats ->
          let lhs' = addInfixFunctionHeadPatternAtomParens lhs
              rhs' = addInfixFunctionHeadPatternAtomParens rhs
           in case tailPats of
                [] -> [lhs', rhs']
                _ ->
                  -- When infix head has tail pats, the infix part is wrapped in parens
                  -- and tail pats use function head atom rules
                  lhs' : rhs' : map addFunctionHeadPatternAtomParens tailPats
        _ -> map addFunctionHeadPatternAtomParens pats

addRhsParens :: Rhs -> Rhs
addRhsParens rhs =
  case rhs of
    UnguardedRhs sp body whereDecls ->
      UnguardedRhs sp (addExprParens body) (fmap (map addDeclParens) whereDecls)
    GuardedRhss sp guards whereDecls ->
      GuardedRhss sp (map (addGuardedRhsParens GuardEquals) guards) (fmap (map addDeclParens) whereDecls)

addGuardedRhsParens :: GuardArrow -> GuardedRhs -> GuardedRhs
addGuardedRhsParens arrow grhs =
  grhs
    { guardedRhsGuards = map (addGuardQualifierParens arrow) (guardedRhsGuards grhs),
      guardedRhsBody = addExprParens (guardedRhsBody grhs)
    }

addGuardQualifierParens :: GuardArrow -> GuardQualifier -> GuardQualifier
addGuardQualifierParens arrow qual =
  case qual of
    GuardExpr sp expr -> GuardExpr sp (addGuardExprParens arrow expr)
    GuardPat sp pat expr -> GuardPat sp (addPatternParens pat) (addGuardExprParens arrow expr)
    GuardLet sp decls -> GuardLet sp (map addDeclParens decls)

addGuardExprParens :: GuardArrow -> Expr -> Expr
addGuardExprParens arrow expr =
  wrapExpr (guardExprNeedsParens arrow expr) (addExprParens expr)

addPatSynDeclParens :: PatSynDecl -> PatSynDecl
addPatSynDeclParens ps =
  ps
    { patSynDeclPat = addPatternParens (patSynDeclPat ps),
      patSynDeclDir = addPatSynDirParens (patSynDeclName ps) (patSynDeclDir ps)
    }

addPatSynDirParens :: UnqualifiedName -> PatSynDir -> PatSynDir
addPatSynDirParens _ PatSynBidirectional = PatSynBidirectional
addPatSynDirParens _ PatSynUnidirectional = PatSynUnidirectional
addPatSynDirParens name (PatSynExplicitBidirectional matches) =
  PatSynExplicitBidirectional (map (addMatchParens name) matches)

addDataDeclParens :: DataDecl -> DataDecl
addDataDeclParens decl =
  decl
    { dataDeclContext = addContextConstraints (dataDeclContext decl),
      dataDeclKind = fmap addTypeParens (dataDeclKind decl),
      dataDeclConstructors = map addDataConDeclParens (dataDeclConstructors decl),
      dataDeclDeriving = map addDerivingClauseParens (dataDeclDeriving decl)
    }

addNewtypeDeclParens :: NewtypeDecl -> NewtypeDecl
addNewtypeDeclParens decl =
  decl
    { newtypeDeclContext = addContextConstraints (newtypeDeclContext decl),
      newtypeDeclKind = fmap addTypeParens (newtypeDeclKind decl),
      newtypeDeclConstructor = fmap addDataConDeclParens (newtypeDeclConstructor decl),
      newtypeDeclDeriving = map addDerivingClauseParens (newtypeDeclDeriving decl)
    }

addDerivingClauseParens :: DerivingClause -> DerivingClause
addDerivingClauseParens dc =
  dc
    { derivingClasses = map addTypeParens (derivingClasses dc),
      derivingViaType = fmap addTypeParens (derivingViaType dc)
    }

addDataConDeclParens :: DataConDecl -> DataConDecl
addDataConDeclParens con =
  case con of
    PrefixCon sp forallVars constraints name fields ->
      PrefixCon sp forallVars (addContextConstraints constraints) name (map addBangTypeParens fields)
    InfixCon sp forallVars constraints lhs op rhs ->
      InfixCon sp forallVars (addContextConstraints constraints) (addBangTypeAtomParens lhs) op (addBangTypeAtomParens rhs)
    RecordCon sp forallVars constraints name fields ->
      RecordCon sp forallVars (addContextConstraints constraints) name (map addRecordFieldDeclParens fields)
    GadtCon sp forallBinders constraints names body ->
      GadtCon sp forallBinders (addContextConstraints constraints) names (addGadtBodyParens body)

addBangTypeParens :: BangType -> BangType
addBangTypeParens bt =
  bt
    { bangType =
        if bangStrict bt
          then addTypeIn CtxTypeAtom (bangType bt)
          else addTypeIn CtxTypeFunArg (bangType bt)
    }

addBangTypeAtomParens :: BangType -> BangType
addBangTypeAtomParens bt =
  let inner = addBangTypeParens bt
   in inner
        { bangType =
            wrapTy (needsTypeParens CtxTypeFunArg (bangType bt)) (bangType inner)
        }

addRecordFieldDeclParens :: FieldDecl -> FieldDecl
addRecordFieldDeclParens fd =
  fd
    { fieldType = addRecordFieldBangTypeParens (fieldType fd)
    }

addRecordFieldBangTypeParens :: BangType -> BangType
addRecordFieldBangTypeParens bt =
  bt
    { bangType = addTypeParens (bangType bt)
    }

addGadtBodyParens :: GadtBody -> GadtBody
addGadtBodyParens body =
  case body of
    GadtPrefixBody args resultTy ->
      GadtPrefixBody (map addBangTypeParens args) (addTypeParens resultTy)
    GadtRecordBody fields resultTy ->
      GadtRecordBody (map addRecordFieldDeclParens fields) (addTypeParens resultTy)

addClassDeclParens :: ClassDecl -> ClassDecl
addClassDeclParens decl =
  decl
    { classDeclContext = fmap addContextConstraints (classDeclContext decl),
      classDeclItems = map addClassItemParens (classDeclItems decl)
    }

addClassItemParens :: ClassDeclItem -> ClassDeclItem
addClassItemParens item =
  case item of
    ClassItemTypeSig sp names ty -> ClassItemTypeSig sp names (addTypeParens ty)
    ClassItemDefaultSig sp name ty -> ClassItemDefaultSig sp name (addTypeParens ty)
    ClassItemFixity {} -> item
    ClassItemDefault sp vdecl -> ClassItemDefault sp (addValueDeclParens vdecl)
    ClassItemTypeFamilyDecl sp tf -> ClassItemTypeFamilyDecl sp (addTypeFamilyDeclParens tf)
    ClassItemDataFamilyDecl sp df -> ClassItemDataFamilyDecl sp (addDataFamilyDeclParens df)
    ClassItemDefaultTypeInst sp tfi -> ClassItemDefaultTypeInst sp (addTypeFamilyInstParens tfi)
    ClassItemPragma {} -> item

addInstanceDeclParens :: InstanceDecl -> InstanceDecl
addInstanceDeclParens decl =
  decl
    { instanceDeclContext = addContextConstraints (instanceDeclContext decl),
      instanceDeclTypes = map (addTypeIn CtxTypeAtom) (instanceDeclTypes decl),
      instanceDeclItems = map addInstanceItemParens (instanceDeclItems decl)
    }

addInstanceItemParens :: InstanceDeclItem -> InstanceDeclItem
addInstanceItemParens item =
  case item of
    InstanceItemBind sp vdecl -> InstanceItemBind sp (addValueDeclParens vdecl)
    InstanceItemTypeSig sp names ty -> InstanceItemTypeSig sp names (addTypeParens ty)
    InstanceItemFixity {} -> item
    InstanceItemTypeFamilyInst sp tfi -> InstanceItemTypeFamilyInst sp (addTypeFamilyInstParens tfi)
    InstanceItemDataFamilyInst sp dfi -> InstanceItemDataFamilyInst sp (addDataFamilyInstParens dfi)
    InstanceItemPragma {} -> item

addStandaloneDerivingParens :: StandaloneDerivingDecl -> StandaloneDerivingDecl
addStandaloneDerivingParens decl =
  decl
    { standaloneDerivingViaType = fmap addTypeParens (standaloneDerivingViaType decl),
      standaloneDerivingContext = addContextConstraints (standaloneDerivingContext decl),
      standaloneDerivingTypes = map (addTypeIn CtxTypeAtom) (standaloneDerivingTypes decl)
    }

addForeignDeclParens :: ForeignDecl -> ForeignDecl
addForeignDeclParens decl =
  decl
    { foreignType = addTypeParens (foreignType decl)
    }

addTypeFamilyDeclParens :: TypeFamilyDecl -> TypeFamilyDecl
addTypeFamilyDeclParens tf =
  tf
    { typeFamilyDeclHead = addTypeParens (typeFamilyDeclHead tf),
      typeFamilyDeclKind = fmap addTypeParens (typeFamilyDeclKind tf),
      typeFamilyDeclEquations = fmap (map addTypeFamilyEqParens) (typeFamilyDeclEquations tf)
    }

addTypeFamilyEqParens :: TypeFamilyEq -> TypeFamilyEq
addTypeFamilyEqParens eq =
  eq
    { typeFamilyEqLhs = addTypeParens (typeFamilyEqLhs eq),
      typeFamilyEqRhs = addTypeParens (typeFamilyEqRhs eq)
    }

addDataFamilyDeclParens :: DataFamilyDecl -> DataFamilyDecl
addDataFamilyDeclParens df =
  df
    { dataFamilyDeclKind = fmap addTypeParens (dataFamilyDeclKind df)
    }

addTypeFamilyInstParens :: TypeFamilyInst -> TypeFamilyInst
addTypeFamilyInstParens tfi =
  tfi
    { typeFamilyInstLhs = addTypeParens (typeFamilyInstLhs tfi),
      typeFamilyInstRhs = addTypeParens (typeFamilyInstRhs tfi)
    }

addDataFamilyInstParens :: DataFamilyInst -> DataFamilyInst
addDataFamilyInstParens dfi =
  dfi
    { dataFamilyInstHead = addTypeParens (dataFamilyInstHead dfi),
      dataFamilyInstConstructors = map addDataConDeclParens (dataFamilyInstConstructors dfi),
      dataFamilyInstDeriving = map addDerivingClauseParens (dataFamilyInstDeriving dfi)
    }

-- ---------------------------------------------------------------------------
-- Expressions
-- ---------------------------------------------------------------------------

-- | Add parentheses to an expression at all required positions.
addExprParens :: Expr -> Expr
addExprParens = addExprParensPrec 0

addExprParensIn :: ExprCtx -> Expr -> Expr
addExprParensIn ctx expr =
  wrapExpr (needsExprParens ctx expr) (addExprParensPrec (exprCtxPrec ctx expr) expr)

-- | Flatten a left-nested application chain.
flattenApps :: Expr -> (Expr, [Expr])
flattenApps = go []
  where
    go args (EApp _ fn arg) = go (arg : args) fn
    go args root = (root, args)

addExprParensPrec :: Int -> Expr -> Expr
addExprParensPrec prec expr =
  case expr of
    EApp {} -> addAppsChainPrec prec expr
    ETypeApp sp fn ty ->
      wrapExpr (prec > 2) (ETypeApp sp (addExprParensIn CtxAppFun fn) (addTypeIn CtxTypeAtom ty))
    EVar {} -> expr
    EInt {} -> expr
    EIntHash {} -> expr
    EIntBase {} -> expr
    EIntBaseHash {} -> expr
    EFloat {} -> expr
    EFloatHash {} -> expr
    EChar {} -> expr
    ECharHash {} -> expr
    EString {} -> expr
    EStringHash {} -> expr
    EOverloadedLabel {} -> expr
    EQuasiQuote {} -> expr
    ETHExpQuote sp body -> ETHExpQuote sp (addExprParens body)
    ETHTypedQuote sp body -> ETHTypedQuote sp (addExprParens body)
    ETHDeclQuote sp decls -> ETHDeclQuote sp (map addDeclParens decls)
    ETHTypeQuote sp ty -> ETHTypeQuote sp (addTypeParens ty)
    ETHPatQuote sp pat -> ETHPatQuote sp (addPatternParens pat)
    ETHNameQuote {} -> expr
    ETHTypeNameQuote {} -> expr
    ETHSplice sp body -> ETHSplice sp (addSpliceBodyParens body)
    ETHTypedSplice sp body -> ETHTypedSplice sp (addSpliceBodyParens body)
    EIf sp cond yes no ->
      wrapExpr
        (prec > 0)
        (EIf sp (addExprParens cond) (addIfBranchParens yes) (addIfBranchParens no))
    EMultiWayIf sp rhss ->
      wrapExpr (prec > 0) (EMultiWayIf sp (map (addGuardedRhsParens GuardArrow) rhss))
    ELambdaPats sp pats body ->
      wrapExpr (prec > 0) (ELambdaPats sp (map addLambdaPatternAtomParens pats) (addExprParens body))
    ELambdaCase sp alts ->
      wrapExpr (prec > 0) (ELambdaCase sp (map addCaseAltParens alts))
    EInfix sp lhs op rhs
      | isArrowTailOp (renderName op) ->
          -- Arrow tail operators always parenthesized, LHS also parenthesized
          wrapExpr
            True
            ( EInfix
                sp
                (wrapExpr True (addExprParens lhs))
                op
                (addExprParens rhs)
            )
      | otherwise ->
          wrapExpr
            (prec > 1)
            ( EInfix
                sp
                (addExprParensIn CtxInfixLhs lhs)
                op
                (addExprParensIn (CtxInfixRhs (prec == 1)) rhs)
            )
    ENegate sp inner ->
      wrapExpr (prec > 2) (ENegate sp (addNegateParens inner))
    ESectionL sp lhs op ->
      -- Sections are always in parens (printed with parens in Pretty.hs)
      -- The LHS needs special handling for greedy/typesig expressions
      let lhs' =
            if isGreedyExpr lhs || isTypeSig lhs
              then wrapExpr True (addExprParens lhs)
              else addExprParensPrec 1 lhs
       in ESectionL sp lhs' op
    ESectionR sp op rhs ->
      ESectionR sp op (addExprParens rhs)
    ELetDecls sp decls body ->
      wrapExpr (prec > 0) (ELetDecls sp (map addDeclParens decls) (addExprParens body))
    ECase sp scrutinee alts ->
      wrapExpr (prec > 0) (ECase sp (addExprParens scrutinee) (map addCaseAltParens alts))
    EDo sp stmts isMdo ->
      wrapExpr (prec > 0) (EDo sp (map addDoStmtParens stmts) isMdo)
    EListComp sp body quals ->
      EListComp sp (addExprParens body) (map addCompStmtParens quals)
    EListCompParallel sp body qualifierGroups ->
      EListCompParallel sp (addExprParens body) (map (map addCompStmtParens) qualifierGroups)
    EArithSeq sp seqInfo -> EArithSeq sp (addArithSeqParens seqInfo)
    ERecordCon sp name fields hasWildcard ->
      ERecordCon sp name [(n, addExprParens e) | (n, e) <- fields] hasWildcard
    ERecordUpd sp base fields ->
      ERecordUpd sp (addExprParensPrec 3 base) [(n, addExprParens e) | (n, e) <- fields]
    ETypeSig sp inner ty ->
      wrapExpr (prec > 1) (ETypeSig sp (addExprParensIn CtxTypeSigBody inner) (addTypeParens ty))
    EParen sp inner ->
      case inner of
        -- Sections are already "in parens" via their EParen wrapper,
        -- don't add double parens
        ESectionL {} -> EParen sp (addExprParens inner)
        ESectionR {} -> EParen sp (addExprParens inner)
        _ -> EParen sp (addExprParens inner)
    EList sp values -> EList sp (map addExprParens values)
    ETuple sp tupleFlavor values -> ETuple sp tupleFlavor (map (fmap addExprParens) values)
    EUnboxedSum sp altIdx arity inner -> EUnboxedSum sp altIdx arity (addExprParens inner)
    EProc sp pat body ->
      wrapExpr (prec > 0) (EProc sp (addPatternParens pat) (addCmdParens body))
    EAnn ann sub -> EAnn ann (addExprParensPrec prec sub)
  where
    isTypeSig :: Expr -> Bool
    isTypeSig (ETypeSig {}) = True
    isTypeSig _ = False

-- | Handle application chains, preserving original EApp spans.
addAppsChainPrec :: Int -> Expr -> Expr
addAppsChainPrec prec expr =
  let (root, args) = flattenApps expr
      -- Get the spans from the original EApp chain
      appSpans = getAppSpans expr
      root' = addExprParensIn CtxAppFun root
      nArgs = length args
      args' =
        [ let isLast = i == nArgs - 1
              ctx
                | isLast, isBlockExpr a = CtxAppArgNoParens
                | isLast = CtxAppArg
                | otherwise = CtxAppArg
           in addExprParensIn ctx a
        | (i, a) <- Prelude.zip [0 :: Int ..] args
        ]
      -- Reconstruct the EApp chain with original spans
      rebuilt = case Prelude.zip appSpans args' of
        [] -> root'
        pairs -> foldl (\fn (sp, arg) -> EApp sp fn arg) root' pairs
   in wrapExpr (prec > 2) rebuilt

getAppSpans :: Expr -> [SourceSpan]
getAppSpans = reverse . go
  where
    go (EApp sp fn _) = sp : go fn
    go _ = []

addSpliceBodyParens :: Expr -> Expr
addSpliceBodyParens body =
  case body of
    -- EParen around a section: the pretty-printer's EParen transparency
    -- means this would print as just the section's parens. We need an
    -- extra EParen so the splice delimiter parens are not swallowed.
    EParen sp inner@(ESectionL {}) -> EParen sp (EParen noSourceSpan (addExprParens inner))
    EParen sp inner@(ESectionR {}) -> EParen sp (EParen noSourceSpan (addExprParens inner))
    EParen sp inner -> EParen sp (addExprParens inner)
    EVar {} -> body
    -- Sections print their own parens via prettyExpr, and EParen is
    -- transparent around them in the pretty-printer (to avoid double parens
    -- in normal code like `x = (+1)`). For splices, we need the EParen to
    -- actually produce parens, so we double-wrap.
    ESectionL {} -> EParen noSourceSpan (EParen noSourceSpan (addExprParens body))
    ESectionR {} -> EParen noSourceSpan (EParen noSourceSpan (addExprParens body))
    -- Any other body needs to be wrapped in EParen so it prints as $(expr).
    _ -> EParen noSourceSpan (addExprParens body)

addNegateParens :: Expr -> Expr
addNegateParens inner =
  if startsWithDollar inner || startsWithOverloadedLabel inner
    then wrapExpr True (addExprParens inner)
    else addExprParensPrec 3 inner

addIfBranchParens :: Expr -> Expr
addIfBranchParens expr =
  if endsWithTypeSig expr
    then wrapExpr True (addExprParens expr)
    else addExprParens expr

addCaseAltParens :: CaseAlt -> CaseAlt
addCaseAltParens (CaseAlt sp pat rhs) =
  CaseAlt sp (addPatternParens pat) (addCaseAltRhsParens rhs)

addCaseAltRhsParens :: Rhs -> Rhs
addCaseAltRhsParens rhs =
  case rhs of
    UnguardedRhs sp body whereDecls ->
      UnguardedRhs sp (addExprParens body) (fmap (map addDeclParens) whereDecls)
    GuardedRhss sp guards whereDecls ->
      GuardedRhss sp (map (addGuardedRhsParens GuardArrow) guards) (fmap (map addDeclParens) whereDecls)

addDoStmtParens :: DoStmt Expr -> DoStmt Expr
addDoStmtParens stmt =
  case stmt of
    DoBind sp pat e -> DoBind sp (addPatternParens pat) (addExprParens e)
    DoLetDecls sp decls -> DoLetDecls sp (map addDeclParens decls)
    DoExpr sp e -> DoExpr sp (addExprParens e)
    DoRecStmt sp stmts -> DoRecStmt sp (map addDoStmtParens stmts)

addCompStmtParens :: CompStmt -> CompStmt
addCompStmtParens stmt =
  case stmt of
    CompGen sp pat e -> CompGen sp (addPatternParens pat) (addExprParens e)
    CompGuard sp e -> CompGuard sp (addExprParens e)
    CompLet sp bindings -> CompLet sp [(n, addExprParens e) | (n, e) <- bindings]
    CompLetDecls sp decls -> CompLetDecls sp (map addDeclParens decls)

addArithSeqParens :: ArithSeq -> ArithSeq
addArithSeqParens seqInfo =
  case seqInfo of
    ArithSeqFrom sp fromE -> ArithSeqFrom sp (addExprGuardedParens fromE)
    ArithSeqFromThen sp fromE thenE -> ArithSeqFromThen sp (addExprGuardedParens fromE) (addExprGuardedParens thenE)
    ArithSeqFromTo sp fromE toE -> ArithSeqFromTo sp (addExprGuardedParens fromE) (addExprParens toE)
    ArithSeqFromThenTo sp fromE thenE toE -> ArithSeqFromThenTo sp (addExprGuardedParens fromE) (addExprGuardedParens thenE) (addExprParens toE)

addExprGuardedParens :: Expr -> Expr
addExprGuardedParens = addExprParensIn CtxGuarded

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Add parentheses to a type at all required positions.
addTypeParens :: Type -> Type
addTypeParens = addTypeParensShared CtxTypeAtom 0

addTypeIn :: TypeCtx -> Type -> Type
addTypeIn ctx ty =
  wrapTy (needsTypeParens ctx ty) (addTypeParensShared ctx 0 ty)

addTypeParensShared :: TypeCtx -> Int -> Type -> Type
addTypeParensShared ctx prec ty =
  let atom = addTypeParensShared CtxTypeAtom
   in case ty of
        TAnn sp sub -> TAnn sp (addTypeParensShared ctx prec sub)
        TVar {} -> ty
        TCon sp name promoted
          | isSymbolicName name, promoted /= Promoted -> TCon sp name promoted
          | otherwise -> TCon sp name promoted
        TImplicitParam sp name inner -> TImplicitParam sp name (addTypeParens inner)
        TTypeLit {} -> ty
        TStar {} -> ty
        TQuasiQuote {} -> ty
        TForall sp binders inner ->
          wrapTy (prec > 0) (TForall sp (map addTyVarBinderParens binders) (atom 0 inner))
        TApp _ (TApp _ op@(TCon _ opName _) lhs) rhs
          | isSymbolicName opName,
            renderName opName /= "->" ->
              -- Infix type operator: args are treated as atoms
              TApp (appSpan ty) (TApp (innerAppSpan ty) op (atom 0 lhs)) (atom 0 rhs)
        TApp sp f x ->
          wrapTy (prec > 2) (TApp sp (addTypeIn CtxTypeFunArg f) (addTypeIn CtxTypeAppArg x))
        TFun sp a b ->
          wrapTy (prec > 0) (TFun sp (addTypeIn CtxTypeFunArg a) (atom 0 b))
        TTuple sp tupleFlavor promoted elems ->
          TTuple sp tupleFlavor promoted (map (atom 0) elems)
        TUnboxedSum sp elems -> TUnboxedSum sp (map (atom 0) elems)
        TList sp promoted elems -> TList sp promoted (map (atom 0) elems)
        -- Inside an explicit TParen, TKindSig does not need an additional
        -- TParen wrapper since the enclosing delimiter already provides it.
        TParen sp inner -> TParen sp (addTypeParensInner inner)
        -- TKindSig always needs parens in most contexts. The parser absorbs
        -- (ty :: kind) as TKindSig directly, so the TParen is not preserved
        -- through roundtrips. The pretty-printer relies on the TParen wrapper
        -- to produce the required parentheses.
        TKindSig sp ty' kind ->
          wrapTy True (TKindSig sp (atom 0 ty') (atom 0 kind))
        TContext sp constraints inner ->
          wrapTy (prec > 0) (TContext sp (addContextConstraints constraints) (atom 0 inner))
        TSplice sp body -> TSplice sp (addSpliceBodyParens body)
        TWildcard {} -> ty
  where
    appSpan :: Type -> SourceSpan
    appSpan (TApp sp _ _) = sp
    appSpan _ = noSourceSpan

    innerAppSpan :: Type -> SourceSpan
    innerAppSpan (TApp _ inner _) = appSpan inner
    innerAppSpan _ = noSourceSpan

addTyVarBinderParens :: TyVarBinder -> TyVarBinder
addTyVarBinderParens tvb =
  tvb {tyVarBinderKind = fmap addTypeParens (tyVarBinderKind tvb)}

-- | Process a type inside explicit delimiters (TParen, TTuple, etc.).
-- TKindSig does not need wrapping here because the enclosing delimiter
-- already provides the necessary parenthesization.
addTypeParensInner :: Type -> Type
addTypeParensInner ty =
  case ty of
    TKindSig sp ty' kind ->
      TKindSig sp (addTypeParensShared CtxTypeAtom 0 ty') (addTypeParensShared CtxTypeAtom 0 kind)
    _ -> addTypeParensShared CtxTypeAtom 0 ty

-- | Process constraint types in a TContext.
-- In multi-constraint contexts, TKindSig doesn't need individual parens
-- (commas delimit). In single-constraint contexts, TKindSig needs parens.
addContextConstraints :: [Type] -> [Type]
addContextConstraints constraints =
  case constraints of
    [single] -> [addContextConstraintSingle single]
    _ -> map addContextConstraintMulti constraints

-- | Add parens to a single constraint in a context.
-- TKindSig needs wrapping here because there's no comma delimiter.
addContextConstraintSingle :: Type -> Type
addContextConstraintSingle = addTypeParens

-- | Add parens to a constraint in a multi-constraint context.
-- TKindSig doesn't need extra wrapping because commas delimit.
-- Strip TParen around TKindSig since it's unnecessary here.
addContextConstraintMulti :: Type -> Type
addContextConstraintMulti ty =
  case ty of
    TKindSig sp ty' kind ->
      -- In multi-constraint context, TKindSig doesn't need wrapping
      TKindSig sp (addTypeParensShared CtxTypeAtom 0 ty') (addTypeParensShared CtxTypeAtom 0 kind)
    TParen _ inner@(TKindSig {}) ->
      -- Strip TParen around TKindSig in multi-constraint context
      addContextConstraintMulti inner
    _ -> addTypeParens ty

-- ---------------------------------------------------------------------------
-- Patterns
-- ---------------------------------------------------------------------------

-- | Add parentheses to a pattern at all required positions.
addPatternParens :: Pattern -> Pattern
addPatternParens pat =
  case pat of
    PAnn sp sub -> PAnn sp (addPatternParens sub)
    PVar {} -> pat
    PWildcard {} -> pat
    PLit sp lit -> PLit sp lit
    PQuasiQuote {} -> pat
    PTuple sp tupleFlavor elems -> PTuple sp tupleFlavor (map addPatternInDelimited elems)
    PUnboxedSum sp altIdx arity inner -> PUnboxedSum sp altIdx arity (addPatternInDelimited inner)
    PList sp elems -> PList sp (map addPatternInDelimited elems)
    PCon sp con args -> PCon sp con (map addPatternAtomParens args)
    PInfix sp lhs op rhs -> PInfix sp (addPatternAtomParens lhs) op (addPatternAtomParens rhs)
    PView sp viewExpr inner ->
      wrapPat True (PView sp (addViewExprParens viewExpr) (addPatternParens inner))
    PAs sp name inner -> PAs sp name (addPatternAtomStrictParens inner)
    PStrict sp inner -> PStrict sp (addPatternAtomStrictParens inner)
    PIrrefutable sp inner -> PIrrefutable sp (addPatternAtomStrictParens inner)
    PNegLit sp lit -> PNegLit sp lit
    PParen sp inner -> PParen sp (addPatternInDelimited inner)
    PRecord sp con fields hasWildcard ->
      PRecord sp con [(fieldName, addPatternParens fieldPat) | (fieldName, fieldPat) <- fields] hasWildcard
    PTypeSig sp inner ty -> PTypeSig sp (addPatternParens inner) (addTypeParens ty)
    PSplice sp body -> PSplice sp (addSpliceBodyParens body)

-- | Add parens for a pattern inside a delimited context (tuples, lists, etc.).
-- View patterns don't need extra parens there.
addPatternInDelimited :: Pattern -> Pattern
addPatternInDelimited pat =
  case pat of
    PView sp viewExpr inner -> PView sp (addViewExprParens viewExpr) (addPatternParens inner)
    PAs sp name inner -> PAs sp name (addPatternAtomStrictParens inner)
    PStrict sp inner -> PStrict sp (addPatternAtomStrictParens inner)
    PIrrefutable sp inner -> PIrrefutable sp (addPatternAtomStrictParens inner)
    _ -> addPatternParens pat

addViewExprParens :: Expr -> Expr
addViewExprParens expr =
  if endsWithTypeSig expr
    then wrapExpr True (addExprParens expr)
    else addExprParens expr

-- | Check if an operator is the cons operator ':'.
isConsOperator :: Name -> Bool
isConsOperator name =
  renderName name == ":"

addPatternAtomParens :: Pattern -> Pattern
addPatternAtomParens pat =
  case pat of
    PVar {} -> addPatternParens pat
    PWildcard {} -> addPatternParens pat
    PLit {} -> addPatternParens pat
    PQuasiQuote {} -> addPatternParens pat
    PNegLit {} -> wrapPat True (addPatternParens pat)
    PList {} -> addPatternParens pat
    PTuple {} -> addPatternParens pat
    PUnboxedSum {} -> addPatternParens pat
    PParen {} -> addPatternParens pat
    PStrict {} -> addPatternParens pat
    PIrrefutable {} -> addPatternParens pat
    PView {} -> addPatternParens pat
    PAs {} -> addPatternParens pat
    PSplice {} -> addPatternParens pat
    PCon _ _ [] -> addPatternParens pat
    PInfix _ _ op _
      | isConsOperator op ->
          -- Cons operator (:) is right-associative, so nested cons patterns
          -- don't need parentheses: x1:x2:xs parses as x1:(x2:xs)
          addPatternParens pat
    _ -> wrapPat True (addPatternParens pat)

-- | Add parens for a pattern in lambda argument position.
addLambdaPatternAtomParens :: Pattern -> Pattern
addLambdaPatternAtomParens pat =
  case pat of
    PNegLit {} -> wrapPat True (addPatternParens pat)
    PCon _ _ [] -> wrapPat True (addPatternParens pat)
    _ -> addPatternAtomParens pat

-- | Add parens for a pattern in function-head argument position.
addFunctionHeadPatternAtomParens :: Pattern -> Pattern
addFunctionHeadPatternAtomParens pat =
  case pat of
    PNegLit {} -> wrapPat True (addPatternParens pat)
    PCon _ _ (_ : _) -> wrapPat True (addPatternParens pat)
    PRecord {} -> addPatternParens pat
    _ -> addPatternAtomParens pat

-- | Add parens for infix function-head operands.
addInfixFunctionHeadPatternAtomParens :: Pattern -> Pattern
addInfixFunctionHeadPatternAtomParens pat =
  case pat of
    PNegLit {} -> wrapPat True (addPatternParens pat)
    _ -> addPatternParens pat

-- | Add parens for the inner pattern of @, !, ~.
addPatternAtomStrictParens :: Pattern -> Pattern
addPatternAtomStrictParens pat =
  case pat of
    PNegLit {} -> wrapPat True (addPatternParens pat)
    PCon _ _ [] -> wrapPat True (addPatternParens pat)
    PStrict {} -> wrapPat True (addPatternParens pat)
    PIrrefutable {} -> wrapPat True (addPatternParens pat)
    PRecord {} -> addPatternParens pat
    _ -> addPatternAtomParens pat

-- ---------------------------------------------------------------------------
-- Arrow commands
-- ---------------------------------------------------------------------------

addCmdParens :: Cmd -> Cmd
addCmdParens cmd =
  case cmd of
    CmdArrApp sp lhs appTy rhs ->
      CmdArrApp sp (addExprParensPrec 1 lhs) appTy (addExprParens rhs)
    CmdInfix sp l op r ->
      CmdInfix sp (addCmdParens l) op (addCmdParens r)
    CmdDo sp stmts ->
      CmdDo sp (map addCmdDoStmtParens stmts)
    CmdIf sp cond yes no ->
      CmdIf sp (addExprParens cond) (addCmdParens yes) (addCmdParens no)
    CmdCase sp scrut alts ->
      CmdCase sp (addExprParens scrut) (map addCmdCaseAltParens alts)
    CmdLet sp decls body ->
      CmdLet sp (map addDeclParens decls) (addCmdParens body)
    CmdLam sp pats body ->
      CmdLam sp (map addPatternAtomParens pats) (addCmdParens body)
    CmdApp sp c e ->
      CmdApp sp (addCmdParens c) (addExprParensPrec 3 e)
    CmdPar sp c ->
      CmdPar sp (addCmdParens c)

addCmdDoStmtParens :: DoStmt Cmd -> DoStmt Cmd
addCmdDoStmtParens stmt =
  case stmt of
    DoBind sp pat cmd' -> DoBind sp (addPatternParens pat) (addCmdParens cmd')
    DoLetDecls sp decls -> DoLetDecls sp (map addDeclParens decls)
    DoExpr sp cmd' -> DoExpr sp (addCmdParens cmd')
    DoRecStmt sp stmts -> DoRecStmt sp (map addCmdDoStmtParens stmts)

addCmdCaseAltParens :: CmdCaseAlt -> CmdCaseAlt
addCmdCaseAltParens alt =
  alt
    { cmdCaseAltPat = addPatternParens (cmdCaseAltPat alt),
      cmdCaseAltBody = addCmdParens (cmdCaseAltBody alt)
    }
