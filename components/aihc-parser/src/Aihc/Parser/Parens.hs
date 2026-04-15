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
import Control.Monad (guard)
import Data.Text (Text)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Wrap an expression in 'EParen' if the predicate holds, unless it is
-- already parenthesised.
wrapExpr :: Bool -> Expr -> Expr
wrapExpr True (EAnn ann sub) = EAnn ann (wrapExpr True sub)
wrapExpr True e@EParen {} = e
wrapExpr True e = EParen e
wrapExpr False e = e

-- | Wrap a pattern in 'PParen' if the predicate holds, unless already wrapped.
wrapPat :: Bool -> Pattern -> Pattern
wrapPat True (PAnn ann sub) = PAnn ann (wrapPat True sub)
wrapPat True p@PParen {} = p
wrapPat True p = PParen p
wrapPat False p = p

-- | Wrap a type in 'TParen' if the predicate holds, unless already wrapped.
wrapTy :: Bool -> Type -> Type
wrapTy True (TAnn ann sub) = TAnn ann (wrapTy True sub)
wrapTy True t@TParen {} = t
wrapTy True t = TParen t
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
  EAnn _ sub -> isBlockExpr sub
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
  EAnn _ sub -> isGreedyExpr sub
  ECase {} -> True
  EIf {} -> True
  ELambdaPats {} -> True
  ELambdaCase {} -> True
  ELetDecls {} -> True
  EDo {} -> True
  EProc {} -> True
  EApp _ arg | isBlockExpr arg -> isOpenEnded arg
  _ -> False

-- | Check if an expression is "braced" - i.e., the pretty-printer always
-- wraps it in explicit @{ }@ braces, making it self-delimiting, AND the parser
-- can parse the resulting @expr { ... } op rhs@ without parentheses.
-- Self-delimiting expressions do not need parentheses on the left-hand side of
-- an infix operator, because the closing @}@ unambiguously ends the expression.
-- Peel outer 'EAnn' annotations so @do@ / @case@ / @\\case@ nested under span metadata still count.
isBracedExpr :: Expr -> Bool
isBracedExpr = \case
  EAnn _ sub -> isBracedExpr sub
  ECase {} -> True
  EDo {} -> True
  ELambdaCase {} -> True
  _ -> False

-- | Check if an expression is "open-ended" - its rightmost component can
-- capture a trailing where clause.
isOpenEnded :: Expr -> Bool
isOpenEnded = \case
  EAnn _ sub -> isOpenEnded sub
  EIf {} -> True
  ELambdaPats {} -> True
  ELetDecls {} -> True
  EProc {} -> True
  EInfix _ _ rhs -> isOpenEnded rhs
  EApp _ arg | isBlockExpr arg -> isOpenEnded arg
  _ -> False

-- | Does the pretty-printed form of an expression end with @:: Type@?
endsWithTypeSig :: Expr -> Bool
endsWithTypeSig = \case
  EAnn _ sub -> endsWithTypeSig sub
  ETypeSig {} -> True
  ELetDecls _ body -> endsWithTypeSig body
  ELambdaPats _ body -> endsWithTypeSig body
  EInfix _ _ rhs -> endsWithTypeSig rhs
  _ -> False

-- | Check whether an expression's pretty-printed form starts with '$'.
startsWithDollar :: Expr -> Bool
startsWithDollar (EAnn _ sub) = startsWithDollar sub
startsWithDollar (ETHSplice {}) = True
startsWithDollar (ETHTypedSplice {}) = True
startsWithDollar (ERecordUpd base _) = startsWithDollar base
startsWithDollar (EApp fn _) = startsWithDollar fn
startsWithDollar _ = False

startsWithOverloadedLabel :: Expr -> Bool
startsWithOverloadedLabel = \case
  EOverloadedLabel {} -> True
  EAnn _ sub -> startsWithOverloadedLabel sub
  EApp fn _ -> startsWithOverloadedLabel fn
  EInfix lhs _ _ -> startsWithOverloadedLabel lhs
  ERecordUpd base _ -> startsWithOverloadedLabel base
  ETypeSig inner _ -> startsWithOverloadedLabel inner
  ETypeApp fn _ -> startsWithOverloadedLabel fn
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
needsExprParens ctx (EAnn _ sub) = needsExprParens ctx sub
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

-- | @TApp@ view after peeling 'TAnn' / 'TParen' to the application head.
typeTAppView :: Type -> Maybe (Type, Type)
typeTAppView (TAnn _ sub) = typeTAppView sub
typeTAppView (TApp a b) = Just (a, b)
typeTAppView _ = Nothing

typeTConView :: Type -> Maybe (Name, TypePromotion)
typeTConView t =
  case peelTypeHead t of
    TCon n p -> Just (n, p)
    _ -> Nothing

-- | @ty@ has shape @lhs \`op\` rhs@ with a symbolic type operator (modulo span
-- and 'TParen' wrappers).
matchSymbolicInfixTypeApp :: Type -> Maybe (Type, Type, Type)
matchSymbolicInfixTypeApp ty = do
  (l, r) <- typeTAppView ty
  (opAst, lhsAst) <- typeTAppView l
  (opName, _) <- typeTConView opAst
  guard (isSymbolicName opName && renderName opName /= "->")
  pure (opAst, lhsAst, r)

data TypeCtx
  = CtxTypeFunArg
  | CtxTypeAppArg
  | CtxTypeAtom
  | CtxKindSig

needsTypeParens :: TypeCtx -> Type -> Bool
needsTypeParens ctx (TAnn _ sub) = needsTypeParens ctx sub
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
  EApp _ arg | isBlockExpr arg -> guardExprNeedsParens arrow arg
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
    DeclValue vdecl -> DeclValue (addValueDeclParens vdecl)
    DeclTypeSig names ty -> DeclTypeSig names (addTypeParens ty)
    DeclPatSyn ps -> DeclPatSyn (addPatSynDeclParens ps)
    DeclPatSynSig names ty -> DeclPatSynSig names (addTypeParens ty)
    DeclStandaloneKindSig name kind -> DeclStandaloneKindSig name (addTypeParens kind)
    DeclFixity {} -> decl
    DeclRoleAnnotation {} -> decl
    DeclTypeSyn synDecl ->
      DeclTypeSyn (synDecl {typeSynBody = addTypeParens (typeSynBody synDecl)})
    DeclData dataDecl -> DeclData (addDataDeclParens dataDecl)
    DeclTypeData dataDecl -> DeclTypeData (addDataDeclParens dataDecl)
    DeclNewtype newtypeDecl -> DeclNewtype (addNewtypeDeclParens newtypeDecl)
    DeclClass classDecl -> DeclClass (addClassDeclParens classDecl)
    DeclInstance instanceDecl -> DeclInstance (addInstanceDeclParens instanceDecl)
    DeclStandaloneDeriving derivingDecl -> DeclStandaloneDeriving (addStandaloneDerivingParens derivingDecl)
    DeclDefault tys -> DeclDefault (map addTypeParens tys)
    DeclForeign foreignDecl -> DeclForeign (addForeignDeclParens foreignDecl)
    DeclSplice body -> DeclSplice (addDeclSpliceParens body)
    DeclTypeFamilyDecl tf -> DeclTypeFamilyDecl (addTypeFamilyDeclParens tf)
    DeclDataFamilyDecl df -> DeclDataFamilyDecl (addDataFamilyDeclParens df)
    DeclTypeFamilyInst tfi -> DeclTypeFamilyInst (addTypeFamilyInstParens tfi)
    DeclDataFamilyInst dfi -> DeclDataFamilyInst (addDataFamilyInstParens dfi)
    DeclPragma {} -> decl

addDeclSpliceParens :: Expr -> Expr
addDeclSpliceParens body =
  case body of
    EVar {} -> body
    EParen inner -> EParen (addExprParens inner)
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
    GuardAnn ann inner -> GuardAnn ann (addGuardQualifierParens arrow inner)
    GuardExpr expr -> GuardExpr (addGuardExprParens arrow expr)
    GuardPat pat expr -> GuardPat (addPatternParens pat) (addGuardExprParens arrow expr)
    GuardLet decls -> GuardLet (map addDeclParens decls)

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
    DataConAnn ann inner -> DataConAnn ann (addDataConDeclParens inner)
    PrefixCon forallVars constraints name fields ->
      PrefixCon forallVars (addContextConstraints constraints) name (map addBangTypeParens fields)
    InfixCon forallVars constraints lhs op rhs ->
      InfixCon forallVars (addContextConstraints constraints) (addBangTypeAtomParens lhs) op (addBangTypeAtomParens rhs)
    RecordCon forallVars constraints name fields ->
      RecordCon forallVars (addContextConstraints constraints) name (map addRecordFieldDeclParens fields)
    GadtCon forallBinders constraints names body ->
      GadtCon forallBinders (addContextConstraints constraints) names (addGadtBodyParens body)

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
    ClassItemAnn ann sub -> ClassItemAnn ann (addClassItemParens sub)
    ClassItemTypeSig names ty -> ClassItemTypeSig names (addTypeParens ty)
    ClassItemDefaultSig name ty -> ClassItemDefaultSig name (addTypeParens ty)
    ClassItemFixity {} -> item
    ClassItemDefault vdecl -> ClassItemDefault (addValueDeclParens vdecl)
    ClassItemTypeFamilyDecl tf -> ClassItemTypeFamilyDecl (addTypeFamilyDeclParens tf)
    ClassItemDataFamilyDecl df -> ClassItemDataFamilyDecl (addDataFamilyDeclParens df)
    ClassItemDefaultTypeInst tfi -> ClassItemDefaultTypeInst (addTypeFamilyInstParens tfi)
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
    InstanceItemAnn ann inner -> InstanceItemAnn ann (addInstanceItemParens inner)
    InstanceItemBind vdecl -> InstanceItemBind (addValueDeclParens vdecl)
    InstanceItemTypeSig names ty -> InstanceItemTypeSig names (addTypeParens ty)
    InstanceItemFixity {} -> item
    InstanceItemTypeFamilyInst tfi -> InstanceItemTypeFamilyInst (addTypeFamilyInstParens tfi)
    InstanceItemDataFamilyInst dfi -> InstanceItemDataFamilyInst (addDataFamilyInstParens dfi)
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
      dataFamilyInstKind = fmap addTypeParens (dataFamilyInstKind dfi),
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
    go args (EAnn _ x) = go args x
    go args (EApp fn arg) = go (arg : args) fn
    go args root = (root, args)

addExprParensPrec :: Int -> Expr -> Expr
addExprParensPrec prec expr =
  case expr of
    EApp {} -> addAppsChainPrec prec expr
    ETypeApp fn ty ->
      wrapExpr (prec > 2) (ETypeApp (addExprParensIn CtxAppFun fn) (addTypeIn CtxTypeAtom ty))
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
    ETHExpQuote body -> ETHExpQuote (addExprParens body)
    ETHTypedQuote body -> ETHTypedQuote (addExprParens body)
    ETHDeclQuote decls -> ETHDeclQuote (map addDeclParens decls)
    ETHTypeQuote ty -> ETHTypeQuote (addTypeParens ty)
    ETHPatQuote pat -> ETHPatQuote (addPatternParens pat)
    ETHNameQuote {} -> expr
    ETHTypeNameQuote {} -> expr
    ETHSplice body -> ETHSplice (addSpliceBodyParens body)
    ETHTypedSplice body -> ETHTypedSplice (addSpliceBodyParens body)
    EIf cond yes no ->
      wrapExpr
        (prec > 0)
        (EIf (addExprParens cond) (addIfBranchParens yes) (addIfBranchParens no))
    EMultiWayIf rhss ->
      wrapExpr (prec > 0) (EMultiWayIf (map (addGuardedRhsParens GuardArrow) rhss))
    ELambdaPats pats body ->
      wrapExpr (prec > 0) (ELambdaPats (map addLambdaPatternAtomParens pats) (addExprParens body))
    ELambdaCase alts ->
      wrapExpr (prec > 0) (ELambdaCase (map addCaseAltParens alts))
    EInfix lhs op rhs
      | isArrowTailOp (renderName op) ->
          -- Arrow tail operators always parenthesized, LHS also parenthesized
          wrapExpr
            True
            ( EInfix
                (wrapExpr True (addExprParens lhs))
                op
                (addExprParens rhs)
            )
      | otherwise ->
          wrapExpr
            (prec > 1)
            ( EInfix
                (addExprParensIn CtxInfixLhs lhs)
                op
                (addExprParensIn (CtxInfixRhs (prec == 1)) rhs)
            )
    ENegate inner ->
      wrapExpr (prec > 2) (ENegate (addNegateParens inner))
    ESectionL lhs op ->
      -- Sections are always in parens (printed with parens in Pretty.hs)
      -- The LHS needs special handling for greedy/typesig expressions
      let lhs' =
            if isGreedyExpr lhs || isTypeSig lhs
              then wrapExpr True (addExprParens lhs)
              else addExprParensPrec 1 lhs
       in ESectionL lhs' op
    ESectionR op rhs ->
      ESectionR op (addExprParens rhs)
    ELetDecls decls body ->
      wrapExpr (prec > 0) (ELetDecls (map addDeclParens decls) (addExprParens body))
    ECase scrutinee alts ->
      wrapExpr (prec > 0) (ECase (addExprParens scrutinee) (map addCaseAltParens alts))
    EDo stmts isMdo ->
      wrapExpr (prec > 0) (EDo (map addDoStmtParens stmts) isMdo)
    EListComp body quals ->
      EListComp (addExprParens body) (map addCompStmtParens quals)
    EListCompParallel body qualifierGroups ->
      EListCompParallel (addExprParens body) (map (map addCompStmtParens) qualifierGroups)
    EArithSeq seqInfo -> EArithSeq (addArithSeqParens seqInfo)
    ERecordCon name fields hasWildcard ->
      ERecordCon name [(n, addExprParens e) | (n, e) <- fields] hasWildcard
    ERecordUpd base fields ->
      ERecordUpd (addExprParensPrec 3 base) [(n, addExprParens e) | (n, e) <- fields]
    ETypeSig inner ty ->
      wrapExpr (prec > 1) (ETypeSig (addExprParensIn CtxTypeSigBody inner) (addTypeParens ty))
    EParen inner ->
      case inner of
        -- Sections are already "in parens" via their EParen wrapper,
        -- don't add double parens
        ESectionL {} -> EParen (addExprParens inner)
        ESectionR {} -> EParen (addExprParens inner)
        _ -> EParen (addExprParens inner)
    EList values -> EList (map addExprParens values)
    ETuple tupleFlavor values -> ETuple tupleFlavor (map (fmap addExprParens) values)
    EUnboxedSum altIdx arity inner -> EUnboxedSum altIdx arity (addExprParens inner)
    EProc pat body ->
      wrapExpr (prec > 0) (EProc (addPatternParens pat) (addCmdParens body))
    EAnn ann sub -> EAnn ann (addExprParensPrec prec sub)
  where
    isTypeSig :: Expr -> Bool
    isTypeSig (EAnn _ sub) = isTypeSig sub
    isTypeSig (ETypeSig {}) = True
    isTypeSig _ = False

-- | Handle application chains, preserving original EApp spans.
addAppsChainPrec :: Int -> Expr -> Expr
addAppsChainPrec prec expr =
  let (root, args) = flattenApps expr
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
      rebuilt = foldl EApp root' args'
   in wrapExpr (prec > 2) rebuilt

addSpliceBodyParens :: Expr -> Expr
addSpliceBodyParens body =
  case body of
    EAnn ann sub -> EAnn ann (addSpliceBodyParens sub)
    -- EParen around a section: the pretty-printer's EParen transparency
    -- means this would print as just the section's parens. We need an
    -- extra EParen so the splice delimiter parens are not swallowed.
    EParen inner
      | ESectionL {} <- peelExprAnn inner -> EParen (EParen (addExprParens inner))
      | ESectionR {} <- peelExprAnn inner -> EParen (EParen (addExprParens inner))
    EParen inner -> EParen (addExprParens inner)
    EVar {} -> body
    -- Sections print their own parens via prettyExpr, and EParen is
    -- transparent around them in the pretty-printer (to avoid double parens
    -- in normal code like `x = (+1)`). For splices, we need the EParen to
    -- actually produce parens, so we double-wrap.
    _
      | ESectionL {} <- peelExprAnn body -> EParen (EParen (addExprParens body))
      | ESectionR {} <- peelExprAnn body -> EParen (EParen (addExprParens body))
    -- Any other body needs to be wrapped in EParen so it prints as $(expr).
    _ -> EParen (addExprParens body)

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
    DoAnn ann inner -> DoAnn ann (addDoStmtParens inner)
    DoBind pat e -> DoBind (addPatternParens pat) (addExprParens e)
    DoLetDecls decls -> DoLetDecls (map addDeclParens decls)
    DoExpr e -> DoExpr (addExprParens e)
    DoRecStmt stmts -> DoRecStmt (map addDoStmtParens stmts)

addCompStmtParens :: CompStmt -> CompStmt
addCompStmtParens stmt =
  case stmt of
    CompAnn ann inner -> CompAnn ann (addCompStmtParens inner)
    CompGen pat e -> CompGen (addPatternParens pat) (addExprParens e)
    CompGuard e -> CompGuard (addExprParens e)
    CompLetDecls decls -> CompLetDecls (map addDeclParens decls)

addArithSeqParens :: ArithSeq -> ArithSeq
addArithSeqParens seqInfo =
  case seqInfo of
    ArithSeqAnn ann inner -> ArithSeqAnn ann (addArithSeqParens inner)
    ArithSeqFrom fromE -> ArithSeqFrom (addExprGuardedParens fromE)
    ArithSeqFromThen fromE thenE -> ArithSeqFromThen (addExprGuardedParens fromE) (addExprGuardedParens thenE)
    ArithSeqFromTo fromE toE -> ArithSeqFromTo (addExprGuardedParens fromE) (addExprParens toE)
    ArithSeqFromThenTo fromE thenE toE -> ArithSeqFromThenTo (addExprGuardedParens fromE) (addExprGuardedParens thenE) (addExprParens toE)

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
        TAnn ann sub -> TAnn ann (addTypeParensShared ctx prec sub)
        TVar {} -> ty
        TCon name promoted
          | isSymbolicName name, promoted /= Promoted -> TCon name promoted
          | otherwise -> TCon name promoted
        TImplicitParam name inner -> TImplicitParam name (addTypeParens inner)
        TTypeLit {} -> ty
        TStar {} -> ty
        TQuasiQuote {} -> ty
        TForall binders inner ->
          wrapTy (prec > 0) (TForall (map addTyVarBinderParens binders) (atom 0 inner))
        tyInfix
          | Just (op, lhs, rhs) <- matchSymbolicInfixTypeApp tyInfix ->
              -- Infix type operator: args are treated as atoms
              TApp (TApp op (atom 0 lhs)) (atom 0 rhs)
        TApp f x ->
          wrapTy (prec > 2) (TApp (addTypeIn CtxTypeFunArg f) (addTypeIn CtxTypeAppArg x))
        TFun a b ->
          wrapTy (prec > 0) (TFun (addTypeIn CtxTypeFunArg a) (atom 0 b))
        TTuple tupleFlavor promoted elems ->
          TTuple tupleFlavor promoted (map (atom 0) elems)
        TUnboxedSum elems -> TUnboxedSum (map (atom 0) elems)
        TList promoted elems -> TList promoted (map (atom 0) elems)
        -- Inside an explicit TParen, TKindSig does not need an additional
        -- TParen wrapper since the enclosing delimiter already provides it.
        TParen inner -> TParen (addTypeParensInner inner)
        -- TKindSig always needs parens in most contexts. The parser absorbs
        -- (ty :: kind) as TKindSig directly, so the TParen is not preserved
        -- through roundtrips. The pretty-printer relies on the TParen wrapper
        -- to produce the required parentheses.
        TKindSig ty' kind ->
          wrapTy True (TKindSig (atom 0 ty') (atom 0 kind))
        TContext constraints inner ->
          wrapTy (prec > 0) (TContext (addContextConstraints constraints) (atom 0 inner))
        TSplice body -> TSplice (addSpliceBodyParens body)
        TWildcard {} -> ty

addTyVarBinderParens :: TyVarBinder -> TyVarBinder
addTyVarBinderParens tvb =
  tvb {tyVarBinderKind = fmap addTypeParens (tyVarBinderKind tvb)}

-- | Process a type inside explicit delimiters (TParen, TTuple, etc.).
-- TKindSig does not need wrapping here because the enclosing delimiter
-- already provides the necessary parenthesization.
addTypeParensInner :: Type -> Type
addTypeParensInner (TAnn _ sub) = addTypeParensInner sub
addTypeParensInner ty =
  case ty of
    TKindSig ty' kind ->
      TKindSig (addTypeParensShared CtxTypeAtom 0 ty') (addTypeParensShared CtxTypeAtom 0 kind)
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
    TAnn ann sub ->
      TAnn ann (addContextConstraintMulti sub)
    TKindSig ty' kind ->
      -- In multi-constraint context, TKindSig doesn't need wrapping
      TKindSig (addTypeParensShared CtxTypeAtom 0 ty') (addTypeParensShared CtxTypeAtom 0 kind)
    TParen inner@(TKindSig {}) ->
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
    PLit lit -> PLit lit
    PQuasiQuote {} -> pat
    PTuple tupleFlavor elems -> PTuple tupleFlavor (map addPatternInDelimited elems)
    PUnboxedSum altIdx arity inner -> PUnboxedSum altIdx arity (addPatternInDelimited inner)
    PList elems -> PList (map addPatternInDelimited elems)
    PCon con args -> PCon con (map addPatternAtomParens args)
    PInfix lhs op rhs -> PInfix (addPatternAtomParens lhs) op (addPatternAtomParens rhs)
    PView viewExpr inner ->
      wrapPat True (PView (addViewExprParens viewExpr) (addPatternParens inner))
    PAs name inner -> PAs name (addPatternAtomStrictParens inner)
    PStrict inner -> PStrict (addPatternAtomStrictParens inner)
    PIrrefutable inner -> PIrrefutable (addPatternAtomStrictParens inner)
    PNegLit lit -> PNegLit lit
    PParen inner -> PParen (addPatternInDelimited inner)
    PRecord con fields hasWildcard ->
      PRecord con [(fieldName, addPatternParens fieldPat) | (fieldName, fieldPat) <- fields] hasWildcard
    PTypeSig inner ty -> PTypeSig (addPatternParens inner) (addTypeParens ty)
    PSplice body -> PSplice (addSpliceBodyParens body)

-- | Add parens for a pattern inside a delimited context (tuples, lists, etc.).
-- View patterns don't need extra parens there.
addPatternInDelimited :: Pattern -> Pattern
addPatternInDelimited pat =
  case peelPatternAnn pat of
    PView viewExpr inner -> PView (addViewExprParens viewExpr) (addPatternParens inner)
    PAs name inner -> PAs name (addPatternAtomStrictParens inner)
    PStrict inner -> PStrict (addPatternAtomStrictParens inner)
    PIrrefutable inner -> PIrrefutable (addPatternAtomStrictParens inner)
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
    PAnn ann sub -> PAnn ann (addPatternAtomParens sub)
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
    PCon _ [] -> addPatternParens pat
    PInfix _ op _
      | isConsOperator op ->
          -- Cons operator (:) is right-associative, so nested cons patterns
          -- don't need parentheses: x1:x2:xs parses as x1:(x2:xs)
          addPatternParens pat
    _ -> wrapPat True (addPatternParens pat)

-- | Add parens for a pattern in lambda argument position.
addLambdaPatternAtomParens :: Pattern -> Pattern
addLambdaPatternAtomParens pat =
  case pat of
    PAnn ann sub -> PAnn ann (addLambdaPatternAtomParens sub)
    PNegLit {} -> wrapPat True (addPatternParens pat)
    PCon _ [] -> wrapPat True (addPatternParens pat)
    _ -> addPatternAtomParens pat

-- | Add parens for a pattern in function-head argument position.
addFunctionHeadPatternAtomParens :: Pattern -> Pattern
addFunctionHeadPatternAtomParens pat =
  case pat of
    PAnn ann sub -> PAnn ann (addFunctionHeadPatternAtomParens sub)
    PNegLit {} -> wrapPat True (addPatternParens pat)
    PCon _ (_ : _) -> wrapPat True (addPatternParens pat)
    PRecord {} -> addPatternParens pat
    _ -> addPatternAtomParens pat

-- | Add parens for infix function-head operands.
addInfixFunctionHeadPatternAtomParens :: Pattern -> Pattern
addInfixFunctionHeadPatternAtomParens pat =
  case pat of
    PAnn ann sub -> PAnn ann (addInfixFunctionHeadPatternAtomParens sub)
    PNegLit {} -> wrapPat True (addPatternParens pat)
    _ -> addPatternParens pat

-- | Add parens for the inner pattern of @, !, ~.
addPatternAtomStrictParens :: Pattern -> Pattern
addPatternAtomStrictParens pat =
  case pat of
    PAnn ann sub -> PAnn ann (addPatternAtomStrictParens sub)
    PNegLit {} -> wrapPat True (addPatternParens pat)
    PCon _ [] -> wrapPat True (addPatternParens pat)
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
    CmdAnn ann inner -> CmdAnn ann (addCmdParens inner)
    CmdArrApp lhs appTy rhs ->
      CmdArrApp (addExprParensPrec 1 lhs) appTy (addExprParens rhs)
    CmdInfix l op r ->
      CmdInfix (addCmdParens l) op (addCmdParens r)
    CmdDo stmts ->
      CmdDo (map addCmdDoStmtParens stmts)
    CmdIf cond yes no ->
      CmdIf (addExprParens cond) (addCmdParens yes) (addCmdParens no)
    CmdCase scrut alts ->
      CmdCase (addExprParens scrut) (map addCmdCaseAltParens alts)
    CmdLet decls body ->
      CmdLet (map addDeclParens decls) (addCmdParens body)
    CmdLam pats body ->
      CmdLam (map addPatternAtomParens pats) (addCmdParens body)
    CmdApp c e ->
      CmdApp (addCmdParens c) (addExprParensPrec 3 e)
    CmdPar c ->
      CmdPar (addCmdParens c)

addCmdDoStmtParens :: DoStmt Cmd -> DoStmt Cmd
addCmdDoStmtParens stmt =
  case stmt of
    DoAnn ann inner -> DoAnn ann (addCmdDoStmtParens inner)
    DoBind pat cmd' -> DoBind (addPatternParens pat) (addCmdParens cmd')
    DoLetDecls decls -> DoLetDecls (map addDeclParens decls)
    DoExpr cmd' -> DoExpr (addCmdParens cmd')
    DoRecStmt stmts -> DoRecStmt (map addCmdDoStmtParens stmts)

addCmdCaseAltParens :: CmdCaseAlt -> CmdCaseAlt
addCmdCaseAltParens alt =
  alt
    { cmdCaseAltPat = addPatternParens (cmdCaseAltPat alt),
      cmdCaseAltBody = addCmdParens (cmdCaseAltBody alt)
    }
