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
import Data.Bifunctor (bimap)
import Data.Char (isHexDigit)
import Data.Maybe (isJust)
import Data.Text qualified as T

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

wrapCmd :: Bool -> Cmd -> Cmd
wrapCmd True (CmdAnn ann sub) = CmdAnn ann (wrapCmd True sub)
wrapCmd True c@CmdPar {} = c
wrapCmd True c = CmdPar c
wrapCmd False c = c

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
  ELambdaCases {} -> True
  ELetDecls {} -> True
  _ -> False

-- | Check if an expression is "greedy" - i.e., it could consume trailing syntax.
isGreedyExpr :: Expr -> Bool
isGreedyExpr = \case
  EAnn _ sub -> isGreedyExpr sub
  ECase {} -> True
  EIf {} -> True
  EMultiWayIf {} -> True
  ELambdaPats {} -> True
  ELambdaCase {} -> True
  ELambdaCases {} -> True
  ELetDecls {} -> True
  EDo {} -> True
  EProc {} -> True
  EApp _ arg | isBlockExpr arg -> isOpenEnded arg
  EApp _ arg -> isGreedyExpr arg
  _ -> False

-- | Check if an expression is "open-ended" - its rightmost component can
-- capture a trailing where clause.
isOpenEnded :: Expr -> Bool
isOpenEnded = \case
  EAnn _ sub -> isOpenEnded sub
  ECase _ alts -> not (null alts)
  EIf {} -> True
  EMultiWayIf {} -> True
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
  EMultiWayIf rhss ->
    case reverse rhss of
      grhs : _ -> endsWithTypeSig (guardedRhsBody grhs)
      [] -> False
  EInfix _ _ rhs -> endsWithTypeSig rhs
  EApp _ arg -> endsWithTypeSig arg
  EIf _ _ no -> endsWithTypeSig no
  _ -> False

-- | Check whether an expression needs parenthesization before a record dot.
-- Qualified variables (e.g., @A.x@), ambiguous TH name quotes, TH splices
-- (@$x@, @$$x@), primitive literals, MagicHash literals, and identifiers ending in @#@ all
-- need parens to prevent ambiguity:
-- - Qualified names: @A.x.field@ looks like the qualified name @A.x.field@
-- - TH quotes: @'M.x.field@ looks like quoting the qualified name @M.x.field@
-- - TH splices: @$x.field@ and @$$x.field@ parse as a splice followed by an
--   unexpected @.@
-- - Primitive numeric literals: @10#.field@ makes @#.@ part of an operator
-- - MagicHash: @'c'#.field@ or @x#.field@ — the @#.@ merges into an operator
needsParensBeforeDot :: Expr -> Bool
needsParensBeforeDot = \case
  EAnn _ sub -> needsParensBeforeDot sub
  ENegate inner -> startsWithPrimitiveLiteral inner
  EVar name -> isJust (nameQualifier name) || T.isSuffixOf "#" (nameText name)
  ETHSplice body -> spliceBodyNeedsParensBeforeDot body
  ETHTypedSplice body -> spliceBodyNeedsParensBeforeDot body
  -- Most TH value name quotes are already delimited enough for record dot:
  -- @'x.field@, @'().field@, and @'(M.+).field@ parse as field access.
  -- Qualified identifiers are different: @'M.x.field@ quotes @M.x.field@.
  -- Hash-ending names also need grouping because @'x#.field@ does not parse
  -- as record-dot access on the quoted name.
  ETHNameQuote body -> thNameQuoteNeedsParensBeforeDot body
  -- TH type name quotes need the same distinction.  Built-in type
  -- constructors are accepted as @''().field@ and @''[].field@, while
  -- @''T.field@ and @''(T).field@ are rejected unless the quote is grouped.
  ETHTypeNameQuote ty -> thTypeNameQuoteNeedsParensBeforeDot ty
  EInt _ nt _ -> nt /= TInteger
  EFloat _ ft _ -> ft /= TFractional
  -- MagicHash literals: the trailing # merges with . to form an operator
  ECharHash {} -> True
  EStringHash {} -> True
  _ -> False

spliceBodyNeedsParensBeforeDot :: Expr -> Bool
spliceBodyNeedsParensBeforeDot = \case
  EAnn _ sub -> spliceBodyNeedsParensBeforeDot sub
  EVar name ->
    T.isSuffixOf "#" (nameText name)
      || ( isJust (nameQualifier name)
             && case nameType name of
               NameVarId -> True
               NameConId -> True
               NameVarSym -> False
               NameConSym -> False
         )
  EInt _ nt _ -> nt /= TInteger
  EFloat _ ft _ -> ft /= TFractional
  ECharHash {} -> True
  EStringHash {} -> True
  _ -> False

thNameQuoteNeedsParensBeforeDot :: Expr -> Bool
thNameQuoteNeedsParensBeforeDot = \case
  EAnn _ sub -> thNameQuoteNeedsParensBeforeDot sub
  EVar name ->
    T.isSuffixOf "#" (nameText name)
      || ( isJust (nameQualifier name)
             && case nameType name of
               NameVarId -> True
               NameConId -> True
               NameVarSym -> False
               NameConSym -> False
         )
  _ -> False

thTypeNameQuoteNeedsParensBeforeDot :: Type -> Bool
thTypeNameQuoteNeedsParensBeforeDot = \case
  TAnn _ sub -> thTypeNameQuoteNeedsParensBeforeDot sub
  TTuple {} -> False
  TBuiltinCon {} -> False
  TList _ [] -> False
  _ -> True

-- | Check whether an expression's pretty-printed form starts with '$'.
startsWithBlockExpr :: Expr -> Bool
startsWithBlockExpr = \case
  EAnn _ sub -> startsWithBlockExpr sub
  EInfix lhs _ _ -> startsWithBlockExpr lhs
  EApp fn _ -> startsWithBlockExpr fn
  ERecordUpd base _ -> startsWithBlockExpr base
  EGetField base _ -> startsWithBlockExpr base
  ETypeSig inner _ -> startsWithBlockExpr inner
  ETypeApp fn _ -> startsWithBlockExpr fn
  expr -> isBlockExpr expr

-- | Check whether an expression starts with a primitive (unboxed) numeric
-- literal.  This matters before record-dot syntax: @10#.field@ is tokenized
-- differently from @(10#).field@.  Negation itself prints with a separating
-- space, so @- 10#@ does not merge into a negative primitive literal token.
startsWithPrimitiveLiteral :: Expr -> Bool
startsWithPrimitiveLiteral = go
  where
    go (EAnn _ sub) = go sub
    go (EInt _ nt _) = nt /= TInteger
    go (EFloat _ ft _) = ft /= TFractional
    go (EApp fn _) = go fn
    go (ERecordUpd base _) = go base
    go (ETypeApp fn _) = go fn
    go (ETypeSig inner _) = go inner
    go _ = False

-- ---------------------------------------------------------------------------
-- Expression contexts
-- ---------------------------------------------------------------------------

data ExprCtx
  = CtxInfixRhs Bool
  | CtxInfixLhs
  | CtxAppFun
  | CtxAppArg
  | CtxAppArgNoParens
  | CtxAppArgGreedy
  | CtxSectionRhs
  | CtxTypeSigBody
  | CtxGuarded

needsExprParens :: ExprCtx -> Expr -> Bool
needsExprParens ctx (EAnn _ sub) = needsExprParens ctx sub
needsExprParens ctx expr =
  case ctx of
    CtxInfixRhs protectOpenEnded ->
      case expr of
        -- The expression parser builds left-associated chains, so a nested
        -- infix expression on the RHS only appears when the source had
        -- explicit parentheses.
        EInfix {} -> True
        ETypeSig {} -> True
        _ | protectOpenEnded && absorbsFollowingInfix expr -> True
        -- ENegate does NOT need parenthesization in infix RHS position.
        -- GHC already rejects `x + - 1` (precedence >= 6) at parse time,
        -- so any ENegate appearing as the RHS of an infix operator in a
        -- successfully parsed module is guaranteed to be valid without
        -- parentheses. Wrapping in EParen would introduce a spurious HsPar
        -- in the GHC AST, causing roundtrip fingerprint mismatches.
        _ -> False
    CtxInfixLhs ->
      case expr of
        ETypeSig {} -> True
        EMultiWayIf {} -> False
        -- ENegate needs parenthesization when its inner expression is
        -- open-ended (e.g. ends with a `let` body).  Without parens, the
        -- parser re-reads `- f let {} in x \`op\` y` as
        -- `-(f (let {} in (x \`op\` y)))` because the `let` body greedily
        -- consumes the infix.  For non-greedy inner expressions (plain
        -- variables, literals, etc.) no parens are needed: `-x + 1` is
        -- correctly re-parsed as `(-x) + 1`.
        ENegate inner -> isOpenEnded inner
        ECase {} -> False
        _ -> isOpenEnded expr
    CtxAppFun ->
      case expr of
        ENegate {} -> True
        _ -> False
    CtxAppArg ->
      case expr of
        _ | endsWithTypeSig expr -> True
        _ | isBlockExpr expr -> False
        -- A pragma as a function argument needs parens: `fn {-# P #-} x` is
        -- ambiguous; `fn ({-# P #-} x)` is unambiguous.
        EPragma {} -> True
        _ -> False
    CtxAppArgNoParens ->
      False
    CtxAppArgGreedy ->
      case expr of
        ECase {} -> False
        EPragma {} -> True
        _ -> isGreedyExpr expr
    CtxSectionRhs ->
      case expr of
        ETypeSig {} -> True
        _ -> False
    CtxTypeSigBody ->
      case expr of
        ETypeSig {} -> True
        ENegate inner -> isGreedyExpr inner || isOpenEnded inner || endsWithTypeSig inner
        ELambdaPats {} -> True
        ECase {} -> False
        EDo {} -> False
        ELambdaCase {} -> False
        ELambdaCases {} -> False
        _ -> isOpenEnded expr
    CtxGuarded -> isGreedyExpr expr

exprCtxPrec :: ExprCtx -> Expr -> Int
exprCtxPrec ctx expr =
  case ctx of
    CtxInfixRhs _
      | isGreedyExpr expr -> 0
      | otherwise -> 1
    CtxInfixLhs
      | isBlockExpr expr -> 0
      | otherwise -> 1
    CtxAppFun -> 2
    CtxAppArg
      | isBlockExpr expr -> 0
      | otherwise -> 3
    CtxAppArgNoParens -> 0
    CtxAppArgGreedy
      | isBlockExpr expr -> 0
      | otherwise -> 3
    CtxSectionRhs -> 0
    CtxTypeSigBody
      | ECase {} <- peelExprAnn expr -> 0
      | EDo {} <- peelExprAnn expr -> 0
      | otherwise -> 1
    CtxGuarded -> 0

-- | Parenthesize a left section operand while protecting any rightmost infix
-- RHS from reassociating with the section operator.
addSectionLhsParens :: Expr -> Expr
addSectionLhsParens expr =
  case peelExprAnn expr of
    EInfix lhs op rhs ->
      EInfix
        (addExprParensIn CtxInfixLhs lhs)
        op
        (addSectionInfixRhsParens rhs)
    ENegate inner
      | isGreedyExpr inner || isOpenEnded inner ->
          wrapExpr True (addExprParens expr)
    _ -> addExprParensPrec 1 expr
  where
    addSectionInfixRhsParens rhs =
      case peelExprAnn rhs of
        EInfix {} -> wrapExpr True (addSectionLhsParens rhs)
        _ | isOpenEnded rhs -> wrapExpr True (addExprParens rhs)
        _ -> addExprParensIn (CtxInfixRhs False) rhs

-- ---------------------------------------------------------------------------
-- Type contexts
-- ---------------------------------------------------------------------------

data TypeCtx
  = CtxTypeFunArg
  | CtxTypeAppFun
  | CtxTypeAppArg
  | CtxTypeAppVisibleArg
  | CtxTypeFamilyOperand
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
        -- TImplicitParam parses greedily: ?x :: T -> U absorbs -> U into the
        -- implicit param type, so TFun (TImplicitParam ..) .. needs parens.
        TImplicitParam {} -> True
        _ -> False
    CtxTypeAppFun ->
      case ty of
        TForall {} -> True
        TFun {} -> True
        TContext {} -> True
        TInfix {} -> True
        TImplicitParam {} -> True
        _ -> False
    CtxTypeAppArg ->
      case ty of
        TQuasiQuote {} -> False
        TApp {} -> True
        TTypeApp {} -> True
        TForall {} -> True
        TFun {} -> True
        TContext {} -> True
        TInfix {} -> True
        TSplice {} -> False
        TImplicitParam {} -> True
        _ -> False
    CtxTypeAppVisibleArg ->
      case ty of
        TQuasiQuote {} -> False
        TApp {} -> True
        TTypeApp {} -> True
        TForall {} -> True
        TFun {} -> True
        TContext {} -> True
        TInfix {} -> True
        -- TStar renders as @*@, which merges with the preceding @\@@ in
        -- visible type application to form a single operator token @\@*@.
        TStar {} -> True
        -- TSplice renders with a leading '$', which merges with the preceding
        -- visible type-application '@' into an operator token.
        TSplice {} -> True
        -- TImplicitParam parses greedily: as a TApp argument ?x :: T -> U absorbs
        -- the surrounding -> U into the implicit param type.
        TImplicitParam {} -> True
        _ -> False
    CtxTypeFamilyOperand ->
      case ty of
        TForall {} -> True
        TFun {} -> True
        TContext {} -> True
        TInfix {} -> True
        TImplicitParam {} -> True
        TKindSig {} -> True
        _ -> False
    CtxTypeAtom ->
      case ty of
        TVar {} -> False
        TCon {} -> False
        TBuiltinCon {} -> False
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
        TInfix {} -> True
        _ -> True
    CtxKindSig ->
      case ty of
        TKindSig {} -> False
        _ -> True

-- ---------------------------------------------------------------------------
-- Guard contexts
-- ---------------------------------------------------------------------------

data GuardArrow = GuardArrow | GuardEquals

guardExprNeedsParensWith :: (Type -> Bool) -> GuardArrow -> Expr -> Bool
guardExprNeedsParensWith typeSigNeedsParens arrow = \case
  EAnn _ sub -> guardExprNeedsParensWith typeSigNeedsParens arrow sub
  EProc {} -> True
  ETypeSig _ ty ->
    case arrow of
      GuardArrow -> typeSigNeedsParens ty
      GuardEquals -> False
  EApp _ arg | isBlockExpr arg -> guardExprNeedsParensWith typeSigNeedsParens arrow arg
  expr ->
    case arrow of
      GuardArrow -> trailingTypeSigNeedsParens typeSigNeedsParens expr
      GuardEquals -> False

trailingTypeSigNeedsParens :: (Type -> Bool) -> Expr -> Bool
trailingTypeSigNeedsParens typeSigNeedsParens expr =
  case expr of
    EAnn _ sub -> trailingTypeSigNeedsParens typeSigNeedsParens sub
    ETypeSig _ ty -> typeSigNeedsParens ty
    ELetDecls _ body -> trailingTypeSigNeedsParens typeSigNeedsParens body
    ELambdaPats _ body -> trailingTypeSigNeedsParens typeSigNeedsParens body
    EInfix _ _ rhs -> trailingTypeSigNeedsParens typeSigNeedsParens rhs
    EApp _ arg -> trailingTypeSigNeedsParens typeSigNeedsParens arg
    EIf _ _ no -> trailingTypeSigNeedsParens typeSigNeedsParens no
    _ -> False

typeHasFunctionArrow :: Type -> Bool
typeHasFunctionArrow ty =
  case ty of
    TAnn _ sub -> typeHasFunctionArrow sub
    TParen sub -> typeHasFunctionArrow sub
    TForall _ inner -> typeHasFunctionArrow inner
    TContext _ inner -> typeHasFunctionArrow inner
    TKindSig lhs rhs -> typeHasFunctionArrow lhs || typeHasFunctionArrow rhs
    TFun {} -> True
    _ -> False

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
      DeclTypeSyn (synDecl {typeSynBody = addTypeTopLevelParens (typeSynBody synDecl)})
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
addDeclSpliceParens = addExprParens

addValueDeclParens :: ValueDecl -> ValueDecl
addValueDeclParens vdecl =
  case vdecl of
    PatternBind multTag pat rhs -> PatternBind multTag (addPatternBindLhsParens pat rhs) (addRhsParens rhs)
    FunctionBind name matches -> FunctionBind name (map (addMatchParens name) matches)

addPatternBindLhsParens :: Pattern -> Rhs Expr -> Pattern
addPatternBindLhsParens pat rhs =
  case pat of
    PAnn ann sub -> PAnn ann (addPatternBindLhsParens sub rhs)
    PTypeSig inner ty ->
      wrapPat
        (typedPatternBindLhsNeedsParens inner)
        (PTypeSig (addPatternInfixOperandParens inner) (addTypeParens ty))
    _ -> addPatternParens pat

typedPatternBindLhsNeedsParens :: Pattern -> Bool
typedPatternBindLhsNeedsParens (PAnn _ sub) = typedPatternBindLhsNeedsParens sub
typedPatternBindLhsNeedsParens PCon {} = True
typedPatternBindLhsNeedsParens _ = False

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

addRhsParens :: Rhs Expr -> Rhs Expr
addRhsParens rhs =
  case rhs of
    UnguardedRhs sp body whereDecls ->
      UnguardedRhs sp (addExprParens body) (fmap (map addDeclParens) whereDecls)
    GuardedRhss sp guards whereDecls ->
      GuardedRhss sp (map (addGuardedRhsParens GuardEquals) guards) (fmap (map addDeclParens) whereDecls)

addGuardedRhsParens :: GuardArrow -> GuardedRhs Expr -> GuardedRhs Expr
addGuardedRhsParens arrow grhs =
  grhs
    { guardedRhsGuards = addGuardQualifiersParens arrow (guardedRhsGuards grhs),
      guardedRhsBody = addExprParens (guardedRhsBody grhs)
    }

addGuardQualifiersParens :: GuardArrow -> [GuardQualifier] -> [GuardQualifier]
addGuardQualifiersParens arrow quals =
  case quals of
    [] -> []
    [qual] -> [addGuardQualifierParens arrow True qual]
    qual : rest -> addGuardQualifierParens arrow False qual : addGuardQualifiersParens arrow rest

addGuardQualifierParens :: GuardArrow -> Bool -> GuardQualifier -> GuardQualifier
addGuardQualifierParens arrow isLast qual =
  case qual of
    GuardAnn ann inner -> GuardAnn ann (addGuardQualifierParens arrow isLast inner)
    GuardExpr expr -> GuardExpr (addGuardExprParens arrow expr)
    GuardPat pat expr -> GuardPat (addPatternParens pat) (addPatternGuardExprParens arrow isLast expr)
    GuardLet decls -> GuardLet (map addDeclParens decls)

addGuardExprParens :: GuardArrow -> Expr -> Expr
addGuardExprParens arrow expr =
  wrapExpr (guardExprNeedsParensWith (const True) arrow expr) (addExprParens expr)

addPatternGuardExprParens :: GuardArrow -> Bool -> Expr -> Expr
addPatternGuardExprParens arrow isLast expr =
  let typeSigNeedsParens
        | isLast = const True
        | otherwise = typeHasFunctionArrow
   in wrapExpr (guardExprNeedsParensWith typeSigNeedsParens arrow expr) (addExprParens expr)

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
    { dataDeclCTypePragma = fmap stripPragmaRawText (dataDeclCTypePragma decl),
      dataDeclContext = addContextConstraints (dataDeclContext decl),
      dataDeclKind = fmap addTypeParens (dataDeclKind decl),
      dataDeclConstructors = map addDataConDeclParens (dataDeclConstructors decl),
      dataDeclDeriving = map addDerivingClauseParens (dataDeclDeriving decl)
    }

addNewtypeDeclParens :: NewtypeDecl -> NewtypeDecl
addNewtypeDeclParens decl =
  decl
    { newtypeDeclCTypePragma = fmap stripPragmaRawText (newtypeDeclCTypePragma decl),
      newtypeDeclContext = addContextConstraints (newtypeDeclContext decl),
      newtypeDeclKind = fmap addTypeParens (newtypeDeclKind decl),
      newtypeDeclConstructor = fmap addDataConDeclParens (newtypeDeclConstructor decl),
      newtypeDeclDeriving = map addDerivingClauseParens (newtypeDeclDeriving decl)
    }

stripPragmaRawText :: Pragma -> Pragma
stripPragmaRawText pragma = pragma {pragmaRawText = ""}

addDerivingClauseParens :: DerivingClause -> DerivingClause
addDerivingClauseParens dc =
  dc
    { derivingClasses =
        case derivingClasses dc of
          Left name -> Left name
          Right classes -> Right (map addTypeParens classes),
      derivingStrategy = fmap addDerivingStrategyParens (derivingStrategy dc)
    }

addDerivingStrategyParens :: DerivingStrategy -> DerivingStrategy
addDerivingStrategyParens strategy =
  case strategy of
    DerivingVia ty -> DerivingVia (addTypeParens ty)
    _ -> strategy

addDataConDeclParens :: DataConDecl -> DataConDecl
addDataConDeclParens con =
  case con of
    DataConAnn ann inner -> DataConAnn ann (addDataConDeclParens inner)
    PrefixCon forallVars constraints name fields ->
      PrefixCon (map addTyVarBinderParens forallVars) (addContextConstraints constraints) name (map addPrefixConBangTypeParens fields)
    InfixCon forallVars constraints lhs op rhs ->
      InfixCon (map addTyVarBinderParens forallVars) (addContextConstraints constraints) (addInfixConBangTypeParens lhs) op (addInfixConBangTypeParens rhs)
    RecordCon forallVars constraints name fields ->
      RecordCon (map addTyVarBinderParens forallVars) (addContextConstraints constraints) name (map addRecordFieldDeclParens fields)
    GadtCon forallBinders constraints names body ->
      GadtCon forallBinders (addContextConstraints constraints) names (addGadtBodyParens body)
    TupleCon forallVars constraints flavor fields ->
      TupleCon (map addTyVarBinderParens forallVars) (addContextConstraints constraints) flavor (map addPrefixConBangTypeParens fields)
    UnboxedSumCon forallVars constraints pos arity field ->
      UnboxedSumCon (map addTyVarBinderParens forallVars) (addContextConstraints constraints) pos arity (addPrefixConBangTypeParens field)
    ListCon forallVars constraints ->
      ListCon (map addTyVarBinderParens forallVars) (addContextConstraints constraints)

addBangTypeParens :: BangType -> BangType
addBangTypeParens bt =
  bt
    { bangType =
        wrapTy
          ( (bangStrict bt || bangLazy bt)
              && bangTypeNeedsPrefixParens (bangType bt)
          )
          (addTypeIn CtxTypeAtom (bangType bt))
    }

-- | Determine whether a type, when directly following a bang (!) or lazy (~)
-- prefix, needs to be parenthesized to avoid lexer ambiguity. This happens
-- when the type's rendering starts with a symbol character that would fuse
-- with the ! or ~ to form a single operator token.
bangTypeNeedsPrefixParens :: Type -> Bool
bangTypeNeedsPrefixParens (TAnn _ sub) = bangTypeNeedsPrefixParens sub
bangTypeNeedsPrefixParens (TParen _) = False
bangTypeNeedsPrefixParens TStar = True
bangTypeNeedsPrefixParens (TCon _ Promoted) = True
bangTypeNeedsPrefixParens (TBuiltinCon TBuiltinCons) = True
bangTypeNeedsPrefixParens (TBuiltinCon _) = False
bangTypeNeedsPrefixParens TImplicitParam {} = True
bangTypeNeedsPrefixParens TSplice {} = True
-- Compound types: the first rendered character comes from the head/lhs.
bangTypeNeedsPrefixParens (TApp f _) = bangTypeNeedsPrefixParens f
bangTypeNeedsPrefixParens (TTypeApp f _) = bangTypeNeedsPrefixParens f
bangTypeNeedsPrefixParens (TFun _ a _) = bangTypeNeedsPrefixParens a
bangTypeNeedsPrefixParens (TKindSig ty _) = bangTypeNeedsPrefixParens ty
bangTypeNeedsPrefixParens (TContext cs _) = case cs of
  [] -> False
  (c : _) -> bangTypeNeedsPrefixParens c
bangTypeNeedsPrefixParens (TForall {}) = True
-- Promoted tuples/lists start with a tick mark.
bangTypeNeedsPrefixParens (TTuple _ promoted _) = promoted == Promoted
bangTypeNeedsPrefixParens (TList promoted _) = promoted == Promoted
-- Infix types render lhs first.
bangTypeNeedsPrefixParens (TInfix lhs _ _ _) = bangTypeNeedsPrefixParens lhs
bangTypeNeedsPrefixParens _ = False

addPrefixConBangTypeParens :: BangType -> BangType
addPrefixConBangTypeParens bt =
  let inner = addBangTypeParens bt
   in inner
        { bangType =
            wrapTy (prefixConBangTypeNeedsParens (bangType bt)) (bangType inner)
        }

addInfixConBangTypeParens :: BangType -> BangType
addInfixConBangTypeParens bt =
  bt
    { bangType =
        wrapTy
          ( ( (bangStrict bt || bangLazy bt)
                && bangTypeNeedsPrefixParens (bangType bt)
            )
              || needsTypeParens CtxTypeFunArg (bangType bt)
              || infixConOperandNeedsParens (bangType bt)
          )
          (addTypeIn CtxTypeFunArg (bangType bt))
    }

-- | Types that would be misinterpreted as data constructor declarations
-- (unboxed sum-con or list-con) when used as an infix constructor operand.
-- Tuple operands are left bare so the raw surface syntax is preserved. The
-- data-con parsers are tried before the infix parser, so the remaining types
-- must be parenthesized to prevent ambiguity.
infixConOperandNeedsParens :: Type -> Bool
infixConOperandNeedsParens (TAnn _ sub) = infixConOperandNeedsParens sub
infixConOperandNeedsParens (TParen _) = False
infixConOperandNeedsParens (TTuple Boxed _ _) = False
infixConOperandNeedsParens (TTuple Unboxed _ _) = False
infixConOperandNeedsParens (TUnboxedSum {}) = True
infixConOperandNeedsParens (TList _ []) = True
infixConOperandNeedsParens (TBuiltinCon TBuiltinList) = True
infixConOperandNeedsParens (TInfix {}) = True
-- Application head determines what the parser sees first.
infixConOperandNeedsParens (TApp f _) = infixConOperandNeedsParens f
infixConOperandNeedsParens (TTypeApp _ TSplice {}) = True
infixConOperandNeedsParens (TTypeApp f _) = infixConOperandNeedsParens f
infixConOperandNeedsParens _ = False

prefixConBangTypeNeedsParens :: Type -> Bool
prefixConBangTypeNeedsParens (TAnn _ sub) = prefixConBangTypeNeedsParens sub
prefixConBangTypeNeedsParens TImplicitParam {} = True
prefixConBangTypeNeedsParens _ = False

addRecordFieldDeclParens :: FieldDecl -> FieldDecl
addRecordFieldDeclParens fd =
  fd
    { fieldMultiplicity = fmap addTypeParens (fieldMultiplicity fd),
      fieldType = addRecordFieldBangTypeParens (fieldType fd)
    }

addRecordFieldBangTypeParens :: BangType -> BangType
addRecordFieldBangTypeParens bt =
  bt
    { bangType =
        wrapTy
          ( (bangStrict bt || bangLazy bt)
              && bangTypeNeedsPrefixParens (bangType bt)
          )
          (addTypeParens (bangType bt))
    }

addGadtBodyParens :: GadtBody -> GadtBody
addGadtBodyParens body =
  case body of
    GadtPrefixBody args resultTy ->
      -- The GADT prefix body parser splits on -> arrows (using typeInfixParser
      -- per component). A result type that is itself a TFun, TForall, TContext,
      -- TImplicitParam, or TKindSig would be misinterpreted as additional
      -- constructor arguments, so we use addTypeIn CtxTypeFunArg to wrap it.
      GadtPrefixBody (map (Data.Bifunctor.bimap addGadtBangTypeParens addArrowKindParens) args) (addTypeIn CtxTypeFunArg resultTy)
    GadtRecordBody fields resultTy ->
      -- Record GADT result type uses typeParser so any type is fine here.
      GadtRecordBody (map addRecordFieldDeclParens fields) (addTypeParens resultTy)

addGadtBangTypeParens :: BangType -> BangType
addGadtBangTypeParens bt =
  bt
    { bangType =
        wrapTy
          ( (bangStrict bt || bangLazy bt)
              && bangTypeNeedsPrefixParens (bangType bt)
          )
          (addTypeIn CtxTypeFunArg (bangType bt))
    }

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
      instanceDeclHead = addTypeParens (instanceDeclHead decl),
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
    { standaloneDerivingStrategy = fmap addDerivingStrategyParens (standaloneDerivingStrategy decl),
      standaloneDerivingContext = addContextConstraints (standaloneDerivingContext decl),
      standaloneDerivingHead = addTypeParens (standaloneDerivingHead decl)
    }

addForeignDeclParens :: ForeignDecl -> ForeignDecl
addForeignDeclParens decl =
  decl
    { foreignType = addTypeParens (foreignType decl)
    }

addTypeFamilyDeclParens :: TypeFamilyDecl -> TypeFamilyDecl
addTypeFamilyDeclParens tf =
  tf
    { typeFamilyDeclExplicitFamilyKeyword = typeFamilyDeclExplicitFamilyKeyword tf,
      typeFamilyDeclHead = addTypeParens (typeFamilyDeclHead tf),
      typeFamilyDeclResultSig = fmap addTypeFamilyResultSigParens (typeFamilyDeclResultSig tf),
      typeFamilyDeclEquations = fmap (map addTypeFamilyEqParens) (typeFamilyDeclEquations tf)
    }

addTypeFamilyResultSigParens :: TypeFamilyResultSig -> TypeFamilyResultSig
addTypeFamilyResultSigParens sig =
  case sig of
    TypeFamilyKindSig kind -> TypeFamilyKindSig (addTypeParens kind)
    TypeFamilyTyVarSig result -> TypeFamilyTyVarSig (addTyVarBinderParens result)
    TypeFamilyInjectiveSig result injectivity -> TypeFamilyInjectiveSig (addTyVarBinderParens result) injectivity

addTypeFamilyEqParens :: TypeFamilyEq -> TypeFamilyEq
addTypeFamilyEqParens eq =
  eq
    { typeFamilyEqLhs = addTypeFamilyLhsParens (typeFamilyEqHeadForm eq) (typeFamilyEqLhs eq),
      typeFamilyEqRhs = addTypeFamilyRhsParens (typeFamilyEqRhs eq)
    }

addDataFamilyDeclParens :: DataFamilyDecl -> DataFamilyDecl
addDataFamilyDeclParens df =
  df
    { dataFamilyDeclKind = fmap addTypeParens (dataFamilyDeclKind df)
    }

addTypeFamilyInstParens :: TypeFamilyInst -> TypeFamilyInst
addTypeFamilyInstParens tfi =
  tfi
    { typeFamilyInstLhs = addTypeFamilyLhsParens (typeFamilyInstHeadForm tfi) (typeFamilyInstLhs tfi),
      typeFamilyInstRhs = addTypeFamilyRhsParens (typeFamilyInstRhs tfi)
    }

addTypeFamilyLhsParens :: TypeHeadForm -> Type -> Type
addTypeFamilyLhsParens headForm ty =
  case headForm of
    TypeHeadPrefix -> addTypeParens ty
    TypeHeadInfix ->
      case peelTypeAnn ty of
        TApp l r ->
          case peelTypeAnn l of
            TApp op lhs ->
              TApp
                (TApp (addTypeParens op) (addTypeIn CtxTypeFamilyOperand lhs))
                (addTypeIn CtxTypeFamilyOperand r)
            _ -> addTypeParens ty
        _ -> addTypeParens ty

addTypeFamilyRhsParens :: Type -> Type
addTypeFamilyRhsParens = addTypeTopLevelParens

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
addExprParensIn ctx@(CtxInfixRhs protectOpenEnded) expr@EInfix {} =
  wrapExpr (needsExprParens ctx expr) (addInfixRhsParens protectOpenEnded expr)
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
      let fn' = wrapExpr (isGreedyExpr fn) (addExprParensIn CtxAppFun fn)
       in wrapExpr (prec > 2) (ETypeApp fn' (addTypeIn CtxTypeAppVisibleArg ty))
    ETypeSyntax form ty -> wrapExpr (prec > 2) (ETypeSyntax form (addTypeParens ty))
    EVar {} -> expr
    EInt {} -> expr
    EFloat {} -> expr
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
    ETHPatQuote pat -> ETHPatQuote (addTHPatQuoteParens pat)
    ETHNameQuote body -> ETHNameQuote (addExprParens body)
    ETHTypeNameQuote ty -> ETHTypeNameQuote (addTypeParens ty)
    ETHSplice body -> ETHSplice (addSpliceBodyParens body)
    ETHTypedSplice body -> ETHTypedSplice (addSpliceBodyParens body)
    EIf cond yes no ->
      wrapExpr
        (prec > 0)
        (EIf (addExprParens cond) (addExprParens yes) (addExprParens no))
    EMultiWayIf rhss ->
      wrapExpr (prec > 0) (EMultiWayIf (map (addGuardedRhsParens GuardArrow) rhss))
    ELambdaPats pats body ->
      wrapExpr (prec > 0) (ELambdaPats (map addArrowBndrPatternParens pats) (addExprParens body))
    ELambdaCase alts ->
      wrapExpr (prec > 0) (ELambdaCase (map addCaseAltParens alts))
    ELambdaCases alts ->
      wrapExpr (prec > 0) (ELambdaCases (map addLambdaCaseAltParens alts))
    EInfix lhs op rhs ->
      wrapExpr
        (prec > 1)
        ( EInfix
            (addTopLevelInfixLhsParens prec lhs)
            op
            (addExprParensIn (CtxInfixRhs (prec == 1)) rhs)
        )
    ENegate inner ->
      wrapExpr (prec > 2) (ENegate (addNegateParens inner))
    ESectionL lhs op ->
      -- Sections always require surrounding parens in source syntax.
      -- Wrap in EParen so the AST matches what the parser produces.
      let lhs' =
            if isGreedyExpr lhs || isTypeSig lhs
              then wrapExpr True (addExprParens lhs)
              else addSectionLhsParens lhs
       in EParen (ESectionL lhs' op)
    ESectionR op rhs ->
      -- Right sections accept ordinary expressions as operands. Only explicit
      -- type signatures need extra grouping to avoid @(`op` x :: T)@.
      EParen (ESectionR op (addExprParensIn CtxSectionRhs rhs))
    ELetDecls decls body ->
      wrapExpr (prec > 0) (ELetDecls (map addDeclParens decls) (addExprParens body))
    ECase scrutinee alts ->
      wrapExpr (prec > 0) (ECase (addExprParens scrutinee) (map addCaseAltParens alts))
    EDo stmts flavor ->
      wrapExpr (prec > 0) (EDo (map addDoStmtParens stmts) flavor)
    EListComp body quals ->
      EListComp (addExprParens body) (map addCompStmtParens quals)
    EListCompParallel body qualifierGroups ->
      EListCompParallel (addExprParens body) (map (map addCompStmtParens) qualifierGroups)
    EArithSeq seqInfo -> EArithSeq (addArithSeqParens seqInfo)
    ERecordCon name fields hasWildcard ->
      ERecordCon name [field {recordFieldValue = addExprParens (recordFieldValue field)} | field <- fields] hasWildcard
    ERecordUpd base fields ->
      ERecordUpd (addExprParensPrec 3 base) [field {recordFieldValue = addExprParens (recordFieldValue field)} | field <- fields]
    EGetField base field ->
      -- Qualified names (A.a) must be parenthesized because A.a.field would be
      -- parsed as the qualified name A.a.field rather than field access on A.a.
      let base' = addExprParensPrec 3 base
       in EGetField (wrapExpr (needsParensBeforeDotField base' field) base') field
    EGetFieldProjection {} -> EParen expr
    ETypeSig inner ty ->
      wrapExpr (prec > 1) (ETypeSig (addTypeSigBodyParens inner) (addTypeParens ty))
    EParen inner ->
      -- If inner is a section or projection, addExprParens(inner) already produces EParen(section/projection).
      -- Delegating avoids double-wrapping and maintains idempotency.
      case peelExprAnn inner of
        ESectionL {} -> addExprParens inner
        ESectionR {} -> addExprParens inner
        EGetFieldProjection {} -> addExprParens inner
        _ -> EParen (addExprParens inner)
    EList values -> EList (map addExprParens values)
    ETuple tupleFlavor values -> ETuple tupleFlavor (map (fmap addExprParens) values)
    EUnboxedSum altIdx arity inner -> EUnboxedSum altIdx arity (addExprParens inner)
    EProc pat body ->
      wrapExpr (prec > 0) (EProc (addArrowBndrPatternParens pat) (addCmdParensIn CtxCmdTop body))
    EPragma pragma inner ->
      -- EPragma is transparent w.r.t. precedence: wrapping decisions are made
      -- by the outer context via needsExprParens, not by a self-prec check.
      EPragma pragma (addExprParens inner)
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
              canStayBare = isBlockExpr a && not (endsWithTypeSig a) && (isLast || not (isOpenEnded a))
              ctx
                | canStayBare = CtxAppArgNoParens
                | isLast = CtxAppArg
                | isGreedyExpr a = CtxAppArgGreedy
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
    -- Bare variable names, including qualified names like $A.a, use the
    -- compact splice syntax. Symbolic names still render with their own
    -- operator parentheses, e.g. $(A.+).
    EVar {} -> body
    -- Tuple and unboxed-sum syntax is already delimited, so the splice marker
    -- can attach directly: $(), $$(1, 2), $(# | x #).
    ETuple tupleFlavor values -> ETuple tupleFlavor (map (fmap addExprParens) values)
    EUnboxedSum altIdx arity inner -> EUnboxedSum altIdx arity (addExprParens inner)
    -- For everything else (including sections, which addExprParens now wraps
    -- in EParen): wrap in one outer EParen so the body prints as $(expr).
    -- A body that was already parenthesized keeps exactly one wrapper for
    -- sections/projections, whose addExprParens result is itself an EParen.
    EParen inner ->
      case peelExprAnn inner of
        ESectionL {} -> addExprParens inner
        ESectionR {} -> addExprParens inner
        EGetFieldProjection {} -> addExprParens inner
        _ -> EParen (addExprParens inner)
    _ -> EParen (addExprParens body)

needsParensBeforeDotField :: Expr -> Name -> Bool
needsParensBeforeDotField base field =
  needsParensBeforeDot base || hexIntegerLiteralDotNeedsParens base field

-- @0x78.a@ is tokenized by GHC as a hexadecimal fractional literal, not as
-- record-dot access. Other integer bases, and non-hex field initials, are safe.
hexIntegerLiteralDotNeedsParens :: Expr -> Name -> Bool
hexIntegerLiteralDotNeedsParens expr field =
  case expr of
    EAnn _ sub -> hexIntegerLiteralDotNeedsParens sub field
    EInt _ TInteger repr -> isHexIntegerLiteral repr && fieldStartsWithHexDigit field
    _ -> False
  where
    isHexIntegerLiteral repr =
      "0x" `T.isPrefixOf` repr || "0X" `T.isPrefixOf` repr
    fieldStartsWithHexDigit name =
      case T.uncons (nameText name) of
        Just (c, _) -> isHexDigit c
        Nothing -> False

addNegateParens :: Expr -> Expr
addNegateParens inner =
  case peelExprAnn inner of
    -- `-(518# {}).a` and similar forms must keep the field access grouped;
    -- otherwise `-518# {}.a` is lexed as a negative primitive literal record update.
    EGetField base _ | startsWithPrimitiveLiteral base -> wrapExpr True (addExprParens inner)
    -- "- - x" is invalid. Tested by `test_parenthesesInsertion`
    ENegate {} -> wrapExpr True (addExprParens inner)
    -- Prefix negation accepts block-shaped operands directly, including
    -- `proc`, so `- \x -> x`, `- do ...`, and `- proc ...` do not need an
    -- extra HsPar around the operand.
    _ | isBareNegateOperand inner -> addExprParens inner
    -- Application and type-application bind tighter than negation, so `-f x`
    -- does not need parens around `f x`.
    _ -> addExprParensPrec 2 inner

isBareNegateOperand :: Expr -> Bool
isBareNegateOperand = \case
  EAnn _ sub -> isBareNegateOperand sub
  EProc {} -> True
  expr -> isBlockExpr expr

addTypeSigBodyParens :: Expr -> Expr
addTypeSigBodyParens expr =
  case expr of
    EAnn ann sub -> EAnn ann (addTypeSigBodyParens sub)
    ELambdaCase alts -> ELambdaCase (map addCaseAltParens alts)
    ELambdaCases alts -> ELambdaCases (map addLambdaCaseAltParens alts)
    _ -> addExprParensIn CtxTypeSigBody expr

addCaseAltParens :: CaseAlt Expr -> CaseAlt Expr
addCaseAltParens (CaseAlt sp pat rhs) =
  CaseAlt sp (addCaseAltPatternParens pat) (addCaseAltRhsParens rhs)

addCaseAltPatternParens :: Pattern -> Pattern
addCaseAltPatternParens pat@(PTypeSig {}) = wrapPat True (addPatternParens pat)
addCaseAltPatternParens (PAnn ann sub) = PAnn ann (addCaseAltPatternParens sub)
addCaseAltPatternParens pat = addPatternParens pat

addLambdaCaseAltParens :: LambdaCaseAlt -> LambdaCaseAlt
addLambdaCaseAltParens (LambdaCaseAlt sp pats rhs) =
  let pats' = case pats of
        [] -> []
        [_] -> map addFunctionHeadPatternAtomParens pats
        _ -> map addPatternAtomParens pats
   in LambdaCaseAlt sp pats' (addCaseAltRhsParens rhs)

addCaseAltRhsParens :: Rhs Expr -> Rhs Expr
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
    CompThen f -> CompThen (addExprParens f)
    -- In 'then f by e', the expression 'f' must not be greedy (let/if/lambda/etc.)
    -- because the parser's compTransformExprParser dispatches let/if/lambda to their
    -- standard parsers which consume 'by' as part of the body. Parenthesize greedy
    -- expressions to prevent 'by' from being swallowed.
    CompThenBy f e -> CompThenBy (addCompTransformExprParens f) (addExprParens e)
    CompGroupUsing f -> CompGroupUsing (addExprParens f)
    -- Same issue: 'then group by e using f' — 'e' must not swallow 'using'.
    CompGroupByUsing e f -> CompGroupByUsing (addCompTransformExprParens e) (addExprParens f)

-- | Parenthesize an expression for use in TransformListComp positions where
-- a trailing keyword ('by'/'using') must not be consumed by the expression.
-- The parser's compTransformExprParser uses negateExprParser which calls the
-- standard appExprParser (not the restricted one), so any multi-token expression
-- risks swallowing the keyword. We parenthesize all non-atomic expressions to
-- be safe.
addCompTransformExprParens :: Expr -> Expr
addCompTransformExprParens expr =
  let parenthesized = addExprParens expr
   in if needsCompTransformParens parenthesized
        then wrapExpr True parenthesized
        else parenthesized

-- | Check if an expression needs parenthesization in a TransformListComp
-- position before 'by' or 'using'. Atomic/self-delimiting expressions are safe;
-- anything else could consume the keyword as part of its body or as applications.
needsCompTransformParens :: Expr -> Bool
needsCompTransformParens = \case
  EAnn _ sub -> needsCompTransformParens sub
  -- Atoms: safe, won't consume trailing tokens
  EVar {} -> False
  EInt {} -> False
  EFloat {} -> False
  EChar {} -> False
  ECharHash {} -> False
  EString {} -> False
  EStringHash {} -> False
  EOverloadedLabel {} -> False
  EQuasiQuote {} -> False
  EList {} -> False
  ETuple {} -> False
  EUnboxedSum {} -> False
  EParen {} -> False
  EGetFieldProjection {} -> False
  -- Everything else: could consume 'by'/'using'
  _ -> True

addArithSeqParens :: ArithSeq -> ArithSeq
addArithSeqParens seqInfo =
  case seqInfo of
    ArithSeqAnn ann inner -> ArithSeqAnn ann (addArithSeqParens inner)
    ArithSeqFrom fromE -> ArithSeqFrom (addExprGuardedParens fromE)
    ArithSeqFromThen fromE thenE -> ArithSeqFromThen (addExprGuardedParens fromE) (addExprGuardedParens thenE)
    ArithSeqFromTo fromE toE -> ArithSeqFromTo (addExprGuardedParens fromE) (addExprParens toE)
    ArithSeqFromThenTo fromE thenE toE -> ArithSeqFromThenTo (addExprGuardedParens fromE) (addExprGuardedParens thenE) (addExprParens toE)

addExprGuardedParens :: Expr -> Expr
addExprGuardedParens expr =
  case peelExprAnn expr of
    EIf {} -> addExprParens expr
    EMultiWayIf {} -> addExprParens expr
    _ -> addExprParensIn CtxGuarded expr

-- | Parenthesize the left operand of a top-level infix expression.
addTopLevelInfixLhsParens :: Int -> Expr -> Expr
addTopLevelInfixLhsParens _prec = addInfixLhsParens

addInfixLhsParens :: Expr -> Expr
addInfixLhsParens expr =
  case expr of
    EAnn ann sub -> EAnn ann (addInfixLhsParens sub)
    EInfix lhs op rhs ->
      EInfix
        (addInfixLhsParens lhs)
        op
        (addExprParensIn (CtxInfixRhs True) rhs)
    _ -> addExprParensIn CtxInfixLhs expr

addInfixRhsParens :: Bool -> Expr -> Expr
addInfixRhsParens protectOpenEnded expr =
  case expr of
    EAnn ann sub -> EAnn ann (addInfixRhsParens protectOpenEnded sub)
    EInfix lhs op rhs ->
      EInfix
        (addInfixLhsParens lhs)
        op
        (addExprParensIn (CtxInfixRhs protectOpenEnded) rhs)
    _ -> addExprParensPrec (exprCtxPrec (CtxInfixRhs protectOpenEnded) expr) expr

absorbsFollowingInfix :: Expr -> Bool
absorbsFollowingInfix = \case
  EAnn _ sub -> absorbsFollowingInfix sub
  EIf {} -> True
  ELambdaPats {} -> True
  ELetDecls {} -> True
  EProc {} -> True
  EApp _ arg | isBlockExpr arg -> absorbsFollowingInfix arg
  EInfix _ _ rhs -> absorbsFollowingInfix rhs
  _ -> False

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Add parentheses to a type at all required positions.
addTypeParens :: Type -> Type
addTypeParens = addTypeParensShared CtxTypeAtom 0

addTypeTopLevelParens :: Type -> Type
addTypeTopLevelParens (TAnn ann sub) = TAnn ann (addTypeTopLevelParens sub)
addTypeTopLevelParens (TKindSig ty kind) = TKindSig (addTypeParensShared CtxTypeAtom 0 ty) (addTypeParensShared CtxTypeAtom 0 kind)
addTypeTopLevelParens (TForall telescope inner) =
  TForall
    (telescope {forallTelescopeBinders = map addTyVarBinderParens (forallTelescopeBinders telescope)})
    (addTypeTopLevelParens inner)
addTypeTopLevelParens (TContext constraints inner) =
  TContext (addContextConstraints constraints) (addContextBodyParens inner)
addTypeTopLevelParens (TParen inner) =
  TParen (addTypeTopLevelParens inner)
addTypeTopLevelParens ty = addTypeParens ty

addTypeIn :: TypeCtx -> Type -> Type
addTypeIn ctx ty =
  wrapTy (needsTypeParens ctx ty) (addTypeParensShared ctx 0 ty)

addTypeParensShared :: TypeCtx -> Int -> Type -> Type
addTypeParensShared ctx prec ty =
  let atom = addTypeParensShared CtxTypeAtom
   in case ty of
        TAnn ann sub -> TAnn ann (addTypeParensShared ctx prec sub)
        TVar {} -> ty
        TCon name promoted -> TCon name promoted
        TBuiltinCon {} -> ty
        TImplicitParam name inner -> TImplicitParam name (addImplicitParamBodyParens inner)
        TTypeLit {} -> ty
        TStar {} -> ty
        TQuasiQuote {} -> ty
        TForall telescope inner ->
          -- forallTypeParser uses contextOrFunTypeParser (not typeParser) for its
          -- body, so a bare nested TForall would fail to parse. Wrap it in TParen.
          wrapTy
            (prec > 0)
            ( TForall
                (telescope {forallTelescopeBinders = map addTyVarBinderParens (forallTelescopeBinders telescope)})
                (addForallBodyParens inner)
            )
        TInfix lhs op promoted rhs ->
          -- Type operators are right-associative in GHC, so the RHS can contain
          -- nested TInfix without parens (a `op1` b `op2` c = a `op1` (b `op2` c)).
          -- The LHS needs parens for nested TInfix to prevent left-association.
          wrapTy (prec > 0) (TInfix (addTypeIn CtxTypeAppFun lhs) op promoted (addTypeIn CtxTypeFunArg rhs))
        TApp f x ->
          wrapTy (prec > 2) (TApp (addTypeIn CtxTypeAppFun f) (addTypeIn CtxTypeAppArg x))
        TTypeApp f x ->
          wrapTy (prec > 2) (TTypeApp (addTypeIn CtxTypeAppFun f) (addTypeIn CtxTypeAppVisibleArg x))
        TFun arrowKind a b ->
          wrapTy (prec > 0) (TFun (addArrowKindParens arrowKind) (addTypeIn CtxTypeFunArg a) (atom 0 b))
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

addArrowKindParens :: ArrowKind -> ArrowKind
addArrowKindParens ArrowUnrestricted = ArrowUnrestricted
addArrowKindParens ArrowLinear = ArrowLinear
addArrowKindParens (ArrowExplicit ty) = ArrowExplicit (addTypeParens ty)

addTyVarBinderParens :: TyVarBinder -> TyVarBinder
addTyVarBinderParens tvb =
  tvb {tyVarBinderKind = fmap addTypeParens (tyVarBinderKind tvb)}

-- | Process the body of a TForall. The forall body is parsed by
-- 'contextOrFunTypeParser' (not 'typeParser'), so a bare nested TForall
-- would fail to parse and must be wrapped in TParen.
addForallBodyParens :: Type -> Type
addForallBodyParens (TAnn ann sub) = TAnn ann (addForallBodyParens sub)
addForallBodyParens ty@(TForall {}) = addTypeParensShared CtxTypeAtom 0 ty
addForallBodyParens ty = addTypeParensShared CtxTypeAtom 0 ty

-- | Process the body of a TImplicitParam. Although 'typeImplicitParamParser'
-- uses 'typeParser' (which handles TContext), the 'startsWithContextType'
-- lookahead will mistake @?x :: C a => T@ for an outer context, consuming
-- the entire implicit param as a constraint item and then failing to find @=>@.
-- Wrap a bare TContext in TParen to prevent this misinterpretation.
--
-- Similarly, a bare TForall whose body contains a TContext produces a @=>@ that
-- is visible to 'startsWithContextType' (the forall binders are balanced braces,
-- but the body's @=>@ appears at top bracket depth after them). Wrapping TForall
-- in TParen hides the inner @=>@ behind a bracket pair.
addImplicitParamBodyParens :: Type -> Type
addImplicitParamBodyParens (TAnn ann sub) = TAnn ann (addImplicitParamBodyParens sub)
addImplicitParamBodyParens ty@(TContext {}) = wrapTy True (addTypeParensShared CtxTypeAtom 0 ty)
addImplicitParamBodyParens ty@(TForall {}) = wrapTy True (addTypeParensShared CtxTypeAtom 0 ty)
addImplicitParamBodyParens ty = addTypeParensShared CtxTypeAtom 0 ty

-- | Process the body to the right of a constraint arrow in a top-level type
-- RHS. A kind signature is already delimited there in cases like
-- @type X = C => T :: K@, but TH splices still need grouping because
-- @type X = C => $x :: K@ is rejected at the @::@ by GHC.
addContextBodyParens :: Type -> Type
addContextBodyParens (TAnn ann sub) = TAnn ann (addContextBodyParens sub)
addContextBodyParens ty =
  case ty of
    TKindSig ty' kind ->
      wrapTy
        (startsWithTypeSplice ty')
        (TKindSig (addTypeParensShared CtxTypeAtom 0 ty') (addTypeParensShared CtxTypeAtom 0 kind))
    _ -> addTypeParensShared CtxTypeAtom 0 ty

startsWithTypeSplice :: Type -> Bool
startsWithTypeSplice (TAnn _ sub) = startsWithTypeSplice sub
startsWithTypeSplice (TParen _) = False
startsWithTypeSplice TSplice {} = True
startsWithTypeSplice (TApp f _) = startsWithTypeSplice f
startsWithTypeSplice (TTypeApp f _) = startsWithTypeSplice f
startsWithTypeSplice _ = False

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
    PTypeBinder binder -> PTypeBinder (addTyVarBinderParens binder)
    PTypeSyntax form ty -> PTypeSyntax form (addTypeParens ty)
    PWildcard {} -> pat
    PLit lit -> PLit lit
    PQuasiQuote {} -> pat
    PTuple tupleFlavor elems -> PTuple tupleFlavor (map addPatternInDelimited elems)
    PUnboxedSum altIdx arity inner -> PUnboxedSum altIdx arity (addPatternInDelimited inner)
    PList elems -> PList (map addPatternInDelimited elems)
    PCon con typeArgs args -> PCon con (map (addTypeIn CtxTypeAtom) typeArgs) (map addPatternAtomParens args)
    PInfix lhs op rhs -> PInfix (addPatternInfixOperandParens lhs) op (addPatternInfixRhsOperandParens rhs)
    PView viewExpr inner ->
      wrapPat True (PView (addViewExprParens viewExpr) (addPatternViewInnerParens inner))
    PAs name inner -> PAs name (addApatParens inner)
    PStrict inner -> PStrict (addApatParens inner)
    PIrrefutable inner -> PIrrefutable (addApatParens inner)
    PNegLit lit -> PNegLit lit
    PParen inner -> PParen (addPatternInDelimited inner)
    PRecord con fields hasWildcard ->
      PRecord con [field {recordFieldValue = addPatternInDelimited (recordFieldValue field)} | field <- fields] hasWildcard
    PTypeSig inner ty -> PTypeSig (addPatternInfixOperandParens inner) (addTypeParens ty)
    PSplice body -> PSplice (addSpliceBodyParens body)

-- | Add parens for a pattern inside a delimited context (tuples, lists, etc.).
-- View patterns don't need extra parens there.
addPatternInDelimited :: Pattern -> Pattern
addPatternInDelimited pat =
  case peelPatternAnn pat of
    PView viewExpr inner -> PView (addViewExprParens viewExpr) (addPatternViewInnerParens inner)
    PAs name inner -> PAs name (addApatParens inner)
    PStrict inner -> PStrict (addApatParens inner)
    PIrrefutable inner -> PIrrefutable (addApatParens inner)
    _ -> addPatternParens pat

-- | Template Haskell pattern quotes accept typed patterns only when they are
-- parenthesized: @[p| (a :: T) |]@ parses, but @[p| a :: T |]@ does not.
addTHPatQuoteParens :: Pattern -> Pattern
addTHPatQuoteParens pat =
  let pat' = addPatternParens pat
   in case peelPatternAnn pat' of
        PTypeSig {} -> wrapPat True pat'
        _ -> pat'

-- | Add parens for a pattern nested inside a view pattern.
-- Nested view patterns do not need extra parens.
addPatternViewInnerParens :: Pattern -> Pattern
addPatternViewInnerParens pat =
  case pat of
    PAnn sp sub -> PAnn sp (addPatternViewInnerParens sub)
    PView viewExpr inner ->
      PView (addViewExprParens viewExpr) (addPatternViewInnerParens inner)
    _ -> addPatternParens pat

addViewExprParens :: Expr -> Expr
addViewExprParens expr =
  if endsWithTypeSig expr || isProcExpr expr || isTypeSyntaxExpr expr
    then wrapExpr True (addExprParens expr)
    else addExprParens expr
  where
    isProcExpr e =
      case peelExprAnn e of
        EProc {} -> True
        _ -> False
    isTypeSyntaxExpr e =
      case peelExprAnn e of
        ETypeSyntax {} -> True
        _ -> False

addPatternAtomParens :: Pattern -> Pattern
addPatternAtomParens pat =
  case pat of
    PAnn ann sub -> PAnn ann (addPatternAtomParens sub)
    PVar {} -> addPatternParens pat
    PTypeBinder {} -> addPatternParens pat
    PTypeSyntax {} -> addPatternParens pat
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
    PRecord {} -> addPatternParens pat
    PCon _ [] [] -> addPatternParens pat
    PInfix {} -> wrapPat True (addPatternParens pat)
    _ -> wrapPat True (addPatternParens pat)

-- | Add parens for a pattern in infix-pattern operand position.
-- In Haskell's grammar, infix patterns have the form @pat10 conop pat10@,
-- where @pat10@ allows constructor application patterns. Non-nullary 'PCon'
-- does not need wrapping here because constructor application binds tighter
-- than infix operators.
addPatternInfixOperandParens :: Pattern -> Pattern
addPatternInfixOperandParens pat =
  case pat of
    PAnn ann sub -> PAnn ann (addPatternInfixOperandParens sub)
    PNegLit _ -> addPatternParens pat
    PCon {} -> addPatternParens pat
    PInfix {} -> addPatternParens pat
    _ -> addPatternAtomParens pat

-- | Add parens for the right operand of a 'PInfix' pattern.
-- The parser builds left-associated chains, so a nested 'PInfix' on the RHS
-- needs explicit grouping.
addPatternInfixRhsOperandParens :: Pattern -> Pattern
addPatternInfixRhsOperandParens pat =
  case pat of
    PAnn ann sub -> PAnn ann (addPatternInfixRhsOperandParens sub)
    -- Negative literal patterns are pat10/lpat forms, so they can appear
    -- directly as either operand of an infix pattern.
    PNegLit {} -> addPatternParens pat
    PCon {} -> addPatternParens pat
    PInfix {} -> wrapPat True (addPatternParens pat)
    _ -> addPatternAtomParens pat

-- | Add parens for a pattern in arrow binder position (lambda/proc).
-- Type-signatured, negated literal, infix, and non-nullary constructor patterns
-- must be parenthesized to avoid ambiguity.
addArrowBndrPatternParens :: Pattern -> Pattern
addArrowBndrPatternParens p@(PTypeSig {}) = wrapPat True (addPatternParens p)
addArrowBndrPatternParens p@(PNegLit {}) = wrapPat True (addPatternParens p)
addArrowBndrPatternParens p@(PInfix {}) = wrapPat True (addPatternParens p)
addArrowBndrPatternParens p@(PCon _ (_ : _) _) = wrapPat True (addPatternParens p)
addArrowBndrPatternParens p@(PCon _ [] (_ : _)) = wrapPat True (addPatternParens p)
addArrowBndrPatternParens pat = addPatternParens pat

-- | Add parens for a pattern in function-head argument position.
addFunctionHeadPatternAtomParens :: Pattern -> Pattern
addFunctionHeadPatternAtomParens pat =
  case pat of
    PAnn ann sub -> PAnn ann (addFunctionHeadPatternAtomParens sub)
    PParen (PTypeSig inner@(PVar {}) ty) ->
      PParen (PTypeSig (addPatternInfixOperandParens inner) (addTypeParens ty))
    PNegLit {} -> wrapPat True (addPatternParens pat)
    PTypeSyntax {} -> wrapPat True (addPatternParens pat)
    PCon _ typeArgs args
      | not (null typeArgs) || not (null args) -> wrapPat True (addPatternParens pat)
    PTypeSig inner@(PVar {}) ty ->
      wrapPat True (PTypeSig (addPatternInfixOperandParens inner) (addTypeParens ty))
    PTypeSig {} -> wrapPat True (addPatternParens pat)
    PAs {} -> addPatternParens pat
    PRecord {} -> addPatternParens pat
    _ -> addPatternAtomParens pat

-- | Add parens for infix function-head operands.
addInfixFunctionHeadPatternAtomParens :: Pattern -> Pattern
addInfixFunctionHeadPatternAtomParens pat =
  case pat of
    PAnn ann sub -> PAnn ann (addInfixFunctionHeadPatternAtomParens sub)
    -- Infix function heads use infix-pattern operand grammar, where negative
    -- literal patterns do not require apat parentheses.
    PNegLit {} -> addPatternParens pat
    PAs {} -> addPatternParens pat
    PTypeSig {} -> wrapPat True (addPatternParens pat)
    PInfix {} -> wrapPat True (addPatternParens pat)
    _ -> addPatternParens pat

-- | Add parens for the inner pattern of @, !, ~.
--
-- A nullary constructor without type arguments (e.g., @EQ@) is an atom and
-- needs no wrapping: @!EQ@, @~EQ@, @y\@EQ@ are all valid Haskell. However,
-- a constructor with type arguments (e.g., @Nothing \@Int@) must be wrapped
-- because @!Nothing \@Int@ is a parse error in GHC — it needs @!(Nothing \@Int)@.
addPatternAtomStrictParens :: Pattern -> Pattern
addPatternAtomStrictParens pat =
  case pat of
    PAnn ann sub -> PAnn ann (addPatternAtomStrictParens sub)
    PNegLit {} -> wrapPat True (addPatternParens pat)
    PTypeSyntax {} -> wrapPat True (addPatternParens pat)
    PCon _ (_ : _) [] -> wrapPat True (addPatternParens pat)
    PAs {} -> addPatternParens pat
    PStrict {} -> wrapPat True (addPatternParens pat)
    PIrrefutable {} -> wrapPat True (addPatternParens pat)
    PRecord {} -> addPatternParens pat
    PSplice {} -> wrapPat True (addPatternParens pat)
    _ -> addPatternAtomParens pat

-- | Add parens for an atomic pattern (@apat@) context.
--
-- As-patterns and the @!@ / @~@ prefixes accept another @apat@ as their body,
-- including symbolic binders such as @(+)\@C@.
addApatParens :: Pattern -> Pattern
addApatParens pat =
  case pat of
    PAnn ann sub -> PAnn ann (addApatParens sub)
    PAs name inner -> PAs name (addApatParens inner)
    _ -> addPatternAtomStrictParens pat

-- ---------------------------------------------------------------------------
-- Arrow commands
-- ---------------------------------------------------------------------------

data CmdCtx
  = CtxCmdTop
  | CtxCmdDoStmt

addCmdParens :: Cmd -> Cmd
addCmdParens = addCmdParensIn CtxCmdTop

addCmdParensIn :: CmdCtx -> Cmd -> Cmd
addCmdParensIn ctx cmd =
  case cmd of
    CmdAnn ann inner -> CmdAnn ann (addCmdParensIn ctx inner)
    CmdArrApp lhs appTy rhs ->
      CmdArrApp (addCmdArrAppLhsParens lhs) appTy (addCmdArrAppRhsParens ctx rhs)
    CmdInfix l op r ->
      CmdInfix (wrapCmdInfixLhs (addCmdParens l)) op (wrapCmdInfixRhs (addCmdParens r))
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
  where
    wrapCmdInfixLhs inner =
      case peelCmdAnn inner of
        CmdArrApp {} -> CmdPar inner
        CmdLet {} -> CmdPar inner
        CmdIf {} -> CmdPar inner
        CmdCase {} -> CmdPar inner
        CmdLam {} -> CmdPar inner
        CmdInfix {} -> CmdPar inner
        _ -> inner

    wrapCmdInfixRhs inner =
      case peelCmdAnn inner of
        CmdArrApp {} -> CmdPar inner
        CmdLet {} -> CmdPar inner
        CmdIf {} -> CmdPar inner
        CmdCase {} -> CmdPar inner
        CmdLam {} -> CmdPar inner
        _ -> inner

addCmdArrAppLhsParens :: Expr -> Expr
addCmdArrAppLhsParens lhs =
  wrapExpr (startsWithBlockExpr lhs || isOpenEnded lhs || endsWithTypeSig lhs || endsWithCmdLayoutTail lhs) (addExprParensPrec 1 lhs)

-- | Arrow tails are parsed outside the expression grammar, so a command lhs that
-- ends in a layout block needs explicit grouping even when the same expression
-- can stay bare before ordinary expression suffixes like @::@ or @\@-type apps.
endsWithCmdLayoutTail :: Expr -> Bool
endsWithCmdLayoutTail = \case
  EAnn _ sub -> endsWithCmdLayoutTail sub
  expr | endsWithCmdLayoutBlock expr -> True
  EInfix _ _ rhs -> endsWithCmdLayoutTail rhs
  ENegate inner -> endsWithCmdLayoutTail inner
  EPragma _ inner -> endsWithCmdLayoutTail inner
  EApp _ arg -> endsWithCmdLayoutBlock arg
  _ -> False

endsWithCmdLayoutBlock :: Expr -> Bool
endsWithCmdLayoutBlock = \case
  EAnn _ sub -> endsWithCmdLayoutBlock sub
  EDo {} -> True
  ELambdaCase {} -> True
  ELambdaCases {} -> True
  EApp _ arg -> endsWithCmdLayoutBlock arg
  _ -> False

addCmdArrAppRhsParens :: CmdCtx -> Expr -> Expr
addCmdArrAppRhsParens ctx rhs =
  wrapExpr (cmdTopNeedsRhsParens && endsWithTypeSig rhs) (addExprParens rhs)
  where
    cmdTopNeedsRhsParens =
      case ctx of
        CtxCmdTop -> True
        CtxCmdDoStmt -> False

addCmdDoStmtParens :: DoStmt Cmd -> DoStmt Cmd
addCmdDoStmtParens stmt =
  case stmt of
    DoAnn ann inner -> DoAnn ann (addCmdDoStmtParens inner)
    DoBind pat cmd' -> DoBind (addPatternParens pat) (addCmdParensIn CtxCmdDoStmt cmd')
    DoLetDecls decls -> DoLetDecls (map addDeclParens decls)
    DoExpr cmd' -> DoExpr (wrapCmd (isLetCmd cmd') (addCmdParensIn CtxCmdDoStmt cmd'))
    DoRecStmt stmts -> DoRecStmt (map addCmdDoStmtParens stmts)
  where
    isLetCmd CmdLet {} = True
    isLetCmd (CmdAnn _ sub) = isLetCmd sub
    isLetCmd _ = False

addCmdCaseAltParens :: CaseAlt Cmd -> CaseAlt Cmd
addCmdCaseAltParens (CaseAlt anns pat rhs) =
  CaseAlt anns (addCaseAltPatternParens pat) (addCmdCaseAltRhsParens rhs)

addCmdCaseAltRhsParens :: Rhs Cmd -> Rhs Cmd
addCmdCaseAltRhsParens rhs =
  case rhs of
    UnguardedRhs sp body whereDecls ->
      UnguardedRhs sp (addCmdParens body) (fmap (map addDeclParens) whereDecls)
    GuardedRhss sp guards whereDecls ->
      GuardedRhss sp (map addCmdGuardedRhsParens guards) (fmap (map addDeclParens) whereDecls)

addCmdGuardedRhsParens :: GuardedRhs Cmd -> GuardedRhs Cmd
addCmdGuardedRhsParens grhs =
  grhs
    { guardedRhsGuards = addGuardQualifiersParens GuardArrow (guardedRhsGuards grhs),
      guardedRhsBody = addCmdParens (guardedRhsBody grhs)
    }
