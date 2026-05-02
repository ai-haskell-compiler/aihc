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
import Data.Bifunctor (bimap)
import Data.Data (Data, gmapT)
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as T
import Data.Typeable (cast)

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

isSymbolicUName :: UnqualifiedName -> Bool
isSymbolicUName name =
  case unqualifiedNameType name of
    NameVarSym -> True
    NameConSym -> True
    _ -> False

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
  EProc {} -> True
  _ -> False

-- | Check if an expression is "greedy" - i.e., it could consume trailing syntax.
isGreedyExpr :: Expr -> Bool
isGreedyExpr = \case
  EAnn _ sub -> isGreedyExpr sub
  ECase {} -> True
  EIf {} -> True
  ELambdaPats {} -> True
  ELambdaCase {} -> True
  ELambdaCases {} -> True
  ELetDecls {} -> True
  EDo {} -> True
  EProc {} -> True
  EApp _ arg | isBlockExpr arg -> isOpenEnded arg
  EApp _ arg -> isGreedyExpr arg
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
  EMultiWayIf {} -> True
  EDo {} -> True
  ELambdaCase {} -> True
  ELambdaCases {} -> True
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
  EIf _ _ no -> endsWithTypeSig no
  _ -> False

-- | Check whether an expression needs parenthesization before a record dot.
-- Non-atomic TH splices (@$(f x)@, @$$([|| x ||])@)
-- need parens to prevent ambiguity:
-- - Non-atomic TH splices need their body delimiters preserved before @.@
-- - TH name quotes whose body is a qualified or MagicHash-suffixed variable
--   are rejected by GHC before record-dot
-- - TH type-name quotes: @''C.field@ is rejected by GHC
needsParensBeforeDot :: Name -> Expr -> Bool
needsParensBeforeDot field = \case
  EAnn _ sub -> needsParensBeforeDot field sub
  ENegate inner -> startsWithPrimitiveLiteral inner
  EVar {} -> False
  ETHNameQuote body -> nameQuoteNeedsParensBeforeDot body
  ETHTypeNameQuote ty -> typeNameQuoteNeedsParensBeforeDot ty
  ETHSplice body -> spliceNeedsParensBeforeDot body
  ETHTypedSplice body -> spliceNeedsParensBeforeDot body
  EInt _ TInteger _repr -> False
  _ -> False

spliceNeedsParensBeforeDot :: Expr -> Bool
spliceNeedsParensBeforeDot = \case
  EAnn _ sub -> spliceNeedsParensBeforeDot sub
  EVar {} -> False
  EParen {} -> False
  _ -> True

nameQuoteNeedsParensBeforeDot :: Expr -> Bool
nameQuoteNeedsParensBeforeDot = \case
  EAnn _ sub -> nameQuoteNeedsParensBeforeDot sub
  EVar {} -> False
  _ -> False

typeNameQuoteNeedsParensBeforeDot :: Type -> Bool
typeNameQuoteNeedsParensBeforeDot = \case
  TAnn _ sub -> typeNameQuoteNeedsParensBeforeDot sub
  TCon name _ | nameText name == "[]" || nameText name == "()" -> False
  TTuple {} -> False
  TUnboxedSum {} -> False
  TList {} -> False
  TParen {} -> False
  _ -> True

-- | Check whether an expression's pretty-printed form starts with '$'.
startsWithDollar :: Expr -> Bool
startsWithDollar (EAnn _ sub) = startsWithDollar sub
startsWithDollar (ETHSplice {}) = True
startsWithDollar (ETHTypedSplice {}) = True
startsWithDollar (ERecordUpd base _) = startsWithDollar base
startsWithDollar (EGetField base _) = startsWithDollar base
startsWithDollar (EApp fn _) = startsWithDollar fn
startsWithDollar (ETypeApp fn _) = startsWithDollar fn
startsWithDollar _ = False

startsWithOverloadedLabel :: Expr -> Bool
startsWithOverloadedLabel = \case
  EOverloadedLabel {} -> True
  EAnn _ sub -> startsWithOverloadedLabel sub
  EApp fn _ -> startsWithOverloadedLabel fn
  EInfix lhs _ _ -> startsWithOverloadedLabel lhs
  ERecordUpd base _ -> startsWithOverloadedLabel base
  EGetField base _ -> startsWithOverloadedLabel base
  ETypeSig inner _ -> startsWithOverloadedLabel inner
  ETypeApp fn _ -> startsWithOverloadedLabel fn
  _ -> False

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
-- literal.  When such an expression appears under 'ENegate', the preceding
-- @-@ merges with the literal at the lexer level, changing the parse.
-- For example, @ENegate (EInt 95 TIntHash "95#")@ pretty-prints as @-95#@
-- which the lexer merges into a single negative literal token, losing the
-- @ENegate@ wrapper.  Wrapping the inner expression in 'EParen' produces
-- @-(95#)@ which round-trips correctly.
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
  | CtxTypeSigBody
  | CtxGuarded

needsExprParens :: ExprCtx -> Expr -> Bool
needsExprParens ctx (EAnn _ sub) = needsExprParens ctx sub
needsExprParens ctx expr =
  case ctx of
    CtxInfixRhs _protectOpenEnded ->
      case expr of
        -- EInfix on the RHS does NOT need parenthesization: the parser
        -- builds right-associated chains, so nested infix on the RHS is
        -- the natural nesting direction.  Printing without parens
        -- produces the same text as the original source.
        ETypeSig {} -> True
        -- ENegate does NOT need parenthesization in infix RHS position.
        -- GHC already rejects `x + - 1` (precedence >= 6) at parse time,
        -- so any ENegate appearing as the RHS of an infix operator in a
        -- successfully parsed module is guaranteed to be valid without
        -- parentheses. Wrapping in EParen would introduce a spurious HsPar
        -- in the GHC AST, causing roundtrip fingerprint mismatches.
        _ -> False
    CtxInfixLhs ->
      case expr of
        -- EInfix on the LHS needs parenthesization: the parser builds
        -- right-associated chains, so a nested EInfix on the LHS can only
        -- appear when the source had explicit parentheses.  Without the
        -- wrapping, the re-parsed tree would right-associate differently.
        EInfix {} -> True
        ETypeSig {} -> True
        -- ENegate needs parenthesization when its inner expression is
        -- open-ended (e.g. ends with a `let` body).  Without parens, the
        -- parser re-reads `- f let {} in x \`op\` y` as
        -- `-(f (let {} in (x \`op\` y)))` because the `let` body greedily
        -- consumes the infix.  For non-greedy inner expressions (plain
        -- variables, literals, etc.) no parens are needed: `-x + 1` is
        -- correctly re-parsed as `(-x) + 1`.
        ENegate inner -> isOpenEnded inner
        _ -> isOpenEnded expr
    CtxAppFun ->
      case expr of
        ENegate {} -> True
        _ -> False
    CtxAppArg ->
      case expr of
        _ | isBlockExpr expr -> False
        -- A pragma as a function argument needs parens: `fn {-# P #-} x` is
        -- ambiguous; `fn ({-# P #-} x)` is unambiguous.
        EPragma {} -> True
        _ -> False
    CtxAppArgNoParens ->
      False
    CtxAppArgGreedy ->
      case expr of
        _ | isBracedExpr expr -> False
        EPragma {} -> True
        _ -> isGreedyExpr expr
    CtxTypeSigBody ->
      case expr of
        ETypeSig {} -> True
        ENegate inner -> isGreedyExpr inner || isOpenEnded inner || endsWithTypeSig inner
        ELambdaPats {} -> True
        ELambdaCases {} -> False
        _ -> isOpenEnded expr
    CtxGuarded -> isGreedyExpr expr

exprCtxPrec :: ExprCtx -> Expr -> Int
exprCtxPrec ctx expr =
  case ctx of
    CtxInfixRhs _
      | isGreedyExpr expr -> 0
      | isBracedExpr expr -> 0
      | otherwise -> 1
    CtxInfixLhs
      | isBracedExpr expr -> 0
      | otherwise -> 1
    CtxAppFun -> 2
    CtxAppArg -> 3
    CtxAppArgNoParens -> 0
    CtxAppArgGreedy
      | isBracedExpr expr -> 0
      | otherwise -> 3
    CtxTypeSigBody
      | isBracedExpr expr -> 0
      | otherwise -> 1
    CtxGuarded -> 0

-- | Parenthesize a left section operand while protecting any rightmost
-- open-ended infix RHS from absorbing the section operator.
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
        EInfix {} -> addSectionLhsParens rhs
        _ | isOpenEnded rhs -> wrapExpr True (addExprParens rhs)
        _ -> addExprParensIn (CtxInfixRhs False) rhs

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
  | CtxTypeAppFun
  | CtxTypePlainAppArg
  | CtxTypeAppArg
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
    CtxTypePlainAppArg ->
      case ty of
        TStar {} -> False
        TSplice {} -> False
        _ -> needsTypeParens CtxTypeAppArg ty
    CtxTypeAppArg ->
      case ty of
        TQuasiQuote {} -> False
        TApp {} -> True
        TTypeApp {} -> True
        TForall {} -> True
        TFun {} -> True
        TContext {} -> True
        TInfix {} -> True
        -- TStar renders as @*@ which merges with the preceding @\@@ in TTypeApp
        -- to form a single operator token @\@*@.
        TStar {} -> True
        -- TSplice renders as @$name@ or @$(expr)@; the @$@ merges with the
        -- preceding @\@@ in TTypeApp to form a single operator token @\@$@.
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

guardExprNeedsParens :: GuardArrow -> Expr -> Bool
guardExprNeedsParens arrow = \case
  EApp _ arg | isBlockExpr arg -> guardExprNeedsParens arrow arg
  expr -> case arrow of
    GuardArrow -> endsWithTypeSig expr
    GuardEquals -> False

-- ---------------------------------------------------------------------------
-- Module
-- ---------------------------------------------------------------------------

addModuleParens :: Module -> Module
addModuleParens modu =
  addModuleGhcParens
    modu
      { moduleDecls = map addDeclParens (moduleDecls modu)
      }

addModuleGhcParens :: Module -> Module
addModuleGhcParens = wrapGhcArrowBndrNegLits

wrapGhcArrowBndrNegLits :: (Data a) => a -> a
wrapGhcArrowBndrNegLits x =
  let x' = gmapT wrapGhcArrowBndrNegLits x
   in case cast x' of
        Just expr -> fromMaybe x' (cast (wrapExprArrowBndrNegLits expr))
        Nothing -> x'

wrapExprArrowBndrNegLits :: Expr -> Expr
wrapExprArrowBndrNegLits expr =
  case expr of
    EMultiWayIf rhss -> EMultiWayIf (map wrapGhcArrowGuardedRhs rhss)
    ECase scrut alts -> ECase scrut (map wrapGhcCaseAlt alts)
    ELambdaCase alts -> ELambdaCase (map wrapGhcCaseAlt alts)
    ELambdaCases alts -> ELambdaCases (map wrapGhcLambdaCaseAlt alts)
    ELambdaPats pats body -> ELambdaPats (map wrapArrowBndrNegLit pats) body
    EProc pat body -> EProc (wrapArrowBndrNegLit pat) body
    ETHPatQuote pat -> ETHPatQuote (wrapGhcTHPatQuote pat)
    ETypeSig inner ty -> ETypeSig inner (wrapGhcExprSigType ty)
    EGetField base field -> EGetField (wrapExpr (ghcNeedsParensBeforeDot base) base) field
    _ -> expr

wrapGhcArrowGuardedRhs :: GuardedRhs Expr -> GuardedRhs Expr
wrapGhcArrowGuardedRhs grhs =
  grhs {guardedRhsGuards = map wrapGhcArrowGuardQualifier (guardedRhsGuards grhs)}

wrapGhcArrowGuardQualifier :: GuardQualifier -> GuardQualifier
wrapGhcArrowGuardQualifier qual =
  case qual of
    GuardAnn ann inner -> GuardAnn ann (wrapGhcArrowGuardQualifier inner)
    GuardExpr expr -> GuardExpr (wrapExpr (endsWithTypeSig expr) expr)
    GuardPat pat expr -> GuardPat pat (wrapExpr (endsWithTypeSig expr) expr)
    GuardLet {} -> qual

ghcNeedsParensBeforeDot :: Expr -> Bool
ghcNeedsParensBeforeDot = \case
  EAnn _ sub -> ghcNeedsParensBeforeDot sub
  EVar name -> (isJust (nameQualifier name) && not (isSymbolicName name)) || T.isSuffixOf "#" (nameText name)
  ETHSplice {} -> True
  ETHTypedSplice {} -> True
  ECharHash {} -> True
  EStringHash {} -> True
  EInt _ nt _ -> nt /= TInteger
  EFloat _ ft _ -> ft /= TFractional
  _ -> False

wrapGhcExprSigType :: Type -> Type
wrapGhcExprSigType ty =
  case ty of
    TAnn ann sub -> TAnn ann (wrapGhcExprSigType sub)
    TKindSig {} -> TParen ty
    _ -> ty

wrapArrowBndrNegLit :: Pattern -> Pattern
wrapArrowBndrNegLit pat =
  case pat of
    PAnn ann sub -> PAnn ann (wrapArrowBndrNegLit sub)
    PNegLit {} -> PParen pat
    PInfix {} -> PParen pat
    PCon _ (_ : _) _ -> PParen pat
    PCon _ [] (_ : _) -> PParen pat
    _ -> pat

wrapGhcTHPatQuote :: Pattern -> Pattern
wrapGhcTHPatQuote pat =
  case pat of
    PAnn ann sub -> PAnn ann (wrapGhcTHPatQuote sub)
    PTypeSig {} -> PParen pat
    _ -> pat

wrapGhcCaseAlt :: CaseAlt Expr -> CaseAlt Expr
wrapGhcCaseAlt alt =
  alt {caseAltPattern = wrapGhcCaseAltPattern (caseAltPattern alt)}

wrapGhcLambdaCaseAlt :: LambdaCaseAlt -> LambdaCaseAlt
wrapGhcLambdaCaseAlt alt =
  alt {lambdaCaseAltPats = map wrapGhcCaseAltPattern (lambdaCaseAltPats alt)}

wrapGhcCaseAltPattern :: Pattern -> Pattern
wrapGhcCaseAltPattern pat =
  case pat of
    PAnn ann sub -> PAnn ann (wrapGhcCaseAltPattern sub)
    PTypeSig {} -> PParen pat
    _ -> pat

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

addTHDeclQuoteParens :: Decl -> Decl
addTHDeclQuoteParens decl =
  case decl of
    DeclAnn ann sub -> DeclAnn ann (addTHDeclQuoteParens sub)
    DeclValue (PatternBind multTag pat rhs) ->
      DeclValue (PatternBind multTag (addTHDeclQuotePatternBindLhsParens pat rhs) (addRhsParens rhs))
    _ -> addDeclParens decl

addValueDeclParens :: ValueDecl -> ValueDecl
addValueDeclParens vdecl =
  case vdecl of
    PatternBind multTag pat rhs -> PatternBind multTag (addPatternBindLhsParens pat rhs) (addRhsParens rhs)
    FunctionBind name matches -> FunctionBind name (map (addMatchParens name) matches)

addPatternBindLhsParens :: Pattern -> Rhs Expr -> Pattern
addPatternBindLhsParens pat rhs =
  case pat of
    PAnn ann sub -> PAnn ann (addPatternBindLhsParens sub rhs)
    -- Bare @name :: ty = rhs@ is valid declaration syntax and is handled by a
    -- dedicated decl parser path. Other typed patterns must stay grouped so the
    -- parser does not reinterpret them as signatures.
    PTypeSig inner@(PVar {}) ty -> PTypeSig (addPatternAtomParens inner) (addTypeParens ty)
    PTypeSig {} -> wrapPat True (addPatternParens pat)
    PSplice {} -> wrapPat True (addPatternParens pat)
    _ -> addPatternParens pat

addTHDeclQuotePatternBindLhsParens :: Pattern -> Rhs Expr -> Pattern
addTHDeclQuotePatternBindLhsParens pat rhs =
  case pat of
    PAnn ann sub -> PAnn ann (addTHDeclQuotePatternBindLhsParens sub rhs)
    PSplice body -> PSplice (addPatternSpliceBodyParens body)
    _ -> addPatternBindLhsParens pat rhs

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
bangTypeNeedsPrefixParens (TCon name promoted)
  | promoted == Promoted = True
  | otherwise = isSymbolicName name
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
infixConOperandNeedsParens (TInfix {}) = True
-- Application head determines what the parser sees first.
infixConOperandNeedsParens (TApp f _) = infixConOperandNeedsParens f
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
addExprParensIn ctx expr =
  let needsWrap = needsExprParens ctx expr
      prec = exprCtxPrec ctx expr
      expr' =
        case expr of
          EAnn ann sub | needsWrap -> EAnn ann (protectWrappedNegation sub)
          ENegate {} | needsWrap -> protectWrappedNegation expr
          _ -> addExprParensPrec prec expr
   in wrapExpr needsWrap expr'

protectWrappedNegation :: Expr -> Expr
protectWrappedNegation expr =
  case expr of
    EAnn ann sub -> EAnn ann (protectWrappedNegation sub)
    ENegate inner -> ENegate (addNegateParens 3 inner)
    _ -> addExprParens expr

protectExplicitParenInner :: Expr -> Expr
protectExplicitParenInner expr =
  case expr of
    EAnn ann sub -> EAnn ann (protectExplicitParenInner sub)
    ENegate {} -> protectWrappedNegation expr
    ETypeSig inner ty -> ETypeSig (protectParenTypeSigBody inner) (addTypeTopLevelParens ty)
    _ -> addExprParens expr

protectParenTypeSigBody :: Expr -> Expr
protectParenTypeSigBody expr =
  case expr of
    EAnn ann sub -> EAnn ann (protectParenTypeSigBody sub)
    ENegate inner
      | startsWithOverloadedLabel inner ->
          protectWrappedNegation expr
    _ -> addExprParensIn CtxTypeSigBody expr

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
       in wrapExpr (prec > 2) (ETypeApp fn' (addTypeIn CtxTypeAppArg ty))
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
    ETHDeclQuote decls -> ETHDeclQuote (map addTHDeclQuoteParens decls)
    ETHTypeQuote ty -> ETHTypeQuote (addTypeTopLevelParens ty)
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
      wrapExpr (prec > 0) (ELambdaPats (map addLambdaBndrPatternParens pats) (addExprParens body))
    ELambdaCase alts ->
      wrapExpr (prec > 0) (ELambdaCase (map addCaseAltParens alts))
    ELambdaCases alts ->
      wrapExpr (prec > 0) (ELambdaCases (map addLambdaCaseAltParens alts))
    EInfix lhs op rhs ->
      wrapExpr
        (prec > 1)
        ( EInfix
            (addExprParensIn CtxInfixLhs lhs)
            op
            (addExprParensIn (CtxInfixRhs (prec == 1)) rhs)
        )
    ENegate inner ->
      wrapExpr (prec > 2) (ENegate (addNegateParens prec inner))
    ESectionL lhs op ->
      -- Sections always require surrounding parens in source syntax.
      -- Wrap in EParen so the AST matches what the parser produces.
      let lhs' =
            if isGreedyExpr lhs || isTypeSig lhs
              then wrapExpr True (addExprParens lhs)
              else addSectionLhsParens lhs
       in EParen (ESectionL lhs' op)
    ESectionR op rhs ->
      -- The RHS operand is parsed as an infix RHS, so low-precedence forms
      -- such as type signatures need their own parentheses:
      -- @(`op` (x :: T))@, not @(`op` x :: T)@.
      EParen (ESectionR op (addExprParensIn (CtxInfixRhs False) rhs))
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
       in EGetField (wrapExpr (needsParensBeforeDot field base') base') field
    EGetFieldProjection {} -> EParen expr
    ETypeSig inner ty ->
      wrapExpr (prec > 1) (ETypeSig (addExprParensIn CtxTypeSigBody inner) (addTypeTopLevelParens ty))
    EParen inner ->
      -- If inner is a section or projection, addExprParens(inner) already produces EParen(section/projection).
      -- Delegating avoids double-wrapping and maintains idempotency.
      case peelExprAnn inner of
        ESectionL {} -> addExprParens inner
        ESectionR {} -> addExprParens inner
        EGetFieldProjection {} -> addExprParens inner
        _ -> EParen (protectExplicitParenInner inner)
    EList values -> EList (map addExprParens values)
    ETuple tupleFlavor values -> ETuple tupleFlavor (map (fmap addExprParens) values)
    EUnboxedSum altIdx arity inner -> EUnboxedSum altIdx arity (addExprParens inner)
    EProc pat body ->
      wrapExpr (prec > 0) (EProc (addProcBndrPatternParens pat) (addCmdParens body))
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
              canStayBare = isBlockExpr a && (isLast || not (isOpenEnded a))
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
    -- Bare variables and operators use the compact splice syntax.
    EVar {} -> body
    -- For everything else (including sections, which addExprParens now wraps
    -- in EParen): wrap in one outer EParen so the body prints as $(expr).
    -- Sections become EParen(EParen(section)) which prints as $((lhs op)).
    EParen inner -> EParen (addExprParens inner)
    _ -> EParen (addExprParens body)

addPatternSpliceBodyParens :: Expr -> Expr
addPatternSpliceBodyParens body =
  case body of
    EAnn ann sub -> EAnn ann (addPatternSpliceBodyParens sub)
    EVar {} -> body
    EParen inner -> EParen (addExprParens inner)
    _ -> EParen (addExprParens body)

addNegateParens :: Int -> Expr -> Expr
addNegateParens prec inner =
  if startsWithDollar inner || startsWithPrimitiveLiteral inner
    then wrapExpr True (addExprParens inner)
    else case peelExprAnn inner of
      -- `-(518# {}).a` and similar forms must keep the field access grouped;
      -- otherwise `-518# {}.a` is lexed as a negative primitive literal record update.
      EGetField base _ | startsWithPrimitiveLiteral base -> wrapExpr True (addExprParens inner)
      -- `- #a` is a negated overloaded label, but `(- #a)` parses as a right
      -- section. If an outer context wraps the negation, keep the label grouped.
      _ | startsWithOverloadedLabel inner -> wrapExpr (prec > 2) (addExprParensPrec 2 inner)
      -- Avoid `--` being lexed as a line comment: wrap nested negation.
      ENegate {} -> wrapExpr True (protectWrappedNegation inner)
      EIf {} -> addExprParens inner
      ECase {} -> addExprParens inner
      EMultiWayIf {} -> addExprParens inner
      EDo {} -> addExprParens inner
      ELetDecls {} -> addExprParens inner
      EProc {} -> addExprParens inner
      -- Application and type-application bind tighter than negation, so `-f x`
      -- does not need parens around `f x`.
      _ -> addExprParensPrec 2 inner

addCaseAltParens :: CaseAlt Expr -> CaseAlt Expr
addCaseAltParens (CaseAlt sp pat rhs) =
  CaseAlt sp (addCaseAltPatternParens pat) (addCaseAltRhsParens rhs)

addCaseAltPatternParens :: Pattern -> Pattern
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
  let parenthesized =
        case expr of
          EAnn ann sub -> EAnn ann (addCompTransformExprParens sub)
          EParen inner ->
            case peelExprAnn inner of
              ESectionL {} -> addExprParens inner
              ESectionR {} -> addExprParens inner
              EGetFieldProjection {} -> addExprParens inner
              _ -> EParen (protectExplicitParenInner inner)
          _ -> addExprParens expr
   in if needsCompTransformParens parenthesized
        then forceWrapExpr parenthesized
        else parenthesized

forceWrapExpr :: Expr -> Expr
forceWrapExpr expr =
  case expr of
    EAnn ann sub -> EAnn ann (forceWrapExpr sub)
    _ -> EParen expr

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
  EParen inner -> compTransformParenContentsNeedOuter inner
  EGetFieldProjection {} -> False
  -- Everything else: could consume 'by'/'using'
  _ -> True

compTransformParenContentsNeedOuter :: Expr -> Bool
compTransformParenContentsNeedOuter = \case
  EAnn _ sub -> compTransformParenContentsNeedOuter sub
  ESectionL {} -> True
  ESectionR {} -> True
  _ -> False

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

addTypeTopLevelParens :: Type -> Type
addTypeTopLevelParens (TAnn ann sub) = TAnn ann (addTypeTopLevelParens sub)
addTypeTopLevelParens (TKindSig ty kind) = TKindSig (addTypeParensShared CtxTypeAtom 0 ty) (addTypeParensShared CtxTypeAtom 0 kind)
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
        TCon name promoted
          | isSymbolicName name, promoted /= Promoted -> TCon name promoted
          | otherwise -> TCon name promoted
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
        tyInfix
          | Just (op, lhs, rhs) <- matchSymbolicInfixTypeApp tyInfix ->
              -- Prefix application of a symbolic type constructor still obeys
              -- normal type-application precedence. In particular, an operand
              -- like @A => B@ must stay parenthesized so it is not re-parsed as
              -- an outer context.
              TApp (TApp op (addTypeIn CtxTypePlainAppArg lhs)) (addTypeIn CtxTypePlainAppArg rhs)
        TInfix lhs op promoted rhs ->
          -- Type operators are right-associative in GHC, so the RHS can contain
          -- nested TInfix without parens (a `op1` b `op2` c = a `op1` (b `op2` c)).
          -- The LHS needs parens for nested TInfix to prevent left-association.
          wrapTy (prec > 0) (TInfix (addTypeIn CtxTypeAppFun lhs) op promoted (addTypeIn CtxTypeFunArg rhs))
        TApp f x ->
          wrapTy (prec > 2) (TApp (addTypeIn CtxTypeAppFun f) (addTypeIn CtxTypePlainAppArg x))
        TTypeApp f x ->
          wrapTy (prec > 2) (TTypeApp (addTypeIn CtxTypeAppFun f) (addTypeIn CtxTypeAppArg x))
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
    PAs name inner -> PAs name (addPatternAtomStrictParens inner)
    PStrict inner -> PStrict (addPatternAtomStrictParens inner)
    PIrrefutable inner -> PIrrefutable (addPatternAtomStrictParens inner)
    PNegLit lit -> PNegLit lit
    PParen inner -> PParen (addPatternInDelimited inner)
    PRecord con fields hasWildcard ->
      PRecord con [field {recordFieldValue = addPatternInDelimited (recordFieldValue field)} | field <- fields] hasWildcard
    PTypeSig inner ty -> PTypeSig (addPatternInfixOperandParens inner) (addTypeParens ty)
    PSplice body -> PSplice (addPatternSpliceBodyParens body)

-- | Add parens for a pattern inside a delimited context (tuples, lists, etc.).
-- View patterns don't need extra parens there.
addPatternInDelimited :: Pattern -> Pattern
addPatternInDelimited pat =
  case peelPatternAnn pat of
    PView viewExpr inner -> PView (addViewExprParens viewExpr) (addPatternViewInnerParens inner)
    PAs name inner -> PAs name (addPatternAtomStrictParens inner)
    PStrict inner -> PStrict (addPatternAtomStrictParens inner)
    PIrrefutable inner -> PIrrefutable (addPatternAtomStrictParens inner)
    _ -> addPatternParens pat

-- | Add required parentheses inside Template Haskell pattern quotes.
addTHPatQuoteParens :: Pattern -> Pattern
addTHPatQuoteParens = addPatternParens

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
  if endsWithTypeSig expr || isProcExpr expr
    then wrapExpr True (addExprParens expr)
    else addExprParens expr
  where
    isProcExpr e =
      case peelExprAnn e of
        EProc {} -> True
        _ -> False

-- | Check if an operator is the cons operator ':'.
isConsOperator :: Name -> Bool
isConsOperator name =
  renderName name == ":"

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
    PAs name _ -> wrapPat (isSymbolicUName name) (addPatternParens pat)
    PSplice {} -> addPatternParens pat
    PRecord {} -> addPatternParens pat
    PCon _ [] [] -> addPatternParens pat
    PInfix _ op _
      | isConsOperator op ->
          -- Cons operator (:) is right-associative, so nested cons patterns
          -- don't need parentheses: x1:x2:xs parses as x1:(x2:xs)
          addPatternParens pat
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
    PCon {} -> addPatternParens pat
    PNegLit {} -> addPatternParens pat
    _ -> addPatternAtomParens pat

-- | Add parens for the right operand of a 'PInfix' pattern.
-- Infix patterns in Haskell are right-recursive (@infixpat -> pat10 conop infixpat@),
-- so a nested 'PInfix' on the RHS is the natural parse and does not need wrapping.
addPatternInfixRhsOperandParens :: Pattern -> Pattern
addPatternInfixRhsOperandParens pat =
  case pat of
    PAnn ann sub -> PAnn ann (addPatternInfixRhsOperandParens sub)
    PCon {} -> addPatternParens pat
    PInfix {} -> addPatternParens pat
    PNegLit {} -> addPatternParens pat
    _ -> addPatternAtomParens pat

-- | Add parens for a pattern in arrow binder position (lambda/proc).
-- Type-signatured, negated literal, infix, and non-nullary constructor patterns
-- must be parenthesized to avoid ambiguity.
addLambdaBndrPatternParens :: Pattern -> Pattern
addLambdaBndrPatternParens p@(PTypeSig {}) = wrapPat True (addPatternParens p)
addLambdaBndrPatternParens p@(PNegLit {}) = wrapPat True (addPatternParens p)
addLambdaBndrPatternParens p@(PInfix {}) = wrapPat True (addPatternParens p)
addLambdaBndrPatternParens p@(PCon _ (_ : _) _) = wrapPat True (addPatternParens p)
addLambdaBndrPatternParens p@(PCon _ [] (_ : _)) = wrapPat True (addPatternParens p)
addLambdaBndrPatternParens pat = addPatternParens pat

addProcBndrPatternParens :: Pattern -> Pattern
addProcBndrPatternParens p@(PTypeSig {}) = wrapPat True (addPatternParens p)
addProcBndrPatternParens pat = addPatternParens pat

-- | Add parens for a pattern in function-head argument position.
addFunctionHeadPatternAtomParens :: Pattern -> Pattern
addFunctionHeadPatternAtomParens pat =
  case pat of
    PAnn ann sub -> PAnn ann (addFunctionHeadPatternAtomParens sub)
    PTypeSyntax {} -> wrapPat True (addPatternParens pat)
    PCon _ typeArgs args
      | not (null typeArgs) || not (null args) -> wrapPat True (addPatternParens pat)
    PAs {} -> addPatternParens pat
    PRecord {} -> addPatternParens pat
    _ -> addPatternAtomParens pat

-- | Add parens for infix function-head operands.
addInfixFunctionHeadPatternAtomParens :: Pattern -> Pattern
addInfixFunctionHeadPatternAtomParens pat =
  case pat of
    PAnn ann sub -> PAnn ann (addInfixFunctionHeadPatternAtomParens sub)
    PAs name _ -> wrapPat (isSymbolicUName name) (addPatternParens pat)
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
    PAs name _ -> wrapPat (isSymbolicUName name) (addPatternParens pat)
    PStrict {} -> wrapPat True (addPatternParens pat)
    PIrrefutable {} -> wrapPat True (addPatternParens pat)
    PRecord {} -> addPatternParens pat
    PSplice {} -> wrapPat True (addPatternParens pat)
    _ -> addPatternAtomParens pat

-- ---------------------------------------------------------------------------
-- Arrow commands
-- ---------------------------------------------------------------------------

addCmdParens :: Cmd -> Cmd
addCmdParens cmd =
  case cmd of
    CmdAnn ann inner -> CmdAnn ann (addCmdParens inner)
    CmdArrApp lhs appTy rhs ->
      CmdArrApp (addCmdArrAppLhsParens lhs) appTy (addExprParens rhs)
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
  wrapExpr (startsWithBlockExpr lhs || isOpenEnded lhs || endsWithTypeSig lhs) (addExprParensPrec 1 lhs)

addCmdDoStmtParens :: DoStmt Cmd -> DoStmt Cmd
addCmdDoStmtParens stmt =
  case stmt of
    DoAnn ann inner -> DoAnn ann (addCmdDoStmtParens inner)
    DoBind pat cmd' -> DoBind (addPatternParens pat) (addCmdParens cmd')
    DoLetDecls decls -> DoLetDecls (map addDeclParens decls)
    DoExpr cmd' -> DoExpr (addCmdParens cmd')
    DoRecStmt stmts -> DoRecStmt (map addCmdDoStmtParens stmts)

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
    { guardedRhsGuards = map (addGuardQualifierParens GuardArrow) (guardedRhsGuards grhs),
      guardedRhsBody = addCmdParens (guardedRhsBody grhs)
    }
