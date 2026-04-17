{-# LANGUAGE OverloadedStrings #-}

-- |
--
-- Module      : Aihc.Parser.Internal.CheckPattern
-- Description : Reclassify Expr trees as Pattern trees
-- License     : Unlicense
--
-- Convert an expression AST into a pattern AST. This implements the
-- \"parse as expression, then reclassify\" strategy described in
-- @docs\/unified-expr-pattern-parsing.md@.
--
-- The core function 'checkPattern' performs a structural recursion over
-- 'Expr', converting each expression node into its pattern counterpart.
-- Expression-only constructs (if, case, do, lambda, etc.) are rejected
-- with a descriptive error message.
module Aihc.Parser.Internal.CheckPattern
  ( checkPattern,
    checkPatterns,
  )
where

import Aihc.Parser.Internal.Common (isConLikeName)
import Aihc.Parser.Syntax
import Data.Maybe (isJust)
import Data.Text (Text)

-- | Convert an expression tree into a pattern.
-- Returns @Left@ with a diagnostic message if the expression cannot be
-- interpreted as a valid pattern.
checkPattern :: Expr -> Either Text Pattern
checkPattern expr = case expr of
  EAnn ann sub -> fmap (PAnn ann) (checkPattern sub)
  -- Variables and constructors
  EVar name
    | nameText name == "_" -> Right PWildcard
    | isConLikeName name -> Right (PCon name [])
    | isJust (nameQualifier name) -> Left "unexpected qualified name in pattern"
    | otherwise -> Right (PVar (mkUnqualifiedName (nameType name) (nameText name)))
  -- Parenthesized expression
  -- When the inner expression is a view-pattern arrow (@expr -> expr@),
  -- produce @PParen (PView f pat)@ to preserve the explicit parens in
  -- the AST, matching the shape produced by the dedicated pattern parser.
  EParen inner
    | Just vp <- asViewPat inner -> Right (PParen vp)
    | otherwise -> PParen <$> checkPattern inner
  -- Tuple
  ETuple fl elems -> do
    pats <- traverse checkTupleElement elems
    Right (PTuple fl pats)
  -- List
  EList elems -> PList <$> traverse checkPattern elems
  -- Unboxed sum
  EUnboxedSum i n e -> PUnboxedSum i n <$> checkPattern e
  -- Infix: only constructor operators (starting with ':') or the view-pattern
  -- arrow @->@ are valid in patterns.
  EInfix l op r
    | renderName op == "->" -> do
        rPat <- checkPattern r
        Right (PView l rPat)
    | isConLikeOp op -> do
        lPat <- checkPattern l
        rPat <- checkPattern r
        Right (PInfix lPat op rPat)
    | otherwise -> Left ("unexpected variable operator '" <> renderName op <> "' in pattern")
  -- Type signature
  ETypeSig e ty -> do
    pat <- checkPattern e
    Right (PTypeSig pat ty)
  -- Negation (must be a literal)
  ENegate inner -> checkNegLitPattern inner
  -- Application: accumulate arguments into PCon
  EApp f x -> do
    fPat <- checkPattern f
    xPat <- checkPattern x
    case peelPatternAnn fPat of
      PCon name args -> Right (PCon name (args ++ [xPat]))
      _ -> Left "invalid pattern: application of non-constructor"
  -- Record construction -> record pattern
  ERecordCon name fields wc -> do
    patFields <- traverse (\(n, e) -> (nameFromText n,) <$> checkPattern e) fields
    Right (PRecord (nameFromText name) patFields wc)
  -- Literals
  EInt n repr -> Right (PLit (LitInt n repr))
  EIntHash n repr -> Right (PLit (LitIntHash n repr))
  EIntBase n repr -> Right (PLit (LitIntBase n repr))
  EIntBaseHash n repr -> Right (PLit (LitIntBaseHash n repr))
  EFloat x repr -> Right (PLit (LitFloat x repr))
  EFloatHash x repr -> Right (PLit (LitFloatHash x repr))
  EChar c repr -> Right (PLit (LitChar c repr))
  ECharHash c repr -> Right (PLit (LitCharHash c repr))
  EString s repr -> Right (PLit (LitString s repr))
  EStringHash s repr -> Right (PLit (LitStringHash s repr))
  EOverloadedLabel {} -> Left "unexpected overloaded label in pattern"
  -- TH splice
  ETHSplice body -> Right (PSplice body)
  -- Quasi-quote
  EQuasiQuote q b -> Right (PQuasiQuote q b)
  -- Expression-only constructs: clear errors
  EIf {} -> Left "unexpected if-then-else in pattern"
  EMultiWayIf {} -> Left "unexpected multi-way if in pattern"
  ECase {} -> Left "unexpected case expression in pattern"
  EDo {} -> Left "unexpected do expression in pattern"
  ELambdaPats {} -> Left "unexpected lambda in pattern"
  ELambdaCase {} -> Left "unexpected lambda-case in pattern"
  ELambdaCases {} -> Left "unexpected lambda-cases in pattern"
  ELetDecls {} -> Left "unexpected let expression in pattern"
  EArithSeq {} -> Left "unexpected arithmetic sequence in pattern"
  EListComp {} -> Left "unexpected list comprehension in pattern"
  EListCompParallel {} -> Left "unexpected parallel list comprehension in pattern"
  ESectionL {} -> Left "unexpected left section in pattern"
  ESectionR {} -> Left "unexpected right section in pattern"
  ERecordUpd {} -> Left "unexpected record update in pattern"
  ETypeApp {} -> Left "unexpected type application in pattern"
  ETHExpQuote {} -> Left "unexpected Template Haskell expression quote in pattern"
  ETHTypedQuote {} -> Left "unexpected Template Haskell typed quote in pattern"
  ETHDeclQuote {} -> Left "unexpected Template Haskell declaration quote in pattern"
  ETHTypeQuote {} -> Left "unexpected Template Haskell type quote in pattern"
  ETHPatQuote {} -> Left "unexpected Template Haskell pattern quote in pattern"
  ETHNameQuote {} -> Left "unexpected Template Haskell name quote in pattern"
  ETHTypeNameQuote {} -> Left "unexpected Template Haskell type name quote in pattern"
  ETHTypedSplice {} -> Left "unexpected typed Template Haskell splice in pattern"
  EProc {} -> Left "unexpected proc expression in pattern"

-- | Convert a list of expressions into patterns.
checkPatterns :: [Expr] -> Either Text [Pattern]
checkPatterns = traverse checkPattern

-- | Check that a tuple element is not a tuple section (Nothing).
checkTupleElement :: Maybe Expr -> Either Text Pattern
checkTupleElement Nothing = Left "unexpected tuple section in pattern"
checkTupleElement (Just e) = checkPattern e

-- | Check that a negated expression is a literal (for PNegLit patterns).
checkNegLitPattern :: Expr -> Either Text Pattern
checkNegLitPattern inner = case inner of
  EInt n repr -> Right (PNegLit (LitInt n repr))
  EIntHash n repr -> Right (PNegLit (LitIntHash n repr))
  EIntBase n repr -> Right (PNegLit (LitIntBase n repr))
  EIntBaseHash n repr -> Right (PNegLit (LitIntBaseHash n repr))
  EFloat x repr -> Right (PNegLit (LitFloat x repr))
  EFloatHash x repr -> Right (PNegLit (LitFloatHash x repr))
  EAnn ann sub -> fmap (PAnn ann) (checkNegLitPattern sub)
  _ -> Left "negation in pattern requires a numeric literal"

-- | Check whether an operator is a constructor operator (starts with ':').
-- Constructor operators and backtick-quoted constructors are valid in patterns;
-- variable operators like @+@ or @*@ are not.
isConLikeOp :: Name -> Bool
isConLikeOp = isConLikeName

-- | Try to interpret an expression as a view pattern @expr -> expr@.
-- Returns 'Just' the corresponding 'PView' when the expression is an
-- 'EInfix' with @->@; 'Nothing' otherwise.  Used by the 'EParen' case of
-- 'checkPattern' to strip the outer parentheses and produce @PView@
-- directly (matching the AST shape that the dedicated pattern parser
-- produces).
asViewPat :: Expr -> Maybe Pattern
asViewPat (EInfix l op r)
  | renderName op == "->" = case checkPattern r of
      Right rPat -> Just (PView l rPat)
      Left _ -> Nothing
asViewPat _ = Nothing
