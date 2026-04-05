{-# LANGUAGE OverloadedStrings #-}

-- |
--
-- Module      : Aihc.Parser.Internal.CheckPattern
-- Description : Reclassify Expr trees as Pattern trees
-- License     : MIT
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

import Aihc.Parser.Syntax
import Data.Char (isUpper)
import Data.Text (Text)
import Data.Text qualified as T

-- | Convert an expression tree into a pattern.
-- Returns @Left@ with a diagnostic message if the expression cannot be
-- interpreted as a valid pattern.
checkPattern :: Expr -> Either Text Pattern
checkPattern expr = case expr of
  -- Variables and constructors
  EVar sp name
    | name == "_" -> Right (PWildcard sp)
    | isConLikeName name -> Right (PCon sp name [])
    | otherwise -> Right (PVar sp name)
  -- Parenthesized expression
  EParen sp inner -> PParen sp <$> checkPattern inner
  -- Tuple
  ETuple sp fl elems -> do
    pats <- traverse checkTupleElement elems
    Right (PTuple sp fl pats)
  -- List
  EList sp elems -> PList sp <$> traverse checkPattern elems
  -- Unboxed sum
  EUnboxedSum sp i n e -> PUnboxedSum sp i n <$> checkPattern e
  -- Infix
  EInfix sp l op r -> do
    lPat <- checkPattern l
    rPat <- checkPattern r
    Right (PInfix sp lPat op rPat)
  -- Type signature
  ETypeSig sp e ty -> do
    pat <- checkPattern e
    Right (PTypeSig sp pat ty)
  -- Negation (must be a literal)
  ENegate sp inner -> checkNegLitPattern sp inner
  -- Application: accumulate arguments into PCon
  EApp sp f x -> do
    fPat <- checkPattern f
    xPat <- checkPattern x
    case fPat of
      PCon _csp name args -> Right (PCon sp name (args ++ [xPat]))
      _ -> Left "invalid pattern: application of non-constructor"
  -- Record construction -> record pattern
  ERecordCon sp name fields wc -> do
    patFields <- traverse (\(n, e) -> (n,) <$> checkPattern e) fields
    Right (PRecord sp name patFields wc)
  -- Literals
  EInt sp n repr -> Right (PLit sp (LitInt sp n repr))
  EIntHash sp n repr -> Right (PLit sp (LitIntHash sp n repr))
  EIntBase sp n repr -> Right (PLit sp (LitIntBase sp n repr))
  EIntBaseHash sp n repr -> Right (PLit sp (LitIntBaseHash sp n repr))
  EFloat sp x repr -> Right (PLit sp (LitFloat sp x repr))
  EFloatHash sp x repr -> Right (PLit sp (LitFloatHash sp x repr))
  EChar sp c repr -> Right (PLit sp (LitChar sp c repr))
  ECharHash sp c repr -> Right (PLit sp (LitCharHash sp c repr))
  EString sp s repr -> Right (PLit sp (LitString sp s repr))
  EStringHash sp s repr -> Right (PLit sp (LitStringHash sp s repr))
  -- TH splice
  ETHSplice sp body -> Right (PSplice sp body)
  -- Quasi-quote
  EQuasiQuote sp q b -> Right (PQuasiQuote sp q b)
  -- Expression-only constructs: clear errors
  EIf {} -> Left "unexpected if-then-else in pattern"
  EMultiWayIf {} -> Left "unexpected multi-way if in pattern"
  ECase {} -> Left "unexpected case expression in pattern"
  EDo {} -> Left "unexpected do expression in pattern"
  ELambdaPats {} -> Left "unexpected lambda in pattern"
  ELambdaCase {} -> Left "unexpected lambda-case in pattern"
  ELetDecls {} -> Left "unexpected let expression in pattern"
  EWhereDecls {} -> Left "unexpected where clause in pattern"
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

-- | Convert a list of expressions into patterns.
checkPatterns :: [Expr] -> Either Text [Pattern]
checkPatterns = traverse checkPattern

-- | Check that a tuple element is not a tuple section (Nothing).
checkTupleElement :: Maybe Expr -> Either Text Pattern
checkTupleElement Nothing = Left "unexpected tuple section in pattern"
checkTupleElement (Just e) = checkPattern e

-- | Check that a negated expression is a literal (for PNegLit patterns).
checkNegLitPattern :: SourceSpan -> Expr -> Either Text Pattern
checkNegLitPattern sp inner = case inner of
  EInt _ n repr -> Right (PNegLit sp (LitInt sp n repr))
  EIntHash _ n repr -> Right (PNegLit sp (LitIntHash sp n repr))
  EIntBase _ n repr -> Right (PNegLit sp (LitIntBase sp n repr))
  EIntBaseHash _ n repr -> Right (PNegLit sp (LitIntBaseHash sp n repr))
  EFloat _ x repr -> Right (PNegLit sp (LitFloat sp x repr))
  EFloatHash _ x repr -> Right (PNegLit sp (LitFloatHash sp x repr))
  _ -> Left "negation in pattern requires a numeric literal"

-- | Check whether a name looks like a constructor (starts with uppercase).
isConLikeName :: Text -> Bool
isConLikeName name =
  case T.uncons name of
    Just (c, _) -> isUpper c
    Nothing -> False
