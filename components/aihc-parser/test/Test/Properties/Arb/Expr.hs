{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.Arb.Expr
  ( genExpr,
    genOperator,
    isValidGeneratedOperator,
    mkIntExpr,
    shrinkExpr,
    span0,
  )
where

import Aihc.Parser.Syntax
import Data.Char (GeneralCategory (..), generalCategory, isAscii, isSpace)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Properties.Arb.Identifiers
  ( genCharValue,
    genConIdent,
    genFieldName,
    genIdent,
    genModuleQualifier,
    genOptionalQualifier,
    genStringValue,
    genTenths,
    showHex,
    shrinkFloat,
    shrinkIdent,
    span0,
  )
import Test.Properties.Arb.Pattern (genPattern, shrinkPattern)
import Test.Properties.Arb.Type (shrinkType)
import Test.QuickCheck

-- | Generate a random expression. Uses QuickCheck's size parameter
-- to control recursion depth.
genExpr :: Gen Expr
genExpr = sized (genExprSizedWith True)

-- | Generate an expression with a given size budget.
-- When size is 0, only generate non-recursive (leaf) expressions.
genExprSized :: Int -> Gen Expr
genExprSized = genExprSizedWith True

-- | Generate an expression, optionally allowing Template Haskell quote forms.
-- Nested TH brackets are rejected by GHC unless separated by splices, so quote
-- bodies disable further quote generation.
genExprSizedWith :: Bool -> Int -> Gen Expr
genExprSizedWith allowTHQuotes n
  | n <= 0 = genExprLeaf
  | otherwise =
      oneof (baseGenerators <> quoteGenerators)
  where
    half = n `div` 2
    third = n `div` 3
    baseGenerators =
      [ -- Leaf expressions
        genExprLeaf,
        -- Recursive expressions (reduce size for subexpressions)
        EApp <$> genExprSizedWith allowTHQuotes half <*> genExprSizedWith allowTHQuotes half,
        EInfix <$> genExprSizedWith allowTHQuotes half <*> genOperatorName <*> genExprSizedWith allowTHQuotes half,
        ENegate <$> genExprSizedWith allowTHQuotes (n - 1),
        ESectionL <$> genExprSizedWith allowTHQuotes (n - 1) <*> genOperatorName,
        ESectionR <$> genOperatorName <*> genExprSizedWith allowTHQuotes (n - 1),
        EIf <$> genExprSizedWith allowTHQuotes third <*> genExprSizedWith allowTHQuotes third <*> genExprSizedWith allowTHQuotes third,
        EMultiWayIf <$> genGuardedRhsListWith allowTHQuotes (n - 1),
        ECase <$> genExprSizedWith allowTHQuotes half <*> genCaseAltsWith allowTHQuotes half,
        ELambdaPats <$> genPatterns half <*> genExprSizedWith allowTHQuotes half,
        ELambdaCase <$> genCaseAltsWith allowTHQuotes (n - 1),
        ELambdaCases <$> genLambdaCaseAltsWith allowTHQuotes (n - 1),
        ELetDecls <$> genValueDeclsWith allowTHQuotes half <*> genExprSizedWith allowTHQuotes half,
        EDo <$> genDoStmtsWith allowTHQuotes (n - 1) <*> arbitrary,
        EListComp <$> genExprSizedWith allowTHQuotes half <*> genCompStmtsWith allowTHQuotes half,
        EListCompParallel <$> genExprSizedWith allowTHQuotes half <*> genParallelCompStmtsWith allowTHQuotes half,
        EList <$> genListElemsWith allowTHQuotes (n - 1),
        ETuple Boxed . map Just <$> genTupleElemsWith allowTHQuotes (n - 1),
        ETuple Unboxed . map Just <$> genUnboxedTupleElemsWith allowTHQuotes (n - 1),
        ETuple Boxed <$> genTupleSectionElemsWith allowTHQuotes (n - 1),
        ETuple Unboxed <$> genTupleSectionElemsWith allowTHQuotes (n - 1),
        genUnboxedSumExprWith allowTHQuotes (n - 1),
        EArithSeq <$> genArithSeqWith allowTHQuotes (n - 1),
        (\name fields -> ERecordCon name fields False) <$> genConName <*> genRecordFieldsWith allowTHQuotes (n - 1),
        ERecordUpd <$> genExprSizedWith allowTHQuotes half <*> genRecordFieldsWith allowTHQuotes half,
        ETypeSig <$> genExprSizedWith allowTHQuotes half <*> genTypeWith allowTHQuotes half,
        ETypeApp . canonicalTypeAppExpr <$> genExprSizedWith allowTHQuotes half <*> genTypeWith allowTHQuotes half,
        EParen <$> genExprSizedWith allowTHQuotes (n - 1),
        -- Template Haskell splices are valid inside quote bodies.
        ETHSplice <$> genSpliceBody (n - 1),
        ETHTypedSplice <$> genTypedSpliceBody (n - 1)
      ]
    quoteGenerators
      | allowTHQuotes =
          [ ETHExpQuote <$> genExprSizedWith False (n - 1),
            ETHTypedQuote <$> genExprSizedWith False (n - 1),
            ETHDeclQuote <$> genValueDeclsWith False (n - 1),
            ETHPatQuote <$> genPattern (n - 1),
            ETHTypeQuote <$> genTypeWith False (n - 1),
            ETHNameQuote <$> genNameQuoteExpr,
            ETHTypeNameQuote <$> genTypeNameQuoteType
          ]
      | otherwise =
          []

-- | Generate a leaf (non-recursive) expression.
genExprLeaf :: Gen Expr
genExprLeaf =
  oneof
    [ EVar <$> genVarName,
      genOverloadedLabel,
      mkIntExpr <$> chooseInteger (0, 999),
      mkHexExpr <$> chooseInteger (0, 255),
      mkFloatExpr <$> genTenths,
      mkCharExpr <$> genCharValue,
      mkStringExpr <$> genStringValue,
      -- MagicHash literals
      (\v -> EInt v TIntHash (T.pack (show v) <> "#")) <$> chooseInteger (0, 999),
      (\v -> ECharHash v (T.pack (show v) <> "#")) <$> genCharValue,
      (\v -> EStringHash v (T.pack (show (T.unpack v)) <> "#")) <$> genStringValue,
      EQuasiQuote <$> genQuasiQuoteName <*> genStringValue,
      pure (EList []),
      pure (ETuple Boxed []),
      pure (ETuple Unboxed []),
      (\n -> ETuple Boxed (replicate n Nothing)) <$> chooseInt (2, 5),
      (\n -> ETuple Unboxed (replicate n Nothing)) <$> chooseInt (2, 5)
    ]

genOverloadedLabel :: Gen Expr
genOverloadedLabel = do
  labelName <- suchThat genIdent (not . T.isSuffixOf "#")
  pure (EOverloadedLabel labelName ("#" <> labelName))

-- | Generate a quasi-quote name, excluding TH bracket names (e, d, p, t) which
-- would collide with Template Haskell bracket syntax ([e|...|], [d|...|], etc.).
genQuasiQuoteName :: Gen Text
genQuasiQuoteName = suchThat genIdent (\name -> name `notElem` ["e", "d", "p", "t"] && not (T.isSuffixOf "#" name))

-- | Generate the body of a TH splice: either a bare variable or a parenthesized expression.
-- Bare variables produce $name syntax; parenthesized produce $(expr) syntax.
genSpliceBody :: Int -> Gen Expr
genSpliceBody n =
  oneof
    [ EVar <$> genVarName,
      EParen <$> genExprSized (max 0 (n - 1))
    ]

-- | Generate the body of a TH typed splice: always parenthesized.
-- Typed splices require parentheses: $$(expr) is valid, $$expr is invalid.
genTypedSpliceBody :: Int -> Gen Expr
genTypedSpliceBody n =
  EParen <$> genExprSized (max 0 (n - 1))

-- | Generate a TH value name quote target.
-- Produces unqualified identifiers plus qualified identifiers and operators
-- such as @M.v@ and @M.+@.
genNameQuoteExpr :: Gen Expr
genNameQuoteExpr =
  oneof
    [ EVar . qualifyName Nothing . mkUnqualifiedName NameVarId <$> genNameQuoteIdent,
      do
        qual <- genModuleQualifier
        EVar . mkName (Just qual) NameVarId <$> genNameQuoteIdent,
      do
        qual <- genModuleQualifier
        op <- genOperator `suchThat` notDotLikeForQualifiedOp
        pure (EVar (mkName (Just qual) NameVarSym op)),
      pure (EList []),
      pure (ETuple Boxed []),
      pure (ETuple Unboxed []),
      (\n -> ETuple Boxed (replicate n Nothing))
        <$> chooseInt (2, 5),
      (\n -> ETuple Unboxed (replicate n Nothing))
        <$> chooseInt (2, 5)
    ]
  where
    -- \| @renderName (mkName (Just q) NameVarSym op) == q <> "." <> op@ must not
    -- insert an extra dot before @op@ (e.g. @op == ".+"@ would yield @q..+@).
    notDotLikeForQualifiedOp :: Text -> Bool
    notDotLikeForQualifiedOp op =
      not (T.null op) && not (".." `T.isInfixOf` op) && not ("." `T.isPrefixOf` op)

genTypeNameQuoteType :: Gen Type
genTypeNameQuoteType =
  oneof
    [ TCon <$> genTypeNameQuote <*> pure Unpromoted,
      pure (TCon (qualifyName Nothing (mkUnqualifiedName NameConId "[]")) Unpromoted),
      pure (TTuple Boxed Unpromoted []),
      pure (TTuple Unboxed Unpromoted [])
    ]

-- | Generate an identifier safe for TH name quotes ('name).
-- Avoids identifiers where the second character is a single quote,
-- as 'x'y would be parsed as char literal 'x' followed by identifier y.
genNameQuoteIdent :: Gen Text
genNameQuoteIdent = do
  ident <- genIdent
  -- If length >= 2 and second char is a quote, regenerate
  if T.length ident >= 2 && T.index ident 1 == '\''
    then genNameQuoteIdent
    else pure ident

-- | Generate an operator symbol
genOperator :: Gen Text
genOperator =
  oneof
    [ elements ["+", "-", "*", "/", "<", ">", "<=", ">=", "==", "/=", "&&", "||", "++", ">>", ">>=", "."],
      genCustomOperator
    ]

genOperatorName :: Gen Name
genOperatorName = do
  qual <- genOptionalQualifier
  op <- mkUnqualifiedName NameVarSym <$> genOperator
  pure (qualifyName qual op)

-- | Generate a custom operator
-- Only uses valid operator characters (matching isOperatorToken in Pretty.hs)
genCustomOperator :: Gen Text
genCustomOperator = do
  len <- chooseInt (1, 3)
  chars <- vectorOf len genOperatorChar
  let candidate = T.pack chars
  -- Avoid reserved operators and symbols that lex as comments.
  if isValidGeneratedOperator candidate
    then pure candidate
    else genCustomOperator

genOperatorChar :: Gen Char
genOperatorChar =
  frequency
    [ (5, elements asciiOperatorChars),
      (3, elements curatedUnicodeOperatorChars),
      (2, elements unicodeOperatorChars)
    ]

asciiOperatorChars :: [Char]
asciiOperatorChars = "!$%&*+./<=>?\\^|-~"

curatedUnicodeOperatorChars :: [Char]
curatedUnicodeOperatorChars =
  [ '⁂',
    '‼',
    '∘',
    '⊕',
    '⋆',
    '¤',
    '₿',
    '¨',
    '¯',
    '©'
  ]

unicodeOperatorChars :: [Char]
unicodeOperatorChars =
  extraUnicodeOperatorChars <> concatMap (filter isAllowedUnicodeOperatorChar . expandRange) unicodeOperatorRanges

extraUnicodeOperatorChars :: [Char]
extraUnicodeOperatorChars =
  ['⁂', '‼']

unicodeOperatorRanges :: [(Char, Char)]
unicodeOperatorRanges =
  [ ('\x00A2', '\x00A9'),
    ('\x2000', '\x206F'),
    ('\x02C2', '\x02DF'),
    ('\x20A0', '\x20CF'),
    ('\x2100', '\x214F'),
    ('\x2190', '\x21FF'),
    ('\x2200', '\x22FF'),
    ('\x2300', '\x23FF'),
    ('\x2460', '\x24FF'),
    ('\x2500', '\x257F'),
    ('\x2580', '\x259F'),
    ('\x25A0', '\x25FF'),
    ('\x2600', '\x26FF'),
    ('\x27C0', '\x27EF'),
    ('\x27F0', '\x27FF'),
    ('\x2900', '\x297F'),
    ('\x2980', '\x29FF'),
    ('\x2A00', '\x2AFF'),
    ('\x2B00', '\x2BFF')
  ]

expandRange :: (Char, Char) -> [Char]
expandRange (lo, hi) = [lo .. hi]

isAllowedUnicodeOperatorChar :: Char -> Bool
isAllowedUnicodeOperatorChar c =
  isTargetUnicodeOperatorCategory c
    && c `notElem` bannedUnicodeOperatorChars

isTargetUnicodeOperatorCategory :: Char -> Bool
isTargetUnicodeOperatorCategory c =
  case generalCategory c of
    MathSymbol -> True
    CurrencySymbol -> True
    ModifierSymbol -> True
    OtherSymbol -> True
    OtherPunctuation -> not (isAscii c)
    _ -> False

bannedUnicodeOperatorChars :: [Char]
bannedUnicodeOperatorChars =
  [ '→',
    '←',
    '⇒',
    '∷',
    '∀',
    '⤙',
    '⤚',
    '⤛',
    '⤜',
    '⦇',
    '⦈',
    '⟦',
    '⟧',
    '⊸',
    '★'
  ]

isValidGeneratedOperator :: Text -> Bool
isValidGeneratedOperator candidate =
  let reserved =
        candidate
          `elem` ["..", "::", "=", "\\", "|", "<-", "->", "~", "=>", "--", "-<", ">-", "-<<", ">>-"]
      dashOnly = T.length candidate >= 2 && T.all (== '-') candidate
      hasBacktick = T.any (== '`') candidate
      hasCanonicalizedUnicode = T.any (`elem` bannedUnicodeOperatorChars) candidate
   in not reserved && not dashOnly && not hasBacktick && not hasCanonicalizedUnicode

-- | Generate a data constructor name
genConName :: Gen Text
genConName = genConIdent

-- | Generate a type name for TH type quotes (''Name).
-- Can produce either a constructor name (e.g., ''Maybe) or an operator name (e.g., ''(:>)).
genTypeNameQuote :: Gen Name
genTypeNameQuote =
  oneof
    [ qualifyName Nothing . mkUnqualifiedName NameConId <$> genConIdent,
      -- Generate operator name for type quotes (use NameVarSym to match lexer behavior)
      qualifyName Nothing . mkUnqualifiedName NameVarSym <$> suchThat genOperator (/= "*")
    ]

genVarName :: Gen Name
genVarName = qualifyName <$> genOptionalQualifier <*> (mkUnqualifiedName NameVarId <$> genIdent)

genConAstName :: Gen Name
genConAstName = qualifyName <$> genOptionalQualifier <*> (mkUnqualifiedName NameConId <$> genConIdent)

-- | Generate simple patterns for lambdas
genPatterns :: Int -> Gen [Pattern]
genPatterns n = do
  count <- chooseInt (1, 3)
  vectorOf count (genPattern n)

genCaseAltsWith :: Bool -> Int -> Gen [CaseAlt]
genCaseAltsWith allowTHQuotes n = do
  count <- chooseInt (0, 3)
  vectorOf count (genCaseAltWith allowTHQuotes n)

genCaseAltWith :: Bool -> Int -> Gen CaseAlt
genCaseAltWith allowTHQuotes n = do
  pat <- genPattern half
  rhs <- genRhsWith allowTHQuotes half
  pure $
    CaseAlt
      { caseAltAnns = [],
        caseAltPattern = pat,
        caseAltRhs = rhs
      }
  where
    half = n `div` 2

genLambdaCaseAltsWith :: Bool -> Int -> Gen [LambdaCaseAlt]
genLambdaCaseAltsWith allowTHQuotes n = do
  count <- chooseInt (0, 3)
  vectorOf count (genLambdaCaseAltWith allowTHQuotes n)

genLambdaCaseAltWith :: Bool -> Int -> Gen LambdaCaseAlt
genLambdaCaseAltWith allowTHQuotes n = do
  pats <- genPatterns half
  rhs <- genRhsWith allowTHQuotes half
  pure $
    LambdaCaseAlt
      { lambdaCaseAltAnns = [],
        lambdaCaseAltPats = pats,
        lambdaCaseAltRhs = rhs
      }
  where
    half = n `div` 2

genRhsWith :: Bool -> Int -> Gen Rhs
genRhsWith allowTHQuotes n =
  oneof
    [ (\e -> UnguardedRhs [] e Nothing) <$> genBindingExprWith allowTHQuotes n,
      (\gs -> GuardedRhss [] gs Nothing) <$> genGuardedRhsListWith allowTHQuotes n
    ]

genGuardedRhsListWith :: Bool -> Int -> Gen [GuardedRhs]
genGuardedRhsListWith allowTHQuotes n = do
  count <- chooseInt (1, 3)
  vectorOf count (genGuardedRhsWith allowTHQuotes (n `div` count))

genGuardedRhsWith :: Bool -> Int -> Gen GuardedRhs
genGuardedRhsWith allowTHQuotes n = do
  guardCount <- chooseInt (1, 2)
  guards <- vectorOf guardCount (genGuardQualifierWith allowTHQuotes (half `div` guardCount))
  body <- genBindingExprWith allowTHQuotes half
  pure $
    GuardedRhs
      { guardedRhsAnns = [],
        guardedRhsGuards = guards,
        guardedRhsBody = body
      }
  where
    half = n `div` 2

-- | Generate a guard qualifier.
genGuardQualifierWith :: Bool -> Int -> Gen GuardQualifier
genGuardQualifierWith allowTHQuotes n =
  oneof
    [ -- Boolean guard: | expr = ...
      GuardExpr <$> genExprSizedWith allowTHQuotes n,
      -- Pattern guard: | pat <- expr = ...
      -- The guarded-qualifier parser now accepts the full pattern generator,
      -- which includes parenthesized view patterns such as `(view -> pat)`.
      GuardPat <$> genPattern half <*> genExprSizedWith allowTHQuotes half,
      -- Let guard: | let decls = ...
      GuardLet <$> genValueDeclsWith allowTHQuotes n
    ]
  where
    half = n `div` 2

-- | Generate value declarations for let/where.
-- Produces a mix of simple pattern bindings (@x = expr@) and function bindings
-- (@f pat ... = expr@ or @f pat ... | guard = expr@), mirroring the parser
-- which creates each equation as a separate 'FunctionBind' with a single
-- 'Match'.
genValueDeclsWith :: Bool -> Int -> Gen [Decl]
genValueDeclsWith allowTHQuotes n = do
  count <- chooseInt (0, 3)
  vectorOf count (genValueDeclWith allowTHQuotes (n `div` max 1 count))

-- | Generate a single value declaration: either a simple pattern binding or a
-- function binding with argument patterns and optional guards.
genValueDeclWith :: Bool -> Int -> Gen Decl
genValueDeclWith allowTHQuotes n =
  oneof
    [ genPatternBindDecl allowTHQuotes n,
      genFunctionBindDecl allowTHQuotes n
    ]

-- | Generate a pattern binding: @pat = expr@ or @pat | guard = expr@.
-- The pattern can be any pattern (bang, as, irrefutable, etc.) and the RHS
-- can be guarded, matching what GHC accepts.
genPatternBindDecl :: Bool -> Int -> Gen Decl
genPatternBindDecl allowTHQuotes n = do
  pat <- genPattern half
  rhs <- genRhsWith allowTHQuotes half
  pure $
    DeclValue
      (PatternBind pat rhs)
  where
    half = n `div` 2

-- | Generate a function binding: @f pat ... = expr@ or @f pat ... | guard = expr@.
-- Produces a single 'Match', consistent with the parser which creates one
-- 'FunctionBind' per equation.
genFunctionBindDecl :: Bool -> Int -> Gen Decl
genFunctionBindDecl allowTHQuotes n = do
  name <- mkUnqualifiedName NameVarId <$> genIdent
  patCount <- chooseInt (1, 3)
  let perItem = n `div` max 1 (patCount + 1)
  pats <- vectorOf patCount (genPattern perItem)
  rhs <- genRhsWith allowTHQuotes perItem
  pure $
    DeclValue
      ( FunctionBind
          name
          [ Match
              { matchAnns = [],
                matchHeadForm = MatchHeadPrefix,
                matchPats = pats,
                matchRhs = rhs
              }
          ]
      )

genBindingExprWith :: Bool -> Int -> Gen Expr
genBindingExprWith = genExprSizedWith

genDoStmtsWith :: Bool -> Int -> Gen [DoStmt Expr]
genDoStmtsWith allowTHQuotes n = do
  count <- chooseInt (1, 3)
  let perStmt = n `div` count
  stmts <- vectorOf (count - 1) (genDoStmtWith allowTHQuotes perStmt)
  -- Last statement must be DoExpr
  lastExpr <- genExprSizedWith allowTHQuotes perStmt
  pure (stmts <> [DoExpr lastExpr])

genDoStmtWith :: Bool -> Int -> Gen (DoStmt Expr)
genDoStmtWith allowTHQuotes n =
  oneof
    [ DoBind <$> genPattern half <*> genExprSizedWith allowTHQuotes half,
      DoLetDecls <$> genValueDeclsWith allowTHQuotes (n - 1),
      DoExpr <$> genExprSizedWith allowTHQuotes (n - 1),
      DoRecStmt <$> genRecDoStmtsWith allowTHQuotes (n - 1)
    ]
  where
    half = n `div` 2

-- | Generate statements for a @rec@ block inside @mdo@/@do@.
-- At least one statement is required.
genRecDoStmtsWith :: Bool -> Int -> Gen [DoStmt Expr]
genRecDoStmtsWith allowTHQuotes n = do
  count <- chooseInt (1, 3)
  let perStmt = n `div` count
  vectorOf count $
    oneof
      [ DoBind <$> genPattern (perStmt `div` 2) <*> genExprSizedWith allowTHQuotes (perStmt `div` 2),
        DoLetDecls <$> genValueDeclsWith allowTHQuotes perStmt,
        DoExpr <$> genExprSizedWith allowTHQuotes perStmt
      ]

genCompStmtsWith :: Bool -> Int -> Gen [CompStmt]
genCompStmtsWith allowTHQuotes n = do
  count <- chooseInt (1, 3)
  vectorOf count (genCompStmtWith allowTHQuotes (n `div` count))

genCompStmtWith :: Bool -> Int -> Gen CompStmt
genCompStmtWith allowTHQuotes n =
  oneof
    [ CompGen <$> genPattern half <*> genExprSizedWith allowTHQuotes half,
      CompGuard <$> genExprSizedWith allowTHQuotes (n - 1),
      CompLetDecls <$> genValueDeclsWith allowTHQuotes (n - 1)
    ]
  where
    half = n `div` 2

genParallelCompStmtsWith :: Bool -> Int -> Gen [[CompStmt]]
genParallelCompStmtsWith allowTHQuotes n = do
  count <- chooseInt (2, 3)
  vectorOf count (genCompStmtsWith allowTHQuotes (n `div` count))

genListElemsWith :: Bool -> Int -> Gen [Expr]
genListElemsWith allowTHQuotes n = do
  count <- chooseInt (0, 4)
  vectorOf count (genExprSizedWith allowTHQuotes (n `div` max 1 count))

-- | Generate tuple elements
genTupleElemsWith :: Bool -> Int -> Gen [Expr]
genTupleElemsWith allowTHQuotes n =
  oneof
    [ pure [],
      do
        count <- chooseInt (2, 4)
        vectorOf count (genExprSizedWith allowTHQuotes (n `div` count))
    ]

-- | Generate elements for an unboxed tuple (0-4 elements).
-- Unlike boxed tuples, unboxed tuples with 0 elements are valid Haskell.
genUnboxedTupleElemsWith :: Bool -> Int -> Gen [Expr]
genUnboxedTupleElemsWith allowTHQuotes n = do
  count <- chooseInt (0, 4)
  vectorOf count (genExprSizedWith allowTHQuotes (n `div` max 1 count))

genUnboxedSumExprWith :: Bool -> Int -> Gen Expr
genUnboxedSumExprWith allowTHQuotes n = do
  arity <- chooseInt (2, 4)
  altIdx <- chooseInt (0, arity - 1)
  inner <- genExprSizedWith allowTHQuotes n
  pure (EUnboxedSum altIdx arity inner)

genTupleSectionElemsWith :: Bool -> Int -> Gen [Maybe Expr]
genTupleSectionElemsWith allowTHQuotes n = do
  count <- chooseInt (2, 4)
  elems <- vectorOf count (genMaybeExprWith allowTHQuotes (n `div` count))
  -- Ensure at least one Nothing (otherwise it's just a tuple)
  if Nothing `notElem` elems
    then do
      idx <- chooseInt (0, count - 1)
      pure (take idx elems <> [Nothing] <> drop (idx + 1) elems)
    else pure elems

genMaybeExprWith :: Bool -> Int -> Gen (Maybe Expr)
genMaybeExprWith allowTHQuotes n =
  oneof
    [ Just <$> genExprSizedWith allowTHQuotes n,
      pure Nothing
    ]

genArithSeqWith :: Bool -> Int -> Gen ArithSeq
genArithSeqWith allowTHQuotes n =
  oneof
    [ ArithSeqFrom <$> genExprSizedWith allowTHQuotes n,
      ArithSeqFromThen <$> genExprSizedWith allowTHQuotes half <*> genExprSizedWith allowTHQuotes half,
      ArithSeqFromTo <$> genExprSizedWith allowTHQuotes half <*> genExprSizedWith allowTHQuotes half,
      ArithSeqFromThenTo <$> genExprSizedWith allowTHQuotes third <*> genExprSizedWith allowTHQuotes third <*> genExprSizedWith allowTHQuotes third
    ]
  where
    half = n `div` 2
    third = n `div` 3

genRecordFieldsWith :: Bool -> Int -> Gen [(Text, Expr)]
genRecordFieldsWith allowTHQuotes n = do
  count <- chooseInt (0, 3)
  names <- vectorOf count genFieldName
  exprs <- vectorOf count (genExprSizedWith allowTHQuotes (n `div` max 1 count))
  pure (zip names exprs)

-- | Generate a type (simple version for use inside expressions).
genTypeWith :: Bool -> Int -> Gen Type
genTypeWith allowTHQuotes n
  | n <= 0 = genTypeLeaf
  | otherwise =
      oneof
        [ genTypeLeaf,
          TApp <$> genTypeWith allowTHQuotes half <*> genTypeWith allowTHQuotes half,
          TFun <$> genTypeWith allowTHQuotes half <*> genTypeWith allowTHQuotes half,
          TList Unpromoted <$> genTypeListElemsWith allowTHQuotes (n - 1),
          TTuple Boxed Unpromoted <$> genTypeTupleElemsWith allowTHQuotes (n - 1),
          TParen <$> genTypeWith allowTHQuotes (n - 1)
        ]
  where
    half = n `div` 2

genTypeLeaf :: Gen Type
genTypeLeaf =
  oneof
    [ TVar <$> genTypeVarName,
      (`TCon` Unpromoted) <$> genConAstName
    ]

genTypeTupleElemsWith :: Bool -> Int -> Gen [Type]
genTypeTupleElemsWith allowTHQuotes n =
  oneof
    [ pure [],
      do
        count <- chooseInt (2, 3)
        vectorOf count (genTypeWith allowTHQuotes (n `div` count))
    ]

genTypeListElemsWith :: Bool -> Int -> Gen [Type]
genTypeListElemsWith allowTHQuotes n = do
  count <- chooseInt (1, 4)
  vectorOf count (genTypeWith allowTHQuotes (n `div` count))

genTypeVarName :: Gen UnqualifiedName
genTypeVarName = mkUnqualifiedName NameVarId <$> genIdent

-- | Wrap an expression in parens if it's not suitable as the LHS of a type
-- application (@expr \@Type@). Type application has the same precedence as
-- function application, so lambda, let, if, case, do, and other open-ended
-- expressions need parens. Even EApp needs parens if its argument is
-- open-ended (e.g., @f let x = 1 in x \@T@ is ambiguous).
canonicalTypeAppExpr :: Expr -> Expr
canonicalTypeAppExpr e = case e of
  EVar {} -> e
  EParen {} -> e
  EList {} -> e
  ETuple {} -> e
  EUnboxedSum {} -> e
  ERecordCon {} -> e
  EInt {} -> e
  EFloat {} -> e
  EChar {} -> e
  ECharHash {} -> e
  EString {} -> e
  EStringHash {} -> e
  EQuasiQuote {} -> e
  EOverloadedLabel {} -> e
  ETHExpQuote {} -> e
  ETHTypedQuote {} -> e
  ETHDeclQuote {} -> e
  ETHTypeQuote {} -> e
  ETHPatQuote {} -> e
  ETHNameQuote {} -> e
  ETHTypeNameQuote {} -> e
  ETHSplice {} -> e
  ETHTypedSplice {} -> e
  _ -> EParen e

-- | Literal expression constructors
mkHexExpr :: Integer -> Expr
mkHexExpr value = EInt value TInteger ("0x" <> T.pack (showHex value))

mkFloatExpr :: Double -> Expr
mkFloatExpr value = EFloat value TFractional (T.pack (show value))

mkCharExpr :: Char -> Expr
mkCharExpr value = EChar value (T.pack (show value))

mkStringExpr :: Text -> Expr
mkStringExpr value = EString value (T.pack (show (T.unpack value)))

-- | Create an integer expression with canonical representation.
mkIntExpr :: Integer -> Expr
mkIntExpr value = EInt value TInteger (T.pack (show value))

shrinkOverloadedLabel :: Text -> Text -> [String]
shrinkOverloadedLabel value raw
  | Just unquoted <- T.stripPrefix "#" raw,
    not ("\"" `T.isPrefixOf` unquoted) =
      [shrunk | shrunk <- shrink (T.unpack value), not (null shrunk), isValidLabelName (T.pack shrunk)]
  | otherwise = []
  where
    isValidLabelName name =
      case T.uncons name of
        Just (first, rest) ->
          (first `elem` (['a' .. 'z'] <> ['A' .. 'Z'] <> ['_']))
            && T.all isUnquotedLabelChar rest
        Nothing -> False
    isUnquotedLabelChar c =
      not (isSpace c) && c `notElem` ("()[]{},;`#\"" :: String)

-- | Shrink an expression for QuickCheck counterexample minimization.
shrinkExpr :: Expr -> [Expr]
shrinkExpr expr =
  case expr of
    EVar name -> [EVar (name {nameText = shrunk}) | shrunk <- shrinkIdent (nameText name)]
    ETypeSyntax form ty -> [ETypeSyntax form ty' | ty' <- shrinkType ty]
    EInt value _ _ -> [mkIntExpr shrunk | shrunk <- shrinkIntegral value]
    EFloat value _ _ -> [mkFloatExpr shrunk | shrunk <- shrinkFloat value]
    EChar {} -> []
    ECharHash {} -> []
    EString value _ -> [mkStringExpr (T.pack shrunk) | shrunk <- shrink (T.unpack value)]
    EStringHash value _ -> [EStringHash (T.pack shrunk) (T.pack (show shrunk) <> "#") | shrunk <- shrink (T.unpack value)]
    EOverloadedLabel value raw ->
      [EOverloadedLabel (T.pack shrunk) ("#" <> T.pack shrunk) | shrunk <- shrinkOverloadedLabel value raw]
    EQuasiQuote quoter body ->
      [EQuasiQuote quoter (T.pack shrunk) | shrunk <- shrink (T.unpack body)]
    EApp fn arg ->
      [fn, arg]
        <> [EApp fn' arg | fn' <- shrinkExpr fn]
        <> [EApp fn arg' | arg' <- shrinkExpr arg]
    EInfix lhs op rhs ->
      [lhs, rhs]
        <> [EInfix lhs' op rhs | lhs' <- shrinkExpr lhs]
        <> [EInfix lhs op rhs' | rhs' <- shrinkExpr rhs]
    ENegate inner -> inner : [ENegate inner' | inner' <- shrinkExpr inner]
    ESectionL inner op -> inner : [ESectionL inner' op | inner' <- shrinkExpr inner]
    ESectionR op inner -> inner : [ESectionR op inner' | inner' <- shrinkExpr inner]
    EIf cond thenE elseE ->
      [thenE, elseE]
        <> [EIf cond' thenE elseE | cond' <- shrinkExpr cond]
        <> [EIf cond thenE' elseE | thenE' <- shrinkExpr thenE]
        <> [EIf cond thenE elseE' | elseE' <- shrinkExpr elseE]
    EMultiWayIf rhss ->
      [EMultiWayIf rhss' | rhss' <- shrinkList shrinkGuardedRhs rhss, not (null rhss')]
    ECase scrutinee alts ->
      scrutinee
        : [ECase scrutinee' alts | scrutinee' <- shrinkExpr scrutinee]
          <> [ECase scrutinee alts' | alts' <- shrinkCaseAlts alts, not (null alts')]
    ELambdaPats pats body ->
      body : [ELambdaPats pats body' | body' <- shrinkExpr body]
    ELambdaCase alts ->
      [ELambdaCase alts' | alts' <- shrinkCaseAlts alts, not (null alts')]
    ELambdaCases alts ->
      [ELambdaCases alts' | alts' <- shrinkLambdaCaseAlts alts, not (null alts')]
    ELetDecls decls body ->
      body
        : [ELetDecls decls body' | body' <- shrinkExpr body]
          <> [ELetDecls decls' body | decls' <- shrinkDecls decls, not (null decls')]
    EDo stmts isMdo ->
      [EDo stmts' isMdo | stmts' <- shrinkDoStmts stmts, not (null stmts')]
    EListComp body stmts ->
      body
        : [EListComp body' stmts | body' <- shrinkExpr body]
          <> [EListComp body stmts' | stmts' <- shrinkCompStmts stmts, not (null stmts')]
    EListCompParallel body stmtss ->
      body
        : [EListCompParallel body' stmtss | body' <- shrinkExpr body]
          -- Each branch needs at least one statement, so filter out empty branches
          <> [EListCompParallel body stmtss' | stmtss' <- shrinkParallelCompStmts stmtss, length stmtss' >= 2]
    EList elems ->
      [EList elems' | elems' <- shrinkList shrinkExpr elems]
    ETuple tupleFlavor elems ->
      [ETuple tupleFlavor elems' | elems' <- shrinkTupleMaybeElems shrinkMaybeExpr elems]
    EArithSeq seq' ->
      [EArithSeq seq'' | seq'' <- shrinkArithSeq seq']
    ERecordCon con fields _ ->
      [ERecordCon con fields' False | fields' <- shrinkRecordFields fields]
    ERecordUpd target fields ->
      target
        : [ERecordUpd target' fields | target' <- shrinkExpr target]
          <> [ERecordUpd target fields' | fields' <- shrinkRecordFields fields]
    ETypeSig inner ty ->
      inner
        : [ETypeSig inner ty' | ty' <- shrinkType ty]
          <> [ ETypeSig
                 inner'
                 (TCon (qualifyName Nothing (mkUnqualifiedName NameConId "T")) Unpromoted)
             | inner' <- shrinkExpr inner
             ]
    ETypeApp inner ty ->
      inner
        : [ETypeApp inner ty' | ty' <- shrinkType ty]
          <> [ ETypeApp
                 inner'
                 (TCon (qualifyName Nothing (mkUnqualifiedName NameConId "T")) Unpromoted)
             | inner' <- shrinkExpr inner
             ]
    EUnboxedSum altIdx arity inner ->
      [EUnboxedSum altIdx arity inner' | inner' <- shrinkExpr inner]
    EParen inner -> inner : [EParen inner' | inner' <- shrinkExpr inner]
    ETHExpQuote body -> body : [ETHExpQuote body' | body' <- shrinkExpr body]
    ETHTypedQuote body -> body : [ETHTypedQuote body' | body' <- shrinkExpr body]
    ETHDeclQuote decls ->
      [ETHDeclQuote decls' | decls' <- shrinkDecls decls, not (null decls')]
    ETHTypeQuote ty -> [ETHTypeQuote ty' | ty' <- shrinkType ty]
    ETHPatQuote pat -> [ETHPatQuote pat' | pat' <- shrinkPattern pat]
    ETHNameQuote {} -> []
    ETHTypeNameQuote {} -> []
    ETHSplice body -> body : [ETHSplice body' | body' <- shrinkExpr body]
    ETHTypedSplice body -> body : [ETHTypedSplice body' | body' <- shrinkExpr body]
    EProc {} -> []
    EAnn _ sub -> shrinkExpr sub

shrinkCaseAlts :: [CaseAlt] -> [[CaseAlt]]
shrinkCaseAlts = shrinkList shrinkCaseAlt

shrinkLambdaCaseAlts :: [LambdaCaseAlt] -> [[LambdaCaseAlt]]
shrinkLambdaCaseAlts = shrinkList shrinkLambdaCaseAlt

shrinkCaseAlt :: CaseAlt -> [CaseAlt]
shrinkCaseAlt alt =
  case caseAltRhs alt of
    UnguardedRhs _ expr _ ->
      [alt {caseAltRhs = UnguardedRhs [] expr' Nothing} | expr' <- shrinkExpr expr]
    GuardedRhss _ rhss _ ->
      -- Shrink to unguarded using the first guard's body
      [alt {caseAltRhs = UnguardedRhs [] (guardedRhsBody firstRhs) Nothing} | firstRhs : _ <- [rhss]]
        <> [alt {caseAltRhs = GuardedRhss [] rhss' Nothing} | rhss' <- shrinkList shrinkGuardedRhs rhss, not (null rhss')]

shrinkLambdaCaseAlt :: LambdaCaseAlt -> [LambdaCaseAlt]
shrinkLambdaCaseAlt alt =
  case lambdaCaseAltRhs alt of
    UnguardedRhs _ expr _ ->
      [alt {lambdaCaseAltRhs = UnguardedRhs [] expr' Nothing} | expr' <- shrinkExpr expr]
    GuardedRhss _ rhss _ ->
      [alt {lambdaCaseAltRhs = UnguardedRhs [] (guardedRhsBody firstRhs) Nothing} | firstRhs : _ <- [rhss]]
        <> [alt {lambdaCaseAltRhs = GuardedRhss [] rhss' Nothing} | rhss' <- shrinkList shrinkGuardedRhs rhss, not (null rhss')]

shrinkGuardedRhs :: GuardedRhs -> [GuardedRhs]
shrinkGuardedRhs grhs =
  [grhs {guardedRhsBody = body'} | body' <- shrinkExpr (guardedRhsBody grhs)]
    <> [grhs {guardedRhsGuards = gs'} | gs' <- shrinkList shrinkGuardQualifier (guardedRhsGuards grhs), not (null gs')]

-- | Shrink a guard qualifier.
shrinkGuardQualifier :: GuardQualifier -> [GuardQualifier]
shrinkGuardQualifier gq =
  case gq of
    GuardAnn _ inner -> inner : shrinkGuardQualifier inner
    GuardExpr expr -> [GuardExpr expr' | expr' <- shrinkExpr expr]
    GuardPat pat expr ->
      [GuardExpr expr]
        <> [GuardPat pat' expr | pat' <- shrinkPattern pat]
        <> [GuardPat pat expr' | expr' <- shrinkExpr expr]
    GuardLet decls -> [GuardLet decls' | decls' <- shrinkDecls decls, not (null decls')]

shrinkDecls :: [Decl] -> [[Decl]]
shrinkDecls = shrinkList shrinkLetDecl

shrinkLetDecl :: Decl -> [Decl]
shrinkLetDecl decl =
  case decl of
    DeclAnn _ inner -> inner : shrinkLetDecl inner
    DeclValue (PatternBind pat rhs) ->
      [DeclValue (PatternBind pat rhs') | rhs' <- shrinkLetRhs rhs]
        <> [DeclValue (PatternBind pat' rhs) | pat' <- shrinkPattern pat]
    DeclValue (FunctionBind name matches) ->
      -- Shrink multiple matches to a single match
      [DeclValue (FunctionBind name [m {matchAnns = []}]) | length matches > 1, m <- matches]
        -- Shrink individual matches
        <> [DeclValue (FunctionBind name ms') | ms' <- shrinkList shrinkLetMatch matches, not (null ms')]
    DeclTypeSig names ty ->
      [DeclTypeSig names ty' | ty' <- shrinkType ty]
    _ -> []

-- | Shrink a match clause within let/where/TH contexts.
shrinkLetMatch :: Match -> [Match]
shrinkLetMatch match =
  [match {matchAnns = [], matchRhs = rhs'} | rhs' <- shrinkLetRhs (matchRhs match)]

-- | Shrink an RHS within let/where/TH contexts.
shrinkLetRhs :: Rhs -> [Rhs]
shrinkLetRhs rhs =
  case rhs of
    UnguardedRhs _ expr mWhere ->
      [UnguardedRhs [] expr Nothing | isJust mWhere]
        <> [UnguardedRhs [] expr' mWhere | expr' <- shrinkExpr expr]
        <> [UnguardedRhs [] expr (Just ds') | Just ds <- [mWhere], ds' <- shrinkDecls ds, not (null ds')]
    GuardedRhss _ rhss mWhere ->
      -- Collapse to unguarded using the first guard's body
      [UnguardedRhs [] (guardedRhsBody firstRhs) Nothing | firstRhs : _ <- [rhss]]
        <> [GuardedRhss [] rhss Nothing | isJust mWhere]
        <> [GuardedRhss [] rhss' mWhere | rhss' <- shrinkList shrinkGuardedRhs rhss, not (null rhss')]
        <> [GuardedRhss [] rhss (Just ds') | Just ds <- [mWhere], ds' <- shrinkDecls ds, not (null ds')]

shrinkDoStmts :: [DoStmt Expr] -> [[DoStmt Expr]]
shrinkDoStmts stmts =
  case stmts of
    [_] -> [] -- Can't shrink a single-element do block
    _ -> shrinkList shrinkDoStmt stmts

shrinkDoStmt :: DoStmt Expr -> [DoStmt Expr]
shrinkDoStmt stmt =
  case peelDoStmtAnn stmt of
    DoBind pat expr -> [DoBind pat expr' | expr' <- shrinkExpr expr]
    DoLetDecls decls -> [DoLetDecls decls' | decls' <- shrinkDecls decls, not (null decls')]
    DoExpr expr -> [DoExpr expr' | expr' <- shrinkExpr expr]
    DoRecStmt stmts -> [DoRecStmt stmts' | stmts' <- shrinkDoStmts stmts, not (null stmts')]
    DoAnn _ _ -> []

shrinkCompStmts :: [CompStmt] -> [[CompStmt]]
shrinkCompStmts = shrinkList shrinkCompStmt

-- | Shrink parallel comprehension branches, ensuring each branch has at least one statement
shrinkParallelCompStmts :: [[CompStmt]] -> [[[CompStmt]]]
shrinkParallelCompStmts =
  -- Each branch must have at least one statement
  shrinkList shrinkCompStmtsNonEmpty
  where
    shrinkCompStmtsNonEmpty stmts =
      [stmts' | stmts' <- shrinkCompStmts stmts, not (null stmts')]

shrinkCompStmt :: CompStmt -> [CompStmt]
shrinkCompStmt stmt =
  case peelCompStmtAnn stmt of
    CompGen pat expr -> [CompGen pat expr' | expr' <- shrinkExpr expr]
    CompGuard expr -> [CompGuard expr' | expr' <- shrinkExpr expr]
    CompLetDecls decls -> [CompLetDecls decls' | decls' <- shrinkDecls decls, not (null decls')]
    CompAnn _ _ -> []

shrinkTupleMaybeElems :: (a -> [a]) -> [a] -> [[a]]
shrinkTupleMaybeElems shrinkElem elems =
  case elems of
    [] -> [] -- Unit can't shrink
    _ ->
      [elems' | elems' <- shrinkList shrinkElem elems, length elems' /= 1]

shrinkMaybeExpr :: Maybe Expr -> [Maybe Expr]
shrinkMaybeExpr mExpr =
  case mExpr of
    Nothing -> []
    Just expr -> Nothing : [Just expr' | expr' <- shrinkExpr expr]

shrinkArithSeq :: ArithSeq -> [ArithSeq]
shrinkArithSeq seq' =
  case peelArithSeqAnn seq' of
    ArithSeqFrom from ->
      [ArithSeqFrom from' | from' <- shrinkExpr from]
    ArithSeqFromThen from thenE ->
      ArithSeqFrom from
        : [ArithSeqFromThen from' thenE | from' <- shrinkExpr from]
          <> [ArithSeqFromThen from thenE' | thenE' <- shrinkExpr thenE]
    ArithSeqFromTo from to ->
      ArithSeqFrom from
        : [ArithSeqFromTo from' to | from' <- shrinkExpr from]
          <> [ArithSeqFromTo from to' | to' <- shrinkExpr to]
    ArithSeqFromThenTo from thenE to ->
      ArithSeqFromTo from to
        : [ArithSeqFromThenTo from' thenE to | from' <- shrinkExpr from]
          <> [ArithSeqFromThenTo from thenE' to | thenE' <- shrinkExpr thenE]
          <> [ArithSeqFromThenTo from thenE to' | to' <- shrinkExpr to]
    ArithSeqAnn _ _ -> []

shrinkRecordFields :: [(Text, Expr)] -> [[(Text, Expr)]]
shrinkRecordFields = shrinkList shrinkRecordField

shrinkRecordField :: (Text, Expr) -> [(Text, Expr)]
shrinkRecordField (name, expr) = [(name, expr') | expr' <- shrinkExpr expr]

instance Arbitrary Expr where
  arbitrary = resize 5 genExpr
  shrink = shrinkExpr
