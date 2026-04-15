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
import Data.Text (Text)
import Data.Text qualified as T
import Test.Properties.Arb.Identifiers
  ( genCharValue,
    genConIdent,
    genFieldName,
    genIdent,
    genOptionalQualifier,
    genStringValue,
    genTenths,
    showHex,
    shrinkFloat,
    shrinkIdent,
    span0,
  )
import Test.Properties.Arb.Pattern (genPattern)
import Test.QuickCheck

expr0 :: Expr -> Expr
expr0 = exprAnnSpan span0

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
        expr0 . uncurry EApp <$> ((,) <$> genExprSizedWith allowTHQuotes half <*> genExprSizedWith allowTHQuotes half),
        (\lhs op rhs -> expr0 (EInfix lhs op rhs)) <$> genExprSizedWith allowTHQuotes half <*> genOperatorName <*> genExprSizedWith allowTHQuotes half,
        expr0 . ENegate <$> genExprSizedWith allowTHQuotes (n - 1),
        (\lhs op -> expr0 (ESectionL lhs op)) <$> genExprSizedWith allowTHQuotes (n - 1) <*> genOperatorName,
        (\op rhs -> expr0 (ESectionR op rhs)) <$> genOperatorName <*> genExprSizedWith allowTHQuotes (n - 1),
        (\a b c -> expr0 (EIf a b c)) <$> genExprSizedWith allowTHQuotes third <*> genExprSizedWith allowTHQuotes third <*> genExprSizedWith allowTHQuotes third,
        expr0 . EMultiWayIf <$> genGuardedRhsListWith allowTHQuotes (n - 1),
        (\scrut alts -> expr0 (ECase scrut alts)) <$> genExprSizedWith allowTHQuotes half <*> genCaseAltsWith allowTHQuotes half,
        (\pats body -> expr0 (ELambdaPats pats body)) <$> genPatterns half <*> genExprSizedWith allowTHQuotes half,
        expr0 . ELambdaCase <$> genCaseAltsWith allowTHQuotes (n - 1),
        (\decls body -> expr0 (ELetDecls decls body)) <$> genValueDeclsWith allowTHQuotes half <*> genExprSizedWith allowTHQuotes half,
        (\stmts isMdo -> expr0 (EDo stmts isMdo)) <$> genDoStmtsWith allowTHQuotes (n - 1) <*> arbitrary,
        (\body stmts -> expr0 (EListComp body stmts)) <$> genExprSizedWith allowTHQuotes half <*> genCompStmtsWith allowTHQuotes half,
        (\body groups -> expr0 (EListCompParallel body groups)) <$> genExprSizedWith allowTHQuotes half <*> genParallelCompStmtsWith allowTHQuotes half,
        expr0 . EList <$> genListElemsWith allowTHQuotes (n - 1),
        expr0 . ETuple Boxed . map Just <$> genTupleElemsWith allowTHQuotes (n - 1),
        expr0 . ETuple Unboxed . map Just <$> genUnboxedTupleElemsWith allowTHQuotes (n - 1),
        expr0 . ETuple Boxed <$> genTupleSectionElemsWith allowTHQuotes (n - 1),
        expr0 . ETuple Unboxed <$> genTupleSectionElemsWith allowTHQuotes (n - 1),
        genUnboxedSumExprWith allowTHQuotes (n - 1),
        expr0 . EArithSeq <$> genArithSeqWith allowTHQuotes (n - 1),
        (\name fields -> expr0 (ERecordCon name fields False)) <$> genConName <*> genRecordFieldsWith allowTHQuotes (n - 1),
        (\target fields -> expr0 (ERecordUpd target fields)) <$> genExprSizedWith allowTHQuotes half <*> genRecordFieldsWith allowTHQuotes half,
        (\e ty -> expr0 (ETypeSig e ty)) <$> genExprSizedWith allowTHQuotes half <*> genTypeWith allowTHQuotes half,
        (\e ty -> expr0 (ETypeApp (canonicalTypeAppExpr e) ty)) <$> genExprSizedWith allowTHQuotes half <*> genTypeWith allowTHQuotes half,
        expr0 . EParen <$> genExprSizedWith allowTHQuotes (n - 1),
        -- Template Haskell splices are valid inside quote bodies.
        expr0 . ETHSplice <$> genSpliceBody (n - 1),
        expr0 . ETHTypedSplice <$> genTypedSpliceBody (n - 1)
      ]
    quoteGenerators
      | allowTHQuotes =
          [ expr0 . ETHExpQuote <$> genExprSizedWith False (n - 1),
            expr0 . ETHTypedQuote <$> genExprSizedWith False (n - 1),
            expr0 . ETHDeclQuote <$> genValueDeclsWith False (n - 1),
            expr0 . ETHPatQuote <$> genPattern (n - 1),
            expr0 . ETHTypeQuote <$> genTypeWith False (n - 1),
            expr0 . ETHNameQuote <$> genNameQuoteIdent,
            expr0 . ETHTypeNameQuote <$> genTypeNameQuote
          ]
      | otherwise =
          []

-- | Generate a leaf (non-recursive) expression.
genExprLeaf :: Gen Expr
genExprLeaf =
  oneof
    [ expr0 . EVar <$> genVarName,
      genOverloadedLabel,
      mkIntExpr <$> chooseInteger (0, 999),
      mkHexExpr <$> chooseInteger (0, 255),
      mkFloatExpr <$> genTenths,
      mkCharExpr <$> genCharValue,
      mkStringExpr <$> genStringValue,
      -- MagicHash literals
      (\v -> expr0 (EIntHash v (T.pack (show v) <> "#"))) <$> chooseInteger (0, 999),
      (\v -> expr0 (EFloatHash v (T.pack (show v) <> "#"))) <$> genTenths,
      (\v -> expr0 (ECharHash v (T.pack (show v) <> "#"))) <$> genCharValue,
      (\v -> expr0 (EStringHash v (T.pack (show (T.unpack v)) <> "#"))) <$> genStringValue,
      (\q b -> expr0 (EQuasiQuote q b)) <$> genQuasiQuoteName <*> genStringValue,
      pure (expr0 (EList [])),
      pure (expr0 (ETuple Boxed [])),
      pure (expr0 (ETuple Unboxed [])),
      (\n -> expr0 (ETuple Boxed (replicate n Nothing))) <$> chooseInt (2, 5),
      (\n -> expr0 (ETuple Unboxed (replicate n Nothing))) <$> chooseInt (2, 5)
    ]

genOverloadedLabel :: Gen Expr
genOverloadedLabel = do
  labelName <- genIdent
  pure (expr0 (EOverloadedLabel labelName ("#" <> labelName)))

-- | Generate a quasi-quote name, excluding TH bracket names (e, d, p, t) which
-- would collide with Template Haskell bracket syntax ([e|...|], [d|...|], etc.).
genQuasiQuoteName :: Gen Text
genQuasiQuoteName = suchThat genIdent (`notElem` ["e", "d", "p", "t"])

-- | Generate the body of a TH splice: either a bare variable or a parenthesized expression.
-- Bare variables produce $name syntax; parenthesized produce $(expr) syntax.
genSpliceBody :: Int -> Gen Expr
genSpliceBody n =
  oneof
    [ expr0 . EVar <$> genVarName,
      expr0 . EParen <$> genExprSized (max 0 (n - 1))
    ]

-- | Generate the body of a TH typed splice: always parenthesized.
-- Typed splices require parentheses: $$(expr) is valid, $$expr is invalid.
genTypedSpliceBody :: Int -> Gen Expr
genTypedSpliceBody n =
  expr0 . EParen <$> genExprSized (max 0 (n - 1))

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
   in not reserved && not dashOnly

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
      qualifyName Nothing . mkUnqualifiedName NameVarSym <$> genOperator
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
      { caseAltSpan = span0,
        caseAltPattern = pat,
        caseAltRhs = rhs
      }
  where
    half = n `div` 2

genRhsWith :: Bool -> Int -> Gen Rhs
genRhsWith allowTHQuotes n =
  oneof
    [ (\e -> UnguardedRhs span0 e Nothing) <$> genBindingExprWith allowTHQuotes n,
      (\gs -> GuardedRhss span0 gs Nothing) <$> genGuardedRhsListWith allowTHQuotes n
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
      { guardedRhsSpan = span0,
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
      GuardExpr span0 <$> genExprSizedWith allowTHQuotes n,
      -- Pattern guard: | pat <- expr = ...
      -- The guarded-qualifier parser now accepts the full pattern generator,
      -- which includes parenthesized view patterns such as `(view -> pat)`.
      GuardPat span0 <$> genPattern half <*> genExprSizedWith allowTHQuotes half,
      -- Let guard: | let decls = ...
      GuardLet span0 <$> genValueDeclsWith allowTHQuotes n
    ]
  where
    half = n `div` 2

-- | Generate value declarations for let/where.
-- Zero-argument bindings are generated as PatternBind so they keep the same
-- AST shape after pretty-print/parser roundtrip.
genValueDeclsWith :: Bool -> Int -> Gen [Decl]
genValueDeclsWith allowTHQuotes n = do
  count <- chooseInt (0, 3)
  names <- vectorOf count (mkUnqualifiedName NameVarId <$> genIdent)
  exprs <- vectorOf count (genBindingExprWith allowTHQuotes (n `div` max 1 count))
  pure
    [ DeclValue
        ( PatternBind
            span0
            (patternAnnSpan span0 (PVar name))
            (UnguardedRhs span0 expr Nothing)
        )
    | (name, expr) <- zip names exprs
    ]

genBindingExprWith :: Bool -> Int -> Gen Expr
genBindingExprWith = genExprSizedWith

genDoStmtsWith :: Bool -> Int -> Gen [DoStmt Expr]
genDoStmtsWith allowTHQuotes n = do
  count <- chooseInt (1, 3)
  let perStmt = n `div` count
  stmts <- vectorOf (count - 1) (genDoStmtWith allowTHQuotes perStmt)
  -- Last statement must be DoExpr
  lastExpr <- genExprSizedWith allowTHQuotes perStmt
  pure (stmts <> [DoExpr span0 lastExpr])

genDoStmtWith :: Bool -> Int -> Gen (DoStmt Expr)
genDoStmtWith allowTHQuotes n =
  oneof
    [ DoBind span0 <$> genPattern half <*> genExprSizedWith allowTHQuotes half,
      DoLetDecls span0 <$> genValueDeclsWith allowTHQuotes (n - 1),
      DoExpr span0 <$> genExprSizedWith allowTHQuotes (n - 1),
      DoRecStmt span0 <$> genRecDoStmtsWith allowTHQuotes (n - 1)
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
      [ DoBind span0 <$> genPattern (perStmt `div` 2) <*> genExprSizedWith allowTHQuotes (perStmt `div` 2),
        DoLetDecls span0 <$> genValueDeclsWith allowTHQuotes perStmt,
        DoExpr span0 <$> genExprSizedWith allowTHQuotes perStmt
      ]

genCompStmtsWith :: Bool -> Int -> Gen [CompStmt]
genCompStmtsWith allowTHQuotes n = do
  count <- chooseInt (1, 3)
  vectorOf count (genCompStmtWith allowTHQuotes (n `div` count))

genCompStmtWith :: Bool -> Int -> Gen CompStmt
genCompStmtWith allowTHQuotes n =
  oneof
    [ CompGen span0 <$> genPattern half <*> genExprSizedWith allowTHQuotes half,
      CompGuard span0 <$> genExprSizedWith allowTHQuotes (n - 1),
      CompLetDecls span0 <$> genValueDeclsWith allowTHQuotes (n - 1)
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
  pure (expr0 (EUnboxedSum altIdx arity inner))

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
    [ ArithSeqFrom span0 <$> genExprSizedWith allowTHQuotes n,
      ArithSeqFromThen span0 <$> genExprSizedWith allowTHQuotes half <*> genExprSizedWith allowTHQuotes half,
      ArithSeqFromTo span0 <$> genExprSizedWith allowTHQuotes half <*> genExprSizedWith allowTHQuotes half,
      ArithSeqFromThenTo span0 <$> genExprSizedWith allowTHQuotes third <*> genExprSizedWith allowTHQuotes third <*> genExprSizedWith allowTHQuotes third
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
          TApp span0 <$> genTypeWith allowTHQuotes half <*> genTypeWith allowTHQuotes half,
          TFun span0 <$> genTypeWith allowTHQuotes half <*> genTypeWith allowTHQuotes half,
          TList span0 Unpromoted <$> genTypeListElemsWith allowTHQuotes (n - 1),
          TTuple span0 Boxed Unpromoted <$> genTypeTupleElemsWith allowTHQuotes (n - 1),
          TParen span0 <$> genTypeWith allowTHQuotes (n - 1)
        ]
  where
    half = n `div` 2

genTypeLeaf :: Gen Type
genTypeLeaf =
  oneof
    [ TVar span0 <$> genTypeVarName,
      (\name -> TCon span0 name Unpromoted) <$> genConAstName
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
  EIntHash {} -> e
  EIntBase {} -> e
  EIntBaseHash {} -> e
  EFloat {} -> e
  EFloatHash {} -> e
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
  _ -> expr0 (EParen e)

-- | Literal expression constructors
mkHexExpr :: Integer -> Expr
mkHexExpr value = expr0 (EIntBase value ("0x" <> T.pack (showHex value)))

mkFloatExpr :: Double -> Expr
mkFloatExpr value = expr0 (EFloat value (T.pack (show value)))

mkCharExpr :: Char -> Expr
mkCharExpr value = expr0 (EChar value (T.pack (show value)))

mkStringExpr :: Text -> Expr
mkStringExpr value = expr0 (EString value (T.pack (show (T.unpack value))))

-- | Create an integer expression with canonical representation.
mkIntExpr :: Integer -> Expr
mkIntExpr value = expr0 (EInt value (T.pack (show value)))

renderIntBaseHash :: Integer -> Text
renderIntBaseHash value
  | value < 0 = "-0x" <> T.pack (showHex (abs value)) <> "#"
  | otherwise = "0x" <> T.pack (showHex value) <> "#"

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
    EVar name -> [expr0 (EVar (name {nameText = shrunk})) | shrunk <- shrinkIdent (nameText name)]
    EInt value _ -> [mkIntExpr shrunk | shrunk <- shrinkIntegral value]
    EIntHash value _ -> [expr0 (EIntHash shrunk (T.pack (show shrunk) <> "#")) | shrunk <- shrinkIntegral value]
    EIntBase value _ -> [mkIntExpr shrunk | shrunk <- shrinkIntegral value]
    EIntBaseHash value _ -> [expr0 (EIntBaseHash shrunk (renderIntBaseHash shrunk)) | shrunk <- shrinkIntegral value]
    EFloat value _ -> [mkFloatExpr shrunk | shrunk <- shrinkFloat value]
    EFloatHash value _ -> [expr0 (EFloatHash shrunk (T.pack (show shrunk) <> "#")) | shrunk <- shrinkFloat value]
    EChar {} -> []
    ECharHash {} -> []
    EString value _ -> [mkStringExpr (T.pack shrunk) | shrunk <- shrink (T.unpack value)]
    EStringHash value _ -> [expr0 (EStringHash (T.pack shrunk) (T.pack (show shrunk) <> "#")) | shrunk <- shrink (T.unpack value)]
    EOverloadedLabel value raw ->
      [expr0 (EOverloadedLabel (T.pack shrunk) ("#" <> T.pack shrunk)) | shrunk <- shrinkOverloadedLabel value raw]
    EQuasiQuote quoter body ->
      [expr0 (EQuasiQuote quoter (T.pack shrunk)) | shrunk <- shrink (T.unpack body)]
    EApp fn arg ->
      [fn, arg]
        <> [expr0 (EApp fn' arg) | fn' <- shrinkExpr fn]
        <> [expr0 (EApp fn arg') | arg' <- shrinkExpr arg]
    EInfix lhs op rhs ->
      [lhs, rhs]
        <> [expr0 (EInfix lhs' op rhs) | lhs' <- shrinkExpr lhs]
        <> [expr0 (EInfix lhs op rhs') | rhs' <- shrinkExpr rhs]
    ENegate inner -> inner : [expr0 (ENegate inner') | inner' <- shrinkExpr inner]
    ESectionL inner op -> inner : [expr0 (ESectionL inner' op) | inner' <- shrinkExpr inner]
    ESectionR op inner -> inner : [expr0 (ESectionR op inner') | inner' <- shrinkExpr inner]
    EIf cond thenE elseE ->
      [thenE, elseE]
        <> [expr0 (EIf cond' thenE elseE) | cond' <- shrinkExpr cond]
        <> [expr0 (EIf cond thenE' elseE) | thenE' <- shrinkExpr thenE]
        <> [expr0 (EIf cond thenE elseE') | elseE' <- shrinkExpr elseE]
    EMultiWayIf rhss ->
      [expr0 (EMultiWayIf rhss') | rhss' <- shrinkList shrinkGuardedRhs rhss, not (null rhss')]
    ECase scrutinee alts ->
      scrutinee
        : [expr0 (ECase scrutinee' alts) | scrutinee' <- shrinkExpr scrutinee]
          <> [expr0 (ECase scrutinee alts') | alts' <- shrinkCaseAlts alts, not (null alts')]
    ELambdaPats pats body ->
      body : [expr0 (ELambdaPats pats body') | body' <- shrinkExpr body]
    ELambdaCase alts ->
      [expr0 (ELambdaCase alts') | alts' <- shrinkCaseAlts alts, not (null alts')]
    ELetDecls decls body ->
      body
        : [expr0 (ELetDecls decls body') | body' <- shrinkExpr body]
          <> [expr0 (ELetDecls decls' body) | decls' <- shrinkDecls decls, not (null decls')]
    EDo stmts isMdo ->
      [expr0 (EDo stmts' isMdo) | stmts' <- shrinkDoStmts stmts, not (null stmts')]
    EListComp body stmts ->
      body
        : [expr0 (EListComp body' stmts) | body' <- shrinkExpr body]
          <> [expr0 (EListComp body stmts') | stmts' <- shrinkCompStmts stmts, not (null stmts')]
    EListCompParallel body stmtss ->
      body
        : [expr0 (EListCompParallel body' stmtss) | body' <- shrinkExpr body]
          -- Each branch needs at least one statement, so filter out empty branches
          <> [expr0 (EListCompParallel body stmtss') | stmtss' <- shrinkParallelCompStmts stmtss, length stmtss' >= 2]
    EList elems ->
      [expr0 (EList elems') | elems' <- shrinkList shrinkExpr elems]
    ETuple tupleFlavor elems ->
      [expr0 (ETuple tupleFlavor elems') | elems' <- shrinkTupleMaybeElems shrinkMaybeExpr elems]
    EArithSeq seq' ->
      [expr0 (EArithSeq seq'') | seq'' <- shrinkArithSeq seq']
    ERecordCon con fields _ ->
      [expr0 (ERecordCon con fields' False) | fields' <- shrinkRecordFields fields]
    ERecordUpd target fields ->
      target
        : [expr0 (ERecordUpd target' fields) | target' <- shrinkExpr target]
          <> [expr0 (ERecordUpd target fields') | fields' <- shrinkRecordFields fields]
    ETypeSig inner _ ->
      inner : [expr0 (ETypeSig inner' (TCon span0 (qualifyName Nothing (mkUnqualifiedName NameConId "T")) Unpromoted)) | inner' <- shrinkExpr inner]
    ETypeApp inner _ ->
      inner : [expr0 (ETypeApp inner' (TCon span0 (qualifyName Nothing (mkUnqualifiedName NameConId "T")) Unpromoted)) | inner' <- shrinkExpr inner]
    EUnboxedSum altIdx arity inner ->
      [expr0 (EUnboxedSum altIdx arity inner') | inner' <- shrinkExpr inner]
    EParen inner -> inner : [expr0 (EParen inner') | inner' <- shrinkExpr inner]
    ETHExpQuote body -> body : [expr0 (ETHExpQuote body') | body' <- shrinkExpr body]
    ETHTypedQuote body -> body : [expr0 (ETHTypedQuote body') | body' <- shrinkExpr body]
    ETHDeclQuote {} -> []
    ETHTypeQuote {} -> []
    ETHPatQuote {} -> []
    ETHNameQuote {} -> []
    ETHTypeNameQuote {} -> []
    ETHSplice body -> body : [expr0 (ETHSplice body') | body' <- shrinkExpr body]
    ETHTypedSplice body -> body : [expr0 (ETHTypedSplice body') | body' <- shrinkExpr body]
    EProc {} -> []
    EAnn _ sub -> shrinkExpr sub

shrinkCaseAlts :: [CaseAlt] -> [[CaseAlt]]
shrinkCaseAlts = shrinkList shrinkCaseAlt

shrinkCaseAlt :: CaseAlt -> [CaseAlt]
shrinkCaseAlt alt =
  case caseAltRhs alt of
    UnguardedRhs _ expr _ ->
      [alt {caseAltRhs = UnguardedRhs span0 expr' Nothing} | expr' <- shrinkExpr expr]
    GuardedRhss _ rhss _ ->
      -- Shrink to unguarded using the first guard's body
      [alt {caseAltRhs = UnguardedRhs span0 (guardedRhsBody firstRhs) Nothing} | firstRhs : _ <- [rhss]]
        <> [alt {caseAltRhs = GuardedRhss span0 rhss' Nothing} | rhss' <- shrinkList shrinkGuardedRhs rhss, not (null rhss')]

shrinkGuardedRhs :: GuardedRhs -> [GuardedRhs]
shrinkGuardedRhs grhs =
  [grhs {guardedRhsBody = body'} | body' <- shrinkExpr (guardedRhsBody grhs)]

shrinkDecls :: [Decl] -> [[Decl]]
shrinkDecls = shrinkList shrinkLetDecl

shrinkLetDecl :: Decl -> [Decl]
shrinkLetDecl decl =
  case decl of
    DeclValue (PatternBind _ pat (UnguardedRhs _ expr _)) ->
      [DeclValue (PatternBind span0 pat (UnguardedRhs span0 expr' Nothing)) | expr' <- shrinkExpr expr]
    DeclValue (FunctionBind _ name [match@Match {matchRhs = UnguardedRhs _ expr _}]) ->
      [ DeclValue
          ( FunctionBind
              span0
              name
              [match {matchSpan = span0, matchRhs = UnguardedRhs span0 expr' Nothing}]
          )
      | expr' <- shrinkExpr expr
      ]
    _ -> []

shrinkDoStmts :: [DoStmt Expr] -> [[DoStmt Expr]]
shrinkDoStmts stmts =
  case stmts of
    [_] -> [] -- Can't shrink a single-element do block
    _ -> shrinkList shrinkDoStmt stmts

shrinkDoStmt :: DoStmt Expr -> [DoStmt Expr]
shrinkDoStmt stmt =
  case stmt of
    DoBind _ pat expr -> [DoBind span0 pat expr' | expr' <- shrinkExpr expr]
    DoLetDecls _ decls -> [DoLetDecls span0 decls' | decls' <- shrinkDecls decls, not (null decls')]
    DoExpr _ expr -> [DoExpr span0 expr' | expr' <- shrinkExpr expr]
    DoRecStmt _ stmts -> [DoRecStmt span0 stmts' | stmts' <- shrinkDoStmts stmts, not (null stmts')]

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
  case stmt of
    CompGen _ pat expr -> [CompGen span0 pat expr' | expr' <- shrinkExpr expr]
    CompGuard _ expr -> [CompGuard span0 expr' | expr' <- shrinkExpr expr]
    CompLet _ bindings -> [CompLet span0 bindings' | bindings' <- shrinkList shrinkBinding bindings, not (null bindings')]
    CompLetDecls _ decls -> [CompLetDecls span0 decls' | decls' <- shrinkDecls decls, not (null decls')]

shrinkBinding :: (Text, Expr) -> [(Text, Expr)]
shrinkBinding (name, expr) = [(name, expr') | expr' <- shrinkExpr expr]

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
  case seq' of
    ArithSeqFrom _ from ->
      [ArithSeqFrom span0 from' | from' <- shrinkExpr from]
    ArithSeqFromThen _ from thenE ->
      ArithSeqFrom span0 from
        : [ArithSeqFromThen span0 from' thenE | from' <- shrinkExpr from]
          <> [ArithSeqFromThen span0 from thenE' | thenE' <- shrinkExpr thenE]
    ArithSeqFromTo _ from to ->
      ArithSeqFrom span0 from
        : [ArithSeqFromTo span0 from' to | from' <- shrinkExpr from]
          <> [ArithSeqFromTo span0 from to' | to' <- shrinkExpr to]
    ArithSeqFromThenTo _ from thenE to ->
      ArithSeqFromTo span0 from to
        : [ArithSeqFromThenTo span0 from' thenE to | from' <- shrinkExpr from]
          <> [ArithSeqFromThenTo span0 from thenE' to | thenE' <- shrinkExpr thenE]
          <> [ArithSeqFromThenTo span0 from thenE to' | to' <- shrinkExpr to]

shrinkRecordFields :: [(Text, Expr)] -> [[(Text, Expr)]]
shrinkRecordFields = shrinkList shrinkRecordField

shrinkRecordField :: (Text, Expr) -> [(Text, Expr)]
shrinkRecordField (name, expr) = [(name, expr') | expr' <- shrinkExpr expr]

instance Arbitrary Expr where
  arbitrary = resize 5 genExpr
  shrink = shrinkExpr
