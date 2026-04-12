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

import Aihc.Parser.Lex (isReservedIdentifier)
import Aihc.Parser.Syntax
import Data.Char (isSpace)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Properties.Arb.Identifiers (extensionReservedIdentifiers, genIdent, shrinkIdent)
import Test.Properties.Arb.Pattern qualified as Pat
import Test.QuickCheck

-- | Canonical empty source span for normalization.
span0 :: SourceSpan
span0 = noSourceSpan

-- | Generate a random expression. Uses QuickCheck's size parameter
-- to control recursion depth.
genExpr :: Gen Expr
genExpr = sized genExprSized

-- | Generate an expression with a given size budget.
-- When size is 0, only generate non-recursive (leaf) expressions.
genExprSized :: Int -> Gen Expr
genExprSized n
  | n <= 0 = genExprLeaf
  | otherwise =
      oneof
        [ -- Leaf expressions
          genExprLeaf,
          -- Recursive expressions (reduce size for subexpressions)
          EApp span0 <$> genExprSized half <*> genExprSized half,
          EInfix span0 <$> genExprSized half <*> genOperatorName <*> genExprSized half,
          ENegate span0 <$> genExprSized (n - 1),
          ESectionL span0 <$> genExprSized (n - 1) <*> genOperatorName,
          ESectionR span0 <$> genOperatorName <*> genExprSized (n - 1),
          EIf span0 <$> genExprSized third <*> genExprSized third <*> genExprSized third,
          ECase span0 <$> genExprSized half <*> genCaseAlts half,
          ELambdaPats span0 <$> genPatterns half <*> genExprSized half,
          ELambdaCase span0 <$> genCaseAlts (n - 1),
          ELetDecls span0 <$> genValueDecls half <*> genExprSized half,
          EWhereDecls span0 <$> genExprSized half <*> genValueDecls half,
          EDo span0 <$> genDoStmts (n - 1) <*> pure False,
          EListComp span0 <$> genExprSized half <*> genCompStmts half,
          EListCompParallel span0 <$> genExprSized half <*> genParallelCompStmts half,
          EList span0 <$> genListElems (n - 1),
          ETuple span0 Boxed . map Just <$> genTupleElems (n - 1),
          ETuple span0 Unboxed . map Just <$> genUnboxedTupleElems (n - 1),
          ETuple span0 Boxed <$> genTupleSectionElems (n - 1),
          ETuple span0 Unboxed <$> genTupleSectionElems (n - 1),
          genUnboxedSumExpr (n - 1),
          EArithSeq span0 <$> genArithSeq (n - 1),
          ERecordCon span0 <$> genConName <*> genRecordFields (n - 1) <*> pure False,
          ERecordUpd span0 <$> genExprSized half <*> genRecordFields half,
          ETypeSig span0 <$> genExprSized half <*> genType half,
          EParen span0 <$> genExprSized (n - 1),
          -- Template Haskell splices and quotes
          ETHSplice span0 <$> genSpliceBody (n - 1),
          ETHTypedSplice span0 <$> genTypedSpliceBody (n - 1),
          ETHExpQuote span0 <$> genExprSized (n - 1),
          ETHTypedQuote span0 <$> genExprSized (n - 1),
          ETHDeclQuote span0 <$> genValueDecls (n - 1),
          ETHPatQuote span0 <$> genSimplePattern (n - 1),
          ETHTypeQuote span0 <$> genType (n - 1),
          ETHNameQuote span0 <$> genNameQuoteIdent,
          ETHTypeNameQuote span0 <$> genConName
        ]
  where
    half = n `div` 2
    third = n `div` 3

-- | Generate a leaf (non-recursive) expression.
genExprLeaf :: Gen Expr
genExprLeaf =
  oneof
    [ EVar span0 <$> genVarName,
      mkIntExpr <$> chooseInteger (0, 999),
      mkHexExpr <$> chooseInteger (0, 255),
      mkFloatExpr <$> genTenths,
      mkCharExpr <$> genCharValue,
      mkStringExpr <$> genStringValue,
      EQuasiQuote span0 <$> genQuasiQuoteName <*> genStringValue,
      pure (EList span0 []),
      pure (ETuple span0 Boxed []),
      pure (ETuple span0 Unboxed []),
      (\n -> ETuple span0 Boxed (replicate n Nothing)) <$> chooseInt (2, 5),
      (\n -> ETuple span0 Unboxed (replicate n Nothing)) <$> chooseInt (2, 5)
    ]

-- | Generate a quasi-quote name, excluding TH bracket names (e, d, p, t) which
-- would collide with Template Haskell bracket syntax ([e|...|], [d|...|], etc.).
genQuasiQuoteName :: Gen Text
genQuasiQuoteName = suchThat genIdent (`notElem` ["e", "d", "p", "t"])

-- | Generate the body of a TH splice: either a bare variable or a parenthesized expression.
-- Bare variables produce $name syntax; parenthesized produce $(expr) syntax.
genSpliceBody :: Int -> Gen Expr
genSpliceBody n =
  oneof
    [ EVar span0 <$> genVarName,
      EParen span0 <$> genExprSized (max 0 (n - 1))
    ]

-- | Generate the body of a TH typed splice: always parenthesized.
-- Typed splices require parentheses: $$(expr) is valid, $$expr is invalid.
genTypedSpliceBody :: Int -> Gen Expr
genTypedSpliceBody n =
  EParen span0 <$> genExprSized (max 0 (n - 1))

-- | Generate a pattern for TH pattern quotes [p| pat |].
-- Any pattern is valid inside [p| |], so use the full pattern generator.
genSimplePattern :: Int -> Gen Pattern
genSimplePattern = Pat.genPattern

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

-- | Generate an optional module qualifier (e.g., Nothing or Just "Data.List").
-- Biased towards Nothing to keep most names unqualified.
genOptionalQualifier :: Gen (Maybe Text)
genOptionalQualifier =
  frequency
    [ (3, pure Nothing),
      (1, Just <$> genModuleQualifier)
    ]

-- | Generate a module qualifier like "Data.List" or "Prelude".
genModuleQualifier :: Gen Text
genModuleQualifier = do
  segCount <- chooseInt (1, 3)
  segs <- vectorOf segCount genModuleSegment
  pure (T.intercalate "." segs)

-- | Generate a single module name segment (starts with uppercase).
genModuleSegment :: Gen Text
genModuleSegment = do
  first <- elements ['A' .. 'Z']
  restLen <- chooseInt (0, 5)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']))
  pure (T.pack (first : rest))

genOperatorName :: Gen Name
genOperatorName = do
  qual <- genOptionalQualifier
  op <- mkUnqualifiedName NameVarSym <$> genOperator
  -- NOTE: Qualified "." creates "Module.." which the lexer sees as ".." (range).
  -- Skip qualified "." until the pretty-printer handles this case.
  if isJust qual && unqualifiedNameText op == "."
    then genOperatorName
    else pure (qualifyName qual op)

-- | Generate a custom operator
-- Only uses valid operator characters (matching isOperatorToken in Pretty.hs)
genCustomOperator :: Gen Text
genCustomOperator = do
  len <- chooseInt (1, 3)
  -- Note: matches ":!#$%&*+./<=>?\\^|-~" from Pretty.hs isOperatorToken
  -- Excluding ':' since that's for constructor operators
  -- Excluding '#' because it conflicts with (# and #) tokens when UnboxedTuples/UnboxedSums is enabled
  chars <- vectorOf len (elements "!$%&*+./<=>?\\^|-~")
  let candidate = T.pack chars
  -- Avoid reserved operators and symbols that lex as comments.
  if isValidGeneratedOperator candidate
    then pure candidate
    else genCustomOperator

isValidGeneratedOperator :: Text -> Bool
isValidGeneratedOperator candidate =
  let reserved =
        candidate
          `elem` ["..", "::", "=", "\\", "|", "<-", "->", "~", "=>", "--", "-<", ">-", "-<<", ">>-"]
      dashOnly = T.length candidate >= 2 && T.all (== '-') candidate
   in not reserved && not dashOnly

-- | Generate a data constructor name
genConName :: Gen Text
genConName = do
  first <- elements ['A' .. 'Z']
  restLen <- chooseInt (0, 5)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'"))
  pure (T.pack (first : rest))

genVarName :: Gen Name
genVarName = qualifyName <$> genOptionalQualifier <*> (mkUnqualifiedName NameVarId <$> genIdent)

genConAstName :: Gen Name
genConAstName = qualifyName <$> genOptionalQualifier <*> (mkUnqualifiedName NameConId <$> genConName)

-- | Generate simple patterns for lambdas
genPatterns :: Int -> Gen [Pattern]
genPatterns n = do
  count <- chooseInt (1, 3)
  vectorOf count (genPattern n)

-- | Generate a pattern using the full pattern generator from the Pattern module.
genPattern :: Int -> Gen Pattern
genPattern = Pat.genPattern

-- | Generate a pattern safe for comprehension/guard contexts.
-- Excludes PView, PIrrefutable, PStrict, and PAs at all depths.
genPatternNoView :: Int -> Gen Pattern
genPatternNoView = Pat.genPatternNoView

-- | Generate case alternatives
genCaseAlts :: Int -> Gen [CaseAlt]
genCaseAlts n = do
  count <- chooseInt (1, 3)
  vectorOf count (genCaseAlt n)

genCaseAlt :: Int -> Gen CaseAlt
genCaseAlt n = do
  pat <- genPattern half
  rhs <- genRhs half
  pure $
    CaseAlt
      { caseAltSpan = span0,
        caseAltPattern = pat,
        caseAltRhs = rhs
      }
  where
    half = n `div` 2

-- | Generate a right-hand side: either unguarded or guarded.
genRhs :: Int -> Gen Rhs
genRhs n =
  oneof
    [ UnguardedRhs span0 <$> genExprSized n,
      GuardedRhss span0 <$> genGuardedRhsList n
    ]

-- | Generate a non-empty list of guarded RHS clauses.
genGuardedRhsList :: Int -> Gen [GuardedRhs]
genGuardedRhsList n = do
  count <- chooseInt (1, 3)
  vectorOf count (genGuardedRhs (n `div` count))

-- | Generate a single guarded RHS: guards and body expression.
genGuardedRhs :: Int -> Gen GuardedRhs
genGuardedRhs n = do
  guardCount <- chooseInt (1, 2)
  guards <- vectorOf guardCount (genGuardQualifier (half `div` guardCount))
  body <- genExprSized half
  pure $
    GuardedRhs
      { guardedRhsSpan = span0,
        guardedRhsGuards = guards,
        guardedRhsBody = body
      }
  where
    half = n `div` 2

-- | Generate a guard qualifier.
genGuardQualifier :: Int -> Gen GuardQualifier
genGuardQualifier n =
  oneof
    [ -- Boolean guard: | expr = ...
      -- TODO: Restore bare genExprSized here once the parser/pretty-printer handles
      -- ETypeSig in guard expressions. Currently, an unparenthesized type signature
      -- like `| expr :: Type -> body` makes the parser interpret `Type -> body` as
      -- a function type rather than the guard's arrow.
      GuardExpr span0 . parenTypeSig <$> genExprSized n,
      -- Pattern guard: | pat <- expr = ...
      -- TODO: Restore genPattern here once the parser supports view patterns inside
      -- guard qualifiers. Currently, the '->' in view patterns (PView) conflicts
      -- with guard/case-alternative syntax and causes parse failures.
      -- The expression is also parenthesized if it's an ETypeSig, since
      -- `| pat <- expr :: Type -> body` has the same ambiguity.
      GuardPat span0 <$> genPatternNoView half <*> (parenTypeSig <$> genExprSized half),
      -- Let guard: | let decls = ...
      GuardLet span0 <$> genValueDecls n
    ]
  where
    half = n `div` 2
    -- Wrap ETypeSig in parens to avoid ambiguity with the guard arrow
    parenTypeSig e@(ETypeSig {}) = EParen span0 e
    parenTypeSig e = e

-- | Generate value declarations for let/where
-- We use FunctionBind format because that's what the parser produces for simple
-- variable bindings like `x = e`.
genValueDecls :: Int -> Gen [Decl]
genValueDecls n = do
  count <- chooseInt (0, 3)
  names <- vectorOf count (mkUnqualifiedName NameVarId <$> genIdent)
  exprs <- vectorOf count (genExprSized (n `div` count))
  pure
    [ DeclValue
        span0
        ( FunctionBind
            span0
            name
            [ Match
                { matchSpan = span0,
                  matchHeadForm = MatchHeadPrefix,
                  matchPats = [],
                  matchRhs = UnguardedRhs span0 expr
                }
            ]
        )
    | (name, expr) <- zip names exprs
    ]

-- | Generate do statements
genDoStmts :: Int -> Gen [DoStmt Expr]
genDoStmts n = do
  count <- chooseInt (1, 3)
  let perStmt = n `div` count
  stmts <- vectorOf (count - 1) (genDoStmt perStmt)
  -- Last statement must be DoExpr
  lastExpr <- genExprSized perStmt
  pure (stmts <> [DoExpr span0 lastExpr])

genDoStmt :: Int -> Gen (DoStmt Expr)
genDoStmt n =
  oneof
    [ DoBind span0 <$> genPattern half <*> genExprSized half,
      DoLetDecls span0 <$> genValueDecls (n - 1),
      DoExpr span0 <$> genExprSized (n - 1)
    ]
  where
    half = n `div` 2

-- | Generate list comprehension statements
genCompStmts :: Int -> Gen [CompStmt]
genCompStmts n = do
  count <- chooseInt (1, 3)
  vectorOf count (genCompStmt (n `div` count))

genCompStmt :: Int -> Gen CompStmt
genCompStmt n =
  oneof
    [ -- TODO: Restore genPattern here once the parser supports all pattern
      -- constructors inside list comprehension generators. Currently, PView (->),
      -- PIrrefutable (~), PStrict (!), and PAs (@) fail when nested inside
      -- compound patterns (PList, PTuple, PCon args) in comprehension contexts.
      CompGen span0 <$> genPatternNoView half <*> genExprSized half,
      CompGuard span0 <$> genExprSized (n - 1)
    ]
  where
    half = n `div` 2

-- | Generate parallel list comprehension statements
genParallelCompStmts :: Int -> Gen [[CompStmt]]
genParallelCompStmts n = do
  count <- chooseInt (2, 3)
  vectorOf count (genCompStmts (n `div` count))

-- | Generate list elements
genListElems :: Int -> Gen [Expr]
genListElems n = do
  count <- chooseInt (0, 4)
  vectorOf count (genExprSized (n `div` max 1 count))

-- | Generate tuple elements
genTupleElems :: Int -> Gen [Expr]
genTupleElems n = do
  isUnit <- arbitrary
  if isUnit
    then pure []
    else do
      count <- chooseInt (2, 4)
      vectorOf count (genExprSized (n `div` count))

-- | Generate elements for an unboxed tuple (0 or 2-4 elements).
-- Unlike boxed tuples, unboxed tuples with 0 elements are valid Haskell.
-- NOTE: 1-element unboxed tuples are valid Haskell but the parser doesn't
-- accept them yet, so we skip generating them for now.
genUnboxedTupleElems :: Int -> Gen [Expr]
genUnboxedTupleElems n = do
  count <- chooseInt (0, 4)
  if count == 1 then pure [] else vectorOf count (genExprSized (n `div` max 1 count))

-- | Generate an unboxed sum expression
genUnboxedSumExpr :: Int -> Gen Expr
genUnboxedSumExpr n = do
  arity <- chooseInt (2, 4)
  altIdx <- chooseInt (0, arity - 1)
  inner <- genExprSized n
  pure (EUnboxedSum span0 altIdx arity inner)

-- | Generate tuple section elements
genTupleSectionElems :: Int -> Gen [Maybe Expr]
genTupleSectionElems n = do
  count <- chooseInt (2, 4)
  elems <- vectorOf count (genMaybeExpr (n `div` count))
  -- Ensure at least one Nothing (otherwise it's just a tuple)
  if Nothing `notElem` elems
    then do
      idx <- chooseInt (0, count - 1)
      pure (take idx elems <> [Nothing] <> drop (idx + 1) elems)
    else pure elems

genMaybeExpr :: Int -> Gen (Maybe Expr)
genMaybeExpr n =
  oneof
    [ Just <$> genExprSized n,
      pure Nothing
    ]

-- | Generate arithmetic sequences
genArithSeq :: Int -> Gen ArithSeq
genArithSeq n =
  oneof
    [ ArithSeqFrom span0 <$> genExprSized n,
      ArithSeqFromThen span0 <$> genExprSized half <*> genExprSized half,
      ArithSeqFromTo span0 <$> genExprSized half <*> genExprSized half,
      ArithSeqFromThenTo span0 <$> genExprSized third <*> genExprSized third <*> genExprSized third
    ]
  where
    half = n `div` 2
    third = n `div` 3

-- | Generate record fields
genRecordFields :: Int -> Gen [(Text, Expr)]
genRecordFields n = do
  count <- chooseInt (0, 3)
  names <- vectorOf count genFieldName
  exprs <- vectorOf count (genExprSized (n `div` max 1 count))
  pure (zip names exprs)

genFieldName :: Gen Text
genFieldName = do
  first <- elements (['a' .. 'z'] <> ['_'])
  restLen <- chooseInt (0, 5)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'"))
  let candidate = T.pack (first : rest)
  if isReservedIdentifier candidate
    then genFieldName
    else pure candidate

-- | Generate a type (simple version for use inside expressions)
genType :: Int -> Gen Type
genType n
  | n <= 0 = genTypeLeaf
  | otherwise =
      oneof
        [ genTypeLeaf,
          TApp span0 <$> genType half <*> genType half,
          TFun span0 <$> genType half <*> genType half,
          TList span0 Unpromoted <$> genTypeListElems (n - 1),
          TTuple span0 Boxed Unpromoted <$> genTypeTupleElems (n - 1),
          TParen span0 <$> genType (n - 1)
        ]
  where
    half = n `div` 2

genTypeLeaf :: Gen Type
genTypeLeaf =
  oneof
    [ TVar span0 <$> genTypeVarName,
      (\name -> TCon span0 name Unpromoted) <$> genConAstName
    ]

genTypeTupleElems :: Int -> Gen [Type]
genTypeTupleElems n = do
  isUnit <- arbitrary
  if isUnit
    then pure []
    else do
      count <- chooseInt (2, 3)
      vectorOf count (genType (n `div` count))

genTypeListElems :: Int -> Gen [Type]
genTypeListElems n = do
  count <- chooseInt (1, 4)
  vectorOf count (genType (n `div` count))

genTypeVarName :: Gen UnqualifiedName
genTypeVarName = do
  first <- elements ['a' .. 'z']
  restLen <- chooseInt (0, 3)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['0' .. '9']))
  let candidate = T.pack (first : rest)
  if isReservedIdentifier candidate || candidate `elem` extensionReservedIdentifiers
    then genTypeVarName
    else pure (mkUnqualifiedName NameVarId candidate)

-- | Literal expression constructors
mkHexExpr :: Integer -> Expr
mkHexExpr value = EIntBase span0 value ("0x" <> T.pack (showHex value))

mkFloatExpr :: Double -> Expr
mkFloatExpr value = EFloat span0 value (T.pack (show value))

mkCharExpr :: Char -> Expr
mkCharExpr value = EChar span0 value (T.pack (show value))

mkStringExpr :: Text -> Expr
mkStringExpr value = EString span0 value (T.pack (show (T.unpack value)))

genTenths :: Gen Double
genTenths = do
  whole <- chooseInteger (0, 99)
  frac <- chooseInteger (0, 9)
  pure (fromInteger whole + fromInteger frac / 10)

genCharValue :: Gen Char
genCharValue = elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> " _")

genStringValue :: Gen Text
genStringValue = do
  len <- chooseInt (0, 8)
  T.pack <$> vectorOf len genCharValue

showHex :: Integer -> String
showHex value
  | value < 16 = [hexDigit value]
  | otherwise = showHex (value `div` 16) <> [hexDigit (value `mod` 16)]
  where
    hexDigit x = "0123456789abcdef" !! fromInteger x

renderIntBaseHash :: Integer -> Text
renderIntBaseHash value
  | value < 0 = "-0x" <> T.pack (showHex (abs value)) <> "#"
  | otherwise = "0x" <> T.pack (showHex value) <> "#"

shrinkOverloadedLabel :: Text -> Text -> [String]
shrinkOverloadedLabel value raw
  | Just unquoted <- T.stripPrefix "#" raw,
    not ("\"" `T.isPrefixOf` unquoted) =
      [shrunk | shrunk <- shrink (T.unpack value), not (null shrunk), T.all isUnquotedLabelChar (T.pack shrunk)]
  | otherwise = []
  where
    isUnquotedLabelChar c =
      not (isSpace c) && c `notElem` ("()[]{},;`#\"" :: String)

-- | Create an integer expression with canonical representation.
mkIntExpr :: Integer -> Expr
mkIntExpr value = EInt span0 value (T.pack (show value))

-- | Shrink an expression for QuickCheck counterexample minimization.
shrinkExpr :: Expr -> [Expr]
shrinkExpr expr =
  case expr of
    EVar _ name -> [EVar span0 (name {nameText = shrunk}) | shrunk <- shrinkIdent (nameText name)]
    EInt _ value _ -> [mkIntExpr shrunk | shrunk <- shrinkIntegral value]
    EIntHash _ value _ -> [EIntHash span0 shrunk (T.pack (show shrunk) <> "#") | shrunk <- shrinkIntegral value]
    EIntBase _ value _ -> [mkIntExpr shrunk | shrunk <- shrinkIntegral value]
    EIntBaseHash _ value _ -> [EIntBaseHash span0 shrunk (renderIntBaseHash shrunk) | shrunk <- shrinkIntegral value]
    EFloat _ value _ -> [mkFloatExpr shrunk | shrunk <- shrinkFloat value]
    EFloatHash _ value _ -> [EFloatHash span0 shrunk (T.pack (show shrunk) <> "#") | shrunk <- shrinkFloat value]
    EChar {} -> []
    ECharHash {} -> []
    EString _ value _ -> [mkStringExpr (T.pack shrunk) | shrunk <- shrink (T.unpack value)]
    EStringHash _ value _ -> [EStringHash span0 (T.pack shrunk) (T.pack (show shrunk) <> "#") | shrunk <- shrink (T.unpack value)]
    EOverloadedLabel _ value raw ->
      [EOverloadedLabel span0 (T.pack shrunk) ("#" <> T.pack shrunk) | shrunk <- shrinkOverloadedLabel value raw]
    EQuasiQuote _ quoter body ->
      [EQuasiQuote span0 quoter (T.pack shrunk) | shrunk <- shrink (T.unpack body)]
    EApp _ fn arg ->
      [fn, arg]
        <> [EApp span0 fn' arg | fn' <- shrinkExpr fn]
        <> [EApp span0 fn arg' | arg' <- shrinkExpr arg]
    EInfix _ lhs op rhs ->
      [lhs, rhs]
        <> [EInfix span0 lhs' op rhs | lhs' <- shrinkExpr lhs]
        <> [EInfix span0 lhs op rhs' | rhs' <- shrinkExpr rhs]
    ENegate _ inner -> inner : [ENegate span0 inner' | inner' <- shrinkExpr inner]
    ESectionL _ inner op -> inner : [ESectionL span0 inner' op | inner' <- shrinkExpr inner]
    ESectionR _ op inner -> inner : [ESectionR span0 op inner' | inner' <- shrinkExpr inner]
    EIf _ cond thenE elseE ->
      [thenE, elseE]
        <> [EIf span0 cond' thenE elseE | cond' <- shrinkExpr cond]
        <> [EIf span0 cond thenE' elseE | thenE' <- shrinkExpr thenE]
        <> [EIf span0 cond thenE elseE' | elseE' <- shrinkExpr elseE]
    EMultiWayIf _ rhss ->
      [EMultiWayIf span0 rhss' | rhss' <- shrinkList shrinkGuardedRhs rhss, not (null rhss')]
    ECase _ scrutinee alts ->
      scrutinee
        : [ECase span0 scrutinee' alts | scrutinee' <- shrinkExpr scrutinee]
          <> [ECase span0 scrutinee alts' | alts' <- shrinkCaseAlts alts, not (null alts')]
    ELambdaPats _ pats body ->
      body : [ELambdaPats span0 pats body' | body' <- shrinkExpr body]
    ELambdaCase _ alts ->
      [ELambdaCase span0 alts' | alts' <- shrinkCaseAlts alts, not (null alts')]
    ELetDecls _ decls body ->
      body
        : [ELetDecls span0 decls body' | body' <- shrinkExpr body]
          <> [ELetDecls span0 decls' body | decls' <- shrinkDecls decls, not (null decls')]
    EWhereDecls _ body decls ->
      body
        : [EWhereDecls span0 body' decls | body' <- shrinkExpr body]
          <> [EWhereDecls span0 body decls' | decls' <- shrinkDecls decls, not (null decls')]
    EDo _ stmts _ ->
      [EDo span0 stmts' False | stmts' <- shrinkDoStmts stmts, not (null stmts')]
    EListComp _ body stmts ->
      body
        : [EListComp span0 body' stmts | body' <- shrinkExpr body]
          <> [EListComp span0 body stmts' | stmts' <- shrinkCompStmts stmts, not (null stmts')]
    EListCompParallel _ body stmtss ->
      body
        : [EListCompParallel span0 body' stmtss | body' <- shrinkExpr body]
          -- Each branch needs at least one statement, so filter out empty branches
          <> [EListCompParallel span0 body stmtss' | stmtss' <- shrinkParallelCompStmts stmtss, length stmtss' >= 2]
    EList _ elems ->
      [EList span0 elems' | elems' <- shrinkList shrinkExpr elems]
    ETuple _ tupleFlavor elems ->
      [ETuple span0 tupleFlavor elems' | elems' <- shrinkTupleMaybeElems shrinkMaybeExpr elems]
    EArithSeq _ seq' ->
      [EArithSeq span0 seq'' | seq'' <- shrinkArithSeq seq']
    ERecordCon _ con fields _ ->
      [ERecordCon span0 con fields' False | fields' <- shrinkRecordFields fields]
    ERecordUpd _ target fields ->
      target
        : [ERecordUpd span0 target' fields | target' <- shrinkExpr target]
          <> [ERecordUpd span0 target fields' | fields' <- shrinkRecordFields fields]
    ETypeSig _ inner _ ->
      inner : [ETypeSig span0 inner' (TCon span0 (qualifyName Nothing (mkUnqualifiedName NameConId "T")) Unpromoted) | inner' <- shrinkExpr inner]
    ETypeApp _ inner _ ->
      inner : [ETypeApp span0 inner' (TCon span0 (qualifyName Nothing (mkUnqualifiedName NameConId "T")) Unpromoted) | inner' <- shrinkExpr inner]
    EUnboxedSum _ altIdx arity inner ->
      [EUnboxedSum span0 altIdx arity inner' | inner' <- shrinkExpr inner]
    EParen _ inner -> inner : [EParen span0 inner' | inner' <- shrinkExpr inner]
    ETHExpQuote _ body -> body : [ETHExpQuote span0 body' | body' <- shrinkExpr body]
    ETHTypedQuote _ body -> body : [ETHTypedQuote span0 body' | body' <- shrinkExpr body]
    ETHDeclQuote {} -> []
    ETHTypeQuote {} -> []
    ETHPatQuote {} -> []
    ETHNameQuote {} -> []
    ETHTypeNameQuote {} -> []
    ETHSplice _ body -> body : [ETHSplice span0 body' | body' <- shrinkExpr body]
    ETHTypedSplice _ body -> body : [ETHTypedSplice span0 body' | body' <- shrinkExpr body]
    EProc {} -> []
    EAnn _ sub -> shrinkExpr sub

shrinkFloat :: Double -> [Double]
shrinkFloat value =
  [fromInteger shrunk / 10 | shrunk <- shrinkIntegral (round (value * 10 :: Double) :: Integer), shrunk >= 0]

shrinkCaseAlts :: [CaseAlt] -> [[CaseAlt]]
shrinkCaseAlts = shrinkList shrinkCaseAlt

shrinkCaseAlt :: CaseAlt -> [CaseAlt]
shrinkCaseAlt alt =
  case caseAltRhs alt of
    UnguardedRhs _ expr ->
      [alt {caseAltRhs = UnguardedRhs span0 expr'} | expr' <- shrinkExpr expr]
    GuardedRhss _ rhss ->
      -- Shrink to unguarded using the first guard's body
      [alt {caseAltRhs = UnguardedRhs span0 (guardedRhsBody firstRhs)} | firstRhs : _ <- [rhss]]
        <> [alt {caseAltRhs = GuardedRhss span0 rhss'} | rhss' <- shrinkList shrinkGuardedRhs rhss, not (null rhss')]

shrinkGuardedRhs :: GuardedRhs -> [GuardedRhs]
shrinkGuardedRhs grhs =
  [grhs {guardedRhsBody = body'} | body' <- shrinkExpr (guardedRhsBody grhs)]

shrinkDecls :: [Decl] -> [[Decl]]
shrinkDecls = shrinkList shrinkLetDecl

shrinkLetDecl :: Decl -> [Decl]
shrinkLetDecl decl =
  case decl of
    DeclValue _ (PatternBind _ pat (UnguardedRhs _ expr)) ->
      [DeclValue span0 (PatternBind span0 pat (UnguardedRhs span0 expr')) | expr' <- shrinkExpr expr]
    DeclValue _ (FunctionBind _ name [match@Match {matchRhs = UnguardedRhs _ expr}]) ->
      [ DeclValue
          span0
          ( FunctionBind
              span0
              name
              [match {matchSpan = span0, matchRhs = UnguardedRhs span0 expr'}]
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
