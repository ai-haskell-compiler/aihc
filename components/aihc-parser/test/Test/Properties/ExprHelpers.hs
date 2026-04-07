{-# LANGUAGE OverloadedStrings #-}

-- | Shared helper functions for expression generation and normalization
-- used by both ExprRoundTrip and ModuleRoundTrip tests.
module Test.Properties.ExprHelpers
  ( genExpr,
    genOperator,
    isValidGeneratedOperator,
    mkIntExpr,
    shrinkExpr,
    normalizeExpr,
    span0,
  )
where

import Aihc.Parser.Lex (isReservedIdentifier)
import Aihc.Parser.Syntax
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Properties.Identifiers (genIdent, shrinkIdent)
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
          EInfix span0 <$> genExprSized half <*> genOperator <*> genExprSized half,
          ENegate span0 <$> genExprSized (n - 1),
          ESectionL span0 <$> genExprSized (n - 1) <*> genOperator,
          ESectionR span0 <$> genOperator <*> genExprSized (n - 1),
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
          ETHPatQuote span0 <$> genSimplePattern,
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
    [ EVar span0 <$> genIdent,
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
    [ EVar span0 <$> genIdent,
      EParen span0 <$> genExprSized (max 0 (n - 1))
    ]

-- | Generate the body of a TH typed splice: always parenthesized.
-- Typed splices require parentheses: $$(expr) is valid, $$expr is invalid.
genTypedSpliceBody :: Int -> Gen Expr
genTypedSpliceBody n =
  EParen span0 <$> genExprSized (max 0 (n - 1))

-- | Generate a simple pattern for TH pattern quotes [p| pat |].
-- Only generates patterns that don't require additional extensions.
genSimplePattern :: Gen Pattern
genSimplePattern =
  oneof
    [ PVar span0 <$> genIdent,
      pure (PWildcard span0),
      PCon span0 <$> genConName <*> pure []
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

-- | Generate simple patterns for lambdas
genPatterns :: Int -> Gen [Pattern]
genPatterns n = do
  count <- chooseInt (1, 3)
  vectorOf count (genPattern n)

-- | Generate a pattern
genPattern :: Int -> Gen Pattern
genPattern n
  | n <= 0 = genPatternLeaf
  | otherwise =
      oneof
        [ genPatternLeaf,
          PTuple span0 Boxed <$> genPatternTupleElems half,
          PList span0 <$> genPatternListElems half
        ]
  where
    half = n `div` 2

-- | Generate a leaf pattern
genPatternLeaf :: Gen Pattern
genPatternLeaf =
  oneof
    [ PVar span0 <$> genIdent,
      pure (PWildcard span0)
    ]

genPatternTupleElems :: Int -> Gen [Pattern]
genPatternTupleElems n = do
  isUnit <- arbitrary
  if isUnit
    then pure []
    else do
      count <- chooseInt (2, 3)
      vectorOf count (genPattern n)

genPatternListElems :: Int -> Gen [Pattern]
genPatternListElems n = do
  count <- chooseInt (0, 3)
  vectorOf count (genPattern n)

-- | Generate case alternatives
genCaseAlts :: Int -> Gen [CaseAlt]
genCaseAlts n = do
  count <- chooseInt (1, 3)
  vectorOf count (genCaseAlt n)

genCaseAlt :: Int -> Gen CaseAlt
genCaseAlt n = do
  pat <- genPattern half
  expr <- genExprSized half
  pure $
    CaseAlt
      { caseAltSpan = span0,
        caseAltPattern = pat,
        caseAltRhs = UnguardedRhs span0 expr
      }
  where
    half = n `div` 2

-- | Generate value declarations for let/where
-- We use FunctionBind format because that's what the parser produces for simple
-- variable bindings like `x = e`.
genValueDecls :: Int -> Gen [Decl]
genValueDecls n = do
  count <- chooseInt (1, 3)
  names <- vectorOf count genIdent
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
    [ CompGen span0 <$> genPattern half <*> genExprSized half,
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

-- | Generate elements for an unboxed tuple (always 2+ elements, no unit)
genUnboxedTupleElems :: Int -> Gen [Expr]
genUnboxedTupleElems n = do
  count <- chooseInt (2, 4)
  vectorOf count (genExprSized (n `div` count))

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

-- | Generate a type
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

-- | Generate a leaf type
genTypeLeaf :: Gen Type
genTypeLeaf =
  oneof
    [ TVar span0 <$> genTypeVarName,
      (\name -> TCon span0 name Unpromoted) <$> genConName
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

genTypeVarName :: Gen Text
genTypeVarName = do
  first <- elements ['a' .. 'z']
  restLen <- chooseInt (0, 3)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['0' .. '9']))
  let candidate = T.pack (first : rest)
  if isReservedIdentifier candidate
    then genTypeVarName
    else pure candidate

-- | Literal expression generators
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
    EVar _ name -> [EVar span0 shrunk | shrunk <- shrinkIdent name]
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
      [ETuple span0 tupleFlavor elems' | elems' <- shrinkTupleElems shrinkMaybeExpr elems]
    EArithSeq _ seq' ->
      [EArithSeq span0 seq'' | seq'' <- shrinkArithSeq seq']
    ERecordCon _ con fields _ ->
      [ERecordCon span0 con fields' False | fields' <- shrinkRecordFields fields]
    ERecordUpd _ target fields ->
      target
        : [ERecordUpd span0 target' fields | target' <- shrinkExpr target]
          <> [ERecordUpd span0 target fields' | fields' <- shrinkRecordFields fields]
    ETypeSig _ inner _ ->
      inner : [ETypeSig span0 inner' (TCon span0 "T" Unpromoted) | inner' <- shrinkExpr inner]
    ETypeApp _ inner _ ->
      inner : [ETypeApp span0 inner' (TCon span0 "T" Unpromoted) | inner' <- shrinkExpr inner]
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
    _ -> []

shrinkGuardedRhs :: GuardedRhs -> [GuardedRhs]
shrinkGuardedRhs grhs =
  [grhs {guardedRhsBody = body'} | body' <- shrinkExpr (guardedRhsBody grhs)]

shrinkDecls :: [Decl] -> [[Decl]]
shrinkDecls = shrinkList shrinkDecl

shrinkDecl :: Decl -> [Decl]
shrinkDecl decl =
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
    DoLet _ bindings -> [DoLet span0 bindings' | bindings' <- shrinkList shrinkBinding bindings, not (null bindings')]
    DoLetDecls _ decls -> [DoLetDecls span0 decls' | decls' <- shrinkDecls decls, not (null decls')]
    DoExpr _ expr -> [DoExpr span0 expr' | expr' <- shrinkExpr expr]
    DoRecStmt _ stmts -> [DoRecStmt span0 stmts' | stmts' <- shrinkDoStmts stmts, not (null stmts')]

shrinkBinding :: (Text, Expr) -> [(Text, Expr)]
shrinkBinding (name, expr) = [(name, expr') | expr' <- shrinkExpr expr]

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

shrinkTupleElems :: (a -> [a]) -> [a] -> [[a]]
shrinkTupleElems shrinkElem elems =
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

-- | Normalize an expression to canonical form for comparison.
-- Removes source spans and simplifies parentheses.
normalizeExpr :: Expr -> Expr
normalizeExpr expr =
  case expr of
    EVar _ name -> EVar span0 name
    EInt _ value _ -> mkIntExpr value
    EIntHash _ value repr -> EIntHash span0 value repr
    EIntBase _ value repr -> EIntBase span0 value repr
    EIntBaseHash _ value repr -> EIntBaseHash span0 value repr
    EFloat _ value repr -> EFloat span0 value repr
    EFloatHash _ value repr -> EFloatHash span0 value repr
    EChar _ value repr -> EChar span0 value repr
    ECharHash _ value repr -> ECharHash span0 value repr
    EString _ value repr -> EString span0 value repr
    EStringHash _ value repr -> EStringHash span0 value repr
    EOverloadedLabel _ value repr -> EOverloadedLabel span0 value repr
    EQuasiQuote _ quoter body -> EQuasiQuote span0 quoter body
    EApp _ fn arg -> EApp span0 (normalizeExpr fn) (normalizeExpr arg)
    EInfix _ lhs op rhs -> EInfix span0 (normalizeExpr lhs) op (normalizeExpr rhs)
    ENegate _ inner -> ENegate span0 (normalizeExpr inner)
    ESectionL _ inner op -> ESectionL span0 (normalizeExpr inner) op
    ESectionR _ op inner -> ESectionR span0 op (normalizeExpr inner)
    EIf _ cond thenE elseE -> EIf span0 (normalizeExpr cond) (normalizeExpr thenE) (normalizeExpr elseE)
    EMultiWayIf _ rhss -> EMultiWayIf span0 (map normalizeGuardedRhs rhss)
    ECase _ scrutinee alts -> ECase span0 (normalizeExpr scrutinee) (map normalizeCaseAlt alts)
    ELambdaPats _ pats body -> ELambdaPats span0 (map normalizePattern pats) (normalizeExpr body)
    ELambdaCase _ alts -> ELambdaCase span0 (map normalizeCaseAlt alts)
    ELetDecls _ decls body -> ELetDecls span0 (map normalizeDecl decls) (normalizeExpr body)
    EWhereDecls _ body decls -> EWhereDecls span0 (normalizeExpr body) (map normalizeDecl decls)
    EDo _ stmts isMdo -> EDo span0 (map normalizeDoStmt stmts) isMdo
    EListComp _ body stmts -> EListComp span0 (normalizeExpr body) (map normalizeCompStmt stmts)
    EListCompParallel _ body stmtss -> EListCompParallel span0 (normalizeExpr body) (map (map normalizeCompStmt) stmtss)
    EList _ elems -> EList span0 (map normalizeExpr elems)
    ETuple _ tupleFlavor elems -> ETuple span0 tupleFlavor (map (fmap normalizeExpr) elems)
    EArithSeq _ seq' -> EArithSeq span0 (normalizeArithSeq seq')
    ERecordCon _ con fields rwc -> ERecordCon span0 con [(name, normalizeExpr e) | (name, e) <- fields] rwc
    ERecordUpd _ target fields -> ERecordUpd span0 (normalizeExpr target) [(name, normalizeExpr e) | (name, e) <- fields]
    ETypeSig _ inner ty -> ETypeSig span0 (normalizeExpr inner) (normalizeType ty)
    ETypeApp _ inner ty -> ETypeApp span0 (normalizeExpr inner) (normalizeType ty)
    EUnboxedSum _ altIdx arity inner -> EUnboxedSum span0 altIdx arity (normalizeExpr inner)
    EParen _ inner -> normalizeExpr inner
    ETHExpQuote _ body -> ETHExpQuote span0 (normalizeExpr body)
    ETHTypedQuote _ body -> ETHTypedQuote span0 (normalizeExpr body)
    ETHDeclQuote _ decls -> ETHDeclQuote span0 (map normalizeDecl decls)
    ETHTypeQuote _ ty -> ETHTypeQuote span0 (normalizeType ty)
    ETHPatQuote _ pat -> ETHPatQuote span0 (normalizePattern pat)
    ETHNameQuote _ name -> ETHNameQuote span0 name
    ETHTypeNameQuote _ name -> ETHTypeNameQuote span0 name
    ETHSplice _ body -> ETHSplice span0 (normalizeExpr body)
    ETHTypedSplice _ body -> ETHTypedSplice span0 (normalizeExpr body)
    EProc _ pat body -> EProc span0 (normalizePattern pat) body
    EAnn ann sub -> EAnn ann (normalizeExpr sub)

normalizeCaseAlt :: CaseAlt -> CaseAlt
normalizeCaseAlt alt =
  CaseAlt
    { caseAltSpan = span0,
      caseAltPattern = normalizePattern (caseAltPattern alt),
      caseAltRhs = normalizeRhs (caseAltRhs alt)
    }

normalizeRhs :: Rhs -> Rhs
normalizeRhs rhs =
  case rhs of
    UnguardedRhs _ body -> UnguardedRhs span0 (normalizeExpr body)
    GuardedRhss _ guards -> GuardedRhss span0 (map normalizeGuardedRhs guards)

normalizeGuardedRhs :: GuardedRhs -> GuardedRhs
normalizeGuardedRhs grhs =
  GuardedRhs
    { guardedRhsSpan = span0,
      guardedRhsGuards = map normalizeGuardQualifier (guardedRhsGuards grhs),
      guardedRhsBody = normalizeExpr (guardedRhsBody grhs)
    }

normalizeGuardQualifier :: GuardQualifier -> GuardQualifier
normalizeGuardQualifier qual =
  case qual of
    GuardExpr _ e -> GuardExpr span0 (normalizeExpr e)
    GuardPat _ pat e -> GuardPat span0 (normalizePattern pat) (normalizeExpr e)
    GuardLet _ decls -> GuardLet span0 (map normalizeDecl decls)

normalizePattern :: Pattern -> Pattern
normalizePattern pat =
  case pat of
    PAnn _ sub -> normalizePattern sub
    PVar _ name -> PVar span0 name
    PWildcard _ -> PWildcard span0
    PLit _ lit -> PLit span0 (normalizeLiteral lit)
    PQuasiQuote _ quoter body -> PQuasiQuote span0 quoter body
    PTuple _ tupleFlavor elems -> PTuple span0 tupleFlavor (map normalizePattern elems)
    PList _ elems -> PList span0 (map normalizePattern elems)
    PCon _ con args -> PCon span0 con (map normalizePattern args)
    PInfix _ lhs op rhs -> PInfix span0 (normalizePattern lhs) op (normalizePattern rhs)
    PView _ e inner -> PView span0 (normalizeExpr e) (normalizePattern inner)
    PAs _ name inner -> PAs span0 name (normalizePattern inner)
    PStrict _ inner -> PStrict span0 (normalizePattern inner)
    PIrrefutable _ inner -> PIrrefutable span0 (normalizePattern inner)
    PNegLit _ lit -> PNegLit span0 (normalizeLiteral lit)
    PParen _ inner -> PParen span0 (normalizePattern inner)
    PUnboxedSum _ altIdx arity inner -> PUnboxedSum span0 altIdx arity (normalizePattern inner)
    PRecord _ con fields rwc -> PRecord span0 con [(name, normalizePattern p) | (name, p) <- fields] rwc
    PTypeSig _ inner ty -> PTypeSig span0 (normalizePattern inner) (normalizeType ty)
    PSplice _ body -> PSplice span0 (normalizeExpr body)

normalizeLiteral :: Literal -> Literal
normalizeLiteral lit =
  case lit of
    LitInt _ value repr -> LitInt span0 value repr
    LitIntHash _ value repr -> LitIntHash span0 value repr
    LitIntBase _ value repr -> LitIntBase span0 value repr
    LitIntBaseHash _ value repr -> LitIntBaseHash span0 value repr
    LitFloat _ value repr -> LitFloat span0 value repr
    LitFloatHash _ value repr -> LitFloatHash span0 value repr
    LitChar _ value repr -> LitChar span0 value repr
    LitCharHash _ value repr -> LitCharHash span0 value repr
    LitString _ value repr -> LitString span0 value repr
    LitStringHash _ value repr -> LitStringHash span0 value repr

normalizeDecl :: Decl -> Decl
normalizeDecl decl =
  case decl of
    DeclValue _ vdecl -> DeclValue span0 (normalizeValueDecl vdecl)
    DeclTypeSig _ names ty -> DeclTypeSig span0 names (normalizeType ty)
    _ -> decl

normalizeValueDecl :: ValueDecl -> ValueDecl
normalizeValueDecl vdecl =
  case vdecl of
    PatternBind _ pat rhs -> PatternBind span0 (normalizePattern pat) (normalizeRhs rhs)
    FunctionBind _ name matches -> FunctionBind span0 name (map normalizeMatch matches)

normalizeMatch :: Match -> Match
normalizeMatch m =
  Match
    { matchSpan = span0,
      matchHeadForm = matchHeadForm m,
      matchPats = map normalizePattern (matchPats m),
      matchRhs = normalizeRhs (matchRhs m)
    }

normalizeDoStmt :: DoStmt Expr -> DoStmt Expr
normalizeDoStmt stmt =
  case stmt of
    DoBind _ pat e -> DoBind span0 (normalizePattern pat) (normalizeExpr e)
    DoLet _ bindings -> DoLet span0 [(name, normalizeExpr e) | (name, e) <- bindings]
    DoLetDecls _ decls -> DoLetDecls span0 (map normalizeDecl decls)
    DoExpr _ e -> DoExpr span0 (normalizeExpr e)
    DoRecStmt _ stmts -> DoRecStmt span0 (map normalizeDoStmt stmts)

normalizeCompStmt :: CompStmt -> CompStmt
normalizeCompStmt stmt =
  case stmt of
    CompGen _ pat e -> CompGen span0 (normalizePattern pat) (normalizeExpr e)
    CompGuard _ e -> CompGuard span0 (normalizeExpr e)
    CompLet _ bindings -> CompLet span0 [(name, normalizeExpr e) | (name, e) <- bindings]
    CompLetDecls _ decls -> CompLetDecls span0 (map normalizeDecl decls)

normalizeArithSeq :: ArithSeq -> ArithSeq
normalizeArithSeq seq' =
  case seq' of
    ArithSeqFrom _ from -> ArithSeqFrom span0 (normalizeExpr from)
    ArithSeqFromThen _ from thenE -> ArithSeqFromThen span0 (normalizeExpr from) (normalizeExpr thenE)
    ArithSeqFromTo _ from to -> ArithSeqFromTo span0 (normalizeExpr from) (normalizeExpr to)
    ArithSeqFromThenTo _ from thenE to -> ArithSeqFromThenTo span0 (normalizeExpr from) (normalizeExpr thenE) (normalizeExpr to)

normalizeType :: Type -> Type
normalizeType ty =
  case ty of
    TVar _ name -> TVar span0 name
    TCon _ name promoted -> TCon span0 name promoted
    TTypeLit _ lit -> TTypeLit span0 lit
    TStar _ -> TStar span0
    TQuasiQuote _ quoter body -> TQuasiQuote span0 quoter body
    TForall _ binders inner -> TForall span0 binders (normalizeType inner)
    TApp _ fn arg -> TApp span0 (normalizeType fn) (normalizeType arg)
    TFun _ lhs rhs -> TFun span0 (normalizeType lhs) (normalizeType rhs)
    TTuple _ tupleFlavor promoted elems -> TTuple span0 tupleFlavor promoted (map normalizeType elems)
    TList _ promoted elems -> TList span0 promoted (map normalizeType elems)
    -- Remove redundant parentheses from types
    TParen _ inner -> normalizeType inner
    TKindSig _ inner kind -> TKindSig span0 (normalizeType inner) (normalizeType kind)
    TUnboxedSum _ elems -> TUnboxedSum span0 (map normalizeType elems)
    TContext _ constraints inner -> TContext span0 (map normalizeConstraint constraints) (normalizeType inner)
    TSplice _ body -> TSplice span0 (normalizeExpr body)
    TWildcard _ -> TWildcard span0
    TAnn ann sub -> TAnn ann (normalizeType sub)

normalizeConstraint :: Constraint -> Constraint
normalizeConstraint c =
  case c of
    Constraint _ cls args ->
      Constraint
        { constraintSpan = span0,
          constraintClass = cls,
          constraintArgs = map normalizeType args
        }
    CParen _ inner ->
      CParen span0 (normalizeConstraint inner)
    CWildcard _ ->
      CWildcard span0
    CKindSig _ ty ->
      CKindSig span0 (normalizeType ty)
