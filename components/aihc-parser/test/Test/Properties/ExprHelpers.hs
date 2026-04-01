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
import Data.Text (Text)
import qualified Data.Text as T
import Test.Properties.BareSyntax
import Test.Properties.Identifiers (genIdent, shrinkIdent)
import Test.QuickCheck

-- | Canonical empty source span for normalization.
span0 :: ()
span0 = ()

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
          EApp <$> genExprSized half <*> genExprSized half,
          EInfix <$> genExprSized half <*> genOperator <*> genExprSized half,
          ENegate <$> genNegateOperand (n - 1),
          ESectionL <$> genExprSized (n - 1) <*> genOperator,
          ESectionR <$> genOperator <*> genExprSized (n - 1),
          EIf <$> genExprSized third <*> genExprSized third <*> genExprSized third,
          ECase <$> genExprSized half <*> genCaseAlts half,
          ELambdaPats <$> genPatterns half <*> genExprSized half,
          ELambdaCase <$> genCaseAlts (n - 1),
          ELetDecls <$> genValueDecls half <*> genExprSized half,
          EWhereDecls <$> genExprSized half <*> genValueDecls half,
          EDo <$> genDoStmts (n - 1),
          EListComp <$> genExprSized half <*> genCompStmts half,
          EListCompParallel <$> genExprSized half <*> genParallelCompStmts half,
          EList <$> genListElems (n - 1),
          ETuple Boxed <$> genTupleElems (n - 1),
          ETuple Unboxed <$> genUnboxedTupleElems (n - 1),
          ETupleSection Boxed <$> genTupleSectionElems (n - 1),
          genUnboxedSumExpr (n - 1),
          EArithSeq <$> genArithSeq (n - 1),
          ERecordCon <$> genConName <*> genRecordFields (n - 1) <*> pure False,
          ERecordUpd <$> genExprSized half <*> genRecordFields half,
          ETypeSig <$> genExprSized half <*> genType half,
          EParen <$> genExprSized (n - 1),
          -- Template Haskell splices and quotes
          ETHSplice <$> genSpliceBody (n - 1),
          ETHTypedSplice <$> genTypedSpliceBody (n - 1),
          ETHExpQuote <$> genExprSized (n - 1),
          ETHTypedQuote <$> genExprSized (n - 1),
          ETHDeclQuote <$> genValueDecls (n - 1),
          ETHPatQuote <$> genSimplePattern,
          ETHTypeQuote <$> genType (n - 1),
          ETHNameQuote <$> genNameQuoteIdent,
          ETHTypeNameQuote <$> genConName
        ]
  where
    half = n `div` 2
    third = n `div` 3

-- | Generate a leaf (non-recursive) expression.
genExprLeaf :: Gen Expr
genExprLeaf =
  oneof
    [ EVar <$> genIdent,
      mkIntExpr <$> chooseInteger (0, 999),
      mkHexExpr <$> chooseInteger (0, 255),
      mkFloatExpr <$> genTenths,
      mkCharExpr <$> genCharValue,
      mkStringExpr <$> genStringValue,
      -- Note: EQuasiQuote requires QuasiQuotes extension, skip for now
      pure (EList []),
      pure (ETuple Boxed []),
      pure (ETuple Unboxed []),
      ETupleCon Boxed <$> chooseInt (2, 5),
      ETupleCon Unboxed <$> chooseInt (2, 5)
    ]

-- | Generate an operand that the current pretty-printer can render
-- unambiguously after a leading minus.
genNegateOperand :: Int -> Gen Expr
genNegateOperand n =
  oneof
    [ genExprLeaf,
      EParen <$> genExprSized (max 0 n)
    ]

-- | Generate the body of a TH splice: either a bare variable or a parenthesized expression.
-- Bare variables produce $name syntax; parenthesized produce $(expr) syntax.
genSpliceBody :: Int -> Gen Expr
genSpliceBody n =
  oneof
    [ EVar <$> genIdent,
      EParen <$> genExprSized (max 0 (n - 1))
    ]

-- | Generate the body of a TH typed splice: always parenthesized.
-- Typed splices require parentheses: $$(expr) is valid, $$expr is invalid.
genTypedSpliceBody :: Int -> Gen Expr
genTypedSpliceBody n =
  EParen <$> genExprSized (max 0 (n - 1))

-- | Generate a simple pattern for TH pattern quotes [p| pat |].
-- Only generates patterns that don't require additional extensions.
genSimplePattern :: Gen Pattern
genSimplePattern =
  oneof
    [ PVar <$> genIdent,
      pure PWildcard,
      PCon <$> genConName <*> pure []
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
  let reserved = candidate `elem` ["..", "::", "=", "\\", "|", "<-", "->", "~", "=>", "--"]
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
          PTuple Boxed <$> genPatternTupleElems half,
          PList <$> genPatternListElems half
        ]
  where
    half = n `div` 2

-- | Generate a leaf pattern
genPatternLeaf :: Gen Pattern
genPatternLeaf =
  oneof
    [ PVar <$> genIdent,
      pure PWildcard
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
      { caseAltPattern = pat,
        caseAltRhs = UnguardedRhs expr
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
        ( FunctionBind
            name
            [ Match
                { matchHeadForm = MatchHeadPrefix,
                  matchPats = [],
                  matchRhs = UnguardedRhs expr
                }
            ]
        )
    | (name, expr) <- zip names exprs
    ]

-- | Generate do statements
genDoStmts :: Int -> Gen [DoStmt]
genDoStmts n = do
  count <- chooseInt (1, 3)
  let perStmt = n `div` count
  stmts <- vectorOf (count - 1) (genDoStmt perStmt)
  -- Last statement must be DoExpr
  lastExpr <- genExprSized perStmt
  pure (stmts <> [DoExpr lastExpr])

genDoStmt :: Int -> Gen DoStmt
genDoStmt n =
  oneof
    [ DoBind <$> genPattern half <*> genExprSized half,
      DoLetDecls <$> genValueDecls (n - 1),
      DoExpr <$> genExprSized (n - 1)
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
    [ CompGen <$> genPattern half <*> genExprSized half,
      CompGuard <$> genExprSized (n - 1)
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
  pure (EUnboxedSum altIdx arity inner)

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
    [ ArithSeqFrom <$> genExprSized n,
      ArithSeqFromThen <$> genExprSized half <*> genExprSized half,
      ArithSeqFromTo <$> genExprSized half <*> genExprSized half,
      ArithSeqFromThenTo <$> genExprSized third <*> genExprSized third <*> genExprSized third
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
          TApp <$> genType half <*> genType half,
          TFun <$> genType half <*> genType half,
          TList Unpromoted <$> genType (n - 1),
          TTuple Boxed Unpromoted <$> genTypeTupleElems (n - 1),
          TParen <$> genType (n - 1)
        ]
  where
    half = n `div` 2

-- | Generate a leaf type
genTypeLeaf :: Gen Type
genTypeLeaf =
  oneof
    [ TVar <$> genTypeVarName,
      (`TCon` Unpromoted) <$> genConName
    ]

genTypeTupleElems :: Int -> Gen [Type]
genTypeTupleElems n = do
  isUnit <- arbitrary
  if isUnit
    then pure []
    else do
      count <- chooseInt (2, 3)
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
mkHexExpr value = EIntBase value ("0x" <> T.pack (showHex value))

mkFloatExpr :: Double -> Expr
mkFloatExpr value = EFloat value (T.pack (show value))

mkCharExpr :: Char -> Expr
mkCharExpr value = EChar value (T.pack (show value))

mkStringExpr :: Text -> Expr
mkStringExpr value = EString value (T.pack (show (T.unpack value)))

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

-- | Create an integer expression with canonical representation.
mkIntExpr :: Integer -> Expr
mkIntExpr value = EInt value (T.pack (show value))

-- | Shrink an expression for QuickCheck counterexample minimization.
shrinkExpr :: Expr -> [Expr]
shrinkExpr expr =
  case expr of
    EVar name -> [EVar shrunk | shrunk <- shrinkIdent name]
    EInt value _ -> [mkIntExpr shrunk | shrunk <- shrinkIntegral value]
    EIntHash value _ -> [EIntHash shrunk (T.pack (show shrunk) <> "#") | shrunk <- shrinkIntegral value]
    EIntBase value _ -> [mkIntExpr shrunk | shrunk <- shrinkIntegral value]
    EIntBaseHash value _ -> [EIntBaseHash shrunk (renderIntBaseHash shrunk) | shrunk <- shrinkIntegral value]
    EFloat value _ -> [mkFloatExpr shrunk | shrunk <- shrinkFloat value]
    EFloatHash value _ -> [EFloatHash shrunk (T.pack (show shrunk) <> "#") | shrunk <- shrinkFloat value]
    EChar {} -> []
    ECharHash {} -> []
    EString value _ -> [mkStringExpr (T.pack shrunk) | shrunk <- shrink (T.unpack value)]
    EStringHash value _ -> [EStringHash (T.pack shrunk) (T.pack (show shrunk) <> "#") | shrunk <- shrink (T.unpack value)]
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
    ELetDecls decls body ->
      body
        : [ELetDecls decls body' | body' <- shrinkExpr body]
          <> [ELetDecls decls' body | decls' <- shrinkDecls decls, not (null decls')]
    EWhereDecls body decls ->
      body
        : [EWhereDecls body' decls | body' <- shrinkExpr body]
          <> [EWhereDecls body decls' | decls' <- shrinkDecls decls, not (null decls')]
    EDo stmts ->
      [EDo stmts' | stmts' <- shrinkDoStmts stmts, not (null stmts')]
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
      [ETuple tupleFlavor elems' | elems' <- shrinkTupleElems shrinkExpr elems]
    ETupleSection tupleFlavor elems ->
      [ETupleSection tupleFlavor elems' | elems' <- shrinkTupleSectionElems elems]
    ETupleCon tupleFlavor n -> [ETupleCon tupleFlavor n' | n' <- shrink n, n' >= 2]
    EArithSeq seq' ->
      [EArithSeq seq'' | seq'' <- shrinkArithSeq seq']
    ERecordCon con fields _ ->
      [ERecordCon con fields' False | fields' <- shrinkRecordFields fields]
    ERecordUpd target fields ->
      target
        : [ERecordUpd target' fields | target' <- shrinkExpr target]
          <> [ERecordUpd target fields' | fields' <- shrinkRecordFields fields]
    ETypeSig inner _ ->
      inner : [ETypeSig inner' (TCon "T" Unpromoted) | inner' <- shrinkExpr inner]
    ETypeApp inner _ ->
      inner : [ETypeApp inner' (TCon "T" Unpromoted) | inner' <- shrinkExpr inner]
    EUnboxedSum altIdx arity inner ->
      [EUnboxedSum altIdx arity inner' | inner' <- shrinkExpr inner]
    EParen inner -> inner : [EParen inner' | inner' <- shrinkExpr inner]
    ETHExpQuote body -> body : [ETHExpQuote body' | body' <- shrinkExpr body]
    ETHTypedQuote body -> body : [ETHTypedQuote body' | body' <- shrinkExpr body]
    ETHDeclQuote {} -> []
    ETHTypeQuote {} -> []
    ETHPatQuote {} -> []
    ETHNameQuote {} -> []
    ETHTypeNameQuote {} -> []
    ETHSplice body -> body : [ETHSplice body' | body' <- shrinkExpr body]
    ETHTypedSplice body -> body : [ETHTypedSplice body' | body' <- shrinkExpr body]

shrinkFloat :: Double -> [Double]
shrinkFloat value =
  [fromInteger shrunk / 10 | shrunk <- shrinkIntegral (round (value * 10 :: Double) :: Integer), shrunk >= 0]

shrinkCaseAlts :: [CaseAlt] -> [[CaseAlt]]
shrinkCaseAlts = shrinkList shrinkCaseAlt

shrinkCaseAlt :: CaseAlt -> [CaseAlt]
shrinkCaseAlt alt =
  case caseAltRhs alt of
    UnguardedRhs expr ->
      [alt {caseAltRhs = UnguardedRhs expr'} | expr' <- shrinkExpr expr]
    _ -> []

shrinkGuardedRhs :: GuardedRhs -> [GuardedRhs]
shrinkGuardedRhs grhs =
  [grhs {guardedRhsBody = body'} | body' <- shrinkExpr (guardedRhsBody grhs)]

shrinkDecls :: [Decl] -> [[Decl]]
shrinkDecls = shrinkList shrinkDecl

shrinkDecl :: Decl -> [Decl]
shrinkDecl decl =
  case decl of
    DeclValue (PatternBind pat (UnguardedRhs expr)) ->
      [DeclValue (PatternBind pat (UnguardedRhs expr')) | expr' <- shrinkExpr expr]
    DeclValue (FunctionBind name [match@Match {matchRhs = UnguardedRhs expr}]) ->
      [ DeclValue
          ( FunctionBind
              name
              [match {matchRhs = UnguardedRhs expr'}]
          )
      | expr' <- shrinkExpr expr
      ]
    _ -> []

shrinkDoStmts :: [DoStmt] -> [[DoStmt]]
shrinkDoStmts stmts =
  case stmts of
    [_] -> [] -- Can't shrink a single-element do block
    _ -> shrinkList shrinkDoStmt stmts

shrinkDoStmt :: DoStmt -> [DoStmt]
shrinkDoStmt stmt =
  case stmt of
    DoBind pat expr -> [DoBind pat expr' | expr' <- shrinkExpr expr]
    DoLet bindings -> [DoLet bindings' | bindings' <- shrinkList shrinkBinding bindings, not (null bindings')]
    DoLetDecls decls -> [DoLetDecls decls' | decls' <- shrinkDecls decls, not (null decls')]
    DoExpr expr -> [DoExpr expr' | expr' <- shrinkExpr expr]

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
    CompGen pat expr -> [CompGen pat expr' | expr' <- shrinkExpr expr]
    CompGuard expr -> [CompGuard expr' | expr' <- shrinkExpr expr]
    CompLet bindings -> [CompLet bindings' | bindings' <- shrinkList shrinkBinding bindings, not (null bindings')]
    CompLetDecls decls -> [CompLetDecls decls' | decls' <- shrinkDecls decls, not (null decls')]

shrinkTupleElems :: (a -> [a]) -> [a] -> [[a]]
shrinkTupleElems shrinkElem elems =
  case elems of
    [] -> [] -- Unit can't shrink
    _ ->
      [elems' | elems' <- shrinkList shrinkElem elems, length elems' /= 1]

shrinkTupleSectionElems :: [Maybe Expr] -> [[Maybe Expr]]
shrinkTupleSectionElems elems =
  [ elems'
  | elems' <- shrinkList shrinkMaybeExpr elems,
    length elems' >= 2,
    Nothing `elem` elems' -- Must have at least one hole
  ]

shrinkMaybeExpr :: Maybe Expr -> [Maybe Expr]
shrinkMaybeExpr mExpr =
  case mExpr of
    Nothing -> []
    Just expr -> Nothing : [Just expr' | expr' <- shrinkExpr expr]

shrinkArithSeq :: ArithSeq -> [ArithSeq]
shrinkArithSeq seq' =
  case seq' of
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

shrinkRecordFields :: [(Text, Expr)] -> [[(Text, Expr)]]
shrinkRecordFields = shrinkList shrinkRecordField

shrinkRecordField :: (Text, Expr) -> [(Text, Expr)]
shrinkRecordField (name, expr) = [(name, expr') | expr' <- shrinkExpr expr]

-- | Normalize an expression to canonical form for comparison.
-- Removes source spans and simplifies parentheses.
normalizeExpr :: Expr -> Expr
normalizeExpr expr =
  case expr of
    EVar name -> EVar name
    EInt value _ -> mkIntExpr value
    EIntHash value repr -> EIntHash value repr
    EIntBase value repr -> EIntBase value repr
    EIntBaseHash value repr -> EIntBaseHash value repr
    EFloat value repr -> EFloat value repr
    EFloatHash value repr -> EFloatHash value repr
    EChar value repr -> EChar value repr
    ECharHash value repr -> ECharHash value repr
    EString value repr -> EString value repr
    EStringHash value repr -> EStringHash value repr
    EQuasiQuote quoter body -> EQuasiQuote quoter body
    EApp fn arg -> EApp (normalizeExpr fn) (normalizeExpr arg)
    EInfix lhs op rhs -> EInfix (normalizeExpr lhs) op (normalizeExpr rhs)
    ENegate inner -> ENegate (normalizeExpr inner)
    ESectionL inner op -> ESectionL (normalizeExpr inner) op
    ESectionR op inner -> ESectionR op (normalizeExpr inner)
    EIf cond thenE elseE -> EIf (normalizeExpr cond) (normalizeExpr thenE) (normalizeExpr elseE)
    EMultiWayIf rhss -> EMultiWayIf (map normalizeGuardedRhs rhss)
    ECase scrutinee alts -> ECase (normalizeExpr scrutinee) (map normalizeCaseAlt alts)
    ELambdaPats pats body -> ELambdaPats (map normalizePattern pats) (normalizeExpr body)
    ELambdaCase alts -> ELambdaCase (map normalizeCaseAlt alts)
    ELetDecls decls body -> ELetDecls (map normalizeDecl decls) (normalizeExpr body)
    EWhereDecls body decls -> EWhereDecls (normalizeExpr body) (map normalizeDecl decls)
    EDo stmts -> EDo (map normalizeDoStmt stmts)
    EListComp body stmts -> EListComp (normalizeExpr body) (map normalizeCompStmt stmts)
    EListCompParallel body stmtss -> EListCompParallel (normalizeExpr body) (map (map normalizeCompStmt) stmtss)
    EList elems -> EList (map normalizeExpr elems)
    ETuple tupleFlavor elems -> ETuple tupleFlavor (map normalizeExpr elems)
    -- When a tuple section has all holes, it becomes a tuple constructor
    ETupleSection tupleFlavor elems
      | all (== Nothing) elems -> ETupleSection tupleFlavor elems
      | otherwise -> ETupleSection tupleFlavor (map (fmap normalizeExpr) elems)
    -- A tuple constructor is equivalent to a tuple section with all holes
    ETupleCon tupleFlavor n -> ETupleSection tupleFlavor (replicate n Nothing)
    EArithSeq seq' -> EArithSeq (normalizeArithSeq seq')
    ERecordCon con fields rwc -> ERecordCon con [(name, normalizeExpr e) | (name, e) <- fields] rwc
    ERecordUpd target fields -> ERecordUpd (normalizeExpr target) [(name, normalizeExpr e) | (name, e) <- fields]
    ETypeSig inner ty -> ETypeSig (normalizeExpr inner) (normalizeType ty)
    ETypeApp inner ty -> ETypeApp (normalizeExpr inner) (normalizeType ty)
    EUnboxedSum altIdx arity inner -> EUnboxedSum altIdx arity (normalizeExpr inner)
    EParen inner -> normalizeExpr inner
    ETHExpQuote body -> ETHExpQuote (normalizeExpr body)
    ETHTypedQuote body -> ETHTypedQuote (normalizeExpr body)
    ETHDeclQuote decls -> ETHDeclQuote (map normalizeDecl decls)
    ETHTypeQuote ty -> ETHTypeQuote (normalizeType ty)
    ETHPatQuote pat -> ETHPatQuote (normalizePattern pat)
    ETHNameQuote name -> ETHNameQuote name
    ETHTypeNameQuote name -> ETHTypeNameQuote name
    ETHSplice body -> ETHSplice (normalizeExpr body)
    ETHTypedSplice body -> ETHTypedSplice (normalizeExpr body)

normalizeCaseAlt :: CaseAlt -> CaseAlt
normalizeCaseAlt alt =
  CaseAlt
    { caseAltPattern = normalizePattern (caseAltPattern alt),
      caseAltRhs = normalizeRhs (caseAltRhs alt)
    }

normalizeRhs :: Rhs -> Rhs
normalizeRhs rhs =
  case rhs of
    UnguardedRhs body -> UnguardedRhs (normalizeExpr body)
    GuardedRhss guards -> GuardedRhss (map normalizeGuardedRhs guards)

normalizeGuardedRhs :: GuardedRhs -> GuardedRhs
normalizeGuardedRhs grhs =
  GuardedRhs
    { guardedRhsGuards = map normalizeGuardQualifier (guardedRhsGuards grhs),
      guardedRhsBody = normalizeExpr (guardedRhsBody grhs)
    }

normalizeGuardQualifier :: GuardQualifier -> GuardQualifier
normalizeGuardQualifier qual =
  case qual of
    GuardExpr e -> GuardExpr (normalizeExpr e)
    GuardPat pat e -> GuardPat (normalizePattern pat) (normalizeExpr e)
    GuardLet decls -> GuardLet (map normalizeDecl decls)

normalizePattern :: Pattern -> Pattern
normalizePattern pat =
  case pat of
    PVar name -> PVar name
    PWildcard -> PWildcard
    PLit lit -> PLit (normalizeLiteral lit)
    PQuasiQuote quoter body -> PQuasiQuote quoter body
    PTuple tupleFlavor elems -> PTuple tupleFlavor (map normalizePattern elems)
    PList elems -> PList (map normalizePattern elems)
    PCon con args -> PCon con (map normalizePattern args)
    PInfix lhs op rhs -> PInfix (normalizePattern lhs) op (normalizePattern rhs)
    PView e inner -> PView (normalizeExpr e) (normalizePattern inner)
    PAs name inner -> PAs name (normalizePattern inner)
    PStrict inner -> PStrict (normalizePattern inner)
    PIrrefutable inner -> PIrrefutable (normalizePattern inner)
    PNegLit lit -> PNegLit (normalizeLiteral lit)
    PParen inner -> PParen (normalizePattern inner)
    PUnboxedSum altIdx arity inner -> PUnboxedSum altIdx arity (normalizePattern inner)
    PRecord con fields rwc -> PRecord con [(name, normalizePattern p) | (name, p) <- fields] rwc
    PTypeSig inner ty -> PTypeSig (normalizePattern inner) (normalizeType ty)
    PSplice body -> PSplice (normalizeExpr body)

normalizeLiteral :: Literal -> Literal
normalizeLiteral lit =
  case lit of
    LitInt value repr -> LitInt value repr
    LitIntHash value repr -> LitIntHash value repr
    LitIntBase value repr -> LitIntBase value repr
    LitIntBaseHash value repr -> LitIntBaseHash value repr
    LitFloat value repr -> LitFloat value repr
    LitFloatHash value repr -> LitFloatHash value repr
    LitChar value repr -> LitChar value repr
    LitCharHash value repr -> LitCharHash value repr
    LitString value repr -> LitString value repr
    LitStringHash value repr -> LitStringHash value repr

normalizeDecl :: Decl -> Decl
normalizeDecl decl =
  case decl of
    DeclValue vdecl -> DeclValue (normalizeValueDecl vdecl)
    DeclTypeSig names ty -> DeclTypeSig names (normalizeType ty)

normalizeValueDecl :: ValueDecl -> ValueDecl
normalizeValueDecl vdecl =
  case vdecl of
    PatternBind pat rhs -> PatternBind (normalizePattern pat) (normalizeRhs rhs)
    FunctionBind name matches -> FunctionBind name (map normalizeMatch matches)

normalizeMatch :: Match -> Match
normalizeMatch m =
  Match
    { matchHeadForm = matchHeadForm m,
      matchPats = map normalizePattern (matchPats m),
      matchRhs = normalizeRhs (matchRhs m)
    }

normalizeDoStmt :: DoStmt -> DoStmt
normalizeDoStmt stmt =
  case stmt of
    DoBind pat e -> DoBind (normalizePattern pat) (normalizeExpr e)
    DoLet bindings -> DoLet [(name, normalizeExpr e) | (name, e) <- bindings]
    DoLetDecls decls -> DoLetDecls (map normalizeDecl decls)
    DoExpr e -> DoExpr (normalizeExpr e)

normalizeCompStmt :: CompStmt -> CompStmt
normalizeCompStmt stmt =
  case stmt of
    CompGen pat e -> CompGen (normalizePattern pat) (normalizeExpr e)
    CompGuard e -> CompGuard (normalizeExpr e)
    CompLet bindings -> CompLet [(name, normalizeExpr e) | (name, e) <- bindings]
    CompLetDecls decls -> CompLetDecls (map normalizeDecl decls)

normalizeArithSeq :: ArithSeq -> ArithSeq
normalizeArithSeq seq' =
  case seq' of
    ArithSeqFrom from -> ArithSeqFrom (normalizeExpr from)
    ArithSeqFromThen from thenE -> ArithSeqFromThen (normalizeExpr from) (normalizeExpr thenE)
    ArithSeqFromTo from to -> ArithSeqFromTo (normalizeExpr from) (normalizeExpr to)
    ArithSeqFromThenTo from thenE to -> ArithSeqFromThenTo (normalizeExpr from) (normalizeExpr thenE) (normalizeExpr to)

normalizeType :: Type -> Type
normalizeType ty =
  case ty of
    TVar name -> TVar name
    TCon name promoted -> TCon name promoted
    TTypeLit lit -> TTypeLit lit
    TStar -> TStar
    TQuasiQuote quoter body -> TQuasiQuote quoter body
    TForall binders inner -> TForall binders (normalizeType inner)
    TApp fn arg -> TApp (normalizeType fn) (normalizeType arg)
    TFun lhs rhs -> TFun (normalizeType lhs) (normalizeType rhs)
    TTuple tupleFlavor promoted elems -> TTuple tupleFlavor promoted (map normalizeType elems)
    TList promoted inner -> TList promoted (normalizeType inner)
    -- Remove redundant parentheses from types
    TParen inner -> normalizeType inner
    TUnboxedSum elems -> TUnboxedSum (map normalizeType elems)
    TContext constraints inner -> TContext (map normalizeConstraint constraints) (normalizeType inner)
    TSplice body -> TSplice (normalizeExpr body)

normalizeConstraint :: Constraint -> Constraint
normalizeConstraint c =
  case c of
    Constraint cls args ->
      Constraint
        { constraintClass = cls,
          constraintArgs = map normalizeType args
        }
    CParen inner ->
      CParen (normalizeConstraint inner)
