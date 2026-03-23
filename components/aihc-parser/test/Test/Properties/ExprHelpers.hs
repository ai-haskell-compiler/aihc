{-# LANGUAGE OverloadedStrings #-}

-- | Shared helper functions for expression generation and normalization
-- used by both ExprRoundTrip and ModuleRoundTrip tests.
module Test.Properties.ExprHelpers
  ( genExpr,
    mkIntExpr,
    shrinkExpr,
    normalizeExpr,
    span0,
  )
where

import Aihc.Lexer (isReservedIdentifier)
import Aihc.Parser.Ast
import Data.Text (Text)
import qualified Data.Text as T
import Test.Properties.Identifiers (genIdent, shrinkIdent)
import Test.QuickCheck

-- | Canonical empty source span for normalization.
span0 :: SourceSpan
span0 = noSourceSpan

-- | Generate a random expression up to the given depth.
genExpr :: Int -> Gen Expr
genExpr depth
  | depth <= 0 =
      -- At depth 0, generate more variables and integers to meet coverage requirements
      frequency
        [ (4, EVar span0 <$> genIdent),
          (4, mkIntExpr <$> chooseInteger (0, 999)),
          (1, mkHexExpr <$> chooseInteger (0, 255)),
          (1, mkFloatExpr <$> genTenths),
          (1, mkCharExpr <$> genCharValue),
          (1, mkStringExpr <$> genStringValue),
          -- Note: EQuasiQuote requires QuasiQuotes extension, skip for now
          (1, pure (EList span0 [])),
          (1, pure (ETuple span0 [])),
          (1, ETupleCon span0 <$> chooseInt (2, 5))
        ]
  | otherwise =
      frequency
        [ -- Variables and literals - higher weights for coverage
          (6, EVar span0 <$> genIdent),
          (8, mkIntExpr <$> chooseInteger (0, 999)),
          (1, mkHexExpr <$> chooseInteger (0, 255)),
          (1, mkFloatExpr <$> genTenths),
          (1, mkCharExpr <$> genCharValue),
          (1, mkStringExpr <$> genStringValue),
          -- Note: EQuasiQuote requires QuasiQuotes extension, skip for now
          -- Application and operators - higher weight for coverage
          -- Note: using genExprAppHead to avoid negation precedence issues
          (15, EApp span0 <$> genExprAppHead (depth - 1) <*> genExprAtom (depth - 1)),
          (2, genInfixExpr depth),
          (1, ENegate span0 <$> genExprAtom (depth - 1)),
          -- Note: sections with complex expressions like let/if/case cause parsing issues
          (2, ESectionL span0 <$> genExprAtom (depth - 1) <*> genOperator),
          (2, ESectionR span0 <$> genOperator <*> genExprAtom (depth - 1)),
          -- Control flow
          -- Note: if-then-else branches with case/let can cause module-level parsing issues
          (2, EIf span0 <$> genExprAtom (depth - 1) <*> genExprAtom (depth - 1) <*> genExprAtom (depth - 1)),
          -- Note: case scrutinee must be atomic to avoid "case case ... of ... of ..." ambiguity
          (2, ECase span0 <$> genExprAtom (depth - 1) <*> genCaseAlts (depth - 1)),
          (2, ELambdaPats span0 <$> genPatterns (depth - 1) <*> genExprAtom (depth - 1)),
          (1, ELambdaCase span0 <$> genCaseAlts (depth - 1)),
          -- Bindings
          -- Note: let body with case/if can cause module-level parsing issues
          (2, ELetDecls span0 <$> genValueDecls (depth - 1) <*> genExprAtom (depth - 1)),
          -- Note: EWhereDecls with arbitrary expressions causes parsing ambiguity
          -- (where binds to innermost expression), so we use atomic expressions only
          (1, EWhereDecls span0 <$> genExprAtom (depth - 1) <*> genValueDecls (depth - 1)),
          -- Do notation and list comprehensions
          (2, EDo span0 <$> genDoStmts (depth - 1)),
          -- Note: list comprehension body with case can cause parsing issues
          (2, EListComp span0 <$> genExprAtom (depth - 1) <*> genCompStmts (depth - 1)),
          (1, EListCompParallel span0 <$> genExprAtom (depth - 1) <*> genParallelCompStmts (depth - 1)),
          -- Collections
          (3, EList span0 <$> genListElems (depth - 1)),
          (3, ETuple span0 <$> genTupleElems (depth - 1)),
          (1, ETupleSection span0 <$> genTupleSectionElems (depth - 1)),
          (1, ETupleCon span0 <$> chooseInt (2, 5)),
          -- Arithmetic sequences
          (1, EArithSeq span0 <$> genArithSeq (depth - 1)),
          -- Records
          (2, ERecordCon span0 <$> genConName <*> genRecordFields (depth - 1)),
          (1, ERecordUpd span0 <$> genExprAtom (depth - 1) <*> genRecordFields (depth - 1)),
          -- Type annotations - using atomic expressions to avoid nested type sigs
          (2, ETypeSig span0 <$> genExprAtom (depth - 1) <*> genType (depth - 1)),
          -- Note: ETypeApp requires TypeApplications extension and has parsing issues
          -- Parentheses
          (2, EParen span0 <$> genExpr (depth - 1))
        ]

-- | Generate an atomic expression (one that doesn't need parentheses in most contexts)
-- NOTE: This intentionally only generates simple expressions (no case/if/let/lambda)
-- to avoid parsing ambiguity in contexts like list elements, tuple elements, etc.
genExprAtom :: Int -> Gen Expr
genExprAtom depth
  | depth <= 0 =
      oneof
        [ EVar span0 <$> genIdent,
          mkIntExpr <$> chooseInteger (0, 999),
          mkFloatExpr <$> genTenths,
          mkCharExpr <$> genCharValue,
          mkStringExpr <$> genStringValue,
          pure (EList span0 []),
          pure (ETuple span0 [])
        ]
  | otherwise =
      frequency
        [ (4, EVar span0 <$> genIdent),
          (3, mkIntExpr <$> chooseInteger (0, 999)),
          (2, EList span0 <$> genSimpleListElems (depth - 1)),
          (2, ETuple span0 <$> genSimpleTupleElems (depth - 1)),
          (1, EParen span0 <$> genExprAtom (depth - 1)),
          -- Higher weight for application to meet coverage requirements
          (4, EApp span0 <$> genExprAtom (depth - 1) <*> genExprAtom (depth - 1))
        ]

-- | Generate simple list elements (no complex expressions)
genSimpleListElems :: Int -> Gen [Expr]
genSimpleListElems depth = do
  n <- chooseInt (0, 3)
  vectorOf n (genExprAtom depth)

-- | Generate simple tuple elements (no complex expressions)
genSimpleTupleElems :: Int -> Gen [Expr]
genSimpleTupleElems depth = do
  isUnit <- arbitrary
  if isUnit
    then pure []
    else do
      n <- chooseInt (2, 3)
      vectorOf n (genExprAtom depth)

-- | Generate an expression suitable for use as the head of an application
-- Avoids negation which has lower precedence than application
genExprAppHead :: Int -> Gen Expr
genExprAppHead depth =
  frequency
    [ (4, EVar span0 <$> genIdent),
      (2, EParen span0 <$> genExpr depth),
      (2, EApp span0 <$> genExprAppHead (max 0 (depth - 1)) <*> genExprAtom (max 0 (depth - 1)))
    ]

-- | Generate an infix expression
-- Uses atomic expressions as operands to avoid precedence issues
genInfixExpr :: Int -> Gen Expr
genInfixExpr depth = do
  lhs <- genExprAtom (depth - 1)
  op <- genOperator
  rhs <- genExprAtom (depth - 1)
  pure (EInfix span0 lhs op rhs)

-- | Generate an operator symbol
genOperator :: Gen Text
genOperator =
  frequency
    [ (5, elements ["+", "-", "*", "/", "<", ">", "<=", ">=", "==", "/=", "&&", "||", "++", ">>", ">>=", "."]),
      (1, genCustomOperator)
    ]

-- | Generate a custom operator
-- Only uses valid operator characters (matching isOperatorToken in Pretty.hs)
genCustomOperator :: Gen Text
genCustomOperator = do
  len <- chooseInt (1, 3)
  -- Note: matches ":!#$%&*+./<=>?\\^|-~" from Pretty.hs isOperatorToken
  -- Excluding ':' since that's for constructor operators
  chars <- vectorOf len (elements "!#$%&*+./<=>?\\^|-~")
  let candidate = T.pack chars
  -- Avoid reserved operators and comment starters
  if candidate `elem` ["..", "::", "=", "\\", "|", "<-", "->", "~", "=>", "--"]
    then genCustomOperator
    else pure candidate

-- | Generate a data constructor name
genConName :: Gen Text
genConName = do
  first <- elements ['A' .. 'Z']
  restLen <- chooseInt (0, 5)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'"))
  pure (T.pack (first : rest))

-- | Generate simple patterns for lambdas
genPatterns :: Int -> Gen [Pattern]
genPatterns depth = do
  n <- chooseInt (1, 3)
  vectorOf n (genSimplePattern depth)

-- | Generate a simple pattern (suitable for lambda patterns)
genSimplePattern :: Int -> Gen Pattern
genSimplePattern depth
  | depth <= 0 =
      oneof
        [ PVar span0 <$> genIdent,
          pure (PWildcard span0)
        ]
  | otherwise =
      frequency
        [ (4, PVar span0 <$> genIdent),
          (2, pure (PWildcard span0)),
          (2, PTuple span0 <$> genPatternTupleElems (depth - 1)),
          (2, PList span0 <$> genPatternListElems (depth - 1))
          -- Note: PParen around patterns can cause parsing ambiguity in some contexts
        ]

genPatternTupleElems :: Int -> Gen [Pattern]
genPatternTupleElems depth = do
  isUnit <- arbitrary
  if isUnit
    then pure []
    else do
      n <- chooseInt (2, 3)
      vectorOf n (genSimplePattern depth)

genPatternListElems :: Int -> Gen [Pattern]
genPatternListElems depth = do
  n <- chooseInt (0, 3)
  vectorOf n (genSimplePattern depth)

-- | Generate case alternatives
-- Using single-alt to avoid layout ambiguity with nested case expressions
genCaseAlts :: Int -> Gen [CaseAlt]
genCaseAlts depth = do
  -- Only generate single alt to avoid parsing ambiguity with nested cases
  let n = 1
  vectorOf n (genCaseAlt depth)

genCaseAlt :: Int -> Gen CaseAlt
genCaseAlt depth = do
  pat <- genSimplePattern depth
  expr <- genExpr depth
  pure $
    CaseAlt
      { caseAltSpan = span0,
        caseAltPattern = pat,
        caseAltRhs = UnguardedRhs span0 expr
      }

-- | Generate value declarations for let/where
-- We use FunctionBind format because that's what the parser produces for simple
-- variable bindings like `x = e`.
-- Using atomic expressions to avoid case/if/let ambiguity with inline layout.
genValueDecls :: Int -> Gen [Decl]
genValueDecls depth = do
  n <- chooseInt (1, 3)
  names <- vectorOf n genIdent
  -- Use atomic expressions to avoid case/if ambiguity when multiple decls in inline layout
  exprs <- vectorOf n (genExprAtom depth)
  pure
    [ DeclValue
        span0
        ( FunctionBind
            span0
            name
            [ Match
                { matchSpan = span0,
                  matchPats = [],
                  matchRhs = UnguardedRhs span0 expr
                }
            ]
        )
    | (name, expr) <- zip names exprs
    ]

-- | Generate do statements
-- Using atomic expressions to avoid parsing ambiguity
genDoStmts :: Int -> Gen [DoStmt]
genDoStmts depth = do
  n <- chooseInt (0, 2)
  stmts <- vectorOf n (genDoStmt depth)
  -- Last statement must be DoExpr with atomic expression
  lastExpr <- genExprAtom depth
  pure (stmts <> [DoExpr span0 lastExpr])

genDoStmt :: Int -> Gen DoStmt
genDoStmt depth =
  frequency
    [ (3, DoBind span0 <$> genSimplePattern depth <*> genExprAtom depth),
      -- Use DoLetDecls instead of DoLet since that's what the parser produces
      (2, DoLetDecls span0 <$> genValueDecls depth),
      -- Use atomic expressions to avoid case/if/let ambiguity in do statements
      (3, DoExpr span0 <$> genExprAtom depth)
    ]

-- | Generate list comprehension statements
-- Using atomic expressions to avoid parsing ambiguity with case/if/let
genCompStmts :: Int -> Gen [CompStmt]
genCompStmts depth = do
  n <- chooseInt (1, 3)
  vectorOf n (genCompStmt depth)

genCompStmt :: Int -> Gen CompStmt
genCompStmt depth =
  frequency
    [ -- Use atomic expressions to avoid case/if/let ambiguity in comp stmts
      (4, CompGen span0 <$> genSimplePattern depth <*> genExprAtom depth),
      (2, CompGuard span0 <$> genExprAtom depth)
      -- Note: CompLet and CompLetDecls have parsing issues in some contexts,
      -- so we skip generating them for now
    ]

-- | Generate parallel list comprehension statements
genParallelCompStmts :: Int -> Gen [[CompStmt]]
genParallelCompStmts depth = do
  n <- chooseInt (2, 3)
  vectorOf n (genCompStmts depth)

-- | Generate list elements
-- Using atomic expressions to avoid parsing ambiguity with case/if/let in lists
genListElems :: Int -> Gen [Expr]
genListElems depth = do
  n <- chooseInt (0, 4)
  vectorOf n (genExprAtom depth)

-- | Generate tuple elements
-- Using atomic expressions to avoid parsing ambiguity
genTupleElems :: Int -> Gen [Expr]
genTupleElems depth = do
  isUnit <- arbitrary
  if isUnit
    then pure []
    else do
      n <- chooseInt (2, 4)
      vectorOf n (genExprAtom depth)

-- | Generate tuple section elements
genTupleSectionElems :: Int -> Gen [Maybe Expr]
genTupleSectionElems depth = do
  n <- chooseInt (2, 4)
  elems <- vectorOf n (genMaybeExpr depth)
  -- Ensure at least one Nothing (otherwise it's just a tuple)
  if Nothing `notElem` elems
    then do
      idx <- chooseInt (0, n - 1)
      pure (take idx elems <> [Nothing] <> drop (idx + 1) elems)
    else pure elems

genMaybeExpr :: Int -> Gen (Maybe Expr)
genMaybeExpr depth =
  frequency
    [ -- Use atomic expressions to avoid case/if/let ambiguity in tuple sections
      (2, Just <$> genExprAtom depth),
      (1, pure Nothing)
    ]

-- | Generate arithmetic sequences
-- Using atomic expressions to avoid parsing ambiguity
genArithSeq :: Int -> Gen ArithSeq
genArithSeq depth =
  oneof
    [ ArithSeqFrom span0 <$> genExprAtom depth,
      ArithSeqFromThen span0 <$> genExprAtom depth <*> genExprAtom depth,
      ArithSeqFromTo span0 <$> genExprAtom depth <*> genExprAtom depth,
      ArithSeqFromThenTo span0 <$> genExprAtom depth <*> genExprAtom depth <*> genExprAtom depth
    ]

-- | Generate record fields
genRecordFields :: Int -> Gen [(Text, Expr)]
genRecordFields depth = do
  n <- chooseInt (0, 3)
  names <- vectorOf n genFieldName
  -- Use atomic expressions to avoid parsing issues with case/if/let in record fields
  exprs <- vectorOf n (genExprAtom depth)
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

-- | Generate a simple type for type annotations
genType :: Int -> Gen Type
genType depth
  | depth <= 0 =
      oneof
        [ TVar span0 <$> genTypeVarName,
          TCon span0 <$> genConName
        ]
  | otherwise =
      frequency
        [ (3, TVar span0 <$> genTypeVarName),
          (3, TCon span0 <$> genConName),
          (2, TApp span0 <$> genType (depth - 1) <*> genTypeAtom (depth - 1)),
          (2, TFun span0 <$> genType (depth - 1) <*> genType (depth - 1)),
          (2, TList span0 <$> genType (depth - 1)),
          (2, TTuple span0 <$> genTypeTupleElems (depth - 1)),
          (1, TParen span0 <$> genType (depth - 1))
        ]

genTypeAtom :: Int -> Gen Type
genTypeAtom depth =
  oneof
    [ TVar span0 <$> genTypeVarName,
      TCon span0 <$> genConName,
      TList span0 <$> genType depth,
      TTuple span0 <$> genTypeTupleElems depth,
      TParen span0 <$> genType depth
    ]

genTypeTupleElems :: Int -> Gen [Type]
genTypeTupleElems depth = do
  isUnit <- arbitrary
  if isUnit
    then pure []
    else do
      n <- chooseInt (2, 3)
      vectorOf n (genType depth)

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
    hexDigit n = "0123456789abcdef" !! fromInteger n

-- | Create an integer expression with canonical representation.
mkIntExpr :: Integer -> Expr
mkIntExpr value = EInt span0 value (T.pack (show value))

-- | Shrink an expression for QuickCheck counterexample minimization.
shrinkExpr :: Expr -> [Expr]
shrinkExpr expr =
  case expr of
    EVar _ name -> [EVar span0 shrunk | shrunk <- shrinkIdent name]
    EInt _ value _ -> [mkIntExpr shrunk | shrunk <- shrinkIntegral value]
    EIntBase _ value _ -> [mkIntExpr shrunk | shrunk <- shrinkIntegral value]
    EFloat _ value _ -> [mkFloatExpr shrunk | shrunk <- shrinkFloat value]
    EChar {} -> []
    EString _ value _ -> [mkStringExpr (T.pack shrunk) | shrunk <- shrink (T.unpack value)]
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
    EDo _ stmts ->
      [EDo span0 stmts' | stmts' <- shrinkDoStmts stmts, not (null stmts')]
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
    ETuple _ elems ->
      [ETuple span0 elems' | elems' <- shrinkTupleElems shrinkExpr elems]
    ETupleSection _ elems ->
      [ETupleSection span0 elems' | elems' <- shrinkTupleSectionElems elems]
    ETupleCon _ n -> [ETupleCon span0 n' | n' <- shrink n, n' >= 2]
    EArithSeq _ seq' ->
      [EArithSeq span0 seq'' | seq'' <- shrinkArithSeq seq']
    ERecordCon _ con fields ->
      [ERecordCon span0 con fields' | fields' <- shrinkRecordFields fields]
    ERecordUpd _ target fields ->
      target
        : [ERecordUpd span0 target' fields | target' <- shrinkExpr target]
          <> [ERecordUpd span0 target fields' | fields' <- shrinkRecordFields fields]
    ETypeSig _ inner _ ->
      inner : [ETypeSig span0 inner' (TCon span0 "T") | inner' <- shrinkExpr inner]
    ETypeApp _ inner _ ->
      inner : [ETypeApp span0 inner' (TCon span0 "T") | inner' <- shrinkExpr inner]
    EParen _ inner -> inner : [EParen span0 inner' | inner' <- shrinkExpr inner]

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

shrinkDecls :: [Decl] -> [[Decl]]
shrinkDecls = shrinkList shrinkDecl

shrinkDecl :: Decl -> [Decl]
shrinkDecl decl =
  case decl of
    DeclValue _ (PatternBind _ pat (UnguardedRhs _ expr)) ->
      [DeclValue span0 (PatternBind span0 pat (UnguardedRhs span0 expr')) | expr' <- shrinkExpr expr]
    _ -> []

shrinkDoStmts :: [DoStmt] -> [[DoStmt]]
shrinkDoStmts stmts =
  case stmts of
    [_] -> [] -- Can't shrink a single-element do block
    _ -> shrinkList shrinkDoStmt stmts

shrinkDoStmt :: DoStmt -> [DoStmt]
shrinkDoStmt stmt =
  case stmt of
    DoBind _ pat expr -> [DoBind span0 pat expr' | expr' <- shrinkExpr expr]
    DoLet _ bindings -> [DoLet span0 bindings' | bindings' <- shrinkList shrinkBinding bindings, not (null bindings')]
    DoLetDecls _ decls -> [DoLetDecls span0 decls' | decls' <- shrinkDecls decls, not (null decls')]
    DoExpr _ expr -> [DoExpr span0 expr' | expr' <- shrinkExpr expr]

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
    EIntBase _ value repr -> EIntBase span0 value repr
    EFloat _ value repr -> EFloat span0 value repr
    EChar _ value repr -> EChar span0 value repr
    EString _ value repr -> EString span0 value repr
    EQuasiQuote _ quoter body -> EQuasiQuote span0 quoter body
    EApp _ fn arg -> EApp span0 (normalizeExpr fn) (normalizeExpr arg)
    EInfix _ lhs op rhs -> EInfix span0 (normalizeExpr lhs) op (normalizeExpr rhs)
    ENegate _ inner -> ENegate span0 (normalizeExpr inner)
    ESectionL _ inner op -> ESectionL span0 (normalizeExpr inner) op
    ESectionR _ op inner -> ESectionR span0 op (normalizeExpr inner)
    EIf _ cond thenE elseE -> EIf span0 (normalizeExpr cond) (normalizeExpr thenE) (normalizeExpr elseE)
    ECase _ scrutinee alts -> ECase span0 (normalizeExpr scrutinee) (map normalizeCaseAlt alts)
    ELambdaPats _ pats body -> ELambdaPats span0 (map normalizePattern pats) (normalizeExpr body)
    ELambdaCase _ alts -> ELambdaCase span0 (map normalizeCaseAlt alts)
    ELetDecls _ decls body -> ELetDecls span0 (map normalizeDecl decls) (normalizeExpr body)
    EWhereDecls _ body decls -> EWhereDecls span0 (normalizeExpr body) (map normalizeDecl decls)
    EDo _ stmts -> EDo span0 (map normalizeDoStmt stmts)
    EListComp _ body stmts -> EListComp span0 (normalizeExpr body) (map normalizeCompStmt stmts)
    EListCompParallel _ body stmtss -> EListCompParallel span0 (normalizeExpr body) (map (map normalizeCompStmt) stmtss)
    EList _ elems -> EList span0 (map normalizeExpr elems)
    ETuple _ elems -> ETuple span0 (map normalizeExpr elems)
    -- When a tuple section has all holes, it becomes a tuple constructor
    ETupleSection _ elems
      | all (== Nothing) elems -> ETupleSection span0 elems
      | otherwise -> ETupleSection span0 (map (fmap normalizeExpr) elems)
    -- A tuple constructor is equivalent to a tuple section with all holes
    ETupleCon _ n -> ETupleSection span0 (replicate n Nothing)
    EArithSeq _ seq' -> EArithSeq span0 (normalizeArithSeq seq')
    ERecordCon _ con fields -> ERecordCon span0 con [(name, normalizeExpr e) | (name, e) <- fields]
    ERecordUpd _ target fields -> ERecordUpd span0 (normalizeExpr target) [(name, normalizeExpr e) | (name, e) <- fields]
    ETypeSig _ inner ty -> ETypeSig span0 (normalizeExpr inner) (normalizeType ty)
    ETypeApp _ inner ty -> ETypeApp span0 (normalizeExpr inner) (normalizeType ty)
    EParen _ inner -> normalizeExpr inner

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
    PVar _ name -> PVar span0 name
    PWildcard _ -> PWildcard span0
    PLit _ lit -> PLit span0 (normalizeLiteral lit)
    PQuasiQuote _ quoter body -> PQuasiQuote span0 quoter body
    PTuple _ elems -> PTuple span0 (map normalizePattern elems)
    PList _ elems -> PList span0 (map normalizePattern elems)
    PCon _ con args -> PCon span0 con (map normalizePattern args)
    PInfix _ lhs op rhs -> PInfix span0 (normalizePattern lhs) op (normalizePattern rhs)
    PView _ e inner -> PView span0 (normalizeExpr e) (normalizePattern inner)
    PAs _ name inner -> PAs span0 name (normalizePattern inner)
    PStrict _ inner -> PStrict span0 (normalizePattern inner)
    PIrrefutable _ inner -> PIrrefutable span0 (normalizePattern inner)
    PNegLit _ lit -> PNegLit span0 (normalizeLiteral lit)
    PParen _ inner -> PParen span0 (normalizePattern inner)
    PRecord _ con fields -> PRecord span0 con [(name, normalizePattern p) | (name, p) <- fields]

normalizeLiteral :: Literal -> Literal
normalizeLiteral lit =
  case lit of
    LitInt _ value repr -> LitInt span0 value repr
    LitIntBase _ value repr -> LitIntBase span0 value repr
    LitFloat _ value repr -> LitFloat span0 value repr
    LitChar _ value repr -> LitChar span0 value repr
    LitString _ value repr -> LitString span0 value repr

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
      matchPats = map normalizePattern (matchPats m),
      matchRhs = normalizeRhs (matchRhs m)
    }

normalizeDoStmt :: DoStmt -> DoStmt
normalizeDoStmt stmt =
  case stmt of
    DoBind _ pat e -> DoBind span0 (normalizePattern pat) (normalizeExpr e)
    DoLet _ bindings -> DoLet span0 [(name, normalizeExpr e) | (name, e) <- bindings]
    DoLetDecls _ decls -> DoLetDecls span0 (map normalizeDecl decls)
    DoExpr _ e -> DoExpr span0 (normalizeExpr e)

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
    TCon _ name -> TCon span0 name
    TStar _ -> TStar span0
    TQuasiQuote _ quoter body -> TQuasiQuote span0 quoter body
    TForall _ binders inner -> TForall span0 binders (normalizeType inner)
    TApp _ fn arg -> TApp span0 (normalizeType fn) (normalizeType arg)
    TFun _ lhs rhs -> TFun span0 (normalizeType lhs) (normalizeType rhs)
    TTuple _ elems -> TTuple span0 (map normalizeType elems)
    TList _ inner -> TList span0 (normalizeType inner)
    -- Remove redundant parentheses from types
    TParen _ inner -> normalizeType inner
    TContext _ constraints inner -> TContext span0 (map normalizeConstraint constraints) (normalizeType inner)

normalizeConstraint :: Constraint -> Constraint
normalizeConstraint c =
  Constraint
    { constraintSpan = span0,
      constraintClass = constraintClass c,
      constraintArgs = map normalizeType (constraintArgs c),
      constraintParen = constraintParen c
    }
