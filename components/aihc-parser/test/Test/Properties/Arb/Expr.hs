{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.Arb.Expr
  ( genExpr,
    genRhsWith,
    mkIntExpr,
    shrinkExpr,
    shrinkGuardQualifier,
  )
where

import Aihc.Parser.Syntax
import Data.Char (isSpace)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Properties.Arb.Decl (genWhereDecls)
import Test.Properties.Arb.Identifiers
  ( genCharValue,
    genConName,
    genStringValue,
    genTenths,
    genVarId,
    genVarName,
    genVarUnqualifiedName,
    showHex,
    shrinkFloat,
    shrinkName,
  )
import Test.Properties.Arb.Pattern (genPattern, shrinkPattern)
import Test.Properties.Arb.Type (shrinkType)
import Test.Properties.Arb.Utils
import Test.QuickCheck

-- | Generate a random expression. Uses QuickCheck's size parameter
-- to control recursion depth.
genExpr :: Gen Expr
genExpr = genExprWith True

-- | Generate an expression, optionally allowing Template Haskell quote forms.
-- Nested TH brackets are rejected by GHC unless separated by splices, so quote
-- bodies disable further quote generation.
genExprWith :: Bool -> Gen Expr
genExprWith allowTHQuotes = scale (`div` 2) $ do
  n <- getSize
  if n <= 0 then genExprLeaf else oneof (baseGenerators <> quoteGenerators)
  where
    baseGenerators =
      [ -- Leaf expressions
        genExprLeaf,
        -- Recursive expressions (reduce size for subexpressions)
        EApp <$> genExprWith allowTHQuotes <*> genExprWith allowTHQuotes,
        EInfix <$> genExprWith allowTHQuotes <*> genVarName <*> genExprWith allowTHQuotes,
        ENegate <$> genExprWith allowTHQuotes,
        ESectionL <$> genExprWith allowTHQuotes <*> genVarName,
        ESectionR <$> genVarName <*> genExprWith allowTHQuotes,
        EIf <$> genExprWith allowTHQuotes <*> genExprWith allowTHQuotes <*> genExprWith allowTHQuotes,
        EMultiWayIf <$> genGuardedRhsListWith allowTHQuotes,
        ECase <$> genExprWith allowTHQuotes <*> genCaseAltsWith allowTHQuotes,
        ELambdaPats <$> genPatterns <*> genExprWith allowTHQuotes,
        ELambdaCase <$> genCaseAltsWith allowTHQuotes,
        ELambdaCases <$> genLambdaCaseAltsWith allowTHQuotes,
        ELetDecls <$> genValueDeclsWith allowTHQuotes <*> genExprWith allowTHQuotes,
        EDo <$> genDoStmtsWith allowTHQuotes <*> genDoFlavor,
        EListComp <$> genExprWith allowTHQuotes <*> genCompStmtsWith allowTHQuotes,
        EListCompParallel <$> genExprWith allowTHQuotes <*> genParallelCompStmtsWith allowTHQuotes,
        EList <$> genListElemsWith allowTHQuotes,
        ETuple Boxed . map Just <$> genTupleElemsWith allowTHQuotes,
        ETuple Unboxed . map Just <$> genUnboxedTupleElemsWith allowTHQuotes,
        ETuple Boxed <$> genTupleSectionElemsWith allowTHQuotes,
        ETuple Unboxed <$> genTupleSectionElemsWith allowTHQuotes,
        genUnboxedSumExprWith allowTHQuotes,
        EArithSeq <$> genArithSeqWith allowTHQuotes,
        ERecordCon <$> genConName <*> genRecordFieldsWith allowTHQuotes <*> pure False,
        ERecordUpd <$> genExprWith allowTHQuotes <*> genRecordFieldsWith allowTHQuotes,
        ETypeSig <$> genExprWith allowTHQuotes <*> genTypeWith allowTHQuotes,
        ETypeApp <$> genExprWith allowTHQuotes <*> genTypeWith allowTHQuotes,
        EParen <$> genExprWith allowTHQuotes,
        EProc <$> genPattern <*> genCmdWith allowTHQuotes,
        -- Template Haskell splices are valid inside quote bodies.
        ETHSplice <$> genSpliceBody,
        ETHTypedSplice <$> genTypedSpliceBody
      ]
    quoteGenerators
      | allowTHQuotes =
          [ ETHExpQuote <$> genExprWith False,
            ETHTypedQuote <$> genExprWith False,
            ETHDeclQuote <$> genValueDeclsWith False,
            ETHPatQuote <$> genPattern,
            ETHTypeQuote <$> genTypeWith False,
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
  labelName <- suchThat genVarId (not . T.isSuffixOf "#")
  pure (EOverloadedLabel labelName ("#" <> labelName))

-- | Generate a quasi-quote name, excluding TH bracket names (e, d, p, t) which
-- would collide with Template Haskell bracket syntax ([e|...|], [d|...|], etc.).
genQuasiQuoteName :: Gen Text
genQuasiQuoteName = suchThat genVarId (\name -> name `notElem` ["e", "d", "p", "t"] && not (T.isSuffixOf "#" name))

-- | Generate the body of a TH splice: either a bare variable or a parenthesized expression.
-- Bare variables produce $name syntax; parenthesized produce $(expr) syntax.
genSpliceBody :: Gen Expr
genSpliceBody =
  oneof
    [ EVar <$> genVarName,
      EParen <$> scale (`div` 2) genExpr
    ]

-- | Generate the body of a TH typed splice: always parenthesized.
-- Typed splices require parentheses: $$(expr) is valid, $$expr is invalid.
genTypedSpliceBody :: Gen Expr
genTypedSpliceBody =
  EParen <$> scale (`div` 2) genExpr

-- | Generate a TH value name quote target.
-- Produces unqualified identifiers plus qualified identifiers and operators
-- such as @M.v@ and @M.+@.
genNameQuoteExpr :: Gen Expr
genNameQuoteExpr =
  oneof
    [ EVar <$> genVarName,
      pure (EList []),
      pure (ETuple Boxed []),
      pure (ETuple Unboxed []),
      (\n -> ETuple Boxed (replicate n Nothing))
        <$> chooseInt (2, 5),
      (\n -> ETuple Unboxed (replicate n Nothing))
        <$> chooseInt (2, 5)
    ]

genTypeNameQuoteType :: Gen Type
genTypeNameQuoteType =
  oneof
    [ TCon <$> genConName <*> pure Unpromoted,
      pure (TCon (qualifyName Nothing (mkUnqualifiedName NameConId "[]")) Unpromoted),
      pure (TTuple Boxed Unpromoted []),
      pure (TTuple Unboxed Unpromoted [])
    ]

-- | Generate simple patterns for lambdas
genPatterns :: Gen [Pattern]
genPatterns = smallList1 genPattern

genCmdWith :: Bool -> Gen Cmd
genCmdWith allowTHQuotes = scale (`div` 2) $ do
  n <- getSize
  if n <= 0 then genCmdLeafWith allowTHQuotes else oneof (genCmdLeafWith allowTHQuotes : genCmdRecursiveWith allowTHQuotes)

genCmdLeafWith :: Bool -> Gen Cmd
genCmdLeafWith allowTHQuotes =
  CmdArrApp <$> genExprWith allowTHQuotes <*> genArrAppType <*> genExprWith allowTHQuotes

genCmdRecursiveWith :: Bool -> [Gen Cmd]
genCmdRecursiveWith allowTHQuotes =
  [ CmdInfix <$> genCmdWith allowTHQuotes <*> genVarName <*> genCmdWith allowTHQuotes,
    CmdDo <$> genCmdDoStmtsWith allowTHQuotes,
    CmdIf <$> genExprWith allowTHQuotes <*> genCmdWith allowTHQuotes <*> genCmdWith allowTHQuotes,
    CmdCase <$> genExprWith allowTHQuotes <*> genCmdCaseAltsWith allowTHQuotes,
    CmdLet <$> genValueDeclsWith allowTHQuotes <*> genCmdWith allowTHQuotes,
    CmdLam <$> genPatterns <*> genCmdWith allowTHQuotes,
    CmdPar <$> genCmdWith allowTHQuotes
  ]

genArrAppType :: Gen ArrAppType
genArrAppType = elements [HsFirstOrderApp, HsHigherOrderApp]

genCmdCaseAltsWith :: Bool -> Gen [CaseAlt Cmd]
genCmdCaseAltsWith allowTHQuotes = smallList1 (genCmdCaseAltWith allowTHQuotes)

genCmdCaseAltWith :: Bool -> Gen (CaseAlt Cmd)
genCmdCaseAltWith allowTHQuotes = scale (`div` 2) $ do
  CaseAlt [] <$> genPattern <*> genCmdRhsWith allowTHQuotes

genCmdDoStmtsWith :: Bool -> Gen [DoStmt Cmd]
genCmdDoStmtsWith allowTHQuotes = smallList1 (genCmdDoStmtWith allowTHQuotes)

genCmdDoStmtWith :: Bool -> Gen (DoStmt Cmd)
genCmdDoStmtWith allowTHQuotes =
  scale (`div` 2) . oneof $
    [ DoBind <$> genPattern <*> genCmdWith allowTHQuotes,
      DoLetDecls <$> genValueDeclsWith allowTHQuotes,
      DoExpr <$> genCmdWith allowTHQuotes,
      DoRecStmt <$> genCmdRecDoStmtsWith allowTHQuotes
    ]

genCmdRecDoStmtsWith :: Bool -> Gen [DoStmt Cmd]
genCmdRecDoStmtsWith allowTHQuotes =
  scale (`div` 2) . smallList1 $
    oneof
      [ DoBind <$> genPattern <*> genCmdWith allowTHQuotes,
        DoLetDecls <$> genValueDeclsWith allowTHQuotes,
        DoExpr <$> genCmdWith allowTHQuotes
      ]

genCaseAltsWith :: Bool -> Gen [CaseAlt Expr]
genCaseAltsWith allowTHQuotes = smallList0 (genCaseAltWith allowTHQuotes)

genCaseAltWith :: Bool -> Gen (CaseAlt Expr)
genCaseAltWith allowTHQuotes = scale (`div` 2) $ do
  CaseAlt [] <$> genPattern <*> genRhsWith allowTHQuotes

genLambdaCaseAltsWith :: Bool -> Gen [LambdaCaseAlt]
genLambdaCaseAltsWith allowTHQuotes = smallList0 (genLambdaCaseAltWith allowTHQuotes)

genLambdaCaseAltWith :: Bool -> Gen LambdaCaseAlt
genLambdaCaseAltWith allowTHQuotes = scale (`div` 2) $ do
  LambdaCaseAlt [] <$> genPatterns <*> genRhsWith allowTHQuotes

genRhsWith :: Bool -> Gen (Rhs Expr)
genRhsWith allowTHQuotes =
  oneof
    [ UnguardedRhs [] <$> genExprWith allowTHQuotes <*> genWhereDecls,
      GuardedRhss [] <$> genGuardedRhsListWith allowTHQuotes <*> genWhereDecls
    ]

genCmdRhsWith :: Bool -> Gen (Rhs Cmd)
genCmdRhsWith allowTHQuotes =
  oneof
    [ UnguardedRhs [] <$> genCmdWith allowTHQuotes <*> genWhereDecls,
      GuardedRhss [] <$> genCmdGuardedRhsListWith allowTHQuotes <*> genWhereDecls
    ]

genGuardedRhsListWith :: Bool -> Gen [GuardedRhs Expr]
genGuardedRhsListWith allowTHQuotes = smallList1 (genGuardedRhsWith allowTHQuotes)

genGuardedRhsWith :: Bool -> Gen (GuardedRhs Expr)
genGuardedRhsWith allowTHQuotes = GuardedRhs [] <$> smallList1 (genGuardQualifierWith allowTHQuotes) <*> genExprWith allowTHQuotes

genCmdGuardedRhsListWith :: Bool -> Gen [GuardedRhs Cmd]
genCmdGuardedRhsListWith allowTHQuotes = smallList1 (genCmdGuardedRhsWith allowTHQuotes)

genCmdGuardedRhsWith :: Bool -> Gen (GuardedRhs Cmd)
genCmdGuardedRhsWith allowTHQuotes = GuardedRhs [] <$> smallList1 (genGuardQualifierWith allowTHQuotes) <*> genCmdWith allowTHQuotes

-- | Generate a guard qualifier.
genGuardQualifierWith :: Bool -> Gen GuardQualifier
genGuardQualifierWith allowTHQuotes =
  oneof
    [ -- Boolean guard: | expr = ...
      GuardExpr <$> genExprWith allowTHQuotes,
      -- Pattern guard: | pat <- expr = ...
      -- The guarded-qualifier parser now accepts the full pattern generator,
      -- which includes parenthesized view patterns such as `(view -> pat)`.
      scale (`div` 2) (GuardPat <$> genPattern <*> genExprWith allowTHQuotes),
      -- Let guard: | let decls = ...
      GuardLet <$> genValueDeclsWith allowTHQuotes
    ]

-- | Generate value declarations for let/where.
-- Produces a mix of simple pattern bindings (@x = expr@) and function bindings
-- (@f pat ... = expr@ or @f pat ... | guard = expr@), mirroring the parser
-- which creates each equation as a separate 'FunctionBind' with a single
-- 'Match'.
genValueDeclsWith :: Bool -> Gen [Decl]
genValueDeclsWith allowTHQuotes = smallList0 (genValueDeclWith allowTHQuotes)

-- | Generate a single value declaration: either a simple pattern binding or a
-- function binding with argument patterns and optional guards.
genValueDeclWith :: Bool -> Gen Decl
genValueDeclWith allowTHQuotes =
  DeclValue
    <$> oneof
      [ genPatternBindDecl allowTHQuotes,
        genFunctionBindDecl allowTHQuotes
      ]

-- | Generate a pattern binding: @pat = expr@ or @pat | guard = expr@.
-- The pattern can be any pattern (bang, as, irrefutable, etc.) and the RHS
-- can be guarded, matching what GHC accepts.
genPatternBindDecl :: Bool -> Gen ValueDecl
genPatternBindDecl allowTHQuotes = PatternBind <$> genPattern <*> genRhsWith allowTHQuotes

-- | Generate a function binding: @f pat ... = expr@ or @f pat ... | guard = expr@.
-- Produces a single 'Match', consistent with the parser which creates one
-- 'FunctionBind' per equation.
genFunctionBindDecl :: Bool -> Gen ValueDecl
genFunctionBindDecl allowTHQuotes = do
  name <- genVarUnqualifiedName
  pats <- smallList1 genPattern
  rhs <- genRhsWith allowTHQuotes
  pure
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

genDoFlavor :: Gen DoFlavor
genDoFlavor = elements [DoPlain, DoMdo]

genDoStmtsWith :: Bool -> Gen [DoStmt Expr]
genDoStmtsWith allowTHQuotes = do
  stmts <- smallList0 (genDoStmtWith allowTHQuotes)
  -- Last statement must be DoExpr
  lastExpr <- genExprWith allowTHQuotes
  pure (stmts <> [DoExpr lastExpr])

genDoStmtWith :: Bool -> Gen (DoStmt Expr)
genDoStmtWith allowTHQuotes =
  scale (`div` 2) $
    oneof
      [ DoBind <$> genPattern <*> genExprWith allowTHQuotes,
        DoLetDecls <$> genValueDeclsWith allowTHQuotes,
        DoExpr <$> genExprWith allowTHQuotes,
        DoRecStmt <$> genRecDoStmtsWith allowTHQuotes
      ]

-- | Generate statements for a @rec@ block inside @mdo@/@do@.
-- At least one statement is required.
genRecDoStmtsWith :: Bool -> Gen [DoStmt Expr]
genRecDoStmtsWith allowTHQuotes =
  scale (`div` 2) $
    smallList1 $
      oneof
        [ DoBind <$> genPattern <*> genExprWith allowTHQuotes,
          DoLetDecls <$> genValueDeclsWith allowTHQuotes,
          DoExpr <$> genExprWith allowTHQuotes
        ]

genCompStmtsWith :: Bool -> Gen [CompStmt]
genCompStmtsWith allowTHQuotes = smallList1 (genCompStmtWith allowTHQuotes)

genCompStmtWith :: Bool -> Gen CompStmt
genCompStmtWith allowTHQuotes =
  scale (`div` 2) $
    oneof
      [ CompGen <$> genPattern <*> genExprWith allowTHQuotes,
        CompGuard <$> genExprWith allowTHQuotes,
        CompLetDecls <$> genValueDeclsWith allowTHQuotes
      ]

genParallelCompStmtsWith :: Bool -> Gen [[CompStmt]]
genParallelCompStmtsWith allowTHQuotes = do
  count <- chooseInt (2, 3)
  scale (`div` count) $ vectorOf count (genCompStmtsWith allowTHQuotes)

genListElemsWith :: Bool -> Gen [Expr]
genListElemsWith allowTHQuotes = smallList0 (genExprWith allowTHQuotes)

-- | Generate tuple elements
genTupleElemsWith :: Bool -> Gen [Expr]
genTupleElemsWith allowTHQuotes =
  oneof
    [ pure [],
      do
        count <- chooseInt (2, 4)
        scale (`div` count) $ vectorOf count (genExprWith allowTHQuotes)
    ]

-- | Generate elements for an unboxed tuple (0-4 elements).
-- Unlike boxed tuples, unboxed tuples with 0 elements are valid Haskell.
-- Sometimes generates layout-sensitive expressions (case, do, if, let, lambda)
-- to ensure proper implicit layout handling with unboxed tuple delimiters.
genUnboxedTupleElemsWith :: Bool -> Gen [Expr]
genUnboxedTupleElemsWith allowTHQuotes =
  oneof
    [ smallList0 (genExprWith allowTHQuotes),
      smallList0 (genLayoutExprWith allowTHQuotes)
    ]

-- | Generate expressions that trigger implicit layout (case, do, if, let, lambda).
-- These require special handling when nested inside delimiters like @(# ... #)@.
genLayoutExprWith :: Bool -> Gen Expr
genLayoutExprWith allowTHQuotes =
  scale (`div` 2) $
    oneof
      [ ECase <$> genExprWith allowTHQuotes <*> genCaseAltsWith allowTHQuotes,
        EDo <$> genDoStmtsWith allowTHQuotes <*> genDoFlavor,
        EIf <$> genExprWith allowTHQuotes <*> genExprWith allowTHQuotes <*> genExprWith allowTHQuotes,
        ELetDecls <$> genValueDeclsWith allowTHQuotes <*> genExprWith allowTHQuotes,
        ELambdaPats <$> genPatterns <*> genExprWith allowTHQuotes,
        ELambdaCase <$> genCaseAltsWith allowTHQuotes,
        ELambdaCases <$> genLambdaCaseAltsWith allowTHQuotes
      ]

genUnboxedSumExprWith :: Bool -> Gen Expr
genUnboxedSumExprWith allowTHQuotes = do
  arity <- chooseInt (2, 4)
  altIdx <- chooseInt (0, arity - 1)
  inner <- genExprWith allowTHQuotes
  pure (EUnboxedSum altIdx arity inner)

genTupleSectionElemsWith :: Bool -> Gen [Maybe Expr]
genTupleSectionElemsWith allowTHQuotes = do
  count <- chooseInt (2, 4)
  elems <- scale (`div` count) $ vectorOf count (genMaybeExprWith allowTHQuotes)
  -- Ensure at least one Nothing (otherwise it's just a tuple)
  if Nothing `notElem` elems
    then do
      idx <- chooseInt (0, count - 1)
      pure (take idx elems <> [Nothing] <> drop (idx + 1) elems)
    else pure elems

genMaybeExprWith :: Bool -> Gen (Maybe Expr)
genMaybeExprWith allowTHQuotes =
  oneof
    [ Just <$> genExprWith allowTHQuotes,
      pure Nothing
    ]

genArithSeqWith :: Bool -> Gen ArithSeq
genArithSeqWith allowTHQuotes =
  scale (`div` 2) $
    oneof
      [ ArithSeqFrom <$> genExprWith allowTHQuotes,
        ArithSeqFromThen <$> genExprWith allowTHQuotes <*> genExprWith allowTHQuotes,
        ArithSeqFromTo <$> genExprWith allowTHQuotes <*> genExprWith allowTHQuotes,
        ArithSeqFromThenTo <$> genExprWith allowTHQuotes <*> genExprWith allowTHQuotes <*> genExprWith allowTHQuotes
      ]

genRecordFieldsWith :: Bool -> Gen [RecordField Expr]
genRecordFieldsWith allowTHQuotes =
  smallList0 $
    RecordField <$> genVarName <*> genExprWith allowTHQuotes <*> pure False

-- | Generate a type (simple version for use inside expressions).
genTypeWith :: Bool -> Gen Type
genTypeWith allowTHQuotes = do
  n <- getSize
  if n <= 0
    then genTypeLeaf
    else
      scale (`div` 2) $
        oneof
          [ genTypeLeaf,
            TApp <$> genTypeWith allowTHQuotes <*> genTypeWith allowTHQuotes,
            TFun <$> genTypeWith allowTHQuotes <*> genTypeWith allowTHQuotes,
            TList Unpromoted <$> genTypeListElemsWith allowTHQuotes,
            TTuple Boxed Unpromoted <$> genTypeTupleElemsWith allowTHQuotes,
            TParen <$> genTypeWith allowTHQuotes
          ]

genTypeLeaf :: Gen Type
genTypeLeaf =
  oneof
    [ TVar <$> genTypeVarName,
      (`TCon` Unpromoted) <$> genConName
    ]

genTypeTupleElemsWith :: Bool -> Gen [Type]
genTypeTupleElemsWith allowTHQuotes =
  oneof
    [ pure [],
      do
        count <- chooseInt (2, 3)
        scale (`div` count) $ vectorOf count (genTypeWith allowTHQuotes)
    ]

genTypeListElemsWith :: Bool -> Gen [Type]
genTypeListElemsWith allowTHQuotes = do
  count <- chooseInt (1, 4)
  scale (`div` count) $ vectorOf count (genTypeWith allowTHQuotes)

genTypeVarName :: Gen UnqualifiedName
genTypeVarName = mkUnqualifiedName NameVarId <$> genVarId

-- | Literal expression constructors
mkHexExpr :: Integer -> Expr
mkHexExpr value = EInt value TInteger ("0x" <> T.pack (showHex value))

mkFloatExpr :: Rational -> Expr
mkFloatExpr value = EFloat value TFractional (renderFloat value)

mkCharExpr :: Char -> Expr
mkCharExpr value = EChar value (T.pack (show value))

mkStringExpr :: Text -> Expr
mkStringExpr value = EString value (T.pack (show (T.unpack value)))

-- | Create an integer expression with canonical representation.
mkIntExpr :: Integer -> Expr
mkIntExpr value = EInt value TInteger (T.pack (show value))

renderFloat :: Rational -> T.Text
renderFloat value = T.pack (show (fromRational value :: Double))

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
    EVar name -> [EVar name' | name' <- shrinkName name]
    ETypeSyntax form ty -> [ETypeSyntax form ty' | ty' <- shrinkType ty]
    EInt value _ _ -> [mkIntExpr shrunk | shrunk <- shrinkIntegral value]
    EFloat value _ _ -> [mkFloatExpr shrunk | shrunk <- shrinkFloat value]
    EChar {} -> []
    ECharHash {} -> []
    EString value _ -> [mkStringExpr (T.pack shrunk) | shrunk <- shrink (T.unpack value)]
    EStringHash value _ -> [EStringHash (T.pack shrunk) (T.pack (show shrunk) <> "#") | shrunk <- shrink (T.unpack value)]
    EOverloadedLabel value raw ->
      [EOverloadedLabel (T.pack shrunk) ("#" <> T.pack shrunk) | shrunk <- shrinkOverloadedLabel value raw]
    EPragma pragma inner -> inner : [EPragma pragma inner' | inner' <- shrinkExpr inner]
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
        <> [EInfix lhs op' rhs | op' <- shrinkName op]
    ENegate inner -> inner : [ENegate inner' | inner' <- shrinkExpr inner]
    ESectionL inner op ->
      inner
        : [ESectionL inner' op | inner' <- shrinkExpr inner]
          <> [ESectionL inner op' | op' <- shrinkName op]
    ESectionR op inner ->
      inner
        : [ESectionR op inner' | inner' <- shrinkExpr inner]
          <> [ESectionR op' inner | op' <- shrinkName op]
    EIf cond thenE elseE ->
      [thenE, elseE]
        <> [EIf cond' thenE elseE | cond' <- shrinkExpr cond]
        <> [EIf cond thenE' elseE | thenE' <- shrinkExpr thenE]
        <> [EIf cond thenE elseE' | elseE' <- shrinkExpr elseE]
    EMultiWayIf rhss ->
      [guardedRhsBody grhs | grhs : _ <- [rhss]]
        <> [EMultiWayIf rhss' | rhss' <- shrinkList shrinkGuardedRhs rhss, not (null rhss')]
    ECase scrutinee alts ->
      scrutinee
        : [ECase scrutinee' alts | scrutinee' <- shrinkExpr scrutinee]
          <> [ECase scrutinee alts' | alts' <- shrinkCaseAlts alts, not (null alts')]
    ELambdaPats pats body ->
      body
        : [ELambdaPats pats body' | body' <- shrinkExpr body]
          <> [ELambdaPats pats' body | pats' <- shrinkList shrinkPattern pats, not (null pats')]
    ELambdaCase alts ->
      [ELambdaCase alts' | alts' <- shrinkCaseAlts alts, not (null alts')]
    ELambdaCases alts ->
      [ELambdaCases alts' | alts' <- shrinkLambdaCaseAlts alts, not (null alts')]
    ELetDecls decls body ->
      body
        : [ELetDecls decls body' | body' <- shrinkExpr body]
          <> [ELetDecls decls' body | decls' <- shrinkDecls decls, not (null decls')]
    EDo stmts flavor ->
      [EDo stmts' flavor | stmts' <- shrinkDoStmts stmts, not (null stmts')]
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
      [ERecordCon con' fields False | con' <- shrinkName con]
        <> [ERecordCon con fields' False | fields' <- shrinkRecordFields fields]
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
    ETHNameQuote e -> [ETHNameQuote e' | e' <- shrinkExpr e]
    ETHTypeNameQuote ty -> [ETHTypeNameQuote ty' | ty' <- shrinkType ty]
    ETHSplice body -> body : [ETHSplice body' | body' <- shrinkExpr body]
    ETHTypedSplice body -> body : [ETHTypedSplice body' | body' <- shrinkExpr body]
    EProc pat body ->
      [EProc pat' body | pat' <- shrinkPattern pat]
        <> [EProc pat body' | body' <- shrinkCmd body]
    EAnn _ sub -> shrinkExpr sub

shrinkCmd :: Cmd -> [Cmd]
shrinkCmd cmd =
  case peelCmdAnn cmd of
    CmdAnn _ inner -> shrinkCmd inner
    CmdArrApp lhs appTy rhs ->
      [CmdArrApp lhs' appTy rhs | lhs' <- shrinkExpr lhs]
        <> [CmdArrApp lhs appTy rhs' | rhs' <- shrinkExpr rhs]
    CmdInfix lhs op rhs ->
      [lhs, rhs]
        <> [CmdInfix lhs' op rhs | lhs' <- shrinkCmd lhs]
        <> [CmdInfix lhs op' rhs | op' <- shrinkName op]
        <> [CmdInfix lhs op rhs' | rhs' <- shrinkCmd rhs]
    CmdDo stmts -> [CmdDo stmts' | stmts' <- shrinkCmdDoStmts stmts, not (null stmts')]
    CmdIf cond yes no ->
      [yes, no]
        <> [CmdIf cond' yes no | cond' <- shrinkExpr cond]
        <> [CmdIf cond yes' no | yes' <- shrinkCmd yes]
        <> [CmdIf cond yes no' | no' <- shrinkCmd no]
    CmdCase scrut alts ->
      [CmdCase scrut' alts | scrut' <- shrinkExpr scrut]
        <> [CmdCase scrut alts' | alts' <- shrinkCmdCaseAlts alts, not (null alts')]
    CmdLet decls body ->
      body
        : [CmdLet decls' body | decls' <- shrinkDecls decls, not (null decls')]
          <> [CmdLet decls body' | body' <- shrinkCmd body]
    CmdLam pats body ->
      body
        : [CmdLam pats' body | pats' <- shrinkList shrinkPattern pats, not (null pats')]
          <> [CmdLam pats body' | body' <- shrinkCmd body]
    CmdApp cmd' expr ->
      cmd'
        : [CmdApp cmd'' expr | cmd'' <- shrinkCmd cmd']
          <> [CmdApp cmd' expr' | expr' <- shrinkExpr expr]
    CmdPar inner -> inner : [CmdPar inner' | inner' <- shrinkCmd inner]

shrinkCmdDoStmts :: [DoStmt Cmd] -> [[DoStmt Cmd]]
shrinkCmdDoStmts = shrinkList shrinkCmdDoStmt

shrinkCmdDoStmt :: DoStmt Cmd -> [DoStmt Cmd]
shrinkCmdDoStmt stmt =
  case peelDoStmtAnn stmt of
    DoBind pat cmd ->
      [DoBind pat' cmd | pat' <- shrinkPattern pat]
        <> [DoBind pat cmd' | cmd' <- shrinkCmd cmd]
    DoLetDecls decls -> [DoLetDecls decls' | decls' <- shrinkDecls decls, not (null decls')]
    DoExpr cmd -> [DoExpr cmd' | cmd' <- shrinkCmd cmd]
    DoRecStmt stmts -> [DoRecStmt stmts' | stmts' <- shrinkCmdDoStmts stmts, not (null stmts')]
    DoAnn _ _ -> []

shrinkCmdCaseAlts :: [CaseAlt Cmd] -> [[CaseAlt Cmd]]
shrinkCmdCaseAlts = shrinkList shrinkCmdCaseAlt

shrinkCmdCaseAlt :: CaseAlt Cmd -> [CaseAlt Cmd]
shrinkCmdCaseAlt alt =
  [alt {caseAltPattern = pat'} | pat' <- shrinkPattern (caseAltPattern alt)]
    <> [alt {caseAltRhs = rhs'} | rhs' <- shrinkCmdRhs (caseAltRhs alt)]

shrinkCaseAlts :: [CaseAlt Expr] -> [[CaseAlt Expr]]
shrinkCaseAlts = shrinkList shrinkCaseAlt

shrinkLambdaCaseAlts :: [LambdaCaseAlt] -> [[LambdaCaseAlt]]
shrinkLambdaCaseAlts = shrinkList shrinkLambdaCaseAlt

shrinkCaseAlt :: CaseAlt Expr -> [CaseAlt Expr]
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

shrinkGuardedRhs :: GuardedRhs Expr -> [GuardedRhs Expr]
shrinkGuardedRhs grhs =
  [grhs {guardedRhsBody = body'} | body' <- shrinkExpr (guardedRhsBody grhs)]
    <> [grhs {guardedRhsGuards = gs'} | gs' <- shrinkList shrinkGuardQualifier (guardedRhsGuards grhs), not (null gs')]

shrinkCmdGuardedRhs :: GuardedRhs Cmd -> [GuardedRhs Cmd]
shrinkCmdGuardedRhs grhs =
  [grhs {guardedRhsBody = body'} | body' <- shrinkCmd (guardedRhsBody grhs)]
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
shrinkLetRhs :: Rhs Expr -> [Rhs Expr]
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

shrinkCmdRhs :: Rhs Cmd -> [Rhs Cmd]
shrinkCmdRhs rhs =
  case rhs of
    UnguardedRhs _ body mWhere ->
      [UnguardedRhs [] body Nothing | isJust mWhere]
        <> [UnguardedRhs [] body' mWhere | body' <- shrinkCmd body]
        <> [UnguardedRhs [] body (Just ds') | Just ds <- [mWhere], ds' <- shrinkDecls ds, not (null ds')]
    GuardedRhss _ rhss mWhere ->
      [UnguardedRhs [] (guardedRhsBody firstRhs) Nothing | firstRhs : _ <- [rhss]]
        <> [GuardedRhss [] rhss Nothing | isJust mWhere]
        <> [GuardedRhss [] rhss' mWhere | rhss' <- shrinkList shrinkCmdGuardedRhs rhss, not (null rhss')]
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
    CompThen f -> [CompThen f' | f' <- shrinkExpr f]
    CompThenBy f e -> [CompThenBy f' e | f' <- shrinkExpr f] <> [CompThenBy f e' | e' <- shrinkExpr e]
    CompGroupUsing f -> [CompGroupUsing f' | f' <- shrinkExpr f]
    CompGroupByUsing e f -> [CompGroupByUsing e' f | e' <- shrinkExpr e] <> [CompGroupByUsing e f' | f' <- shrinkExpr f]
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

shrinkRecordFields :: [RecordField Expr] -> [[RecordField Expr]]
shrinkRecordFields = shrinkList shrinkRecordField

shrinkRecordField :: RecordField Expr -> [RecordField Expr]
shrinkRecordField field =
  [field {recordFieldName = name'} | name' <- shrinkName (recordFieldName field)]
    <> [field {recordFieldValue = expr'} | expr' <- shrinkExpr (recordFieldValue field)]

instance Arbitrary Expr where
  arbitrary = resize 5 genExpr
  shrink = shrinkExpr
