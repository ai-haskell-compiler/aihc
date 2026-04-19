{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.Arb.Expr
  ( genExpr,
    genRhsWith,
    mkIntExpr,
    shrinkExpr,
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
    genConId,
    genFieldName,
    genModuleQualifier,
    genOptionalQualifier,
    genStringValue,
    genTenths,
    genVarId,
    genVarName,
    genVarSym,
    showHex,
    shrinkFloat,
    shrinkIdent,
  )
import Test.Properties.Arb.Pattern (genPattern, shrinkPattern)
import Test.Properties.Arb.Type (shrinkType)
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
        EInfix <$> genExprWith allowTHQuotes <*> genOperatorName <*> genExprWith allowTHQuotes,
        ENegate <$> genExprWith allowTHQuotes,
        ESectionL <$> genExprWith allowTHQuotes <*> genOperatorName,
        ESectionR <$> genOperatorName <*> genExprWith allowTHQuotes,
        EIf <$> genExprWith allowTHQuotes <*> genExprWith allowTHQuotes <*> genExprWith allowTHQuotes,
        EMultiWayIf <$> genGuardedRhsListWith allowTHQuotes,
        ECase <$> genExprWith allowTHQuotes <*> genCaseAltsWith allowTHQuotes,
        ELambdaPats <$> genPatterns <*> genExprWith allowTHQuotes,
        ELambdaCase <$> genCaseAltsWith allowTHQuotes,
        ELambdaCases <$> genLambdaCaseAltsWith allowTHQuotes,
        ELetDecls <$> genValueDeclsWith allowTHQuotes <*> genExprWith allowTHQuotes,
        EDo <$> genDoStmtsWith allowTHQuotes <*> arbitrary,
        EListComp <$> genExprWith allowTHQuotes <*> genCompStmtsWith allowTHQuotes,
        EListCompParallel <$> genExprWith allowTHQuotes <*> genParallelCompStmtsWith allowTHQuotes,
        EList <$> genListElemsWith allowTHQuotes,
        ETuple Boxed . map Just <$> genTupleElemsWith allowTHQuotes,
        ETuple Unboxed . map Just <$> genUnboxedTupleElemsWith allowTHQuotes,
        ETuple Boxed <$> genTupleSectionElemsWith allowTHQuotes,
        ETuple Unboxed <$> genTupleSectionElemsWith allowTHQuotes,
        genUnboxedSumExprWith allowTHQuotes,
        EArithSeq <$> genArithSeqWith allowTHQuotes,
        (\name fields -> ERecordCon name fields False) <$> genConId <*> genRecordFieldsWith allowTHQuotes,
        ERecordUpd <$> genExprWith allowTHQuotes <*> genRecordFieldsWith allowTHQuotes,
        ETypeSig <$> genExprWith allowTHQuotes <*> genTypeWith allowTHQuotes,
        ETypeApp . canonicalTypeAppExpr <$> genExprWith allowTHQuotes <*> genTypeWith allowTHQuotes,
        EParen <$> genExprWith allowTHQuotes,
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
    [ EVar . qualifyName Nothing . mkUnqualifiedName NameVarId <$> genNameQuoteIdent,
      do
        qual <- genModuleQualifier
        EVar . mkName (Just qual) NameVarId <$> genNameQuoteIdent,
      do
        qual <- genModuleQualifier
        op <- genVarSym `suchThat` notDotLikeForQualifiedOp
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
  ident <- genVarId
  -- If length >= 2 and second char is a quote, regenerate
  if T.length ident >= 2 && T.index ident 1 == '\''
    then genNameQuoteIdent
    else pure ident

-- | Generate a type name for TH type quotes (''Name).
-- Can produce either a constructor name (e.g., ''Maybe) or an operator name (e.g., ''(:>)).
genTypeNameQuote :: Gen Name
genTypeNameQuote =
  oneof
    [ qualifyName Nothing . mkUnqualifiedName NameConId <$> genConId,
      -- Generate operator name for type quotes (use NameVarSym to match lexer behavior)
      qualifyName Nothing . mkUnqualifiedName NameVarSym <$> suchThat genVarSym (/= "*")
    ]

genOperatorName :: Gen Name
genOperatorName = do
  qual <- genOptionalQualifier
  op <- mkUnqualifiedName NameVarSym <$> genVarSym
  pure (qualifyName qual op)

genConAstName :: Gen Name
genConAstName = qualifyName <$> genOptionalQualifier <*> (mkUnqualifiedName NameConId <$> genConId)

-- | Generate simple patterns for lambdas
genPatterns :: Gen [Pattern]
genPatterns = do
  count <- chooseInt (1, 3)
  vectorOf count genPattern

genCaseAltsWith :: Bool -> Gen [CaseAlt]
genCaseAltsWith allowTHQuotes = do
  count <- chooseInt (0, 3)
  vectorOf count (genCaseAltWith allowTHQuotes)

genCaseAltWith :: Bool -> Gen CaseAlt
genCaseAltWith allowTHQuotes = scale (`div` 2) $ do
  pat <- genPattern
  rhs <- genRhsWith allowTHQuotes
  pure $
    CaseAlt
      { caseAltAnns = [],
        caseAltPattern = pat,
        caseAltRhs = rhs
      }

genLambdaCaseAltsWith :: Bool -> Gen [LambdaCaseAlt]
genLambdaCaseAltsWith allowTHQuotes = do
  count <- chooseInt (0, 3)
  vectorOf count (genLambdaCaseAltWith allowTHQuotes)

genLambdaCaseAltWith :: Bool -> Gen LambdaCaseAlt
genLambdaCaseAltWith allowTHQuotes = scale (`div` 2) $ do
  pats <- genPatterns
  rhs <- genRhsWith allowTHQuotes
  pure $
    LambdaCaseAlt
      { lambdaCaseAltAnns = [],
        lambdaCaseAltPats = pats,
        lambdaCaseAltRhs = rhs
      }

genRhsWith :: Bool -> Gen Rhs
genRhsWith allowTHQuotes =
  oneof
    [ UnguardedRhs [] <$> genBindingExprWith allowTHQuotes <*> genWhereDecls,
      GuardedRhss [] <$> genGuardedRhsListWith allowTHQuotes <*> genWhereDecls
    ]

genGuardedRhsListWith :: Bool -> Gen [GuardedRhs]
genGuardedRhsListWith allowTHQuotes = do
  count <- chooseInt (1, 3)
  scale (`div` count) $ vectorOf count (genGuardedRhsWith allowTHQuotes)

genGuardedRhsWith :: Bool -> Gen GuardedRhs
genGuardedRhsWith allowTHQuotes = scale (`div` 2) $ do
  guardCount <- chooseInt (1, 2)
  guards <- vectorOf guardCount (genGuardQualifierWith allowTHQuotes)
  body <- genBindingExprWith allowTHQuotes
  pure $
    GuardedRhs
      { guardedRhsAnns = [],
        guardedRhsGuards = guards,
        guardedRhsBody = body
      }

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
genValueDeclsWith allowTHQuotes = do
  count <- chooseInt (0, 3)
  scale (`div` max 1 count) $ vectorOf count (genValueDeclWith allowTHQuotes)

-- | Generate a single value declaration: either a simple pattern binding or a
-- function binding with argument patterns and optional guards.
genValueDeclWith :: Bool -> Gen Decl
genValueDeclWith allowTHQuotes =
  oneof
    [ genPatternBindDecl allowTHQuotes,
      genFunctionBindDecl allowTHQuotes
    ]

-- | Generate a pattern binding: @pat = expr@ or @pat | guard = expr@.
-- The pattern can be any pattern (bang, as, irrefutable, etc.) and the RHS
-- can be guarded, matching what GHC accepts.
genPatternBindDecl :: Bool -> Gen Decl
genPatternBindDecl allowTHQuotes = scale (`div` 2) $ do
  pat <- genPattern
  rhs <- genRhsWith allowTHQuotes
  pure $ DeclValue (PatternBind pat rhs)

-- | Generate a function binding: @f pat ... = expr@ or @f pat ... | guard = expr@.
-- Produces a single 'Match', consistent with the parser which creates one
-- 'FunctionBind' per equation.
genFunctionBindDecl :: Bool -> Gen Decl
genFunctionBindDecl allowTHQuotes = do
  name <- mkUnqualifiedName NameVarId <$> genVarId
  patCount <- chooseInt (1, 3)
  scale (`div` (patCount + 1)) $ do
    pats <- vectorOf patCount genPattern
    rhs <- genRhsWith allowTHQuotes
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

genBindingExprWith :: Bool -> Gen Expr
genBindingExprWith = genExprWith

genDoStmtsWith :: Bool -> Gen [DoStmt Expr]
genDoStmtsWith allowTHQuotes = do
  count <- chooseInt (1, 3)
  scale (`div` count) $ do
    stmts <- vectorOf (count - 1) (genDoStmtWith allowTHQuotes)
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
genRecDoStmtsWith allowTHQuotes = do
  count <- chooseInt (1, 3)
  scale (`div` count) $
    vectorOf count $
      oneof
        [ scale (`div` 2) (DoBind <$> genPattern <*> genExprWith allowTHQuotes),
          DoLetDecls <$> genValueDeclsWith allowTHQuotes,
          DoExpr <$> genExprWith allowTHQuotes
        ]

genCompStmtsWith :: Bool -> Gen [CompStmt]
genCompStmtsWith allowTHQuotes = do
  count <- chooseInt (1, 3)
  scale (`div` count) $ vectorOf count (genCompStmtWith allowTHQuotes)

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
genListElemsWith allowTHQuotes = do
  count <- chooseInt (0, 4)
  scale (`div` max 1 count) $ vectorOf count (genExprWith allowTHQuotes)

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
genUnboxedTupleElemsWith :: Bool -> Gen [Expr]
genUnboxedTupleElemsWith allowTHQuotes = do
  count <- chooseInt (0, 4)
  scale (`div` max 1 count) $ vectorOf count (genExprWith allowTHQuotes)

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
  oneof
    [ ArithSeqFrom <$> genExprWith allowTHQuotes,
      scale (`div` 2) (ArithSeqFromThen <$> genExprWith allowTHQuotes <*> genExprWith allowTHQuotes),
      scale (`div` 2) (ArithSeqFromTo <$> genExprWith allowTHQuotes <*> genExprWith allowTHQuotes),
      scale (`div` 3) (ArithSeqFromThenTo <$> genExprWith allowTHQuotes <*> genExprWith allowTHQuotes <*> genExprWith allowTHQuotes)
    ]

genRecordFieldsWith :: Bool -> Gen [(Text, Expr)]
genRecordFieldsWith allowTHQuotes = do
  count <- chooseInt (0, 3)
  names <- vectorOf count genFieldName
  exprs <- scale (`div` max 1 count) $ vectorOf count (genExprWith allowTHQuotes)
  quals <- vectorOf count genOptionalQualifier
  let qualifiedNames = zipWith (\q name -> maybe name (<> "." <> name) q) quals names
  pure (zip qualifiedNames exprs)

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
      (`TCon` Unpromoted) <$> genConAstName
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
