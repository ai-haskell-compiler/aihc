{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.Arb.Expr
  ( genExpr,
    genRhs,
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
import Test.Properties.Arb.Decl (genDeclValue, genWhereDecls, shrinkFunctionHeadPats)
import Test.Properties.Arb.Identifiers
  ( genCharValue,
    genConName,
    genFieldName,
    genModuleQualifier,
    genStringValue,
    genTenths,
    genVarId,
    genVarName,
    showHex,
    shrinkFloat,
    shrinkName,
    shrinkUnqualifiedName,
  )
import Test.Properties.Arb.Pattern (genPattern, shrinkPattern)
import Test.Properties.Arb.Type (genType, shrinkType)
import Test.Properties.Arb.Utils
import Test.QuickCheck

-- | Generate a random expression. Uses QuickCheck's size parameter
-- to control recursion depth.
genExpr :: Gen Expr
genExpr = scale (`div` 2) $ do
  n <- getSize
  if n <= 0 then genExprLeaf else oneof (baseGenerators <> quoteGenerators)
  where
    baseGenerators =
      [ -- Leaf expressions
        genExprLeaf,
        -- Recursive expressions (reduce size for subexpressions)
        EApp <$> genExpr <*> genExpr,
        EInfix <$> genExpr <*> genVarName <*> genExpr,
        ENegate <$> genExpr,
        ESectionL <$> genExpr <*> genVarName,
        ESectionR <$> genVarName <*> genExpr,
        EIf <$> genExpr <*> genExpr <*> genExpr,
        EMultiWayIf <$> genGuardedRhsListWith,
        ECase <$> genExpr <*> genCaseAltsWith,
        ELambdaPats <$> genPatterns <*> genExpr,
        ELambdaCase <$> genCaseAltsWith,
        ELambdaCases <$> genLambdaCaseAltsWith,
        ELetDecls <$> smallList0 (DeclValue <$> genDeclValue) <*> genExpr,
        EDo <$> genDoStmtsWith <*> genDoFlavor,
        EListComp <$> genExpr <*> genCompStmtsWith,
        EListCompParallel <$> genExpr <*> genParallelCompStmtsWith,
        EList <$> genListElemsWith,
        ETuple Boxed . map Just <$> genTupleElemsWith,
        ETuple Unboxed . map Just <$> genUnboxedTupleElemsWith,
        ETuple Boxed <$> genTupleSectionElemsWith,
        ETuple Unboxed <$> genTupleSectionElemsWith,
        genUnboxedSumExprWith,
        EArithSeq <$> genArithSeqWith,
        ERecordCon <$> genConName <*> genRecordFieldsWith <*> pure False,
        ERecordUpd <$> genExpr <*> genRecordFieldsWith,
        ETypeSig <$> genExpr <*> genType,
        ETypeApp <$> genExpr <*> genType,
        EParen <$> genExpr,
        EProc <$> genPattern <*> genCmdWith,
        -- OverloadedRecordDot
        EGetField <$> genExpr <*> genRecordFieldName,
        EGetFieldProjection <$> smallList1 genRecordFieldName,
        -- Template Haskell splices are valid inside quote bodies.
        ETHSplice <$> genSpliceBody,
        ETHTypedSplice <$> genTypedSpliceBody
      ]
    quoteGenerators =
      [ ETHExpQuote <$> genExpr,
        ETHTypedQuote <$> genExpr,
        ETHDeclQuote <$> smallList0 (DeclValue <$> genDeclValue),
        ETHPatQuote <$> genPattern,
        ETHTypeQuote <$> genType,
        ETHNameQuote <$> genNameQuoteExpr,
        ETHTypeNameQuote <$> genTypeNameQuoteType
      ]

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
genQuasiQuoteName = suchThat genVarId isValidQuasiQuoteName

isValidQuasiQuoteName :: Text -> Bool
isValidQuasiQuoteName name = name `notElem` ["e", "d", "p", "t", ""] && not (T.isSuffixOf "#" name)

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

genCmdWith :: Gen Cmd
genCmdWith = scale (`div` 2) $ do
  n <- getSize
  if n <= 0 then genCmdLeafWith else oneof (genCmdLeafWith : genCmdRecursiveWith)

genCmdLeafWith :: Gen Cmd
genCmdLeafWith =
  CmdArrApp <$> genExpr <*> genArrAppType <*> genExpr

genCmdRecursiveWith :: [Gen Cmd]
genCmdRecursiveWith =
  [ CmdInfix <$> genCmdWith <*> genVarName <*> genCmdWith,
    CmdDo <$> genCmdDoStmtsWith,
    CmdIf <$> genExpr <*> genCmdWith <*> genCmdWith,
    CmdCase <$> genExpr <*> genCmdCaseAltsWith,
    CmdLet <$> smallList0 (DeclValue <$> genDeclValue) <*> genCmdWith,
    CmdLam <$> genPatterns <*> genCmdWith,
    CmdPar <$> genCmdWith
  ]

genArrAppType :: Gen ArrAppType
genArrAppType = elements [HsFirstOrderApp, HsHigherOrderApp]

genCmdCaseAltsWith :: Gen [CaseAlt Cmd]
genCmdCaseAltsWith = smallList1 genCmdCaseAltWith

genCmdCaseAltWith :: Gen (CaseAlt Cmd)
genCmdCaseAltWith = scale (`div` 2) $ do
  CaseAlt [] <$> genPattern <*> genCmdRhsWith

genCmdDoStmtsWith :: Gen [DoStmt Cmd]
genCmdDoStmtsWith = smallList1 genCmdDoStmtWith

genCmdDoStmtWith :: Gen (DoStmt Cmd)
genCmdDoStmtWith =
  scale (`div` 2) . oneof $
    [ DoBind <$> genPattern <*> genCmdWith,
      DoLetDecls <$> smallList0 (DeclValue <$> genDeclValue),
      DoExpr <$> genCmdWith,
      DoRecStmt <$> genCmdRecDoStmtsWith
    ]

genCmdRecDoStmtsWith :: Gen [DoStmt Cmd]
genCmdRecDoStmtsWith =
  scale (`div` 2) . smallList1 $
    oneof
      [ DoBind <$> genPattern <*> genCmdWith,
        DoLetDecls <$> smallList0 (DeclValue <$> genDeclValue),
        DoExpr <$> genCmdWith
      ]

genCaseAltsWith :: Gen [CaseAlt Expr]
genCaseAltsWith = smallList0 genCaseAltWith

genCaseAltWith :: Gen (CaseAlt Expr)
genCaseAltWith = CaseAlt [] <$> genPattern <*> genRhs

genLambdaCaseAltsWith :: Gen [LambdaCaseAlt]
genLambdaCaseAltsWith = smallList0 genLambdaCaseAltWith

genLambdaCaseAltWith :: Gen LambdaCaseAlt
genLambdaCaseAltWith = scale (`div` 2) $ do
  LambdaCaseAlt [] <$> genPatterns <*> genRhs

genRhs :: Gen (Rhs Expr)
genRhs =
  oneof
    [ UnguardedRhs [] <$> genExpr <*> genWhereDecls,
      GuardedRhss [] <$> genGuardedRhsListWith <*> genWhereDecls
    ]

genCmdRhsWith :: Gen (Rhs Cmd)
genCmdRhsWith =
  oneof
    [ UnguardedRhs [] <$> genCmdWith <*> genWhereDecls,
      GuardedRhss [] <$> genCmdGuardedRhsListWith <*> genWhereDecls
    ]

genGuardedRhsListWith :: Gen [GuardedRhs Expr]
genGuardedRhsListWith = smallList1 genGuardedRhsWith

genGuardedRhsWith :: Gen (GuardedRhs Expr)
genGuardedRhsWith = GuardedRhs [] <$> smallList1 genGuardQualifierWith <*> genExpr

genCmdGuardedRhsListWith :: Gen [GuardedRhs Cmd]
genCmdGuardedRhsListWith = smallList1 genCmdGuardedRhsWith

genCmdGuardedRhsWith :: Gen (GuardedRhs Cmd)
genCmdGuardedRhsWith = GuardedRhs [] <$> smallList1 genGuardQualifierWith <*> genCmdWith

-- | Generate a guard qualifier.
genGuardQualifierWith :: Gen GuardQualifier
genGuardQualifierWith =
  oneof
    [ -- Boolean guard: | expr = ...
      GuardExpr <$> genExpr,
      -- Pattern guard: | pat <- expr = ...
      -- The guarded-qualifier parser now accepts the full pattern generator,
      -- which includes parenthesized view patterns such as `(view -> pat)`.
      scale (`div` 2) (GuardPat <$> genPattern <*> genExpr),
      -- Let guard: | let decls = ...
      GuardLet <$> smallList0 (DeclValue <$> genDeclValue)
    ]

genDoFlavor :: Gen DoFlavor
genDoFlavor =
  oneof
    [ pure DoPlain,
      pure DoMdo,
      DoQualified <$> genModuleQualifier,
      DoQualifiedMdo <$> genModuleQualifier
    ]

genDoStmtsWith :: Gen [DoStmt Expr]
genDoStmtsWith = do
  stmts <- smallList0 genDoStmtWith
  -- Last statement must be DoExpr
  lastExpr <- genExpr
  pure (stmts <> [DoExpr lastExpr])

genDoStmtWith :: Gen (DoStmt Expr)
genDoStmtWith =
  scale (`div` 2) $
    oneof
      [ DoBind <$> genPattern <*> genExpr,
        DoLetDecls <$> smallList0 (DeclValue <$> genDeclValue),
        DoExpr <$> genExpr,
        DoRecStmt <$> genRecDoStmtsWith
      ]

-- | Generate statements for a @rec@ block inside @mdo@/@do@.
-- At least one statement is required.
genRecDoStmtsWith :: Gen [DoStmt Expr]
genRecDoStmtsWith =
  scale (`div` 2) $
    smallList1 $
      oneof
        [ DoBind <$> genPattern <*> genExpr,
          DoLetDecls <$> smallList0 (DeclValue <$> genDeclValue),
          DoExpr <$> genExpr
        ]

genCompStmtsWith :: Gen [CompStmt]
genCompStmtsWith = smallList1 genCompStmtWith

genCompStmtWith :: Gen CompStmt
genCompStmtWith =
  scale (`div` 2) $
    oneof
      [ CompGen <$> genPattern <*> genExpr,
        CompGuard <$> genExpr,
        CompLetDecls <$> smallList0 (DeclValue <$> genDeclValue),
        CompThen <$> genExpr,
        CompThenBy <$> genExpr <*> genExpr,
        CompGroupUsing <$> genExpr,
        CompGroupByUsing <$> genExpr <*> genExpr
      ]

genParallelCompStmtsWith :: Gen [[CompStmt]]
genParallelCompStmtsWith = smallList2 genCompStmtsWith

genListElemsWith :: Gen [Expr]
genListElemsWith = smallList0 genExpr

-- | Generate tuple elements
genTupleElemsWith :: Gen [Expr]
genTupleElemsWith = smallList2 genExpr

-- | Generate elements for an unboxed tuple (0-4 elements).
-- Unlike boxed tuples, unboxed tuples with 0 elements are valid Haskell.
-- Sometimes generates layout-sensitive expressions (case, do, if, let, lambda)
-- to ensure proper implicit layout handling with unboxed tuple delimiters.
genUnboxedTupleElemsWith :: Gen [Expr]
genUnboxedTupleElemsWith =
  smallList0 genExpr

genUnboxedSumExprWith :: Gen Expr
genUnboxedSumExprWith = do
  arity <- chooseInt (2, 4)
  altIdx <- chooseInt (0, arity - 1)
  EUnboxedSum altIdx arity <$> genExpr

genTupleSectionElemsWith :: Gen [Maybe Expr]
genTupleSectionElemsWith = do
  elems <- smallList2 genMaybeExprWith
  -- Ensure at least one Nothing (otherwise it's just a tuple)
  if Nothing `notElem` elems
    then do
      idx <- chooseInt (0, length elems - 1)
      pure (take idx elems <> [Nothing] <> drop (idx + 1) elems)
    else pure elems

genMaybeExprWith :: Gen (Maybe Expr)
genMaybeExprWith =
  optional genExpr

genArithSeqWith :: Gen ArithSeq
genArithSeqWith =
  scale (`div` 2) $
    oneof
      [ ArithSeqFrom <$> genExpr,
        ArithSeqFromThen <$> genExpr <*> genExpr,
        ArithSeqFromTo <$> genExpr <*> genExpr,
        ArithSeqFromThenTo <$> genExpr <*> genExpr <*> genExpr
      ]

genRecordFieldsWith :: Gen [RecordField Expr]
genRecordFieldsWith =
  smallList0 $
    RecordField <$> genVarName <*> genExpr <*> pure False

-- | Generate a field name for OverloadedRecordDot.
-- Uses an unqualified variable name (field names are always unqualified).
genRecordFieldName :: Gen Name
genRecordFieldName = qualifyName Nothing . mkUnqualifiedName NameVarId <$> genFieldName

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
      [ T.unpack candidate
      | candidate <-
          ["a"]
            <> [T.map replaceUnicode value | T.any (> '\x7f') value]
            <> map T.pack (shrink (T.unpack value)),
        candidate /= value,
        isValidLabelName candidate
      ]
  | otherwise = []
  where
    replaceUnicode c = if c > '\x7f' then 'a' else c
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
      [EVar (qualifyName Nothing (mkUnqualifiedName NameVarId "a"))]
        <> [EQuasiQuote quoter' body | quoter' <- "q" : T.inits quoter, isValidQuasiQuoteName quoter', quoter' /= quoter]
        <> [EQuasiQuote quoter (T.pack shrunk) | shrunk <- shrink (T.unpack body)]
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
      [body | CaseAlt {caseAltRhs = UnguardedRhs _ body _} <- alts]
        <> [ELambdaCase alts' | alts' <- shrinkCaseAlts alts, not (null alts')]
    ELambdaCases alts ->
      [body | LambdaCaseAlt {lambdaCaseAltRhs = UnguardedRhs _ body _} <- alts]
        <> [ELambdaCases alts' | alts' <- shrinkLambdaCaseAlts alts, not (null alts')]
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
      [ETuple Boxed elems | tupleFlavor == Unboxed, length elems /= 1]
        <> [ETuple tupleFlavor elems' | elems' <- shrinkTupleMaybeElems shrinkMaybeExpr elems]
    EArithSeq seq' ->
      [EArithSeq seq'' | seq'' <- shrinkArithSeq seq']
    ERecordCon con fields _ ->
      [ERecordCon con' fields False | con' <- shrinkName con]
        <> [ERecordCon con fields' False | fields' <- shrinkRecordFields fields]
    ERecordUpd target fields ->
      target
        : [ERecordUpd target' fields | target' <- shrinkExpr target]
          <> [ERecordUpd target fields' | fields' <- shrinkRecordFields fields]
    EGetField base fieldName ->
      base
        : [EGetField base' fieldName | base' <- shrinkExpr base]
          <> [EGetField base fieldName' | fieldName' <- shrinkName fieldName]
    EGetFieldProjection fields ->
      [EGetFieldProjection fields' | fields' <- shrinkList shrinkName fields, not (null fields')]
    ETypeSig inner ty ->
      inner
        : [ETypeSig inner ty' | ty' <- shrinkType ty]
          <> [ETypeSig inner' ty | inner' <- shrinkExpr inner]
    ETypeApp inner ty ->
      inner
        : [ETypeApp inner ty' | ty' <- shrinkType ty]
          <> [ETypeApp inner' ty | inner' <- shrinkExpr inner]
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
  [alt {caseAltPattern = pat'} | pat' <- shrinkPattern (caseAltPattern alt)]
    <> [alt {caseAltRhs = rhs'} | rhs' <- shrinkLetRhs (caseAltRhs alt)]

shrinkLambdaCaseAlt :: LambdaCaseAlt -> [LambdaCaseAlt]
shrinkLambdaCaseAlt alt =
  [alt {lambdaCaseAltPats = pats'} | pats' <- shrinkList shrinkPattern (lambdaCaseAltPats alt), not (null pats')]
    <> [alt {lambdaCaseAltRhs = rhs'} | rhs' <- shrinkLetRhs (lambdaCaseAltRhs alt)]

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
    DeclValue (PatternBind multTag pat rhs) ->
      [DeclValue (PatternBind multTag pat rhs') | rhs' <- shrinkLetRhs rhs]
        <> [DeclValue (PatternBind multTag pat' rhs) | pat' <- shrinkPattern pat]
    DeclValue (FunctionBind name matches) ->
      -- Shrink multiple matches to a single match
      [DeclValue (FunctionBind name [m {matchAnns = []}]) | length matches > 1, m <- matches]
        -- Shrink individual matches
        <> [DeclValue (FunctionBind name ms') | ms' <- shrinkList shrinkLetMatch matches, not (null ms')]
        <> [DeclValue (FunctionBind name' matches) | name' <- shrinkUnqualifiedName name]
        <> [ DeclValue (PatternBind NoMultiplicityTag (PVar name) (matchRhs match))
           | match <- matches
           ]
    DeclTypeSig names ty ->
      [DeclTypeSig names ty' | ty' <- shrinkType ty]
    _ -> []

-- | Shrink a match clause within let/where/TH contexts.
shrinkLetMatch :: Match -> [Match]
shrinkLetMatch match =
  [match {matchAnns = [], matchRhs = rhs'} | rhs' <- shrinkLetRhs (matchRhs match)]
    <> [match {matchAnns = [], matchPats = pats'} | pats' <- shrinkFunctionHeadPats (matchHeadForm match) (matchPats match)]

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
