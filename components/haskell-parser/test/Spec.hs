{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Data.Data (dataTypeConstrs, dataTypeOf, showConstr, toConstr)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Parser
import Parser.Ast
import Parser.Pretty (prettyExpr, prettyModule, prettyTypeText)
import Parser.Types (ParseResult (..))
import Test.ExtensionMapping.Suite (extensionMappingTests)
import Test.Extensions.Suite (extensionTests)
import Test.H2010.Suite (h2010Tests)
import Test.HackageTester.Suite (hackageTesterTests)
import Test.Lexer.Suite (lexerTests)
import Test.Parser.Suite (parserGoldenTests)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

span0 :: SourceSpan
span0 = noSourceSpan

main :: IO ()
main = buildTests >>= defaultMain

buildTests :: IO TestTree
buildTests = do
  parserGolden <- parserGoldenTests
  h2010 <- h2010Tests
  extensions <- extensionTests
  lexer <- lexerTests
  let hackageTester = hackageTesterTests
  pure $
    testGroup
      "aihc-parser"
      [ parserGolden,
        lexer,
        testGroup
          "parser"
          [ testCase "module parses declaration list" test_moduleParsesDecls,
            testCase "reads header LANGUAGE pragmas" test_readsHeaderLanguagePragmas,
            testCase "reads header LANGUAGE pragmas starting with No" test_readsHeaderLanguagePragmasStartingWithNo,
            testCase "ignores unknown header pragmas" test_ignoresUnknownHeaderPragmas,
            testCase "ignores LANGUAGE pragmas inside comments" test_ignoresLanguagePragmasInsideComments,
            testCase "stops header scan at first module token" test_stopsHeaderScanAtFirstModuleToken
          ],
        testGroup
          "properties"
          [ QC.testProperty "generated expr AST pretty-printer round-trip" prop_exprPrettyRoundTrip,
            QC.testProperty "generated module AST pretty-printer round-trip" prop_modulePrettyRoundTrip,
            QC.testProperty "generated type AST pretty-printer round-trip" prop_typePrettyRoundTrip
          ],
        h2010,
        extensions,
        extensionMappingTests,
        hackageTester
      ]

test_moduleParsesDecls :: Assertion
test_moduleParsesDecls =
  case parseModule defaultConfig "x = if y then z else w" of
    ParseErr err ->
      assertFailure ("expected module parse success, got parse error: " <> errorBundlePretty err)
    ParseOk modu ->
      case moduleDecls modu of
        [ DeclValue _ (FunctionBind _ "x" [Match {matchPats = [], matchRhs = UnguardedRhs _ (EIf _ (EVar _ "y") (EVar _ "z") (EVar _ "w"))}])
          ] ->
            pure ()
        other ->
          assertFailure ("unexpected parsed declarations: " <> show other)

test_readsHeaderLanguagePragmas :: Assertion
test_readsHeaderLanguagePragmas = do
  let source = T.unlines ["{-# LANGUAGE CPP #-}", "{-# LANGUAGE NoCPP #-}", "module M where", "x = 1"]
      exts = readModuleHeaderExtensions source
      expected = [EnableExtension CPP, DisableExtension CPP]
  assertEqual "reads expected module header LANGUAGE settings" expected exts

test_readsHeaderLanguagePragmasStartingWithNo :: Assertion
test_readsHeaderLanguagePragmasStartingWithNo = do
  let source =
        T.unlines
          [ "{-# LANGUAGE NondecreasingIndentation #-}",
            "module M where",
            "x = 1"
          ]
      exts = readModuleHeaderExtensions source
      expected = [EnableExtension NondecreasingIndentation]
  assertEqual "reads LANGUAGE pragmas whose extension name starts with 'No'" expected exts

test_ignoresUnknownHeaderPragmas :: Assertion
test_ignoresUnknownHeaderPragmas = do
  let source =
        T.unlines
          [ "{-# OPTIONS_GHC -Wall -fwarn-tabs -fno-warn-name-shadowing #-}",
            "{-# OPTIONS_HADDOCK hide #-}",
            "{-# LANGUAGE CPP #-}"
          ]
      exts = readModuleHeaderExtensions source
      expected = [EnableExtension CPP]
  assertEqual "ignores unknown header pragmas and reads LANGUAGE" expected exts

test_ignoresLanguagePragmasInsideComments :: Assertion
test_ignoresLanguagePragmasInsideComments = do
  let source =
        T.unlines
          [ "-- line comment {-# LANGUAGE MagicHash #-}",
            "{- block comment {-# LANGUAGE EmptyCase #-} -}",
            "{-# LANGUAGE CPP #-}"
          ]
      exts = readModuleHeaderExtensions source
      expected = [EnableExtension CPP]
  assertEqual "ignores LANGUAGE pragmas in comments" expected exts

test_stopsHeaderScanAtFirstModuleToken :: Assertion
test_stopsHeaderScanAtFirstModuleToken = do
  let source =
        T.unlines
          [ "module M where",
            "{-# LANGUAGE CPP #-}",
            "x = 1"
          ]
      exts = readModuleHeaderExtensions source
  assertEqual "stops before body pragmas" [] exts

prop_exprPrettyRoundTrip :: GenExpr -> Property
prop_exprPrettyRoundTrip generated =
  let expr = toExpr generated
      source = prettyExpr expr
   in counterexample (T.unpack source) $
        case parseExpr defaultConfig source of
          ParseOk reparsed ->
            case (expr, reparsed) of
              (EVar _ expected, EVar _ actual) ->
                counterexample ("reparsed variable mismatch: " <> show reparsed) (property (expected == actual))
              _ -> property True
          ParseErr _ -> property True

prop_modulePrettyRoundTrip :: GenModule -> Property
prop_modulePrettyRoundTrip generated =
  let modu = toModule generated
      source = prettyModule modu
      shouldParse = moduleOnlyUsesSupportedExprs generated
   in counterexample (T.unpack source) $
        case parseModule defaultConfig source of
          ParseOk reparsed ->
            counterexample ("unexpected successful parse shape: " <> show reparsed) (property shouldParse)
          ParseErr _ -> property True

prop_typePrettyRoundTrip :: Type -> Property
prop_typePrettyRoundTrip ty =
  let source = prettyTypeText ty
      expected = normalizeType ty
   in checkCoverage $
        applyCoverage (typeCtorCoverage ty) $
          counterexample (T.unpack source) $
            case parseType defaultConfig source of
              ParseErr err ->
                counterexample (errorBundlePretty err) False
              ParseOk parsed ->
                let actual = normalizeType parsed
                 in counterexample ("expected: " <> show expected <> "\nactual: " <> show actual) (expected == actual)

typeCtorCoverage :: Type -> [Property -> Property]
typeCtorCoverage ty =
  let allCtors = map showConstr (dataTypeConstrs (dataTypeOf (undefined :: Type)))
      seenCtors = typeCtorNames ty
   in [cover 1 (ctor `Set.member` seenCtors) ctor | ctor <- allCtors]

applyCoverage :: [Property -> Property] -> Property -> Property
applyCoverage wrappers prop = foldr (\wrap acc -> wrap acc) prop wrappers

typeCtorNames :: Type -> Set.Set String
typeCtorNames ty =
  let here = Set.singleton (showConstr (toConstr ty))
   in case ty of
        TVar {} -> here
        TCon {} -> here
        TQuasiQuote {} -> here
        TForall _ _ inner -> here <> typeCtorNames inner
        TApp _ f x -> here <> typeCtorNames f <> typeCtorNames x
        TFun _ a b -> here <> typeCtorNames a <> typeCtorNames b
        TTuple _ elems -> here <> mconcat (map typeCtorNames elems)
        TList _ inner -> here <> typeCtorNames inner
        TParen _ inner -> here <> typeCtorNames inner
        TContext _ constraints inner ->
          here <> mconcat (map constraintTypeCtorNames constraints) <> typeCtorNames inner

constraintTypeCtorNames :: Constraint -> Set.Set String
constraintTypeCtorNames constraint = mconcat (map typeCtorNames (constraintArgs constraint))

instance Arbitrary Type where
  arbitrary = sized (genType . min 6)
  shrink = shrinkType

shrinkType :: Type -> [Type]
shrinkType ty =
  case ty of
    TVar _ name ->
      [TVar span0 shrunk | shrunk <- shrinkIdent name]
    TCon _ name ->
      [TCon span0 shrunk | shrunk <- shrinkTypeConName name]
    TQuasiQuote _ quoter body ->
      [TQuasiQuote span0 q body | q <- shrinkIdent quoter]
        <> [TQuasiQuote span0 quoter b | b <- map T.pack (shrink (T.unpack body))]
    TForall _ binders inner ->
      [canonicalForallInner inner]
        <> [TForall span0 binders' (canonicalForallInner inner) | binders' <- shrinkTypeBinders binders]
        <> [TForall span0 binders (canonicalForallInner inner') | inner' <- shrinkType inner]
    TApp _ fn arg ->
      [canonicalAppHead fn, canonicalAppArg arg]
        <> [TApp span0 (canonicalAppHead fn') (canonicalAppArg arg) | fn' <- shrinkType fn]
        <> [TApp span0 (canonicalAppHead fn) (canonicalAppArg arg') | arg' <- shrinkType arg]
    TFun _ lhs rhs ->
      [canonicalFunLeft lhs, rhs]
        <> [TFun span0 (canonicalFunLeft lhs') rhs | lhs' <- shrinkType lhs]
        <> [TFun span0 (canonicalFunLeft lhs) rhs' | rhs' <- shrinkType rhs]
    TTuple _ elems ->
      shrinkTupleElems elems
    TList _ inner ->
      [inner] <> [TList span0 inner' | inner' <- shrinkType inner]
    TParen _ inner ->
      [inner] <> [TParen span0 inner' | inner' <- shrinkType inner]
    TContext _ constraints inner ->
      [inner]
        <> [TContext span0 constraints' inner | constraints' <- shrinkConstraints constraints]
        <> [TContext span0 constraints inner' | inner' <- shrinkType inner]

canonicalForallInner :: Type -> Type
canonicalForallInner ty =
  case ty of
    TForall {} -> TParen span0 ty
    _ -> ty

shrinkTypeBinders :: [Text] -> [[Text]]
shrinkTypeBinders binders =
  [ shrunk
  | shrunk <- shrinkList shrinkIdent binders,
    not (null shrunk)
  ]

shrinkTypeConName :: Text -> [Text]
shrinkTypeConName name =
  [ candidate
  | candidate <- map T.pack (shrink (T.unpack name)),
    isValidTypeConName candidate
  ]

isValidTypeConName :: Text -> Bool
isValidTypeConName ident =
  case T.uncons ident of
    Just (first, rest) ->
      (first `elem` ['A' .. 'Z'])
        && T.all (`elem` (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'")) rest
    Nothing -> False

shrinkTupleElems :: [Type] -> [Type]
shrinkTupleElems elems =
  [ candidate
  | shrunk <- shrinkList shrinkType elems,
    candidate <- case shrunk of
      [] -> [TTuple span0 []]
      [_] -> []
      _ -> [TTuple span0 shrunk]
  ]

shrinkConstraints :: [Constraint] -> [[Constraint]]
shrinkConstraints = shrinkList shrinkConstraint

shrinkConstraint :: Constraint -> [Constraint]
shrinkConstraint constraint =
  [ constraint {constraintArgs = shrunk}
  | shrunk <- shrinkList shrinkType (constraintArgs constraint)
  ]

genType :: Int -> Gen Type
genType depth
  | depth <= 0 =
      oneof
        [ TVar span0 <$> genTypeVarName,
          TCon span0 <$> genTypeConName,
          TQuasiQuote span0 <$> genQuoterName <*> genQuasiBody,
          TTuple span0 <$> elements [[], [TVar span0 "a", TCon span0 "B"]],
          TList span0 <$> genTypeAtom 0,
          TParen span0 <$> genTypeAtom 0
        ]
  | otherwise =
      frequency
        [ (3, TVar span0 <$> genTypeVarName),
          (3, TCon span0 <$> genTypeConName),
          (2, TQuasiQuote span0 <$> genQuoterName <*> genQuasiBody),
          (2, TForall span0 <$> genTypeBinders <*> genForallInner (depth - 1)),
          (4, genTypeApp depth),
          (4, genTypeFun depth),
          (3, TTuple span0 <$> genTypeTupleElems (depth - 1)),
          (3, TList span0 <$> genType (depth - 1)),
          (3, TParen span0 <$> genType (depth - 1)),
          (3, TContext span0 <$> genConstraints (depth - 1) <*> genContextInner (depth - 1))
        ]

genTypeApp :: Int -> Gen Type
genTypeApp depth = do
  fn <- genType (depth - 1)
  arg <- genType (depth - 1)
  pure (TApp span0 (canonicalAppHead fn) (canonicalAppArg arg))

genTypeFun :: Int -> Gen Type
genTypeFun depth = do
  lhs <- genType (depth - 1)
  rhs <- genType (depth - 1)
  pure (TFun span0 (canonicalFunLeft lhs) rhs)

genForallInner :: Int -> Gen Type
genForallInner depth = do
  inner <- genType depth
  pure $
    case inner of
      TForall {} -> TParen span0 inner
      _ -> inner

genContextInner :: Int -> Gen Type
genContextInner depth = do
  inner <- genType depth
  pure $
    case inner of
      TContext {} -> TParen span0 inner
      _ -> inner

genTypeTupleElems :: Int -> Gen [Type]
genTypeTupleElems depth = do
  isUnit <- arbitrary
  if isUnit
    then pure []
    else do
      n <- chooseInt (2, 4)
      vectorOf n (genType depth)

genTypeAtom :: Int -> Gen Type
genTypeAtom depth =
  oneof
    [ TVar span0 <$> genTypeVarName,
      TCon span0 <$> genTypeConName,
      TQuasiQuote span0 <$> genQuoterName <*> genQuasiBody,
      TTuple span0 <$> genTypeTupleElems depth,
      TList span0 <$> genType depth,
      TParen span0 <$> genType depth
    ]

genConstraints :: Int -> Gen [Constraint]
genConstraints depth = do
  n <- chooseInt (1, 3)
  vectorOf n (genConstraint depth)

genConstraint :: Int -> Gen Constraint
genConstraint depth = do
  cls <- genTypeConName
  argCount <- chooseInt (0, 2)
  args <- vectorOf argCount (genConstraintArg depth)
  pure $
    Constraint
      { constraintSpan = span0,
        constraintClass = cls,
        constraintArgs = args,
        constraintParen = False
      }

genConstraintArg :: Int -> Gen Type
genConstraintArg depth = do
  arg <- genType depth
  pure (canonicalConstraintArg arg)

canonicalFunLeft :: Type -> Type
canonicalFunLeft ty =
  case ty of
    TForall {} -> TParen span0 ty
    TFun {} -> TParen span0 ty
    TContext {} -> TParen span0 ty
    _ -> ty

canonicalAppHead :: Type -> Type
canonicalAppHead ty =
  case ty of
    TForall {} -> TParen span0 ty
    TFun {} -> TParen span0 ty
    TContext {} -> TParen span0 ty
    _ -> ty

canonicalAppArg :: Type -> Type
canonicalAppArg ty =
  case ty of
    TApp {} -> TParen span0 ty
    TForall {} -> TParen span0 ty
    TFun {} -> TParen span0 ty
    TContext {} -> TParen span0 ty
    _ -> ty

canonicalConstraintArg :: Type -> Type
canonicalConstraintArg ty =
  case ty of
    TVar {} -> ty
    TCon {} -> ty
    TQuasiQuote {} -> ty
    TList {} -> ty
    TTuple {} -> ty
    TParen {} -> ty
    _ -> TParen span0 ty

genTypeBinders :: Gen [Text]
genTypeBinders = do
  n <- chooseInt (1, 3)
  vectorOf n genTypeVarName

genTypeVarName :: Gen Text
genTypeVarName = do
  first <- elements (['a' .. 'z'] <> ['_'])
  restLen <- chooseInt (0, 5)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'"))
  let candidate = T.pack (first : rest)
  if candidate `elem` reservedWords
    then genTypeVarName
    else pure candidate

genTypeConName :: Gen Text
genTypeConName = do
  first <- elements ['A' .. 'Z']
  restLen <- chooseInt (0, 5)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'"))
  pure (T.pack (first : rest))

genQuoterName :: Gen Text
genQuoterName = do
  first <- elements (['a' .. 'z'] <> ['_'])
  restLen <- chooseInt (0, 4)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'"))
  pure (T.pack (first : rest))

genQuasiBody :: Gen Text
genQuasiBody = do
  len <- chooseInt (0, 12)
  chars <- vectorOf len (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> " +-*/_()"))
  pure (T.pack chars)

normalizeType :: Type -> Type
normalizeType ty =
  case ty of
    TVar _ name -> TVar span0 name
    TCon _ name -> TCon span0 name
    TQuasiQuote _ quoter body -> TQuasiQuote span0 quoter body
    TForall _ binders inner -> TForall span0 binders (normalizeType inner)
    TApp _ f x -> TApp span0 (normalizeType f) (normalizeType x)
    TFun _ a b -> TFun span0 (normalizeType a) (normalizeType b)
    TTuple _ elems -> TTuple span0 (map normalizeType elems)
    TList _ inner -> TList span0 (normalizeType inner)
    TParen _ inner -> TParen span0 (normalizeType inner)
    TContext _ constraints inner -> TContext span0 (map normalizeConstraint constraints) (normalizeType inner)

normalizeConstraint :: Constraint -> Constraint
normalizeConstraint constraint =
  Constraint
    { constraintSpan = span0,
      constraintClass = constraintClass constraint,
      constraintArgs = map normalizeType (constraintArgs constraint),
      constraintParen = False
    }

moduleOnlyUsesSupportedExprs :: GenModule -> Bool
moduleOnlyUsesSupportedExprs (GenModule decls) = all (isModuleSupportedExpr . snd) decls

isModuleSupportedExpr :: GenExpr -> Bool
isModuleSupportedExpr generated =
  case generated of
    GVar _ -> True
    GInt _ -> True
    GApp fn arg -> isModuleSupportedExpr fn && isModuleSupportedExpr arg

newtype GenModule = GenModule {unGenModule :: [(Text, GenExpr)]}
  deriving (Show)

instance Arbitrary GenModule where
  arbitrary = do
    n <- chooseInt (1, 6)
    names <- vectorOf n genIdent
    exprs <- vectorOf n (genExpr 4)
    pure (GenModule (zip names exprs))

newtype Ident = Ident {unIdent :: Text}
  deriving (Show)

genIdent :: Gen Text
genIdent = do
  first <- elements (['a' .. 'z'] <> ['_'])
  restLen <- chooseInt (0, 8)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'"))
  let candidate = T.pack (first : rest)
  if candidate `elem` reservedWords
    then genIdent
    else pure candidate

reservedWords :: [Text]
reservedWords =
  [ "_",
    "case",
    "class",
    "data",
    "default",
    "deriving",
    "do",
    "else",
    "export",
    "foreign",
    "forall",
    "if",
    "import",
    "in",
    "infix",
    "infixl",
    "infixr",
    "instance",
    "let",
    "module",
    "newtype",
    "of",
    "then",
    "type",
    "where"
  ]

data GenExpr
  = GVar Text
  | GInt Integer
  | GApp GenExpr GenExpr
  deriving (Eq, Show)

instance Arbitrary GenExpr where
  arbitrary = sized (genExpr . min 5)
  shrink expr =
    case expr of
      GVar name -> [GVar shrunk | shrunk <- shrinkIdent name]
      GInt value -> [GInt shrunk | shrunk <- shrinkIntegral value]
      GApp fn arg -> [fn, arg] <> [GApp fn' arg | fn' <- shrink fn] <> [GApp fn arg' | arg' <- shrink arg]

genExpr :: Int -> Gen GenExpr
genExpr depth
  | depth <= 0 = oneof [GVar <$> genIdent, GInt <$> chooseInteger (0, 999)]
  | otherwise =
      frequency
        [ (3, GVar <$> genIdent),
          (3, GInt <$> chooseInteger (0, 999)),
          (4, GApp <$> genExpr (depth - 1) <*> genExpr (depth - 1))
        ]

shrinkIdent :: Text -> [Text]
shrinkIdent name =
  [ candidate
  | candidate <- map T.pack (shrink (T.unpack name)),
    not (T.null candidate),
    isValidGeneratedIdent candidate
  ]

isValidGeneratedIdent :: Text -> Bool
isValidGeneratedIdent ident =
  case T.uncons ident of
    Just (first, rest) ->
      (first `elem` (['a' .. 'z'] <> ['_']))
        && T.all (`elem` (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'")) rest
        && ident `notElem` reservedWords
    Nothing -> False

toExpr :: GenExpr -> Expr
toExpr generated =
  case generated of
    GVar name -> EVar span0 name
    GInt value -> EInt span0 value (T.pack (show value))
    GApp fn arg -> EApp span0 (toExpr fn) (toExpr arg)

toModule :: GenModule -> Module
toModule (GenModule decls) =
  Module
    { moduleSpan = span0,
      moduleName = Just "Generated",
      moduleLanguagePragmas = [],
      moduleWarningText = Nothing,
      moduleExports = Nothing,
      moduleImports = [],
      moduleDecls =
        [ DeclValue
            span0
            ( FunctionBind
                span0
                name
                [ Match
                    { matchSpan = span0,
                      matchPats = [],
                      matchRhs = UnguardedRhs span0 (toExpr expr)
                    }
                ]
            )
        | (name, expr) <- decls
        ]
    }
