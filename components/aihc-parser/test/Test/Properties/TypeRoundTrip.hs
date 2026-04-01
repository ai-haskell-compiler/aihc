{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.TypeRoundTrip
  ( prop_typePrettyRoundTrip,
  )
where

import Aihc.Parser
import Aihc.Parser.Lex (isReservedIdentifier)
import Aihc.Parser.Syntax (Extension (TemplateHaskell, UnboxedSums, UnboxedTuples))
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.BareSyntax
import Test.Properties.ExprHelpers (normalizeExpr)
import Test.Properties.Identifiers (genIdent, shrinkIdent)
import Test.QuickCheck

typeConfig :: ParserConfig
typeConfig =
  defaultConfig
    { parserExtensions = [UnboxedTuples, UnboxedSums, TemplateHaskell]
    }

prop_typePrettyRoundTrip :: Type -> Property
prop_typePrettyRoundTrip ty =
  let source = renderStrict (layoutPretty defaultLayoutOptions (pretty (toSyntaxType ty)))
      expected = normalizeType ty
   in counterexample (T.unpack source) $
        case parseType typeConfig source of
          ParseErr err ->
            counterexample (errorBundlePretty (Just source) err) False
          ParseOk parsed ->
            let actual = normalizeType (eraseType parsed)
             in counterexample ("expected: " <> show expected <> "\nactual: " <> show actual) (expected == actual)

instance Arbitrary Type where
  arbitrary = sized (genType . min 6)
  shrink = shrinkType

shrinkType :: Type -> [Type]
shrinkType ty =
  case ty of
    TVar name ->
      [TVar shrunk | shrunk <- shrinkIdent name]
    TCon name promoted ->
      [TCon shrunk promoted | shrunk <- shrinkTypeConName name]
    TTypeLit {} ->
      []
    TStar ->
      []
    TQuasiQuote quoter body ->
      [TQuasiQuote q body | q <- shrinkIdent quoter]
        <> [TQuasiQuote quoter b | b <- map T.pack (shrink (T.unpack body))]
    TForall binders inner ->
      [canonicalForallInner inner]
        <> [TForall binders' (canonicalForallInner inner) | binders' <- shrinkTypeBinders binders]
        <> [TForall binders (canonicalForallInner inner') | inner' <- shrinkType inner]
    TApp fn arg ->
      [canonicalAppHead fn, canonicalAppArg arg]
        <> [TApp (canonicalAppHead fn') (canonicalAppArg arg) | fn' <- shrinkType fn]
        <> [TApp (canonicalAppHead fn) (canonicalAppArg arg') | arg' <- shrinkType arg]
    TFun lhs rhs ->
      [canonicalFunLeft lhs, rhs]
        <> [TFun (canonicalFunLeft lhs') rhs | lhs' <- shrinkType lhs]
        <> [TFun (canonicalFunLeft lhs) rhs' | rhs' <- shrinkType rhs]
    TTuple tupleFlavor _ elems ->
      shrinkTupleElems tupleFlavor elems
    TList _ inner ->
      [inner] <> [TList Unpromoted inner' | inner' <- shrinkType inner]
    TParen inner ->
      [inner] <> [TParen inner' | inner' <- shrinkType inner]
    TUnboxedSum elems ->
      [TUnboxedSum elems' | elems' <- shrinkList shrinkType elems, length elems' >= 2]
    TContext constraints inner ->
      [inner]
        <> [TContext constraints' inner | constraints' <- shrinkConstraints constraints]
        <> [TContext constraints inner' | inner' <- shrinkType inner]
    TSplice {} ->
      []

canonicalForallInner :: Type -> Type
canonicalForallInner ty =
  case ty of
    TForall {} -> TParen ty
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

shrinkTupleElems :: TupleFlavor -> [Type] -> [Type]
shrinkTupleElems tupleFlavor elems =
  [ candidate
  | shrunk <- shrinkList shrinkType elems,
    candidate <- case shrunk of
      [] -> [TTuple tupleFlavor Unpromoted []]
      [_] -> []
      _ -> [TTuple tupleFlavor Unpromoted shrunk]
  ]

shrinkConstraints :: [Constraint] -> [[Constraint]]
shrinkConstraints = shrinkList shrinkConstraint

shrinkConstraint :: Constraint -> [Constraint]
shrinkConstraint constraint =
  case constraint of
    Constraint cls args ->
      [ Constraint cls shrunk
      | shrunk <- shrinkList shrinkType args
      ]
    CParen inner ->
      inner : [CParen shrunk | shrunk <- shrinkConstraint inner]

genType :: Int -> Gen Type
genType depth
  | depth <= 0 =
      oneof
        [ TVar <$> genTypeVarName,
          (`TCon` Unpromoted) <$> genTypeConName,
          TTypeLit <$> genTypeLiteral,
          pure TStar,
          TQuasiQuote <$> genQuoterName <*> genQuasiBody,
          TTuple Boxed Unpromoted <$> elements [[], [TVar "a", TCon "B" Unpromoted]],
          TTuple Unboxed Unpromoted <$> elements [[], [TVar "a", TCon "B" Unpromoted]],
          TList Unpromoted <$> genTypeAtom 0,
          TParen <$> genTypeAtom 0,
          TUnboxedSum <$> genUnboxedSumElems 0
        ]
  | otherwise =
      frequency
        [ (3, TVar <$> genTypeVarName),
          (3, (`TCon` Unpromoted) <$> genTypeConName),
          (1, TTypeLit <$> genTypeLiteral),
          (1, pure TStar),
          (2, TQuasiQuote <$> genQuoterName <*> genQuasiBody),
          (2, TForall <$> genTypeBinders <*> genForallInner (depth - 1)),
          (4, genTypeApp depth),
          (4, genTypeFun depth),
          (3, TTuple Boxed Unpromoted <$> genTypeTupleElems (depth - 1)),
          (2, TTuple Unboxed Unpromoted <$> genTypeTupleElems (depth - 1)),
          (2, TUnboxedSum <$> genUnboxedSumElems (depth - 1)),
          (3, TList Unpromoted <$> genType (depth - 1)),
          (3, TParen <$> genType (depth - 1)),
          (3, TContext <$> genConstraints (depth - 1) <*> genContextInner (depth - 1)),
          (2, TSplice <$> genTypeSpliceBody)
        ]

genTypeApp :: Int -> Gen Type
genTypeApp depth = do
  fn <- genType (depth - 1)
  arg <- genType (depth - 1)
  pure (TApp (canonicalAppHead fn) (canonicalAppArg arg))

genTypeFun :: Int -> Gen Type
genTypeFun depth = do
  lhs <- genType (depth - 1)
  rhs <- genType (depth - 1)
  pure (TFun (canonicalFunLeft lhs) rhs)

genForallInner :: Int -> Gen Type
genForallInner depth = do
  inner <- genType depth
  pure $
    case inner of
      TForall {} -> TParen inner
      _ -> inner

genContextInner :: Int -> Gen Type
genContextInner depth = do
  inner <- genType depth
  pure $
    case inner of
      TContext {} -> TParen inner
      _ -> inner

genTypeSpliceBody :: Gen Expr
genTypeSpliceBody =
  oneof
    [ EVar <$> genIdent,
      EParen . EVar <$> genIdent
    ]

genTypeTupleElems :: Int -> Gen [Type]
genTypeTupleElems depth = do
  isUnit <- arbitrary
  if isUnit
    then pure []
    else do
      n <- chooseInt (2, 4)
      vectorOf n (genType depth)

genUnboxedSumElems :: Int -> Gen [Type]
genUnboxedSumElems depth = do
  n <- chooseInt (2, 4)
  vectorOf n (genType depth)

genTypeAtom :: Int -> Gen Type
genTypeAtom depth =
  oneof
    [ TVar <$> genTypeVarName,
      (`TCon` Unpromoted) <$> genTypeConName,
      TTypeLit <$> genTypeLiteral,
      pure TStar,
      TQuasiQuote <$> genQuoterName <*> genQuasiBody,
      TTuple Boxed Unpromoted <$> genTypeTupleElems depth,
      TTuple Unboxed Unpromoted <$> genTypeTupleElems depth,
      TUnboxedSum <$> genUnboxedSumElems depth,
      TList Unpromoted <$> genType depth,
      TParen <$> genType depth
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
  pure (Constraint cls args)

genConstraintArg :: Int -> Gen Type
genConstraintArg depth = do
  arg <- genType depth
  pure (canonicalConstraintArg arg)

canonicalFunLeft :: Type -> Type
canonicalFunLeft ty =
  case ty of
    TForall {} -> TParen ty
    TFun {} -> TParen ty
    TContext {} -> TParen ty
    _ -> ty

canonicalAppHead :: Type -> Type
canonicalAppHead ty =
  case ty of
    TForall {} -> TParen ty
    TFun {} -> TParen ty
    TContext {} -> TParen ty
    _ -> ty

canonicalAppArg :: Type -> Type
canonicalAppArg ty =
  case ty of
    TApp {} -> TParen ty
    TForall {} -> TParen ty
    TFun {} -> TParen ty
    TContext {} -> TParen ty
    _ -> ty

canonicalConstraintArg :: Type -> Type
canonicalConstraintArg ty =
  case ty of
    TVar {} -> ty
    TCon {} -> ty
    TTypeLit {} -> ty
    TStar {} -> ty
    TQuasiQuote {} -> ty
    TList {} -> ty
    TTuple {} -> ty
    TUnboxedSum {} -> ty
    TParen {} -> ty
    _ -> TParen ty

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
  if isReservedIdentifier candidate
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
  let candidate = T.pack (first : rest)
  if candidate `elem` ["e", "t", "d", "p"]
    then genQuoterName
    else pure candidate

genQuasiBody :: Gen Text
genQuasiBody = do
  len <- chooseInt (0, 12)
  chars <- vectorOf len (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> " +-*/_()"))
  pure (T.pack chars)

genTypeLiteral :: Gen TypeLiteral
genTypeLiteral =
  oneof
    [ do
        n <- chooseInteger (0, 1000)
        pure (TypeLitInteger n (T.pack (show n))),
      do
        txt <- genSymbolText
        pure (TypeLitSymbol txt (T.pack (show (T.unpack txt)))),
      do
        c <- genTypeLiteralChar
        pure (TypeLitChar c (T.pack (show c)))
    ]

genSymbolText :: Gen Text
genSymbolText = do
  len <- chooseInt (0, 8)
  chars <- vectorOf len genTypeLiteralChar
  pure (T.pack chars)

genTypeLiteralChar :: Gen Char
genTypeLiteralChar = elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> " _")

normalizeType :: Type -> Type
normalizeType ty =
  case ty of
    TVar name -> TVar name
    TCon name promoted -> TCon name promoted
    TTypeLit lit -> TTypeLit lit
    TStar -> TStar
    TQuasiQuote quoter body -> TQuasiQuote quoter body
    TForall binders inner -> TForall binders (normalizeType inner)
    TApp f x -> TApp (normalizeType f) (normalizeType x)
    TFun a b -> TFun (normalizeType a) (normalizeType b)
    TTuple tupleFlavor promoted elems -> TTuple tupleFlavor promoted (map normalizeType elems)
    TList promoted inner -> TList promoted (normalizeType inner)
    TParen inner -> TParen (normalizeType inner)
    TUnboxedSum elems -> TUnboxedSum (map normalizeType elems)
    TContext constraints inner -> TContext (map normalizeConstraint constraints) (normalizeType inner)
    TSplice body -> TSplice (normalizeExpr body)

normalizeConstraint :: Constraint -> Constraint
normalizeConstraint constraint =
  case constraint of
    Constraint cls args -> Constraint cls (map normalizeType args)
    CParen inner -> CParen (normalizeConstraint inner)
