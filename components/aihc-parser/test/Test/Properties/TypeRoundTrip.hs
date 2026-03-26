{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.TypeRoundTrip
  ( prop_typePrettyRoundTrip,
  )
where

import Aihc.Parser
import Aihc.Parser.Lex (isReservedIdentifier)
import Aihc.Parser.Syntax
import Data.Data (dataTypeConstrs, dataTypeOf, showConstr, toConstr)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.Identifiers (shrinkIdent)
import Test.QuickCheck

span0 :: SourceSpan
span0 = noSourceSpan

prop_typePrettyRoundTrip :: Type -> Property
prop_typePrettyRoundTrip ty =
  let source = renderStrict (layoutPretty defaultLayoutOptions (pretty ty))
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
        TTypeLit {} -> here
        TStar {} -> here
        TQuasiQuote {} -> here
        TForall _ _ inner -> here <> typeCtorNames inner
        TApp _ f x -> here <> typeCtorNames f <> typeCtorNames x
        TFun _ a b -> here <> typeCtorNames a <> typeCtorNames b
        TTuple _ _ elems -> here <> mconcat (map typeCtorNames elems)
        TList _ _ inner -> here <> typeCtorNames inner
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
    TCon _ name promoted ->
      [TCon span0 shrunk promoted | shrunk <- shrinkTypeConName name]
    TTypeLit {} ->
      []
    TStar _ ->
      []
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
    TTuple _ _ elems ->
      shrinkTupleElems elems
    TList _ _ inner ->
      [inner] <> [TList span0 Unpromoted inner' | inner' <- shrinkType inner]
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
      [] -> [TTuple span0 Unpromoted []]
      [_] -> []
      _ -> [TTuple span0 Unpromoted shrunk]
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
          (\name -> TCon span0 name Unpromoted) <$> genTypeConName,
          TTypeLit span0 <$> genTypeLiteral,
          pure (TStar span0),
          TQuasiQuote span0 <$> genQuoterName <*> genQuasiBody,
          TTuple span0 Unpromoted <$> elements [[], [TVar span0 "a", TCon span0 "B" Unpromoted]],
          TList span0 Unpromoted <$> genTypeAtom 0,
          TParen span0 <$> genTypeAtom 0
        ]
  | otherwise =
      frequency
        [ (3, TVar span0 <$> genTypeVarName),
          (3, (\name -> TCon span0 name Unpromoted) <$> genTypeConName),
          (1, TTypeLit span0 <$> genTypeLiteral),
          (1, pure (TStar span0)),
          (2, TQuasiQuote span0 <$> genQuoterName <*> genQuasiBody),
          (2, TForall span0 <$> genTypeBinders <*> genForallInner (depth - 1)),
          (4, genTypeApp depth),
          (4, genTypeFun depth),
          (3, TTuple span0 Unpromoted <$> genTypeTupleElems (depth - 1)),
          (3, TList span0 Unpromoted <$> genType (depth - 1)),
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
      (\name -> TCon span0 name Unpromoted) <$> genTypeConName,
      TTypeLit span0 <$> genTypeLiteral,
      pure (TStar span0),
      TQuasiQuote span0 <$> genQuoterName <*> genQuasiBody,
      TTuple span0 Unpromoted <$> genTypeTupleElems depth,
      TList span0 Unpromoted <$> genType depth,
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
    TTypeLit {} -> ty
    TStar {} -> ty
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
  pure (T.pack (first : rest))

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
    TVar _ name -> TVar span0 name
    TCon _ name promoted -> TCon span0 name promoted
    TTypeLit _ lit -> TTypeLit span0 lit
    TStar _ -> TStar span0
    TQuasiQuote _ quoter body -> TQuasiQuote span0 quoter body
    TForall _ binders inner -> TForall span0 binders (normalizeType inner)
    TApp _ f x -> TApp span0 (normalizeType f) (normalizeType x)
    TFun _ a b -> TFun span0 (normalizeType a) (normalizeType b)
    TTuple _ promoted elems -> TTuple span0 promoted (map normalizeType elems)
    TList _ promoted inner -> TList span0 promoted (normalizeType inner)
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
