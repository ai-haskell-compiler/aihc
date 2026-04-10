{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.TypeRoundTrip
  ( prop_typePrettyRoundTrip,
  )
where

import Aihc.Parser
import Aihc.Parser.Lex (isReservedIdentifier)
import Aihc.Parser.Syntax
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.Coverage (assertCtorCoverage)
import Test.Properties.ExprHelpers (normalizeExpr)
import Test.Properties.Identifiers (genIdent, shrinkIdent)
import Test.QuickCheck
import Text.Megaparsec.Error qualified as MPE

span0 :: SourceSpan
span0 = noSourceSpan

typeConfig :: ParserConfig
typeConfig =
  defaultConfig
    { parserExtensions = [BlockArguments, UnboxedTuples, UnboxedSums, TemplateHaskell]
    }

prop_typePrettyRoundTrip :: Type -> Property
prop_typePrettyRoundTrip ty =
  let source = renderStrict (layoutPretty defaultLayoutOptions (pretty ty))
      expected = normalizeType (canonicalTopLevelType ty)
   in checkCoverage $
        withMaxShrinks 100 $
          assertCtorCoverage ["TAnn", "TContext", "TImplicitParam"] ty $
            counterexample (T.unpack source) $
              case parseType typeConfig source of
                ParseErr err ->
                  counterexample (MPE.errorBundlePretty err) False
                ParseOk parsed ->
                  let actual = normalizeType parsed
                   in counterexample ("expected: " <> show expected <> "\nactual: " <> show actual) (expected == actual)

canonicalTopLevelType :: Type -> Type
canonicalTopLevelType ty =
  case ty of
    TContext _ constraints inner -> canonicalContextType constraints inner
    _ -> canonicalTypeSplice ty

canonicalContextType :: [Type] -> Type -> Type
canonicalContextType constraints = TContext span0 (canonicalContextItems constraints)

canonicalContextItems :: [Type] -> [Type]
canonicalContextItems constraints =
  case map canonicalContextItem constraints of
    [TTuple _ Boxed Unpromoted []] -> []
    [TParen _ (TTuple _ Boxed Unpromoted [])] -> []
    items -> items

canonicalContextItem :: Type -> Type
canonicalContextItem ty =
  case ty of
    TParen _ inner@(TParen _ (TKindSig {})) -> TParen span0 (canonicalContextItem inner)
    TParen _ inner -> TParen span0 (canonicalContextItem inner)
    TKindSig _ inner kind -> TParen span0 (TKindSig span0 (canonicalKindSigSubject inner) (canonicalKindSigKind kind))
    TTuple _ Boxed Unpromoted [] -> TParen span0 (TTuple span0 Boxed Unpromoted [])
    TContext _ constraints inner -> canonicalContextType constraints inner
    _ -> canonicalTypeSplice ty

canonicalTypeSplice :: Type -> Type
canonicalTypeSplice ty =
  case ty of
    TSplice _ (EVar _ name) -> TSplice span0 (EParen span0 (EVar span0 name))
    _ -> ty

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
    TImplicitParam _ name inner ->
      [inner]
        <> [TImplicitParam span0 name' (canonicalImplicitParamType inner) | name' <- shrinkImplicitParamName name]
        <> [TImplicitParam span0 name (canonicalImplicitParamType inner') | inner' <- shrinkType inner]
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
    TTuple _ tupleFlavor _ elems ->
      shrinkTupleElems tupleFlavor elems
    TList _ _ elems ->
      [TList span0 Unpromoted elems' | elems' <- shrinkList shrinkType elems, not (null elems')]
    TParen _ inner ->
      [inner] <> [TParen span0 inner' | inner' <- shrinkType inner]
    TKindSig _ ty' kind ->
      [canonicalKindSigSubject ty', canonicalKindSigKind kind]
        <> [TKindSig span0 (canonicalKindSigSubject ty'') (canonicalKindSigKind kind) | ty'' <- shrinkType ty']
        <> [TKindSig span0 (canonicalKindSigSubject ty') (canonicalKindSigKind kind') | kind' <- shrinkType kind]
    TUnboxedSum _ elems ->
      [TUnboxedSum span0 elems' | elems' <- shrinkList shrinkType elems, length elems' >= 2]
    TContext _ constraints inner ->
      [inner]
        <> [canonicalContextType constraints' inner | constraints' <- shrinkContextItems constraints]
        <> [canonicalContextType constraints inner' | inner' <- shrinkType inner]
    TSplice {} ->
      []
    TWildcard _ ->
      []
    TAnn _ sub -> shrinkType sub

canonicalForallInner :: Type -> Type
canonicalForallInner ty =
  case ty of
    TForall {} -> TParen span0 ty
    _ -> ty

shrinkTypeBinders :: [TyVarBinder] -> [[TyVarBinder]]
shrinkTypeBinders binders =
  [ shrunk
  | shrunk <- shrinkList shrinkTyVarBinder binders,
    not (null shrunk)
  ]

shrinkTyVarBinder :: TyVarBinder -> [TyVarBinder]
shrinkTyVarBinder tvb =
  [tvb {tyVarBinderName = name'} | name' <- shrinkIdent (tyVarBinderName tvb)]

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
      [] -> [TTuple span0 tupleFlavor Unpromoted []]
      [_] -> []
      _ -> [TTuple span0 tupleFlavor Unpromoted shrunk]
  ]

shrinkContextItems :: [Type] -> [[Type]]
shrinkContextItems = shrinkList shrinkContextItem

shrinkContextItem :: Type -> [Type]
shrinkContextItem ty =
  case ty of
    TImplicitParam _ name inner ->
      [inner]
        <> [TImplicitParam span0 name' (canonicalImplicitParamType inner) | name' <- shrinkImplicitParamName name]
        <> [TImplicitParam span0 name (canonicalImplicitParamType inner') | inner' <- shrinkType inner]
    TParen _ inner ->
      inner : [TParen span0 inner' | inner' <- shrinkContextItem inner]
    TKindSig _ subj kind ->
      [canonicalKindSigSubject subj, canonicalKindSigKind kind]
        <> [canonicalContextItem (TKindSig span0 subj' (canonicalKindSigKind kind)) | subj' <- shrinkType subj]
        <> [TKindSig span0 (canonicalKindSigSubject subj) (canonicalKindSigKind kind') | kind' <- shrinkType kind]
    _ -> shrinkType ty

genType :: Int -> Gen Type
genType depth
  | depth <= 0 =
      oneof
        [ TVar span0 <$> genTypeVarName,
          (\name -> TCon span0 name Unpromoted) <$> genTypeConName,
          TTypeLit span0 <$> genTypeLiteral,
          pure (TStar span0),
          pure (TWildcard span0),
          TQuasiQuote span0 <$> genQuoterName <*> genQuasiBody,
          TTuple span0 Boxed Unpromoted <$> elements [[], [TVar span0 "a", TCon span0 "B" Unpromoted]],
          TTuple span0 Unboxed Unpromoted <$> elements [[], [TVar span0 "a", TCon span0 "B" Unpromoted]],
          TList span0 Unpromoted <$> genTypeListElems 0,
          TParen span0 <$> genTypeAtom 0,
          TUnboxedSum span0 <$> genUnboxedSumElems 0
        ]
  | otherwise =
      frequency
        [ (3, TVar span0 <$> genTypeVarName),
          (3, (\name -> TCon span0 name Unpromoted) <$> genTypeConName),
          (1, TTypeLit span0 <$> genTypeLiteral),
          (1, pure (TStar span0)),
          (1, pure (TWildcard span0)),
          (2, TQuasiQuote span0 <$> genQuoterName <*> genQuasiBody),
          (2, TForall span0 <$> genTypeBinders <*> genForallInner (depth - 1)),
          (4, genTypeApp depth),
          (4, genTypeFun depth),
          (3, TTuple span0 Boxed Unpromoted <$> genTypeTupleElems (depth - 1)),
          (2, TTuple span0 Unboxed Unpromoted <$> genTypeTupleElems (depth - 1)),
          (2, TUnboxedSum span0 <$> genUnboxedSumElems (depth - 1)),
          (3, TList span0 Unpromoted <$> genTypeListElems (depth - 1)),
          (3, TParen span0 <$> genType (depth - 1)),
          (2, TSplice span0 <$> genTypeSpliceBody)
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

-- | Generate the body of a TH type splice: either a bare variable or a parenthesized expression.
genTypeSpliceBody :: Gen Expr
genTypeSpliceBody =
  oneof
    [ EVar span0 <$> genIdent,
      EParen span0 . EVar span0 <$> genIdent
    ]

genTypeTupleElems :: Int -> Gen [Type]
genTypeTupleElems depth = do
  isUnit <- arbitrary
  if isUnit
    then pure []
    else do
      n <- chooseInt (2, 4)
      vectorOf n (genType depth)

genTypeListElems :: Int -> Gen [Type]
genTypeListElems depth = do
  n <- chooseInt (1, 4)
  vectorOf n (genType depth)

genUnboxedSumElems :: Int -> Gen [Type]
genUnboxedSumElems depth = do
  n <- chooseInt (2, 4)
  vectorOf n (genType depth)

genTypeAtom :: Int -> Gen Type
genTypeAtom depth =
  oneof
    [ genSimpleTypeAtom depth,
      TKindSig span0 <$> genKindSigSubject depth <*> genKindSigKind depth
    ]

genSimpleTypeAtom :: Int -> Gen Type
genSimpleTypeAtom depth =
  oneof
    [ TVar span0 <$> genTypeVarName,
      (\name -> TCon span0 name Unpromoted) <$> genTypeConName,
      TTypeLit span0 <$> genTypeLiteral,
      pure (TStar span0),
      pure (TWildcard span0),
      TQuasiQuote span0 <$> genQuoterName <*> genQuasiBody,
      TTuple span0 Boxed Unpromoted <$> genTypeTupleElems depth,
      TTuple span0 Unboxed Unpromoted <$> genTypeTupleElems depth,
      TUnboxedSum span0 <$> genUnboxedSumElems depth,
      TList span0 Unpromoted <$> genTypeListElems depth,
      TParen span0 <$> genType depth
    ]

shrinkImplicitParamName :: Text -> [Text]
shrinkImplicitParamName name =
  case T.stripPrefix "?" name of
    Nothing -> []
    Just inner -> ["?" <> candidate | candidate <- shrinkIdent inner]

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

canonicalKindSigSubject :: Type -> Type
canonicalKindSigSubject ty =
  case ty of
    TTuple {} -> ty
    TUnboxedSum {} -> ty
    TList {} -> ty
    TParen {} -> ty
    _ -> TParen span0 ty

canonicalKindSigKind :: Type -> Type
canonicalKindSigKind = id

genKindSigSubject :: Int -> Gen Type
genKindSigSubject depth = do
  subject <- genSimpleTypeAtom depth
  pure (canonicalKindSigSubject subject)

genKindSigKind :: Int -> Gen Type
genKindSigKind depth =
  frequency
    [ (3, genSimpleTypeAtom depth),
      (1, TFun span0 <$> genSimpleTypeAtom depth <*> genSimpleTypeAtom depth)
    ]

canonicalImplicitParamType :: Type -> Type
canonicalImplicitParamType ty =
  case ty of
    TForall {} -> TParen span0 ty
    TContext {} -> TParen span0 ty
    _ -> ty

genTypeBinders :: Gen [TyVarBinder]
genTypeBinders = do
  n <- chooseInt (1, 3)
  vectorOf n genTyVarBinder

genTyVarBinder :: Gen TyVarBinder
genTyVarBinder = do
  name <- genTypeVarName
  pure (TyVarBinder span0 name Nothing TyVarBSpecified)

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
  -- Exclude names that clash with TH quote brackets when TemplateHaskell is enabled
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
    TVar _ name -> TVar span0 name
    TCon _ name promoted -> TCon span0 name promoted
    TImplicitParam _ name inner -> TImplicitParam span0 name (normalizeType inner)
    TTypeLit _ lit -> TTypeLit span0 lit
    TStar _ -> TStar span0
    TWildcard _ -> TWildcard span0
    TQuasiQuote _ quoter body -> TQuasiQuote span0 quoter body
    TForall _ binders inner -> TForall span0 (map normalizeTyVarBinder binders) (normalizeType inner)
    TApp _ f x -> TApp span0 (normalizeType f) (normalizeType x)
    TFun _ a b -> TFun span0 (normalizeType a) (normalizeType b)
    TTuple _ tupleFlavor promoted elems -> TTuple span0 tupleFlavor promoted (map normalizeType elems)
    TList _ promoted elems -> TList span0 promoted (map normalizeType elems)
    TParen _ inner -> TParen span0 (normalizeType inner)
    TKindSig _ ty' kind -> TKindSig span0 (normalizeType ty') (normalizeType kind)
    TUnboxedSum _ elems -> TUnboxedSum span0 (map normalizeType elems)
    TContext _ constraints inner -> canonicalContextType (map normalizeType constraints) (normalizeType inner)
    TSplice _ body -> TSplice span0 (normalizeExpr body)
    TAnn ann sub -> TAnn ann (normalizeType sub)

normalizeTyVarBinder :: TyVarBinder -> TyVarBinder
normalizeTyVarBinder tvb =
  tvb
    { tyVarBinderSpan = span0,
      tyVarBinderKind = fmap normalizeType (tyVarBinderKind tvb)
    }
