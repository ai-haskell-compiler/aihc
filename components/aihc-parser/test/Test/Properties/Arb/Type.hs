{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.Arb.Type
  ( genType,
    shrinkType,
  )
where

import Aihc.Parser.Lex (isReservedIdentifier)
import Aihc.Parser.Syntax
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Test.Properties.Arb.Identifiers
  ( genCharValue,
    genConIdent,
    genIdent,
    genOptionalQualifier,
    genQuasiBody,
    genQuoterName,
    shrinkConIdent,
    shrinkIdent,
  )
import Test.QuickCheck

-- | All extensions enabled for maximum keyword coverage in testing.
allExtensions :: Set.Set Extension
allExtensions = Set.fromList allKnownExtensions

instance Arbitrary Type where
  arbitrary = sized (genType . min 6)
  shrink = shrinkType

genType :: Int -> Gen Type
genType depth
  | depth <= 0 =
      oneof
        [ TVar <$> genTypeVarName,
          (`TCon` Unpromoted) <$> genTypeConAstName,
          (`TCon` Promoted) <$> genPromotableTypeConName,
          TTypeLit <$> genTypeLiteral,
          pure TStar,
          pure TWildcard,
          TQuasiQuote <$> genQuoterName <*> genQuasiBody,
          TTuple Boxed Unpromoted <$> elements [[], [TVar "a", TCon (qualifyName Nothing (mkUnqualifiedName NameConId "B")) Unpromoted]],
          TTuple Unboxed Unpromoted <$> elements [[], [TVar "a", TCon (qualifyName Nothing (mkUnqualifiedName NameConId "B")) Unpromoted]],
          TList Unpromoted <$> genTypeListElems 0,
          TParen <$> genTypeAtom 0,
          TUnboxedSum <$> genUnboxedSumElems 0
        ]
  | otherwise =
      frequency
        [ (3, TVar <$> genTypeVarName),
          (3, (`TCon` Unpromoted) <$> genTypeConAstName),
          (1, (`TCon` Promoted) <$> genPromotableTypeConName),
          (1, TTypeLit <$> genTypeLiteral),
          (1, pure TStar),
          (1, pure TWildcard),
          (2, TQuasiQuote <$> genQuoterName <*> genQuasiBody),
          (2, TForall <$> genForallTelescope <*> genForallInner (depth - 1)),
          (4, genTypeApp depth),
          (4, genTypeFun depth),
          (3, TTuple Boxed Unpromoted <$> genTypeTupleElems (depth - 1)),
          (1, TTuple Boxed Promoted <$> genPromotedTupleElems),
          (2, TTuple Unboxed Unpromoted <$> genTypeTupleElems (depth - 1)),
          (2, TUnboxedSum <$> genUnboxedSumElems (depth - 1)),
          (3, TList Unpromoted <$> genTypeListElems (depth - 1)),
          (1, TList Promoted <$> genPromotedListElems),
          (3, TParen <$> genType (depth - 1)),
          (2, TSplice <$> genTypeSpliceBody),
          (2, genTypeContext depth),
          (2, genTypeImplicitParam depth),
          (2, TKindSig <$> genKindSigSubject (depth - 1) <*> genKindSigKind (depth - 1))
        ]

genTypeApp :: Int -> Gen Type
genTypeApp depth = TApp <$> genType (depth - 1) <*> genType (depth - 1)

genTypeFun :: Int -> Gen Type
genTypeFun depth = TFun <$> genType (depth - 1) <*> genType (depth - 1)

genForallInner :: Int -> Gen Type
genForallInner = genType

-- | Generate the body of a TH type splice: either a bare variable or a parenthesized expression.
genTypeSpliceBody :: Gen Expr
genTypeSpliceBody =
  oneof
    [ EVar <$> genTypeVarExprName,
      EParen . EVar <$> genTypeVarExprName
    ]

genTypeContext :: Int -> Gen Type
genTypeContext depth = do
  n <- chooseInt (1, 3)
  constraints <- vectorOf n (genConstraintType (depth - 1))
  inner <- genType (depth - 1)
  pure $ TContext constraints inner

-- | Generate a constraint type (used in contexts).
-- Typically a type constructor applied to some arguments.
genConstraintType :: Int -> Gen Type
genConstraintType depth = do
  className <- (`TCon` Unpromoted) <$> genTypeConAstName
  oneof
    [ -- Simple constraint: ClassName tyvar
      TApp className . TVar <$> genTypeVarName,
      -- Applied constraint: ClassName (Type)
      TApp className . TParen <$> genType (max 0 depth)
    ]

genTypeImplicitParam :: Int -> Gen Type
genTypeImplicitParam depth = do
  name <- ("?" <>) <$> genIdent
  inner <- genType (depth - 1)
  pure $ TImplicitParam name inner

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

-- | Generate elements for a promoted tuple or list. Uses simple types only
-- to avoid nesting ambiguities with kind signatures and unboxed tuples
-- inside promoted containers.
genPromotedTupleElems :: Gen [Type]
genPromotedTupleElems = do
  isUnit <- arbitrary
  if isUnit
    then pure []
    else do
      n <- chooseInt (2, 3)
      vectorOf n genPromotedElem

genPromotedListElems :: Gen [Type]
genPromotedListElems = do
  n <- chooseInt (1, 3)
  vectorOf n genPromotedElem

-- | Generate a simple type suitable for use inside promoted tuples/lists.
-- Avoids character type literals since 'c' conflicts with the promotion tick.
genPromotedElem :: Gen Type
genPromotedElem =
  oneof
    [ TVar <$> genTypeVarName,
      (`TCon` Unpromoted) <$> genTypeConAstName,
      genPromotedSafeTypeLiteral,
      pure TStar
    ]

-- | Generate type literals safe for use in promoted contexts (no char literals).
genPromotedSafeTypeLiteral :: Gen Type
genPromotedSafeTypeLiteral =
  TTypeLit
    <$> oneof
      [ do
          n <- chooseInteger (0, 1000)
          pure (TypeLitInteger n (T.pack (show n))),
        do
          txt <- genSymbolText
          pure (TypeLitSymbol txt (T.pack (show (T.unpack txt))))
      ]

genTypeAtom :: Int -> Gen Type
genTypeAtom depth =
  oneof
    [ genSimpleTypeAtom depth,
      TKindSig <$> genKindSigSubject depth <*> genKindSigKind depth
    ]

genSimpleTypeAtom :: Int -> Gen Type
genSimpleTypeAtom depth =
  oneof
    [ TVar <$> genTypeVarName,
      (`TCon` Unpromoted) <$> genTypeConAstName,
      TTypeLit <$> genTypeLiteral,
      pure TStar,
      pure TWildcard,
      TQuasiQuote <$> genQuoterName <*> genQuasiBody,
      TTuple Boxed Unpromoted <$> genTypeTupleElems depth,
      TTuple Unboxed Unpromoted <$> genTypeTupleElems depth,
      TUnboxedSum <$> genUnboxedSumElems depth,
      TList Unpromoted <$> genTypeListElems depth,
      TParen <$> genType depth
    ]

genKindSigSubject :: Int -> Gen Type
genKindSigSubject = genSimpleTypeAtom

genKindSigKind :: Int -> Gen Type
genKindSigKind depth =
  frequency
    [ (3, genSimpleTypeAtom depth),
      (1, TFun <$> genSimpleTypeAtom depth <*> genSimpleTypeAtom depth)
    ]

genTypeBinders :: Gen [TyVarBinder]
genTypeBinders = do
  n <- chooseInt (1, 3)
  vectorOf n genTyVarBinder

genForallTelescope :: Gen ForallTelescope
genForallTelescope =
  ForallTelescope <$> elements [ForallInvisible, ForallVisible] <*> genTypeBinders

genTyVarBinder :: Gen TyVarBinder
genTyVarBinder = do
  name <- genTypeVarName
  oneof
    [ -- Plain specified binder: a
      pure (TyVarBinder [] (renderUnqualifiedName name) Nothing TyVarBSpecified TyVarBVisible),
      -- Plain inferred binder: {a}
      pure (TyVarBinder [] (renderUnqualifiedName name) Nothing TyVarBInferred TyVarBVisible),
      -- Kinded inferred binder: {a :: Kind}
      do
        kind <- genSimpleTypeAtom 0
        pure (TyVarBinder [] (renderUnqualifiedName name) (Just kind) TyVarBInferred TyVarBVisible),
      -- Kinded specified binder: (a :: Kind)
      do
        kind <- genSimpleTypeAtom 0
        pure (TyVarBinder [] (renderUnqualifiedName name) (Just kind) TyVarBSpecified TyVarBVisible)
    ]

genTypeVarName :: Gen UnqualifiedName
genTypeVarName = do
  first <- elements (['a' .. 'z'] <> ['_'])
  restLen <- chooseInt (0, 5)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'"))
  let candidate = T.pack (first : rest)
  if isReservedIdentifier allExtensions candidate
    then genTypeVarName
    else pure (mkUnqualifiedName NameVarId candidate)

genTypeConAstName :: Gen Name
genTypeConAstName = qualifyName <$> genOptionalQualifier <*> (mkUnqualifiedName NameConId <$> genConIdent)

-- | Generate a type constructor name that is safe for promotion with @'@.
-- Avoids names containing @'@ since @'Name'rest@ would be lexed as
-- a character literal @'N'@ followed by @amerest@.
genPromotableTypeConName :: Gen Name
genPromotableTypeConName = do
  name <- suchThat genConIdent (\n -> T.length n >= 2 && not (T.any (== '\'') n))
  pure (qualifyName Nothing (mkUnqualifiedName NameConId name))

genTypeVarExprName :: Gen Name
genTypeVarExprName = qualifyName Nothing . mkUnqualifiedName NameVarId <$> genIdent

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
        c <- genCharValue
        pure (TypeLitChar c (T.pack (show c)))
    ]

genSymbolText :: Gen Text
genSymbolText = do
  len <- chooseInt (0, 8)
  chars <- vectorOf len genCharValue
  pure (T.pack chars)

shrinkType :: Type -> [Type]
shrinkType ty =
  case ty of
    TVar name ->
      [TVar (mkUnqualifiedName NameVarId shrunk) | shrunk <- shrinkIdent (renderUnqualifiedName name)]
    TCon name promoted ->
      [ TCon (name {nameText = shrunk}) promoted
      | shrunk <- shrinkConIdent (nameText name),
        promoted == Unpromoted || (T.length shrunk >= 2 && not (T.any (== '\'') shrunk))
      ]
    TImplicitParam name inner ->
      [inner]
        <> [TImplicitParam name' inner | name' <- shrinkImplicitParamName name]
        <> [TImplicitParam name inner' | inner' <- shrinkType inner]
    TTypeLit {} ->
      []
    TStar ->
      []
    TQuasiQuote quoter body ->
      [TQuasiQuote q body | q <- shrinkIdent quoter]
        <> [TQuasiQuote quoter b | b <- map T.pack (shrink (T.unpack body))]
    TForall telescope inner ->
      [inner]
        <> [TForall telescope' inner | telescope' <- shrinkForallTelescope telescope]
        <> [TForall telescope inner' | inner' <- shrinkType inner]
    TApp fn arg ->
      [fn, arg]
        <> [TApp fn' arg | fn' <- shrinkType fn]
        <> [TApp fn arg' | arg' <- shrinkType arg]
    TFun lhs rhs ->
      [lhs, rhs]
        <> [TFun lhs' rhs | lhs' <- shrinkType lhs]
        <> [TFun lhs rhs' | rhs' <- shrinkType rhs]
    TInfix lhs op promoted rhs ->
      [lhs, rhs]
        <> [TInfix lhs' op promoted rhs | lhs' <- shrinkType lhs]
        <> [TInfix lhs op promoted rhs' | rhs' <- shrinkType rhs]
    TTuple tupleFlavor _ elems ->
      shrinkTypeTupleElems tupleFlavor elems
    TList _ elems ->
      [TList Unpromoted elems' | elems' <- shrinkList shrinkType elems, not (null elems')]
    TParen inner ->
      [inner]
        <> [TParen inner' | inner' <- shrinkType inner]
    TKindSig ty' kind ->
      [ty', kind]
        <> [TKindSig ty'' kind | ty'' <- shrinkType ty']
        <> [TKindSig ty' kind' | kind' <- shrinkType kind]
    TUnboxedSum elems ->
      [TUnboxedSum elems' | elems' <- shrinkList shrinkType elems, length elems' >= 2]
    TContext constraints inner ->
      [inner]
        <> [TContext constraints' inner | constraints' <- shrinkList shrinkType constraints, not (null constraints')]
        <> [TContext constraints inner' | inner' <- shrinkType inner]
    TSplice {} ->
      []
    TWildcard ->
      []
    TAnn _ sub -> shrinkType sub

shrinkTypeBinders :: [TyVarBinder] -> [[TyVarBinder]]
shrinkTypeBinders binders =
  [ shrunk
  | shrunk <- shrinkList shrinkTyVarBinder binders,
    not (null shrunk)
  ]

shrinkTyVarBinder :: TyVarBinder -> [TyVarBinder]
shrinkTyVarBinder tvb =
  [tvb {tyVarBinderName = name'} | name' <- shrinkIdent (tyVarBinderName tvb)]

shrinkForallTelescope :: ForallTelescope -> [ForallTelescope]
shrinkForallTelescope telescope =
  [ telescope {forallTelescopeBinders = binders'}
  | binders' <- shrinkTypeBinders (forallTelescopeBinders telescope)
  ]
    <> [ telescope {forallTelescopeVisibility = ForallInvisible}
       | forallTelescopeVisibility telescope == ForallVisible
       ]

shrinkTypeTupleElems :: TupleFlavor -> [Type] -> [Type]
shrinkTypeTupleElems tupleFlavor elems =
  [ candidate
  | shrunk <- shrinkList shrinkType elems,
    candidate <- case shrunk of
      [] -> [TTuple tupleFlavor Unpromoted []]
      [_] -> []
      _ -> [TTuple tupleFlavor Unpromoted shrunk]
  ]

shrinkImplicitParamName :: Text -> [Text]
shrinkImplicitParamName name =
  case T.stripPrefix "?" name of
    Nothing -> []
    Just inner -> ["?" <> candidate | candidate <- shrinkIdent inner]
