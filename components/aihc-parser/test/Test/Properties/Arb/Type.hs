{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.Arb.Type
  ( genType,
    shrinkType,
    canonicalTopLevelType,
    canonicalContextType,
    canonicalFunLeft,
    canonicalAppHead,
    canonicalAppArg,
    canonicalKindSigSubject,
    canonicalKindSigKind,
    canonicalImplicitParamType,
    canonicalForallInner,
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
    span0,
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
        [ TVar span0 <$> genTypeVarName,
          (\name -> TCon span0 name Unpromoted) <$> genTypeConAstName,
          (\name -> TCon span0 name Promoted) <$> genPromotableTypeConName,
          TTypeLit span0 <$> genTypeLiteral,
          pure (TStar span0),
          pure (TWildcard span0),
          TQuasiQuote span0 <$> genQuoterName <*> genQuasiBody,
          TTuple span0 Boxed Unpromoted <$> elements [[], [TVar span0 "a", TCon span0 (qualifyName Nothing (mkUnqualifiedName NameConId "B")) Unpromoted]],
          TTuple span0 Unboxed Unpromoted <$> elements [[], [TVar span0 "a", TCon span0 (qualifyName Nothing (mkUnqualifiedName NameConId "B")) Unpromoted]],
          TList span0 Unpromoted <$> genTypeListElems 0,
          TParen span0 <$> genTypeAtom 0,
          TUnboxedSum span0 <$> genUnboxedSumElems 0
        ]
  | otherwise =
      frequency
        [ (3, TVar span0 <$> genTypeVarName),
          (3, (\name -> TCon span0 name Unpromoted) <$> genTypeConAstName),
          (1, (\name -> TCon span0 name Promoted) <$> genPromotableTypeConName),
          (1, TTypeLit span0 <$> genTypeLiteral),
          (1, pure (TStar span0)),
          (1, pure (TWildcard span0)),
          (2, TQuasiQuote span0 <$> genQuoterName <*> genQuasiBody),
          (2, TForall span0 <$> genTypeBinders <*> genForallInner (depth - 1)),
          (4, genTypeApp depth),
          (4, genTypeFun depth),
          (3, TTuple span0 Boxed Unpromoted <$> genTypeTupleElems (depth - 1)),
          (1, TTuple span0 Boxed Promoted <$> genPromotedTupleElems),
          (2, TTuple span0 Unboxed Unpromoted <$> genTypeTupleElems (depth - 1)),
          (2, TUnboxedSum span0 <$> genUnboxedSumElems (depth - 1)),
          (3, TList span0 Unpromoted <$> genTypeListElems (depth - 1)),
          (1, TList span0 Promoted <$> genPromotedListElems),
          (3, TParen span0 <$> genType (depth - 1)),
          (2, TSplice span0 <$> genTypeSpliceBody),
          (2, genTypeContext depth),
          (2, genTypeImplicitParam depth),
          (2, TKindSig span0 <$> genKindSigSubject (depth - 1) <*> genKindSigKind (depth - 1))
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
    [ exprAnnSpan span0 . EVar <$> genTypeVarExprName,
      exprAnnSpan span0 . EParen . exprAnnSpan span0 . EVar <$> genTypeVarExprName
    ]

-- | Generate a type with a context (constraints => type).
-- Always wrapped in parens because constraints => type is a top-level
-- form that needs parens in sub-type positions.
genTypeContext :: Int -> Gen Type
genTypeContext depth = do
  n <- chooseInt (1, 3)
  constraints <- vectorOf n (genConstraintType (depth - 1))
  inner <- genType (depth - 1)
  pure $ TParen span0 (canonicalContextType (map canonicalContextItem constraints) inner)

-- | Generate a constraint type (used in contexts).
-- Typically a type constructor applied to some arguments.
genConstraintType :: Int -> Gen Type
genConstraintType depth = do
  className <- (\n -> TCon span0 n Unpromoted) <$> genTypeConAstName
  oneof
    [ -- Simple constraint: ClassName tyvar
      TApp span0 className . TVar span0 <$> genTypeVarName,
      -- Applied constraint: ClassName (Type)
      TApp span0 className . TParen span0 <$> genType (max 0 depth)
    ]

genTypeImplicitParam :: Int -> Gen Type
genTypeImplicitParam depth = do
  name <- ("?" <>) <$> genIdent
  inner <- canonicalImplicitParamType <$> genType (depth - 1)
  pure $ TParen span0 (TImplicitParam span0 name inner)

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
    [ TVar span0 <$> genTypeVarName,
      (\name -> TCon span0 name Unpromoted) <$> genTypeConAstName,
      genPromotedSafeTypeLiteral,
      pure (TStar span0)
    ]

-- | Generate type literals safe for use in promoted contexts (no char literals).
genPromotedSafeTypeLiteral :: Gen Type
genPromotedSafeTypeLiteral =
  TTypeLit span0
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
      TKindSig span0 <$> genKindSigSubject depth <*> genKindSigKind depth
    ]

genSimpleTypeAtom :: Int -> Gen Type
genSimpleTypeAtom depth =
  oneof
    [ TVar span0 <$> genTypeVarName,
      (\name -> TCon span0 name Unpromoted) <$> genTypeConAstName,
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

genTypeBinders :: Gen [TyVarBinder]
genTypeBinders = do
  n <- chooseInt (1, 3)
  vectorOf n genTyVarBinder

genTyVarBinder :: Gen TyVarBinder
genTyVarBinder = do
  name <- genTypeVarName
  oneof
    [ -- Plain specified binder: a
      pure (TyVarBinder span0 (renderUnqualifiedName name) Nothing TyVarBSpecified),
      -- Plain inferred binder: {a}
      pure (TyVarBinder span0 (renderUnqualifiedName name) Nothing TyVarBInferred),
      -- Kinded inferred binder: {a :: Kind}
      do
        kind <- genSimpleTypeAtom 0
        pure (TyVarBinder span0 (renderUnqualifiedName name) (Just kind) TyVarBInferred),
      -- Kinded specified binder: (a :: Kind)
      do
        kind <- genSimpleTypeAtom 0
        pure (TyVarBinder span0 (renderUnqualifiedName name) (Just kind) TyVarBSpecified)
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
    TSplice _ (EVar name) -> TSplice span0 (exprAnnSpan span0 (EParen (exprAnnSpan span0 (EVar name))))
    _ -> ty

canonicalForallInner :: Type -> Type
canonicalForallInner ty =
  case ty of
    TForall {} -> TParen span0 ty
    _ -> ty

canonicalFunLeft :: Type -> Type
canonicalFunLeft ty =
  case ty of
    TForall {} -> TParen span0 ty
    TFun {} -> TParen span0 ty
    TContext {} -> TParen span0 ty
    TImplicitParam {} -> TParen span0 ty
    _ -> ty

canonicalAppHead :: Type -> Type
canonicalAppHead ty =
  case ty of
    TForall {} -> TParen span0 ty
    TFun {} -> TParen span0 ty
    TContext {} -> TParen span0 ty
    TImplicitParam {} -> TParen span0 ty
    _ -> ty

canonicalAppArg :: Type -> Type
canonicalAppArg ty =
  case ty of
    TApp {} -> TParen span0 ty
    TForall {} -> TParen span0 ty
    TFun {} -> TParen span0 ty
    TContext {} -> TParen span0 ty
    TImplicitParam {} -> TParen span0 ty
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

canonicalImplicitParamType :: Type -> Type
canonicalImplicitParamType ty =
  case ty of
    TForall {} -> TParen span0 ty
    TContext {} -> TParen span0 ty
    _ -> ty

-- | Types that require parentheses when appearing inside compound types
-- (tuples, lists, application arguments, etc.). These are "top-level" type
-- forms whose syntax is ambiguous without parens.
needsParensInSubPosition :: Type -> Bool
needsParensInSubPosition ty =
  case ty of
    TImplicitParam {} -> True
    TContext {} -> True
    TForall {} -> True
    TFun {} -> True
    _ -> False

shrinkType :: Type -> [Type]
shrinkType ty =
  case ty of
    TVar _ name ->
      [TVar span0 (mkUnqualifiedName NameVarId shrunk) | shrunk <- shrinkIdent (renderUnqualifiedName name)]
    TCon _ name promoted ->
      [ TCon span0 (name {nameText = shrunk}) promoted
      | shrunk <- shrinkConIdent (nameText name),
        -- For promoted constructors, avoid names that cause ambiguity
        -- with character literals (e.g., 'A'x lexed as char 'A' + var x)
        promoted == Unpromoted || (T.length shrunk >= 2 && not (T.any (== '\'') shrunk))
      ]
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
      shrinkTypeTupleElems tupleFlavor elems
    TList _ _ elems ->
      [TList span0 Unpromoted elems' | elems' <- shrinkList shrinkType elems, not (null elems')]
    TParen _ inner ->
      -- Don't unwrap parens around types that require them in sub-type positions
      [inner | not (needsParensInSubPosition inner)]
        <> [TParen span0 inner' | inner' <- shrinkType inner]
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

shrinkTypeBinders :: [TyVarBinder] -> [[TyVarBinder]]
shrinkTypeBinders binders =
  [ shrunk
  | shrunk <- shrinkList shrinkTyVarBinder binders,
    not (null shrunk)
  ]

shrinkTyVarBinder :: TyVarBinder -> [TyVarBinder]
shrinkTyVarBinder tvb =
  [tvb {tyVarBinderName = name'} | name' <- shrinkIdent (tyVarBinderName tvb)]

shrinkTypeTupleElems :: TupleFlavor -> [Type] -> [Type]
shrinkTypeTupleElems tupleFlavor elems =
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

shrinkImplicitParamName :: Text -> [Text]
shrinkImplicitParamName name =
  case T.stripPrefix "?" name of
    Nothing -> []
    Just inner -> ["?" <> candidate | candidate <- shrinkIdent inner]
