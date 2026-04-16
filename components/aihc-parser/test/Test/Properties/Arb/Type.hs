{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.Arb.Type
  ( genType,
    genPromotedFunctionType,
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
          (2, TForall <$> genTypeBinders <*> genForallInner (depth - 1)),
          (4, genTypeApp depth),
          (2, genPromotedFunctionType depth),
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
genTypeApp depth = do
  fn <- genType (depth - 1)
  arg <- genType (depth - 1)
  pure (TApp (canonicalAppHead fn) (canonicalAppArg arg))

genTypeFun :: Int -> Gen Type
genTypeFun depth = do
  lhs <- genType (depth - 1)
  rhs <- genType (depth - 1)
  pure (TFun (canonicalFunLeft lhs) rhs)

genPromotedFunctionType :: Int -> Gen Type
genPromotedFunctionType depth = do
  lhs <- genPromotedFunctionArg
  rhs <- genPromotedFunctionResult (depth - 1)
  pure (TFun lhs rhs)

genPromotedFunctionArg :: Gen Type
genPromotedFunctionArg =
  oneof
    [ (`TCon` Promoted) <$> genPromotableTypeConName,
      TTuple Boxed Promoted <$> genPromotedTupleElems,
      TList Promoted <$> genPromotedListElems
    ]

genPromotedFunctionResult :: Int -> Gen Type
genPromotedFunctionResult depth =
  frequency
    [ (3, pure promotedFunctionResultType),
      (2, genSimpleTypeAtom (max 0 depth)),
      (1, genType (max 0 depth))
    ]

promotedFunctionResultType :: Type
promotedFunctionResultType =
  TCon (qualifyName Nothing (mkUnqualifiedName NameConId "Type")) Unpromoted

genForallInner :: Int -> Gen Type
genForallInner depth = do
  inner <- genType depth
  pure $
    case inner of
      TForall {} -> TParen inner
      _ -> inner

-- | Generate the body of a TH type splice: either a bare variable or a parenthesized expression.
genTypeSpliceBody :: Gen Expr
genTypeSpliceBody =
  oneof
    [ EVar <$> genTypeVarExprName,
      EParen . EVar <$> genTypeVarExprName
    ]

-- | Generate a type with a context (constraints => type).
-- Always wrapped in parens because constraints => type is a top-level
-- form that needs parens in sub-type positions.
genTypeContext :: Int -> Gen Type
genTypeContext depth = do
  n <- chooseInt (1, 3)
  constraints <- vectorOf n (genConstraintType (depth - 1))
  inner <- genType (depth - 1)
  pure $ TParen (canonicalContextType (map canonicalContextItem constraints) inner)

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
  inner <- canonicalImplicitParamType <$> genType (depth - 1)
  pure $ TParen (TImplicitParam name inner)

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
genKindSigSubject depth = do
  subject <- genSimpleTypeAtom depth
  pure (canonicalKindSigSubject subject)

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
    TContext constraints inner -> canonicalContextType constraints inner
    _ -> canonicalTypeSplice ty

canonicalContextType :: [Type] -> Type -> Type
canonicalContextType constraints = TContext (canonicalContextItems constraints)

canonicalContextItems :: [Type] -> [Type]
canonicalContextItems constraints =
  case map canonicalContextItem constraints of
    [TTuple Boxed Unpromoted []] -> []
    [TParen (TTuple Boxed Unpromoted [])] -> []
    items -> items

canonicalContextItem :: Type -> Type
canonicalContextItem ty =
  case ty of
    TParen inner@(TParen (TKindSig {})) -> TParen (canonicalContextItem inner)
    TParen inner -> TParen (canonicalContextItem inner)
    TKindSig inner kind -> TParen (TKindSig (canonicalKindSigSubject inner) (canonicalKindSigKind kind))
    TTuple Boxed Unpromoted [] -> TParen (TTuple Boxed Unpromoted [])
    TContext constraints inner -> canonicalContextType constraints inner
    _ -> canonicalTypeSplice ty

canonicalTypeSplice :: Type -> Type
canonicalTypeSplice ty =
  case ty of
    TSplice (EVar name) -> TSplice (EParen (EVar name))
    _ -> ty

canonicalForallInner :: Type -> Type
canonicalForallInner ty =
  case ty of
    TForall {} -> TParen ty
    _ -> ty

canonicalFunLeft :: Type -> Type
canonicalFunLeft ty =
  case ty of
    TForall {} -> TParen ty
    TFun {} -> TParen ty
    TContext {} -> TParen ty
    TImplicitParam {} -> TParen ty
    _ -> ty

canonicalAppHead :: Type -> Type
canonicalAppHead ty =
  case ty of
    TForall {} -> TParen ty
    TFun {} -> TParen ty
    TContext {} -> TParen ty
    TImplicitParam {} -> TParen ty
    _ -> ty

canonicalAppArg :: Type -> Type
canonicalAppArg ty =
  case ty of
    TApp {} -> TParen ty
    TForall {} -> TParen ty
    TFun {} -> TParen ty
    TContext {} -> TParen ty
    TImplicitParam {} -> TParen ty
    _ -> ty

canonicalKindSigSubject :: Type -> Type
canonicalKindSigSubject ty =
  case ty of
    TTuple {} -> ty
    TUnboxedSum {} -> ty
    TList {} -> ty
    TParen {} -> ty
    _ -> TParen ty

canonicalKindSigKind :: Type -> Type
canonicalKindSigKind = id

canonicalImplicitParamType :: Type -> Type
canonicalImplicitParamType ty =
  case ty of
    TForall {} -> TParen ty
    TContext {} -> TParen ty
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
    TVar name ->
      [TVar (mkUnqualifiedName NameVarId shrunk) | shrunk <- shrinkIdent (renderUnqualifiedName name)]
    TCon name promoted ->
      [ TCon (name {nameText = shrunk}) promoted
      | shrunk <- shrinkConIdent (nameText name),
        -- For promoted constructors, avoid names that cause ambiguity
        -- with character literals (e.g., 'A'x lexed as char 'A' + var x)
        promoted == Unpromoted || (T.length shrunk >= 2 && not (T.any (== '\'') shrunk))
      ]
    TImplicitParam name inner ->
      [inner]
        <> [TImplicitParam name' (canonicalImplicitParamType inner) | name' <- shrinkImplicitParamName name]
        <> [TImplicitParam name (canonicalImplicitParamType inner') | inner' <- shrinkType inner]
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
      shrinkTypeTupleElems tupleFlavor elems
    TList _ elems ->
      [TList Unpromoted elems' | elems' <- shrinkList shrinkType elems, not (null elems')]
    TParen inner ->
      -- Don't unwrap parens around types that require them in sub-type positions
      [inner | not (needsParensInSubPosition inner)]
        <> [TParen inner' | inner' <- shrinkType inner]
    TKindSig ty' kind ->
      [canonicalKindSigSubject ty', canonicalKindSigKind kind]
        <> [TKindSig (canonicalKindSigSubject ty'') (canonicalKindSigKind kind) | ty'' <- shrinkType ty']
        <> [TKindSig (canonicalKindSigSubject ty') (canonicalKindSigKind kind') | kind' <- shrinkType kind]
    TUnboxedSum elems ->
      [TUnboxedSum elems' | elems' <- shrinkList shrinkType elems, length elems' >= 2]
    TContext constraints inner ->
      [inner]
        <> [canonicalContextType constraints' inner | constraints' <- shrinkContextItems constraints]
        <> [canonicalContextType constraints inner' | inner' <- shrinkType inner]
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

shrinkTypeTupleElems :: TupleFlavor -> [Type] -> [Type]
shrinkTypeTupleElems tupleFlavor elems =
  [ candidate
  | shrunk <- shrinkList shrinkType elems,
    candidate <- case shrunk of
      [] -> [TTuple tupleFlavor Unpromoted []]
      [_] -> []
      _ -> [TTuple tupleFlavor Unpromoted shrunk]
  ]

shrinkContextItems :: [Type] -> [[Type]]
shrinkContextItems = shrinkList shrinkContextItem

shrinkContextItem :: Type -> [Type]
shrinkContextItem ty =
  case ty of
    TImplicitParam name inner ->
      [inner]
        <> [TImplicitParam name' (canonicalImplicitParamType inner) | name' <- shrinkImplicitParamName name]
        <> [TImplicitParam name (canonicalImplicitParamType inner') | inner' <- shrinkType inner]
    TParen inner ->
      inner : [TParen inner' | inner' <- shrinkContextItem inner]
    TKindSig subj kind ->
      [canonicalKindSigSubject subj, canonicalKindSigKind kind]
        <> [canonicalContextItem (TKindSig subj' (canonicalKindSigKind kind)) | subj' <- shrinkType subj]
        <> [TKindSig (canonicalKindSigSubject subj) (canonicalKindSigKind kind') | kind' <- shrinkType kind]
    _ -> shrinkType ty

shrinkImplicitParamName :: Text -> [Text]
shrinkImplicitParamName name =
  case T.stripPrefix "?" name of
    Nothing -> []
    Just inner -> ["?" <> candidate | candidate <- shrinkIdent inner]
