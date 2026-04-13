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
import Data.Text (Text)
import Data.Text qualified as T
import Test.Properties.Arb.Identifiers
  ( genCharValue,
    genConIdent,
    genIdent,
    genQuasiBody,
    genQuoterName,
    shrinkConIdent,
    shrinkIdent,
    span0,
  )
import Test.QuickCheck

instance Arbitrary Type where
  arbitrary = sized (genType . min 6)
  shrink = shrinkType

genType :: Int -> Gen Type
genType depth
  | depth <= 0 =
      oneof
        [ TVar span0 <$> genTypeVarName,
          (\name -> TCon span0 name Unpromoted) <$> genTypeConAstName,
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
    [ EVar span0 <$> genTypeVarExprName,
      EParen span0 . EVar span0 <$> genTypeVarExprName
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
  pure (TyVarBinder span0 (renderUnqualifiedName name) Nothing TyVarBSpecified)

genTypeVarName :: Gen UnqualifiedName
genTypeVarName = do
  first <- elements (['a' .. 'z'] <> ['_'])
  restLen <- chooseInt (0, 5)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'"))
  let candidate = T.pack (first : rest)
  if isReservedIdentifier candidate
    then genTypeVarName
    else pure (mkUnqualifiedName NameVarId candidate)

genTypeConAstName :: Gen Name
genTypeConAstName = qualifyName Nothing . mkUnqualifiedName NameConId <$> genConIdent

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
    TSplice _ (EVar _ name) -> TSplice span0 (EParen span0 (EVar span0 name))
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

canonicalImplicitParamType :: Type -> Type
canonicalImplicitParamType ty =
  case ty of
    TForall {} -> TParen span0 ty
    TContext {} -> TParen span0 ty
    _ -> ty

shrinkType :: Type -> [Type]
shrinkType ty =
  case ty of
    TVar _ name ->
      [TVar span0 (mkUnqualifiedName NameVarId shrunk) | shrunk <- shrinkIdent (renderUnqualifiedName name)]
    TCon _ name promoted ->
      [TCon span0 (name {nameText = shrunk}) promoted | shrunk <- shrinkConIdent (nameText name)]
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
