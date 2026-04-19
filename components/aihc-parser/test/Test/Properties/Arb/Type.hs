{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.Arb.Type
  ( genType,
    shrinkType,
  )
where

import Aihc.Parser.Syntax
import Data.Text (Text)
import Data.Text qualified as T
import Test.Properties.Arb.Identifiers
  ( genCharValue,
    genConName,
    genQuasiBody,
    genQuoterName,
    genVarId,
    shrinkConIdent,
    shrinkIdent,
  )
import Test.QuickCheck

instance Arbitrary Type where
  arbitrary = genType
  shrink = shrinkType

-- | Generate a random type. Uses QuickCheck's size parameter to control
-- recursion depth.
genType :: Gen Type
genType = scale (`div` 2) $ do
  n <- getSize
  if n <= 0
    then
      oneof
        [ TVar <$> genTypeVarName,
          (`TCon` Unpromoted) <$> genConName,
          (`TCon` Promoted) <$> genConName,
          TTypeLit <$> genTypeLiteral,
          pure TStar,
          pure TWildcard,
          TQuasiQuote <$> genQuoterName <*> genQuasiBody,
          TTuple Boxed Unpromoted <$> elements [[], [TVar "a", TCon (qualifyName Nothing (mkUnqualifiedName NameConId "B")) Unpromoted]],
          TTuple Unboxed Unpromoted <$> elements [[], [TVar "a", TCon (qualifyName Nothing (mkUnqualifiedName NameConId "B")) Unpromoted]],
          TList Unpromoted <$> genTypeListElems,
          TUnboxedSum <$> genUnboxedSumElems
        ]
    else
      oneof
        [ TVar <$> genTypeVarName,
          (`TCon` Unpromoted) <$> genConName,
          (`TCon` Promoted) <$> genConName,
          TTypeLit <$> genTypeLiteral,
          pure TStar,
          pure TWildcard,
          TQuasiQuote <$> genQuoterName <*> genQuasiBody,
          TForall <$> genForallTelescope <*> genType,
          genTypeApp,
          genTypeFun,
          TTuple Boxed Unpromoted <$> genTypeTupleElems,
          TTuple Boxed Promoted <$> genPromotedTupleElems,
          TTuple Unboxed Unpromoted <$> genTypeTupleElems,
          TUnboxedSum <$> genUnboxedSumElems,
          TList Unpromoted <$> genTypeListElems,
          TList Promoted <$> genPromotedListElems,
          TParen <$> genType,
          TSplice <$> genTypeSpliceBody,
          genTypeContext,
          genTypeImplicitParam,
          TKindSig <$> genKindSigSubject <*> genKindSigKind
        ]

genTypeApp :: Gen Type
genTypeApp = TApp <$> genType <*> genType

genTypeFun :: Gen Type
genTypeFun = TFun <$> genType <*> genType

-- | Generate the body of a TH type splice: either a bare variable or a parenthesized expression.
genTypeSpliceBody :: Gen Expr
genTypeSpliceBody =
  oneof
    [ EVar <$> genTypeVarExprName,
      EParen . EVar <$> genTypeVarExprName
    ]

genTypeContext :: Gen Type
genTypeContext = do
  n <- chooseInt (1, 3)
  constraints <- vectorOf n (genConstraintType)
  inner <- genType
  pure $ TContext constraints inner

-- | Generate a constraint type (used in contexts).
-- Typically a type constructor applied to some arguments.
genConstraintType :: Gen Type
genConstraintType = do
  s <- getSize
  className <- (`TCon` Unpromoted) <$> genConName
  oneof
    [ -- Simple constraint: ClassName tyvar
      TApp className . TVar <$> genTypeVarName,
      -- Applied constraint: ClassName (Type)
      TApp className . TParen <$> resize s genType
    ]

genTypeImplicitParam :: Gen Type
genTypeImplicitParam = do
  name <- ("?" <>) <$> genVarId
  inner <- genType
  pure $ TImplicitParam name inner

genTypeTupleElems :: Gen [Type]
genTypeTupleElems = do
  isUnit <- arbitrary
  if isUnit
    then pure []
    else do
      n <- chooseInt (2, 4)
      scale (`div` n) $ vectorOf n genType

genTypeListElems :: Gen [Type]
genTypeListElems = do
  n <- chooseInt (1, 4)
  scale (`div` n) $ vectorOf n genType

genUnboxedSumElems :: Gen [Type]
genUnboxedSumElems = do
  n <- chooseInt (2, 4)
  scale (`div` n) $ vectorOf n genType

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
      (`TCon` Unpromoted) <$> genConName,
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

genSimpleTypeAtom :: Gen Type
genSimpleTypeAtom =
  oneof
    [ TVar <$> genTypeVarName,
      (`TCon` Unpromoted) <$> genConName,
      TTypeLit <$> genTypeLiteral,
      pure TStar,
      pure TWildcard,
      TQuasiQuote <$> genQuoterName <*> genQuasiBody,
      TTuple Boxed Unpromoted <$> genTypeTupleElems,
      TTuple Unboxed Unpromoted <$> genTypeTupleElems,
      TUnboxedSum <$> genUnboxedSumElems,
      TList Unpromoted <$> genTypeListElems,
      TParen <$> genType
    ]

genKindSigSubject :: Gen Type
genKindSigSubject = genSimpleTypeAtom

genKindSigKind :: Gen Type
genKindSigKind =
  frequency
    [ (3, genSimpleTypeAtom),
      (1, TFun <$> genSimpleTypeAtom <*> genSimpleTypeAtom)
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
        kind <- resize 0 genSimpleTypeAtom
        pure (TyVarBinder [] (renderUnqualifiedName name) (Just kind) TyVarBInferred TyVarBVisible),
      -- Kinded specified binder: (a :: Kind)
      do
        kind <- resize 0 genSimpleTypeAtom
        pure (TyVarBinder [] (renderUnqualifiedName name) (Just kind) TyVarBSpecified TyVarBVisible)
    ]

genTypeVarName :: Gen UnqualifiedName
genTypeVarName = mkUnqualifiedName NameVarId <$> genVarId

genTypeVarExprName :: Gen Name
genTypeVarExprName = qualifyName Nothing . mkUnqualifiedName NameVarId <$> genVarId

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
      [TVar $ unqualifiedNameFromText "a"]
        <> [TVar (mkUnqualifiedName NameVarId shrunk) | shrunk <- shrinkIdent (renderUnqualifiedName name)]
    TCon name promoted ->
      [TCon (nameFromText "A") promoted | T.length (renderName name) > 1]
        <> [ TCon (name {nameText = shrunk}) promoted
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
        <> elems
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
