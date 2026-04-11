{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.Arb.Decl
  ( genDecl,
    genFunctionDecl,
    shrinkDecl,
  )
where

import Aihc.Parser.Syntax
import Data.Text (Text)
import Data.Text qualified as T
import Test.Properties.Arb.Expr (genExpr, shrinkExpr, span0)
import Test.Properties.Arb.Identifiers (genIdent, shrinkIdent)
import Test.QuickCheck

instance Arbitrary Decl where
  arbitrary = genDecl
  shrink = shrinkDecl

genDecl :: Gen Decl
genDecl = sized $ \n ->
  oneof
    [ genDeclValue n,
      genDeclTypeSig
    ]

genDeclValue :: Int -> Gen Decl
genDeclValue n = do
  name <- mkUnqualifiedName NameVarId <$> genIdent
  expr <- resize n genExpr
  genFunctionDecl (name, expr)

genFunctionDecl :: (UnqualifiedName, Expr) -> Gen Decl
genFunctionDecl (name, expr) = do
  infixHead <- arbitrary
  if infixHead
    then do
      lhs <- genIdent
      rhs <- genIdent
      pure $
        DeclValue
          span0
          ( FunctionBind
              span0
              name
              [ Match
                  { matchSpan = span0,
                    matchHeadForm = MatchHeadInfix,
                    matchPats =
                      [ PVar span0 (mkUnqualifiedName NameVarId lhs),
                        PVar span0 (mkUnqualifiedName NameVarId rhs)
                      ],
                    matchRhs = UnguardedRhs span0 expr
                  }
              ]
          )
    else
      pure $
        DeclValue
          span0
          ( FunctionBind
              span0
              name
              [ Match
                  { matchSpan = span0,
                    matchHeadForm = MatchHeadPrefix,
                    matchPats = [],
                    matchRhs = UnguardedRhs span0 expr
                  }
              ]
          )

genDeclTypeSig :: Gen Decl
genDeclTypeSig = do
  name <- mkUnqualifiedName NameVarId <$> genIdent
  ty <- genSimpleType
  pure $ DeclTypeSig span0 [name] ty

-- | Generate a simple type for type signatures.
genSimpleType :: Gen Type
genSimpleType =
  oneof
    [ TVar span0 . mkUnqualifiedName NameVarId <$> genIdent,
      (\n -> TCon span0 (qualifyName Nothing (mkUnqualifiedName NameConId n)) Unpromoted) <$> genTypeConName,
      TFun span0
        <$> (TVar span0 . mkUnqualifiedName NameVarId <$> genIdent)
        <*> (TVar span0 . mkUnqualifiedName NameVarId <$> genIdent)
    ]

genTypeConName :: Gen Text
genTypeConName = do
  first <- elements ['A' .. 'Z']
  restLen <- chooseInt (0, 4)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']))
  pure (T.pack (first : rest))

shrinkDecl :: Decl -> [Decl]
shrinkDecl decl =
  case decl of
    DeclValue _ (PatternBind _ pat (UnguardedRhs _ expr)) ->
      [DeclValue span0 (PatternBind span0 pat (UnguardedRhs span0 expr')) | expr' <- shrinkExpr expr]
    DeclValue _ (FunctionBind _ name [match@Match {matchRhs = UnguardedRhs _ expr}]) ->
      [ DeclValue
          span0
          ( FunctionBind
              span0
              name
              [match {matchSpan = span0, matchRhs = UnguardedRhs span0 expr'}]
          )
      | expr' <- shrinkExpr expr
      ]
        <> [DeclValue span0 (FunctionBind span0 name' [match {matchSpan = span0, matchRhs = UnguardedRhs span0 expr}]) | name' <- shrinkUnqualifiedVarName name]
    DeclTypeSig _ names ty ->
      [DeclTypeSig span0 names' ty | names' <- shrinkList shrinkBinderName names, not (null names')]
    _ -> []

shrinkUnqualifiedVarName :: UnqualifiedName -> [UnqualifiedName]
shrinkUnqualifiedVarName name =
  [mkUnqualifiedName NameVarId candidate | candidate <- shrinkIdent (renderUnqualifiedName name)]

shrinkBinderName :: BinderName -> [BinderName]
shrinkBinderName = shrinkUnqualifiedVarName
