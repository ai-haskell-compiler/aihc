{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.ExprModuleRoundTrip
  ( prop_exprPrettyRoundTrip,
    prop_modulePrettyRoundTrip,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Parser
import Parser.Ast
import Parser.Pretty (prettyExpr, prettyModule)
import Parser.Types (ParseResult (..))
import Test.Properties.Identifiers (genIdent, shrinkIdent)
import Test.QuickCheck

span0 :: SourceSpan
span0 = noSourceSpan

prop_exprPrettyRoundTrip :: Expr -> Property
prop_exprPrettyRoundTrip expr =
  let source = prettyExpr expr
      expected = normalizeExpr expr
   in checkCoverage $
        exprCoverage expr $
          counterexample (T.unpack source) $
            case parseExpr defaultConfig source of
              ParseErr err ->
                counterexample (errorBundlePretty err) False
              ParseOk parsed ->
                let actual = normalizeExpr parsed
                 in counterexample ("expected: " <> show expected <> "\nactual: " <> show actual) (expected == actual)

exprCoverage :: Expr -> Property -> Property
exprCoverage expr =
  cover 20 (hasVarExpr expr) "contains variable"
    . cover 20 (hasIntExpr expr) "contains integer"
    . cover 20 (hasAppExpr expr) "contains application"

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

moduleOnlyUsesSupportedExprs :: GenModule -> Bool
moduleOnlyUsesSupportedExprs (GenModule decls) = all (isModuleSupportedExpr . snd) decls

isModuleSupportedExpr :: Expr -> Bool
isModuleSupportedExpr expr =
  case expr of
    EVar _ _ -> True
    EInt {} -> True
    EApp _ fn arg -> isModuleSupportedExpr fn && isModuleSupportedExpr arg
    EParen _ inner -> isModuleSupportedExpr inner
    _ -> False

newtype GenModule = GenModule {unGenModule :: [(Text, Expr)]}
  deriving (Show)

instance Arbitrary GenModule where
  arbitrary = do
    n <- chooseInt (1, 6)
    names <- vectorOf n genIdent
    exprs <- vectorOf n (genExpr 4)
    pure (GenModule (zip names exprs))

instance Arbitrary Expr where
  arbitrary = sized (genExpr . min 5)
  shrink = shrinkExpr

shrinkExpr :: Expr -> [Expr]
shrinkExpr expr =
  case expr of
    EVar _ name -> [EVar span0 shrunk | shrunk <- shrinkIdent name]
    EInt _ value _ -> [mkIntExpr shrunk | shrunk <- shrinkIntegral value]
    EApp _ fn arg ->
      [fn, arg]
        <> [EApp span0 fn' arg | fn' <- shrinkExpr fn]
        <> [EApp span0 fn arg' | arg' <- shrinkExpr arg]
    EParen _ inner -> inner : [EParen span0 inner' | inner' <- shrinkExpr inner]
    _ -> []

genExpr :: Int -> Gen Expr
genExpr depth
  | depth <= 0 = oneof [EVar span0 <$> genIdent, mkIntExpr <$> chooseInteger (0, 999)]
  | otherwise =
      frequency
        [ (3, EVar span0 <$> genIdent),
          (3, mkIntExpr <$> chooseInteger (0, 999)),
          (4, EApp span0 <$> genExpr (depth - 1) <*> genExpr (depth - 1))
        ]

mkIntExpr :: Integer -> Expr
mkIntExpr value = EInt span0 value (T.pack (show value))

hasVarExpr :: Expr -> Bool
hasVarExpr expr =
  case expr of
    EVar _ _ -> True
    EApp _ fn arg -> hasVarExpr fn || hasVarExpr arg
    EParen _ inner -> hasVarExpr inner
    _ -> False

hasIntExpr :: Expr -> Bool
hasIntExpr expr =
  case expr of
    EInt {} -> True
    EApp _ fn arg -> hasIntExpr fn || hasIntExpr arg
    EParen _ inner -> hasIntExpr inner
    _ -> False

hasAppExpr :: Expr -> Bool
hasAppExpr expr =
  case expr of
    EApp {} -> True
    EParen _ inner -> hasAppExpr inner
    _ -> False

normalizeExpr :: Expr -> Expr
normalizeExpr expr =
  case expr of
    EVar _ name -> EVar span0 name
    EInt _ value _ -> mkIntExpr value
    EApp _ fn arg -> EApp span0 (normalizeExpr fn) (normalizeExpr arg)
    EParen _ inner -> normalizeExpr inner
    _ -> expr

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
                      matchRhs = UnguardedRhs span0 expr
                    }
                ]
            )
        | (name, expr) <- decls
        ]
    }
