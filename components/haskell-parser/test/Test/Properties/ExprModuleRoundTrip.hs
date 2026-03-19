{-# LANGUAGE OverloadedStrings #-}

module Test.Properties.ExprModuleRoundTrip
  ( prop_exprPrettyRoundTrip,
    prop_modulePrettyRoundTrip,
    GenExpr,
    GenModule,
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

prop_exprPrettyRoundTrip :: GenExpr -> Property
prop_exprPrettyRoundTrip generated =
  let expr = toExpr generated
      source = prettyExpr expr
   in counterexample (T.unpack source) $
        case parseExpr defaultConfig source of
          ParseOk reparsed ->
            case (expr, reparsed) of
              (EVar _ expected, EVar _ actual) ->
                counterexample ("reparsed variable mismatch: " <> show reparsed) (property (expected == actual))
              _ -> property True
          ParseErr _ -> property True

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

isModuleSupportedExpr :: GenExpr -> Bool
isModuleSupportedExpr generated =
  case generated of
    GVar _ -> True
    GInt _ -> True
    GApp fn arg -> isModuleSupportedExpr fn && isModuleSupportedExpr arg

newtype GenModule = GenModule {unGenModule :: [(Text, GenExpr)]}
  deriving (Show)

instance Arbitrary GenModule where
  arbitrary = do
    n <- chooseInt (1, 6)
    names <- vectorOf n genIdent
    exprs <- vectorOf n (genExpr 4)
    pure (GenModule (zip names exprs))

data GenExpr
  = GVar Text
  | GInt Integer
  | GApp GenExpr GenExpr
  deriving (Eq, Show)

instance Arbitrary GenExpr where
  arbitrary = sized (genExpr . min 5)
  shrink expr =
    case expr of
      GVar name -> [GVar shrunk | shrunk <- shrinkIdent name]
      GInt value -> [GInt shrunk | shrunk <- shrinkIntegral value]
      GApp fn arg -> [fn, arg] <> [GApp fn' arg | fn' <- shrink fn] <> [GApp fn arg' | arg' <- shrink arg]

genExpr :: Int -> Gen GenExpr
genExpr depth
  | depth <= 0 = oneof [GVar <$> genIdent, GInt <$> chooseInteger (0, 999)]
  | otherwise =
      frequency
        [ (3, GVar <$> genIdent),
          (3, GInt <$> chooseInteger (0, 999)),
          (4, GApp <$> genExpr (depth - 1) <*> genExpr (depth - 1))
        ]

toExpr :: GenExpr -> Expr
toExpr generated =
  case generated of
    GVar name -> EVar span0 name
    GInt value -> EInt span0 value (T.pack (show value))
    GApp fn arg -> EApp span0 (toExpr fn) (toExpr arg)

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
                      matchRhs = UnguardedRhs span0 (toExpr expr)
                    }
                ]
            )
        | (name, expr) <- decls
        ]
    }
