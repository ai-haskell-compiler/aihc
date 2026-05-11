{-# LANGUAGE OverloadedStrings #-}

module Test.Properties.MinimalParentheses
  ( prop_minimalParenthesesExpr,
  )
where

import Aihc.Parser (ParseResult (..), ParserConfig (..), defaultConfig, parseExpr)
import Aihc.Parser.Parens (addExprParens)
import Aihc.Parser.Pretty (prettyExpr)
import Aihc.Parser.Shorthand (Shorthand (shorthand))
import Aihc.Parser.Syntax
import Data.Text qualified as T
import ParserValidation (stripParens)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.Arb.Utils (requiredExtensions)
import Test.QuickCheck

config :: ParserConfig
config =
  defaultConfig
    { parserExtensions = requiredExtensions
    }

prop_minimalParenthesesExpr :: Expr -> Property
prop_minimalParenthesesExpr expr = do
  counterexample
    ( "Found smaller, valid expression. "
        ++ "Original:\n"
        ++ show (shorthand parenthesized)
        ++ "\n"
        ++ T.unpack (renderStrict (layoutPretty defaultLayoutOptions (prettyExpr parenthesized)))
        ++ "\nSmaller:\n"
        ++ show (shorthand expr)
        ++ "\n"
        ++ T.unpack (renderStrict (layoutPretty defaultLayoutOptions (prettyExpr expr)))
    )
    (isMinimal || not exprIsValid)
  where
    parenthesized = addExprParens (stripParens expr)
    source = renderStrict (layoutPretty defaultLayoutOptions (prettyExpr expr))
    isMinimal = parenthesized == expr
    exprIsValid = case parseExpr config source of ParseOk parsed -> stripAnnotations parsed == stripAnnotations expr; ParseErr {} -> False