{-# LANGUAGE OverloadedStrings #-}

module Test.Properties.MinimalParentheses
  ( prop_minimalParenthesesExpr,
    prop_minimalParenthesesPattern,
  )
where

import Aihc.Parser (ParseResult (..), ParserConfig (..), defaultConfig, parseExpr, parsePattern)
import Aihc.Parser.Parens (addExprParens, addPatternParens)
import Aihc.Parser.Pretty (prettyExpr, prettyPattern)
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
        ++ show (shorthand noParens)
        ++ "\n"
        ++ T.unpack (renderStrict (layoutPretty defaultLayoutOptions (prettyExpr noParens)))
    )
    (isMinimal || not exprIsValid)
  where
    noParens = stripParens expr
    parenthesized = addExprParens noParens
    source = renderStrict (layoutPretty defaultLayoutOptions (prettyExpr noParens))
    isMinimal = parenthesized == noParens
    exprIsValid = case parseExpr config source of ParseOk parsed -> stripAnnotations parsed == stripAnnotations noParens; ParseErr {} -> False

prop_minimalParenthesesPattern :: Pattern -> Property
prop_minimalParenthesesPattern pattern = do
  counterexample
    ( "Found smaller, valid pattern. "
        ++ "Original:\n"
        ++ show (shorthand parenthesized)
        ++ "\n"
        ++ T.unpack (renderStrict (layoutPretty defaultLayoutOptions (prettyPattern parenthesized)))
        ++ "\nSmaller:\n"
        ++ show (shorthand noParens)
        ++ "\n"
        ++ T.unpack (renderStrict (layoutPretty defaultLayoutOptions (prettyPattern noParens)))
    )
    (isMinimal || not patternIsValid)
  where
    noParens = stripParens pattern
    parenthesized = addPatternParens noParens
    source = renderStrict (layoutPretty defaultLayoutOptions (prettyPattern noParens))
    isMinimal = parenthesized == noParens
    patternIsValid = case parsePattern config source of ParseOk parsed -> stripAnnotations parsed == stripAnnotations noParens; ParseErr {} -> False
