{-# LANGUAGE OverloadedStrings #-}

module Test.Properties.MinimalParentheses
  ( prop_minimalParenthesesExpr,
    prop_minimalParenthesesPattern,
    prop_minimalParenthesesType,
    prop_minimalParenthesesSignatureType,
  )
where

import Aihc.Parser (ParseResult (..), ParserConfig (..), defaultConfig, parseExpr, parsePattern, parseSignatureType, parseType)
import Aihc.Parser.Parens (addExprParens, addPatternParens, addSignatureTypeParens, addTypeParens)
import Aihc.Parser.Pretty (prettyExpr, prettyPattern, prettyType)
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
prop_minimalParenthesesPattern pat = do
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
    noParens = stripParens pat
    parenthesized = addPatternParens noParens
    source = renderStrict (layoutPretty defaultLayoutOptions (prettyPattern noParens))
    isMinimal = parenthesized == noParens
    patternIsValid = case parsePattern config source of ParseOk parsed -> stripAnnotations parsed == stripAnnotations noParens; ParseErr {} -> False

prop_minimalParenthesesSignatureType :: Type -> Property
prop_minimalParenthesesSignatureType ty = do
  counterexample
    ( "Found smaller, valid signature type. "
        ++ "Original:\n"
        ++ show (shorthand parenthesized)
        ++ "\n"
        ++ T.unpack (renderStrict (layoutPretty defaultLayoutOptions (prettyType parenthesized)))
        ++ "\nSmaller:\n"
        ++ show (shorthand noParens)
        ++ "\n"
        ++ T.unpack (renderStrict (layoutPretty defaultLayoutOptions (prettyType noParens)))
    )
    (isMinimal || not typeIsValid)
  where
    noParens = stripParens ty
    parenthesized = addSignatureTypeParens noParens
    source = renderStrict (layoutPretty defaultLayoutOptions (prettyType noParens))
    isMinimal = parenthesized == noParens
    typeIsValid = case parseSignatureType config source of ParseOk parsed -> stripAnnotations parsed == stripAnnotations noParens; ParseErr {} -> False

prop_minimalParenthesesType :: Type -> Property
prop_minimalParenthesesType ty = do
  counterexample
    ( "Found smaller, valid type. "
        ++ "Original:\n"
        ++ show (shorthand parenthesized)
        ++ "\n"
        ++ T.unpack (renderStrict (layoutPretty defaultLayoutOptions (prettyType parenthesized)))
        ++ "\nSmaller:\n"
        ++ show (shorthand noParens)
        ++ "\n"
        ++ T.unpack (renderStrict (layoutPretty defaultLayoutOptions (prettyType noParens)))
    )
    (isMinimal || not typeIsValid)
  where
    noParens = stripParens ty
    parenthesized = addTypeParens noParens
    source = renderStrict (layoutPretty defaultLayoutOptions (prettyType noParens))
    isMinimal = parenthesized == noParens
    typeIsValid = case parseType config source of ParseOk parsed -> stripAnnotations parsed == stripAnnotations noParens; ParseErr {} -> False
