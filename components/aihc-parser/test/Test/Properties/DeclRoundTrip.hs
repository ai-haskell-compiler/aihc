{-# LANGUAGE OverloadedStrings #-}

module Test.Properties.DeclRoundTrip
  ( prop_declPrettyRoundTrip,
  )
where

import Aihc.Parser (ParseResult (..), ParserConfig (..), defaultConfig, formatParseErrors, parseDecl)
import Aihc.Parser.Parens (addDeclParens)
import Aihc.Parser.Pretty ()
import Aihc.Parser.Shorthand (Shorthand (shorthand))
import Aihc.Parser.Syntax
import Data.Text qualified as T
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.Arb.Utils (requiredExtensions)
import Test.Properties.Coverage (assertCtorCoverage)
import Test.QuickCheck

declConfig :: ParserConfig
declConfig =
  defaultConfig
    { parserExtensions = requiredExtensions
    }

prop_declPrettyRoundTrip :: Decl -> Property
prop_declPrettyRoundTrip decl =
  let parenthesized = addDeclParens decl
      source = renderStrict (layoutPretty defaultLayoutOptions (pretty parenthesized))
      expected = stripAnnotations parenthesized
      addValueDeclCoverage prop =
        case decl of
          DeclValue valueDecl -> assertCtorCoverage [] valueDecl prop
          _ -> prop
   in checkCoverage $
        addValueDeclCoverage $
          assertCtorCoverage ["DeclAnn"] decl $
            counterexample (T.unpack source) $
              case parseDecl declConfig source of
                ParseErr err ->
                  counterexample (formatParseErrors (parserSourceName declConfig) (Just source) err) False
                ParseOk parsed ->
                  let actual = stripAnnotations parsed
                   in counterexample ("expected:\n" <> show (shorthand expected) <> "\nactual:\n" <> show (shorthand actual)) (expected == actual)
