{-# LANGUAGE OverloadedStrings #-}

module Test.Properties.PatternRoundTrip
  ( prop_patternPrettyRoundTrip,
  )
where

import Aihc.Parser (ParseResult (..), ParserConfig (..), defaultConfig, formatParseErrors, parsePattern)
import Aihc.Parser.Parens (addPatternParens)
import Aihc.Parser.Syntax
import Data.Text qualified as T
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.Arb.Pattern ()
import Test.Properties.Arb.Utils (requiredExtensions)
import Test.Properties.Coverage (assertCtorCoverage)
import Test.QuickCheck

patternConfig :: ParserConfig
patternConfig =
  defaultConfig
    { parserExtensions = requiredExtensions
    }

prop_patternPrettyRoundTrip :: Pattern -> Property
prop_patternPrettyRoundTrip pat =
  let source = renderStrict (layoutPretty defaultLayoutOptions (pretty pat))
      expected = stripAnnotations (addPatternParens pat)
   in checkCoverage $
        assertCtorCoverage ["PAnn", "PTypeBinder", "PTypeSyntax"] pat $
          counterexample (T.unpack source) $
            case parsePattern patternConfig source of
              ParseErr err ->
                counterexample (formatParseErrors (parserSourceName patternConfig) (Just source) err) False
              ParseOk parsed ->
                let actual = stripAnnotations parsed
                 in counterexample ("expected: " <> show expected <> "\nactual: " <> show actual) (expected == actual)
