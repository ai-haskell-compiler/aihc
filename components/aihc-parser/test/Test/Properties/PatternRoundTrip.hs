{-# LANGUAGE OverloadedStrings #-}

module Test.Properties.PatternRoundTrip
  ( prop_patternPrettyRoundTrip,
  )
where

import Aihc.Parser (ParseResult (..), ParserConfig (..), defaultConfig, parsePattern)
import Aihc.Parser.Parens (addPatternParens)
import Aihc.Parser.Syntax
import Data.Text qualified as T
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.Arb.Pattern ()
import Test.Properties.Coverage (assertCtorCoverage)
import Test.QuickCheck
import Text.Megaparsec.Error qualified as MPE

patternConfig :: ParserConfig
patternConfig =
  defaultConfig
    { parserExtensions = [Arrows, BlockArguments, UnboxedTuples, UnboxedSums, TemplateHaskell, MagicHash, OverloadedLabels, TypeApplications, MultiWayIf, RecursiveDo, TupleSections, ImplicitParams, ExplicitNamespaces, TypeAbstractions, RequiredTypeArguments, LambdaCase]
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
                counterexample (MPE.errorBundlePretty err) False
              ParseOk parsed ->
                let actual = stripAnnotations parsed
                 in counterexample ("expected: " <> show expected <> "\nactual: " <> show actual) (expected == actual)
