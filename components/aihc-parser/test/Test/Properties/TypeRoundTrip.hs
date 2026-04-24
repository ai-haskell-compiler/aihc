{-# LANGUAGE OverloadedStrings #-}

module Test.Properties.TypeRoundTrip
  ( prop_typePrettyRoundTrip,
  )
where

import Aihc.Parser
import Aihc.Parser.Parens (addTypeParens)
import Aihc.Parser.Syntax
import Data.Text qualified as T
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.Coverage (assertCtorCoverage)
import Test.QuickCheck
import Text.Megaparsec.Error qualified as MPE

typeConfig :: ParserConfig
typeConfig =
  defaultConfig
    { parserExtensions = effectiveExtensions GHC2024Edition [EnableExtension BlockArguments, EnableExtension UnboxedTuples, EnableExtension UnboxedSums, EnableExtension TemplateHaskell, EnableExtension MagicHash, EnableExtension ImplicitParams]
    }

prop_typePrettyRoundTrip :: Type -> Property
prop_typePrettyRoundTrip ty =
  let source = renderStrict (layoutPretty defaultLayoutOptions (pretty ty))
      expected = stripAnnotations (addTypeParens ty)
   in checkCoverage $
        withMaxShrinks 100 $
          assertCtorCoverage ["TAnn", "TInfix", "TTypeApp"] ty $
            counterexample (T.unpack source) $
              case parseType typeConfig source of
                ParseErr err ->
                  counterexample (MPE.errorBundlePretty err) False
                ParseOk parsed ->
                  let actual = stripAnnotations parsed
                   in counterexample ("expected: " <> show expected <> "\nactual: " <> show actual) (expected == actual)
