{-# LANGUAGE OverloadedStrings #-}

module Test.Properties.DeclRoundTrip
  ( prop_declPrettyRoundTrip,
  )
where

import Aihc.Parser (ParseResult (..), ParserConfig (..), defaultConfig, parseDecl)
import Aihc.Parser.Pretty ()
import Aihc.Parser.Syntax
import Data.Text qualified as T
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.Coverage (assertCtorCoverage)
import Test.Properties.ExprHelpers (normalizeDecl)
import Test.QuickCheck
import Text.Megaparsec.Error qualified as MPE

declConfig :: ParserConfig
declConfig =
  defaultConfig
    { parserExtensions = [BlockArguments, UnboxedTuples, UnboxedSums, TemplateHaskell, PatternSynonyms]
    }

prop_declPrettyRoundTrip :: Decl -> Property
prop_declPrettyRoundTrip decl =
  let source = renderStrict (layoutPretty defaultLayoutOptions (pretty decl))
      expected = normalizeDecl decl
   in assertCtorCoverage ["DeclAnn"] decl $
        counterexample (T.unpack source) $
          case parseDecl declConfig source of
            ParseErr err ->
              counterexample (MPE.errorBundlePretty err) False
            ParseOk parsed ->
              let actual = normalizeDecl parsed
               in counterexample ("expected: " <> show expected <> "\nactual: " <> show actual) (expected == actual)
