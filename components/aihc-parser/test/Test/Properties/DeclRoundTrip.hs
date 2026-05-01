{-# LANGUAGE OverloadedStrings #-}

module Test.Properties.DeclRoundTrip
  ( prop_declPrettyRoundTrip,
  )
where

import Aihc.Parser (ParseResult (..), ParserConfig (..), defaultConfig, parseDecl)
import Aihc.Parser.Parens (addDeclParens)
import Aihc.Parser.Pretty ()
import Aihc.Parser.Syntax
import Data.Text qualified as T
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.Coverage (assertCtorCoverage)
import Test.QuickCheck
import Text.Megaparsec.Error qualified as MPE

declConfig :: ParserConfig
declConfig =
  defaultConfig
    { parserExtensions = effectiveExtensions GHC2024Edition [EnableExtension BlockArguments, EnableExtension Arrows, EnableExtension UnboxedTuples, EnableExtension UnboxedSums, EnableExtension TemplateHaskell, EnableExtension QuasiQuotes, EnableExtension PatternSynonyms, EnableExtension UnicodeSyntax, EnableExtension MagicHash, EnableExtension OverloadedLabels, EnableExtension MultiWayIf, EnableExtension RecursiveDo, EnableExtension TypeApplications, EnableExtension TupleSections, EnableExtension CApiFFI, EnableExtension ImplicitParams, EnableExtension ExplicitNamespaces, EnableExtension TypeAbstractions, EnableExtension RequiredTypeArguments, EnableExtension LambdaCase, EnableExtension LinearTypes, EnableExtension OverloadedRecordDot, EnableExtension TransformListComp, EnableExtension QualifiedDo]
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
                  counterexample (MPE.errorBundlePretty err) False
                ParseOk parsed ->
                  let actual = stripAnnotations parsed
                   in counterexample ("expected:\n" <> show expected <> "\nactual:\n" <> show actual) (expected == actual)
