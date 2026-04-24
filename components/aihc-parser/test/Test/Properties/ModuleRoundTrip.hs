{-# LANGUAGE OverloadedStrings #-}

module Test.Properties.ModuleRoundTrip
  ( prop_modulePrettyRoundTrip,
  )
where

import Aihc.Parser
import Aihc.Parser.Parens (addModuleParens)
import Aihc.Parser.Shorthand (Shorthand (shorthand))
import Aihc.Parser.Syntax
import Data.Text qualified as T
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.Arb.Module ()
import Test.QuickCheck

prop_modulePrettyRoundTrip :: Module -> Property
prop_modulePrettyRoundTrip modu =
  let source = renderStrict (layoutPretty defaultLayoutOptions (pretty modu))
      (errs, reparsed) = parseModule moduleConfig source
      reparsedSource = renderStrict (layoutPretty defaultLayoutOptions (pretty reparsed))
   in counterexample ("Original source:\n" <> T.unpack source) $
        counterexample ("Reparsed source:\n" <> T.unpack reparsedSource) $
          case errs of
            [] ->
              let expected = stripAnnotations (addModuleParens modu)
                  actual = stripAnnotations reparsed
               in counterexample ("Original AST:\n" <> show (shorthand expected) <> "\nActual AST:\n" <> show (shorthand actual)) (expected == actual)
            _ ->
              counterexample (formatParseErrors "<quickcheck>" (Just source) errs) False

moduleConfig :: ParserConfig
moduleConfig =
  defaultConfig
    { parserExtensions = effectiveExtensions GHC2024Edition [EnableExtension BlockArguments, EnableExtension Arrows, EnableExtension UnboxedTuples, EnableExtension UnboxedSums, EnableExtension TemplateHaskell, EnableExtension UnicodeSyntax, EnableExtension QuasiQuotes, EnableExtension PatternSynonyms, EnableExtension MagicHash, EnableExtension OverloadedLabels, EnableExtension MultiWayIf, EnableExtension RecursiveDo, EnableExtension TypeApplications, EnableExtension TupleSections, EnableExtension CApiFFI, EnableExtension ImplicitParams, EnableExtension ExplicitNamespaces, EnableExtension TypeAbstractions, EnableExtension RequiredTypeArguments, EnableExtension LambdaCase]
    }
