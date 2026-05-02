{-# LANGUAGE OverloadedStrings #-}

module Test.Properties.ModuleRoundTrip
  ( prop_modulePrettyRoundTrip,
    prop_moduleValidator,
  )
where

import Aihc.Parser
import Aihc.Parser.Parens (addModuleParens)
import Aihc.Parser.Shorthand (Shorthand (shorthand))
import Aihc.Parser.Syntax
import Data.Text qualified as T
import ParserValidation (validateParser)
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.Arb.Module ()
import Test.Properties.Arb.Utils (requiredExtensions)
import Test.QuickCheck

prop_modulePrettyRoundTrip :: Module -> Property
prop_modulePrettyRoundTrip modu =
  let source = renderStrict (layoutPretty defaultLayoutOptions (pretty modu))
      (errs, reparsed) = parseModule moduleConfig source
      reparsedSource = renderStrict (layoutPretty defaultLayoutOptions (pretty reparsed))
   in counterexample ("Original source:\n" <> T.unpack source) $
        case errs of
          [] ->
            counterexample ("Reparsed source:\n" <> T.unpack reparsedSource) $
              let expected = stripAnnotations (addModuleParens modu)
                  actual = stripAnnotations reparsed
               in counterexample ("Original AST:\n" <> show (shorthand expected) <> "\nActual AST:\n" <> show (shorthand actual)) (expected == actual)
          _ ->
            counterexample (formatParseErrors "<quickcheck>" (Just source) errs) False

prop_moduleValidator :: Module -> Property
prop_moduleValidator modu =
  let source = renderStrict (layoutPretty defaultLayoutOptions (pretty modu))
      mbErr = validateParser "<quickcheck>" GHC2024Edition (map EnableExtension requiredExtensions) source
   in counterexample ("Original source:\n" <> T.unpack source) $
        case mbErr of
          Nothing -> property True
          Just errs ->
            counterexample (show errs) False

moduleConfig :: ParserConfig
moduleConfig =
  defaultConfig
    { parserExtensions = requiredExtensions
    }
