{-# LANGUAGE OverloadedStrings #-}

module ParserQuickCheck.Registry
  ( registeredParserProperties,
  )
where

import ParserQuickCheck.Runner (RegisteredProperty (..))
import Test.Properties.ExprModuleRoundTrip
  ( prop_exprPrettyRoundTrip,
    prop_modulePrettyRoundTrip,
  )
import Test.Properties.PatternRoundTrip (prop_patternPrettyRoundTrip)
import Test.Properties.TypeRoundTrip (prop_typePrettyRoundTrip)

registeredParserProperties :: [RegisteredProperty]
registeredParserProperties =
  [ RegisteredProperty "generated expr AST pretty-printer round-trip" prop_exprPrettyRoundTrip,
    RegisteredProperty "generated module AST pretty-printer round-trip" prop_modulePrettyRoundTrip,
    RegisteredProperty "generated pattern AST pretty-printer round-trip" prop_patternPrettyRoundTrip,
    RegisteredProperty "generated type AST pretty-printer round-trip" prop_typePrettyRoundTrip
  ]
