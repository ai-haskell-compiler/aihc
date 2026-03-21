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
import Test.Properties.NoExceptions
  ( prop_declParserArbitraryTokensNoExceptions,
    prop_exprParserArbitraryTokensNoExceptions,
    prop_importDeclParserArbitraryTokensNoExceptions,
    prop_lexerArbitraryTextNoExceptions,
    prop_moduleHeaderParserArbitraryTokensNoExceptions,
    prop_moduleParserArbitraryTokensNoExceptions,
    prop_patternParserArbitraryTokensNoExceptions,
    prop_preprocessorArbitraryTextNoExceptions,
    prop_typeParserArbitraryTokensNoExceptions,
  )
import Test.Properties.PatternRoundTrip (prop_patternPrettyRoundTrip)
import Test.Properties.TypeRoundTrip (prop_typePrettyRoundTrip)

registeredParserProperties :: [RegisteredProperty]
registeredParserProperties =
  [ RegisteredProperty "preprocessor arbitrary Text does not throw exceptions" prop_preprocessorArbitraryTextNoExceptions,
    RegisteredProperty "lexer arbitrary Text does not throw exceptions" prop_lexerArbitraryTextNoExceptions,
    RegisteredProperty "module parser arbitrary token stream does not throw exceptions" prop_moduleParserArbitraryTokensNoExceptions,
    RegisteredProperty "expr parser arbitrary token stream does not throw exceptions" prop_exprParserArbitraryTokensNoExceptions,
    RegisteredProperty "type parser arbitrary token stream does not throw exceptions" prop_typeParserArbitraryTokensNoExceptions,
    RegisteredProperty "pattern parser arbitrary token stream does not throw exceptions" prop_patternParserArbitraryTokensNoExceptions,
    RegisteredProperty "decl parser arbitrary token stream does not throw exceptions" prop_declParserArbitraryTokensNoExceptions,
    RegisteredProperty "import decl parser arbitrary token stream does not throw exceptions" prop_importDeclParserArbitraryTokensNoExceptions,
    RegisteredProperty "module header parser arbitrary token stream does not throw exceptions" prop_moduleHeaderParserArbitraryTokensNoExceptions,
    RegisteredProperty "generated expr AST pretty-printer round-trip" prop_exprPrettyRoundTrip,
    RegisteredProperty "generated module AST pretty-printer round-trip" prop_modulePrettyRoundTrip,
    RegisteredProperty "generated pattern AST pretty-printer round-trip" prop_patternPrettyRoundTrip,
    RegisteredProperty "generated type AST pretty-printer round-trip" prop_typePrettyRoundTrip
  ]
