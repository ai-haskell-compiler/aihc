-- | Continuously runnable QuickCheck properties owned by @aihc-parser@.
module Aihc.Parser.Fuzz
  ( parserFuzzProperties,
  )
where

import Test.Properties.DeclRoundTrip (prop_declPrettyRoundTrip)
import Test.Properties.ExprRoundTrip (prop_exprPrettyRoundTrip)
import Test.Properties.MinimalParentheses (prop_minimalParenthesesExpr, prop_minimalParenthesesPattern, prop_minimalParenthesesSignatureType, prop_minimalParenthesesType)
import Test.Properties.ModuleRoundTrip (prop_modulePrettyRoundTrip, prop_moduleValidator)
import Test.Properties.NoExceptions
  ( prop_declParserArbitraryTokensNoExceptions,
    prop_exprParserArbitraryTokensNoExceptions,
    prop_genLexTokenKindConstructorCoverage,
    prop_importDeclParserArbitraryTokensNoExceptions,
    prop_lexerArbitraryTextNoExceptions,
    prop_moduleHeaderParserArbitraryTokensNoExceptions,
    prop_moduleParserArbitraryTokensNoExceptions,
    prop_patternParserArbitraryTokensNoExceptions,
    prop_preprocessorArbitraryTextNoExceptions,
    prop_typeParserArbitraryTokensNoExceptions,
  )
import Test.Properties.ParensIdempotency (prop_declParensIdempotent, prop_exprParensIdempotent, prop_moduleParensIdempotent, prop_patternParensIdempotent, prop_typeParensIdempotent)
import Test.Properties.PatternRoundTrip (prop_patternPrettyRoundTrip)
import Test.Properties.ShorthandSubset (prop_shorthandDeclSubsetOfShow, prop_shorthandExprSubsetOfShow, prop_shorthandLexTokenSubsetOfShow, prop_shorthandModuleSubsetOfShow, prop_shorthandTypeSubsetOfShow)
import Test.Properties.TypeRoundTrip (prop_typePrettyRoundTrip)
import Test.QuickCheck (Property, Testable, property)

parserFuzzProperties :: [(String, Property)]
parserFuzzProperties =
  [ named "expr paren insertion is minimal" prop_minimalParenthesesExpr,
    named "pattern paren insertion is minimal" prop_minimalParenthesesPattern,
    named "signature type paren insertion is minimal" prop_minimalParenthesesSignatureType,
    named "type paren insertion is minimal" prop_minimalParenthesesType,
    named "generated expr AST pretty-printer round-trip" prop_exprPrettyRoundTrip,
    named "generated decl AST pretty-printer round-trip" prop_declPrettyRoundTrip,
    named "generated module AST pretty-printer round-trip" prop_modulePrettyRoundTrip,
    named "generated module AST validator" prop_moduleValidator,
    named "generated pattern AST pretty-printer round-trip" prop_patternPrettyRoundTrip,
    named "generated type AST pretty-printer round-trip" prop_typePrettyRoundTrip,
    named "module paren insertion is idempotent" prop_moduleParensIdempotent,
    named "decl paren insertion is idempotent" prop_declParensIdempotent,
    named "expr paren insertion is idempotent" prop_exprParensIdempotent,
    named "pattern paren insertion is idempotent" prop_patternParensIdempotent,
    named "type paren insertion is idempotent" prop_typeParensIdempotent,
    named "module shorthand is a subset of Show" prop_shorthandModuleSubsetOfShow,
    named "decl shorthand is a subset of Show" prop_shorthandDeclSubsetOfShow,
    named "expr shorthand is a subset of Show" prop_shorthandExprSubsetOfShow,
    named "type shorthand is a subset of Show" prop_shorthandTypeSubsetOfShow,
    named "lex token shorthand is a subset of Show" prop_shorthandLexTokenSubsetOfShow,
    named "lex token kind generator covers constructors" prop_genLexTokenKindConstructorCoverage,
    named "no exceptions.preprocessor accepts arbitrary text" prop_preprocessorArbitraryTextNoExceptions,
    named "no exceptions.lexer accepts arbitrary text" prop_lexerArbitraryTextNoExceptions,
    named "no exceptions.module parser accepts arbitrary tokens" prop_moduleParserArbitraryTokensNoExceptions,
    named "no exceptions.expr parser accepts arbitrary tokens" prop_exprParserArbitraryTokensNoExceptions,
    named "no exceptions.type parser accepts arbitrary tokens" prop_typeParserArbitraryTokensNoExceptions,
    named "no exceptions.pattern parser accepts arbitrary tokens" prop_patternParserArbitraryTokensNoExceptions,
    named "no exceptions.decl parser accepts arbitrary tokens" prop_declParserArbitraryTokensNoExceptions,
    named "no exceptions.import decl parser accepts arbitrary tokens" prop_importDeclParserArbitraryTokensNoExceptions,
    named "no exceptions.module header parser accepts arbitrary tokens" prop_moduleHeaderParserArbitraryTokensNoExceptions
  ]
  where
    named :: (Testable prop) => String -> prop -> (String, Property)
    named name value = (name, property value)
