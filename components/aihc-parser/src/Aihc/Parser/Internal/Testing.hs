-- | Internal parser entry points used by generative tests.
--
-- This module is exposed so the test-only @fuzz@ component can exercise the
-- token parsers compiled into the main library. It is not a stable public API.
module Aihc.Parser.Internal.Testing
  ( LexToken (..),
    LexTokenKind (..),
    TokenOrigin (..),
    lexModuleTokens,
    lexTokens,
    parseDeclFromTokens,
    parseExprFromTokens,
    parseImportDeclFromTokens,
    parseModuleFromTokens,
    parseModuleHeaderFromTokens,
    parsePatternFromTokens,
    parseTypeFromTokens,
  )
where

import Aihc.Parser.Internal.FromTokens
import Aihc.Parser.Lex
