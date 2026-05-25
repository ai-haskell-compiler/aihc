{-# LANGUAGE PatternSynonyms #-}

-- | Public token API for consumers that need to inspect lexer output.
module Aihc.Parser.Token
  ( TokenOrigin (..),
    LexToken (..),
    LexTokenKind (..),
    pattern TkVarRole,
    pattern TkVarFamily,
    pattern TkVarAs,
    pattern TkVarHiding,
    pattern TkVarQualified,
    pattern TkVarSafe,
    readModuleHeaderExtensions,
    readModuleHeaderPragmas,
    lexTokensWithExtensions,
    lexModuleTokensWithExtensions,
    lexTokensWithSourceNameAndExtensions,
    lexModuleTokensWithSourceNameAndExtensions,
    lexTokens,
    lexModuleTokens,
  )
where

import Aihc.Parser.Lex
