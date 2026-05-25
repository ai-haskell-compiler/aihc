{-# LANGUAGE PatternSynonyms #-}

-- | Public token API for consumers that need to inspect lexer output.
module Aihc.Parser.Token
  ( TokenOrigin (..),
    LexToken (..),
    LexTokenKind (..),
    readModuleHeaderExtensions,
    readModuleHeaderPragmas,
    lexTokensWithExtensions,
    lexModuleTokensWithExtensions,
    lexTokensWithSourceNameAndExtensions,
    lexModuleTokensWithSourceNameAndExtensions,
  )
where

import Aihc.Parser.Lex
