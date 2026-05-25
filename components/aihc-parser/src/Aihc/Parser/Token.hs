-- |
-- Module      : Aihc.Parser.Token
-- Description : Public token API for inspecting lexer output
--
-- This module exposes the stable token-facing surface of the parser without
-- making callers depend on the internal lexer implementation. It is intended
-- for diagnostics, tooling, generators, and tests that need to inspect the
-- token stream directly instead of parsing it into the AST.
--
-- A 'LexToken' records the token 'LexTokenKind', original source text, source
-- span, whether it started at the beginning of a logical line, and its
-- 'TokenOrigin'. Tokens with 'FromSource' come from the input text; tokens with
-- 'InsertedLayout' are virtual braces or semicolons inserted by the layout
-- pass.
--
-- The @lexTokens*@ functions scan arbitrary source fragments. The
-- @lexModuleTokens*@ variants additionally read the module header first, apply
-- any @LANGUAGE@ pragmas (including implied extensions), and then run layout in
-- module mode so top-level declarations receive the same virtual layout tokens
-- as the parser. Use the @WithSourceName@ variants when diagnostics should
-- carry a caller-provided source name instead of @"<input>"@.
--
-- 'readModuleHeaderExtensions' returns the raw extension settings discovered in
-- the leading module pragmas, while 'readModuleHeaderPragmas' separates a
-- language edition from explicit extension toggles.
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
