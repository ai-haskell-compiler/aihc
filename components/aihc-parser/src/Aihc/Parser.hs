{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Aihc.Parser
-- Description : Haskell parser for the AIHC compiler
-- License     : Unlicense
--
-- This module provides parsing functions for Haskell source code.
-- The main entry point is 'parseModule' for parsing complete Haskell modules.
-- Additional functions are provided for parsing individual expressions,
-- patterns, and types.
module Aihc.Parser
  ( -- * Parsing modules
    parseModule,

    -- * Configuration
    ParserConfig (..),
    defaultConfig,

    -- * Parse results
    ParseResult (..),
    ParseErrorBundle,
    errorBundlePretty,

    -- * Parsing expressions, patterns, and types
    parseExpr,
    parseType,
    parsePattern,
  )
where

import Aihc.Parser.Internal.Expr (exprParser, patternParser, typeParser)
import Aihc.Parser.Internal.Module (moduleParser)
import Aihc.Parser.Lex
  ( lexModuleTokensWithExtensions,
    lexTokensWithExtensions,
    readModuleHeaderExtensions,
  )
import Aihc.Parser.Pretty ()
import Aihc.Parser.Syntax (Expr, Extension (..), ExtensionSetting (..), Module, Pattern, Type)
import Aihc.Parser.Types
import Data.List qualified as List
import Data.Text (Text)
import Text.Megaparsec (runParser)
import Text.Megaparsec qualified as MP

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Aihc.Parser
-- >>> import Aihc.Parser.Syntax (moduleName)
-- >>> import Aihc.Parser.Shorthand (Shorthand(..))

-- | Default parser configuration.
--
-- * 'parserSourceName' is set to @\"\<input\>\"@
-- * 'parserExtensions' is empty (no extensions enabled by default)
--
-- >>> parserSourceName defaultConfig
-- "<input>"
--
-- >>> parserExtensions defaultConfig
-- []
defaultConfig :: ParserConfig
defaultConfig =
  ParserConfig
    { parserSourceName = "<input>",
      parserExtensions = []
    }

-- | Parse a Haskell expression.
--
-- >>> shorthand $ parseExpr defaultConfig "1 + 2"
-- ParseOk (EInfix (EInt 1) "+" (EInt 2))
--
-- >>> shorthand $ parseExpr defaultConfig "\\x -> x + 1"
-- ParseOk (ELambdaPats [PVar "x"] (EInfix (EVar "x") "+" (EInt 1)))
--
-- Parse errors are returned as 'ParseErr':
--
-- >>> case parseExpr defaultConfig "1 +" of { ParseErr _ -> "error"; ParseOk _ -> "ok" }
-- "error"
parseExpr :: ParserConfig -> Text -> ParseResult Expr
parseExpr cfg input =
  let toks = lexTokensWithExtensions (parserExtensions cfg) input
   in case runParser (exprParser <* MP.eof) (parserSourceName cfg) (TokStream toks) of
        Left bundle -> ParseErr bundle
        Right expr -> ParseOk expr

-- | Parse a Haskell pattern.
--
-- >>> shorthand $ parsePattern defaultConfig "(x, y)"
-- ParseOk (PTuple [PVar "x", PVar "y"])
--
-- >>> shorthand $ parsePattern defaultConfig "Just x"
-- ParseOk (PCon "Just" [PVar "x"])
parsePattern :: ParserConfig -> Text -> ParseResult Pattern
parsePattern cfg input =
  let toks = lexTokensWithExtensions (parserExtensions cfg) input
   in case runParser (patternParser <* MP.eof) (parserSourceName cfg) (TokStream toks) of
        Left bundle -> ParseErr bundle
        Right pat -> ParseOk pat

-- | Parse a Haskell type.
--
-- >>> shorthand $ parseType defaultConfig "Int -> Bool"
-- ParseOk (TFun (TCon "Int") (TCon "Bool"))
--
-- >>> shorthand $ parseType defaultConfig "Maybe a"
-- ParseOk (TApp (TCon "Maybe") (TVar "a"))
parseType :: ParserConfig -> Text -> ParseResult Type
parseType cfg input =
  let toks = lexTokensWithExtensions (parserExtensions cfg) input
   in case runParser (typeParser <* MP.eof) (parserSourceName cfg) (TokStream toks) of
        Left bundle -> ParseErr bundle
        Right ty -> ParseOk ty

-- | Parse a complete Haskell module.
--
-- >>> shorthand $ parseModule defaultConfig "module Main where\nmain = putStrLn \"Hello\""
-- ParseOk (Module {name = "Main", decls = [DeclValue (FunctionBind "main" [Match {rhs = UnguardedRhs (EApp (EVar "putStrLn") (EString "Hello"))}])]})
--
-- Modules without a header are also supported:
--
-- >>> case parseModule defaultConfig "x = 1" of { ParseOk m -> moduleName m; ParseErr _ -> Just "error" }
-- Nothing
parseModule :: ParserConfig -> Text -> ParseResult Module
parseModule cfg input =
  let toks = lexModuleTokensWithExtensions effectiveExtensions input
   in case runParser (moduleParser <* MP.eof) (parserSourceName cfg) (TokStream toks) of
        Left bundle -> ParseErr bundle
        Right modu -> ParseOk modu
  where
    effectiveExtensions =
      applyExtensionSettings
        (parserExtensions cfg)
        (readModuleHeaderExtensions input)

applyExtensionSettings :: [Extension] -> [ExtensionSetting] -> [Extension]
applyExtensionSettings = List.foldl' applySetting
  where
    applySetting exts setting =
      case setting of
        EnableExtension ext
          | ext `elem` exts -> exts
          | otherwise -> exts <> [ext]
        DisableExtension ext -> filter (/= ext) exts

-- | Pretty-print a parse error bundle.
errorBundlePretty :: ParseErrorBundle -> String
errorBundlePretty = MP.errorBundlePretty
