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

import Aihc.Lexer
  ( lexModuleTokensWithExtensions,
    lexTokensWithExtensions,
    readModuleHeaderExtensions,
  )
import Aihc.Parser.Ast (Expr, Extension (..), ExtensionSetting (..), Module, Pattern, Type)
import Aihc.Parser.Internal.Expr (exprParser, patternParser, typeParser)
import Aihc.Parser.Internal.Parser (moduleParser)
import Aihc.Parser.Types
import qualified Data.List as List
import Data.Text (Text)
import Text.Megaparsec (runParser)
import qualified Text.Megaparsec as MP

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Aihc.Parser
-- >>> import Aihc.Parser.Ast (moduleName)
-- >>> import Aihc.Parser.PrettyAST (prettyASTExpr, prettyASTPattern, prettyASTType, prettyASTModule)

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
-- >>> case parseExpr defaultConfig "1 + 2" of { ParseOk expr -> prettyASTExpr expr; ParseErr _ -> "error" }
-- "EInfix (EInt 1) \"+\" (EInt 2)"
--
-- >>> case parseExpr defaultConfig "\\x -> x + 1" of { ParseOk expr -> prettyASTExpr expr; ParseErr _ -> "error" }
-- "ELambdaPats [PVar \"x\"] (EInfix (EVar \"x\") \"+\" (EInt 1))"
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
-- >>> case parsePattern defaultConfig "(x, y)" of { ParseOk pat -> prettyASTPattern pat; ParseErr _ -> "error" }
-- "PTuple [PVar \"x\", PVar \"y\"]"
--
-- >>> case parsePattern defaultConfig "Just x" of { ParseOk pat -> prettyASTPattern pat; ParseErr _ -> "error" }
-- "PCon \"Just\" [PVar \"x\"]"
parsePattern :: ParserConfig -> Text -> ParseResult Pattern
parsePattern cfg input =
  let toks = lexTokensWithExtensions (parserExtensions cfg) input
   in case runParser (patternParser <* MP.eof) (parserSourceName cfg) (TokStream toks) of
        Left bundle -> ParseErr bundle
        Right pat -> ParseOk pat

-- | Parse a Haskell type.
--
-- >>> case parseType defaultConfig "Int -> Bool" of { ParseOk ty -> prettyASTType ty; ParseErr _ -> "error" }
-- "TFun (TCon \"Int\") (TCon \"Bool\")"
--
-- >>> case parseType defaultConfig "Maybe a" of { ParseOk ty -> prettyASTType ty; ParseErr _ -> "error" }
-- "TApp (TCon \"Maybe\") (TVar \"a\")"
parseType :: ParserConfig -> Text -> ParseResult Type
parseType cfg input =
  let toks = lexTokensWithExtensions (parserExtensions cfg) input
   in case runParser (typeParser <* MP.eof) (parserSourceName cfg) (TokStream toks) of
        Left bundle -> ParseErr bundle
        Right ty -> ParseOk ty

-- | Parse a complete Haskell module.
--
-- >>> case parseModule defaultConfig "module Main where\nmain = putStrLn \"Hello\"" of { ParseOk m -> prettyASTModule m; ParseErr _ -> "error" }
-- "Module {name = \"Main\", decls = [DeclValue (FunctionBind \"main\" [Match {rhs = UnguardedRhs (EApp (EVar \"putStrLn\") (EString \"Hello\"))}])]}"
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
