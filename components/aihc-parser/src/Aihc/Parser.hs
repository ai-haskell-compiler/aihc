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
  ( LexToken (..),
    LexTokenKind (..),
    lexModuleTokensWithExtensions,
    lexTokensWithExtensions,
    readModuleHeaderExtensions,
  )
import Aihc.Parser.Pretty ()
import Aihc.Parser.Syntax (Expr, Extension (..), ExtensionSetting (..), Module, Pattern, SourceSpan (..), Type)
import Aihc.Parser.Types
import Control.Monad (guard)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec (runParser)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Error qualified as MPE

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
-- ParseOk (Module {name = "Main", decls = [DeclValue (FunctionBind "main" [Match {headForm = Prefix, rhs = UnguardedRhs (EApp (EVar "putStrLn") (EString "Hello"))}])]})
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
errorBundlePretty :: Maybe Text -> ParseErrorBundle -> String
errorBundlePretty mSource bundle =
  case renderCustomMessage mSource bundle of
    Just rendered -> rendered
    Nothing -> MP.errorBundlePretty bundle

renderCustomMessage :: Maybe Text -> ParseErrorBundle -> Maybe String
renderCustomMessage mSource bundle = do
  guard (NE.length (MPE.bundleErrors bundle) == 1)
  let err = NE.head (MPE.bundleErrors bundle)
  custom <- extractCustomError err
  renderCustomError mSource bundle err custom

extractCustomError :: MPE.ParseError TokStream ParserErrorComponent -> Maybe ParserErrorComponent
extractCustomError err =
  case err of
    MPE.FancyError _ fancySet -> listToMaybe (mapMaybe fromFancy (Set.toList fancySet))
    _ -> Nothing
  where
    fromFancy fancyErr =
      case fancyErr of
        MPE.ErrorCustom custom -> Just custom
        _ -> Nothing

renderCustomError :: Maybe Text -> ParseErrorBundle -> MPE.ParseError TokStream ParserErrorComponent -> ParserErrorComponent -> Maybe String
renderCustomError mSource bundle err custom = do
  let pst = MPE.bundlePosState bundle
      source = MP.pstateInput pst
      sourceName = MP.sourceName (MP.pstateSourcePos pst)
      offset = MPE.errorOffset err
      (lineNo, colNo) = sourcePosForOffset source offset
      location = sourceName <> ":" <> show lineNo <> ":" <> show colNo <> ":"
  case custom of
    MissingModuleName mFound -> do
      srcLine <- getSourceLine mSource lineNo
      let markerLen = markerLength mFound
          marker = replicate (max 0 (colNo - 1)) ' ' <> replicate markerLen '^'
          lineNoText = show lineNo
          markerPrefix = replicate (length lineNoText) ' ' <> " | "
          unexpectedLine = maybe "unexpected end of input" renderUnexpectedToken mFound
      pure . unlines $
        [ location,
          lineNoText <> " | " <> T.unpack srcLine,
          markerPrefix <> marker,
          unexpectedLine,
          "expecting module name"
        ]
    MissingImportModuleName mFound -> do
      srcLine <- getSourceLine mSource lineNo
      let markerLen = markerLength mFound
          marker = replicate (max 0 (colNo - 1)) ' ' <> replicate markerLen '^'
          lineNoText = show lineNo
          markerPrefix = replicate (length lineNoText) ' ' <> " | "
          unexpectedLine = maybe "unexpected end of input" renderUnexpectedToken mFound
      pure . unlines $
        [ location,
          lineNoText <> " | " <> T.unpack srcLine,
          markerPrefix <> marker,
          unexpectedLine,
          "expecting imported module name"
        ]

sourcePosForOffset :: TokStream -> Int -> (Int, Int)
sourcePosForOffset stream off =
  let toks = unTokStream stream
      idx = max 0 off
      atIndex n = if n >= 0 && n < length toks then Just (toks !! n) else Nothing
   in case atIndex idx of
        Just tok
          | lexTokenKind tok == TkSpecialRBrace ->
              case atIndex (idx - 1) of
                Just prevTok -> spanEnd (lexTokenSpan prevTok)
                Nothing -> spanStart (lexTokenSpan tok)
          | otherwise -> spanStart (lexTokenSpan tok)
        Nothing ->
          case reverse toks of
            tok : _ -> spanEnd (lexTokenSpan tok)
            [] -> (1, 1)
  where
    spanStart span' =
      case span' of
        SourceSpan line col _ _ -> (line, col)
        NoSourceSpan -> (1, 1)
    spanEnd span' =
      case span' of
        SourceSpan _ _ line col -> (line, col)
        NoSourceSpan -> (1, 1)

getSourceLine :: Maybe Text -> Int -> Maybe Text
getSourceLine mSource lineNo = do
  source <- mSource
  let sourceLines = T.lines source
  let idx = lineNo - 1
  if idx < 0 || idx >= length sourceLines
    then Nothing
    else Just (sourceLines !! idx)

markerLength :: Maybe FoundToken -> Int
markerLength mFound =
  case mFound of
    Just found -> max 1 (T.length (foundTokenText found))
    Nothing -> 1

renderUnexpectedToken :: FoundToken -> String
renderUnexpectedToken found =
  "unexpected " <> tokenDescriptor found

tokenDescriptor :: FoundToken -> String
tokenDescriptor found =
  case foundTokenKind found of
    TkKeywordWhere -> "'where' keyword"
    _ -> "'" <> T.unpack (foundTokenText found) <> "'"
