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
    TokenOrigin (..),
    lexModuleTokensWithExtensions,
    lexTokensWithExtensions,
    readModuleHeaderExtensions,
  )
import Aihc.Parser.Pretty ()
import Aihc.Parser.Syntax (Expr, Extension (..), ExtensionSetting (..), Module, Pattern, SourceSpan (..), Type)
import Aihc.Parser.Types
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
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
errorBundlePretty = renderErrorBlocks

extractCustomErrors :: MPE.ParseError TokStream ParserErrorComponent -> [ParserErrorComponent]
extractCustomErrors err =
  case err of
    MPE.FancyError _ fancySet -> mapMaybe fromFancy (Set.toList fancySet)
    _ -> []
  where
    fromFancy fancyErr =
      case fancyErr of
        MPE.ErrorCustom custom -> Just custom
        _ -> Nothing

extractUnexpecteds :: MPE.ParseError TokStream ParserErrorComponent -> [ParserErrorComponent]
extractUnexpecteds err =
  [custom | custom@UnexpectedTokenExpecting {} <- extractCustomErrors err]

bestFoundTokenForError :: MPE.ParseError TokStream ParserErrorComponent -> Maybe FoundToken
bestFoundTokenForError err =
  listToMaybe
    [found | UnexpectedTokenExpecting (Just found) _ _ <- extractUnexpecteds err]

bestUnexpectedForError :: MPE.ParseError TokStream ParserErrorComponent -> Maybe ParserErrorComponent
bestUnexpectedForError err =
  listToMaybe (extractUnexpecteds err)

sourcePosForOffset :: TokStream -> Int -> (Int, Int)
sourcePosForOffset stream off =
  let toks = unTokStream stream
      idx = max 0 off
      atIndex n = if n >= 0 && n < length toks then Just (toks !! n) else Nothing
      prevSourceToken n
        | n < 0 = Nothing
        | otherwise =
            case atIndex n of
              Just tok
                | lexTokenOrigin tok == FromSource -> Just tok
                | otherwise -> prevSourceToken (n - 1)
              Nothing -> Nothing
   in case atIndex idx of
        Just tok
          | lexTokenOrigin tok == InsertedLayout ->
              case prevSourceToken (idx - 1) of
                Just prevTok -> spanEnd (lexTokenSpan prevTok)
                Nothing -> spanStart (lexTokenSpan tok)
          | lexTokenKind tok == TkSpecialRBrace ->
              case atIndex (idx - 1) of
                Just prevTok -> spanEnd (lexTokenSpan prevTok)
                Nothing -> spanStart (lexTokenSpan tok)
          | otherwise -> spanStart (lexTokenSpan tok)
        Nothing ->
          case prevSourceToken (length toks - 1) of
            Just tok -> spanEnd (lexTokenSpan tok)
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
    Just found
      | foundTokenOrigin found == InsertedLayout -> 1
      | T.null (foundTokenText found) -> 1
      | otherwise -> max 1 (T.length (foundTokenText found))
    Nothing -> 1

renderUnexpectedToken :: FoundToken -> String
renderUnexpectedToken found =
  "unexpected " <> tokenDescriptor found

tokenDescriptor :: FoundToken -> String
tokenDescriptor found =
  case foundTokenOrigin found of
    InsertedLayout -> "end of input"
    FromSource ->
      case foundTokenKind found of
        Nothing -> "end of input"
        Just TkKeywordWhere -> "'where' keyword"
        Just _ -> "'" <> T.unpack (foundTokenText found) <> "'"

renderErrorBlocks :: Maybe Text -> ParseErrorBundle -> String
renderErrorBlocks mSource bundle =
  let pst = MPE.bundlePosState bundle
      sourceName = MP.sourceName (MP.pstateSourcePos pst)
      stream = MP.pstateInput pst
      errs = NE.toList (MPE.bundleErrors bundle)
      blocks = map (renderErrorBlock sourceName mSource stream) errs
   in List.intercalate "\n\n" blocks

renderErrorBlock :: FilePath -> Maybe Text -> TokStream -> MPE.ParseError TokStream ParserErrorComponent -> String
renderErrorBlock sourceName mSource stream err =
  let (lineNo, colNo) = positionForError stream err
      lineNoText = show lineNo
      srcLine = fromMaybe "" (getSourceLine mSource lineNo)
      markerPrefix = replicate (length lineNoText) ' ' <> " | "
      markerLen = markerLengthForError err
      marker = replicate (max 0 (colNo - 1)) ' ' <> replicate markerLen '^'
      msgLines = renderMessageLines err
      header = [sourceName <> ":" <> lineNoText <> ":" <> show colNo <> ":"]
      snippetLines =
        case mSource of
          Just _ -> [lineNoText <> " | " <> T.unpack srcLine, markerPrefix <> marker]
          Nothing -> []
   in List.intercalate "\n" (header <> snippetLines <> msgLines)

positionForError :: TokStream -> MPE.ParseError TokStream ParserErrorComponent -> (Int, Int)
positionForError stream err =
  case bestFoundTokenForError err of
    Just found
      | foundTokenOrigin found == InsertedLayout ->
          sourcePosForOffset stream (MPE.errorOffset err)
      | otherwise ->
          spanStart (foundTokenSpan found)
    Nothing -> sourcePosForOffset stream (MPE.errorOffset err)
  where
    spanStart span' =
      case span' of
        SourceSpan line col _ _ -> (line, col)
        NoSourceSpan -> (1, 1)

markerLengthForError :: MPE.ParseError TokStream ParserErrorComponent -> Int
markerLengthForError err =
  markerLength (bestFoundTokenForError err)

renderMessageLines :: MPE.ParseError TokStream ParserErrorComponent -> [String]
renderMessageLines err =
  case bestUnexpectedForError err of
    Just (UnexpectedTokenExpecting _ expecting contexts) ->
      [ maybe "unexpected end of input" renderUnexpectedToken (bestFoundTokenForError err),
        "expecting " <> T.unpack expecting
      ]
        <> map (\context -> "context: " <> T.unpack context) contexts
    _ ->
      [List.dropWhileEnd (`elem` ['\n', '\r']) (MPE.parseErrorTextPretty err)]
