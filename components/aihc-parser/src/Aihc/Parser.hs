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
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter (Doc, colon, defaultLayoutOptions, layoutPretty, pretty, vcat)
import Prettyprinter.Render.String (renderString)
import Text.Megaparsec (runParser)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Error (ErrorFancy (..), ErrorItem (..))
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
   in case runParser (exprParser <* MP.eof) (parserSourceName cfg) (mkTokStream toks) of
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
   in case runParser (patternParser <* MP.eof) (parserSourceName cfg) (mkTokStream toks) of
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
   in case runParser (typeParser <* MP.eof) (parserSourceName cfg) (mkTokStream toks) of
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
   in case runParser (moduleParser <* MP.eof) (parserSourceName cfg) (mkTokStream toks) of
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

-- | Turn a Megaparsec 'MPE.ParseError' into message blocks with optional spans.
--
-- * 'MPE.TrivialError': text from 'MPE.parseErrorTextPretty' (Megaparsec\'s built-in
--   formatting for unexpected / expected items). A span is attached when the
--   unexpected item is token content ('ErrorItem.Tokens').
-- * 'MPE.FancyError': one block per fancy error. 'ErrorFail' and 'ErrorIndentation'
--   are formatted with 'MPE.parseErrorTextPretty' on a singleton fancy error (same
--   text Megaparsec uses internally). 'ErrorCustom' uses our unexpected line,
--   'MPE.showErrorComponent', and context lines.
renderParseErrors :: MPE.ParseError TokStream ParserErrorComponent -> [(Maybe SourceSpan, Doc ann)]
renderParseErrors err =
  case err of
    MPE.TrivialError _ mUnexpected _ ->
      let mSpan = trivialUnexpectedSpan mUnexpected
          text =
            List.dropWhileEnd
              (`elem` ['\n', '\r'])
              (MPE.parseErrorTextPretty err)
       in [(mSpan, vcat (map pretty (lines text)))]
    MPE.FancyError _ fancySet ->
      map renderFancyError (Set.toAscList fancySet)
  where
    trivialUnexpectedSpan :: Maybe (ErrorItem LexToken) -> Maybe SourceSpan
    trivialUnexpectedSpan mItem =
      case mItem of
        Just (Tokens ts) -> Just (lexTokenSpan (NE.head ts))
        _ -> Nothing

    renderFancyError :: ErrorFancy ParserErrorComponent -> (Maybe SourceSpan, Doc ann)
    renderFancyError fancy =
      case fancy of
        ErrorCustom custom ->
          ( customFoundSpan custom,
            vcat (map pretty (customMessageLines custom))
          )
        _ ->
          ( Nothing,
            pretty
              ( List.dropWhileEnd
                  (`elem` ['\n', '\r'])
                  ( MPE.parseErrorTextPretty
                      ( MPE.FancyError 0 (Set.singleton fancy) ::
                          MPE.ParseError TokStream ParserErrorComponent
                      )
                  )
              )
          )

    customFoundSpan :: ParserErrorComponent -> Maybe SourceSpan
    customFoundSpan (UnexpectedTokenExpecting (Just found) _ _) =
      Just (foundTokenSpan found)
    customFoundSpan _ = Nothing

    customMessageLines :: ParserErrorComponent -> [String]
    customMessageLines e@(UnexpectedTokenExpecting mFound _ contexts) =
      [maybe "unexpected end of input" renderUnexpectedToken mFound, MPE.showErrorComponent e]
        <> map (\context -> "context: " <> T.unpack context) contexts

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
        Just kind -> show kind

renderErrorBlocks :: Maybe Text -> ParseErrorBundle -> String
renderErrorBlocks mSource bundle =
  let pst = MPE.bundlePosState bundle
      sourceName = MP.sourceName (MP.pstateSourcePos pst)
      errs = NE.toList (MPE.bundleErrors bundle)
      opts = defaultLayoutOptions
      blocks =
        map
          (renderString . layoutPretty opts . renderErrorBlock sourceName mSource)
          errs
   in List.intercalate "\n\n" blocks

-- renderSourceReference "<input>" "x = 1" (SourceSpan 1 5 1 6) = """
-- <input>:1:5:
-- 1 | x = 1
--   |     ^
-- """
-- renderSourceReference "<input>" "module where" (SourceSpan 1 8 1 13) = """
-- <input>:1:5:
-- 1 | module where
--   |        ^^^^^
-- """
renderSourceReference :: String -> Text -> SourceSpan -> Doc ann
renderSourceReference origin source srcSpan =
  let (lineNo, colNo, endCol) = case srcSpan of
        SourceSpan sourceLine col _ endC -> (sourceLine, col, endC)
        NoSourceSpan -> (1, 1, 1)
      lineNoText = show lineNo
      sourceLines = T.lines source
      lineIdx = lineNo - 1
      srcLine =
        if lineIdx < 0 || lineIdx >= length sourceLines
          then ""
          else T.unpack (sourceLines !! lineIdx)
      markerPrefix = replicate (length lineNoText) ' ' ++ " | "
      markerStart = max 0 (colNo - 1)
      markerLen = max 1 (endCol - colNo)
      marker = replicate markerStart ' ' ++ replicate markerLen '^'
      header =
        pretty origin <> colon <> pretty lineNo <> colon <> pretty colNo <> colon
   in vcat
        [ header,
          pretty (lineNoText ++ " | " ++ srcLine),
          pretty (markerPrefix ++ marker)
        ]

renderErrorBlock :: FilePath -> Maybe Text -> MPE.ParseError TokStream ParserErrorComponent -> Doc ann
renderErrorBlock sourceName mSource err =
  vcat
    [ case (mbSpan, mSource) of
        (Just srcSpan, Just source) -> vcat [renderSourceReference sourceName source srcSpan, doc]
        _ -> vcat [pretty sourceName, doc]
    | (mbSpan, doc) <- renderParseErrors err
    ]
